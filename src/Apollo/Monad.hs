{-|
 -
 - The apollo monad is a free monad with a few very high-level actions.
 -}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module Apollo.Monad
( -- * Apollo monad
  ApolloF(..)
, Apollo
  -- ** Actions
, youtubeDl
, getPlayerStatus
, enqueueTracks
, deleteTracks
, getPlaylist
, transcodeTrack
, readTrackLazily
, readTranscodeLazily
, readArchiveLazily
, makeArchive
, getStaticUrl
, getApiLink
, startAsyncJob
, queryAsyncJob
, getMusicDir
, getTranscodeDir
  -- ** Interpreters
, interpretApolloIO
, ApolloIO
, runApolloIO
, ApolloError(..)
  -- * Misc
, ApolloSettings(..)
, ServerSettings(..)
, MpdSettings(..)
, DirLock
, MpdLock
, makeDirLock
, makeMpdLock
, newJobBankVar
) where

import Apollo.Archive
import Apollo.Background
import Apollo.Monad.Types
import Apollo.Transcoding
import Apollo.Track
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import qualified Codec.Archive.Zip as Zip
import Control.Monad ( foldM, forM_ )
import Control.Monad.Free
import Control.Monad.Except
import Data.Aeson ( ToJSON )
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable ( for_ )
import Data.Maybe ( fromJust )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Traversable ( for )
import qualified Network.MPD as MPD
import Network.URI
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

-- | Convert an MPD @Song@ into a @PlaylistEntry@ using 'fromJust' for the
-- components that are wrapped in 'Maybe'. This is only safe to do when the
-- songs come from a playlist, in which case the components wrapped in @Maybe@
-- are guaranteed to be 'Just'.
unsafeSongToPlaylistEntry :: MPD.Song -> PlaylistEntry
unsafeSongToPlaylistEntry MPD.Song{..} = PlaylistEntry
  { entryPath = MPD.toString sgFilePath
  , entryId = PlaylistItemId . (\(MPD.Id i) -> i) $ fromJust sgId
  , entryPosition = PlaylistPosition $ fromJust sgIndex
  , entryDuration = Seconds sgLength
  }

runApolloIO :: ApolloIO a -> IO (Either ApolloError a)
runApolloIO = runExceptT . unApolloIO

-- | Execute apollo actions in the IO monad, with some distinguished errors.
interpretApolloIO :: ApolloSettings -> Apollo a -> ApolloIO a
interpretApolloIO ApolloSettings{..} = iterM phi where
  MpdSettings{..} = apolloMpdSettings

  runMpd = MPD.withMPDEx mpdHost mpdPort mpdPassword

  runMpdLockedEx action = withMpdLock' $ do
    r <- liftIO $ runMpd action
    case r of
      Left e -> throwError $ ApolloMpdError (MpdError e)
      Right x -> pure x

  musicDirP = "music"
  transcodeDirP = "transcoded"
  archiveDirP = "archives"

  queueJobApollo
    :: (RunnableJob j, ToJSON (Result j), ToJSON (UserError j))
    => JobS j
    -> Env j
    -> Params j
    -> IO JobId
  queueJobApollo j e p = queueJob j e p apolloJobBank

  queryJobApollo :: JobId -> JobS j -> IO (Maybe SomeJobStatus)
  queryJobApollo i j = queryJob i j apolloJobBank

  inDir d action = withDirLock' $ withCwd d $ do
    liftIO $ putStrLn $ "cwd -> " ++ d
    x <- action
    liftIO $ putStrLn $ "cwd <- " ++ d
    pure x
  inMusicDir = inDir musicDirP

  withTempDirectory' :: String -> String -> (FilePath -> ApolloIO a) -> ApolloIO a
  withTempDirectory' x y action =
    ApolloIO $ ExceptT $ withTempDirectory x y (runExceptT . unApolloIO . action)

  withDirLock' :: ApolloIO a -> ApolloIO a
  withDirLock' = ApolloIO . ExceptT . withDirLock apolloDirLock . runExceptT . unApolloIO

  withMpdLock' :: ApolloIO a -> ApolloIO a
  withMpdLock' = ApolloIO . ExceptT . withMpdLock apolloMpdLock . runExceptT . unApolloIO

  withCwd :: FilePath -> ApolloIO a -> ApolloIO a
  withCwd d = ApolloIO . ExceptT . Dir.withCurrentDirectory d . runExceptT . unApolloIO

  phi :: ApolloF (ApolloIO a) -> ApolloIO a
  phi = \case
    YoutubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) k -> do
      let dp = T.unpack musicDirT
      let url = T.unpack dlUrl

      entries <- withTempDirectory' "/tmp" "apollo." $ \dirPath -> do
        outputFiles <- withDirLock' $ do
          withCwd dirPath $ liftIO $ do
            Y.youtubeDl url
            Dir.listDirectory "."

        inMusicDir $ liftIO $ do
          Dir.createDirectoryIfMissing True dp
          forM_ outputFiles $ \outputFile -> do
            Dir.copyFile (dirPath </> outputFile) (dp </> outputFile)

        pure (Entry musicDir . T.pack <$> outputFiles)

      k entries

    GetPlayerStatus k -> do
      status <- runMpdLockedEx $ do
        MPD.Status{..} <- MPD.status
        MPD.Stats{..} <- MPD.stats
        pure PlayerStatus
          { psState = PlaybackState stState
          , psPlaylistLength = stPlaylistLength
          , psTrackId = PlaylistItemId . (\(MPD.Id i) -> i) <$> stSongID
          , psNextTrackId = PlaylistItemId . (\(MPD.Id i) -> i) <$> stNextSongID
          , psUptime = stsUptime
          , psPlaytime = stsPlaytime
          , psLastUpdateTime = stsDbUpdate
          }

      k status

    EnqueueTracks pos tracks k -> do
      rs <- runMpdLockedEx $ do
        liftIO $ print pos
        enqueuePos <- case pos of
          FromBeginning (nonZero -> n) -> pure $ Just $ if n < 0 then 0 else n
          FromEnd (nonZero -> n) -> do
            l <- fromIntegral . MPD.stPlaylistLength <$> MPD.status
            pure $ Just $ if n < 0 then l + n else l
          FromPlaying (nonZero -> n) ->
            fmap (if n < 0 then (+ (n+1)) else (+ n)) . MPD.stSongPos <$> MPD.status

        for (reverse tracks) $
          \track ->
            PlaylistItemId . (\(MPD.Id i) -> i)
              <$> MPD.addId (fromString track) enqueuePos
      k rs

    DeleteTracks items k -> (k <*) $ runMpdLockedEx $ do
      for_ items $ \(PlaylistItemId i) -> MPD.deleteId (MPD.Id i)

    GetPlaylist k -> do
      (entries, st) <- runMpdLockedEx $ (,)
        <$> ((unsafeSongToPlaylistEntry <$>) <$> MPD.playlistInfo Nothing)
        <*> (MPD.stSongID <$> MPD.status)
      k Playlist
        { playlistTracks = entries
        , playlistNowPlaying = st >>= (\(MPD.Id i) -> pure $ PlaylistItemId i)
        }

    Transcode track params k -> do
      let e = (musicDirP, transcodeDirP)
      let p = (track, params)
      k =<< liftIO (runJobDefaultEx (job (proxy TranscodeS)) e p)

    ReadTrackLazily track k
      -> k =<< (liftIO $ readTrackLazilyIO (musicDirP </> track))

    ReadTranscodeLazily t params k -> do
      mp <- liftIO $ getExistingTranscode (Just transcodeDirP) t params
      d <- case mp of
        Just transcodePath -> liftIO $ readTrackLazilyIO transcodePath
        Nothing -> throwError $ NoSuchTranscode t params
      k d

    ReadArchiveLazily (ArchiveId (Sha1Hash b)) k ->
      let p = C8.unpack b
      in k =<< (liftIO $ readArchiveLazilyIO (archiveDirP </> p))

    MakeArchive entries k -> do
      let archiveId@(ArchiveId (Sha1Hash idB)) = makeArchiveId entries

      liftIO (getExistingArchive (Just archiveDirP) archiveId) >>= \case
        Just _ -> pure ()
        Nothing -> do
          let forFoldM i xs g = foldM g i xs
          archive <- forFoldM Zip.emptyArchive entries $ \a e -> do
            -- compute the path to read from, and the path to put in the archive
            (srcP, dstP) <- case e of
              ArchiveTrack p -> pure (musicDirP </> p, "raw" </> p)
              ArchiveTranscode tid params -> do
                m <- liftIO $ getExistingTranscode (Just transcodeDirP) tid params
                p <- maybe (throwError $ NoSuchTranscode tid params) pure m
                pure (p, "transcoded" </> transcodeDirectoryFor tid params)
            let opts = [Zip.OptVerbose, Zip.OptLocation dstP False]
            liftIO $ Zip.addFilesToArchive opts a [srcP]

          let archivePath = archiveDirP </> T.unpack (decodeUtf8 idB)
          let utf8str = encodeUtf8 . T.pack
          liftIO $ Bin.encodeFile
            archivePath
            archive { Zip.zComment = LBS.fromStrict (utf8str "Apollo archive " <> idB) }

      k archiveId

    GetStaticUrl res k -> (>>= k) $ case res of
      StaticArchive (ArchiveId (Sha1Hash b)) -> do
        let (Url baseUrl) = serverSettingsBaseUrl apolloStaticServerSettings
        pure $ Url $ (baseUrl <> "/archives/" <> decodeUtf8 b)
      StaticTranscode tid params -> error "StaticTranscode" tid params
      StaticTrack p -> error "StaticTrack" p

    GetApiLink u k -> do
      let sc = T.unpack (serverScheme apolloApiServerSettings) ++ ":"
      let d = T.unpack (serverDomain apolloApiServerSettings)
      let auth = Just URIAuth { uriUserInfo = "", uriRegName = d, uriPort = "" }
      let uri = u { uriScheme = sc, uriAuthority = auth }
      let s = uriToString id uri ""
      k (Url (T.pack s))

    StartAsyncJob j e p k -> k =<< liftIO (queueJobApollo j e p)

    QueryAsyncJob i j k -> do
      m <- liftIO $ queryJobApollo i j
      SomeJobStatus _ s <- maybe (throwError $ NoSuchJob i) pure m
      k (JobQueryResult s)

    GetMusicDir k -> k musicDirP

    GetTranscodeDir k -> k transcodeDirP
