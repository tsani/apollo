{-|
 -
 - The apollo monad is a free monad with a few very high-level actions.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Apollo.Monad
( -- * Apollo monad
  ApolloF(..)
, Apollo
  -- ** Interpreters
, runApollo'
, runApollo
, ApolloIO
, runApolloIO
, ApolloError(..)
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
, getArchiveDir
  -- ** Derived actions
, forUpdateLen
, forUpdate
, startBatchAsyncJob
  -- * Misc
, ApolloSettings(..)
, ServerSettings(..)
, MpdSettings(..)
, DirLock
, MpdLock
, makeDirLock
, makeMpdLock
, doTranscode
, doMakeArchive
) where

import Apollo.Archive
import Apollo.Types.Job
import Apollo.Monad.Types
import Apollo.Misc ( (<#>), untilM )
import Apollo.Transcoding
import Apollo.Track
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import qualified Codec.Archive.Zip as Zip
import Control.Concurrent.MVar
import Control.Monad ( foldM, forM_ )
import Control.Monad.Free
import Control.Monad.Except
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as N
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

-- | Interpret an @Apollo@ action directly to @IO@.
runApollo'
  :: (Bounded k, Ord k, Enum k)
  => ApolloSettings k e r
  -> Apollo k e r a
  -> IO (Either (ApolloError k) a)
runApollo' s = runApolloIO . runApollo s

-- | Interpret the intermediate @ApolloIO@ to true @IO@.
runApolloIO :: ApolloIO k a -> IO (Either (ApolloError k) a)
runApolloIO = runExceptT . unApolloIO

-- | Interpret the base @Apollo@ action into an @ApolloIO@.
runApollo
  :: forall k e r a. (Bounded k, Ord k, Enum k)
  => ApolloSettings k e r
  -> Apollo k e r a
  -> ApolloIO k a
runApollo ApolloSettings{..} = iterM phi where
  MpdSettings{..} = apolloMpdSettings

  runMpd :: MPD.MPD b -> IO (MPD.Response b)
  runMpd = MPD.withMPDEx mpdHost mpdPort mpdPassword

  runMpdLockedEx :: MPD.MPD b -> ApolloIO k' b
  runMpdLockedEx action = withMpdLock' $ do
    r <- liftIO $ runMpd action
    case r of
      Left e -> throwError $ ApolloMpdError (MpdError e)
      Right x -> pure x

  musicDirP = "music"
  transcodeDirP = "transcoded"
  archiveDirP = "archives"

  withTempDirectory'
    :: String
    -> String
    -> (FilePath -> ApolloIO k b)
    -> ApolloIO k b
  withTempDirectory' x y action =
    ApolloIO $ ExceptT $ withTempDirectory x y (runExceptT . unApolloIO . action)

  withDirLock' :: ApolloIO k' b -> ApolloIO k' b
  withDirLock' = ApolloIO . ExceptT . withDirLock apolloDirLock . runApolloIO

  withMpdLock' :: ApolloIO k' b -> ApolloIO k' b
  withMpdLock' = ApolloIO . ExceptT . withMpdLock apolloMpdLock . runApolloIO

  withCwd :: FilePath -> ApolloIO k' b -> ApolloIO k' b
  withCwd d = ApolloIO . ExceptT . Dir.withCurrentDirectory d . runApolloIO

  withJobBank
    :: (JobBank k e r -> ApolloIO k (JobBank k e r, b))
    -> ApolloIO k b
  withJobBank f = ApolloIO . ExceptT $ do
    modifyMVar apolloJobBank $ \j -> do
      either ((j,) . Left) (Right <$>) <$> runApolloIO (f j)

  inDir d action = withDirLock' $ withCwd d $ do
    liftIO $ putStrLn $ "cwd -> " ++ d
    x <- action
    liftIO $ putStrLn $ "cwd <- " ++ d
    pure x
  inMusicDir = inDir musicDirP

  queueJob :: Progress -> Job e r -> ApolloIO k k
  queueJob p j = withJobBank $ \b -> liftIO (startJob p b j)

  queryJob :: k -> ApolloIO k (JobInfo e r)
  queryJob i = withJobBank $ \b -> liftIO (checkJob i b) >>= \case
    Nothing -> throwError (NoSuchJob i)
    Just x -> pure x

  -- | Runs an MPD database update and polls the daemon until it completes.
  updateDB :: Maybe MPD.Path -> ApolloIO k ()
  updateDB p = do
    j <- runMpdLockedEx $ MPD.update p
    -- loop until we discover that either our job isn't running, or that a
    -- different update job is running
    untilM $ runMpdLockedEx $ MPD.status <#> \MPD.Status{..} ->
      case stUpdatingDb of
        Just j' -> j' /= j
        Nothing -> True

  phi :: forall a'. ApolloF k e r (ApolloIO k a') -> ApolloIO k a'
  phi = \case
    YoutubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) k -> do
      let dp = T.unpack musicDirT
      let url = T.unpack dlUrl

      entries <- withTempDirectory' "/tmp" "apollo." $ \dirPath -> do
        outputFiles <- withDirLock' $ do
          withCwd dirPath $ do
            xs <- liftIO $ do
              Y.youtubeDl url
              Dir.listDirectory "."
            maybe (throwError EmptyYoutubeDlResult) pure $ N.nonEmpty xs

        inMusicDir $ liftIO $ do
          Dir.createDirectoryIfMissing True dp
          forM_ outputFiles $ \outputFile -> do
            Dir.copyFile (dirPath </> outputFile) (dp </> outputFile)

        pure (Entry musicDir . T.pack <$> outputFiles)

      let musicDirS = T.unpack musicDirT
      liftIO (putStrLn $ "updating MPD database in " ++ musicDirS)
      updateDB (Just $ fromString musicDirS)

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
          FromPlaying (nonZero -> n) -> do
            fmap (if n < 0 then (+ (n+1)) else (+ n)) . MPD.stSongPos <$> MPD.status

        liftIO (putStrLn $ "current song pos: " ++ show enqueuePos)

        for tracks $
          \track ->
            PlaylistItemId . (\(MPD.Id i) -> i)
              <$> MPD.addId (fromString track) enqueuePos

      liftIO (putStrLn $ "entries: " ++ show rs)

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

    Transcode track params k ->
      k =<< liftIO (doTranscode musicDirP transcodeDirP track params)

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
      m <- liftIO $ runJob $
        doMakeArchive musicDirP transcodeDirP archiveDirP entries
      case m of
        Left e -> throwError e
        Right i -> k i

    GetStaticUrl res k -> (>>= k) $ case res of
      StaticArchive (ArchiveId (Sha1Hash b)) -> do
        let (Url baseUrl) = serverSettingsBaseUrl apolloStaticServerSettings
        pure $ Url $ (baseUrl <> "/archives/" <> decodeUtf8 b)
      StaticTranscode tid params -> error "StaticTranscode" tid params
      StaticTrack p -> error "StaticTrack" p

    GetApiLink u k -> do
      let f = ('/' :)
      let sc = T.unpack (serverScheme apolloApiServerSettings) ++ ":"
      let d = T.unpack (serverDomain apolloApiServerSettings)
      let p = ":" ++ show (serverPort apolloApiServerSettings)
      let auth = Just URIAuth { uriUserInfo = "", uriRegName = d, uriPort = p }
      let uri = u { uriScheme = sc, uriAuthority = auth, uriPath = f $ uriPath u }
      let s = uriToString id uri ""
      k (Url (T.pack s))

    StartAsyncJob p j k -> k =<< queueJob p j

    QueryAsyncJob i k -> k =<< queryJob i

    GetMusicDir k -> k musicDirP

    GetTranscodeDir k -> k transcodeDirP

    GetArchiveDir k -> k archiveDirP

doTranscode
  :: FilePath -- ^ music dir
  -> FilePath -- ^ transcode dir
  -> FilePath -- ^ track path
  -> TranscodingParameters -> IO TrackId
doTranscode musicDirP transcodeDirP track params = do
  d <- readTrackIO (musicDirP </> track)
  let tid = trackId d
  putStrLn $ "computed track ID " ++ show tid
  mp <- getExistingTranscode (Just transcodeDirP) tid params
  putStrLn "got existing transcode"
  _ <- case mp of
    Just transcodePath -> pure (transcodeDirP </> transcodePath)
      -- then the transcode exists, so no work to do.
    Nothing -> do -- no transcode exists, so we have to make one
      let dir = transcodeDirectoryFor tid params
      Dir.createDirectoryIfMissing True (transcodeDirP </> dir)
      transcode (musicDirP </> track) (transcodeDirP </> dir) params
  pure tid

doMakeArchive
  :: Traversable t
  => FilePath -- ^ music dir
  -> FilePath -- ^ transcode dir
  -> FilePath -- ^ archive dir
  -> t ArchiveEntry
  -> Job (ApolloError k) ArchiveId
doMakeArchive musicDirP transcodeDirP archiveDirP entries = do
  let archiveId@(ArchiveId (Sha1Hash idB)) = makeArchiveId entries
  let getExisting = getExistingArchive (Just archiveDirP) archiveId
  let l = length entries

  v <- liftIO (newIORef 0)

  liftIO getExisting >>= \case
    Just _ -> pure () -- archive already exists; nothing to do
    Nothing -> do
      let forFoldM i xs g = foldM g i xs
      archive <- forFoldM Zip.emptyArchive entries $ \a e -> do
        -- compute the path to read from, and the path to put in the archive
        (srcP, dstP) <- case e of
          ArchiveTrack p -> pure (musicDirP </> p, "raw" </> p)
          ArchiveTranscode tid params -> do
            m <- liftIO $ getExistingTranscode (Just transcodeDirP) tid params
            p <- maybe (jobError $ NoSuchTranscode tid params) pure m
            pure (p, "transcoded" </> transcodeDirectoryFor tid params)

        i <- liftIO $ atomicModifyIORef v (\i -> (i + 1, i))
        reportProgress (Progress i l)

        let opts = [Zip.OptVerbose, Zip.OptLocation dstP False]
        liftIO $ Zip.addFilesToArchive opts a [srcP]

      let archivePath = archiveDirP </> T.unpack (decodeUtf8 idB)
      let utf8str = encodeUtf8 . T.pack
      liftIO $ Bin.encodeFile
        archivePath
        archive
          { Zip.zComment = LBS.fromStrict (utf8str "Apollo archive " <> idB)
          }

  pure archiveId
