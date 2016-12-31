{-|
 -
 - The apollo monad is a free monad with a few very high-level actions.
 -}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
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
) where

import Apollo.Archive
import Apollo.Crypto
import Apollo.Transcoding
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import qualified Codec.Archive.Zip as Zip
import Control.Concurrent.MVar
import Control.Monad ( foldM, forM_ )
import Control.Monad.Free
import Control.Monad.Except
import qualified Data.Binary as Bin
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class
import Data.Foldable ( for_ )
import Data.Maybe ( fromJust )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Traversable ( for )
import qualified Network.MPD as MPD
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

-- | Base functor for the 'Apollo' monad.
data ApolloF next
  -- | Call youtube-dl on the given URL and store all generated tracks into the
  -- given musicdir.
  = YoutubeDl MusicDir YoutubeDlUrl ([Entry] -> next)
  -- | Gets the playback status.
  | GetPlayerStatus (PlayerStatus -> next)
  -- | Enqueues the given tracks for playback after the current playing track,
  -- if any.
  | EnqueueTracks PositionBetweenTracks [FilePath] ([PlaylistItemId] -> next)
  -- | Deletes the given tracks from the playlist.
  | DeleteTracks [PlaylistItemId] next
  -- | Gets the playlist.
  | GetPlaylist (Playlist -> next)
  -- | Transcode a given track with given transcoding parameters.
  -- The unique identifier for the track is returned.
  | Transcode FilePath TranscodingParameters (TrackId -> next)
  -- | Lazily read track data from the filesystem. This can be used to stream a
  -- track from disk to a network client.
  | ReadTrackLazily FilePath (LazyTrackData -> next)
  -- | Lazily read a transcode from the filesystem. This can be used to stream
  -- a transcoded track from disk to a network client.
  | ReadTranscodeLazily TrackId TranscodingParameters (LazyTrackData -> next)
  -- | Lazily read an archive from the filesystem. This can be used to stream
  -- an archive from disk to a network client.
  | ReadArchiveLazily ArchiveId (LazyArchiveData -> next)
  -- | Create an archive with the given items.
  | MakeArchive [ArchiveEntry] (ArchiveId -> next)
  | GetStaticUrl StaticResource (Url -> next)
  deriving Functor

-- | The Apollo free monad.
type Apollo = Free ApolloF

-- | See 'YoutubeDl'.
youtubeDl :: MusicDir -> YoutubeDlUrl -> Apollo [Entry]
youtubeDl x y = liftF $ YoutubeDl x y id

-- | See 'GetPlayerStatus'.
getPlayerStatus :: Apollo PlayerStatus
getPlayerStatus = liftF $ GetPlayerStatus id

-- | See 'EnqueueTracks'.
enqueueTracks :: PositionBetweenTracks -> [FilePath] -> Apollo [PlaylistItemId]
enqueueTracks pos tracks = liftF $ EnqueueTracks pos tracks id

deleteTracks :: [PlaylistItemId] -> Apollo ()
deleteTracks items = liftF $ DeleteTracks items ()

getPlaylist :: Apollo Playlist
getPlaylist = liftF $ GetPlaylist id

-- | See 'Transcode'.
transcodeTrack :: FilePath -> TranscodingParameters -> Apollo TrackId
transcodeTrack track params = liftF $ Transcode track params id

-- | See 'ReadTrackLazily'.
readTrackLazily :: FilePath -> Apollo LazyTrackData
readTrackLazily p = liftF $ ReadTrackLazily p id

-- | See 'ReadTranscodeLazily'.
readTranscodeLazily :: TrackId -> TranscodingParameters -> Apollo LazyTrackData
readTranscodeLazily t params = liftF $ ReadTranscodeLazily t params id

readArchiveLazily :: ArchiveId -> Apollo LazyArchiveData
readArchiveLazily i = liftF $ ReadArchiveLazily i id

makeArchive :: [ArchiveEntry] -> Apollo ArchiveId
makeArchive entries = liftF $ MakeArchive entries id

getStaticUrl :: StaticResource -> Apollo Url
getStaticUrl res = liftF $ GetStaticUrl res id

-- | A lock for the current working directory. Since the CWD is global state,
-- we need to ensure that only one thread changes directories at a time.
newtype DirLock = DirLock (MVar ())

-- | Guards an IO action with the directory lock.
withDirLock :: DirLock -> IO a -> IO a
withDirLock (DirLock d) = withMVar d . const

-- | Constructs a directory lock.
makeDirLock :: IO DirLock
makeDirLock = DirLock <$> newMVar ()

-- | A lock for MPD access. This lock enforces a sort of transactional
-- processing of MPD actions. The 'interpretApolloIO' interpreter batches all
-- MPD actions performed via 'mpd' into one transaction protected by this lock,
-- so that concurrent web requests will block for MPD access.
newtype MpdLock = MpdLock (MVar ())

-- | Guards an IO action with the MPD lock.
withMpdLock :: MpdLock -> IO a -> IO a
withMpdLock (MpdLock d) = withMVar d . const

-- | Constructs an MPD lock.
makeMpdLock :: IO MpdLock
makeMpdLock = MpdLock <$> newMVar ()

-- | Connection settings for MPD.
data MpdSettings
  = MpdSettings
    { mpdHost :: !String
    -- ^ The hostname to connect to.
    , mpdPort :: !Integer
    -- ^ The port to connect to.
    , mpdPassword :: !String
    -- ^ The password to authenticate with the MPD instance.
    }

-- | Localhost, post 6600, no password.
instance Default MpdSettings where
  def = MpdSettings
    { mpdHost = "localhost"
    , mpdPort = 6600
    , mpdPassword = ""
    }

newtype MpdError = MpdError MPD.MPDError deriving Show

data ServerSettings
  = ServerSettings
    { serverDomain :: T.Text
    , serverScheme :: T.Text
    , serverPort :: Int
    }

serverSettingsBaseUrl :: ServerSettings -> Url
serverSettingsBaseUrl ServerSettings{..} = Url u where
  u = serverScheme <> "://" <> serverDomain <> ":" <> T.pack (show serverPort)

data ApolloSettings
  = ApolloSettings
    { apolloApiServerSettings :: ServerSettings
    , apolloStaticServerSettings :: ServerSettings
    , apolloDirLock :: DirLock
    , apolloMpdLock :: MpdLock
    , apolloMpdSettings :: MpdSettings
    }

-- | Reads an entire track into memory strictly.
--
-- Precondition: must be in the music directory.
readTrackIO :: FilePath -> IO TrackData -- BS.ByteString
readTrackIO = fmap TrackData . BS.readFile

-- | Reads an entire track lazily. This can operate in constant memory.
readTrackLazilyIO :: FilePath -> IO LazyTrackData -- LBS.ByteString
readTrackLazilyIO = fmap LazyTrackData . LBS.readFile

-- | Reads an entire archive lazily. This can operate in constant memory.
readArchiveLazilyIO :: FilePath -> IO LazyArchiveData -- LBS.ByteString
readArchiveLazilyIO = fmap LazyArchiveData . LBS.readFile

-- | Computes the track ID given track data.
trackId :: TrackData -> TrackId
trackId (TrackData b) = TrackId (sha1 b)

-- | Errors that can arise during the IO interpretation of the 'Apollo' monad,
-- 'interpretApolloIO'.
data ApolloError
  = ApolloMpdError MpdError
  | NoSuchTranscode TrackId TranscodingParameters

-- | Result of 'interpretApolloIO'. This is just an IO monad with some
-- distinguished errors 'ApolloError'.
newtype ApolloIO a
  = ApolloIO
    { unApolloIO :: ExceptT ApolloError IO a
    }
  deriving (Functor, Applicative, Monad, MonadError ApolloError, MonadIO)

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
      -- TODO we read the entire track into memory to compute the hash. This is
      -- wasteful. We could stream the data on disk into the hashing function
      -- to operate in constant memory.
      d <- liftIO $ readTrackIO (musicDirP </> track)
      let tid = trackId d
      liftIO $ putStrLn $ "computed track ID " ++ show tid
      mp <- liftIO $ getExistingTranscode (Just transcodeDirP) tid params
      liftIO $ putStrLn "got existing transcode"
      _ <- case mp of
        Just transcodePath -> pure (transcodeDirP </> transcodePath)
          -- then the transcode exists, so no work to do.
        Nothing -> liftIO $ do -- no transcode exists, so we have to make one
          let dir = transcodeDirectoryFor tid params
          Dir.createDirectoryIfMissing True (transcodeDirP </> dir)
          transcode (musicDirP </> track) (transcodeDirP </> dir) params

      k tid

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
