{-|
 -
 - The apollo monad is a free monad with a few very high-level actions.
 -}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Apollo.Monad
( -- * Apollo monad
  ApolloF(..)
, Apollo
  -- ** Actions
, youtubeDl
, getPlayerStatus
, enqueueTracks
, transcodeTrack
, readTrackLazily
, readTranscodeLazily
  -- ** Interpreters
, interpretApolloIO
  -- * Misc
, MpdSettings(..)
, DirLock
, MpdLock
, makeDirLock
, makeMpdLock
) where

import Apollo.Crypto
import Apollo.Transcoding
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import Control.Concurrent.MVar
import Control.Exception ( Exception, throwIO )
import Control.Monad ( forM_ )
import Control.Monad.Free
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Traversable ( for )
import qualified Network.MPD as MPD
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

data ApolloF next
  -- | Call youtube-dl on the given URL and store all generated tracks into the
  -- given musicdir.
  = YoutubeDl MusicDir YoutubeDlUrl ([Entry] -> next)
  -- | Gets the playback status.
  | GetPlayerStatus (PlayerStatus -> next)
  -- | Enqueues the given tracks for playback after the current playing track,
  -- if any.
  | EnqueueTracks [FilePath] ([PlaylistItemId] -> next)
  | Transcode FilePath TranscodingParameters (TrackId -> next)
  | ReadTrackLazily FilePath (LazyTrackData -> next)
  | ReadTranscodeLazily TrackId TranscodingParameters (LazyTrackData -> next)
  deriving Functor

type Apollo = Free ApolloF

-- | See 'YoutubeDl'.
youtubeDl :: MusicDir -> YoutubeDlUrl -> Apollo [Entry]
youtubeDl x y = liftF $ YoutubeDl x y id

getPlayerStatus :: Apollo PlayerStatus
getPlayerStatus = liftF $ GetPlayerStatus id

enqueueTracks :: [FilePath] -> Apollo [PlaylistItemId]
enqueueTracks tracks = liftF $ EnqueueTracks tracks id

transcodeTrack :: FilePath -> TranscodingParameters -> Apollo TrackId
transcodeTrack track params = liftF $ Transcode track params id

readTrackLazily :: FilePath -> Apollo LazyTrackData
readTrackLazily p = liftF $ ReadTrackLazily p id

readTranscodeLazily :: TrackId -> TranscodingParameters -> Apollo LazyTrackData
readTranscodeLazily t params = liftF $ ReadTranscodeLazily t params id

-- | A lock for the current working directory. Since the CWD is global state,
-- we need to ensure that only one thread changes directories at a time.
newtype DirLock = DirLock (MVar ())

withDirLock :: DirLock -> IO a -> IO a
withDirLock (DirLock d) = withMVar d . const

makeDirLock :: IO DirLock
makeDirLock = DirLock <$> newMVar ()

-- | A lock for MPD access. This lock enforces a sort of transactional
-- processing of MPD actions. The 'interpretApolloIO' interpreter batches all
-- MPD actions performed via 'mpd' into one transaction protected by this lock,
-- so that concurrent web requests will block for MPD access.
newtype MpdLock = MpdLock (MVar ())

withMpdLock :: MpdLock -> IO a -> IO a
withMpdLock (MpdLock d) = withMVar d . const

makeMpdLock :: IO MpdLock
makeMpdLock = MpdLock <$> newMVar ()

-- | Connection settings for MPD.
data MpdSettings
  = MpdSettings
    { mpdHost :: !String
    , mpdPort :: !Integer
    , mpdPassword :: !String
    }

instance Default MpdSettings where
  def = MpdSettings
    { mpdHost = "localhost"
    , mpdPort = 6600
    , mpdPassword = ""
    }

newtype MpdError = MpdError MPD.MPDError deriving Show
instance Exception MpdError

-- | Reads an entire track into memory.
--
-- Precondition: must be in the music directory.
readTrackIO :: FilePath -> IO TrackData -- BS.ByteString
readTrackIO = fmap TrackData . BS.readFile

readTrackLazilyIO :: FilePath -> IO LazyTrackData -- LBS.ByteString
readTrackLazilyIO = fmap LazyTrackData . LBS.readFile

-- | Computes the track ID given track data.
trackId :: TrackData -> TrackId
trackId (TrackData b) = TrackId (sha1 b)

-- | Execute apollo actions in the IO monad.
interpretApolloIO :: MpdSettings -> MpdLock -> DirLock -> Apollo a -> IO a
interpretApolloIO MpdSettings{..} mpdLock dirLock = iterM phi where
  runMpd = MPD.withMPDEx mpdHost mpdPort mpdPassword
  runMpdLockedEx action = withMpdLock mpdLock $ do
    r <- runMpd action
    case r of
      Left e -> throwIO e
      Right x -> pure x

  musicDirP = "music"
  transcodeDirP = "transcoded"
  archiveDirP = "archives"

  inDir d action = withDirLock dirLock $ Dir.withCurrentDirectory d $ do
    putStrLn $ "cwd -> " ++ d
    x <- action
    putStrLn $ "cwd <- " ++ d
    pure x
  inMusicDir = inDir musicDirP
  inTranscodeDir = inDir transcodeDirP
  inArchiveDir = inDir archiveDirP

  phi :: ApolloF (IO a) -> IO a
  phi m = case m of
    YoutubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) k -> do
      let dp = T.unpack musicDirT
      let url = T.unpack dlUrl

      entries <- withTempDirectory "/tmp" "apollo." $ \dirPath -> do
        outputFiles <- withDirLock dirLock $ do
          Dir.withCurrentDirectory dirPath $ do
            Y.youtubeDl url
            Dir.listDirectory "."

        inMusicDir $ do
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

    EnqueueTracks tracks k -> do
      rs <- runMpdLockedEx $ do
        enqueuePos <- fmap (+1) . MPD.stSongPos <$> MPD.status
        for (reverse tracks) $
          \track ->
            PlaylistItemId . (\(MPD.Id i) -> i)
              <$> MPD.addId (fromString track) enqueuePos
      k rs

    Transcode track params k -> do
      -- TODO we read the entire track into memory to compute the hash. This is
      -- wasteful. We could stream the data on disk into the hashing function
      -- to operate in constant memory.
      d <- inMusicDir (readTrackIO track)
      let tid = trackId d
      putStrLn $ "computed track ID " ++ show tid
      mp <- inTranscodeDir (getExistingTranscode tid params)
      putStrLn "got existing transcode"
      _ <- case mp of
        Just transcodePath -> pure (transcodeDirP </> transcodePath)
          -- then the transcode exists, so no work to do.
        Nothing -> do -- no transcode exists, so we have to make one
          let dir = transcodeDirectoryFor tid params
          inTranscodeDir (Dir.createDirectoryIfMissing True dir)
          outFile <- transcode (musicDirP </> track) (transcodeDirP </> dir) params
          pure outFile

      k tid

    ReadTrackLazily track k -> k =<< inMusicDir (readTrackLazilyIO track)

    ReadTranscodeLazily t params k -> do
      d <- inTranscodeDir $ do
        mp <- getExistingTranscode t params
        case mp of
          Just transcodePath -> readTrackLazilyIO transcodePath
          Nothing -> fail "no such transcode"
            -- TODO this needs to translate into a 404
      k d
