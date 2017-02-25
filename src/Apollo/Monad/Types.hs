{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Apollo.Monad.Types where

import Apollo.Types

import Control.Concurrent.MVar
import Control.Monad.Free
import Control.Monad.Except
import Data.Aeson ( ToJSON(..), object, (.=) )
import Data.Default.Class
import Data.IORef
import Data.List.NonEmpty ( NonEmpty )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Network.MPD as MPD
import Network.URI ( URI )

------------------------------------------------------------------------
--- Core Apollo Monad actions                                        ---
------------------------------------------------------------------------

-- | Base functor for the 'Apollo' monad.
data ApolloF k e a next where
  -- | Call youtube-dl on the given URL and store all generated tracks into the
  -- given musicdir.
  YoutubeDl
    :: MusicDir
    -> YoutubeDlUrl
    -> ([Entry] -> next)
    -> ApolloF k e a next
  -- | Gets the playback status.
  GetPlayerStatus
    :: (PlayerStatus -> next)
    -> ApolloF k e a next
  -- | Enqueues the given tracks for playback after the current playing track,
  -- if any.
  EnqueueTracks
    :: PositionBetweenTracks
    -> [FilePath]
    -> ([PlaylistItemId] -> next)
    -> ApolloF k e a next
  -- | Deletes the given tracks from the playlist.
  DeleteTracks
    :: [PlaylistItemId]
    -> next
    -> ApolloF k e a next
  -- | Gets the playlist.
  GetPlaylist
    :: (Playlist -> next)
    -> ApolloF k e a next
  -- | Transcode a given track with given transcoding parameters.
  -- The unique identifier for the track is returned.
  Transcode
    :: FilePath
    -> TranscodingParameters
    -> (TrackId -> next)
    -> ApolloF k e a next
  StartAsyncJob
    :: Progress
    -> Job e a
    -> (k -> next)
    -> ApolloF k e a next
  QueryAsyncJob
    :: k
    -> (JobInfo e a -> next)
    -> ApolloF k e a next
  -- | Lazily read track data from the filesystem. This can be used to stream a
  -- track from disk to a network client.
  ReadTrackLazily
    :: FilePath
    -> (LazyTrackData -> next)
    -> ApolloF k e a next
  -- | Lazily read a transcode from the filesystem. This can be used to stream
  -- a transcoded track from disk to a network client.
  ReadTranscodeLazily
    :: TrackId
    -> TranscodingParameters
    -> (LazyTrackData -> next)
    -> ApolloF k e a next
  -- | Lazily read an archive from the filesystem. This can be used to stream
  -- an archive from disk to a network client.
  ReadArchiveLazily
    :: ArchiveId
    -> (LazyArchiveData -> next)
    -> ApolloF k e a next
  -- | Create an archive with the given items.
  MakeArchive
    :: NonEmpty ArchiveEntry
    -> (ArchiveId -> next)
    -> ApolloF k e a next
  GetStaticUrl
    :: StaticResource
    -> (Url -> next)
    -> ApolloF k e a next
  -- | The given URI will have its scheme and authority fields set to those of
  -- the Apollo API server and will be converted to a Url.
  GetApiLink
    :: URI
    -> (Url -> next)
    -> ApolloF k e a next
  GetMusicDir
    :: (FilePath -> next)
    -> ApolloF k e a next
  GetTranscodeDir
    :: (FilePath -> next)
    -> ApolloF k e a next
  GetArchiveDir
    :: (FilePath -> next)
    -> ApolloF k e a next

deriving instance Functor (ApolloF k e a)

-- | The Apollo free monad.
type Apollo k e a = Free (ApolloF k e a)

-- | See 'YoutubeDl'.
youtubeDl :: MusicDir -> YoutubeDlUrl -> Apollo k e a [Entry]
youtubeDl x y = liftF $ YoutubeDl x y id

-- | See 'GetPlayerStatus'.
getPlayerStatus :: Apollo k e a PlayerStatus
getPlayerStatus = liftF $ GetPlayerStatus id

-- | See 'EnqueueTracks'.
enqueueTracks
  :: PositionBetweenTracks
  -> [FilePath]
  -> Apollo k e a [PlaylistItemId]
enqueueTracks pos tracks = liftF $ EnqueueTracks pos tracks id

-- | See 'DeleteTracks'.
deleteTracks :: [PlaylistItemId] -> Apollo k e a ()
deleteTracks items = liftF $ DeleteTracks items ()

-- | See 'GetPlaylist'.
getPlaylist :: Apollo k e a Playlist
getPlaylist = liftF $ GetPlaylist id

-- | See 'Transcode'.
transcodeTrack :: FilePath -> TranscodingParameters -> Apollo k e a TrackId
transcodeTrack track params = liftF $ Transcode track params id

-- | See 'ReadTrackLazily'.
readTrackLazily :: FilePath -> Apollo k e a LazyTrackData
readTrackLazily p = liftF $ ReadTrackLazily p id

-- | See 'ReadTranscodeLazily'.
readTranscodeLazily
  :: TrackId
  -> TranscodingParameters
  -> Apollo k e a LazyTrackData
readTranscodeLazily t params = liftF $ ReadTranscodeLazily t params id

-- | See 'ReadArchiveLazily'.
readArchiveLazily :: ArchiveId -> Apollo k e a LazyArchiveData
readArchiveLazily i = liftF $ ReadArchiveLazily i id

-- | See 'MakeArchive'.
makeArchive :: NonEmpty ArchiveEntry -> Apollo k e a ArchiveId
makeArchive entries = liftF $ MakeArchive entries id

-- | See 'GetStaticUrl'.
getStaticUrl :: StaticResource -> Apollo k e a Url
getStaticUrl res = liftF $ GetStaticUrl res id

getApiLink :: URI -> Apollo k e a Url
getApiLink u = liftF $ GetApiLink u id

-- | See 'StartAsyncJob'.
startAsyncJob
  :: Progress
  -> Job e a
  -> Apollo k e a k
startAsyncJob p m = liftF $ StartAsyncJob p m id

-- | See 'QueryAsyncJob'.
queryAsyncJob
  :: k
  -> Apollo k e a (JobInfo e a)
queryAsyncJob i = liftF $ QueryAsyncJob i id

getMusicDir :: Apollo k e a FilePath
getMusicDir = liftF $ GetMusicDir id

getTranscodeDir :: Apollo k e a FilePath
getTranscodeDir = liftF $ GetTranscodeDir id

getArchiveDir :: Apollo k e a FilePath
getArchiveDir = liftF $ GetArchiveDir id

------------------------------------------------------------------------
--- Derived actions                                                  ---
------------------------------------------------------------------------

-- | Essentially 'forM' but as a 'Job' that updates the progress after each
-- item processed. The length of the traversable to process must be specified.
-- If you're just going to use 'length' provided by 'Traversable', then use
-- 'forAsync' which does this for you.
forUpdateLen :: Traversable t => Int -> t x -> (x -> Job e a) -> Job e (t a)
forUpdateLen n xs f = do
  v <- liftIO (newIORef 0)
  forM xs $ \x -> do
    y <- f x
    i <- liftIO $ atomicModifyIORef v (\i -> (i + 1, i))
    reportProgress (Progress i n)
    pure y

-- | Essentially 'forM' but as a 'Job' that updates the progress after each
-- item processed. The
forUpdate :: Traversable t => t x -> (x -> Job e a) -> Job e (t a)
forUpdate xs = forUpdateLen (length xs) xs

startBatchAsyncJob :: Traversable t => t x -> Job e a -> Apollo k e a k
startBatchAsyncJob xs = startAsyncJob (Progress 0 (length xs))

------------------------------------------------------------------------
--- Locks & Settings                                                 ---
------------------------------------------------------------------------

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

-- | Settings of a server.
data ServerSettings
  = ServerSettings
    { serverDomain :: Text
    , serverScheme :: Text
    , serverPort :: Int
    }

-- | Converts server settings into a /base/ URL onto which a path and query
-- string may be appended to obtain a /full/ URL.
serverSettingsBaseUrl :: ServerSettings -> Url
serverSettingsBaseUrl ServerSettings{..} = Url u where
  u = serverScheme <> "://" <> serverDomain <> ":" <> T.pack (show serverPort)

-- | The overall configuration for the 'ApolloIO' interpretation of 'Apollo'.
data ApolloSettings k e a
  = ApolloSettings
    { apolloApiServerSettings :: ServerSettings
    , apolloStaticServerSettings :: ServerSettings
    , apolloDirLock :: DirLock
    , apolloMpdLock :: MpdLock
    , apolloMpdSettings :: MpdSettings
    , apolloJobBank :: MVar (JobBank k e a)
    }

------------------------------------------------------------------------
--- ApolloIO target monad                                            ---
------------------------------------------------------------------------

-- | Errors that can arise during the IO interpretation of the 'Apollo' monad,
-- 'runApolloIO'.
data ApolloError k
  = ApolloMpdError MpdError
  | NoSuchTranscode TrackId TranscodingParameters
  | NoSuchJob k
  | WrongAsyncResult
    String -- ^ expected
    String -- ^ actual

instance ToJSON k => ToJSON (ApolloError k) where
  toJSON = \case
    ApolloMpdError e -> object
      [ "type" .= (id @Text "mpd")
      , "error" .= show e
      ]

    NoSuchTranscode tid params -> object
      [ "type" .= (id @Text "transcode not found")
      , "trackId" .= tid
      , "params" .= params
      ]

    NoSuchJob k -> object
      [ "type" .= (id @Text "job not found")
      , "jobId" .= k
      ]

    WrongAsyncResult expected actual -> object
      [ "type" .= (id @Text "wrong async result")
      , "message" .= (id @Text "an async job completed with an unexpected type")
      , "expected" .= expected
      , "actual" .= actual
      ]

-- | Result of 'runApolloIO'. This is just an IO monad with some
-- distinguished errors 'ApolloError'.
newtype ApolloIO k a
  = ApolloIO
    { unApolloIO :: ExceptT (ApolloError k) IO a
    }
  deriving (Functor, Applicative, Monad, MonadError (ApolloError k), MonadIO)
