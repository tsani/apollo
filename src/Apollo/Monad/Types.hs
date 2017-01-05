{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Apollo.Monad.Types where

import Apollo.Background
  ( JobS, Env, Params, RunnableJob, JobBankVar, Result, UserError )
import Apollo.Types

import Control.Concurrent.MVar
import Control.Monad.Free
import Control.Monad.Except
import Data.Aeson ( ToJSON )
import Data.Default.Class
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Network.MPD as MPD
import Network.URI ( URI )

------------------------------------------------------------------------
--- Core Apollo Monad actions                                        ---
------------------------------------------------------------------------

-- | Base functor for the 'Apollo' monad.
data ApolloF next where
  -- | Call youtube-dl on the given URL and store all generated tracks into the
  -- given musicdir.
  YoutubeDl
    :: MusicDir
    -> YoutubeDlUrl
    -> ([Entry] -> next)
    -> ApolloF next
  -- | Gets the playback status.
  GetPlayerStatus
    :: (PlayerStatus -> next)
    -> ApolloF next
  -- | Enqueues the given tracks for playback after the current playing track,
  -- if any.
  EnqueueTracks
    :: PositionBetweenTracks
    -> [FilePath]
    -> ([PlaylistItemId] -> next)
    -> ApolloF next
  -- | Deletes the given tracks from the playlist.
  DeleteTracks
    :: [PlaylistItemId]
    -> next
    -> ApolloF next
  -- | Gets the playlist.
  GetPlaylist
    :: (Playlist -> next)
    -> ApolloF next
  -- | Transcode a given track with given transcoding parameters.
  -- The unique identifier for the track is returned.
  Transcode
    :: FilePath
    -> TranscodingParameters
    -> (TrackId -> next)
    -> ApolloF next
  StartAsyncJob
    :: (RunnableJob j, ToJSON (Result j), ToJSON (UserError j))
    => JobS j
    -> Env j
    -> Params j
    -> (JobId -> next)
    -> ApolloF next
  QueryAsyncJob
    :: JobId
    -> JobS j
    -> (JobQueryResult -> next)
    -> ApolloF next
  -- | Lazily read track data from the filesystem. This can be used to stream a
  -- track from disk to a network client.
  ReadTrackLazily
    :: FilePath
    -> (LazyTrackData -> next)
    -> ApolloF next
  -- | Lazily read a transcode from the filesystem. This can be used to stream
  -- a transcoded track from disk to a network client.
  ReadTranscodeLazily
    :: TrackId
    -> TranscodingParameters
    -> (LazyTrackData -> next)
    -> ApolloF next
  -- | Lazily read an archive from the filesystem. This can be used to stream
  -- an archive from disk to a network client.
  ReadArchiveLazily
    :: ArchiveId
    -> (LazyArchiveData -> next)
    -> ApolloF next
  -- | Create an archive with the given items.
  MakeArchive
    :: [ArchiveEntry]
    -> (ArchiveId -> next)
    -> ApolloF next
  GetStaticUrl
    :: StaticResource
    -> (Url -> next)
    -> ApolloF next
  -- | The given URI will have its scheme and authority fields set to those of
  -- the Apollo API server and will be converted to a Url.
  GetApiLink
    :: URI
    -> (Url -> next)
    -> ApolloF next
  GetMusicDir
    :: (FilePath -> next)
    -> ApolloF next
  GetTranscodeDir
    :: (FilePath -> next)
    -> ApolloF next

deriving instance Functor ApolloF

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

-- | See 'DeleteTracks'.
deleteTracks :: [PlaylistItemId] -> Apollo ()
deleteTracks items = liftF $ DeleteTracks items ()

-- | See 'GetPlaylist'.
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

-- | See 'ReadArchiveLazily'.
readArchiveLazily :: ArchiveId -> Apollo LazyArchiveData
readArchiveLazily i = liftF $ ReadArchiveLazily i id

-- | See 'MakeArchive'.
makeArchive :: [ArchiveEntry] -> Apollo ArchiveId
makeArchive entries = liftF $ MakeArchive entries id

-- | See 'GetStaticUrl'.
getStaticUrl :: StaticResource -> Apollo Url
getStaticUrl res = liftF $ GetStaticUrl res id

getApiLink :: URI -> Apollo Url
getApiLink u = liftF $ GetApiLink u id

-- | See 'StartAsyncJob'.
startAsyncJob
  :: (RunnableJob j, ToJSON (Result j), ToJSON (UserError j))
  => JobS j -> Env j -> Params j -> Apollo JobId
startAsyncJob j e p = liftF $ StartAsyncJob j e p id

-- | See 'QueryAsyncJob'.
queryAsyncJob :: JobId -> JobS j -> Apollo JobQueryResult
queryAsyncJob i j = liftF $ QueryAsyncJob i j id

getMusicDir :: Apollo FilePath
getMusicDir = liftF $ GetMusicDir id

getTranscodeDir :: Apollo FilePath
getTranscodeDir = liftF $ GetTranscodeDir id

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
data ApolloSettings
  = ApolloSettings
    { apolloApiServerSettings :: ServerSettings
    , apolloStaticServerSettings :: ServerSettings
    , apolloDirLock :: DirLock
    , apolloMpdLock :: MpdLock
    , apolloMpdSettings :: MpdSettings
    , apolloJobBank :: JobBankVar
    }

------------------------------------------------------------------------
--- ApolloIO target monad                                            ---
------------------------------------------------------------------------

-- | Errors that can arise during the IO interpretation of the 'Apollo' monad,
-- 'interpretApolloIO'.
data ApolloError
  = ApolloMpdError MpdError
  | NoSuchTranscode TrackId TranscodingParameters
  | NoSuchJob JobId

-- | Result of 'interpretApolloIO'. This is just an IO monad with some
-- distinguished errors 'ApolloError'.
newtype ApolloIO a
  = ApolloIO
    { unApolloIO :: ExceptT ApolloError IO a
    }
  deriving (Functor, Applicative, Monad, MonadError ApolloError, MonadIO)
