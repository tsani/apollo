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
import Apollo.YoutubeDl

import Control.Concurrent.MVar
import Control.Monad.Except
import Control.Monad.Trans.Control
import Data.Aeson ( ToJSON(..), object, (.=) )
import Data.Default.Class
import Data.IORef
import Data.List.NonEmpty ( NonEmpty )
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Network.MPD as MPD
import Network.URI ( URI )

------------------------------------------------------------------------
--- Core Apollo Monad actions                                        ---
------------------------------------------------------------------------

-- | An Apollo monad can control asynchronous jobs, runs in the IO monad, and
-- can perform a number of distinguished actions.
class (MonadJobControl m, MonadBaseControl IO m) => MonadApollo m where
  -- | Call youtube-dl on the given URL and store all the generated tracks into
  -- the given music directory.
  youtubeDl
    :: MusicDir -- ^ directory into which to place the downloaded files
    -> YoutubeDlUrl -- ^ url to download
    -> YoutubeDlSettings -- ^ settings for the download
    -> ((Int, Int) -> m ()) -- ^ action to perform on progress updates
    -> m (NonEmpty Entry)

  -- | Gets the playback status.
  getPlayerStatus :: m PlayerStatus

  -- | Enqueues the given tracks for playback after the current playing track,
  -- if any.
  enqueueTracks
    :: PositionBetweenTracks -> NonEmpty FilePath -> m (NonEmpty PlaylistItemId)

  -- | Deletes the given tracks from the playlist.
  deleteTracks
    :: [PlaylistItemId]
    -> m ()

  -- | Gets the current playlist.
  getPlaylist :: m Playlist

  -- | Transcodes a given track with given transcoding parameters.
  -- The unique identifier for the track is returned.
  makeTranscode :: FilePath -> TranscodingParameters -> m TrackId

  -- | Reads a track lazily.
  readTrackLazily :: FilePath -> m LazyTrackData

  -- | Reads a transcode lazily.
  -- A transcode is identified by its sourc track id and its parameters.
  readTranscodeLazily :: TrackId -> TranscodingParameters -> m LazyTrackData

  -- | Reads an archive lazily.
  readArchiveLazily :: ArchiveId -> m LazyArchiveData

  -- | Constructs an archive with the given entries.
  makeArchive :: Compressor -> NonEmpty ArchiveEntry -> m ArchiveId

  -- | Gets a URL to a static resource such as an archive.
  getStaticUrl :: StaticResource -> m Url

  getApiLink :: URI -> m Url

-- getMusicDir :: Apollo k e a FilePath
-- getMusicDir = liftF $ GetMusicDir id
--
-- getTranscodeDir :: Apollo k e a FilePath
-- getTranscodeDir = liftF $ GetTranscodeDir id
--
-- getArchiveDir :: Apollo k e a FilePath
-- getArchiveDir = liftF $ GetArchiveDir id

------------------------------------------------------------------------
--- Derived actions                                                  ---
------------------------------------------------------------------------

-- | Essentially 'forM' but as a 'Job' that updates the progress after each
-- item processed. The length of the traversable to process must be specified.
-- If you're just going to use 'length' provided by 'Traversable', then use
-- 'forAsync' which does this for you.
forUpdateLen :: Traversable t => Int -> t x -> (x -> Job e a) -> Job e (t a)
forUpdateLen n xs f = do
  -- we're forced to use IORefs here instead of just zipping with [1..] beacuse
  -- the container is an arbitrary Traversable and I can't figure out a nice
  -- way to achieve the zipping.
  v <- liftIO (newIORef 0)
  forM xs $ \x -> do
    y <- f x
    i <- liftIO $ atomicModifyIORef v (\i -> (i + 1, i))
    reportProgress (Progress i n)
    pure y

-- | Essentially 'forM' but as a 'Job' that updates the progress after each
-- item processed.
forUpdate :: Traversable t => t x -> (x -> Job e a) -> Job e (t a)
forUpdate xs = forUpdateLen (length xs) xs

------------------------------------------------------------------------
--- Locks & Settings & Errors                                        ---
------------------------------------------------------------------------

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
    , apolloMpdLock :: MpdLock
    , apolloMpdSettings :: MpdSettings
    , apolloJobBank :: MVar (JobBank k e a)
    , apolloMusicDirP :: FilePath
    , apolloTranscodeDirP :: FilePath
    , apolloArchiveDirP :: FilePath
    , apolloTmpDir :: FilePath
    }

-- | Errors that can arise during the IO interpretation of the 'Apollo' monad,
-- 'runApolloIO'.
data ApolloError k
  -- | When an internal MPD error occurs, we just forward it along.
  = ApolloMpdError MpdError

  -- | A transcode was requested, but it could not be located.
  | NoSuchTranscode TrackId TranscodingParameters

  -- | A job was requested, but it could not be located.
  | NoSuchJob k

  -- | An async result was found, but it was of the wrong type.
  | WrongAsyncResult
    String
    String

  -- | A youtube-dl subprocess produced no output files.
  | EmptyYoutubeDlResult

  -- | A subprocess failed unexpectedly.
  | SubprocessDied String

  -- | An unknown error occurred.
  -- (Used to implement a MonadFail instance.)
  | Failure String

instance ToJSON k => ToJSON (ApolloError k) where
  toJSON = \case
    ApolloMpdError e -> object
      [ "type" .= (id @Text "mpd")
      , "message" .= show e
      ]

    NoSuchTranscode tid params -> object
      [ "type" .= (id @Text "transcode not found")
      , "message" .= (id @Text "no such transcode")
      , "trackId" .= tid
      , "params" .= params
      ]

    NoSuchJob k -> object
      [ "type" .= (id @Text "job not found")
      , "message" .= (id @Text "no such job")
      , "jobId" .= k
      ]

    WrongAsyncResult expected actual -> object
      [ "type" .= (id @Text "wrong async result")
      , "message" .= (id @Text "an async job completed with an unexpected type")
      , "expected" .= expected
      , "actual" .= actual
      ]

    EmptyYoutubeDlResult -> object
      [ "type" .= id @Text "empty youtube-dl result"
      , "message" .= id @Text "youtube-dl subprocess produced no output files"
      ]

    SubprocessDied msg -> object
      [ "type" .= id @Text "subprocess died"
      , "message" .= msg
      ]

    Failure msg -> object
      [ "type" .= id @Text "unknown error"
      , "message" .= msg
      ]
