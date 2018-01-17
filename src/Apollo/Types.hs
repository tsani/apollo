module Apollo.Types
( YoutubeDlReq(..)
, PlaylistItemId(..)
, MusicDir(..)
, TrackData(..)
, LazyTrackData(..)
, LazyArchiveData(..)
, Sha1Hash(..)
, TrackId(..)
, TrackIdW(..)
, Entry(..)
, YoutubeDlUrl(..)
, urlToString
, PlaybackState(..)
, PlayerStatus(..)
, TranscodingParameters(..)
, Format(..)
, Bitrate(..)
, Quality(..)
, TranscodeReq(..)
, ArchiveId(..)
, ArchivalResult(..)
, ArchiveEntry(..)
, Playlist(..)
, PlaylistEntry(..)
, PlaylistPosition(..)
, PositionBetweenTracks(..)
, Seconds(..)
, NonZero
, nonZero
, Url(..)
, StaticResource(..)
, JobId(..)
, JobQueueResult(..)
, JobQueryResult(..)
, JsonVoid(JsonVoid)
, module Apollo.Types.Job
) where

import Apollo.Types.Job

import Data.Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import Data.Default.Class ( Default(..) )
import Data.List ( intercalate )
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Void
import Data.Word ( Word16 )
import qualified Network.MPD as MPD
import Servant.API
import System.FilePath ( FilePath )
import Text.Read ( readMaybe )
import Web.HttpApiData ( FromHttpApiData(..), ToHttpApiData )

-- | A request to download audio from an external source using @youtube-dl@.
-- The resulting tracks will be stored in a given 'MusicDir'.
data YoutubeDlReq
  = YoutubeDlReq
    { downloadPath :: MusicDir
    , downloadUrl :: YoutubeDlUrl
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON YoutubeDlReq where
  parseJSON (Object v) = YoutubeDlReq
    <$> v .: "path"
    <*> v .: "url"
  parseJSON _ = fail "cannot parse download request from non object"

-- | A unique identifier for an item in a playlist.
newtype PlaylistItemId = PlaylistItemId { unPlaylistItemId :: Int }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

instance FromHttpApiData [PlaylistItemId] where
  parseUrlPiece "" = pure []
  parseUrlPiece (T.breakOn "," -> (h, T.drop 1 -> t))
    = (:) <$> (me $ PlaylistItemId <$> readMaybe (T.unpack h)) <*> parseUrlPiece t where
      me Nothing = Left ("failed to parse playlist item id " <> h)
      me (Just x) = Right x

-- | A subdirectory of the music directory, typically in the format
-- @Artist/Album@.
newtype MusicDir
  = MusicDir
    { unMusicDir :: Text
    }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

-- | A strict bytestring representing audio.
newtype TrackData = TrackData { unTrackData :: ByteString }
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

-- | A lazy bytestring representing audio.
newtype LazyTrackData = LazyTrackData { unLazyTrackData :: LBS.ByteString }
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

-- | A wrapper to identify bytestrings as hashes.
newtype Sha1Hash
  = Sha1Hash
    { unHash :: ByteString
    }
  deriving (Eq, Ord, Read, Show)

-- | SHA-1 hash of the track file. This is used to identify transcodes of the
-- track.
newtype TrackId
  = TrackId
    { unTrackId :: Sha1Hash
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON TrackId where
  toJSON (TrackId (Sha1Hash b)) = String (T.pack (C8.unpack b))

instance FromJSON TrackId where
  parseJSON (String s) = pure $ TrackId (Sha1Hash (encodeUtf8 s))
  parseJSON _ = fail "cannot parse trackId from non-object"

data Entry
  = Entry
    { entryMusicDir :: !MusicDir
    , entryName :: !Text
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON Entry where
  toJSON Entry{..} = String (unMusicDir entryMusicDir <> "/" <> entryName)

-- -- | Converts an entry into a path relative to the music directory.
-- entryToPath :: Entry -> FilePath
-- entryToPath Entry{..}
--   = T.unpack (unMusicDir entryMusicDir) </> T.unpack entryName

-- | A URL to be given to @youtube-dl@ for downloading.
newtype YoutubeDlUrl
  = YoutubeDlUrl
    { unYoutubeDlUrl :: Text
    }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)

urlToString :: YoutubeDlUrl -> String
urlToString = T.unpack . unYoutubeDlUrl

-- | Wraps 'MPD.State' so modules that import this one won't need to import
-- libmpd modules.
newtype PlaybackState
  = PlaybackState
    { unPlaybackState :: MPD.State
    }
  deriving (Eq, Show)

instance ToJSON PlaybackState where
  toJSON (PlaybackState s) = String $ case s of
    MPD.Playing -> "playing"
    MPD.Stopped -> "stopped"
    MPD.Paused -> "paused"

instance FromJSON PlaybackState where
  parseJSON (String s) = case s of
    "playing" -> pure $ PlaybackState MPD.Playing
    "stopped" -> pure $ PlaybackState MPD.Stopped
    "paused" -> pure $ PlaybackState MPD.Paused
    _ -> fail "invalid playback state"
  parseJSON _ = fail "cannot encode non-string as playback state"

-- | The status of the audio player.
data PlayerStatus
  = PlayerStatus
    { psState :: PlaybackState
    , psPlaylistLength :: Integer
    , psTrackId :: Maybe PlaylistItemId
    , psNextTrackId :: Maybe PlaylistItemId
    , psUptime :: Integer
    , psPlaytime :: Integer
    , psLastUpdateTime :: Integer
    }
  deriving (Eq, Show)

instance ToJSON PlayerStatus where
  toJSON PlayerStatus{..} = object
    [ "state" .= toJSON psState
    , "playlistLength" .= toJSON psPlaylistLength
    , "trackId" .= toJSON psTrackId
    , "nextTrackId" .= toJSON psNextTrackId
    , "uptime" .= toJSON psUptime
    , "playtime" .= toJSON psPlaytime
    , "lastDbUpdate" .= toJSON psLastUpdateTime
    ]

data TranscodingParameters
  = TranscodingParameters
    { transFormat :: Format
    , transBitrate :: Bitrate
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON TranscodingParameters where
  toJSON TranscodingParameters{..} = object
    [ "format" .= transFormat
    , "bitrate" .= transBitrate
    ]

instance FromJSON TranscodingParameters where
  parseJSON (Object o) = TranscodingParameters
    <$> o .: "format"
    <*> o .: "bitrate"
  parseJSON _ = fail "cannot parse transcoding parameters from non-object"

instance FromHttpApiData TranscodingParameters where
  parseUrlPiece t = do
    let (formatStr, bitrateStr) = T.drop 1 <$> T.breakOn "-" t
    format <- case formatStr of
      "mp3" -> pure Mp3
      _ -> Left "unsupported format"

    let (brTypeStr, brValueStr) = T.unpack . T.drop 1 <$> T.breakOn "-" bitrateStr
    bitrate <- case brTypeStr of
      "vbr" -> do
        n <- nothingLeft "failed to parse VBR value" $ readMaybe brValueStr
        q <- nothingLeft "invalid VBR value" $ qualityFromInt n
        pure (VBR q)
      "cbr" -> do
        n <- nothingLeft "failed to parse CBR value" $ readMaybe brValueStr
        pure (CBR n)
      _ -> Left "unknown bitrate type"

    pure TranscodingParameters
      { transFormat = format
      , transBitrate = bitrate
      }

    where
      -- A natural transformation from 'Maybe' to 'Either String'.
      nothingLeft msg = maybe (Left msg) Right

instance FromHttpApiData TrackId where
  parseUrlPiece t = Right (TrackId (Sha1Hash (encodeUtf8 t)))

data Format
  = Mp3
  deriving (Eq, Ord, Read, Show)

instance ToJSON Format where
  toJSON = \case
    Mp3 -> String "mp3"

instance FromJSON Format where
  parseJSON (String s) = case s of
    "mp3" -> pure Mp3
    _ -> fail "unsupported format"
  parseJSON _ = fail "format must be a string"

-- | Specifies a bitrate.
data Bitrate
  = CBR Word16
  -- ^ A constant bitrate.
  | VBR Quality
  -- ^ A variable bitrate.
  deriving (Eq, Ord, Read, Show)

-- | A measure of audio quality used for VBR encodings.
data Quality
  = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- | Convert a quality to an integer.
qualityToInt :: Quality -> Int
qualityToInt = fromEnum

-- | Convert an integer to a quality.
qualityFromInt :: Int -> Maybe Quality
qualityFromInt n
  | n >= minB && n <= maxB = Just (toEnum n)
  | otherwise = Nothing where
    minB = fromEnum (minBound :: Quality)
    maxB = fromEnum (maxBound :: Quality)

instance ToJSON Bitrate where
  toJSON = \case
    CBR x -> object
      [ "type" .= id @Text "cbr"
      , "value" .= x
      ]
    VBR x -> object
      [ "type" .= id @Text "vbr"
      , "value" .= qualityToInt x
      ]

instance FromJSON Bitrate where
  parseJSON (Object o) = do
    t <- o .: "type"
    n <- o .: "value"
    case t :: String of
      "cbr" -> do
        pure (CBR n)
      "vbr" -> case qualityFromInt (fromIntegral n) of
        Just q -> pure (VBR q)
        Nothing -> fail "VBR value out of range"
      _ -> fail "unsupported bitrate type"
  parseJSON _ = fail "cannot parse bitrate from non-object"

-- | A request to transcode a track with some parameters.
data TranscodeReq
  = TranscodeReq
    { transSource :: FilePath
    , transParams :: TranscodingParameters
    }

instance FromJSON TranscodeReq where
  parseJSON (Object o) = TranscodeReq
    <$> o .: "source"
    <*> o .: "params"
  parseJSON _ = fail "cannot parse transcoding request from non-object"

-- | A 'TrackId' that (de)serializes to a JSON object.
newtype TrackIdW
  = TrackIdW
    { transTrackId :: TrackId
    }

instance ToJSON TrackIdW where
  toJSON TrackIdW{..} = object [ "trackId" .= toJSON transTrackId ]

newtype ArchiveId
  = ArchiveId Sha1Hash
  deriving (Eq, Ord, Read, Show)

instance FromHttpApiData ArchiveId where
  parseUrlPiece t = Right (ArchiveId (Sha1Hash (encodeUtf8 t)))

instance ToJSON ArchiveId where
  toJSON (ArchiveId (Sha1Hash b)) = String (decodeUtf8 b)

-- | Wrapper to serialize 'ArchiveId' as an object instead of as a string.
data ArchivalResult
  = ArchivalResult
    { archivalResId :: ArchiveId
    , archivalResUrl :: Url
    }

instance ToJSON ArchivalResult where
  toJSON ArchivalResult{..} = object
    [ "archiveId" .= archivalResId
    , "archiveUrl" .= archivalResUrl
    ]

newtype LazyArchiveData
  = LazyArchiveData LBS.ByteString
  deriving
    ( Eq, Ord, Read, Show
    , MimeRender OctetStream, MimeUnrender OctetStream
    )

-- | An item to archive.
data ArchiveEntry
  -- | Archive a raw track.
  = ArchiveTrack FilePath
  -- | Archive a transcode.
  | ArchiveTranscode TrackId TranscodingParameters
  deriving (Eq, Ord, Read, Show)

data ArchiveEntryType
  = ArchiveEntryTrackType
  | ArchiveEntryTranscodeType

instance FromJSON ArchiveEntryType where
  parseJSON (String s) = case s of
    "track" -> pure ArchiveEntryTrackType
    "transcode" -> pure ArchiveEntryTranscodeType
    _ -> fail "unknown archive entry type"
  parseJSON _ = fail "cannot parse archive entry type from non-string"

instance FromJSON ArchiveEntry where
  parseJSON (Object o) = do
    ty <- o .: "type"
    case ty of
      ArchiveEntryTrackType ->
        ArchiveTrack <$> (T.unpack <$> o .: "trackPath")
      ArchiveEntryTranscodeType ->
        ArchiveTranscode
          <$> o .: "trackId"
          <*> o .: "transParams"
  parseJSON _ = fail "cannot parse archive entry from non-object"

data Playlist
  = Playlist
    { playlistTracks :: [PlaylistEntry]
    , playlistNowPlaying :: Maybe PlaylistItemId
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON Playlist where
  toJSON Playlist{..} = object
    [ "tracks" .= playlistTracks
    , "nowPlaying" .= playlistNowPlaying
    ]

data PlaylistEntry
  = PlaylistEntry
    { entryPath :: FilePath
    , entryId :: PlaylistItemId
    , entryPosition :: PlaylistPosition
    , entryDuration :: Seconds
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON PlaylistEntry where
  toJSON PlaylistEntry{..} = object
    [ "id" .= entryId
    , "path" .= entryPath
    , "duration" .= entryDuration
    , "position" .= entryPosition
    ]

-- | Represents a position between tracks, for specifying where tracks ought to
-- be inserted in the playlist.
--
-- Each relative mark in the playlist is offset by a nonzero integer. We omit
-- zero because it feels especially arbitrary to say that inserting a track
-- /zero/ tracks before or after another should mean that the inserted track
-- comes before or after.
--
-- Hence, @FromBeginning 1@ refers to the position after the first track in the
-- playlist, but before the second, and @FromBeginning (-1)@ refers to the
-- position immediately before the first track in the playlist.
data PositionBetweenTracks
  -- | A position relative to the beginning of the playlist.
  --
  -- All negative values are equivalent in this constructor.
  = FromBeginning NonZero
  -- | A position relative to the end of the playlist.
  --
  -- All positive values are equivalent in this constructor.
  | FromEnd NonZero
  -- | A position relative to the currently playing track.
  | FromPlaying NonZero
  deriving (Eq, Ord, Read, Show)

instance Default PositionBetweenTracks where
  def = FromPlaying def

instance FromHttpApiData PositionBetweenTracks where
  parseUrlPiece = parse . T.breakOn "_" where
    parse (markStr, T.drop 1 -> offsetStr)
      = parseMark markStr <*> parseOffset offsetStr

    parseMark s = case s of
      "start" -> Right FromBeginning
      "end" -> Right FromEnd
      "playing" -> Right FromPlaying
      _ -> Left $ T.pack $ intercalate " "
        [ "unknown relative position"
        , T.unpack s
        , "- valid positions are:"
        , intercalate ", " ["start", "end", "playing"]
        ]

    parseOffset s = case readMaybe (T.unpack s) >>= nonZeroFromInt of
      Just n -> Right n
      Nothing -> Left "failed to parse nonzero number"

newtype NonZero = NonZero { nonZero :: Int }
  deriving (Eq, Ord, Read, Show)

instance Default NonZero where
  def = NonZero 1

nonZeroFromInt :: Int -> Maybe NonZero
nonZeroFromInt 0 = Nothing
nonZeroFromInt n = Just (NonZero n)

newtype PlaylistPosition
  = PlaylistPosition Int
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

newtype Seconds
  = Seconds Integer
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

data StaticResource
  = StaticArchive ArchiveId
  | StaticTranscode TrackId TranscodingParameters
  | StaticTrack FilePath
  deriving (Eq, Ord, Read, Show)

newtype Url
  = Url
    { unUrl :: T.Text
    }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

newtype JobId = JobId Int
  deriving
    ( Eq
    , Ord
    , Read
    , Show
    , Enum
    , FromHttpApiData
    , ToHttpApiData
    )

instance ToJSON JobId where
  toJSON (JobId n) = toJSON (show n)

instance FromJSON JobId where
  parseJSON (String s) = case readMaybe (T.unpack s) of
    Just x -> pure x
    Nothing -> fail "failed to parse JobId"
  parseJSON _ = fail "cannot parse JobId from non-string"

instance Bounded JobId where
  minBound = JobId 0
  maxBound = JobId maxBound

data JobQueueResult
  = JobQueueResult
    { jobQueueId :: JobId
    , jobQueueQueryUrl :: Url
    }

instance ToJSON JobQueueResult where
  toJSON JobQueueResult{..} = object
    [ "jobId" .= jobQueueId
    , "query" .= jobQueueQueryUrl
    ]

newtype JobQueryResult e r = JobQueryResult (JobInfo e r)

instance (ToJSON e, ToJSON r) => ToJSON (JobQueryResult e r) where
  toJSON (JobQueryResult s) = case s of
    JobInProgress p -> object
      [ "status" .= ("running" :: Text)
      , "progress" .= JsonJobProgress p
      ]
    JobFailed e -> object
      [ "status" .= ("failed" :: Text)
      , "error" .= e
      ]
    JobComplete x -> object
      [ "status" .= ("complete" :: Text)
      , "result" .= x
      ]

newtype JsonJobProgress = JsonJobProgress Progress

instance ToJSON JsonJobProgress where
  toJSON (JsonJobProgress (Progress n d)) = object
    [ "done" .= n
    , "outOf" .= d
    ]

newtype JsonVoid = JsonVoid Void

instance ToJSON JsonVoid where
  toJSON (JsonVoid v) = absurd v
