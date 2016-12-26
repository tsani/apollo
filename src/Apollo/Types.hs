{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Apollo.Types where

import Data.Aeson
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as C8
import Data.Monoid ( (<>) )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
import Data.Word ( Word16 )
import qualified Network.MPD as MPD
import Servant.API
import System.FilePath ( (</>), FilePath )
import Text.Read ( readMaybe )
import Web.HttpApiData ( FromHttpApiData(..) )

type ApolloApiV1
  =
    "tracks"
      :> "add"
        :> "youtube-dl"
          :> ReqBody '[JSON] YoutubeDlReq :> Post '[JSON] AddedTracks
  :<|>
    "playlist"
      :> "enqueue"
          :> ReqBody '[JSON] [FilePath] :> Post '[JSON] [PlaylistItemId]
  :<|>
    "status"
      :> Get '[JSON] PlayerStatus
  :<|>
    "transcode" :> (
      ReqBody '[JSON] TranscodeReq :> Post '[JSON] TranscodeRes
    :<|>
      Capture "trackId" TrackId
      :> Capture "transcodingParameters" TranscodingParameters
      :> Get '[OctetStream] LazyTrackData
    )

type ApolloApi = "v1" :> ApolloApiV1

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

data AddedTracks
  = AddedTracks
    { addedTracks :: [Entry]
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON AddedTracks where
  toJSON AddedTracks{..} = object [ "addedTracks" .= addedTracks ]

-- | A unique identifier for an item in a playlist.
newtype PlaylistItemId = PlaylistItemId { unPlaylistItemId :: Int }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

-- | A subdirectory of the music directory, typically in the format
-- @Artist/Album@.
newtype MusicDir
  = MusicDir
    { unMusicDir :: Text
    }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

newtype TrackData = TrackData { unTrackData :: ByteString }
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

newtype LazyTrackData = LazyTrackData { unLazyTrackData :: LBS.ByteString }
  deriving (MimeUnrender OctetStream, MimeRender OctetStream)

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

data Entry
  = Entry
    { entryMusicDir :: !MusicDir
    , entryName :: !Text
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON Entry where
  toJSON Entry{..} = String (unMusicDir entryMusicDir <> "/" <> entryName)

entryToPath :: Entry -> FilePath
entryToPath Entry{..} = T.unpack (unMusicDir entryMusicDir) </> T.unpack entryName

newtype YoutubeDlUrl
  = YoutubeDlUrl
    { unYoutubeDlUrl :: Text
    }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)

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

    where nothingLeft msg = maybe (Left msg) Right

instance FromHttpApiData TrackId where
  parseUrlPiece t = Right (TrackId (Sha1Hash (encodeUtf8 t)))

data Format
  = Mp3
  deriving (Eq, Ord, Read, Show)

instance FromJSON Format where
  parseJSON (String s) = case s of
    "mp3" -> pure Mp3
    _ -> fail "unsupported format"
  parseJSON _ = fail "format must be a string"

data Quality
  = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

qualityFromInt :: Int -> Maybe Quality
qualityFromInt n
  | n >= minB && n <= maxB = Just (toEnum n)
  | otherwise = Nothing where
    minB = fromEnum (minBound :: Quality)
    maxB = fromEnum (maxBound :: Quality)

data Bitrate
  = CBR Word16
  | VBR Quality
  deriving (Eq, Ord, Read, Show)

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

data TranscodeRes
  = TranscodeRes
    { transTrackId :: TrackId
    }

instance ToJSON TranscodeRes where
  toJSON TranscodeRes{..} = object [ "trackId" .= toJSON transTrackId ]
