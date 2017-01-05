module Apollo.Track
( TrackData(..)
, readTrackIO
, LazyTrackData(..)
, readTrackLazilyIO
, LazyArchiveData(..)
, readArchiveLazilyIO
, TrackId(..)
, trackId
) where

import Apollo.Crypto
import Apollo.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

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

