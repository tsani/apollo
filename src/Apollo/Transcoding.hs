{-# LANGUAGE RecordWildCards #-}

module Apollo.Transcoding
( mp3Format
, kbps192
, v2
, Bitrate
, Format
, TranscodingParameters(..)
, getExistingTranscode
, transcodeDirectoryFor
, transcode
) where

import Apollo.Types

import Data.ByteString.Char8 ( unpack )
import System.Directory ( doesDirectoryExist, listDirectory )
import System.FilePath ( (</>), (-<.>), replaceDirectory )
import System.Process ( callProcess )

mp3Format :: Format
mp3Format = Mp3

kbps192 :: Bitrate
kbps192 = CBR 192

v2 :: Bitrate
v2 = VBR Q2

-- | Converts transcoding parameters into a path component. This path component
-- is used as a subdirectory within the track ID directory to identify an
-- available transcode for the given track.
transcodingParametersToPathComponent :: TranscodingParameters -> FilePath
transcodingParametersToPathComponent TranscodingParameters{..}
  = concat [formatComponent, "-", bitrateComponent] where
    formatComponent = case transFormat of
      Mp3 -> "mp3"
    bitrateComponent = case transBitrate of
      CBR w -> show w ++ "kbps"
      VBR q -> "v" ++ show (fromEnum q)

-- | Checks whether a transcode of a given transcode exists for the track with
-- these parameters.
--
-- Returns 'Nothing' if the transcode does not exist. Else, returns the path
-- relative to the transcode directory where the transcode may be found.
--
-- The @Maybe FilePath@ parameter is a prefix to apply to computed paths to
-- get into the transcode directory. If it is 'Nothing', then we assume that we
-- are in the transcode directory.
getExistingTranscode
  :: Maybe FilePath
  -> TrackId
  -> TranscodingParameters
  -> IO (Maybe FilePath)
getExistingTranscode prefix tid tsp
  = do
    t <- doesDirectoryExist dir
    if t
    then do
      mf <- listDirectory dir
      pure $ case mf of
        [] -> Nothing
        (f:_) -> Just (dir </> f)
    else pure Nothing
  where
    dir = let p = transcodeDirectoryFor tid tsp in maybe p (</> p) prefix

-- | Computes the relative path to the directory within the transcode directory
-- where the transcoded audio of the given track ID ought to be stored.
transcodeDirectoryFor :: TrackId -> TranscodingParameters -> FilePath
transcodeDirectoryFor (TrackId (Sha1Hash b)) tsp = idPC </> tspPC where
  idPC = unpack b
  tspPC = transcodingParametersToPathComponent tsp

-- | Transcodes a file with the given parameters.
--
-- /Remark:/ the output parameter is a /directory/. The output filename is
-- computed from the desired output encoding and the input filename.
--
-- Generally, the output directory should be obtained from
-- 'transcodeDirectoryFor'.
--
-- Returns the path to the output file, i.e. the given output directory plus
-- the computed filename.
transcode
  :: FilePath -- ^ input file path
  -> FilePath -- ^ output directory
  -> TranscodingParameters
  -> IO FilePath
transcode inp out TranscodingParameters{..}
  = callProcess "ffmpeg" params *> pure outPath where
    params = [ "-i", inp ] ++ outBitrate ++ [ outPath ]
    outPath = replaceDirectory inp out -<.> ext
    ext = case transFormat of
      Mp3 -> "mp3"
    outBitrate = case transBitrate of
      CBR w -> [ "-b:a", show w ++ "k" ]
      VBR q -> [ "-q:a", show (fromEnum q) ]
