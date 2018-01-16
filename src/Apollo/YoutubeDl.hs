module Apollo.YoutubeDl
( youtubeDl
, YoutubeDlSettings(..)
, defaultYoutubeDlSettings
) where

import System.Process ( callProcess )

data YoutubeDlSettings
  = YoutubeDlSettings
    { ytdlAudioFormat :: String
    -- ^ What format to extract audio to.
    -- Default @"mp3"@.
    , ytdlIgnore404 :: Bool
    -- ^ Whether to ignore 404 errors encountered during downloading.
    -- This is especially useful when downloading large playlists that might
    -- contains some links that have become invalid.
    -- Default @True@.
    , ytdlAddMetadata :: Bool
    -- ^ Whether to add metadata.
    -- Default @True@.
    , ytdlExtractAudio :: Bool
    -- ^ Whether to extract the audio from the downloaded resource.
    -- For audio streaming services this typically doesn't have any effect, but
    -- for services that actually stream video (e.g. YouTube) this is
    -- essential.
    -- Default @True@.
    }

defaultYoutubeDlSettings :: YoutubeDlSettings
defaultYoutubeDlSettings = YoutubeDlSettings
  { ytdlAudioFormat = "mp3"
  , ytdlIgnore404 = True
  , ytdlAddMetadata = True
  , ytdlExtractAudio = True
  }

type CommandBuilder = [String] -> [String]

renderSettings :: YoutubeDlSettings -> CommandBuilder
renderSettings YoutubeDlSettings{..} =
  option "--add-metadata" ytdlAddMetadata .
  option "--extract-audio" ytdlExtractAudio .
  argument "--audio-format" ytdlAudioFormat .
  option "-i" ytdlIgnore404 where
    literal s = (++ pure s)
    argument s v = (++ [s, v])
    option s True = literal s
    option _ False = id

-- | Call youtube-dl in the current directory with flags to produce mp3 files
-- in the current working directory.
youtubeDl :: YoutubeDlSettings -> String -> IO ()
youtubeDl settings = callProcess "youtube-dl" . youtubeDlArgs where
  youtubeDlArgs s = (++ pure s) . renderSettings settings $ []
