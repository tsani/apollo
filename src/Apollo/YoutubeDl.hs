module Apollo.YoutubeDl where

import System.Process ( callProcess )

-- | Call youtube-dl in the current directory with flags to produce mp3 files
-- in the current working directory.
youtubeDl :: String -> IO ()
youtubeDl = callProcess "youtube-dl" . youtubeDlArgs where
  youtubeDlArgs = (["--extract-audio", "--audio-format", "mp3"] ++) . pure
