module Apollo.YoutubeDl
( youtubeDl
, youtubeDlProgress
, YoutubeDlSettings(..)
, defaultYoutubeDlSettings
, countTracks
) where

import Apollo.Parser

import Control.Monad.IO.Class
import Data.Maybe ( isNothing )
import System.Process
import System.IO

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

-- | Call youtube-dl in the current directory with flags to produce mp3 files.
-- If a playlist is downloaded, then the numerator and denominator that
-- determine the progress of the batch download will be passed to the given
-- function and the resulting IO action executed.
youtubeDlProgress
  :: MonadIO m
  => YoutubeDlSettings
  -> String
  -> ((Int, Int) -> m ())
  -> m ()
youtubeDlProgress settings url f = do
  (_, Just h, _, p) <- liftIO $ createProcess
    (proc "youtube-dl" (renderSettings settings [url]))
    { std_out = CreatePipe }
  let notEOF = not <$> liftIO (hIsEOF h)
  let isAlive = isNothing <$> liftIO (getProcessExitCode p)
  let traceLine = liftIO (hGetLine h) >>= \l -> liftIO (putStrLn l) *> pure l
  whileM_ ((&&) <$> isAlive <*> notEOF) $ do
    (parseDlProgress <$> traceLine) >>= \case
      -- if we couldn't parse the line, then it must have been different kind
      -- of output line
      Nothing -> pure ()
      Just progress -> f progress

-- | Determines the progress of a batch youtube-dl command by parsing a line of
-- youtube-dl output.
parseDlProgress :: String -> Maybe (Int, Int)
parseDlProgress = fmap snd . parse (go <* eof) where
  go :: Parser (Int, Int)
  go = do
    string_ "[download] Downloading video "
    n <- int
    string_ " of "
    d <- int
    pure (n, d)

-- | Determines how many downloads will be incurred by downloading the given
-- URL.
countTracks :: String -> IO Int
countTracks url = count <$> readProcess "youtube-dl" args "" where
  -- --flat-playlist ensures that we don't download the playlist, and instead
  -- only print out /as if/ we were downloading it.
  -- -s ensures that we don't download a single track if the url points to one.
  args = ["--flat-playlist", "-s", url]
  -- to figure out how many items there are, we break the output of
  -- youtube-dl into words and look for the word "Collected" which
  -- immediately precedes the count.
  count s = case dropWhile ("Collected" /=) $ words s of
    -- "Collected":"104":foo
    (_:cs:_) -> read cs
    -- otherwise, the word "Collected" wasn't found, so the URL does not refer
    -- to a playlist, so only one download will be performed.
    _ -> 1

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ mb ma = mb >>= \case
  True -> ma *> whileM_ mb ma
  False -> pure ()
