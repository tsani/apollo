{-|
 - Functions for constructing archives of raw and transcoded tracks.
 -}

{-# LANGUAGE ViewPatterns #-}

module Apollo.Archive where

import Apollo.Crypto ( sha1 )
import Apollo.Types

import Data.List ( sort )
import Data.Text ( pack )
import Data.Text ( unpack )
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Foldable ( toList )
import qualified System.Directory as Dir
import System.FilePath ( (</>) )

makeArchiveId :: Traversable t => t ArchiveEntry -> ArchiveId
makeArchiveId (toList -> sort -> entries) = ArchiveId h where
  h = sha1 $ encodeUtf8 $ pack $ show entries

getExistingArchive :: Maybe FilePath -> ArchiveId -> IO (Maybe FilePath)
getExistingArchive prefix (ArchiveId (Sha1Hash b)) = do
  let p = maybe (unpack $ decodeUtf8 b) (</> unpack (decodeUtf8 b)) prefix
  d <- Dir.doesFileExist p
  pure $ if d
    then (Just p)
    else Nothing
