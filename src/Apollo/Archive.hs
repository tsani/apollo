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
import System.FilePath ( (</>), (<.>) )

-- | Constructs the path to an archive with the given parameters.
getArchivePath
  :: Compressor -- ^ the compression used for the archive
  -> Maybe FilePath -- ^ the prefix of the path, e.g. @"archive"@.
  -> ArchiveId -- ^ the identifier of the archive
  -> FilePath -- ^ the path to the archive.
getArchivePath c prefix (ArchiveId (Sha1Hash b)) =
  maybe b' (</> b') prefix <.> ("tar" ++ compressExtension c) where
    b' = unpack (decodeUtf8 b)

makeArchiveId :: Traversable t => t ArchiveEntry -> ArchiveId
makeArchiveId =
  ArchiveId . sha1 . encodeUtf8 . pack . show . sort . toList
