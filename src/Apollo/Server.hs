{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.YoutubeDl

import Control.Concurrent.MVar
import Control.Monad ( forM_ )
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import Servant
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

server :: MVar () -> Server ApolloApi
server dirLock = serveDownload where
  serveDownload :: YoutubeDlReq -> Handler AddedTracks
  serveDownload YoutubeDlReq{..} = liftIO $ do
    createdEntries <- withTempDirectory "/tmp" "apollo." $ \dirPath -> do
      outputFiles <- withMVar dirLock $ \_ -> do
        Dir.withCurrentDirectory dirPath $ do
          youtubeDl (T.unpack downloadUrl)
          Dir.listDirectory "."
      Dir.createDirectoryIfMissing True downloadPath
      forM_ outputFiles $ \outputFile -> do
        Dir.copyFile (dirPath </> outputFile) (downloadPath </> outputFile)
      pure ( (downloadPath </>) <$> outputFiles)
    pure AddedTracks
      { addedTracks = createdEntries
      }
