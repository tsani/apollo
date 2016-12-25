{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.YoutubeDl

import Control.Concurrent.MVar
import Control.Monad.IO.Class ( liftIO )
import qualified Data.Text as T
import Servant
import System.Directory ( createDirectoryIfMissing, withCurrentDirectory )

server :: MVar () -> Server ApolloApi
server dirLock = serveDownload where
  serveDownload :: YoutubeDlReq -> Handler ()
  serveDownload YoutubeDlReq{..} = liftIO $ do
    createDirectoryIfMissing True downloadPath
    withMVar dirLock $ \ _ -> withCurrentDirectory downloadPath $ do
      youtubeDl (T.unpack downloadUrl)

