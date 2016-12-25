{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.Monad

import Control.Concurrent.MVar
import Control.Monad.IO.Class ( liftIO )
import Servant

server :: MVar () -> Server ApolloApi
server dirLock = serveDownload where
  serveDownload :: YoutubeDlReq -> Handler AddedTracks
  serveDownload YoutubeDlReq{..} = liftIO $ do
    interpretApolloIO dirLock $ do
      createdEntries <- youtubeDl downloadPath downloadUrl
      pure AddedTracks
        { addedTracks = createdEntries
        }
