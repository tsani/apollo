{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.Monad

import Control.Monad ( forM )
import Control.Monad.IO.Class ( liftIO )
import Data.String ( fromString )
import qualified Network.MPD as MPD
import Servant

server :: MpdSettings -> MpdLock -> DirLock -> Server ApolloApi
server mpdSettings mpdLock dirLock
  = serveDownload :<|> servePlaylistEnqueue where

    apollo = interpretApolloIO mpdSettings mpdLock dirLock

    serveDownload :: YoutubeDlReq -> Handler AddedTracks
    serveDownload YoutubeDlReq{..} = liftIO . apollo $ do
      createdEntries <- youtubeDl downloadPath downloadUrl
      pure AddedTracks
        { addedTracks = createdEntries
        }

    servePlaylistEnqueue :: [FilePath] -> Handler [PlaylistItemId]
    servePlaylistEnqueue entries = liftIO . apollo . mpd $ do
      posM <- fmap (+1) . MPD.stSongPos <$> MPD.status
      forM (reverse entries) $ \entry -> do
        let p = fromString entry
        PlaylistItemId . (\(MPD.Id i) -> i) <$> MPD.addId p posM
