{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.Monad

import Control.Monad.IO.Class ( liftIO )
import Servant

server :: MpdSettings -> MpdLock -> DirLock -> Server ApolloApi
server mpdSettings mpdLock dirLock
  = download :<|> playlistEnqueue :<|> status :<|> transcodings where

    apollo = interpretApolloIO mpdSettings mpdLock dirLock

    download :: YoutubeDlReq -> Handler AddedTracks
    download YoutubeDlReq{..} = liftIO . apollo $ do
      createdEntries <- youtubeDl downloadPath downloadUrl
      pure AddedTracks
        { addedTracks = createdEntries
        }

    playlistEnqueue :: [FilePath] -> Handler [PlaylistItemId]
    playlistEnqueue entries = liftIO . apollo $ enqueueTracks entries

    status :: Handler PlayerStatus
    status = liftIO . apollo $ getPlayerStatus

    transcodings = makeTranscode :<|> getTranscode where
      makeTranscode :: TranscodeReq -> Handler TranscodeRes
      makeTranscode TranscodeReq{..} = liftIO . apollo $ do
        trackId <- transcodeTrack transSource transParams
        pure TranscodeRes
          { transTrackId = trackId
          }

      getTranscode :: TrackId -> TranscodingParameters -> Handler LazyTrackData
      getTranscode tid params = liftIO . apollo $ readTranscodeLazily tid params
