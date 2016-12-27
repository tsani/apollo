{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.Monad ( Apollo )
import qualified Apollo.Monad as A

import Servant

type ApolloServer = ServerT ApolloApi Apollo

server :: ApolloServer
server = topRoutes where
  topRoutes
    = download
    :<|> playlistEnqueue
    :<|> status
    :<|> transcodings
    :<|> archives

  download :: YoutubeDlReq -> Apollo [Entry]
  download YoutubeDlReq{..} = A.youtubeDl downloadPath downloadUrl

  playlistEnqueue :: [FilePath] -> Apollo [PlaylistItemId]
  playlistEnqueue = A.enqueueTracks

  status :: Apollo PlayerStatus
  status = A.getPlayerStatus

  transcodings = makeTranscode :<|> getTranscode where
    makeTranscode :: TranscodeReq -> Apollo TrackIdW
    makeTranscode TranscodeReq{..} = TrackIdW
      <$> A.transcodeTrack transSource transParams

    getTranscode :: TrackId -> TranscodingParameters -> Apollo LazyTrackData
    getTranscode = A.readTranscodeLazily

  archives = makeArchive :<|> getArchive where
    makeArchive :: [ArchiveEntry] -> Apollo ArchiveIdW
    makeArchive = fmap ArchiveIdW . A.makeArchive

    getArchive :: ArchiveId -> Apollo LazyArchiveData
    getArchive = A.readArchiveLazily
