{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
) where

import Apollo.Types
import Apollo.Monad ( Apollo )
import qualified Apollo.Monad as A

import Data.Default.Class ( def )
import Servant

type ApolloServer = ServerT ApolloApi Apollo

server :: ApolloServer
server = topRoutes where
  topRoutes
    = download
    :<|> playlist
    :<|> status
    :<|> transcodings
    :<|> archives

  download :: YoutubeDlReq -> Apollo [Entry]
  download YoutubeDlReq{..} = A.youtubeDl downloadPath downloadUrl

  playlist = deleteTracks :<|> enqueueTracks :<|> getPlaylist where
    deleteTracks :: [PlaylistItemId] -> Apollo Playlist
    deleteTracks items = A.deleteTracks items *> A.getPlaylist

    enqueueTracks
      :: Maybe PositionBetweenTracks
      -> [FilePath]
      -> Apollo [PlaylistItemId]
    enqueueTracks Nothing = A.enqueueTracks def
    enqueueTracks (Just pos) = A.enqueueTracks pos

    getPlaylist :: Apollo Playlist
    getPlaylist = A.getPlaylist

  status :: Apollo PlayerStatus
  status = A.getPlayerStatus

  transcodings = makeTranscode :<|> getTranscode where
    makeTranscode :: TranscodeReq -> Apollo TrackIdW
    makeTranscode TranscodeReq{..} = TrackIdW
      <$> A.transcodeTrack transSource transParams

    getTranscode :: TrackId -> TranscodingParameters -> Apollo LazyTrackData
    getTranscode = A.readTranscodeLazily

  archives = makeArchive :<|> getArchive where
    makeArchive :: [ArchiveEntry] -> Apollo ArchivalResult
    makeArchive entries = do
      archiveId <- A.makeArchive entries
      archiveUrl <- A.getStaticUrl (StaticArchive archiveId)
      pure ArchivalResult
        { archivalResId = archiveId
        , archivalResUrl = archiveUrl
        }

    getArchive :: ArchiveId -> Apollo LazyArchiveData
    getArchive = A.readArchiveLazily
