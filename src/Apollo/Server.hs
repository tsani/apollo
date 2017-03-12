{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
, AsyncResult(..)
) where

import Apollo.Api
import Apollo.Misc
import Apollo.Monad ( Apollo, ApolloError )
import qualified Apollo.Monad as A
import Apollo.Types

import Control.Concurrent ( threadDelay )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad ( forM_, join )
import Data.Default.Class ( def )
import Data.IORef
import Data.List.NonEmpty ( NonEmpty )
import Servant

type ApolloServer k e r
  = ServerT (ApolloApi k) (Apollo k e r)

data AsyncResult
  = AsyncTranscodeResult (NonEmpty TrackId)
  | AsyncTestResult Int
  | AsyncArchiveResult ArchiveId

server :: ApolloServer JobId (ApolloError JobId) AsyncResult
server = topRoutes where
  topRoutes
    = download
    :<|> playlist
    :<|> status
    :<|> transcodings
    :<|> archives
    :<|> testAsync

  download = youtubeDlSync :<|> youtubeDlAsync where
    youtubeDlSync :: YoutubeDlReq -> Apollo JobId e AsyncResult (NonEmpty Entry)
    youtubeDlSync YoutubeDlReq{..} = A.youtubeDl downloadPath downloadUrl

    youtubeDlAsync = make :<|> check where
      make :: YoutubeDlReq -> Apollo JobId e AsyncResult JobQueueResult
      make = error "youtube-dl async not implemented"

      check
        :: JobId
        -> Apollo
          JobId
          e
          AsyncResult
          (JobQueryResult (ApolloError JobId) (NonEmpty Entry))
      check = error "youtube-dl async not implemented"

  playlist = deleteTracks :<|> enqueueTracks :<|> getPlaylist where
    deleteTracks :: [PlaylistItemId] -> Apollo JobId e AsyncResult Playlist
    deleteTracks items = A.deleteTracks items *> A.getPlaylist

    enqueueTracks
      :: Maybe PositionBetweenTracks
      -> (NonEmpty FilePath)
      -> Apollo JobId e AsyncResult (NonEmpty PlaylistItemId)
    enqueueTracks p = A.enqueueTracks (maybe def id p)

    getPlaylist :: Apollo JobId e AsyncResult Playlist
    getPlaylist = A.getPlaylist

  status :: Apollo JobId e AsyncResult PlayerStatus
  status = A.getPlayerStatus

  transcodings = makeTranscode :<|> getTranscode :<|> asyncTranscoding where
    makeTranscode :: TranscodeReq -> Apollo JobId e AsyncResult TrackIdW
    makeTranscode TranscodeReq{..} = TrackIdW
      <$> A.transcodeTrack transSource transParams

    getTranscode
      :: TrackId
      -> TranscodingParameters
      -> Apollo JobId e AsyncResult LazyTrackData
    getTranscode = A.readTranscodeLazily

    asyncTranscoding = startAsyncTranscode :<|> checkAsyncTranscode where
      startAsyncTranscode
        :: NonEmpty TranscodeReq
        -> Apollo JobId e AsyncResult JobQueueResult
      startAsyncTranscode reqs = do
        m <- A.getMusicDir
        t <- A.getTranscodeDir

        let f TranscodeReq{..} = A.doTranscode m t transSource transParams
        i <- A.startBatchAsyncJob reqs $ do
          AsyncTranscodeResult <$> A.forUpdate reqs (liftIO . f)

        url <- A.getApiLink $
          apiLink' (Proxy :: Proxy (QueryAsyncTranscode JobId)) i

        pure JobQueueResult
          { jobQueueId = i
          , jobQueueQueryUrl = url
          }

      checkAsyncTranscode
        :: JobId
        -> Apollo
          JobId
          (ApolloError JobId)
          AsyncResult
          (JobQueryResult (ApolloError JobId) (NonEmpty TrackId))
      checkAsyncTranscode i = do
        r <- A.queryAsyncJob i
        pure . JobQueryResult . join . (r <#>) $ \case
          AsyncTranscodeResult n ->
            JobComplete n

          AsyncTestResult{} ->
            JobFailed (A.WrongAsyncResult "transcode" "test")

          AsyncArchiveResult{} ->
            JobFailed (A.WrongAsyncResult "archive" "test")

  archives = makeArchive :<|> getArchive :<|> asyncArchive where
    makeArchive :: NonEmpty ArchiveEntry -> Apollo k e a ArchivalResult
    makeArchive entries = do
      archiveId <- A.makeArchive entries
      archiveUrl <- A.getStaticUrl (StaticArchive archiveId)
      pure ArchivalResult
        { archivalResId = archiveId
        , archivalResUrl = archiveUrl
        }

    getArchive :: ArchiveId -> Apollo k e a LazyArchiveData
    getArchive = A.readArchiveLazily

    asyncArchive = make :<|> check where
      make
        :: NonEmpty ArchiveEntry
        -> Apollo JobId (ApolloError JobId) AsyncResult JobQueueResult
      make entries = do
        md <- A.getMusicDir
        td <- A.getTranscodeDir
        ad <- A.getArchiveDir

        i <- A.startBatchAsyncJob entries $ do
          AsyncArchiveResult <$> A.doMakeArchive md td ad entries

        url <- A.getApiLink $
          apiLink'
            (Proxy @(QueryAsyncArchive JobId))
            i

        pure JobQueueResult
          { jobQueueId = i
          , jobQueueQueryUrl = url
          }

      check
        :: JobId
        -> Apollo
          JobId
          (ApolloError JobId)
          AsyncResult
          (JobQueryResult (ApolloError JobId) ArchivalResult)
      check i = do
        r <- A.queryAsyncJob i

        p <- pure . join . (r <#>) $ \case
          AsyncTranscodeResult{} ->
            JobFailed (A.WrongAsyncResult "archive" "transcode")

          AsyncTestResult{} ->
            JobFailed (A.WrongAsyncResult "archive" "test")

          AsyncArchiveResult n ->
            JobComplete n

        fmap JobQueryResult . sequence $ p <#> \x -> do
          ArchivalResult <$> pure x <*> A.getStaticUrl (StaticArchive x)

  testAsync = make :<|> check where
    make :: [Int] -> Apollo JobId e AsyncResult JobQueueResult
    make ns = do
      let l = length ns

      -- run the async job to add up the numbers
      i <- A.startAsyncJob (Progress 0 l) $ do
        -- a sum starts a zero
        v <- liftIO (newIORef 0)

        -- loop over the numbers and add them to the variable, while updating
        -- the progress
        forM_ (zip [1..] ns) $ \(i, n) -> do
          reportProgress (Progress i l)
          liftIO $ do
            modifyIORef v (n +)
            threadDelay 1000000

        -- the result of the async job is just whatever's in the variable
        AsyncTestResult <$> liftIO (readIORef v)

      url <- A.getApiLink $
        apiLink'
          (Proxy @(QueryAsyncTest JobId))
          i

      pure JobQueueResult
        { jobQueueId = i
        , jobQueueQueryUrl = url
        }

    check
      :: JobId
      -> Apollo
        JobId
        (ApolloError JobId)
        AsyncResult
        (JobQueryResult (ApolloError JobId) Foo)
    check i = do
      r <- A.queryAsyncJob i
      pure . JobQueryResult . join . (r <#>) $ \case
        AsyncTranscodeResult{} ->
          JobFailed $ A.WrongAsyncResult "test" "transcode"

        AsyncTestResult n ->
          JobComplete $ Foo n

        AsyncArchiveResult{} ->
          JobFailed $ A.WrongAsyncResult "test" "archive"
