{-# LANGUAGE RecordWildCards #-}

module Apollo.Server
( ApolloApi
, server
, AsyncResult(..)
) where

import Apollo.Api
import Apollo.Misc
import qualified Apollo.Monad.Types as A
import Apollo.Monad
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import Control.Concurrent ( threadDelay )
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad ( forM_, join )
import Control.Monad.IO.Class ( liftIO )
import Control.Monad.Reader
import Data.Default.Class ( def )
import Data.IORef
import Data.List.NonEmpty ( NonEmpty )
import Servant

type ApolloServer k e r
  = ServerT (ApolloApi k) (ApolloIO k e r)

data AsyncResult
  = AsyncTranscodeResult (NonEmpty TrackId)
  -- ^ Result of asynchronously transcoding.
  | AsyncTestResult Int
  -- ^ Result of asynchronous test.
  | AsyncArchiveResult ArchiveId Compressor
  -- ^ Result of asynchronously archiving.
  | AsyncYoutubeDlResult (NonEmpty Entry)
  -- ^ Result of asynchronously downloading music.

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
    youtubeDlSync
      :: YoutubeDlReq
      -> Maybe Bool -- ^ ignore 404
      -> Maybe Bool -- ^ extract audio
      -> Maybe Bool -- ^ add metadata
      -> Maybe String -- ^ audio format
      -> ApolloIO JobId e AsyncResult (NonEmpty Entry)
    youtubeDlSync YoutubeDlReq{..} m404 maudio mmeta mfmt =
        A.youtubeDl
          downloadPath
          downloadUrl
          (settings m404 maudio mmeta mfmt)
          (const $ pure ())

    youtubeDlAsync = make :<|> check where
      make
        :: YoutubeDlReq
        -> Maybe Bool -- ^ ignore 404
        -> Maybe Bool -- ^ extract audio
        -> Maybe Bool -- ^ add metadata
        -> Maybe String -- ^ audio format
        -> ApolloIO JobId (ApolloError JobId) AsyncResult JobQueueResult
      make YoutubeDlReq{..} m404 maudio mmeta mfmt = do
        let s = settings m404 maudio mmeta mfmt

        r <- ask
        i <- startAsyncJob (Progress 0 1) $ do
          chan <- liftIO newChan
          -- in one thread, perform the download and send progress reports
          -- through the chan
          a <- liftIO $ async $ runApolloIO r $ do
            -- write progress reports using Right into the chan
            A.youtubeDl downloadPath downloadUrl s (liftIO . writeChan chan . Right)

          -- we spawn a new thread that monitors the one we created
          _ <- liftIO $ async $ waitCatch a >>= \case
            -- if the other thread blew up, then we write a death message into
            -- the chan
            Left e -> writeChan chan (Left $ Left e)
            -- if the other thread successfully completed, then we write a
            -- success message into the chan
            Right es -> writeChan chan (Left $ Right es)

          -- in the main job thread, we pump the chan
          either jobError (pure . AsyncYoutubeDlResult) =<< pumpProgress chan

        apiUrl <- A.getApiLink $ linkURI $
          apiLink' (Proxy :: Proxy (QueryAsyncYoutubeDl JobId)) i

        pure JobQueueResult
          { jobQueueId = i
          , jobQueueQueryUrl = apiUrl
          }

      check
        :: JobId
        -> ApolloIO
          JobId
          (ApolloError JobId)
          AsyncResult
          (JobQueryResult (ApolloError JobId) (NonEmpty Entry))
      check i = do
        r <- queryAsyncJob i
        pure . JobQueryResult . join . (r <#>) $ \case
          AsyncTranscodeResult _ ->
            JobFailed (WrongAsyncResult "youtube-dl" "transcode")

          AsyncTestResult{} ->
            JobFailed (WrongAsyncResult "youtube-dl" "test")

          AsyncArchiveResult{} ->
            JobFailed (WrongAsyncResult "youtube-dl" "archive")

          AsyncYoutubeDlResult entries ->
            JobComplete entries

    settings m404 maudio mmeta mfmt =
      a404 . aaudio . ameta . afmt $ Y.defaultYoutubeDlSettings where
        a404 = maybe id (\x s -> s { Y.ytdlIgnore404 = x }) m404
        aaudio = maybe id (\x s -> s { Y.ytdlExtractAudio = x }) maudio
        ameta = maybe id (\x s -> s { Y.ytdlAddMetadata = x }) mmeta
        afmt = maybe id (\x s -> s { Y.ytdlAudioFormat = x }) mfmt

  playlist = deleteTracks :<|> enqueueTracks :<|> getPlaylist where
    deleteTracks :: [PlaylistItemId] -> ApolloIO JobId e AsyncResult Playlist
    deleteTracks items = A.deleteTracks items *> getPlaylist

    enqueueTracks
      :: Maybe PositionBetweenTracks
      -> (NonEmpty FilePath)
      -> ApolloIO JobId e AsyncResult (NonEmpty PlaylistItemId)
    enqueueTracks p = A.enqueueTracks (maybe def id p)

    getPlaylist :: ApolloIO JobId e AsyncResult Playlist
    getPlaylist = A.getPlaylist

  status :: ApolloIO JobId e AsyncResult PlayerStatus
  status = A.getPlayerStatus

  transcodings = makeTranscode :<|> getTranscode :<|> asyncTranscoding where
    makeTranscode :: TranscodeReq -> ApolloIO JobId e AsyncResult TrackIdW
    makeTranscode TranscodeReq{..} = TrackIdW
      <$> A.makeTranscode transSource transParams

    getTranscode
      :: TrackId
      -> TranscodingParameters
      -> ApolloIO JobId e AsyncResult LazyTrackData
    getTranscode = A.readTranscodeLazily

    asyncTranscoding = startAsyncTranscode :<|> checkAsyncTranscode where
      startAsyncTranscode
        :: NonEmpty TranscodeReq
        -> ApolloIO JobId e AsyncResult JobQueueResult
      startAsyncTranscode reqs = do
        m <- asks apolloMusicDirP
        t <- asks apolloTranscodeDirP

        let f TranscodeReq{..} = doTranscode m t transSource transParams
        i <- startAsyncJob (Progress 0 $ length reqs) $ do
          AsyncTranscodeResult <$> forUpdate reqs (liftIO . f)

        url <- A.getApiLink $ linkURI $
          apiLink' (Proxy :: Proxy (QueryAsyncTranscode JobId)) i

        pure JobQueueResult
          { jobQueueId = i
          , jobQueueQueryUrl = url
          }

      checkAsyncTranscode
        :: JobId
        -> ApolloIO
          JobId
          (ApolloError JobId)
          AsyncResult
          (JobQueryResult (ApolloError JobId) (NonEmpty TrackId))
      checkAsyncTranscode i = do
        r <- queryAsyncJob i
        pure . JobQueryResult . join . (r <#>) $ \case
          AsyncTranscodeResult n ->
            JobComplete n

          AsyncTestResult{} ->
            JobFailed (WrongAsyncResult "transcode" "test")

          AsyncArchiveResult{} ->
            JobFailed (WrongAsyncResult "transcode" "archive")

          AsyncYoutubeDlResult _ ->
            JobFailed (WrongAsyncResult "transcode" "youtube-dl")

  archives = makeArchive :<|> getArchive :<|> asyncArchive where
    makeArchive
      :: (Bounded k, Enum k, Ord k)
      => NonEmpty ArchiveEntry -> Maybe Compressor -> ApolloIO k e a ArchivalResult
    makeArchive entries (maybe def id -> c) = do
      archiveId <- A.makeArchive c entries
      archiveUrl <- A.getStaticUrl (StaticArchive archiveId c)
      pure ArchivalResult
        { archivalResId = archiveId
        , archivalResUrl = archiveUrl
        }

    getArchive
      :: (Bounded k, Enum k, Ord k)
      => ArchiveId -> ApolloIO k e a LazyArchiveData
    getArchive = A.readArchiveLazily

    asyncArchive = make :<|> check where
      make
        :: NonEmpty ArchiveEntry
        -> Maybe Compressor
        -> ApolloIO JobId (ApolloError JobId) AsyncResult JobQueueResult
      make entries (maybe def id -> c) = do
        md <- asks apolloMusicDirP
        td <- asks apolloTranscodeDirP
        ad <- asks apolloArchiveDirP

        i <- startAsyncJob (Progress 0 1) $ do
          AsyncArchiveResult <$> doMakeArchive c md td ad entries <*> pure c

        url <- A.getApiLink $
          linkURI $ apiLink'
            (Proxy @(QueryAsyncArchive JobId))
            i

        pure JobQueueResult
          { jobQueueId = i
          , jobQueueQueryUrl = url
          }

      check
        :: JobId
        -> ApolloIO
          JobId
          (ApolloError JobId)
          AsyncResult
          (JobQueryResult (ApolloError JobId) ArchivalResult)
      check i = do
        r <- queryAsyncJob i

        p <- pure . join . (r <#>) $ \case
          AsyncTranscodeResult{} ->
            JobFailed (WrongAsyncResult "archive" "transcode")

          AsyncTestResult{} ->
            JobFailed (WrongAsyncResult "archive" "test")

          AsyncArchiveResult n c ->
            JobComplete (n, c)

          AsyncYoutubeDlResult _ ->
            JobFailed (WrongAsyncResult "archive" "youtube-dl")

        fmap JobQueryResult . sequence $ p <#> \(x, c) -> do
          ArchivalResult <$> pure x <*> A.getStaticUrl (StaticArchive x c)

  testAsync = make :<|> check where
    make :: [Int] -> ApolloIO JobId e AsyncResult JobQueueResult
    make ns = do
      let l = length ns

      -- run the async job to add up the numbers
      i <- startAsyncJob (Progress 0 l) $ do
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

      url <- A.getApiLink $ linkURI $
        apiLink'
          (Proxy @(QueryAsyncTest JobId))
          i

      pure JobQueueResult
        { jobQueueId = i
        , jobQueueQueryUrl = url
        }

    check
      :: JobId
      -> ApolloIO
        JobId
        (ApolloError JobId)
        AsyncResult
        (JobQueryResult (ApolloError JobId) Foo)
    check i = do
      r <- queryAsyncJob i
      pure . JobQueryResult . join . (r <#>) $ \case
        AsyncTranscodeResult{} ->
          JobFailed $ WrongAsyncResult "test" "transcode"

        AsyncTestResult n ->
          JobComplete $ Foo n

        AsyncArchiveResult{} ->
          JobFailed $ WrongAsyncResult "test" "archive"

        AsyncYoutubeDlResult _ ->
          JobFailed $ WrongAsyncResult "test" "youtube-dl"

pumpProgress
  :: (JobError m ~ ApolloError k, Show a, MonadJob m, MonadIO m)
  => Chan (Either (Either a b) (Int, Int)) -> m b
pumpProgress chan = liftIO (readChan chan) >>= \case
  Left (Left e) -> jobError $ SubprocessDied (show e)
  Left (Right es) -> pure es
  Right (n, d) -> reportProgress (Progress n d) *> pumpProgress chan
