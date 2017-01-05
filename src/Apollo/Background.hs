{-|
 - Background jobs
 -}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Apollo.Background
( Job(..)
, RunnableJob(..)
, JobK
, JobS(..)
, JobProgress(..)
, JobStatus(..)
, SomeJobStatus(..)
, JobState(..)
, SomeJobState(..)
, Error(..)
, SystemError(..)
, Proxiable(..)
, JobBank
, JobBankVar
, newJobBankVar
, queryJob
, queueJob
, runJobDefault
, runJobDefaultEx
) where

import Apollo.Reflection
import Apollo.Track
import Apollo.Transcoding
import Apollo.Types

import Control.Concurrent ( forkIO )
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad ( void )
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson ( ToJSON(..), object, (.=) )
import Data.Default.Class
import Data.IntMap ( IntMap )
import qualified Data.IntMap as M
import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Text ( Text )
import Data.Type.Equality
import Data.Void
import qualified System.Directory as Dir
import System.FilePath ( (</>) )

data JobK
  = Transcode
  | Archive
  | Bulk JobK

data JobS :: JobK -> * where
  TranscodeS :: JobS 'Transcode
  ArchiveS :: JobS 'Archive
  BulkS :: JobS a -> JobS ('Bulk a)

instance TestEquality JobS where
  testEquality a b = case (a, b) of
    (TranscodeS, TranscodeS) -> Just Refl
    (ArchiveS, ArchiveS) -> Just Refl
    (BulkS a', BulkS b') -> case testEquality a' b' of
      Just Refl -> Just Refl
      Nothing -> Nothing
    _ -> Nothing

instance Proxiable JobS where
  proxy = \case
    TranscodeS -> Proxy
    ArchiveS-> Proxy
    BulkS _ -> Proxy

type instance Demote' 'KProxy = JobS

instance ReflectS 'Transcode where reflectS _ = TranscodeS
instance ReflectS 'Archive where reflectS _ = ArchiveS
instance ReflectS a => ReflectS ('Bulk a) where
  reflectS _ = BulkS (reflectS (Proxy :: Proxy a))

data JobProgress
  = Discrete Int Int

instance ToJSON JobProgress where
  toJSON = \case
    Discrete n d -> object
      [ "complete" .= n
      , "total" .= d
      ]

-- | Errors that can occur during a job. Either the job killed itself by
-- raising some user error of type @u@ or an unhandled exception occurred.
data Error u
  = UserError u
  | SystemError SystemError

instance ToJSON u => ToJSON (Error u) where
  toJSON = \case
    UserError u -> object
      [ "cause" .= ("internal" :: Text)
      , "detail" .= u
      ]
    SystemError (ExecutionException e) -> object
      [ "cause" .= ("external" :: Text)
      , "detail" .= show e
      ]

type JobError j = Error (UserError j)

data JobState e a
  = JobState
    { jobResult :: MVar (Either e a)
    , jobProgress :: IORef JobProgress
    }

data SomeJobState
  = forall j. (ToJSON (JobError j), ToJSON (Result j))
  => SomeJobState (JobS j) (JobState (JobError j) (Result j))

type JobMap = IntMap SomeJobState

data JobBank
  = JobBank
    { jobBankJobs :: JobMap
    , jobBankNextId :: Int
    }

instance Default JobBank where
  def = emptyJobBank

emptyJobBank :: JobBank
emptyJobBank = JobBank { jobBankJobs = M.empty, jobBankNextId = 0 }

newtype JobBankVar = JobBankVar (MVar JobBank)

newJobBankVar :: IO JobBankVar
newJobBankVar = JobBankVar <$> newMVar emptyJobBank

data SomeJobStatus where
  SomeJobStatus
    :: (ToJSON (JobError j), ToJSON (Result j))
    => JobS j
    -> JobStatus JobProgress (JobError j) (Result j)
    -> SomeJobStatus

data SystemError
  = ExecutionException SomeException

data Job env q e r
  = Job
    { initialProgress :: q -> JobProgress
    , runJob :: forall m. MonadJob e m => env -> q -> m r
    }

-- | Monad in which jobs are executed.
newtype RunJob e a
  = RunJob (ReaderT JobConf (ExceptT e IO) a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader JobConf, MonadError e, MonadIO
    )

data JobConf
  = JobConf
    { jobConfProgress :: JobProgress -> IO ()
    }

instance Default JobConf where
  def = JobConf { jobConfProgress = const (pure ()) }

runRunJob :: RunJob e a -> JobConf -> IO (Either e a)
runRunJob (RunJob m) c = runExceptT (runReaderT m c)

class (MonadError e m, MonadIO m) => MonadJob e m where
  reportProgress :: JobProgress -> m ()

instance MonadJob e (RunJob e) where
  reportProgress p = liftIO =<< asks jobConfProgress <*> pure p

-- | Transform a job monad so that progress reporting does nothing.
newtype IgnoringProgress (e :: *) m a = IgnoringProgress (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e)

instance MonadJob e m => MonadJob e (IgnoringProgress e m) where
  reportProgress _ = IgnoringProgress (pure ())

-- | Any monad @m@ satisfying @MonadError e m@ can be made an instance of
-- @MonadJob e m@ by making progress reports do nothing.
newtype WithoutProgress (e :: *) m a = WithoutProgress (m a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError e)

instance (MonadIO m, MonadError e m) => MonadJob e (WithoutProgress e m) where
  reportProgress _ = WithoutProgress (pure ())
instance RunnableJob 'Transcode where
  type Env 'Transcode = (FilePath, FilePath)
  type Params 'Transcode = (FilePath, TranscodingParameters)
  type Result 'Transcode = TrackId
  type UserError 'Transcode = Void

  job _ = Job
    { initialProgress = const (Discrete 0 1)
    , runJob = \(musicDirP, transcodeDirP) (track, params) -> do
      d <- liftIO $ readTrackIO (musicDirP </> track)
      let tid = trackId d
      liftIO $ putStrLn $ "computed track ID " ++ show tid
      mp <- liftIO $ getExistingTranscode (Just transcodeDirP) tid params
      liftIO $ putStrLn "got existing transcode"
      _ <- case mp of
        Just transcodePath -> pure (transcodeDirP </> transcodePath)
          -- then the transcode exists, so no work to do.
        Nothing -> liftIO $ do -- no transcode exists, so we have to make one
          let dir = transcodeDirectoryFor tid params
          Dir.createDirectoryIfMissing True (transcodeDirP </> dir)
          transcode (musicDirP </> track) (transcodeDirP </> dir) params

      pure tid
    }

class RunnableJob j where
  type Env j
  type Params j
  type Result j
  type UserError j

  job :: Proxy j -> Job (Env j) (Params j) (UserError j) (Result j)

instance RunnableJob j => RunnableJob ('Bulk j) where
  type Env ('Bulk j) = Env j
  type Params ('Bulk j) = NonEmpty (Params j)
  type Result ('Bulk j) = NonEmpty (Result j)
  type UserError ('Bulk j) = UserError j

  job _ = j where
    p = Proxy :: Proxy j
    Job { runJob = runSubJob } = job p
    j = Job
      { initialProgress = Discrete 0 . NE.length
      , runJob = run
      }

    run :: forall m. MonadJob (UserError ('Bulk j)) m
      => Env ('Bulk j) -> Params ('Bulk j) -> m (Result ('Bulk j))
    run env params = m where
      jobCount = NE.length params
      ip = IgnoringProgress -- shorthand
      (IgnoringProgress m) = forM (NE.zip (1 :| [2..]) params) phi where
        -- this looks weird! It looks like we're suppressing the reportProgress
        -- but we actually aren't. reportProgress is suppressed in the
        -- IgnoringProgress monad, but not in the underlying monad that it
        -- transforms! Otherwise, IgnoringProgress is just an identity monad
        -- transformer, so the reportProgress of the underlying monad is
        -- honoured.
        phi (i, q) = runSubJob env q <* ip (reportProgress (Discrete i jobCount))

-- | Run a job in a given environment with given parameters, ignoring any
-- progress reports, in the @IO@ monad.
runJobDefault :: Job env q e r -> env -> q -> IO (Either e r)
runJobDefault Job { runJob = r } e q = runRunJob (r e q) def

-- | Run a job in a given environment with given parameters, ignoring any
-- progress reports and throwing an impure exception in case of error, in the
-- @IO@ monad.
runJobDefaultEx :: Exception e => Job env q e r -> env -> q -> IO r
runJobDefaultEx j e q = rethrow =<< runJobDefault j e q where
  rethrow = \case
    Right x -> pure x
    Left err -> throwIO err

queryJob :: JobId -> JobS j -> JobBankVar -> IO (Maybe SomeJobStatus)
queryJob (JobId i) j (JobBankVar v) = modifyMVar v wrap where
  wrap bank@JobBank { jobBankJobs = m } = do
    (m', s) <- go m
    pure (bank { jobBankJobs = m' }, s)

  go :: JobMap -> IO (JobMap, Maybe SomeJobStatus)
  go m = case M.lookup i m of
    Nothing -> pure (m, Nothing)
    Just (SomeJobState w s) -> case testEquality w j of
      Just Refl -> tryTakeMVar (jobResult s) >>= \case
        -- the job produced a result, so now we see: is it success or error
        Just r -> case r of
          Right x -> pure (M.delete i m, Just (SomeJobStatus w (Complete x)))
          Left e -> pure (M.delete i m, Just (SomeJobStatus w (Aborted e)))

        -- the job has not produced a result yet, so read its progress and return
        -- that.
        Nothing -> do
          p <- readIORef (jobProgress s)
          pure (m, Just (SomeJobStatus w (InProgress p)))
      Nothing -> pure (m, Nothing)

queueJob
  :: forall j.
    (RunnableJob j, ToJSON (Result j), ToJSON (UserError j))
  => JobS j -> Env j -> Params j -> JobBankVar -> IO JobId
queueJob w env q (JobBankVar v) = modifyMVar v wrap where
  p = Proxy :: Proxy j
  Job { initialProgress = i, runJob = run } = job p

  wrap bank@JobBank { jobBankJobs = m, jobBankNextId = n } = do
    m' <- go m n
    pure (bank { jobBankJobs = m', jobBankNextId = n + 1 }, JobId (n + 1))

  go :: JobMap -> Int -> IO JobMap
  go m n = do
    resultV <- newEmptyMVar :: IO (MVar (Either (JobError j) (Result j)))
    progressV <- newIORef (i q)
    void . forkIO $ do
      a <- async $ do
        runRunJob (run env q) JobConf
          { jobConfProgress = writeIORef progressV
          }
      r <- waitCatch a
      case r of
        Left e -> putMVar resultV (Left (SystemError (ExecutionException e)))
        Right r' -> case r' of
          Left e -> putMVar resultV (Left (UserError e))
          Right x -> putMVar resultV (Right x)
    let s = JobState { jobResult = resultV , jobProgress = progressV }
    pure $ M.insert n (SomeJobState w s) m
