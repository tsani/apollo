module Apollo.Types.Job
( Progress(..)
, MonadJob(..)
, MonadJobControl(..)
, Exists(..)
, Some
, JobInfo(..)
, JobBank
, emptyJobBank
, newJobBankVar
, Job
, startJob
, checkJob
, runJob
) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Bool ( bool )
import Data.IORef
import Data.Map ( Map )
import qualified Data.Map as Map

data Progress = Progress Int Int

class MonadJobControl m where
  -- | Type of identifiers of jobs.
  type ControlJobId m :: *

  -- | Type of errors raised during execution of jobs.
  type ControlJobError m :: *

  -- | Type of results produced by jobs.
  type ControlJobResult m :: *

  -- | Launches an asynchronous 'Job'.
  startAsyncJob
    :: Progress -- ^ Initial progress of the job.
    -> Job (ControlJobError m) (ControlJobResult m)
    -> m (ControlJobId m)

  queryAsyncJob
    :: ControlJobId m
    -> m (JobInfo (ControlJobError m) (ControlJobResult m))

-- | Abstract monad with distinguished actions job code may perform.
class MonadBaseControl IO m => MonadJob m where
  -- | The type of errors raised by this job code.
  type JobError m :: *

  -- | Signal to the scheduler that progress has been made in the job.
  reportProgress :: Progress -> m ()

  -- | Abort the job with an error.
  jobError :: JobError m -> m a

-- | A datatype that /forgets/ what the particular type of a value is, and
-- remembers only what typeclass(es) it satisfies.
data Exists c where
  Given :: c a => a -> Exists c

-- | Sometimes you do want to remember the exact type of a value is, so you can
-- use this constraint to do just that.
class (a ~ b) => Some a b where

-- | The status of a job.
data JobInfo e a where
  -- | The job is complete, and its result is here.
  JobComplete :: a -> JobInfo e a
  -- | The job failed, and its error is here.
  JobFailed :: e -> JobInfo e a
  -- | The job failed catastrophically, by throwing an impure exception.
  JobDied :: SomeException -> JobInfo e a
  -- | The job is running, and its progress measure is here.
  JobInProgress :: Progress -> JobInfo e a

deriving instance Functor (JobInfo e)
deriving instance Foldable (JobInfo e)
deriving instance Traversable (JobInfo e)

instance Applicative (JobInfo e) where
  pure x = JobComplete x
  (<*>) = ap

instance Monad (JobInfo e) where
  return = pure
  (JobComplete x) >>= k = k x
  (JobInProgress p) >>= _ = JobInProgress p
  (JobFailed e) >>= _ = JobFailed e
  (JobDied e) >>= _ = JobDied e

-- | A job that's running.
data RunningJob e a
  = RunningJob
    { rjThread :: Async (Either e a)
    -- ^ The result of the computation. This MVar is empty until the
    -- computation completes, at which point the success or failure is written
    -- into the MVar.
    , rjProgress :: IORef Progress
    -- ^ The progress of the computation.
    }

-- | An opaque variable for holding jobs.
newtype JobBank i e a
  = JobBank (Map i (RunningJob e a))

-- | An empty bank of jobs.
emptyJobBank :: JobBank i e a
emptyJobBank = JobBank Map.empty

newJobBankVar :: IO (MVar (JobBank i e a))
newJobBankVar = newMVar emptyJobBank

-- | Concrete monad for executing jobs.
newtype RunJob e a
  = RunJob (ReaderT (IORef Progress) (ExceptT e IO) a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError e
    , MonadBase IO
    , MonadReader (IORef Progress)
    )

-- | Interpret a 'RunJob' computation in @IO@.
runRunJob :: IORef Progress -> RunJob e a -> IO (Either e a)
runRunJob pv (RunJob f) = runExceptT (runReaderT f pv)

instance MonadBaseControl IO (RunJob e) where
  type StM (RunJob e) a = Either e a

  liftBaseWith f = ask >>= \r -> liftIO $ f (runRunJob r)
  restoreM s = RunJob $ ReaderT $ const $ ExceptT $ pure s

instance MonadJob (RunJob e) where
  type JobError (RunJob e) = e
  reportProgress p = RunJob $ do
    pv <- ask
    liftIO $ writeIORef pv p
  jobError = throwError

newtype NoProgress e a
  = NoProgress
    { unNoProgress :: ExceptT e IO a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError e
    , MonadIO
    , MonadBase IO
    )

runNoProgress :: NoProgress e a -> IO (Either e a)
runNoProgress = runExceptT . unNoProgress

instance MonadBaseControl IO (NoProgress e) where
  type StM (NoProgress e) a = Either e a

  liftBaseWith f = liftIO $ f runNoProgress
  restoreM s = NoProgress $ ExceptT $ pure s

-- | Progress reporting is a no-op and errors are reported through 'ExceptT'.
instance MonadJob (NoProgress e) where
  type JobError (NoProgress e) = e
  reportProgress _ = pure ()
  jobError = throwError

-- | A job is simply a monadic computation capable of being interpreted in an
-- arbitrary monad satisfying 'MonadJob' and 'MonadIO', i.e. in which both job
-- actions and IO actions can be performed.
--
type Job e a = forall m. (MonadJob m, MonadIO m, JobError m ~ e) => m a

-- | Run a job synchronously. Progress reports are ignored, and errors raised
-- by the job are captured by an 'Either'.
runJob :: Job e a -> IO (Either e a)
runJob = runExceptT . unNoProgress

-- | Run a job asynchronously
startJob
  :: forall e k a.  (Ord k, Bounded k, Enum k)
  => Progress -- ^ initial progress
  -> JobBank k e a
  -- ^ where to store the job record
  -> Job e a
  -- ^ job to run
  -> IO (JobBank k e a, k)
  -- ^ the key to check the job later and the transformed job bank
startJob p (JobBank v) m = do
  let i = bool (succ . fst $ Map.findMax v) minBound (Map.null v)
  pv <- newIORef p -- progress variable
  t <- async (runRunJob pv m)
  let j = RunningJob { rjThread = t, rjProgress = pv }
  pure (JobBank (Map.insert i j v), i)

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) = flip fmap

-- | Check on a job.
--
-- The status is the job is reported. If the job is finished (either by error
-- or successfully) an adjusted job bank is returned with the relevant job
-- record removed.
--
-- Hence there is the following guarantee: if you check a completed job twice,
-- threading the modified job bank through to the second call, then you get
-- 'Nothing' the second time.
checkJob
  :: forall e k a. Ord k
  => k -- ^ the job to check
  -> JobBank k e a
  -> IO (Maybe (JobBank k e a, JobInfo e a))
checkJob i b@(JobBank v) = sequence $ Map.lookup i v <#> \RunningJob{..} -> do
  let b' = JobBank (Map.delete i v)
  poll rjThread >>= \case
    Nothing -> (b,) <$> (JobInProgress <$> readIORef rjProgress)
    Just r ->
      pure . (b',) $ either JobDied (either JobFailed JobComplete) r
