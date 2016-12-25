{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module Apollo.Monad
( -- * Apollo monad
  ApolloF(..)
, Apollo
  -- ** Actions
, youtubeDl
, mpd
  -- ** Interpreters
, interpretApolloIO
  -- * Misc
, MpdSettings(..)
, DirLock
, MpdLock
, makeDirLock
, makeMpdLock
) where

import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import Control.Concurrent.MVar
import Control.Exception ( Exception, throwIO )
import Control.Monad ( forM_ )
import Control.Monad.Free
import Data.Default.Class
import qualified Data.Text as T
import Network.MPD
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

data ApolloF next
  = YoutubeDl MusicDir YoutubeDlUrl ([Entry] -> next)
  | forall a. DoMpd (MPD a) (a -> next)

deriving instance Functor ApolloF

type Apollo = Free ApolloF

youtubeDl :: MusicDir -> YoutubeDlUrl -> Apollo [Entry]
youtubeDl x y = liftF $ YoutubeDl x y id

mpd :: MPD a -> Apollo a
mpd m = liftF $ DoMpd m id

newtype DirLock = DirLock (MVar ())

withDirLock :: DirLock -> IO a -> IO a
withDirLock (DirLock d) = withMVar d . const

makeDirLock :: IO DirLock
makeDirLock = DirLock <$> newMVar ()

newtype MpdLock = MpdLock (MVar ())

withMpdLock :: MpdLock -> IO a -> IO a
withMpdLock (MpdLock d) = withMVar d . const

makeMpdLock :: IO MpdLock
makeMpdLock = MpdLock <$> newMVar ()

data MpdSettings
  = MpdSettings
    { mpdHost :: !String
    , mpdPort :: !Integer
    , mpdPassword :: !String
    }

instance Default MpdSettings where
  def = MpdSettings
    { mpdHost = "localhost"
    , mpdPort = 6600
    , mpdPassword = ""
    }

newtype MpdError = MpdError MPDError deriving Show
instance Exception MpdError

interpretApolloIO :: MpdSettings -> MpdLock -> DirLock -> Apollo a -> IO a
interpretApolloIO MpdSettings{..} mpdLock dirLock = iterM phi where
  runMpd = withMPDEx mpdHost mpdPort mpdPassword

  phi :: ApolloF (IO a) -> IO a
  phi m = case m of
    YoutubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) k -> do
      let dp = T.unpack musicDirT
      let url = T.unpack dlUrl

      entries <- withTempDirectory "/tmp" "apollo." $ \dirPath -> do
        outputFiles <- withDirLock dirLock $ do
          Dir.withCurrentDirectory dirPath $ do
            Y.youtubeDl url
            Dir.listDirectory "."

        Dir.createDirectoryIfMissing True dp
        forM_ outputFiles $ \outputFile -> do
          Dir.copyFile (dirPath </> outputFile) (dp </> outputFile)

        pure (Entry musicDir . T.pack <$> outputFiles)

      k entries

    DoMpd action k -> do
      r <- withMpdLock mpdLock (runMpd action)
      case r of
        Left e -> throwIO e
        Right x -> k x
