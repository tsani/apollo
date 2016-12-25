{-# LANGUAGE DeriveFunctor #-}

module Apollo.Monad where

import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import Control.Concurrent.MVar
import Control.Monad ( forM_ )
import Control.Monad.Free
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import System.IO.Temp ( withTempDirectory )

data ApolloF a
  = YoutubeDl MusicDir YoutubeDlUrl ([Entry] -> a)
  deriving Functor

type Apollo = Free ApolloF

youtubeDl :: MusicDir -> YoutubeDlUrl -> Apollo [Entry]
youtubeDl x y = liftF $ YoutubeDl x y id

interpretApolloIO :: MVar () -> Apollo a -> IO a
interpretApolloIO dirLock = iterM phi where
  phi :: ApolloF (IO a) -> IO a
  phi m = case m of
    YoutubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) k -> do
      let dp = T.unpack musicDirT
      let url = T.unpack dlUrl

      entries <- withTempDirectory "/tmp" "apollo." $ \dirPath -> do
        outputFiles <- withMVar dirLock $ \_ ->
          Dir.withCurrentDirectory dirPath $ do
            Y.youtubeDl url
            Dir.listDirectory "."

        Dir.createDirectoryIfMissing True dp
        forM_ outputFiles $ \outputFile -> do
          Dir.copyFile (dirPath </> outputFile) (dp </> outputFile)

        pure (Entry musicDir . T.pack <$> outputFiles)

      k entries
