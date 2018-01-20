{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import Apollo.Server
import Apollo.Monad
  ( ApolloIO
  , ApolloError(..)
  , runApolloIO
  , makeMpdLock
  , ApolloSettings(..)
  , ServerSettings(..)
  , MpdSettings(..)
  )
import Apollo.Types

import Control.Category ( (.) )
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode )
import qualified Data.Text as T
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant
import System.Environment ( getEnv )

import Prelude hiding ( (.) )

type ApolloSettings' = ApolloSettings JobId (ApolloError JobId) AsyncResult

loadConfig :: IO (Int, ApolloSettings')
loadConfig = do
  port <- read <$> getEnv "APOLLO_PORT"

  mHost <- getEnv "APOLLO_MPD_HOST"
  mPort <- read <$> getEnv "APOLLO_MPD_PORT"
  mPass <- getEnv "APOLLO_MPD_PASSWORD"

  apiDomain <- T.pack <$> getEnv "APOLLO_API_DOMAIN"
  apiScheme <- T.pack <$> getEnv "APOLLO_API_SCHEME"
  apiPort <- read <$> getEnv "APOLLO_API_PORT"

  staticDomain <- T.pack <$> getEnv "APOLLO_STATIC_DOMAIN"
  staticScheme <- T.pack <$> getEnv "APOLLO_STATIC_SCHEME"
  staticPort <- read <$> getEnv "APOLLO_STATIC_PORT"

  musicDir <- getEnv "APOLLO_MUSIC_DIR"
  transDir <- getEnv "APOLLO_TRANSCODE_DIR"
  archiveDir <- getEnv "APOLLO_ARCHIVE_DIR"
  tmpDir <- getEnv "APOLLO_TMP_DIR"

  mpdLock <- makeMpdLock
  jobBank <- newJobBankVar

  pure $ (,) port ApolloSettings
    { apolloMpdSettings = MpdSettings
      { mpdHost = mHost
      , mpdPort = mPort
      , mpdPassword = mPass
      }
    , apolloMpdLock = mpdLock
    , apolloJobBank = jobBank
    , apolloApiServerSettings = ServerSettings
      { serverDomain = apiDomain
      , serverScheme = apiScheme
      , serverPort = apiPort
      }
    , apolloStaticServerSettings = ServerSettings
      { serverDomain = staticDomain
      , serverScheme = staticScheme
      , serverPort = staticPort
      }
    , apolloMusicDirP = musicDir
    , apolloTranscodeDirP = transDir
    , apolloArchiveDirP = archiveDir
    , apolloTmpDir = tmpDir
    }

main :: IO ()
main = do
  (httpPort, settings) <- loadConfig
  putStrLn $ "Listening on port " ++ show httpPort
  run httpPort (logStdoutDev $ app settings)

app :: ApolloSettings' -> Application
app settings = serve api server' where
  api :: Proxy (ApolloApi k)
  api = Proxy

  server' :: Server (ApolloApi JobId)
  server' = enter nat server

  nat :: ApolloIO JobId (ApolloError JobId) AsyncResult :~> Handler
  nat = NT $ (>>= adjust) . liftIO . runApolloIO settings

  adjust :: Either (ApolloError JobId) a -> Handler a
  adjust m = case m of
    Left e -> throwError $ case e of
      ApolloMpdError{} -> err500 { errBody = encode e }
      NoSuchTranscode{} -> err404 { errBody = encode e }
      NoSuchJob{} -> err404 { errBody = encode e }
      WrongAsyncResult{} -> err400 { errBody = encode e }
      EmptyYoutubeDlResult{} -> err500 { errBody = encode e }
      SubprocessDied{} -> err500 { errBody = encode e }
    Right x -> pure x
