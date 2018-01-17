{-# LANGUAGE TypeOperators #-}

module Main ( main ) where

import Apollo.Server
import Apollo.Monad
  ( ApolloIO
  , ApolloError(..)
  , runApolloIO
  , makeMpdLock
  , makeDirLock
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
import System.Environment ( getArgs )

import Prelude hiding ( (.) )

main :: IO ()
main = do
  [p, host, port', pass, apiScheme, apiDomain, apiPort', stScheme, stDomain, stPort'] <- getArgs
  let (port, apiPort, stPort) = (read port', read apiPort', read stPort')

  mpdLock <- makeMpdLock
  dirLock <- makeDirLock
  jobBank <- newJobBankVar

  let settings = ApolloSettings
        { apolloMpdSettings = MpdSettings
          { mpdHost = host
          , mpdPort = port
          , mpdPassword = pass
          }
        , apolloDirLock = dirLock
        , apolloMpdLock = mpdLock
        , apolloJobBank = jobBank
        , apolloApiServerSettings = ServerSettings
          { serverDomain = T.pack apiDomain
          , serverScheme = T.pack apiScheme
          , serverPort = apiPort
          }
        , apolloStaticServerSettings = ServerSettings
          { serverDomain = T.pack stDomain
          , serverScheme = T.pack stScheme
          , serverPort = stPort
          }
        , apolloMusicDirP = "music"
        , apolloTranscodeDirP = "transcode"
        , apolloArchiveDirP = "archive"
        }

  let httpPort = read p :: Int
  putStrLn $ "Listening on port " ++ show httpPort
  run httpPort (logStdoutDev $ app settings)

app :: ApolloSettings JobId (ApolloError JobId) AsyncResult -> Application
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
