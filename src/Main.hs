{-# LANGUAGE TypeOperators #-}

module Main where

import Apollo.Server
import Apollo.Monad
  ( Apollo
  , ApolloError(..)
  , runApolloIO
  , makeMpdLock
  , makeDirLock
  , newJobBankVar
  , ApolloSettings(..)
  , ServerSettings(..)
  , MpdSettings(..)
  , interpretApolloIO
  )

import Control.Category ( (.) )
import Control.Monad.IO.Class ( liftIO )
import Data.ByteString.Lazy ( fromStrict )
import Data.List ( intercalate )
import qualified Data.Text as T
import Data.Text.Encoding ( encodeUtf8 )
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
        }

  let httpPort = read p :: Int
  putStrLn $ "Listening on port " ++ show httpPort
  run httpPort (logStdoutDev $ app settings)

app :: ApolloSettings -> Application
app settings = serve api server' where
  api :: Proxy ApolloApi
  api = Proxy

  server' :: Server ApolloApi
  server' = enter nat server

  nat :: Apollo :~> Handler
  nat = Nat $ (>>= adjust) . liftIO . runApolloIO . interpretApolloIO settings

  adjust :: Either ApolloError a -> Handler a
  adjust m = case m of
    Left e -> throwError $ case e of
      ApolloMpdError mpdE -> err500 { errBody = lazyEncode $ show mpdE }
      NoSuchTranscode tid params -> err404
        { errBody = lazyEncode $ intercalate " "
          [ "No such transcode with trackId"
          , show tid
          , "and transcoding parameters"
          , show params
          ]
        }
      NoSuchJob i -> err404
        { errBody = lazyEncode "no such job"
        }
    Right x -> pure x

  lazyEncode = fromStrict . encodeUtf8 . T.pack
