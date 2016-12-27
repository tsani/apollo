{-# LANGUAGE TypeOperators #-}

module Main where

import Apollo.Server
import Apollo.Monad
  ( Apollo
  , ApolloError(..)
  , runApolloIO
  , makeMpdLock
  , makeDirLock
  , MpdSettings(..)
  , MpdLock
  , DirLock
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
  [host, port', pass] <- getArgs
  let port = read port'
  let settings = MpdSettings { mpdHost = host, mpdPort = port, mpdPassword = pass }
  mpdLock <- makeMpdLock
  dirLock <- makeDirLock

  let httpPort = 8082
  putStrLn $ "Listening on port " ++ show httpPort
  run httpPort (logStdoutDev $ app settings mpdLock dirLock)

app :: MpdSettings -> MpdLock -> DirLock -> Application
app mpdSettings mpdLock dirLock = serve api server' where
  api :: Proxy ApolloApi
  api = Proxy

  server' :: Server ApolloApi
  server' = enter nat server

  nat :: Apollo :~> Handler
  nat = Nat $
    (>>= adjust) . liftIO . runApolloIO .
    interpretApolloIO mpdSettings mpdLock dirLock

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
    Right x -> pure x

  lazyEncode = fromStrict . encodeUtf8 . T.pack
