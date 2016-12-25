module Main where

import Apollo.Server
import Apollo.Monad ( makeMpdLock, makeDirLock, MpdSettings(..), MpdLock, DirLock )

import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant
import System.Environment ( getArgs )

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
app mpdSettings mpdLock dirLock
  = serve (Proxy :: Proxy ApolloApi) (server mpdSettings mpdLock dirLock)
