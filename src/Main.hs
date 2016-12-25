module Main where

import Apollo.Server

import Control.Concurrent.MVar
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Network.Wai.Middleware.RequestLogger ( logStdoutDev )
import Servant

main :: IO ()
main = do
  dirLock <- newMVar ()
  let port = 8082
  putStrLn $ "Listening on port " ++ show port
  run port (logStdoutDev $ app dirLock)

app :: MVar () -> Application
app dirLock = serve (Proxy :: Proxy ApolloApi) (server dirLock)
