{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent.MVar
import Control.Monad.IO.Class ( liftIO )
import Data.Aeson
import qualified Data.Text as T
import Network.Wai ( Application )
import Network.Wai.Handler.Warp ( run )
import Servant
import System.Directory ( createDirectoryIfMissing, withCurrentDirectory )
import System.FilePath
import System.Process ( callProcess )

data DownloadReq
  = DownloadReq
    { downloadPath :: FilePath
    , downloadUrl :: T.Text
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON DownloadReq where
  parseJSON (Object v) = DownloadReq <$> (T.unpack <$> v .: "path") <*> v .: "url"
  parseJSON _ = fail "cannot parse download request from non object"

type ApolloApi
  = "download" :> ReqBody '[JSON] DownloadReq :> Post '[JSON] ()

server :: MVar () -> Server ApolloApi
server dirLock = serveDownload where
  serveDownload :: DownloadReq -> Handler ()
  serveDownload DownloadReq{..} = liftIO $ do
    createDirectoryIfMissing True downloadPath
    withMVar dirLock $ \ _ -> withCurrentDirectory downloadPath $ do
      callProcess "youtube-dl" ["--extract-audio", T.unpack downloadUrl]

app :: MVar () -> Application
app dirLock = serve (Proxy :: Proxy ApolloApi) (server dirLock)

main :: IO ()
main = do
  dirLock <- newMVar ()
  run 8082 (app dirLock)
