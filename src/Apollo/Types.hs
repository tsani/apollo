{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Apollo.Types where

import Data.Aeson
import Data.Text ( Text, unpack )
import Servant.API

type ApolloApi
  = "download" :> ReqBody '[JSON] YoutubeDlReq :> Post '[JSON] ()

data YoutubeDlReq
  = YoutubeDlReq
    { downloadPath :: FilePath
    , downloadUrl :: Text
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON YoutubeDlReq where
  parseJSON (Object v) = YoutubeDlReq
    <$> (unpack <$> v .: "path")
    <*> v .: "url"
  parseJSON _ = fail "cannot parse download request from non object"

