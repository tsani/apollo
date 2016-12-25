{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Apollo.Types where

import Data.Aeson
import Data.Text ( Text, unpack )
import Servant.API

type ApolloApiV1
  = "tracks"
    :> "add"
      :> "youtube-dl"
        :> ReqBody '[JSON] YoutubeDlReq :> Post '[JSON] AddedTracks

type ApolloApi = "v1" :> ApolloApiV1

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

data AddedTracks
  = AddedTracks
    { addedTracks :: [FilePath]
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON AddedTracks where
  toJSON AddedTracks{..} = object [ "addedTracks" .= addedTracks ]

