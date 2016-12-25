{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Apollo.Types where

import Data.Aeson
import Data.Text ( Text, unpack )
import Data.Monoid ( (<>) )
import Servant.API
import System.FilePath ( (</>), FilePath )

type ApolloApiV1
  = "tracks"
    :> "add"
      :> "youtube-dl"
        :> ReqBody '[JSON] YoutubeDlReq :> Post '[JSON] AddedTracks

type ApolloApi = "v1" :> ApolloApiV1

data YoutubeDlReq
  = YoutubeDlReq
    { downloadPath :: MusicDir
    , downloadUrl :: YoutubeDlUrl
    }
  deriving (Eq, Ord, Read, Show)

instance FromJSON YoutubeDlReq where
  parseJSON (Object v) = YoutubeDlReq
    <$> v .: "path"
    <*> v .: "url"
  parseJSON _ = fail "cannot parse download request from non object"

data AddedTracks
  = AddedTracks
    { addedTracks :: [Entry]
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON AddedTracks where
  toJSON AddedTracks{..} = object [ "addedTracks" .= addedTracks ]

-- | A subdirectory of the music directory, typically in the format
-- @Artist/Album@.
newtype MusicDir
  = MusicDir
    { unMusicDir :: Text
    }
  deriving (Eq, Ord, Read, Show, ToJSON, FromJSON)

data Entry
  = Entry
    { entryMusicDir :: !MusicDir
    , entryName :: !Text
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON Entry where
  toJSON Entry{..} = String (unMusicDir entryMusicDir <> "/" <> entryName)

entryToPath :: Entry -> FilePath
entryToPath Entry{..} = unpack (unMusicDir entryMusicDir) </> unpack entryName

newtype YoutubeDlUrl
  = YoutubeDlUrl
    { unYoutubeDlUrl :: Text
    }
  deriving (Eq, Ord, Read, Show, FromJSON, ToJSON)
