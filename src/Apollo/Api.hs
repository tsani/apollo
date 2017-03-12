module Apollo.Api where

import Apollo.Monad ( ApolloError )
import Apollo.Types
import Apollo.Types.Servant

import Data.Aeson
import Data.List.NonEmpty ( NonEmpty )
import Data.Proxy

import Servant.API

type ApolloApiV1 k
  =
    "tracks"
      :> "add"
        :> "youtube-dl" :> (
          ReqBody '[JSON] YoutubeDlReq
            :> Post '[JSON] (NonEmpty Entry)
        :<|>
          "async" :> (
            ReqBody '[JSON] YoutubeDlReq
              :> Post '[JSON] JobQueueResult
          :<|>
            StrictQueryParam "id" k
              :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty Entry))
        )
      )
  :<|>
    "playlist" :> (
      Capture "tracks" [PlaylistItemId] :> Delete '[JSON] Playlist
    :<|>
      QueryParam "position" PositionBetweenTracks
        :> ReqBody '[JSON] (NonEmpty FilePath)
        :> Put '[JSON] (NonEmpty PlaylistItemId)
    :<|>
      Get '[JSON] Playlist
    )
  :<|>
    "status"
      :> Get '[JSON] PlayerStatus
  :<|>
    "transcode" :> (
      ReqBody '[JSON] TranscodeReq :> Post '[JSON] TrackIdW
    :<|>
      StrictQueryParam "trackId" TrackId
        :> StrictQueryParam "params" TranscodingParameters
        :> Get '[OctetStream] LazyTrackData
    :<|>
      "async" :> (
        ReqBody '[JSON] (NonEmpty TranscodeReq)
          :> Post '[JSON] JobQueueResult
      :<|>
        StrictQueryParam "id" k
          :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty TrackId))
      )
    )
  :<|>
    "archive" :> (
      ReqBody '[JSON] (NonEmpty ArchiveEntry)
        :> Post '[JSON] ArchivalResult
    :<|>
      StrictQueryParam "id" ArchiveId
        :> Get '[OctetStream] LazyArchiveData
    :<|>
      "async" :> (
        ReqBody '[JSON] (NonEmpty ArchiveEntry)
          :> Post '[JSON] JobQueueResult
      :<|>
        StrictQueryParam "id" k
          :> Get '[JSON] (JobQueryResult (ApolloError k) ArchivalResult)
      )
    )
  :<|>
    "test_async" :> (
      ReqBody '[JSON] [Int] :> Post '[JSON] JobQueueResult
    :<|>
      Capture "id" k
        :> Get '[JSON] (JobQueryResult (ApolloError k) Foo)
    )

newtype Foo = Foo Int

instance ToJSON Foo where
  toJSON (Foo n) = object [ "foo" .= n ]

type V1 = "v1"

type ApolloApi k = V1 :> ApolloApiV1 k

-- | A proxy for the Apollo API type.
apolloApi :: Proxy k -> Proxy (ApolloApi k)
apolloApi _ = Proxy

-- | A function for generating typesafe links within the API.
apiLink :: forall e k.
  (IsElem e (ApolloApi k), HasLink e)
  => Proxy k -> Proxy e -> MkLink e
apiLink p = safeLink (apolloApi p)

apiLink' :: (IsElem e (ApolloApi JobId), HasLink e) => Proxy e -> MkLink e
apiLink' = apiLink (Proxy @JobId)

type QueryAsyncTranscode k
  = V1
  :> "transcode"
  :> "async"
  :> StrictQueryParam "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty TrackId))

type QueryAsyncArchive k
  = V1
  :> "archive"
  :> "async"
  :> StrictQueryParam "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) ArchivalResult)

type QueryAsyncTest k
  = V1
  :> "test_async"
  :> Capture "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) Foo)

type QueryAsyncYoutubeDl k
  = V1
  :> "tracks"
  :> "add"
  :> "youtube-dl"
  :> StrictQueryParam "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty Entry))
