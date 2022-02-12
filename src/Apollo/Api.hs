module Apollo.Api where

import Apollo.Monad ( ApolloError )
import Apollo.Types
import Apollo.Types.Servant

import Data.Aeson
import Data.List.NonEmpty ( NonEmpty )
import Data.Proxy

import Servant.API

type AsyncQueryEndpoint k a =
  Capture "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) a)

type YoutubeDlEndpoint a =
  ReqBody '[JSON] YoutubeDlReq
  :> QueryParam "ignore-404" Bool
  :> QueryParam "extract-audio" Bool
  :> QueryParam "add-metadata" Bool
  :> QueryParam "audio-format" String
  :> Post '[JSON] a

type CreateArchiveEndpoint a =
  ReqBody '[JSON] (NonEmpty ArchiveEntry)
  :> QueryParam "compress" Compressor
  :> Post '[JSON] a

-- | The Apollo API version 1, parameterized by the type of keys used to access
-- jobs.
type ApolloApiV1 k =
  "tracks" :> "add" :> "youtube-dl" :> (
    YoutubeDlEndpoint (NonEmpty Entry)
    :<|>
    "async" :> (
      YoutubeDlEndpoint JobQueueResult
      :<|>
      AsyncQueryEndpoint k (NonEmpty Entry)))
  :<|>
  "playlist" :> (
    Capture "tracks" [PlaylistItemId] :> Delete '[JSON] Playlist
    :<|>
      QueryParam "position" PositionBetweenTracks
        :> ReqBody '[JSON] (NonEmpty FilePath)
        :> Put '[JSON] (NonEmpty PlaylistItemId)
    :<|> Get '[JSON] Playlist
    :<|> QueryParam "name" String :> Post '[JSON] SaveResult)
  :<|>
  "status" :> Get '[JSON] PlayerStatus
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
      AsyncQueryEndpoint k (NonEmpty TrackId)))
  :<|>
  "archive" :> (
    CreateArchiveEndpoint ArchivalResult
    :<|>
    StrictQueryParam "id" ArchiveId
    :> Get '[OctetStream] LazyArchiveData
    :<|>
    "async" :> (
      CreateArchiveEndpoint JobQueueResult
      :<|>
      AsyncQueryEndpoint k ArchivalResult))


type V1 = "v1"

type ApolloApi k = V1 :> ApolloApiV1 k

-- | A proxy for the Apollo API type.
apolloApi :: Proxy k -> Proxy (ApolloApi k)
apolloApi _ = Proxy

-- | A function for generating typesafe links within the API.
apiLink :: forall e k.
  (IsElem e (ApolloApi k), HasLink e)
  => Proxy k -> Proxy e -> MkLink e Link
apiLink p = safeLink (apolloApi p)

apiLink' :: (IsElem e (ApolloApi JobId), HasLink e) => Proxy e -> MkLink e Link
apiLink' = apiLink (Proxy @JobId)

type QueryAsyncTranscode k
  = V1
  :> "transcode"
  :> "async"
  :> Capture "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty TrackId))

type QueryAsyncArchive k
  = V1
  :> "archive"
  :> "async"
  :> Capture "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) ArchivalResult)

type QueryAsyncYoutubeDl k
  = V1
  :> "tracks"
  :> "add"
  :> "youtube-dl"
  :> "async"
  :> Capture "id" k
  :> Get '[JSON] (JobQueryResult (ApolloError k) (NonEmpty Entry))
