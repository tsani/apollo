{-|
 -
 - The apollo monad is a free monad with a few very high-level actions.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Apollo.Monad
( -- ** Interpreters
  ApolloIO
, runApolloIO
, ApolloError(..)
  -- ** Derived actions
, forUpdateLen
, forUpdate
  -- * Misc
, ApolloSettings(..)
, ServerSettings(..)
, MpdSettings(..)
, MpdLock
, makeMpdLock
, doTranscode
, doMakeArchive
) where

import Apollo.Archive
import Apollo.Types.Job
import Apollo.Monad.Types
import Apollo.Misc ( (<#>), untilM )
import Apollo.Transcoding
import Apollo.Track
import Apollo.Types
import qualified Apollo.YoutubeDl as Y

import qualified Codec.Archive.Zip as Zip
import Control.Concurrent.MVar
import Control.Monad ( foldM, forM_ )
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.Binary as Bin
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromJust )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Traversable ( for )
import qualified Network.MPD as MPD
import Network.URI
import qualified System.Directory as Dir
import System.FilePath ( (</>) )
import qualified System.IO.Temp as D

------------------------------------------------------------------------
--- ApolloIO target monad                                            ---
------------------------------------------------------------------------

-- | Result of 'runApolloIO'. This is just an IO monad with some
-- distinguished errors 'ApolloError'.
newtype ApolloIO k e r a
  = ApolloIO
    { unApolloIO
      :: ExceptT (ApolloError k) (ReaderT (ApolloSettings k e r) IO) a
    }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError (ApolloError k)
    , MonadReader (ApolloSettings k e r)
    , MonadIO
    , MonadBase IO
    )

-- | Interpret @ApolloIO@ in @IO@.
runApolloIO
  :: ApolloSettings k e r
  -> ApolloIO k e r a
  -> IO (Either (ApolloError k) a)
runApolloIO s = flip runReaderT s . runExceptT . unApolloIO

-- we can't derive 'MonadBaseControl IO' for 'ApolloIO k' because the deriving
-- mechanism would generate an unjustified recursive call in the StM type
-- family.
-- Instead, since 'ApolloIO k' is the same as 'ExceptT (ApolloError k) IO', we
-- can essentially inline the instance code for ExceptT and add the
-- (un)wrapping of the ApolloIO newtype manually.
instance MonadBaseControl IO (ApolloIO k e r) where
  type StM (ApolloIO k e r) a = Either (ApolloError k) a

  liftBaseWith f = ask >>= \r -> liftIO $ f (runApolloIO r)
  restoreM s = ApolloIO $ ExceptT $ ReaderT $ const $ pure s

instance (Bounded k, Ord k, Enum k) => MonadJobControl (ApolloIO k e r) where
  type ControlJobId (ApolloIO k e r) = k
  type ControlJobError (ApolloIO k e r) = e
  type ControlJobResult (ApolloIO k e r) = r

  queryAsyncJob i = withJobBank $ \b ->
    liftIO (checkJob i b) >>= maybe (throwError (NoSuchJob i)) pure

  startAsyncJob p j = withJobBank $ \b ->
    liftIO (startJob p b j)

instance (Enum k, Ord k, Bounded k) => MonadApollo (ApolloIO k e r) where
  youtubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) s a = do
    let dp = T.unpack musicDirT
    let url = T.unpack dlUrl

    tmpDir <- asks apolloTmpDir
    entries <- withTempDirectory tmpDir "apollo." $ \dirPath -> do
      outputFiles <- do
        xs <- do
          Y.youtubeDlProgress (Just dirPath) s url a
          liftIO (Dir.listDirectory ".")
        maybe (throwError EmptyYoutubeDlResult) pure $ N.nonEmpty xs

      md <- asks apolloMusicDirP
      liftIO $ do
        -- create the destination directory if necessary
        Dir.createDirectoryIfMissing True (md </> dp)
        -- copy the output files into the destination directory
        forM_ outputFiles $ \outputFile -> do
          Dir.copyFile (dirPath </> outputFile) (md </> dp </> outputFile)

      pure (Entry musicDir . T.pack <$> outputFiles)

    liftIO (putStrLn $ "updating MPD database in " ++ dp)
    updateDB (Just $ fromString dp)

    pure entries

  getPlayerStatus = runMpdLockedEx $ do
    MPD.Status{..} <- MPD.status
    MPD.Stats{..} <- MPD.stats
    pure PlayerStatus
      { psState = PlaybackState stState
      , psPlaylistLength = stPlaylistLength
      , psTrackId = PlaylistItemId . (\(MPD.Id i) -> i) <$> stSongID
      , psNextTrackId = PlaylistItemId . (\(MPD.Id i) -> i) <$> stNextSongID
      , psUptime = stsUptime
      , psPlaytime = stsPlaytime
      , psLastUpdateTime = stsDbUpdate
      }

  enqueueTracks pos tracks = do
    rs <- runMpdLockedEx $ do
      liftIO $ print pos
      enqueuePos <- case pos of
        FromBeginning (nonZero -> n) -> pure $ Just $ if n < 0 then 0 else n
        FromEnd (nonZero -> n) -> do
          l <- fromIntegral . MPD.stPlaylistLength <$> MPD.status
          pure $ Just $ if n < 0 then l + n else l
        FromPlaying (nonZero -> n) -> do
          fmap (if n < 0 then (+ (n+1)) else (+ n)) . MPD.stSongPos <$> MPD.status

      liftIO (putStrLn $ "current song pos: " ++ show enqueuePos)

      for tracks $
        \track ->
          PlaylistItemId . (\(MPD.Id i) -> i)
            <$> MPD.addId (fromString track) enqueuePos

    liftIO (putStrLn $ "entries: " ++ show rs)

    pure rs

  deleteTracks items = runMpdLockedEx $
    for_ items $ \(PlaylistItemId i) -> MPD.deleteId (MPD.Id i)

  getPlaylist = do
    (entries, st) <- runMpdLockedEx $ (,)
      <$> ((unsafeSongToPlaylistEntry <$>) <$> MPD.playlistInfo Nothing)
      <*> (MPD.stSongID <$> MPD.status)
    pure Playlist
      { playlistTracks = entries
      , playlistNowPlaying = st >>= (\(MPD.Id i) -> pure $ PlaylistItemId i)
      }

  makeTranscode track params = do
    md <- asks apolloMusicDirP
    td <- asks apolloTranscodeDirP
    liftIO $ doTranscode md td track params

  readTrackLazily track = do
    md <- asks apolloMusicDirP
    liftIO $ readTrackLazilyIO (md </> track)

  readTranscodeLazily t params = do
    td <- asks apolloTranscodeDirP
    liftIO (getExistingTranscode (Just td) t params) >>= \case
      Just transcodePath -> liftIO $ readTrackLazilyIO transcodePath
      Nothing -> throwError $ NoSuchTranscode t params

  readArchiveLazily (ArchiveId (Sha1Hash b)) = do
    ad <- asks apolloArchiveDirP
    liftIO $ readArchiveLazilyIO (ad </> C8.unpack b)

  makeArchive entries = do
    md <- asks apolloMusicDirP
    td <- asks apolloTranscodeDirP
    ad <- asks apolloArchiveDirP
    either throwError pure =<< liftIO (runJob $ doMakeArchive md td ad entries)

  getStaticUrl res = case res of
    StaticArchive (ArchiveId (Sha1Hash b)) -> do
      s <- asks apolloStaticServerSettings
      let (Url baseUrl) = serverSettingsBaseUrl s
      pure $ Url $ (baseUrl <> "/archives/" <> decodeUtf8 b)
    StaticTranscode tid params -> error "StaticTranscode" tid params
    StaticTrack p -> error "StaticTrack" p

  getApiLink u = do
    s <- asks apolloApiServerSettings
    let f = ('/' :)
    let sc = T.unpack (serverScheme s) ++ ":"
    let d = T.unpack (serverDomain s)
    let p = ":" ++ show (serverPort s)
    let auth = Just URIAuth { uriUserInfo = "", uriRegName = d, uriPort = p }
    let uri = u { uriScheme = sc, uriAuthority = auth, uriPath = f $ uriPath u }
    let x = uriToString id uri ""
    pure $ Url (T.pack x)

-- | Convert an MPD @Song@ into a @PlaylistEntry@ using 'fromJust' for the
-- components that are wrapped in 'Maybe'. This is only safe to do when the
-- songs come from a playlist, in which case the components wrapped in @Maybe@
-- are guaranteed to be 'Just'.
unsafeSongToPlaylistEntry :: MPD.Song -> PlaylistEntry
unsafeSongToPlaylistEntry MPD.Song{..} = PlaylistEntry
  { entryPath = MPD.toString sgFilePath
  , entryId = PlaylistItemId . (\(MPD.Id i) -> i) $ fromJust sgId
  , entryPosition = PlaylistPosition $ fromJust sgIndex
  , entryDuration = Seconds sgLength
  }

doTranscode
  :: FilePath -- ^ music dir
  -> FilePath -- ^ transcode dir
  -> FilePath -- ^ track path
  -> TranscodingParameters -> IO TrackId
doTranscode musicDirP transcodeDirP track params = do
  d <- readTrackIO (musicDirP </> track)
  let tid = trackId d
  putStrLn $ "computed track ID " ++ show tid
  mp <- getExistingTranscode (Just transcodeDirP) tid params
  putStrLn "got existing transcode"
  _ <- case mp of
    Just transcodePath -> pure (transcodeDirP </> transcodePath)
      -- then the transcode exists, so no work to do.
    Nothing -> do -- no transcode exists, so we have to make one
      let dir = transcodeDirectoryFor tid params
      Dir.createDirectoryIfMissing True (transcodeDirP </> dir)
      transcode (musicDirP </> track) (transcodeDirP </> dir) params
  pure tid

doMakeArchive
  :: Traversable t
  => FilePath -- ^ music dir
  -> FilePath -- ^ transcode dir
  -> FilePath -- ^ archive dir
  -> t ArchiveEntry
  -> Job (ApolloError k) ArchiveId
doMakeArchive musicDirP transcodeDirP archiveDirP entries = do
  let archiveId@(ArchiveId (Sha1Hash idB)) = makeArchiveId entries
  let getExisting = getExistingArchive (Just archiveDirP) archiveId
  let l = length entries

  v <- liftIO (newIORef 0)

  liftIO getExisting >>= \case
    Just _ -> pure () -- archive already exists; nothing to do
    Nothing -> do
      let forFoldM i xs g = foldM g i xs
      archive <- forFoldM Zip.emptyArchive entries $ \a e -> do
        -- compute the path to read from, and the path to put in the archive
        (srcP, dstP) <- case e of
          ArchiveTrack p -> pure (musicDirP </> p, "raw" </> p)
          ArchiveTranscode tid params -> do
            m <- liftIO $ getExistingTranscode (Just transcodeDirP) tid params
            p <- maybe (jobError $ NoSuchTranscode tid params) pure m
            pure (p, "transcoded" </> transcodeDirectoryFor tid params)

        i <- liftIO $ atomicModifyIORef v (\i -> (i + 1, i))
        reportProgress (Progress i l)

        let opts = [Zip.OptVerbose, Zip.OptLocation dstP False]
        liftIO $ Zip.addFilesToArchive opts a [srcP]

      let archivePath = archiveDirP </> T.unpack (decodeUtf8 idB)
      let utf8str = encodeUtf8 . T.pack
      liftIO $ Bin.encodeFile
        archivePath
        archive
          { Zip.zComment = LBS.fromStrict (utf8str "Apollo archive " <> idB)
          }

  pure archiveId

------------------------------------------------------------------------
--- Helpers                                                          ---
------------------------------------------------------------------------
--
-- These are mainly lifted IO actions for specific tasks.

withJobBank
  :: (JobBank k e r -> ApolloIO k e r (JobBank k e r, a))
  -> ApolloIO k e r a
withJobBank f = asks apolloJobBank >>= \b ->
  liftBaseWith (\runInBase ->
    modifyMVar b $ \j -> do
      either ((j,) . Left) (Right <$>) <$> runInBase (f j)
  ) >>= restoreM

withTempDirectory
  :: String
  -> String
  -> (FilePath -> ApolloIO k e r a)
  -> ApolloIO k e r a
withTempDirectory x y action =
  liftBaseWith
    (\runInBase -> D.withTempDirectory x y (\p -> runInBase $ action p))
  >>= restoreM

withMpdLock' :: ApolloIO k e r a -> ApolloIO k e r a
withMpdLock' m = do
  l <- asks apolloMpdLock
  liftBaseWith (\runInBase -> withMpdLock l $ runInBase m) >>= restoreM

runMpd :: MpdSettings -> MPD.MPD b -> IO (MPD.Response b)
runMpd MpdSettings{..} = MPD.withMPDEx mpdHost mpdPort mpdPassword

runMpdLockedEx :: MPD.MPD b -> ApolloIO k e r b
runMpdLockedEx m = withMpdLock' $ do
  s <- asks apolloMpdSettings
  liftIO (runMpd s m) >>= \case
    Left e -> throwError $ ApolloMpdError (MpdError e)
    Right x -> pure x

updateDB :: Maybe MPD.Path -> ApolloIO k e r ()
updateDB p = do
  j <- runMpdLockedEx $ MPD.update p
  -- loop until we discover that either our job isn't running, or that a
  -- different update job is running
  untilM $ runMpdLockedEx $ MPD.status <#> \MPD.Status{..} ->
    case stUpdatingDb of
      Just j' -> j' /= j
      Nothing -> True
