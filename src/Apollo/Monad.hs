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

import qualified Codec.Archive.Tar as A
import Control.Concurrent.MVar
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Control
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable ( for_ )
import qualified Data.List.NonEmpty as N
import Data.Maybe ( fromJust )
import qualified Data.Map as M
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format.ISO8601 ( iso8601Show )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Traversable ( for )
import qualified Network.MPD as MPD
import Network.URI
import qualified System.Directory as Dir
import System.FilePath ( (</>), splitExtension, addExtension )
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

instance MonadFail (ApolloIO k e r) where
  fail = throwError . Failure

-- | Because the tar archives that we use for exports have a 99-char limit on
-- the filename, we implement this ridiculus function to drop the trailing part
-- of the filename so that we max out at 99 chars.
clampFileName :: FilePath -> FilePath
clampFileName fp = case splitExtension fp of
  (fn, ex) -> addExtension (take (99 - length ex) fn) ex

instance (Enum k, Ord k, Bounded k) => MonadApollo (ApolloIO k e r) where
  youtubeDl musicDir@(MusicDir musicDirT) (YoutubeDlUrl dlUrl) s a = do
    let dp = T.unpack musicDirT
    let url = T.unpack dlUrl

    tmpDir <- asks apolloTmpDir
    entries <- withTempDirectory tmpDir "apollo." $ \dirPath -> do
      outputFiles <- do
        xs <- do
          Y.youtubeDlProgress (Just dirPath) s url a
          liftIO (Dir.listDirectory dirPath)
        maybe (throwError EmptyYoutubeDlResult) pure $ N.nonEmpty xs

      md <- asks apolloMusicDirP
      outputFiles' <- liftIO $ do
        -- create the destination directory if necessary
        Dir.createDirectoryIfMissing True (md </> dp)
        -- copy the output files into the destination directory
        forM outputFiles $ \outputFile -> do
          let outputFile' = clampFileName outputFile
          Dir.copyFile
            (dirPath </> outputFile)
            (md </> dp </> outputFile')
          pure outputFile'

      pure (Entry musicDir . T.pack <$> outputFiles')

    liftIO (putStrLn $ "updating MPD database in " ++ dp)
    updateDB (Just $ fromString dp)

    pure entries

  getPlayerStatus = runMpdLockedEx $ do
    MPD.Status{..} <- MPD.status
    MPD.Stats{..} <- MPD.stats
    songs <- M.fromList
      . concatMap (\s -> maybe [] (\i -> [(i, s)]) $ MPD.sgId s)
      <$> MPD.playlistInfo Nothing
    let nowPlaying = flip M.lookup songs =<< stSongID
    let nextPlaying = flip M.lookup songs =<< stNextSongID
    pure PlayerStatus
      { psState = PlaybackState stState
      , psPlaylistLength = stPlaylistLength
      , psTrack = basicSongInfo <$> nowPlaying
      , psNextTrack = basicSongInfo <$> nextPlaying
      , psTime = uncurry SongPlayTime <$> stTime
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

  savePlaylist mname = do
    let addSuffix = maybe id (\s s1 -> s1 ++ "-" ++ s) mname
    realName <- addSuffix <$> liftIO getNowString
    runMpdLockedEx $ MPD.save (MPD.PlaylistName (T.encodeUtf8 (T.pack realName)))
    pure realName

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

  makeArchive c entries = do
    md <- asks apolloMusicDirP
    td <- asks apolloTranscodeDirP
    ad <- asks apolloArchiveDirP
    either throwError pure =<< liftIO (runJob $ doMakeArchive c md td ad entries)

  getStaticUrl res = case res of
    StaticArchive a c -> do
      s <- asks apolloStaticServerSettings
      let (Url baseUrl) = serverSettingsBaseUrl s
      let p = getArchivePath c (Just "/archives") a
      pure $ Url $ (baseUrl <> T.pack p)
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
  :: Compressor
  -> FilePath -- ^ music dir
  -> FilePath -- ^ transcode dir
  -> FilePath -- ^ archive dir
  -> N.NonEmpty ArchiveEntry
  -> Job (ApolloError k) ArchiveId
doMakeArchive c musicDirP transcodeDirP archiveDirP entries = do
  let archiveId = makeArchiveId entries
  let p = getArchivePath c (Just archiveDirP) archiveId

  liftIO (Dir.doesFileExist p) >>= \case
    True -> pure () -- archive already exists; nothing to do
    False -> do
      liftIO . putStrLn $ "constructing archive " ++ p ++
        " with " ++ show (length entries) ++ " entries"
      es <- mapM (toFilePath musicDirP transcodeDirP) entries
      liftIO $ LBS.writeFile p . compressWith c . A.write
        =<< A.pack "." (N.toList es)

  reportProgress (Progress 1 1)

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

toFilePath :: FilePath -> FilePath -> ArchiveEntry -> Job (ApolloError k) FilePath
toFilePath md td a = case a of
  ArchiveTrack p -> pure (md </> p)
  ArchiveTranscode tid params ->
    maybe (jobError $ NoSuchTranscode tid params) pure
      =<< liftIO (getExistingTranscode (Just td) tid params)

getNowString :: IO FilePath
getNowString = iso8601Show <$> getCurrentTime
