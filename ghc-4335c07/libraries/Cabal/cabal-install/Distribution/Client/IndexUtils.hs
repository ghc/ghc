{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@community.haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Distribution.Client.IndexUtils (
  getIndexFileAge,
  getInstalledPackages,
  Configure.getInstalledPackagesMonitorFiles,
  getSourcePackages,
  getSourcePackagesMonitorFiles,

  IndexState(..),
  getSourcePackagesAtIndexState,

  Index(..),
  PackageEntry(..),
  parsePackageIndex,
  updateRepoIndexCache,
  updatePackageIndexCacheFile,
  writeIndexTimestamp,
  currentIndexTimestamp,
  readCacheStrict, -- only used by soon-to-be-obsolete sandbox code

  BuildTreeRefType(..), refTypeFromTypeCode, typeCodeFromRefType
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Tar.Index as Tar
import qualified Distribution.Client.Tar as Tar
import Distribution.Client.IndexUtils.Timestamp
import Distribution.Client.Types
import Distribution.Verbosity

import Distribution.Package
         ( PackageId, PackageIdentifier(..), mkPackageName
         , Package(..), packageVersion, packageName )
import Distribution.Types.Dependency
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import Distribution.PackageDescription
         ( GenericPackageDescription )
import Distribution.Simple.Compiler
         ( Compiler, PackageDBStack )
import Distribution.Simple.Program
         ( ProgramDb )
import qualified Distribution.Simple.Configure as Configure
         ( getInstalledPackages, getInstalledPackagesMonitorFiles )
import Distribution.Version
         ( mkVersion, intersectVersionRanges )
import Distribution.Text
         ( display, simpleParse )
import Distribution.Simple.Utils
         ( die', warn, info )
import Distribution.Client.Setup
         ( RepoContext(..) )

import Distribution.PackageDescription.Parsec
         ( parseGenericPackageDescriptionMaybe )
import qualified Distribution.PackageDescription.Parsec as PackageDesc.Parse

import           Distribution.Solver.Types.PackageIndex (PackageIndex)
import qualified Distribution.Solver.Types.PackageIndex as PackageIndex
import           Distribution.Solver.Types.SourcePackage

import qualified Data.Map as Map
import Control.DeepSeq
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS.Char8
import qualified Data.ByteString.Char8 as BSS
import Data.ByteString.Lazy (ByteString)
import Distribution.Client.GZipUtils (maybeDecompress)
import Distribution.Client.Utils ( byteStringToFilePath
                                 , tryFindAddSourcePackageDesc )
import Distribution.Compat.Binary
import Distribution.Compat.Exception (catchIO)
import Distribution.Compat.Time (getFileAge, getModTime)
import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath
         ( (</>), (<.>), takeExtension, replaceExtension, splitDirectories, normalise )
import System.FilePath.Posix as FilePath.Posix
         ( takeFileName )
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO)
import System.IO.Error (isDoesNotExistError)

import qualified Hackage.Security.Client    as Sec
import qualified Hackage.Security.Util.Some as Sec

-- | Reduced-verbosity version of 'Configure.getInstalledPackages'
getInstalledPackages :: Verbosity -> Compiler
                     -> PackageDBStack -> ProgramDb
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packageDbs progdb =
    Configure.getInstalledPackages verbosity' comp packageDbs progdb
  where
    verbosity'  = lessVerbose verbosity


-- | Get filename base (i.e. without file extension) for index-related files
--
-- /Secure/ cabal repositories use a new extended & incremental
-- @01-index.tar@. In order to avoid issues resulting from clobbering
-- new/old-style index data, we save them locally to different names.
--
-- Example: Use @indexBaseName repo <.> "tar.gz"@ to compute the 'FilePath' of the
-- @00-index.tar.gz@/@01-index.tar.gz@ file.
indexBaseName :: Repo -> FilePath
indexBaseName repo = repoLocalDir repo </> fn
  where
    fn = case repo of
           RepoSecure {} -> "01-index"
           RepoRemote {} -> "00-index"
           RepoLocal  {} -> "00-index"

------------------------------------------------------------------------
-- Reading the source package index
--

-- Note: 'data IndexState' is defined in
-- "Distribution.Client.IndexUtils.Timestamp" to avoid import cycles

-- | 'IndexStateInfo' contains meta-information about the resulting
-- filtered 'Cache' 'after applying 'filterCache' according to a
-- requested 'IndexState'.
data IndexStateInfo = IndexStateInfo
    { isiMaxTime  :: !Timestamp
    -- ^ 'Timestamp' of maximum/latest 'Timestamp' in the current
    -- filtered view of the cache.
    --
    -- The following property holds
    --
    -- > filterCache (IndexState (isiMaxTime isi)) cache == (cache, isi)
    --

    , isiHeadTime :: !Timestamp
    -- ^ 'Timestamp' equivalent to 'IndexStateHead', i.e. the latest
    -- known 'Timestamp'; 'isiHeadTime' is always greater or equal to
    -- 'isiMaxTime'.
    }

emptyStateInfo :: IndexStateInfo
emptyStateInfo = IndexStateInfo nullTimestamp nullTimestamp

-- | Filters a 'Cache' according to an 'IndexState'
-- specification. Also returns 'IndexStateInfo' describing the
-- resulting index cache.
--
-- Note: 'filterCache' is idempotent in the 'Cache' value
filterCache :: IndexState -> Cache -> (Cache, IndexStateInfo)
filterCache IndexStateHead cache = (cache, IndexStateInfo{..})
  where
    isiMaxTime  = cacheHeadTs cache
    isiHeadTime = cacheHeadTs cache
filterCache (IndexStateTime ts0) cache0 = (cache, IndexStateInfo{..})
  where
    cache = Cache { cacheEntries = ents, cacheHeadTs = isiMaxTime }
    isiHeadTime = cacheHeadTs cache0
    isiMaxTime  = maximumTimestamp (map cacheEntryTimestamp ents)
    ents = filter ((<= ts0) . cacheEntryTimestamp) (cacheEntries cache0)

-- | Read a repository index from disk, from the local files specified by
-- a list of 'Repo's.
--
-- All the 'SourcePackage's are marked as having come from the appropriate
-- 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
getSourcePackages :: Verbosity -> RepoContext -> IO SourcePackageDb
getSourcePackages verbosity repoCtxt =
    getSourcePackagesAtIndexState verbosity repoCtxt Nothing

-- | Variant of 'getSourcePackages' which allows getting the source
-- packages at a particular 'IndexState'.
--
-- Current choices are either the latest (aka HEAD), or the index as
-- it was at a particular time.
--
-- TODO: Enhance to allow specifying per-repo 'IndexState's and also
-- report back per-repo 'IndexStateInfo's (in order for @new-freeze@
-- to access it)
getSourcePackagesAtIndexState :: Verbosity -> RepoContext -> Maybe IndexState
                           -> IO SourcePackageDb
getSourcePackagesAtIndexState verbosity repoCtxt _
  | null (repoContextRepos repoCtxt) = do
      -- In the test suite, we routinely don't have any remote package
      -- servers, so don't bleat about it
      warn (verboseUnmarkOutput verbosity) $
        "No remote package servers have been specified. Usually " ++
        "you would have one specified in the config file."
      return SourcePackageDb {
        packageIndex       = mempty,
        packagePreferences = mempty
      }
getSourcePackagesAtIndexState verbosity repoCtxt mb_idxState = do
  let describeState IndexStateHead        = "most recent state"
      describeState (IndexStateTime time) = "historical state as of " ++ display time

  pkgss <- forM (repoContextRepos repoCtxt) $ \r -> do
      let rname = maybe "" remoteRepoName $ maybeRepoRemote r
      info verbosity ("Reading available packages of " ++ rname ++ "...")

      idxState <- case mb_idxState of
        Just idxState -> do
          info verbosity $ "Using " ++ describeState idxState ++
            " as explicitly requested (via command line / project configuration)"
          return idxState
        Nothing -> do
          mb_idxState' <- readIndexTimestamp (RepoIndex repoCtxt r)
          case mb_idxState' of
            Nothing -> do
              info verbosity "Using most recent state (could not read timestamp file)"
              return IndexStateHead
            Just idxState -> do
              info verbosity $ "Using " ++ describeState idxState ++
                " specified from most recent cabal update"
              return idxState

      unless (idxState == IndexStateHead) $
          case r of
            RepoLocal path -> warn verbosity ("index-state ignored for old-format repositories (local repository '" ++ path ++ "')")
            RepoRemote {} -> warn verbosity ("index-state ignored for old-format (remote repository '" ++ rname ++ "')")
            RepoSecure {} -> pure ()

      let idxState' = case r of
            RepoSecure {} -> idxState
            _             -> IndexStateHead

      (pis,deps,isi) <- readRepoIndex verbosity repoCtxt r idxState'

      case idxState' of
        IndexStateHead -> do
            info verbosity ("index-state("++rname++") = " ++
                              display (isiHeadTime isi))
            return ()
        IndexStateTime ts0 -> do
            when (isiMaxTime isi /= ts0) $
                if ts0 > isiMaxTime isi
                    then warn verbosity $
                                   "Requested index-state" ++ display ts0
                                ++ " is newer than '" ++ rname ++ "'!"
                                ++ " Falling back to older state ("
                                ++ display (isiMaxTime isi) ++ ")."
                    else info verbosity $
                                   "Requested index-state " ++ display ts0
                                ++ " does not exist in '"++rname++"'!"
                                ++ " Falling back to older state ("
                                ++ display (isiMaxTime isi) ++ ")."
            info verbosity ("index-state("++rname++") = " ++
                              display (isiMaxTime isi) ++ " (HEAD = " ++
                              display (isiHeadTime isi) ++ ")")

      pure (pis,deps)

  let (pkgs, prefs) = mconcat pkgss
      prefs' = Map.fromListWith intersectVersionRanges
                 [ (name, range) | Dependency name range <- prefs ]
  _ <- evaluate pkgs
  _ <- evaluate prefs'
  return SourcePackageDb {
    packageIndex       = pkgs,
    packagePreferences = prefs'
  }

readCacheStrict :: Verbosity -> Index -> (PackageEntry -> pkg) -> IO ([pkg], [Dependency])
readCacheStrict verbosity index mkPkg = do
    updateRepoIndexCache verbosity index
    cache <- readIndexCache verbosity index
    withFile (indexFile index) ReadMode $ \indexHnd ->
      packageListFromCache verbosity mkPkg indexHnd cache ReadPackageIndexStrict

-- | Read a repository index from disk, from the local file specified by
-- the 'Repo'.
--
-- All the 'SourcePackage's are marked as having come from the given 'Repo'.
--
-- This is a higher level wrapper used internally in cabal-install.
--
readRepoIndex :: Verbosity -> RepoContext -> Repo -> IndexState
              -> IO (PackageIndex UnresolvedSourcePackage, [Dependency], IndexStateInfo)
readRepoIndex verbosity repoCtxt repo idxState =
  handleNotFound $ do
    warnIfIndexIsOld =<< getIndexFileAge repo
    updateRepoIndexCache verbosity (RepoIndex repoCtxt repo)
    readPackageIndexCacheFile verbosity mkAvailablePackage
                              (RepoIndex repoCtxt repo)
                              idxState

  where
    mkAvailablePackage pkgEntry =
      SourcePackage {
        packageInfoId      = pkgid,
        packageDescription = packageDesc pkgEntry,
        packageSource      = case pkgEntry of
          NormalPackage _ _ _ _       -> RepoTarballPackage repo pkgid Nothing
          BuildTreeRef  _  _ _ path _ -> LocalUnpackedPackage path,
        packageDescrOverride = case pkgEntry of
          NormalPackage _ _ pkgtxt _ -> Just pkgtxt
          _                          -> Nothing
      }
      where
        pkgid = packageId pkgEntry

    handleNotFound action = catchIO action $ \e -> if isDoesNotExistError e
      then do
        case repo of
          RepoRemote{..} -> warn verbosity $ errMissingPackageList repoRemote
          RepoSecure{..} -> warn verbosity $ errMissingPackageList repoRemote
          RepoLocal{..}  -> warn verbosity $
               "The package list for the local repo '" ++ repoLocalDir
            ++ "' is missing. The repo is invalid."
        return (mempty,mempty,emptyStateInfo)
      else ioError e

    isOldThreshold = 15 --days
    warnIfIndexIsOld dt = do
      when (dt >= isOldThreshold) $ case repo of
        RepoRemote{..} -> warn verbosity $ errOutdatedPackageList repoRemote dt
        RepoSecure{..} -> warn verbosity $ errOutdatedPackageList repoRemote dt
        RepoLocal{..}  -> return ()

    errMissingPackageList repoRemote =
         "The package list for '" ++ remoteRepoName repoRemote
      ++ "' does not exist. Run 'cabal update' to download it."
    errOutdatedPackageList repoRemote dt =
         "The package list for '" ++ remoteRepoName repoRemote
      ++ "' is " ++ shows (floor dt :: Int) " days old.\nRun "
      ++ "'cabal update' to get the latest list of available packages."

-- | Return the age of the index file in days (as a Double).
getIndexFileAge :: Repo -> IO Double
getIndexFileAge repo = getFileAge $ indexBaseName repo <.> "tar"

-- | A set of files (or directories) that can be monitored to detect when
-- there might have been a change in the source packages.
--
getSourcePackagesMonitorFiles :: [Repo] -> [FilePath]
getSourcePackagesMonitorFiles repos =
    concat [ [ indexBaseName repo <.> "cache"
             , indexBaseName repo <.> "timestamp" ]
           | repo <- repos ]

-- | It is not necessary to call this, as the cache will be updated when the
-- index is read normally. However you can do the work earlier if you like.
--
updateRepoIndexCache :: Verbosity -> Index -> IO ()
updateRepoIndexCache verbosity index =
    whenCacheOutOfDate index $ do
      updatePackageIndexCacheFile verbosity index

whenCacheOutOfDate :: Index -> IO () -> IO ()
whenCacheOutOfDate index action = do
  exists <- doesFileExist $ cacheFile index
  if not exists
    then action
    else do
      indexTime <- getModTime $ indexFile index
      cacheTime <- getModTime $ cacheFile index
      when (indexTime > cacheTime) action

------------------------------------------------------------------------
-- Reading the index file
--

-- | An index entry is either a normal package, or a local build tree reference.
data PackageEntry =
  NormalPackage  PackageId GenericPackageDescription ByteString BlockNo
  | BuildTreeRef BuildTreeRefType
                 PackageId GenericPackageDescription FilePath   BlockNo

-- | A build tree reference is either a link or a snapshot.
data BuildTreeRefType = SnapshotRef | LinkRef
                      deriving (Eq,Generic)

instance Binary BuildTreeRefType

refTypeFromTypeCode :: Tar.TypeCode -> BuildTreeRefType
refTypeFromTypeCode t
  | t == Tar.buildTreeRefTypeCode      = LinkRef
  | t == Tar.buildTreeSnapshotTypeCode = SnapshotRef
  | otherwise                          =
    error "Distribution.Client.IndexUtils.refTypeFromTypeCode: unknown type code"

typeCodeFromRefType :: BuildTreeRefType -> Tar.TypeCode
typeCodeFromRefType LinkRef     = Tar.buildTreeRefTypeCode
typeCodeFromRefType SnapshotRef = Tar.buildTreeSnapshotTypeCode

instance Package PackageEntry where
  packageId (NormalPackage  pkgid _ _ _) = pkgid
  packageId (BuildTreeRef _ pkgid _ _ _) = pkgid

packageDesc :: PackageEntry -> GenericPackageDescription
packageDesc (NormalPackage  _ descr _ _) = descr
packageDesc (BuildTreeRef _ _ descr _ _) = descr

-- | Parse an uncompressed \"00-index.tar\" repository index file represented
-- as a 'ByteString'.
--

data PackageOrDep = Pkg PackageEntry | Dep Dependency

-- | Read @00-index.tar.gz@ and extract @.cabal@ and @preferred-versions@ files
--
-- We read the index using 'Tar.read', which gives us a lazily constructed
-- 'TarEntries'. We translate it to a list of entries using  'tarEntriesList',
-- which preserves the lazy nature of 'TarEntries', and finally 'concatMap' a
-- function over this to translate it to a list of IO actions returning
-- 'PackageOrDep's. We can use 'lazySequence' to turn this into a list of
-- 'PackageOrDep's, still maintaining the lazy nature of the original tar read.
parsePackageIndex :: Verbosity -> ByteString -> [IO (Maybe PackageOrDep)]
parsePackageIndex verbosity = concatMap (uncurry extract) . tarEntriesList . Tar.read
  where
    extract :: BlockNo -> Tar.Entry -> [IO (Maybe PackageOrDep)]
    extract blockNo entry = tryExtractPkg ++ tryExtractPrefs
      where
        tryExtractPkg = do
          mkPkgEntry <- maybeToList $ extractPkg verbosity entry blockNo
          return $ fmap (fmap Pkg) mkPkgEntry

        tryExtractPrefs = do
          prefs' <- maybeToList $ extractPrefs entry
          fmap (return . Just . Dep) prefs'

-- | Turn the 'Entries' data structure from the @tar@ package into a list,
-- and pair each entry with its block number.
--
-- NOTE: This preserves the lazy nature of 'Entries': the tar file is only read
-- as far as the list is evaluated.
tarEntriesList :: Show e => Tar.Entries e -> [(BlockNo, Tar.Entry)]
tarEntriesList = go 0
  where
    go !_ Tar.Done         = []
    go !_ (Tar.Fail e)     = error ("tarEntriesList: " ++ show e)
    go !n (Tar.Next e es') = (n, e) : go (Tar.nextEntryOffset e n) es'

extractPkg :: Verbosity -> Tar.Entry -> BlockNo -> Maybe (IO (Maybe PackageEntry))
extractPkg verbosity entry blockNo = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeExtension fileName == ".cabal"
    -> case splitDirectories (normalise fileName) of
        [pkgname,vers,_] -> case simpleParse vers of
          Just ver -> Just . return $ Just (NormalPackage pkgid descr content blockNo)
            where
              pkgid  = PackageIdentifier (mkPackageName pkgname) ver
              parsed = parseGenericPackageDescriptionMaybe (BS.toStrict content)
              descr = case parsed of
                  Just d  -> d
                  Nothing -> error $ "Couldn't read cabal file "
                                    ++ show fileName
          _ -> Nothing
        _ -> Nothing

  Tar.OtherEntryType typeCode content _
    | Tar.isBuildTreeRefTypeCode typeCode ->
      Just $ do
        let path = byteStringToFilePath content
        dirExists <- doesDirectoryExist path
        result <- if not dirExists then return Nothing
                  else do
                    cabalFile <- tryFindAddSourcePackageDesc verbosity path "Error reading package index."
                    descr     <- PackageDesc.Parse.readGenericPackageDescription normal cabalFile
                    return . Just $ BuildTreeRef (refTypeFromTypeCode typeCode) (packageId descr)
                                                 descr path blockNo
        return result

  _ -> Nothing

  where
    fileName = Tar.entryPath entry

extractPrefs :: Tar.Entry -> Maybe [Dependency]
extractPrefs entry = case Tar.entryContent entry of
  Tar.NormalFile content _
     | takeFileName entrypath == "preferred-versions"
    -> Just prefs
    where
      entrypath = Tar.entryPath entry
      prefs     = parsePreferredVersions content
  _ -> Nothing

parsePreferredVersions :: ByteString -> [Dependency]
parsePreferredVersions = mapMaybe simpleParse
                       . filter (not . isPrefixOf "--")
                       . lines
                       . BS.Char8.unpack -- TODO: Are we sure no unicode?

------------------------------------------------------------------------
-- Reading and updating the index cache
--

-- | Variation on 'sequence' which evaluates the actions lazily
--
-- Pattern matching on the result list will execute just the first action;
-- more generally pattern matching on the first @n@ '(:)' nodes will execute
-- the first @n@ actions.
lazySequence :: [IO a] -> IO [a]
lazySequence = unsafeInterleaveIO . go
  where
    go []     = return []
    go (x:xs) = do x'  <- x
                   xs' <- lazySequence xs
                   return (x' : xs')

-- | A lazy unfolder for lookup operations which return the current
-- value and (possibly) the next key
lazyUnfold :: (k -> IO (v, Maybe k)) -> k -> IO [(k,v)]
lazyUnfold step = goLazy . Just
  where
    goLazy s = unsafeInterleaveIO (go s)

    go Nothing  = return []
    go (Just k) = do
        (v, mk') <- step k
        vs' <- goLazy mk'
        return ((k,v):vs')

-- | Which index do we mean?
data Index =
    -- | The main index for the specified repository
    RepoIndex RepoContext Repo

    -- | A sandbox-local repository
    -- Argument is the location of the index file
  | SandboxIndex FilePath

indexFile :: Index -> FilePath
indexFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "tar"
indexFile (SandboxIndex index)   = index

cacheFile :: Index -> FilePath
cacheFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "cache"
cacheFile (SandboxIndex index)   = index `replaceExtension` "cache"

timestampFile :: Index -> FilePath
timestampFile (RepoIndex _ctxt repo) = indexBaseName repo <.> "timestamp"
timestampFile (SandboxIndex index)   = index `replaceExtension` "timestamp"

-- | Return 'True' if 'Index' uses 01-index format (aka secure repo)
is01Index :: Index -> Bool
is01Index (RepoIndex _ repo) = case repo of
                                 RepoSecure {} -> True
                                 RepoRemote {} -> False
                                 RepoLocal  {} -> False
is01Index (SandboxIndex _)   = False


updatePackageIndexCacheFile :: Verbosity -> Index -> IO ()
updatePackageIndexCacheFile verbosity index = do
    info verbosity ("Updating index cache file " ++ cacheFile index ++ " ...")
    withIndexEntries verbosity index $ \entries -> do
      let !maxTs = maximumTimestamp (map cacheEntryTimestamp entries)
          cache = Cache { cacheHeadTs  = maxTs
                        , cacheEntries = entries
                        }
      writeIndexCache index cache
      info verbosity ("Index cache updated to index-state "
                      ++ display (cacheHeadTs cache))

-- | Read the index (for the purpose of building a cache)
--
-- The callback is provided with list of cache entries, which is guaranteed to
-- be lazily constructed. This list must ONLY be used in the scope of the
-- callback; when the callback is terminated the file handle to the index will
-- be closed and further attempts to read from the list will result in (pure)
-- I/O exceptions.
--
-- In the construction of the index for a secure repo we take advantage of the
-- index built by the @hackage-security@ library to avoid reading the @.tar@
-- file as much as possible (we need to read it only to extract preferred
-- versions). This helps performance, but is also required for correctness:
-- the new @01-index.tar.gz@ may have multiple versions of preferred-versions
-- files, and 'parsePackageIndex' does not correctly deal with that (see #2956);
-- by reading the already-built cache from the security library we will be sure
-- to only read the latest versions of all files.
--
-- TODO: It would be nicer if we actually incrementally updated @cabal@'s
-- cache, rather than reconstruct it from zero on each update. However, this
-- would require a change in the cache format.
withIndexEntries :: Verbosity -> Index -> ([IndexCacheEntry] -> IO a) -> IO a
withIndexEntries _ (RepoIndex repoCtxt repo@RepoSecure{..}) callback =
    repoContextWithSecureRepo repoCtxt repo $ \repoSecure ->
      Sec.withIndex repoSecure $ \Sec.IndexCallbacks{..} -> do
        -- Incrementally (lazily) read all the entries in the tar file in order,
        -- including all revisions, not just the last revision of each file
        indexEntries <- lazyUnfold indexLookupEntry (Sec.directoryFirst indexDirectory)
        callback [ cacheEntry
                 | (dirEntry, indexEntry) <- indexEntries
                 , cacheEntry <- toCacheEntries dirEntry indexEntry ]
  where
    toCacheEntries :: Sec.DirectoryEntry -> Sec.Some Sec.IndexEntry
                   -> [IndexCacheEntry]
    toCacheEntries dirEntry (Sec.Some sie) =
        case Sec.indexEntryPathParsed sie of
          Nothing                            -> [] -- skip unrecognized file
          Just (Sec.IndexPkgMetadata _pkgId) -> [] -- skip metadata
          Just (Sec.IndexPkgCabal pkgId)     -> force
              [CachePackageId pkgId blockNo timestamp]
          Just (Sec.IndexPkgPrefs _pkgName)  -> force
              [ CachePreference dep blockNo timestamp
              | dep <- parsePreferredVersions (Sec.indexEntryContent sie)
              ]
      where
        blockNo = Sec.directoryEntryBlockNo dirEntry
        timestamp = fromMaybe (error "withIndexEntries: invalid timestamp") $
                              epochTimeToTimestamp $ Sec.indexEntryTime sie

withIndexEntries verbosity index callback = do -- non-secure repositories
    withFile (indexFile index) ReadMode $ \h -> do
      bs          <- maybeDecompress `fmap` BS.hGetContents h
      pkgsOrPrefs <- lazySequence $ parsePackageIndex verbosity bs
      callback $ map toCache (catMaybes pkgsOrPrefs)
  where
    toCache :: PackageOrDep -> IndexCacheEntry
    toCache (Pkg (NormalPackage pkgid _ _ blockNo)) = CachePackageId pkgid blockNo nullTimestamp
    toCache (Pkg (BuildTreeRef refType _ _ _ blockNo)) = CacheBuildTreeRef refType blockNo
    toCache (Dep d) = CachePreference d 0 nullTimestamp

data ReadPackageIndexMode = ReadPackageIndexStrict
                          | ReadPackageIndexLazyIO

readPackageIndexCacheFile :: Package pkg
                          => Verbosity
                          -> (PackageEntry -> pkg)
                          -> Index
                          -> IndexState
                          -> IO (PackageIndex pkg, [Dependency], IndexStateInfo)
readPackageIndexCacheFile verbosity mkPkg index idxState = do
    cache0    <- readIndexCache verbosity index
    indexHnd <- openFile (indexFile index) ReadMode
    let (cache,isi) = filterCache idxState cache0
    (pkgs,deps) <- packageIndexFromCache verbosity mkPkg indexHnd cache ReadPackageIndexLazyIO
    pure (pkgs,deps,isi)


packageIndexFromCache :: Package pkg
                      => Verbosity
                     -> (PackageEntry -> pkg)
                      -> Handle
                      -> Cache
                      -> ReadPackageIndexMode
                      -> IO (PackageIndex pkg, [Dependency])
packageIndexFromCache verbosity mkPkg hnd cache mode = do
     (pkgs, prefs) <- packageListFromCache verbosity mkPkg hnd cache mode
     pkgIndex <- evaluate $ PackageIndex.fromList pkgs
     return (pkgIndex, prefs)

-- | Read package list
--
-- The result package releases and preference entries are guaranteed
-- to be unique.
--
-- Note: 01-index.tar is an append-only index and therefore contains
-- all .cabal edits and preference-updates. The masking happens
-- here, i.e. the semantics that later entries in a tar file mask
-- earlier ones is resolved in this function.
packageListFromCache :: Verbosity
                     -> (PackageEntry -> pkg)
                     -> Handle
                     -> Cache
                     -> ReadPackageIndexMode
                     -> IO ([pkg], [Dependency])
packageListFromCache verbosity mkPkg hnd Cache{..} mode = accum mempty [] mempty cacheEntries
  where
    accum !srcpkgs btrs !prefs [] = return (Map.elems srcpkgs ++ btrs, Map.elems prefs)

    accum srcpkgs btrs prefs (CachePackageId pkgid blockno _ : entries) = do
      -- Given the cache entry, make a package index entry.
      -- The magic here is that we use lazy IO to read the .cabal file
      -- from the index tarball if it turns out that we need it.
      -- Most of the time we only need the package id.
      ~(pkg, pkgtxt) <- unsafeInterleaveIO $ do
        pkgtxt <- getEntryContent blockno
        pkg    <- readPackageDescription pkgtxt
        return (pkg, pkgtxt)
      case mode of
        ReadPackageIndexLazyIO -> pure ()
        ReadPackageIndexStrict -> evaluate pkg *> evaluate pkgtxt *> pure ()
      let srcpkg = mkPkg (NormalPackage pkgid pkg pkgtxt blockno)
      accum (Map.insert pkgid srcpkg srcpkgs) btrs prefs entries

    accum srcpkgs btrs prefs (CacheBuildTreeRef refType blockno : entries) = do
      -- We have to read the .cabal file eagerly here because we can't cache the
      -- package id for build tree references - the user might edit the .cabal
      -- file after the reference was added to the index.
      path <- liftM byteStringToFilePath . getEntryContent $ blockno
      pkg  <- do let err = "Error reading package index from cache."
                 file <- tryFindAddSourcePackageDesc verbosity path err
                 PackageDesc.Parse.readGenericPackageDescription normal file
      let srcpkg = mkPkg (BuildTreeRef refType (packageId pkg) pkg path blockno)
      accum srcpkgs (srcpkg:btrs) prefs entries

    accum srcpkgs btrs prefs (CachePreference pref@(Dependency pn _) _ _ : entries) =
      accum srcpkgs btrs (Map.insert pn pref prefs) entries

    getEntryContent :: BlockNo -> IO ByteString
    getEntryContent blockno = do
      entry <- Tar.hReadEntry hnd blockno
      case Tar.entryContent entry of
        Tar.NormalFile content _size -> return content
        Tar.OtherEntryType typecode content _size
          | Tar.isBuildTreeRefTypeCode typecode
          -> return content
        _ -> interror "unexpected tar entry type"

    readPackageDescription :: ByteString -> IO GenericPackageDescription
    readPackageDescription content =
      case parseGenericPackageDescriptionMaybe (BS.toStrict content) of
        Just gpd -> return gpd
        Nothing  -> interror "failed to parse .cabal file"

    interror :: String -> IO a
    interror msg = die' verbosity $ "internal error when reading package index: " ++ msg
                      ++ "The package index or index cache is probably "
                      ++ "corrupt. Running cabal update might fix it."

------------------------------------------------------------------------
-- Index cache data structure
--

-- | Read the 'Index' cache from the filesystem
--
-- If a corrupted index cache is detected this function regenerates
-- the index cache and then reattempt to read the index once (and
-- 'die's if it fails again).
readIndexCache :: Verbosity -> Index -> IO Cache
readIndexCache verbosity index = do
    cacheOrFail <- readIndexCache' index
    case cacheOrFail of
      Left msg -> do
          warn verbosity $ concat
              [ "Parsing the index cache failed (", msg, "). "
              , "Trying to regenerate the index cache..."
              ]

          updatePackageIndexCacheFile verbosity index

          either (die' verbosity) (return . hashConsCache) =<< readIndexCache' index

      Right res -> return (hashConsCache res)

-- | Read the 'Index' cache from the filesystem without attempting to
-- regenerate on parsing failures.
readIndexCache' :: Index -> IO (Either String Cache)
readIndexCache' index
  | is01Index index = decodeFileOrFail' (cacheFile index)
  | otherwise       = liftM (Right .read00IndexCache) $
                      BSS.readFile (cacheFile index)

-- | Write the 'Index' cache to the filesystem
writeIndexCache :: Index -> Cache -> IO ()
writeIndexCache index cache
  | is01Index index = encodeFile (cacheFile index) cache
  | otherwise       = writeFile (cacheFile index) (show00IndexCache cache)

-- | Write the 'IndexState' to the filesystem
writeIndexTimestamp :: Index -> IndexState -> IO ()
writeIndexTimestamp index st
  = writeFile (timestampFile index) (display st)

-- | Read out the "current" index timestamp, i.e., what
-- timestamp you would use to revert to this version
currentIndexTimestamp :: Verbosity -> RepoContext -> Repo -> IO Timestamp
currentIndexTimestamp verbosity repoCtxt r = do
    mb_is <- readIndexTimestamp (RepoIndex repoCtxt r)
    case mb_is of
      Just (IndexStateTime ts) -> return ts
      _ -> do
        (_,_,isi) <- readRepoIndex verbosity repoCtxt r IndexStateHead
        return (isiHeadTime isi)

-- | Read the 'IndexState' from the filesystem
readIndexTimestamp :: Index -> IO (Maybe IndexState)
readIndexTimestamp index
  = fmap simpleParse (readFile (timestampFile index))
        `catchIO` \e ->
            if isDoesNotExistError e
                then return Nothing
                else ioError e

-- | Optimise sharing of equal values inside 'Cache'
--
-- c.f. https://en.wikipedia.org/wiki/Hash_consing
hashConsCache :: Cache -> Cache
hashConsCache cache0
    = cache0 { cacheEntries = go mempty mempty (cacheEntries cache0) }
  where
    -- TODO/NOTE:
    --
    -- If/when we redo the binary serialisation via e.g. CBOR and we
    -- are able to use incremental decoding, we may want to move the
    -- hash-consing into the incremental deserialisation, or
    -- alterantively even do something like
    -- http://cbor.schmorp.de/value-sharing
    --
    go _ _ [] = []
    -- for now we only optimise only CachePackageIds since those
    -- represent the vast majority
    go !pns !pvs (CachePackageId pid bno ts : rest)
        = CachePackageId pid' bno ts : go pns' pvs' rest
      where
        !pid' = PackageIdentifier pn' pv'
        (!pn',!pns') = mapIntern pn pns
        (!pv',!pvs') = mapIntern pv pvs
        PackageIdentifier pn pv = pid

    go pns pvs (x:xs) = x : go pns pvs xs

    mapIntern :: Ord k => k -> Map.Map k k -> (k,Map.Map k k)
    mapIntern k m = maybe (k,Map.insert k k m) (\k' -> (k',m)) (Map.lookup k m)

-- | Cabal caches various information about the Hackage index
data Cache = Cache
    { cacheHeadTs  :: Timestamp
      -- ^ maximum/latest 'Timestamp' among 'cacheEntries'; unless the
      -- invariant of 'cacheEntries' being in chronological order is
      -- violated, this corresponds to the last (seen) 'Timestamp' in
      -- 'cacheEntries'
    , cacheEntries :: [IndexCacheEntry]
    }

instance NFData Cache where
    rnf = rnf . cacheEntries

-- | Tar files are block structured with 512 byte blocks. Every header and file
-- content starts on a block boundary.
--
type BlockNo = Word32 -- Tar.TarEntryOffset


data IndexCacheEntry
    = CachePackageId PackageId !BlockNo !Timestamp
    | CachePreference Dependency !BlockNo !Timestamp
    | CacheBuildTreeRef !BuildTreeRefType !BlockNo
      -- NB: CacheBuildTreeRef is irrelevant for 01-index & new-build
  deriving (Eq,Generic)

instance NFData IndexCacheEntry where
    rnf (CachePackageId pkgid _ _) = rnf pkgid
    rnf (CachePreference dep _ _) = rnf dep
    rnf (CacheBuildTreeRef _ _) = ()

cacheEntryTimestamp :: IndexCacheEntry -> Timestamp
cacheEntryTimestamp (CacheBuildTreeRef _ _)  = nullTimestamp
cacheEntryTimestamp (CachePreference _ _ ts) = ts
cacheEntryTimestamp (CachePackageId _ _ ts)  = ts

----------------------------------------------------------------------------
-- new binary 01-index.cache format

instance Binary Cache where
    put (Cache headTs ents) = do
        -- magic / format version
        --
        -- NB: this currently encodes word-size implicitly; when we
        -- switch to CBOR encoding, we will have a platform
        -- independent binary encoding
        put (0xcaba1002::Word)
        put headTs
        put ents

    get = do
        magic <- get
        when (magic /= (0xcaba1002::Word)) $
            fail ("01-index.cache: unexpected magic marker encountered: " ++ show magic)
        Cache <$> get <*> get

instance Binary IndexCacheEntry

----------------------------------------------------------------------------
-- legacy 00-index.cache format

packageKey, blocknoKey, buildTreeRefKey, preferredVersionKey :: String
packageKey = "pkg:"
blocknoKey = "b#"
buildTreeRefKey     = "build-tree-ref:"
preferredVersionKey = "pref-ver:"

-- legacy 00-index.cache format
read00IndexCache :: BSS.ByteString -> Cache
read00IndexCache bs = Cache
  { cacheHeadTs  = nullTimestamp
  , cacheEntries = mapMaybe read00IndexCacheEntry $ BSS.lines bs
  }

read00IndexCacheEntry :: BSS.ByteString -> Maybe IndexCacheEntry
read00IndexCacheEntry = \line ->
  case BSS.words line of
    [key, pkgnamestr, pkgverstr, sep, blocknostr]
      | key == BSS.pack packageKey && sep == BSS.pack blocknoKey ->
      case (parseName pkgnamestr, parseVer pkgverstr [],
            parseBlockNo blocknostr) of
        (Just pkgname, Just pkgver, Just blockno)
          -> Just (CachePackageId (PackageIdentifier pkgname pkgver)
                                  blockno nullTimestamp)
        _ -> Nothing
    [key, typecodestr, blocknostr] | key == BSS.pack buildTreeRefKey ->
      case (parseRefType typecodestr, parseBlockNo blocknostr) of
        (Just refType, Just blockno)
          -> Just (CacheBuildTreeRef refType blockno)
        _ -> Nothing

    (key: remainder) | key == BSS.pack preferredVersionKey -> do
      pref <- simpleParse (BSS.unpack (BSS.unwords remainder))
      return $ CachePreference pref 0 nullTimestamp

    _  -> Nothing
  where
    parseName str
      | BSS.all (\c -> isAlphaNum c || c == '-') str
                  = Just (mkPackageName (BSS.unpack str))
      | otherwise = Nothing

    parseVer str vs =
      case BSS.readInt str of
        Nothing        -> Nothing
        Just (v, str') -> case BSS.uncons str' of
          Just ('.', str'') -> parseVer str'' (v:vs)
          Just _            -> Nothing
          Nothing           -> Just (mkVersion (reverse (v:vs)))

    parseBlockNo str =
      case BSS.readInt str of
        Just (blockno, remainder)
          | BSS.null remainder -> Just (fromIntegral blockno)
        _                      -> Nothing

    parseRefType str =
      case BSS.uncons str of
        Just (typeCode, remainder)
          | BSS.null remainder && Tar.isBuildTreeRefTypeCode typeCode
            -> Just (refTypeFromTypeCode typeCode)
        _   -> Nothing

-- legacy 00-index.cache format
show00IndexCache :: Cache -> String
show00IndexCache Cache{..} = unlines $ map show00IndexCacheEntry cacheEntries

show00IndexCacheEntry :: IndexCacheEntry -> String
show00IndexCacheEntry entry = unwords $ case entry of
   CachePackageId pkgid b _ -> [ packageKey
                               , display (packageName pkgid)
                               , display (packageVersion pkgid)
                               , blocknoKey
                               , show b
                               ]
   CacheBuildTreeRef tr b   -> [ buildTreeRefKey
                               , [typeCodeFromRefType tr]
                               , show b
                               ]
   CachePreference dep _ _  -> [ preferredVersionKey
                               , display dep
                               ]
