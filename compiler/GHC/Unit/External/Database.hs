module GHC.Unit.External.Database (
  -- * 'ExternalUnitDatabases'
  ExternalUnitDatabases,
  emptyExternalUnitDatabases,
  insertExternalUnitDatabases,
  deleteExternalUnitDatabases,
  lookupExternalUnitDatabases,
  -- * 'UnitDatabase' and how to merge them.
  UnitDatabase (..),
  mergeDatabases,
  UnitPrecedenceMap,
  sortByPreference,
  compareByPreference,
  -- * Reading packages from disk.
  UnitDbConfig (..),
  readUnitDatabase,
  getUnitDbRefs,
  resolveUnitDatabase,
) where

import GHC.Prelude

import GHC.Data.Maybe
import GHC.Data.OsPath (OsPath)
import GHC.Data.OsPath qualified as OsPath
import GHC.Data.ShortText qualified as ST
import GHC.Driver.DynFlags
import GHC.Platform.ArchOS
import GHC.Types.Unique.Map
import GHC.Unit.Database
import GHC.Unit.Info
import GHC.Unit.Types
import GHC.Utils.Error
import GHC.Utils.Exception
import GHC.Utils.Logger
import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import Control.Monad
import Data.Char
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Ord
import Data.Set (Set)
import Data.Set qualified as Set
import System.Directory
import System.Environment (getEnv)
import System.FilePath as FilePath

-- ----------------------------------------------------------------------------
-- ExternalUnitDatabases
-- ----------------------------------------------------------------------------

-- | Caches unit databases in-memory.
data ExternalUnitDatabases unit = ExternalUnitDatabases
  { eud_cachedDatabases :: !(Map OsPath (UnitDatabase unit))
  }

emptyExternalUnitDatabases :: ExternalUnitDatabases unit
emptyExternalUnitDatabases =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.empty
    }

insertExternalUnitDatabases :: UnitDatabase unit -> ExternalUnitDatabases unit -> ExternalUnitDatabases unit
insertExternalUnitDatabases unit_db eud =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.insert (unitDatabasePath unit_db) unit_db (eud_cachedDatabases eud)
    }

deleteExternalUnitDatabases :: OsPath -> ExternalUnitDatabases unit -> ExternalUnitDatabases unit
deleteExternalUnitDatabases unit_db_path eud =
  ExternalUnitDatabases
    { eud_cachedDatabases = Map.delete unit_db_path (eud_cachedDatabases eud)
    }

lookupExternalUnitDatabases :: OsPath -> ExternalUnitDatabases unit -> Maybe (UnitDatabase unit)
lookupExternalUnitDatabases key eud =
  Map.lookup key (eud_cachedDatabases eud)

-- ----------------------------------------------------------------------------
-- UnitDatabase
-- ----------------------------------------------------------------------------

-- | Unit database entry.
data UnitDatabase unit = UnitDatabase
  { unitDatabasePath :: OsPath
  , unitDatabaseUnits :: [GenUnitInfo unit]
  }

instance (Outputable u) => Outputable (UnitDatabase u) where
  ppr (UnitDatabase fp _u) = text "DB:" <+> ppr fp

-- ----------------------------------------------------------------------------
--
-- Merging databases
--

-- | For each unit, a mapping from uid -> i indicates that this
-- unit was brought into GHC by the ith @-package-db@ flag on
-- the command line.  We use this mapping to make sure we prefer
-- units that were defined later on the command line, if there
-- is an ambiguity.
type UnitPrecedenceMap = UniqMap UnitId Int

-- | Given a list of databases, merge them together, where
-- units with the same unit id in later databases override
-- earlier ones.  This does NOT check if the resulting database
-- makes sense (that's done by 'validateDatabase').
mergeDatabases :: Logger -> [UnitDatabase UnitId]
               -> IO (UnitInfoMap, UnitPrecedenceMap)
mergeDatabases logger = foldM merge (emptyUniqMap, emptyUniqMap) . zip [1..]
  where
    merge (pkg_map, prec_map) (i, UnitDatabase db_path db) = do
      debugTraceMsg logger 2 $
          text "loading package database" <+> ppr db_path
      when (logVerbAtLeast logger 2) $
        forM_ (Set.toList override_set) $ \pkg ->
            debugTraceMsg logger 2 $
                text "package" <+> ppr pkg <+>
                text "overrides a previously defined package"
      return (pkg_map', prec_map')
     where
      db_map = mk_pkg_map db
      mk_pkg_map = listToUniqMap . map (\p -> (unitId p, p))

      -- The set of UnitIds which appear in both db and pkgs.  These are the
      -- ones that get overridden.  Compute this just to give some
      -- helpful debug messages at -v2
      override_set :: Set UnitId
      override_set = Set.intersection (nonDetUniqMapToKeySet db_map)
                                      (nonDetUniqMapToKeySet pkg_map)

      -- Now merge the sets together (NB: in case of duplicate,
      -- first argument preferred)
      pkg_map' :: UnitInfoMap
      pkg_map' = pkg_map `plusUniqMap` db_map

      prec_map' :: UnitPrecedenceMap
      prec_map' = prec_map `plusUniqMap` (mapUniqMap (const i) db_map)

-- | This sorts a list of packages, putting "preferred" packages first.
-- See 'compareByPreference' for the semantics of "preference".
sortByPreference :: UnitPrecedenceMap -> [UnitInfo] -> [UnitInfo]
sortByPreference prec_map = sortBy (flip (compareByPreference prec_map))

-- | Returns 'GT' if @pkg@ should be preferred over @pkg'@ when picking
-- which should be "active".  Here is the order of preference:
--
--      1. First, prefer the latest version
--      2. If the versions are the same, prefer the package that
--      came in the latest package database.
--
-- Pursuant to #12518, we could change this policy to, for example, remove
-- the version preference, meaning that we would always prefer the units
-- in later unit database.
compareByPreference
    :: UnitPrecedenceMap
    -> UnitInfo
    -> UnitInfo
    -> Ordering
compareByPreference prec_map pkg pkg'
  = case comparing unitPackageVersion pkg pkg' of
        GT -> GT
        EQ | Just prec  <- lookupUniqMap prec_map (unitId pkg)
           , Just prec' <- lookupUniqMap prec_map (unitId pkg')
           -- Prefer the unit from the later DB flag (i.e., higher
           -- precedence)
           -> compare prec prec'
           | otherwise
           -> EQ
        LT -> LT

-- -----------------------------------------------------------------------------
-- Reading the unit database(s)

data UnitDbConfig = UnitDbConfig
  { unitDbConfigFlagsDB :: [PackageDBFlag]
  , unitDbConfigProgramName :: String
  , unitDbConfigDBName :: FilePath
  , unitDbConfigPlatformArchOS :: ArchOS
  , unitDbConfigGlobalDB :: FilePath
  , unitDbConfigGHCDir :: FilePath
  }

getUnitDbRefs :: UnitDbConfig -> IO [PkgDbRef]
getUnitDbRefs cfg = do
  let system_conf_refs = [UserPkgDb, GlobalPkgDb]

  e_pkg_path <- tryIO (getEnv $ map toUpper (unitDbConfigProgramName cfg) ++ "_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | Just (xs, x) <- snocView path, isSearchPathSeparator x
         -> map PkgDbPath (OsPath.splitSearchPath (OsPath.unsafeEncodeUtf xs)) ++ system_conf_refs
         | otherwise
         -> map PkgDbPath (OsPath.splitSearchPath (OsPath.unsafeEncodeUtf path))

  -- Apply the package DB-related flags from the command line to get the
  -- final list of package DBs.
  --
  -- Notes on ordering:
  --  * The list of flags is reversed (later ones first)
  --  * We work with the package DB list in "left shadows right" order
  --  * and finally reverse it at the end, to get "right shadows left"
  --
  return $ reverse (foldr doFlag base_conf_refs (unitDbConfigFlagsDB cfg))
 where
  doFlag (PackageDB p) dbs = p : dbs
  doFlag NoUserPackageDB dbs = filter isNotUser dbs
  doFlag NoGlobalPackageDB dbs = filter isNotGlobal dbs
  doFlag ClearPackageDBs _ = []

  isNotUser UserPkgDb = False
  isNotUser _ = True

  isNotGlobal GlobalPkgDb = False
  isNotGlobal _ = True

-- | Return the path of a package database from a 'PkgDbRef'. Return 'Nothing'
-- when the user database filepath is expected but the latter doesn't exist.
--
-- NB: This logic is reimplemented in Cabal, so if you change it,
-- make sure you update Cabal. (Or, better yet, dump it in the
-- compiler info so Cabal can use the info.)
resolveUnitDatabase :: UnitDbConfig -> PkgDbRef -> IO (Maybe OsPath)
resolveUnitDatabase cfg GlobalPkgDb = return $ Just $ OsPath.unsafeEncodeUtf $ unitDbConfigGlobalDB cfg
resolveUnitDatabase cfg UserPkgDb = runMaybeT $ do
  dir <- versionedAppDir (unitDbConfigProgramName cfg) (unitDbConfigPlatformArchOS cfg)
  let pkgconf = dir </> unitDbConfigDBName cfg
  exist <- tryMaybeT $ doesDirectoryExist pkgconf
  if exist then return (OsPath.unsafeEncodeUtf pkgconf) else mzero
resolveUnitDatabase _ (PkgDbPath name) = return $ Just name

-- | Read the 'UnitDatabase' at the given location.
readUnitDatabase :: Logger -> UnitDbConfig -> OsPath -> IO (UnitDatabase UnitId)
readUnitDatabase logger cfg conf_file = do
  isdir <- OsPath.doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then readDirStyleUnitInfo conf_file
       else do
            isfile <- OsPath.doesFileExist conf_file
            if isfile
               then do
                 mpkgs <- tryReadOldFileStyleUnitInfo
                 case mpkgs of
                   Just pkgs -> return pkgs
                   Nothing   -> throwGhcExceptionIO $ InstallationError $
                      "ghc no longer supports single-file style package " ++
                      "databases (" ++ show conf_file ++
                      ") use 'ghc-pkg init' to create the database with " ++
                      "the correct format."
               else throwGhcExceptionIO $ InstallationError $
                      "can't find a package database at " ++ show conf_file

  let
      -- Fix #16360: remove trailing slash from conf_file before calculating pkgroot
      conf_file' = OsPath.dropTrailingPathSeparator conf_file
      top_dir = OsPath.unsafeEncodeUtf (unitDbConfigGHCDir cfg)
      pkgroot = OsPath.takeDirectory conf_file'
      pkg_configs1 = map (mungeUnitInfo top_dir pkgroot . mapUnitInfo (\(UnitKey x) -> UnitId x) . mkUnitKeyInfo)
                         proto_pkg_configs
  --
  return $ UnitDatabase conf_file' pkg_configs1
  where
    readDirStyleUnitInfo :: OsPath -> IO [DbUnitInfo]
    readDirStyleUnitInfo conf_dir = do
      let filename = conf_dir OsPath.</> (OsPath.unsafeEncodeUtf "package.cache")
      cache_exists <- OsPath.doesFileExist filename
      if cache_exists
        then do
          debugTraceMsg logger 2 $ text "Using binary package database:" <+> ppr filename
          readPackageDbForGhc filename
        else do
          -- If there is no package.cache file, we check if the database is not
          -- empty by inspecting if the directory contains any .conf file. If it
          -- does, something is wrong and we fail. Otherwise we assume that the
          -- database is empty.
          debugTraceMsg logger 2 $ text "There is no package.cache in"
                      <+> ppr conf_dir
                       <> text ", checking if the database is empty"
          db_empty <- all (not . OsPath.isSuffixOf (OsPath.unsafeEncodeUtf ".conf"))
                   <$> OsPath.getDirectoryContents conf_dir
          if db_empty
            then do
              debugTraceMsg logger 3 $ text "There are no .conf files in"
                          <+> ppr conf_dir <> text ", treating"
                          <+> text "package database as empty"
              return []
            else
              throwGhcExceptionIO $ InstallationError $
                "there is no package.cache in " ++ show conf_dir ++
                " even though package database is not empty"


    -- Single-file style package dbs have been deprecated for some time, but
    -- it turns out that Cabal was using them in one place. So this is a
    -- workaround to allow older Cabal versions to use this newer ghc.
    -- We check if the file db contains just "[]" and if so, we look for a new
    -- dir-style db in conf_file.d/, ie in a dir next to the given file.
    -- We cannot just replace the file with a new dir style since Cabal still
    -- assumes it's a file and tries to overwrite with 'writeFile'.
    -- ghc-pkg also cooperates with this workaround.
    tryReadOldFileStyleUnitInfo = do
      content <- readFile (OsPath.unsafeDecodeUtf conf_file) `catchIO` \_ -> return ""
      if take 2 content == "[]"
        then do
          let conf_dir = conf_file OsPath.<.> OsPath.unsafeEncodeUtf "d"
          direxists <- OsPath.doesDirectoryExist conf_dir
          if direxists
             then do debugTraceMsg logger 2 (text "Ignoring old file-style db and trying:" <+> ppr conf_dir)
                     liftM Just (readDirStyleUnitInfo conf_dir)
             else return (Just []) -- ghc-pkg will create it when it's updated
        else return Nothing

mungeUnitInfo :: OsPath -> OsPath
                   -> UnitInfo -> UnitInfo
mungeUnitInfo top_dir pkgroot =
    mungeBytecodeLibFields
  . mungeLibDirFields
  . mungeUnitInfoPaths (ST.pack (OsPath.unsafeDecodeUtf top_dir)) (ST.pack (OsPath.unsafeDecodeUtf pkgroot))

mungeLibDirFields :: UnitInfo -> UnitInfo
mungeLibDirFields pkg =
    pkg {
      unitLibraryDynDirs = case unitLibraryDynDirs pkg of
         [] -> unitLibraryDirs pkg
         ds -> ds
      , unitLibraryDirsStatic = case unitLibraryDirsStatic pkg of
         [] -> unitLibraryDirs pkg
         ds -> ds
    }

-- | Default to using library-dirs if bytecode library dirs is not explicitly set.
mungeBytecodeLibFields :: UnitInfo -> UnitInfo
mungeBytecodeLibFields pkg =
    pkg {
      unitLibraryBytecodeDirs = case unitLibraryBytecodeDirs pkg of
         [] -> unitLibraryDirs pkg
         ds -> ds
    }
