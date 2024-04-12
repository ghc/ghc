{-
(c) The University of Glasgow, 2000-2006

-}


{-# LANGUAGE FlexibleContexts #-}

-- | Module finder
module GHC.Unit.Finder (
    FindResult(..),
    InstalledFindResult(..),
    FinderOpts(..),
    FinderCache,
    initFinderCache,
    flushFinderCaches,
    findImportedModule,
    findPluginModule,
    findExactModule,
    findHomeModule,
    findExposedPackageModule,
    mkHomeModLocation,
    mkHomeModLocation2,
    mkHiOnlyModLocation,
    mkHiPath,
    mkObjPath,
    addModuleToFinder,
    addHomeModuleToFinder,
    uncacheModule,
    mkStubPaths,

    findObjectLinkableMaybe,
    findObjectLinkable,

    -- Hash cache
    lookupFileCache
  ) where

import GHC.Prelude

import GHC.Platform.Ways

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Unit.Env
import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Unit.Home
import GHC.Unit.State
import GHC.Unit.Finder.Types

import GHC.Data.Maybe    ( expectJust )
import qualified GHC.Data.ShortText as ST

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Linker.Types
import GHC.Types.PkgQual

import GHC.Fingerprint
import GHC.Driver.Env
    ( hsc_home_unit_maybe, HscEnv(hsc_FC, hsc_dflags, hsc_unit_env) )
import GHC.Driver.Config.Finder

import Control.Monad
import Data.IORef
import qualified Data.Map as M
import qualified Data.Set as Set
import Data.Time
import Data.Traversable (for)
import System.FilePath
import qualified System.Directory.OsPath as OsPath
import System.OsPath (OsPath)
import qualified System.OsPath as OsPath

type FileExt = String   -- Filename extension

-- -----------------------------------------------------------------------------
-- The Finder

-- The Finder provides a thin filesystem abstraction to the rest of
-- the compiler.  For a given module, it can tell you where the
-- source, interface, and object files for that module live.

-- It does *not* know which particular package a module lives in.  Use
-- Packages.lookupModuleInAllUnits for that.

-- -----------------------------------------------------------------------------
-- The finder's cache


initFinderCache :: IO FinderCache
initFinderCache = FinderCache <$> newIORef emptyInstalledModuleEnv
                              <*> newIORef M.empty

-- remove all the home modules from the cache; package modules are
-- assumed to not move around during a session; also flush the file hash
-- cache
flushFinderCaches :: FinderCache -> UnitEnv -> IO ()
flushFinderCaches (FinderCache ref file_ref) ue = do
  atomicModifyIORef' ref $ \fm -> (filterInstalledModuleEnv is_ext fm, ())
  atomicModifyIORef' file_ref $ \_ -> (M.empty, ())
 where
  is_ext mod _ = not (isUnitEnvInstalledModule ue mod)

addToFinderCache :: FinderCache -> InstalledModule -> InstalledFindResult -> IO ()
addToFinderCache (FinderCache ref _) key val =
  atomicModifyIORef' ref $ \c -> (extendInstalledModuleEnv c key val, ())

removeFromFinderCache :: FinderCache -> InstalledModule -> IO ()
removeFromFinderCache (FinderCache ref _) key =
  atomicModifyIORef' ref $ \c -> (delInstalledModuleEnv c key, ())

lookupFinderCache :: FinderCache -> InstalledModule -> IO (Maybe InstalledFindResult)
lookupFinderCache (FinderCache ref _) key = do
   c <- readIORef ref
   return $! lookupInstalledModuleEnv c key

lookupFileCache :: FinderCache -> FilePath -> IO Fingerprint
lookupFileCache (FinderCache _ ref) key = do
   c <- readIORef ref
   case M.lookup key c of
     Nothing -> do
       hash <- getFileHash key
       atomicModifyIORef' ref $ \c -> (M.insert key hash c, ())
       return hash
     Just fp -> return fp

-- -----------------------------------------------------------------------------
-- The three external entry points


-- | Locate a module that was imported by the user.  We have the
-- module's name, and possibly a package name.  Without a package
-- name, this function will use the search path and the known exposed
-- packages to find the module, if a package is specified then only
-- that package is searched for the module.

findImportedModule :: HscEnv -> ModuleName -> PkgQual -> IO FindResult
findImportedModule hsc_env mod pkg_qual =
  let fc        = hsc_FC hsc_env
      mhome_unit = hsc_home_unit_maybe hsc_env
      dflags    = hsc_dflags hsc_env
      fopts     = initFinderOpts dflags
  in do
    findImportedModuleNoHsc fc fopts (hsc_unit_env hsc_env) mhome_unit mod pkg_qual

findImportedModuleNoHsc
  :: FinderCache
  -> FinderOpts
  -> UnitEnv
  -> Maybe HomeUnit
  -> ModuleName
  -> PkgQual
  -> IO FindResult
findImportedModuleNoHsc fc fopts ue mhome_unit mod_name mb_pkg =
  case mb_pkg of
    NoPkgQual  -> unqual_import
    ThisPkg uid | (homeUnitId <$> mhome_unit) == Just uid -> home_import
                | Just os <- lookup uid other_fopts -> home_pkg_import (uid, os)
                | otherwise -> pprPanic "findImportModule" (ppr mod_name $$ ppr mb_pkg $$ ppr (homeUnitId <$> mhome_unit) $$ ppr uid $$ ppr (map fst all_opts))
    OtherPkg _ -> pkg_import
  where
    all_opts = case mhome_unit of
                Nothing -> other_fopts
                Just home_unit -> (homeUnitId home_unit, fopts) : other_fopts


    home_import = case mhome_unit of
                   Just home_unit -> findHomeModule fc fopts home_unit mod_name
                   Nothing -> pure $ NoPackage (panic "findImportedModule: no home-unit")


    home_pkg_import (uid, opts)
      -- If the module is reexported, then look for it as if it was from the perspective
      -- of that package which reexports it.
      | mod_name `Set.member` finder_reexportedModules opts =
        findImportedModuleNoHsc fc opts ue (Just $ DefiniteHomeUnit uid Nothing) mod_name NoPkgQual
      | mod_name `Set.member` finder_hiddenModules opts =
        return (mkHomeHidden uid)
      | otherwise =
        findHomePackageModule fc opts uid mod_name

    -- Do not be smart and change this to `foldr orIfNotFound home_import hs` as
    -- that is not the same!! home_import is first because we need to look within ourselves
    -- first before looking at the packages in order.
    any_home_import = foldr1 orIfNotFound (home_import: map home_pkg_import other_fopts)

    pkg_import    = findExposedPackageModule fc fopts units  mod_name mb_pkg

    unqual_import = any_home_import
                    `orIfNotFound`
                    findExposedPackageModule fc fopts units mod_name NoPkgQual

    units     = case mhome_unit of
                  Nothing -> ue_units ue
                  Just home_unit -> homeUnitEnv_units $ ue_findHomeUnitEnv (homeUnitId home_unit) ue
    hpt_deps :: [UnitId]
    hpt_deps  = homeUnitDepends units
    other_fopts  = map (\uid -> (uid, initFinderOpts (homeUnitEnv_dflags (ue_findHomeUnitEnv uid ue)))) hpt_deps

-- | Locate a plugin module requested by the user, for a compiler
-- plugin.  This consults the same set of exposed packages as
-- 'findImportedModule', unless @-hide-all-plugin-packages@ or
-- @-plugin-package@ are specified.
findPluginModule :: FinderCache -> FinderOpts -> UnitState -> Maybe HomeUnit -> ModuleName -> IO FindResult
findPluginModule fc fopts units (Just home_unit) mod_name =
  findHomeModule fc fopts home_unit mod_name
  `orIfNotFound`
  findExposedPluginPackageModule fc fopts units mod_name
findPluginModule fc fopts units Nothing mod_name =
  findExposedPluginPackageModule fc fopts units mod_name

-- | Locate a specific 'Module'.  The purpose of this function is to
-- create a 'ModLocation' for a given 'Module', that is to find out
-- where the files associated with this module live.  It is used when
-- reading the interface for a module mentioned by another interface,
-- for example (a "system import").

findExactModule :: FinderCache -> FinderOpts ->  UnitEnvGraph FinderOpts -> UnitState -> Maybe HomeUnit -> InstalledModule -> IO InstalledFindResult
findExactModule fc fopts other_fopts unit_state mhome_unit mod = do
  case mhome_unit of
    Just home_unit
     | isHomeInstalledModule home_unit mod
        -> findInstalledHomeModule fc fopts (homeUnitId home_unit) (moduleName mod)
     | Just home_fopts <- unitEnv_lookup_maybe (moduleUnit mod) other_fopts
        -> findInstalledHomeModule fc home_fopts (moduleUnit mod) (moduleName mod)
    _ -> findPackageModule fc unit_state fopts mod

-- -----------------------------------------------------------------------------
-- Helpers

-- | Given a monadic actions @this@ and @or_this@, first execute
-- @this@.  If the returned 'FindResult' is successful, return
-- it; otherwise, execute @or_this@.  If both failed, this function
-- also combines their failure messages in a reasonable way.
orIfNotFound :: Monad m => m FindResult -> m FindResult -> m FindResult
orIfNotFound this or_this = do
  res <- this
  case res of
    NotFound { fr_paths = paths1, fr_mods_hidden = mh1
             , fr_pkgs_hidden = ph1, fr_unusables = u1, fr_suggestions = s1 }
     -> do res2 <- or_this
           case res2 of
             NotFound { fr_paths = paths2, fr_pkg = mb_pkg2, fr_mods_hidden = mh2
                      , fr_pkgs_hidden = ph2, fr_unusables = u2
                      , fr_suggestions = s2 }
              -> return (NotFound { fr_paths = paths1 ++ paths2
                                  , fr_pkg = mb_pkg2 -- snd arg is the package search
                                  , fr_mods_hidden = mh1 ++ mh2
                                  , fr_pkgs_hidden = ph1 ++ ph2
                                  , fr_unusables = u1 ++ u2
                                  , fr_suggestions = s1  ++ s2 })
             _other -> return res2
    _other -> return res

-- | Helper function for 'findHomeModule': this function wraps an IO action
-- which would look up @mod_name@ in the file system (the home package),
-- and first consults the 'hsc_FC' cache to see if the lookup has already
-- been done.  Otherwise, do the lookup (with the IO action) and save
-- the result in the finder cache and the module location cache (if it
-- was successful.)
homeSearchCache :: FinderCache -> UnitId -> ModuleName -> IO InstalledFindResult -> IO InstalledFindResult
homeSearchCache fc home_unit mod_name do_this = do
  let mod = mkModule home_unit mod_name
  modLocationCache fc mod do_this

findExposedPackageModule :: FinderCache -> FinderOpts -> UnitState -> ModuleName -> PkgQual -> IO FindResult
findExposedPackageModule fc fopts units mod_name mb_pkg =
  findLookupResult fc fopts
    $ lookupModuleWithSuggestions units mod_name mb_pkg

findExposedPluginPackageModule :: FinderCache -> FinderOpts -> UnitState -> ModuleName -> IO FindResult
findExposedPluginPackageModule fc fopts units mod_name =
  findLookupResult fc fopts
    $ lookupPluginModuleWithSuggestions units mod_name NoPkgQual

findLookupResult :: FinderCache -> FinderOpts -> LookupResult -> IO FindResult
findLookupResult fc fopts r = case r of
     LookupFound m pkg_conf -> do
       let im = fst (getModuleInstantiation m)
       r' <- findPackageModule_ fc fopts im (fst pkg_conf)
       case r' of
        -- TODO: ghc -M is unlikely to do the right thing
        -- with just the location of the thing that was
        -- instantiated; you probably also need all of the
        -- implicit locations from the instances
        InstalledFound loc   _ -> return (Found loc m)
        InstalledNoPackage   _ -> return (NoPackage (moduleUnit m))
        InstalledNotFound fp _ -> return (NotFound{ fr_paths = fp, fr_pkg = Just (moduleUnit m)
                                         , fr_pkgs_hidden = []
                                         , fr_mods_hidden = []
                                         , fr_unusables = []
                                         , fr_suggestions = []})
     LookupMultiple rs ->
       return (FoundMultiple rs)
     LookupHidden pkg_hiddens mod_hiddens ->
       return (NotFound{ fr_paths = [], fr_pkg = Nothing
                       , fr_pkgs_hidden = map (moduleUnit.fst) pkg_hiddens
                       , fr_mods_hidden = map (moduleUnit.fst) mod_hiddens
                       , fr_unusables = []
                       , fr_suggestions = [] })
     LookupUnusable unusable ->
       let unusables' = map get_unusable unusable
           get_unusable (_, ModUnusable r) = r
           get_unusable (_, r)             =
             pprPanic "findLookupResult: unexpected origin" (ppr r)
       in return (NotFound{ fr_paths = [], fr_pkg = Nothing
                          , fr_pkgs_hidden = []
                          , fr_mods_hidden = []
                          , fr_unusables = unusables'
                          , fr_suggestions = [] })
     LookupNotFound suggest -> do
       let suggest'
             | finder_enableSuggestions fopts = suggest
             | otherwise = []
       return (NotFound{ fr_paths = [], fr_pkg = Nothing
                       , fr_pkgs_hidden = []
                       , fr_mods_hidden = []
                       , fr_unusables = []
                       , fr_suggestions = suggest' })

modLocationCache :: FinderCache -> InstalledModule -> IO InstalledFindResult -> IO InstalledFindResult
modLocationCache fc mod do_this = do
  m <- lookupFinderCache fc mod
  case m of
    Just result -> return result
    Nothing     -> do
        result <- do_this
        addToFinderCache fc mod result
        return result

addModuleToFinder :: FinderCache -> Module -> ModLocation -> IO ()
addModuleToFinder fc mod loc = do
  let imod = toUnitId <$> mod
  addToFinderCache fc imod (InstalledFound loc imod)

-- This returns a module because it's more convenient for users
addHomeModuleToFinder :: FinderCache -> HomeUnit -> ModuleName -> ModLocation -> IO Module
addHomeModuleToFinder fc home_unit mod_name loc = do
  let mod = mkHomeInstalledModule home_unit mod_name
  addToFinderCache fc mod (InstalledFound loc mod)
  return (mkHomeModule home_unit mod_name)

uncacheModule :: FinderCache -> HomeUnit -> ModuleName -> IO ()
uncacheModule fc home_unit mod_name = do
  let mod = mkHomeInstalledModule home_unit mod_name
  removeFromFinderCache fc mod

-- -----------------------------------------------------------------------------
--      The internal workers

findHomeModule :: FinderCache -> FinderOpts -> HomeUnit -> ModuleName -> IO FindResult
findHomeModule fc fopts  home_unit mod_name = do
  let uid       = homeUnitAsUnit home_unit
  r <- findInstalledHomeModule fc fopts (homeUnitId home_unit) mod_name
  return $ case r of
    InstalledFound loc _ -> Found loc (mkHomeModule home_unit mod_name)
    InstalledNoPackage _ -> NoPackage uid -- impossible
    InstalledNotFound fps _ -> NotFound {
        fr_paths = fps,
        fr_pkg = Just uid,
        fr_mods_hidden = [],
        fr_pkgs_hidden = [],
        fr_unusables = [],
        fr_suggestions = []
      }

mkHomeHidden :: UnitId -> FindResult
mkHomeHidden uid =
  NotFound { fr_paths = []
           , fr_pkg = Just (RealUnit (Definite uid))
           , fr_mods_hidden = [RealUnit (Definite uid)]
           , fr_pkgs_hidden = []
           , fr_unusables = []
           , fr_suggestions = []}

findHomePackageModule :: FinderCache -> FinderOpts -> UnitId -> ModuleName -> IO FindResult
findHomePackageModule fc fopts  home_unit mod_name = do
  let uid       = RealUnit (Definite home_unit)
  r <- findInstalledHomeModule fc fopts home_unit mod_name
  return $ case r of
    InstalledFound loc _ -> Found loc (mkModule uid mod_name)
    InstalledNoPackage _ -> NoPackage uid -- impossible
    InstalledNotFound fps _ -> NotFound {
        fr_paths = fps,
        fr_pkg = Just uid,
        fr_mods_hidden = [],
        fr_pkgs_hidden = [],
        fr_unusables = [],
        fr_suggestions = []
      }

-- | Implements the search for a module name in the home package only.  Calling
-- this function directly is usually *not* what you want; currently, it's used
-- as a building block for the following operations:
--
--  1. When you do a normal package lookup, we first check if the module
--  is available in the home module, before looking it up in the package
--  database.
--
--  2. When you have a package qualified import with package name "this",
--  we shortcut to the home module.
--
--  3. When we look up an exact 'Module', if the unit id associated with
--  the module is the current home module do a look up in the home module.
--
--  4. Some special-case code in GHCi (ToDo: Figure out why that needs to
--  call this.)
findInstalledHomeModule :: FinderCache -> FinderOpts -> UnitId -> ModuleName -> IO InstalledFindResult
findInstalledHomeModule fc fopts home_unit mod_name =
  homeSearchCache fc home_unit mod_name $ do
    maybe_working_dir <- traverse OsPath.encodeFS (finder_workingDirectory fopts)
    import_paths <- traverse OsPath.encodeFS (finder_importPaths fopts)
    let
      home_path = case maybe_working_dir of
                   Nothing -> import_paths
                   Just fp -> augmentImports fp import_paths
    hiDir <- traverse OsPath.encodeFS (finder_hiDir fopts)
    hisuf <- OsPath.encodeFS (finder_hiSuf fopts)
    let
      hi_dir_path = case hiDir of
        Just hd -> case maybe_working_dir of
                        Nothing -> [hd]
                        Just fp -> [fp OsPath.</> hd]
        Nothing -> home_path

    source_exts <- for ["hs", "lhs", "hsig", "lhsig"] $ \ ext -> do
      ext' <- OsPath.encodeFS ext
      pure (ext', mkHomeModLocationSearched fopts mod_name ext)

    let
      mod = mkModule home_unit mod_name

      -- we use mkHomeModHiOnlyLocation instead of mkHiOnlyModLocation so that
      -- when hiDir field is set in dflags, we know to look there (see #16500)
      hi_exts =
        [ (hisuf,                     mkHomeModHiOnlyLocation fopts mod_name)
        , (addBootSuffixOsPath hisuf, mkHomeModHiOnlyLocation fopts mod_name)
        ]

         -- In compilation manager modes, we look for source files in the home
         -- package because we can compile these automatically.  In one-shot
         -- compilation mode we look for .hi and .hi-boot files only.
      (search_dirs, exts)
           | finder_lookupHomeInterfaces fopts = (hi_dir_path, hi_exts)
           | otherwise                         = (home_path, source_exts)

    -- special case for GHC.Prim; we won't find it in the filesystem.
    -- This is important only when compiling the base package (where GHC.Prim
    -- is a home module).
    if mod `installedModuleEq` gHC_PRIM
          then return (InstalledFound (error "GHC.Prim ModLocation") mod)
          else searchPathExts search_dirs mod exts

-- | Prepend the working directory to the search path.
augmentImports :: OsPath -> [OsPath] -> [OsPath]
augmentImports _work_dir [] = []
augmentImports work_dir (fp:fps) | OsPath.isAbsolute fp = fp : augmentImports work_dir fps
                                 | otherwise     = (work_dir OsPath.</> fp) : augmentImports work_dir fps

-- | Search for a module in external packages only.
findPackageModule :: FinderCache -> UnitState -> FinderOpts -> InstalledModule -> IO InstalledFindResult
findPackageModule fc unit_state fopts mod = do
  let pkg_id = moduleUnit mod
  case lookupUnitId unit_state pkg_id of
     Nothing -> return (InstalledNoPackage pkg_id)
     Just u  -> findPackageModule_ fc fopts mod u

-- | Look up the interface file associated with module @mod@.  This function
-- requires a few invariants to be upheld: (1) the 'Module' in question must
-- be the module identifier of the *original* implementation of a module,
-- not a reexport (this invariant is upheld by "GHC.Unit.State") and (2)
-- the 'UnitInfo' must be consistent with the unit id in the 'Module'.
-- The redundancy is to avoid an extra lookup in the package state
-- for the appropriate config.
findPackageModule_ :: FinderCache -> FinderOpts -> InstalledModule -> UnitInfo -> IO InstalledFindResult
findPackageModule_ fc fopts mod pkg_conf = do
  massertPpr (moduleUnit mod == unitId pkg_conf)
             (ppr (moduleUnit mod) <+> ppr (unitId pkg_conf))
  modLocationCache fc mod $

    -- special case for GHC.Prim; we won't find it in the filesystem.
    if mod `installedModuleEq` gHC_PRIM
          then return (InstalledFound (error "GHC.Prim ModLocation") mod)
          else do

    let
       tag = waysBuildTag (finder_ways fopts)

             -- hi-suffix for packages depends on the build tag.
    package_hisuf <- OsPath.encodeFS $ if null tag then "hi" else tag ++ "_hi"
    package_dynhisuf <- OsPath.encodeFS $
      waysBuildTag (addWay WayDyn (finder_ways fopts)) ++ "_hi"
    let

       mk_hi_loc = mkHiOnlyModLocation fopts package_hisuf package_dynhisuf
        -- we never look for a .hi-boot file in an external package;
        -- .hi-boot files only make sense for the home package.
    import_dirs <- traverse (OsPath.encodeFS . ST.unpack) $ unitImportDirs pkg_conf
    case import_dirs of
      [one] | finder_bypassHiFileCheck fopts -> do
            -- there's only one place that this .hi file can be, so
            -- don't bother looking for it.
            basename <- OsPath.encodeFS (moduleNameSlashes (moduleName mod))
            loc <- mk_hi_loc one basename
            return $ InstalledFound loc mod
      _otherwise ->
            searchPathExts import_dirs mod [(package_hisuf, mk_hi_loc)]

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts :: [OsPath]      -- paths to search
               -> InstalledModule -- module name
               -> [ (
                     OsPath,                             -- suffix
                     OsPath -> OsPath -> IO ModLocation  -- action
                    )
                  ]
               -> IO InstalledFindResult

searchPathExts paths mod exts = do
  basename <- OsPath.encodeFS (moduleNameSlashes (moduleName mod))
  dot <- OsPath.encodeFS "."
  let
    to_search :: [(OsPath, IO ModLocation)]
    to_search = [ (file, fn path basename)
                | path <- paths,
                  (ext,fn) <- exts,
                  let base | path == dot = basename
                           | otherwise = path OsPath.</> basename
                      file = base OsPath.<.> ext
                ]

    search [] = do
      fps <- traverse (OsPath.decodeFS . fst) to_search
      return (InstalledNotFound fps (Just (moduleUnit mod)))

    search ((file, getLoc) : rest) = do
      b <- OsPath.doesFileExist file
      if b
        then do
          loc <- getLoc
          return $ InstalledFound loc mod
        else search rest

  search to_search
  where

mkHomeModLocationSearched :: FinderOpts -> ModuleName -> FileExt
                          -> OsPath -> OsPath -> IO ModLocation
mkHomeModLocationSearched fopts mod ext path basename = do
  mkHomeModLocationOsPath fopts mod (path OsPath.</> basename) ext


-- -----------------------------------------------------------------------------
-- Constructing a home module location

-- This is where we construct the ModLocation for a module in the home
-- package, for which we have a source file.  It is called from three
-- places:
--
--  (a) Here in the finder, when we are searching for a module to import,
--      using the search path (-i option).
--
--  (b) The compilation manager, when constructing the ModLocation for
--      a "root" module (a source file named explicitly on the command line
--      or in a :load command in GHCi).
--
--  (c) The driver in one-shot mode, when we need to construct a
--      ModLocation for a source file named on the command-line.
--
-- Parameters are:
--
-- mod
--      The name of the module
--
-- path
--      (a): The search path component where the source file was found.
--      (b) and (c): "."
--
-- src_basename
--      (a): (moduleNameSlashes mod)
--      (b) and (c): The filename of the source file, minus its extension
--
-- ext
--      The filename extension of the source file (usually "hs" or "lhs").

mkHomeModLocation :: FinderOpts -> ModuleName -> FilePath -> IO ModLocation
mkHomeModLocation dflags mod src_filename =
   let (basename,extension) = splitExtension src_filename
   in mkHomeModLocation2 dflags mod basename extension

mkHomeModLocation2 :: FinderOpts
                   -> ModuleName
                   -> FilePath  -- Of source module, without suffix
                   -> String    -- Suffix
                   -> IO ModLocation
mkHomeModLocation2 fopts mod src_basename ext = do
  src_basename_os <- OsPath.encodeFS src_basename
  mkHomeModLocationOsPath fopts mod src_basename_os ext

mkHomeModLocationOsPath :: FinderOpts
                        -> ModuleName
                        -> OsPath  -- Of source module, without suffix
                        -> FileExt
                        -> IO ModLocation
mkHomeModLocationOsPath fopts mod src_basename ext = do
  mod_basename <- OsPath.encodeFS (moduleNameSlashes mod)
  obj_fn <- mkObjPath fopts src_basename mod_basename
  dyn_obj_fn <- mkDynObjPath fopts src_basename mod_basename
  hi_fn  <- mkHiPath fopts src_basename mod_basename
  dyn_hi_fn  <- mkDynHiPath fopts src_basename mod_basename
  hie_fn <- mkHiePath fopts src_basename mod_basename
  original <- OsPath.decodeFS src_basename
  pure ModLocation {
    ml_hs_file = Just (original <.> ext),
    ml_hi_file = hi_fn,
    ml_dyn_hi_file = dyn_hi_fn,
    ml_obj_file = obj_fn,
    ml_dyn_obj_file = dyn_obj_fn,
    ml_hie_file = hie_fn
  }

mkHomeModHiOnlyLocation :: FinderOpts
                        -> ModuleName
                        -> OsPath
                        -> OsPath
                        -> IO ModLocation
mkHomeModHiOnlyLocation fopts mod path basename = do
  loc <- mkHomeModLocationOsPath fopts mod (path OsPath.</> basename) ""
  pure loc { ml_hs_file = Nothing }

-- This function is used to make a ModLocation for a package module. Hence why
-- we explicitly pass in the interface file suffixes.
mkHiOnlyModLocation :: FinderOpts -> OsPath -> OsPath -> OsPath -> OsPath
                    -> IO ModLocation
mkHiOnlyModLocation fopts hisuf dynhisuf path basename = do
  let full_basename = path OsPath.</> basename
  obj_fn <- mkObjPath fopts full_basename basename
  dyn_obj_fn <- mkDynObjPath fopts full_basename basename
  hie_fn <- mkHiePath fopts full_basename basename
  pure ModLocation {
    ml_hs_file = Nothing,
    ml_hi_file = full_basename OsPath.<.> hisuf,
    -- Remove the .hi-boot suffix from
    -- hi_file, if it had one.  We always
    -- want the name of the real .hi file
    -- in the ml_hi_file field.
    ml_dyn_obj_file = dyn_obj_fn,
    -- MP: TODO
    ml_dyn_hi_file = full_basename OsPath.<.> dynhisuf,
    ml_obj_file = obj_fn,
    ml_hie_file = hie_fn
  }

mk_path
  :: (FinderOpts -> Maybe FilePath)
  -> (FinderOpts -> FilePath)
  -> FinderOpts
  -> OsPath -- the filename of the source file, minus the extension
  -> OsPath -- the module name with dots replaced by slashes
  -> IO OsPath
mk_path get_suf_dir get_suf fopts basename mod_basename = do
  suf <- OsPath.encodeFS (get_suf fopts)
  suf_dir <- traverse OsPath.encodeFS (get_suf_dir fopts)
  let
    obj_basename
      | Just dir <- suf_dir
      = dir OsPath.</> mod_basename
      | otherwise
      = basename
  pure (obj_basename OsPath.<.> suf)

-- | Constructs the filename of a .o file for a given source file.
-- Does /not/ check whether the .o file exists
mkObjPath :: FinderOpts -> OsPath -> OsPath -> IO OsPath
mkObjPath = mk_path finder_objectDir finder_objectSuf

-- | Constructs the filename of a .dyn_o file for a given source file.
-- Does /not/ check whether the .dyn_o file exists
mkDynObjPath :: FinderOpts -> OsPath -> OsPath -> IO OsPath
mkDynObjPath = mk_path finder_objectDir finder_dynObjectSuf


-- | Constructs the filename of a .hi file for a given source file.
-- Does /not/ check whether the .hi file exists
mkHiPath :: FinderOpts -> OsPath -> OsPath -> IO OsPath
mkHiPath = mk_path finder_hiDir finder_hiSuf

-- | Constructs the filename of a .dyn_hi file for a given source file.
-- Does /not/ check whether the .dyn_hi file exists
mkDynHiPath :: FinderOpts -> OsPath -> OsPath -> IO OsPath
mkDynHiPath = mk_path finder_hiDir finder_dynHiSuf

-- | Constructs the filename of a .hie file for a given source file.
-- Does /not/ check whether the .hie file exists
mkHiePath :: FinderOpts -> OsPath -> OsPath -> IO OsPath
mkHiePath = mk_path finder_hieDir finder_hieSuf



-- -----------------------------------------------------------------------------
-- Filenames of the stub files

-- We don't have to store these in ModLocations, because they can be derived
-- from other available information, and they're only rarely needed.

mkStubPaths
  :: FinderOpts
  -> ModuleName
  -> ModLocation
  -> FilePath

mkStubPaths fopts mod location
  = let
        stubdir = finder_stubDir fopts

        mod_basename = moduleNameSlashes mod
        src_basename = dropExtension $ expectJust "mkStubPaths"
                                                  (ml_hs_file location)

        stub_basename0
            | Just dir <- stubdir = dir </> mod_basename
            | otherwise           = src_basename

        stub_basename = stub_basename0 ++ "_stub"
     in
        stub_basename <.> "h"

-- -----------------------------------------------------------------------------
-- findLinkable isn't related to the other stuff in here,
-- but there's no other obvious place for it

findObjectLinkableMaybe :: Module -> ModLocation -> IO (Maybe Linkable)
findObjectLinkableMaybe mod locn
   = do let obj_op = ml_obj_file locn
        maybe_obj_time <- modificationTimeIfExists obj_op
        case maybe_obj_time of
          Nothing -> return Nothing
          Just obj_time -> do
            obj_fn <- OsPath.decodeFS obj_op
            liftM Just (findObjectLinkable mod obj_fn obj_time)

-- Make an object linkable when we know the object file exists, and we know
-- its modification time.
findObjectLinkable :: Module -> FilePath -> UTCTime -> IO Linkable
findObjectLinkable mod obj_fn obj_time = return (LM obj_time mod [DotO obj_fn])
  -- We used to look for _stub.o files here, but that was a bug (#706)
  -- Now GHC merges the stub.o into the main .o (#3687)

