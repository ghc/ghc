{-
(c) The University of Glasgow, 2000-2006

-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Module finder
module GHC.Unit.Finder (
    FindResult(..),
    InstalledFindResult(..),
    FinderCache,
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
    addHomeModuleToFinder,
    uncacheModule,
    mkStubPaths,

    findObjectLinkableMaybe,
    findObjectLinkable,

  ) where

#include "HsVersions.h"

import GHC.Prelude

import GHC.Driver.Env
import GHC.Driver.Session

import GHC.Platform.Ways

import GHC.Builtin.Names ( gHC_PRIM )

import GHC.Unit.Types
import GHC.Unit.Module
import GHC.Unit.Home
import GHC.Unit.State
import GHC.Unit.Finder.Types

import GHC.Data.FastString
import GHC.Data.Maybe    ( expectJust )
import qualified GHC.Data.ShortText as ST

import GHC.Utils.Misc
import GHC.Utils.Outputable as Outputable
import GHC.Utils.Panic

import GHC.Linker.Types

import Data.IORef       ( IORef, readIORef, atomicModifyIORef' )
import System.Directory
import System.FilePath
import Control.Monad
import Data.Time


type FileExt = String   -- Filename extension
type BaseName = String  -- Basename of file

-- -----------------------------------------------------------------------------
-- The Finder

-- The Finder provides a thin filesystem abstraction to the rest of
-- the compiler.  For a given module, it can tell you where the
-- source, interface, and object files for that module live.

-- It does *not* know which particular package a module lives in.  Use
-- Packages.lookupModuleInAllUnits for that.

-- -----------------------------------------------------------------------------
-- The finder's cache

-- remove all the home modules from the cache; package modules are
-- assumed to not move around during a session.
flushFinderCaches :: HscEnv -> IO ()
flushFinderCaches hsc_env =
  atomicModifyIORef' fc_ref $ \fm -> (filterInstalledModuleEnv is_ext fm, ())
 where
        fc_ref       = hsc_FC hsc_env
        home_unit    = hsc_home_unit hsc_env
        is_ext mod _ = not (isHomeInstalledModule home_unit mod)

addToFinderCache :: IORef FinderCache -> InstalledModule -> InstalledFindResult -> IO ()
addToFinderCache ref key val =
  atomicModifyIORef' ref $ \c -> (extendInstalledModuleEnv c key val, ())

removeFromFinderCache :: IORef FinderCache -> InstalledModule -> IO ()
removeFromFinderCache ref key =
  atomicModifyIORef' ref $ \c -> (delInstalledModuleEnv c key, ())

lookupFinderCache :: IORef FinderCache -> InstalledModule -> IO (Maybe InstalledFindResult)
lookupFinderCache ref key = do
   c <- readIORef ref
   return $! lookupInstalledModuleEnv c key

-- -----------------------------------------------------------------------------
-- The three external entry points

-- | Locate a module that was imported by the user.  We have the
-- module's name, and possibly a package name.  Without a package
-- name, this function will use the search path and the known exposed
-- packages to find the module, if a package is specified then only
-- that package is searched for the module.

findImportedModule :: HscEnv -> ModuleName -> Maybe FastString -> IO FindResult
findImportedModule hsc_env mod_name mb_pkg =
  case mb_pkg of
        Nothing                        -> unqual_import
        Just pkg | pkg == fsLit "this" -> home_import -- "this" is special
                 | otherwise           -> pkg_import
  where
    home_import   = findHomeModule hsc_env mod_name

    pkg_import    = findExposedPackageModule hsc_env mod_name mb_pkg

    unqual_import = home_import
                    `orIfNotFound`
                    findExposedPackageModule hsc_env mod_name Nothing

-- | Locate a plugin module requested by the user, for a compiler
-- plugin.  This consults the same set of exposed packages as
-- 'findImportedModule', unless @-hide-all-plugin-packages@ or
-- @-plugin-package@ are specified.
findPluginModule :: HscEnv -> ModuleName -> IO FindResult
findPluginModule hsc_env mod_name =
  findHomeModule hsc_env mod_name
  `orIfNotFound`
  findExposedPluginPackageModule hsc_env mod_name

-- | Locate a specific 'Module'.  The purpose of this function is to
-- create a 'ModLocation' for a given 'Module', that is to find out
-- where the files associated with this module live.  It is used when
-- reading the interface for a module mentioned by another interface,
-- for example (a "system import").

findExactModule :: HscEnv -> InstalledModule -> IO InstalledFindResult
findExactModule hsc_env mod =
    let home_unit = hsc_home_unit hsc_env
    in if isHomeInstalledModule home_unit mod
       then findInstalledHomeModule hsc_env (moduleName mod)
       else findPackageModule hsc_env mod

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
homeSearchCache :: HscEnv -> ModuleName -> IO InstalledFindResult -> IO InstalledFindResult
homeSearchCache hsc_env mod_name do_this = do
  let home_unit = hsc_home_unit hsc_env
      mod = mkHomeInstalledModule home_unit mod_name
  modLocationCache hsc_env mod do_this

findExposedPackageModule :: HscEnv -> ModuleName -> Maybe FastString
                         -> IO FindResult
findExposedPackageModule hsc_env mod_name mb_pkg
  = findLookupResult hsc_env
  $ lookupModuleWithSuggestions
        (hsc_units hsc_env) mod_name mb_pkg

findExposedPluginPackageModule :: HscEnv -> ModuleName
                               -> IO FindResult
findExposedPluginPackageModule hsc_env mod_name
  = findLookupResult hsc_env
  $ lookupPluginModuleWithSuggestions
        (hsc_units hsc_env) mod_name Nothing

findLookupResult :: HscEnv -> LookupResult -> IO FindResult
findLookupResult hsc_env r = case r of
     LookupFound m pkg_conf -> do
       let im = fst (getModuleInstantiation m)
       r' <- findPackageModule_ hsc_env im (fst pkg_conf)
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
           get_unusable (m, ModUnusable r) = (moduleUnit m, r)
           get_unusable (_, r)             =
             pprPanic "findLookupResult: unexpected origin" (ppr r)
       in return (NotFound{ fr_paths = [], fr_pkg = Nothing
                          , fr_pkgs_hidden = []
                          , fr_mods_hidden = []
                          , fr_unusables = unusables'
                          , fr_suggestions = [] })
     LookupNotFound suggest -> do
       let suggest'
             | gopt Opt_HelpfulErrors (hsc_dflags hsc_env) = suggest
             | otherwise = []
       return (NotFound{ fr_paths = [], fr_pkg = Nothing
                       , fr_pkgs_hidden = []
                       , fr_mods_hidden = []
                       , fr_unusables = []
                       , fr_suggestions = suggest' })

modLocationCache :: HscEnv -> InstalledModule -> IO InstalledFindResult -> IO InstalledFindResult
modLocationCache hsc_env mod do_this = do
  m <- lookupFinderCache (hsc_FC hsc_env) mod
  case m of
    Just result -> return result
    Nothing     -> do
        result <- do_this
        addToFinderCache (hsc_FC hsc_env) mod result
        return result

-- This returns a module because it's more convenient for users
addHomeModuleToFinder :: HscEnv -> ModuleName -> ModLocation -> IO Module
addHomeModuleToFinder hsc_env mod_name loc = do
  let home_unit = hsc_home_unit hsc_env
      mod = mkHomeInstalledModule home_unit mod_name
  addToFinderCache (hsc_FC hsc_env) mod (InstalledFound loc mod)
  return (mkHomeModule home_unit mod_name)

uncacheModule :: HscEnv -> ModuleName -> IO ()
uncacheModule hsc_env mod_name = do
  let home_unit = hsc_home_unit hsc_env
      mod = mkHomeInstalledModule home_unit mod_name
  removeFromFinderCache (hsc_FC hsc_env) mod

-- -----------------------------------------------------------------------------
--      The internal workers

findHomeModule :: HscEnv -> ModuleName -> IO FindResult
findHomeModule hsc_env mod_name = do
  r <- findInstalledHomeModule hsc_env mod_name
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
 where
  home_unit = hsc_home_unit hsc_env
  uid       = homeUnitAsUnit home_unit

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
findInstalledHomeModule :: HscEnv -> ModuleName -> IO InstalledFindResult
findInstalledHomeModule hsc_env mod_name =
   homeSearchCache hsc_env mod_name $
   let
     dflags = hsc_dflags hsc_env
     home_unit = hsc_home_unit hsc_env
     home_path = importPaths dflags
     hisuf = hiSuf dflags
     mod = mkHomeInstalledModule home_unit mod_name

     source_exts =
      [ ("hs",   mkHomeModLocationSearched dflags mod_name "hs")
      , ("lhs",  mkHomeModLocationSearched dflags mod_name "lhs")
      , ("hsig",  mkHomeModLocationSearched dflags mod_name "hsig")
      , ("lhsig",  mkHomeModLocationSearched dflags mod_name "lhsig")
      ]

     -- we use mkHomeModHiOnlyLocation instead of mkHiOnlyModLocation so that
     -- when hiDir field is set in dflags, we know to look there (see #16500)
     hi_exts = [ (hisuf,                mkHomeModHiOnlyLocation dflags mod_name)
               , (addBootSuffix hisuf,  mkHomeModHiOnlyLocation dflags mod_name)
               ]

        -- In compilation manager modes, we look for source files in the home
        -- package because we can compile these automatically.  In one-shot
        -- compilation mode we look for .hi and .hi-boot files only.
     exts | isOneShot (ghcMode dflags) = hi_exts
          | otherwise                  = source_exts
   in

  -- special case for GHC.Prim; we won't find it in the filesystem.
  -- This is important only when compiling the base package (where GHC.Prim
  -- is a home module).
  if mod `installedModuleEq` gHC_PRIM
        then return (InstalledFound (error "GHC.Prim ModLocation") mod)
        else searchPathExts home_path mod exts


-- | Search for a module in external packages only.
findPackageModule :: HscEnv -> InstalledModule -> IO InstalledFindResult
findPackageModule hsc_env mod = do
  let pkg_id = moduleUnit mod
  case lookupUnitId (hsc_units hsc_env) pkg_id of
     Nothing -> return (InstalledNoPackage pkg_id)
     Just u  -> findPackageModule_ hsc_env mod u

-- | Look up the interface file associated with module @mod@.  This function
-- requires a few invariants to be upheld: (1) the 'Module' in question must
-- be the module identifier of the *original* implementation of a module,
-- not a reexport (this invariant is upheld by "GHC.Unit.State") and (2)
-- the 'UnitInfo' must be consistent with the unit id in the 'Module'.
-- The redundancy is to avoid an extra lookup in the package state
-- for the appropriate config.
findPackageModule_ :: HscEnv -> InstalledModule -> UnitInfo -> IO InstalledFindResult
findPackageModule_ hsc_env mod pkg_conf =
  ASSERT2( moduleUnit mod == unitId pkg_conf, ppr (moduleUnit mod) <+> ppr (unitId pkg_conf) )
  modLocationCache hsc_env mod $

  -- special case for GHC.Prim; we won't find it in the filesystem.
  if mod `installedModuleEq` gHC_PRIM
        then return (InstalledFound (error "GHC.Prim ModLocation") mod)
        else

  let
     dflags = hsc_dflags hsc_env
     tag = waysBuildTag (ways dflags)

           -- hi-suffix for packages depends on the build tag.
     package_hisuf | null tag  = "hi"
                   | otherwise = tag ++ "_hi"

     mk_hi_loc = mkHiOnlyModLocation dflags package_hisuf

     import_dirs = map ST.unpack $ unitImportDirs pkg_conf
      -- we never look for a .hi-boot file in an external package;
      -- .hi-boot files only make sense for the home package.
  in
  case import_dirs of
    [one] | MkDepend <- ghcMode dflags -> do
          -- there's only one place that this .hi file can be, so
          -- don't bother looking for it.
          let basename = moduleNameSlashes (moduleName mod)
          loc <- mk_hi_loc one basename
          return (InstalledFound loc mod)
    _otherwise ->
          searchPathExts import_dirs mod [(package_hisuf, mk_hi_loc)]

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts :: [FilePath]      -- paths to search
               -> InstalledModule -- module name
               -> [ (
                     FileExt,                                -- suffix
                     FilePath -> BaseName -> IO ModLocation  -- action
                    )
                  ]
               -> IO InstalledFindResult

searchPathExts paths mod exts = search to_search
  where
    basename = moduleNameSlashes (moduleName mod)

    to_search :: [(FilePath, IO ModLocation)]
    to_search = [ (file, fn path basename)
                | path <- paths,
                  (ext,fn) <- exts,
                  let base | path == "." = basename
                           | otherwise   = path </> basename
                      file = base <.> ext
                ]

    search [] = return (InstalledNotFound (map fst to_search) (Just (moduleUnit mod)))

    search ((file, mk_result) : rest) = do
      b <- doesFileExist file
      if b
        then do { loc <- mk_result; return (InstalledFound loc mod) }
        else search rest

mkHomeModLocationSearched :: DynFlags -> ModuleName -> FileExt
                          -> FilePath -> BaseName -> IO ModLocation
mkHomeModLocationSearched dflags mod suff path basename =
  mkHomeModLocation2 dflags mod (path </> basename) suff

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

mkHomeModLocation :: DynFlags -> ModuleName -> FilePath -> IO ModLocation
mkHomeModLocation dflags mod src_filename = do
   let (basename,extension) = splitExtension src_filename
   mkHomeModLocation2 dflags mod basename extension

mkHomeModLocation2 :: DynFlags
                   -> ModuleName
                   -> FilePath  -- Of source module, without suffix
                   -> String    -- Suffix
                   -> IO ModLocation
mkHomeModLocation2 dflags mod src_basename ext = do
   let mod_basename = moduleNameSlashes mod

       obj_fn = mkObjPath  dflags src_basename mod_basename
       hi_fn  = mkHiPath   dflags src_basename mod_basename
       hie_fn = mkHiePath  dflags src_basename mod_basename

   return (ModLocation{ ml_hs_file   = Just (src_basename <.> ext),
                        ml_hi_file   = hi_fn,
                        ml_obj_file  = obj_fn,
                        ml_hie_file  = hie_fn })

mkHomeModHiOnlyLocation :: DynFlags
                        -> ModuleName
                        -> FilePath
                        -> BaseName
                        -> IO ModLocation
mkHomeModHiOnlyLocation dflags mod path basename = do
   loc <- mkHomeModLocation2 dflags mod (path </> basename) ""
   return loc { ml_hs_file = Nothing }

mkHiOnlyModLocation :: DynFlags -> Suffix -> FilePath -> String
                    -> IO ModLocation
mkHiOnlyModLocation dflags hisuf path basename
 = do let full_basename = path </> basename
          obj_fn = mkObjPath  dflags full_basename basename
          hie_fn = mkHiePath  dflags full_basename basename
      return ModLocation{    ml_hs_file   = Nothing,
                             ml_hi_file   = full_basename <.> hisuf,
                                -- Remove the .hi-boot suffix from
                                -- hi_file, if it had one.  We always
                                -- want the name of the real .hi file
                                -- in the ml_hi_file field.
                             ml_obj_file  = obj_fn,
                             ml_hie_file  = hie_fn
                  }

-- | Constructs the filename of a .o file for a given source file.
-- Does /not/ check whether the .o file exists
mkObjPath
  :: DynFlags
  -> FilePath           -- the filename of the source file, minus the extension
  -> String             -- the module name with dots replaced by slashes
  -> FilePath
mkObjPath dflags basename mod_basename = obj_basename <.> osuf
  where
                odir = objectDir dflags
                osuf = objectSuf dflags

                obj_basename | Just dir <- odir = dir </> mod_basename
                             | otherwise        = basename


-- | Constructs the filename of a .hi file for a given source file.
-- Does /not/ check whether the .hi file exists
mkHiPath
  :: DynFlags
  -> FilePath           -- the filename of the source file, minus the extension
  -> String             -- the module name with dots replaced by slashes
  -> FilePath
mkHiPath dflags basename mod_basename = hi_basename <.> hisuf
 where
                hidir = hiDir dflags
                hisuf = hiSuf dflags

                hi_basename | Just dir <- hidir = dir </> mod_basename
                            | otherwise         = basename

-- | Constructs the filename of a .hie file for a given source file.
-- Does /not/ check whether the .hie file exists
mkHiePath
  :: DynFlags
  -> FilePath           -- the filename of the source file, minus the extension
  -> String             -- the module name with dots replaced by slashes
  -> FilePath
mkHiePath dflags basename mod_basename = hie_basename <.> hiesuf
 where
                hiedir = hieDir dflags
                hiesuf = hieSuf dflags

                hie_basename | Just dir <- hiedir = dir </> mod_basename
                             | otherwise          = basename



-- -----------------------------------------------------------------------------
-- Filenames of the stub files

-- We don't have to store these in ModLocations, because they can be derived
-- from other available information, and they're only rarely needed.

mkStubPaths
  :: DynFlags
  -> ModuleName
  -> ModLocation
  -> FilePath

mkStubPaths dflags mod location
  = let
        stubdir = stubDir dflags

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
   = do let obj_fn = ml_obj_file locn
        maybe_obj_time <- modificationTimeIfExists obj_fn
        case maybe_obj_time of
          Nothing -> return Nothing
          Just obj_time -> liftM Just (findObjectLinkable mod obj_fn obj_time)

-- Make an object linkable when we know the object file exists, and we know
-- its modification time.
findObjectLinkable :: Module -> FilePath -> UTCTime -> IO Linkable
findObjectLinkable mod obj_fn obj_time = return (LM obj_time mod [DotO obj_fn])
  -- We used to look for _stub.o files here, but that was a bug (#706)
  -- Now GHC merges the stub.o into the main .o (#3687)

