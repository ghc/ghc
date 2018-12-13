{-
(c) The University of Glasgow, 2000-2006

\section[Finder]{Module Finder}
-}

{-# LANGUAGE CPP #-}

module Finder (
    flushFinderCaches,
    FindResult(..),
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

    cannotFindModule,
    cannotFindInterface,

  ) where

#include "HsVersions.h"

import GhcPrelude

import Module
import HscTypes
import Packages
import FastString
import Util
import PrelNames        ( gHC_PRIM )
import DynFlags
import Outputable
import Maybes           ( expectJust )

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
-- Packages.lookupModuleInAllPackages for that.

-- -----------------------------------------------------------------------------
-- The finder's cache

-- remove all the home modules from the cache; package modules are
-- assumed to not move around during a session.
flushFinderCaches :: HscEnv -> IO ()
flushFinderCaches hsc_env =
  atomicModifyIORef' fc_ref $ \fm -> (filterInstalledModuleEnv is_ext fm, ())
 where
        this_pkg = thisPackage (hsc_dflags hsc_env)
        fc_ref = hsc_FC hsc_env
        is_ext mod _ | not (installedModuleUnitId mod `installedUnitIdEq` this_pkg) = True
                     | otherwise = False

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
    let dflags = hsc_dflags hsc_env
    in if installedModuleUnitId mod `installedUnitIdEq` thisPackage dflags
       then findInstalledHomeModule hsc_env (installedModuleName mod)
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
  let mod = mkHomeInstalledModule (hsc_dflags hsc_env) mod_name
  modLocationCache hsc_env mod do_this

findExposedPackageModule :: HscEnv -> ModuleName -> Maybe FastString
                         -> IO FindResult
findExposedPackageModule hsc_env mod_name mb_pkg
  = findLookupResult hsc_env
  $ lookupModuleWithSuggestions
        (hsc_dflags hsc_env) mod_name mb_pkg

findExposedPluginPackageModule :: HscEnv -> ModuleName
                               -> IO FindResult
findExposedPluginPackageModule hsc_env mod_name
  = findLookupResult hsc_env
  $ lookupPluginModuleWithSuggestions
        (hsc_dflags hsc_env) mod_name Nothing

findLookupResult :: HscEnv -> LookupResult -> IO FindResult
findLookupResult hsc_env r = case r of
     LookupFound m pkg_conf -> do
       let im = fst (splitModuleInsts m)
       r' <- findPackageModule_ hsc_env im pkg_conf
       case r' of
        -- TODO: ghc -M is unlikely to do the right thing
        -- with just the location of the thing that was
        -- instantiated; you probably also need all of the
        -- implicit locations from the instances
        InstalledFound loc   _ -> return (Found loc m)
        InstalledNoPackage   _ -> return (NoPackage (moduleUnitId m))
        InstalledNotFound fp _ -> return (NotFound{ fr_paths = fp, fr_pkg = Just (moduleUnitId m)
                                         , fr_pkgs_hidden = []
                                         , fr_mods_hidden = []
                                         , fr_unusables = []
                                         , fr_suggestions = []})
     LookupMultiple rs ->
       return (FoundMultiple rs)
     LookupHidden pkg_hiddens mod_hiddens ->
       return (NotFound{ fr_paths = [], fr_pkg = Nothing
                       , fr_pkgs_hidden = map (moduleUnitId.fst) pkg_hiddens
                       , fr_mods_hidden = map (moduleUnitId.fst) mod_hiddens
                       , fr_unusables = []
                       , fr_suggestions = [] })
     LookupUnusable unusable ->
       let unusables' = map get_unusable unusable
           get_unusable (m, ModUnusable r) = (moduleUnitId m, r)
           get_unusable (_, r)             =
             pprPanic "findLookupResult: unexpected origin" (ppr r)
       in return (NotFound{ fr_paths = [], fr_pkg = Nothing
                          , fr_pkgs_hidden = []
                          , fr_mods_hidden = []
                          , fr_unusables = unusables'
                          , fr_suggestions = [] })
     LookupNotFound suggest ->
       return (NotFound{ fr_paths = [], fr_pkg = Nothing
                       , fr_pkgs_hidden = []
                       , fr_mods_hidden = []
                       , fr_unusables = []
                       , fr_suggestions = suggest })

modLocationCache :: HscEnv -> InstalledModule -> IO InstalledFindResult -> IO InstalledFindResult
modLocationCache hsc_env mod do_this = do
  m <- lookupFinderCache (hsc_FC hsc_env) mod
  case m of
    Just result -> return result
    Nothing     -> do
        result <- do_this
        addToFinderCache (hsc_FC hsc_env) mod result
        return result

mkHomeInstalledModule :: DynFlags -> ModuleName -> InstalledModule
mkHomeInstalledModule dflags mod_name =
  let iuid = fst (splitUnitIdInsts (thisPackage dflags))
  in InstalledModule iuid mod_name

-- This returns a module because it's more convenient for users
addHomeModuleToFinder :: HscEnv -> ModuleName -> ModLocation -> IO Module
addHomeModuleToFinder hsc_env mod_name loc = do
  let mod = mkHomeInstalledModule (hsc_dflags hsc_env) mod_name
  addToFinderCache (hsc_FC hsc_env) mod (InstalledFound loc mod)
  return (mkModule (thisPackage (hsc_dflags hsc_env)) mod_name)

uncacheModule :: HscEnv -> ModuleName -> IO ()
uncacheModule hsc_env mod_name = do
  let mod = mkHomeInstalledModule (hsc_dflags hsc_env) mod_name
  removeFromFinderCache (hsc_FC hsc_env) mod

-- -----------------------------------------------------------------------------
--      The internal workers

findHomeModule :: HscEnv -> ModuleName -> IO FindResult
findHomeModule hsc_env mod_name = do
  r <- findInstalledHomeModule hsc_env mod_name
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
 where
  dflags = hsc_dflags hsc_env
  uid = thisPackage dflags

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
     home_path = importPaths dflags
     hisuf = hiSuf dflags
     mod = mkHomeInstalledModule dflags mod_name

     source_exts =
      [ ("hs",   mkHomeModLocationSearched dflags mod_name "hs")
      , ("lhs",  mkHomeModLocationSearched dflags mod_name "lhs")
      , ("hsig",  mkHomeModLocationSearched dflags mod_name "hsig")
      , ("lhsig",  mkHomeModLocationSearched dflags mod_name "lhsig")
      ]

     hi_exts = [ (hisuf,                mkHiOnlyModLocation dflags hisuf)
               , (addBootSuffix hisuf,  mkHiOnlyModLocation dflags hisuf)
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
  let
        dflags = hsc_dflags hsc_env
        pkg_id = installedModuleUnitId mod
  --
  case lookupInstalledPackage dflags pkg_id of
     Nothing -> return (InstalledNoPackage pkg_id)
     Just pkg_conf -> findPackageModule_ hsc_env mod pkg_conf

-- | Look up the interface file associated with module @mod@.  This function
-- requires a few invariants to be upheld: (1) the 'Module' in question must
-- be the module identifier of the *original* implementation of a module,
-- not a reexport (this invariant is upheld by @Packages.hs@) and (2)
-- the 'PackageConfig' must be consistent with the unit id in the 'Module'.
-- The redundancy is to avoid an extra lookup in the package state
-- for the appropriate config.
findPackageModule_ :: HscEnv -> InstalledModule -> PackageConfig -> IO InstalledFindResult
findPackageModule_ hsc_env mod pkg_conf =
  ASSERT2( installedModuleUnitId mod == installedPackageConfigId pkg_conf, ppr (installedModuleUnitId mod) <+> ppr (installedPackageConfigId pkg_conf) )
  modLocationCache hsc_env mod $

  -- special case for GHC.Prim; we won't find it in the filesystem.
  if mod `installedModuleEq` gHC_PRIM
        then return (InstalledFound (error "GHC.Prim ModLocation") mod)
        else

  let
     dflags = hsc_dflags hsc_env
     tag = buildTag dflags

           -- hi-suffix for packages depends on the build tag.
     package_hisuf | null tag  = "hi"
                   | otherwise = tag ++ "_hi"

     mk_hi_loc = mkHiOnlyModLocation dflags package_hisuf

     import_dirs = importDirs pkg_conf
      -- we never look for a .hi-boot file in an external package;
      -- .hi-boot files only make sense for the home package.
  in
  case import_dirs of
    [one] | MkDepend <- ghcMode dflags -> do
          -- there's only one place that this .hi file can be, so
          -- don't bother looking for it.
          let basename = moduleNameSlashes (installedModuleName mod)
          loc <- mk_hi_loc one basename
          return (InstalledFound loc mod)
    _otherwise ->
          searchPathExts import_dirs mod [(package_hisuf, mk_hi_loc)]

-- -----------------------------------------------------------------------------
-- General path searching

searchPathExts
  :: [FilePath]         -- paths to search
  -> InstalledModule             -- module name
  -> [ (
        FileExt,                                -- suffix
        FilePath -> BaseName -> IO ModLocation  -- action
       )
     ]
  -> IO InstalledFindResult

searchPathExts paths mod exts
   = do result <- search to_search
{-
        hPutStrLn stderr (showSDoc $
                vcat [text "Search" <+> ppr mod <+> sep (map (text. fst) exts)
                    , nest 2 (vcat (map text paths))
                    , case result of
                        Succeeded (loc, p) -> text "Found" <+> ppr loc
                        Failed fs          -> text "not found"])
-}
        return result

  where
    basename = moduleNameSlashes (installedModuleName mod)

    to_search :: [(FilePath, IO ModLocation)]
    to_search = [ (file, fn path basename)
                | path <- paths,
                  (ext,fn) <- exts,
                  let base | path == "." = basename
                           | otherwise   = path </> basename
                      file = base <.> ext
                ]

    search [] = return (InstalledNotFound (map fst to_search) (Just (installedModuleUnitId mod)))

    search ((file, mk_result) : rest) = do
      b <- doesFileExist file
      if b
        then do { loc <- mk_result; return (InstalledFound loc mod) }
        else search rest

mkHomeModLocationSearched :: DynFlags -> ModuleName -> FileExt
                          -> FilePath -> BaseName -> IO ModLocation
mkHomeModLocationSearched dflags mod suff path basename = do
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

-- -----------------------------------------------------------------------------
-- Error messages

cannotFindModule :: DynFlags -> ModuleName -> FindResult -> SDoc
cannotFindModule flags mod res =
  cantFindErr (sLit cannotFindMsg)
              (sLit "Ambiguous module name")
              flags mod res
  where
    cannotFindMsg =
      case res of
        NotFound { fr_mods_hidden = hidden_mods
                 , fr_pkgs_hidden = hidden_pkgs
                 , fr_unusables = unusables }
          | not (null hidden_mods && null hidden_pkgs && null unusables)
          -> "Could not load module"
        _ -> "Could not find module"

cannotFindInterface  :: DynFlags -> ModuleName -> InstalledFindResult -> SDoc
cannotFindInterface = cantFindInstalledErr (sLit "Failed to load interface for")
                                           (sLit "Ambiguous interface for")

cantFindErr :: PtrString -> PtrString -> DynFlags -> ModuleName -> FindResult
            -> SDoc
cantFindErr _ multiple_found _ mod_name (FoundMultiple mods)
  | Just pkgs <- unambiguousPackages
  = hang (ptext multiple_found <+> quotes (ppr mod_name) <> colon) 2 (
       sep [text "it was found in multiple packages:",
                hsep (map ppr pkgs) ]
    )
  | otherwise
  = hang (ptext multiple_found <+> quotes (ppr mod_name) <> colon) 2 (
       vcat (map pprMod mods)
    )
  where
    unambiguousPackages = foldl' unambiguousPackage (Just []) mods
    unambiguousPackage (Just xs) (m, ModOrigin (Just _) _ _ _)
        = Just (moduleUnitId m : xs)
    unambiguousPackage _ _ = Nothing

    pprMod (m, o) = text "it is bound as" <+> ppr m <+>
                                text "by" <+> pprOrigin m o
    pprOrigin _ ModHidden = panic "cantFindErr: bound by mod hidden"
    pprOrigin _ (ModUnusable _) = panic "cantFindErr: bound by mod unusable"
    pprOrigin m (ModOrigin e res _ f) = sep $ punctuate comma (
      if e == Just True
          then [text "package" <+> ppr (moduleUnitId m)]
          else [] ++
      map ((text "a reexport in package" <+>)
                .ppr.packageConfigId) res ++
      if f then [text "a package flag"] else []
      )

cantFindErr cannot_find _ dflags mod_name find_result
  = ptext cannot_find <+> quotes (ppr mod_name)
    $$ more_info
  where
    more_info
      = case find_result of
            NoPackage pkg
                -> text "no unit id matching" <+> quotes (ppr pkg) <+>
                   text "was found"

            NotFound { fr_paths = files, fr_pkg = mb_pkg
                     , fr_mods_hidden = mod_hiddens, fr_pkgs_hidden = pkg_hiddens
                     , fr_unusables = unusables, fr_suggestions = suggest }
                | Just pkg <- mb_pkg, pkg /= thisPackage dflags
                -> not_found_in_package pkg files

                | not (null suggest)
                -> pp_suggestions suggest $$ tried_these files dflags

                | null files && null mod_hiddens &&
                  null pkg_hiddens && null unusables
                -> text "It is not a module in the current program, or in any known package."

                | otherwise
                -> vcat (map pkg_hidden pkg_hiddens) $$
                   vcat (map mod_hidden mod_hiddens) $$
                   vcat (map unusable unusables) $$
                   tried_these files dflags

            _ -> panic "cantFindErr"

    build_tag = buildTag dflags

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         text "Perhaps you haven't installed the " <> text build <>
         text " libraries for package " <> quotes (ppr pkg) <> char '?' $$
         tried_these files dflags

       | otherwise
       = text "There are files missing in the " <> quotes (ppr pkg) <>
         text " package," $$
         text "try running 'ghc-pkg check'." $$
         tried_these files dflags

    pkg_hidden :: UnitId -> SDoc
    pkg_hidden pkgid =
        text "It is a member of the hidden package"
        <+> quotes (ppr pkgid)
        --FIXME: we don't really want to show the unit id here we should
        -- show the source package id or installed package id if it's ambiguous
        <> dot $$ pkg_hidden_hint pkgid
    pkg_hidden_hint pkgid
     | gopt Opt_BuildingCabalPackage dflags
        = let pkg = expectJust "pkg_hidden" (lookupPackage dflags pkgid)
           in text "Perhaps you need to add" <+>
              quotes (ppr (packageName pkg)) <+>
              text "to the build-depends in your .cabal file."
     | Just pkg <- lookupPackage dflags pkgid
         = text "You can run" <+>
           quotes (text ":set -package " <> ppr (packageName pkg)) <+>
           text "to expose it." $$
           text "(Note: this unloads all the modules in the current scope.)"
     | otherwise = Outputable.empty

    mod_hidden pkg =
        text "it is a hidden module in the package" <+> quotes (ppr pkg)

    unusable (pkg, reason)
      = text "It is a member of the package"
      <+> quotes (ppr pkg)
      $$ pprReason (text "which is") reason

    pp_suggestions :: [ModuleSuggestion] -> SDoc
    pp_suggestions sugs
      | null sugs = Outputable.empty
      | otherwise = hang (text "Perhaps you meant")
                       2 (vcat (map pp_sugg sugs))

    -- NB: Prefer the *original* location, and then reexports, and then
    -- package flags when making suggestions.  ToDo: if the original package
    -- also has a reexport, prefer that one
    pp_sugg (SuggestVisible m mod o) = ppr m <+> provenance o
      where provenance ModHidden = Outputable.empty
            provenance (ModUnusable _) = Outputable.empty
            provenance (ModOrigin{ fromOrigPackage = e,
                                   fromExposedReexport = res,
                                   fromPackageFlag = f })
              | Just True <- e
                 = parens (text "from" <+> ppr (moduleUnitId mod))
              | f && moduleName mod == m
                 = parens (text "from" <+> ppr (moduleUnitId mod))
              | (pkg:_) <- res
                 = parens (text "from" <+> ppr (packageConfigId pkg)
                    <> comma <+> text "reexporting" <+> ppr mod)
              | f
                 = parens (text "defined via package flags to be"
                    <+> ppr mod)
              | otherwise = Outputable.empty
    pp_sugg (SuggestHidden m mod o) = ppr m <+> provenance o
      where provenance ModHidden =  Outputable.empty
            provenance (ModUnusable _) = Outputable.empty
            provenance (ModOrigin{ fromOrigPackage = e,
                                   fromHiddenReexport = rhs })
              | Just False <- e
                 = parens (text "needs flag -package-key"
                    <+> ppr (moduleUnitId mod))
              | (pkg:_) <- rhs
                 = parens (text "needs flag -package-id"
                    <+> ppr (packageConfigId pkg))
              | otherwise = Outputable.empty

cantFindInstalledErr :: PtrString -> PtrString -> DynFlags -> ModuleName
                     -> InstalledFindResult -> SDoc
cantFindInstalledErr cannot_find _ dflags mod_name find_result
  = ptext cannot_find <+> quotes (ppr mod_name)
    $$ more_info
  where
    more_info
      = case find_result of
            InstalledNoPackage pkg
                -> text "no unit id matching" <+> quotes (ppr pkg) <+>
                   text "was found" $$ looks_like_srcpkgid pkg

            InstalledNotFound files mb_pkg
                | Just pkg <- mb_pkg, not (pkg `installedUnitIdEq` thisPackage dflags)
                -> not_found_in_package pkg files

                | null files
                -> text "It is not a module in the current program, or in any known package."

                | otherwise
                -> tried_these files dflags

            _ -> panic "cantFindInstalledErr"

    build_tag = buildTag dflags

    looks_like_srcpkgid :: InstalledUnitId -> SDoc
    looks_like_srcpkgid pk
     -- Unsafely coerce a unit id FastString into a source package ID
     -- FastString and see if it means anything.
     | (pkg:pkgs) <- searchPackageId dflags (SourcePackageId (installedUnitIdFS pk))
     = parens (text "This unit ID looks like the source package ID;" $$
       text "the real unit ID is" <+> quotes (ftext (installedUnitIdFS (unitId pkg))) $$
       (if null pkgs then Outputable.empty
        else text "and" <+> int (length pkgs) <+> text "other candidates"))
     -- Todo: also check if it looks like a package name!
     | otherwise = Outputable.empty

    not_found_in_package pkg files
       | build_tag /= ""
       = let
            build = if build_tag == "p" then "profiling"
                                        else "\"" ++ build_tag ++ "\""
         in
         text "Perhaps you haven't installed the " <> text build <>
         text " libraries for package " <> quotes (ppr pkg) <> char '?' $$
         tried_these files dflags

       | otherwise
       = text "There are files missing in the " <> quotes (ppr pkg) <>
         text " package," $$
         text "try running 'ghc-pkg check'." $$
         tried_these files dflags

tried_these :: [FilePath] -> DynFlags -> SDoc
tried_these files dflags
    | null files = Outputable.empty
    | verbosity dflags < 3 =
          text "Use -v (or `:set -v` in ghci) " <>
              text "to see a list of the files searched for."
    | otherwise =
          hang (text "Locations searched:") 2 $ vcat (map text files)
