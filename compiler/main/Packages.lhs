%
% (c) The University of Glasgow, 2006
%
\begin{code}
{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- | Package manipulation
module Packages (
        module PackageConfig,

        -- * Reading the package config, and processing cmdline args
        PackageState(preloadPackages),
        initPackages,

        -- * Querying the package config
        lookupPackage,
        resolveInstalledPackageId,
        searchPackageId,
        getPackageDetails,
        listVisibleModuleNames,
        lookupModuleInAllPackages,
        lookupModuleWithSuggestions,
        LookupResult(..),
        ModuleSuggestion(..),
        ModuleOrigin(..),

        -- * Inspecting the set of packages in scope
        getPackageIncludePath,
        getPackageLibraryPath,
        getPackageLinkOpts,
        getPackageExtraCcOpts,
        getPackageFrameworkPath,
        getPackageFrameworks,
        getPreloadPackagesAnd,

        collectIncludeDirs, collectLibraryPaths, collectLinkOpts,
        packageHsLibs,
        ModuleExport(..),

        -- * Utils
        packageKeyPackageIdString,
        pprFlag,
        pprPackages,
        pprPackagesSimple,
        pprModuleMap,
        isDllName
    )
where

#include "HsVersions.h"

import GHC.PackageDb
import PackageConfig
import DynFlags
import Config           ( cProjectVersion )
import Name             ( Name, nameModule_maybe )
import UniqFM
import Module
import Util
import Panic
import Outputable
import Maybes

import System.Environment ( getEnv )
import FastString
import ErrUtils         ( debugTraceMsg, MsgDoc )
import Exception
import Unique

import System.Directory
import System.FilePath as FilePath
import qualified System.FilePath.Posix as FilePath.Posix
import Control.Monad
import Data.List as List
import Data.Map (Map)
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid hiding ((<>))
#endif
import qualified Data.Map as Map
import qualified FiniteMap as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------------
-- The Package state

-- | Package state is all stored in 'DynFlags', including the details of
-- all packages, which packages are exposed, and which modules they
-- provide.
--
-- The package state is computed by 'initPackages', and kept in DynFlags.
-- It is influenced by various package flags:
--
--   * @-package <pkg>@ and @-package-id <pkg>@ cause @<pkg>@ to become exposed.
--     If @-hide-all-packages@ was not specified, these commands also cause
--      all other packages with the same name to become hidden.
--
--   * @-hide-package <pkg>@ causes @<pkg>@ to become hidden.
--
--   * (there are a few more flags, check below for their semantics)
--
-- The package state has the following properties.
--
--   * Let @exposedPackages@ be the set of packages thus exposed.
--     Let @depExposedPackages@ be the transitive closure from @exposedPackages@ of
--     their dependencies.
--
--   * When searching for a module from an preload import declaration,
--     only the exposed modules in @exposedPackages@ are valid.
--
--   * When searching for a module from an implicit import, all modules
--     from @depExposedPackages@ are valid.
--
--   * When linking in a compilation manager mode, we link in packages the
--     program depends on (the compiler knows this list by the
--     time it gets to the link step).  Also, we link in all packages
--     which were mentioned with preload @-package@ flags on the command-line,
--     or are a transitive dependency of same, or are \"base\"\/\"rts\".
--     The reason for this is that we might need packages which don't
--     contain any Haskell modules, and therefore won't be discovered
--     by the normal mechanism of dependency tracking.

-- Notes on DLLs
-- ~~~~~~~~~~~~~
-- When compiling module A, which imports module B, we need to
-- know whether B will be in the same DLL as A.
--      If it's in the same DLL, we refer to B_f_closure
--      If it isn't, we refer to _imp__B_f_closure
-- When compiling A, we record in B's Module value whether it's
-- in a different DLL, by setting the DLL flag.

-- | Given a module name, there may be multiple ways it came into scope,
-- possibly simultaneously.  This data type tracks all the possible ways
-- it could have come into scope.  Warning: don't use the record functions,
-- they're partial!
data ModuleOrigin =
    -- | Module is hidden, and thus never will be available for import.
    -- (But maybe the user didn't realize), so we'll still keep track
    -- of these modules.)
    ModHidden
    -- | Module is public, and could have come from some places.
  | ModOrigin {
        -- | @Just False@ means that this module is in
        -- someone's @exported-modules@ list, but that package is hidden;
        -- @Just True@ means that it is available; @Nothing@ means neither
        -- applies.
        fromOrigPackage :: Maybe Bool
        -- | Is the module available from a reexport of an exposed package?
        -- There could be multiple.
      , fromExposedReexport :: [PackageConfig]
        -- | Is the module available from a reexport of a hidden package?
      , fromHiddenReexport :: [PackageConfig]
        -- | Did the module export come from a package flag? (ToDo: track
        -- more information.
      , fromPackageFlag :: Bool
      }

instance Outputable ModuleOrigin where
    ppr ModHidden = text "hidden module"
    ppr (ModOrigin e res rhs f) = sep (punctuate comma (
        (case e of
            Nothing -> []
            Just False -> [text "hidden package"]
            Just True -> [text "exposed package"]) ++
        (if null res
            then []
            else [text "reexport by" <+>
                    sep (map (ppr . packageConfigId) res)]) ++
        (if null rhs
            then []
            else [text "hidden reexport by" <+>
                    sep (map (ppr . packageConfigId) res)]) ++
        (if f then [text "package flag"] else [])
        ))

-- | Smart constructor for a module which is in @exposed-modules@.  Takes
-- as an argument whether or not the defining package is exposed.
fromExposedModules :: Bool -> ModuleOrigin
fromExposedModules e = ModOrigin (Just e) [] [] False

-- | Smart constructor for a module which is in @reexported-modules@.  Takes
-- as an argument whether or not the reexporting package is expsed, and
-- also its 'PackageConfig'.
fromReexportedModules :: Bool -> PackageConfig -> ModuleOrigin
fromReexportedModules True pkg = ModOrigin Nothing [pkg] [] False
fromReexportedModules False pkg = ModOrigin Nothing [] [pkg] False

-- | Smart constructor for a module which was bound by a package flag.
fromFlag :: ModuleOrigin
fromFlag = ModOrigin Nothing [] [] True

instance Monoid ModuleOrigin where
    mempty = ModOrigin Nothing [] [] False
    mappend (ModOrigin e res rhs f) (ModOrigin e' res' rhs' f') =
        ModOrigin (g e e') (res ++ res') (rhs ++ rhs') (f || f')
      where g (Just b) (Just b')
                | b == b'   = Just b
                | otherwise = panic "ModOrigin: package both exposed/hidden"
            g Nothing x = x
            g x Nothing = x
    mappend _ _ = panic "ModOrigin: hidden module redefined"

-- | Is the name from the import actually visible? (i.e. does it cause
-- ambiguity, or is it only relevant when we're making suggestions?)
originVisible :: ModuleOrigin -> Bool
originVisible ModHidden = False
originVisible (ModOrigin b res _ f) = b == Just True || not (null res) || f

-- | Are there actually no providers for this module?  This will never occur
-- except when we're filtering based on package imports.
originEmpty :: ModuleOrigin -> Bool
originEmpty (ModOrigin Nothing [] [] False) = True
originEmpty _ = False

-- | When we do a plain lookup (e.g. for an import), initially, all we want
-- to know is if we can find it or not (and if we do and it's a reexport,
-- what the real name is).  If the find fails, we'll want to investigate more
-- to give a good error message.
data SimpleModuleConf =
    SModConf Module PackageConfig ModuleOrigin
  | SModConfAmbiguous

-- | 'UniqFM' map from 'ModuleName'
type ModuleNameMap = UniqFM

-- | 'UniqFM' map from 'PackageKey'
type PackageKeyMap = UniqFM

-- | 'UniqFM' map from 'PackageKey' to 'PackageConfig'
type PackageConfigMap = PackageKeyMap PackageConfig

-- | 'UniqFM' map from 'PackageKey' to (1) whether or not all modules which
-- are exposed should be dumped into scope, (2) any custom renamings that
-- should also be apply, and (3) what package name is associated with the
-- key, if it might be hidden
type VisibilityMap =
    PackageKeyMap (Bool, [(ModuleName, ModuleName)], FastString)

-- | Map from 'ModuleName' to 'Module' to all the origins of the bindings
-- in scope.  The 'PackageConf' is not cached, mostly for convenience reasons
-- (since this is the slow path, we'll just look it up again).
type ModuleToPkgConfAll =
    Map ModuleName (Map Module ModuleOrigin)

data PackageState = PackageState {
  -- | A mapping of 'PackageKey' to 'PackageConfig'.  This list is adjusted
  -- so that only valid packages are here.  Currently, we also flip the
  -- exposed/trusted bits based on package flags; however, the hope is to
  -- stop doing that.
  pkgIdMap              :: PackageConfigMap,

  -- | The packages we're going to link in eagerly.  This list
  -- should be in reverse dependency order; that is, a package
  -- is always mentioned before the packages it depends on.
  preloadPackages      :: [PackageKey],

  -- | This is a simplified map from 'ModuleName' to original 'Module' and
  -- package configuration providing it.
  moduleToPkgConf       :: ModuleNameMap SimpleModuleConf,

  -- | This is a full map from 'ModuleName' to all modules which may possibly
  -- be providing it.  These providers may be hidden (but we'll still want
  -- to report them in error messages), or it may be an ambiguous import.
  moduleToPkgConfAll    :: ModuleToPkgConfAll,

  -- | This is a map from 'InstalledPackageId' to 'PackageKey', since GHC
  -- internally deals in package keys but the database may refer to installed
  -- package IDs.
  installedPackageIdMap :: InstalledPackageIdMap
  }

type InstalledPackageIdMap = Map InstalledPackageId PackageKey
type InstalledPackageIndex = Map InstalledPackageId PackageConfig

-- | Empty package configuration map
emptyPackageConfigMap :: PackageConfigMap
emptyPackageConfigMap = emptyUFM

-- | Find the package we know about with the given key (e.g. @foo_HASH@), if any
lookupPackage :: DynFlags -> PackageKey -> Maybe PackageConfig
lookupPackage dflags = lookupPackage' (pkgIdMap (pkgState dflags))

lookupPackage' :: PackageConfigMap -> PackageKey -> Maybe PackageConfig
lookupPackage' = lookupUFM

-- | Search for packages with a given package ID (e.g. \"foo-0.1\")
searchPackageId :: DynFlags -> SourcePackageId -> [PackageConfig]
searchPackageId dflags pid = filter ((pid ==) . sourcePackageId)
                               (listPackageConfigMap dflags)

-- | Extends the package configuration map with a list of package configs.
extendPackageConfigMap
   :: PackageConfigMap -> [PackageConfig] -> PackageConfigMap
extendPackageConfigMap pkg_map new_pkgs
  = foldl add pkg_map new_pkgs
  where add pkg_map p = addToUFM pkg_map (packageConfigId p) p

-- | Looks up the package with the given id in the package state, panicing if it is
-- not found
getPackageDetails :: DynFlags -> PackageKey -> PackageConfig
getPackageDetails dflags pid =
    expectJust "getPackageDetails" (lookupPackage dflags pid)

-- | Get a list of entries from the package database.  NB: be careful with
-- this function, it may not do what you expect it to.
listPackageConfigMap :: DynFlags -> [PackageConfig]
listPackageConfigMap dflags = eltsUFM (pkgIdMap (pkgState dflags))

-- | Looks up a 'PackageKey' given an 'InstalledPackageId'
resolveInstalledPackageId :: DynFlags -> InstalledPackageId -> PackageKey
resolveInstalledPackageId dflags ipid =
    expectJust "resolveInstalledPackageId"
        (Map.lookup ipid (installedPackageIdMap (pkgState dflags)))

-- ----------------------------------------------------------------------------
-- Loading the package db files and building up the package state

-- | Call this after 'DynFlags.parseDynFlags'.  It reads the package
-- database files, and sets up various internal tables of package
-- information, according to the package-related flags on the
-- command-line (@-package@, @-hide-package@ etc.)
--
-- Returns a list of packages to link in if we're doing dynamic linking.
-- This list contains the packages that the user explicitly mentioned with
-- @-package@ flags.
--
-- 'initPackages' can be called again subsequently after updating the
-- 'packageFlags' field of the 'DynFlags', and it will update the
-- 'pkgState' in 'DynFlags' and return a list of packages to
-- link in.
initPackages :: DynFlags -> IO (DynFlags, [PackageKey])
initPackages dflags = do
  pkg_db <- case pkgDatabase dflags of
                Nothing -> readPackageConfigs dflags
                Just db -> return $ setBatchPackageFlags dflags db
  (pkg_state, preload, this_pkg)
        <- mkPackageState dflags pkg_db [] (thisPackage dflags)
  return (dflags{ pkgDatabase = Just pkg_db,
                  pkgState = pkg_state,
                  thisPackage = this_pkg },
          preload)

-- -----------------------------------------------------------------------------
-- Reading the package database(s)

readPackageConfigs :: DynFlags -> IO [PackageConfig]
readPackageConfigs dflags = do
  let system_conf_refs = [UserPkgConf, GlobalPkgConf]

  e_pkg_path <- tryIO (getEnv "GHC_PACKAGE_PATH")
  let base_conf_refs = case e_pkg_path of
        Left _ -> system_conf_refs
        Right path
         | null (last cs)
         -> map PkgConfFile (init cs) ++ system_conf_refs
         | otherwise
         -> map PkgConfFile cs
         where cs = parseSearchPath path
         -- if the path ends in a separator (eg. "/foo/bar:")
         -- then we tack on the system paths.

  let conf_refs = reverse (extraPkgConfs dflags base_conf_refs)
  -- later packages shadow earlier ones.  extraPkgConfs
  -- is in the opposite order to the flags on the
  -- command line.
  confs <- liftM catMaybes $ mapM (resolvePackageConfig dflags) conf_refs

  liftM concat $ mapM (readPackageConfig dflags) confs

resolvePackageConfig :: DynFlags -> PkgConfRef -> IO (Maybe FilePath)
resolvePackageConfig dflags GlobalPkgConf = return $ Just (systemPackageConfig dflags)
resolvePackageConfig _ UserPkgConf = handleIO (\_ -> return Nothing) $ do
  appdir <- getAppUserDataDirectory "ghc"
  let dir = appdir </> (TARGET_ARCH ++ '-':TARGET_OS ++ '-':cProjectVersion)
      pkgconf = dir </> "package.conf.d"
  exist <- doesDirectoryExist pkgconf
  return $ if exist then Just pkgconf else Nothing
resolvePackageConfig _ (PkgConfFile name) = return $ Just name

readPackageConfig :: DynFlags -> FilePath -> IO [PackageConfig]
readPackageConfig dflags conf_file = do
  isdir <- doesDirectoryExist conf_file

  proto_pkg_configs <-
    if isdir
       then do let filename = conf_file </> "package.cache"
               debugTraceMsg dflags 2 (text "Using binary package database:" <+> text filename)
               readPackageDbForGhc filename
       else do
            isfile <- doesFileExist conf_file
            if isfile
               then throwGhcExceptionIO $ InstallationError $
                      "ghc no longer supports single-file style package " ++
                      "databases (" ++ conf_file ++
                      ") use 'ghc-pkg init' to create the database with " ++
                      "the correct format."
               else throwGhcExceptionIO $ InstallationError $
                      "can't find a package database at " ++ conf_file

  let
      top_dir = topDir dflags
      pkgroot = takeDirectory conf_file
      pkg_configs1 = map (mungePackagePaths top_dir pkgroot) proto_pkg_configs
      pkg_configs2 = setBatchPackageFlags dflags pkg_configs1
  --
  return pkg_configs2

setBatchPackageFlags :: DynFlags -> [PackageConfig] -> [PackageConfig]
setBatchPackageFlags dflags pkgs = maybeDistrustAll pkgs
  where
    maybeDistrustAll pkgs'
      | gopt Opt_DistrustAllPackages dflags = map distrust pkgs'
      | otherwise                           = pkgs'

    distrust pkg = pkg{ trusted = False }

-- TODO: This code is duplicated in utils/ghc-pkg/Main.hs
mungePackagePaths :: FilePath -> FilePath -> PackageConfig -> PackageConfig
-- Perform path/URL variable substitution as per the Cabal ${pkgroot} spec
-- (http://www.haskell.org/pipermail/libraries/2009-May/011772.html)
-- Paths/URLs can be relative to ${pkgroot} or ${pkgrooturl}.
-- The "pkgroot" is the directory containing the package database.
--
-- Also perform a similar substitution for the older GHC-specific
-- "$topdir" variable. The "topdir" is the location of the ghc
-- installation (obtained from the -B option).
mungePackagePaths top_dir pkgroot pkg =
    pkg {
      importDirs  = munge_paths (importDirs pkg),
      includeDirs = munge_paths (includeDirs pkg),
      libraryDirs = munge_paths (libraryDirs pkg),
      frameworkDirs = munge_paths (frameworkDirs pkg),
      haddockInterfaces = munge_paths (haddockInterfaces pkg),
      haddockHTMLs = munge_urls (haddockHTMLs pkg)
    }
  where
    munge_paths = map munge_path
    munge_urls  = map munge_url

    munge_path p
      | Just p' <- stripVarPrefix "${pkgroot}" p = pkgroot ++ p'
      | Just p' <- stripVarPrefix "$topdir"    p = top_dir ++ p'
      | otherwise                                = p

    munge_url p
      | Just p' <- stripVarPrefix "${pkgrooturl}" p = toUrlPath pkgroot p'
      | Just p' <- stripVarPrefix "$httptopdir"   p = toUrlPath top_dir p'
      | otherwise                                   = p

    toUrlPath r p = "file:///"
                 -- URLs always use posix style '/' separators:
                 ++ FilePath.Posix.joinPath
                        (r : -- We need to drop a leading "/" or "\\"
                             -- if there is one:
                             dropWhile (all isPathSeparator)
                                       (FilePath.splitDirectories p))

    -- We could drop the separator here, and then use </> above. However,
    -- by leaving it in and using ++ we keep the same path separator
    -- rather than letting FilePath change it to use \ as the separator
    stripVarPrefix var path = case stripPrefix var path of
                              Just [] -> Just []
                              Just cs@(c : _) | isPathSeparator c -> Just cs
                              _ -> Nothing


-- -----------------------------------------------------------------------------
-- Modify our copy of the package database based on a package flag
-- (-package, -hide-package, -ignore-package).

-- | A horrible hack, the problem is the package key we'll turn
-- up here is going to get edited when we select the wired in
-- packages, so preemptively pick up the right one.  Also, this elem
-- test is slow.  The alternative is to change wired in packages first, but
-- then we are no longer able to match against package keys e.g. from when
-- a user passes in a package flag.
calcKey :: PackageConfig -> PackageKey
calcKey p | pk <- packageNameString p
          , pk `elem` wired_in_pkgids
                      = stringToPackageKey pk
          | otherwise = packageConfigId p

applyPackageFlag
   :: DynFlags
   -> UnusablePackages
   -> ([PackageConfig], VisibilityMap)           -- Initial database
   -> PackageFlag               -- flag to apply
   -> IO ([PackageConfig], VisibilityMap)        -- new database

-- ToDo: Unfortunately, we still have to plumb the package config through,
-- because Safe Haskell trust is still implemented by modifying the database.
-- Eventually, track that separately and then axe @[PackageConfig]@ from
-- this fold entirely

applyPackageFlag dflags unusable (pkgs, vm) flag =
  case flag of
    ExposePackage arg m_rns ->
       case selectPackages (matching arg) pkgs unusable of
         Left ps         -> packageFlagErr dflags flag ps
         Right (p:_,_) -> return (pkgs, vm')
          where
           n = fsPackageName p
           vm' = addToUFM_C edit vm_cleared (calcKey p)
                              (case m_rns of
                                   Nothing   -> (True, [], n)
                                   Just rns' -> (False, map convRn rns', n))
           edit (b, rns, n) (b', rns', _) = (b || b', rns ++ rns', n)
           convRn (a,b) = (mkModuleName a, mkModuleName b)
           -- ToDo: ATM, -hide-all-packages implicitly triggers change in
           -- behavior, maybe eventually make it toggleable with a separate
           -- flag
           vm_cleared | gopt Opt_HideAllPackages dflags = vm
                      -- NB: -package foo-0.1 (Foo as Foo1) does NOT hide
                      -- other versions of foo. Presence of renaming means
                      -- user probably wanted both.
                      | Just _ <- m_rns = vm
                      | otherwise = filterUFM_Directly
                            (\k (_,_,n') -> k == getUnique (calcKey p)
                                                || n /= n') vm
         _ -> panic "applyPackageFlag"

    HidePackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,_) -> return (pkgs, vm')
          where vm' = delListFromUFM vm (map calcKey ps)

    -- we trust all matching packages. Maybe should only trust first one?
    -- and leave others the same or set them untrusted
    TrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,qs) -> return (map trust ps ++ qs, vm)
          where trust p = p {trusted=True}

    DistrustPackage str ->
       case selectPackages (matchingStr str) pkgs unusable of
         Left ps       -> packageFlagErr dflags flag ps
         Right (ps,qs) -> return (map distrust ps ++ qs, vm)
          where distrust p = p {trusted=False}

    IgnorePackage _ -> panic "applyPackageFlag: IgnorePackage"

selectPackages :: (PackageConfig -> Bool) -> [PackageConfig]
               -> UnusablePackages
               -> Either [(PackageConfig, UnusablePackageReason)]
                  ([PackageConfig], [PackageConfig])
selectPackages matches pkgs unusable
  = let (ps,rest) = partition matches pkgs
    in if null ps
        then Left (filter (matches.fst) (Map.elems unusable))
        else Right (sortByVersion ps, rest)

-- A package named on the command line can either include the
-- version, or just the name if it is unambiguous.
matchingStr :: String -> PackageConfig -> Bool
matchingStr str p
        =  str == sourcePackageIdString p
        || str == packageNameString p

matchingId :: String -> PackageConfig -> Bool
matchingId str p =  str == installedPackageIdString p

matchingKey :: String -> PackageConfig -> Bool
matchingKey str p = str == packageKeyString (packageConfigId p)

matching :: PackageArg -> PackageConfig -> Bool
matching (PackageArg str) = matchingStr str
matching (PackageIdArg str) = matchingId str
matching (PackageKeyArg str) = matchingKey str

sortByVersion :: [PackageConfig] -> [PackageConfig]
sortByVersion = sortBy (flip (comparing packageVersion))

comparing :: Ord a => (t -> a) -> t -> t -> Ordering
comparing f a b = f a `compare` f b

packageFlagErr :: DynFlags
               -> PackageFlag
               -> [(PackageConfig, UnusablePackageReason)]
               -> IO a

-- for missing DPH package we emit a more helpful error message, because
-- this may be the result of using -fdph-par or -fdph-seq.
packageFlagErr dflags (ExposePackage (PackageArg pkg) _) []
  | is_dph_package pkg
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ dph_err))
  where dph_err = text "the " <> text pkg <> text " package is not installed."
                  $$ text "To install it: \"cabal install dph\"."
        is_dph_package pkg = "dph" `isPrefixOf` pkg

packageFlagErr dflags flag reasons
  = throwGhcExceptionIO (CmdLineError (showSDoc dflags $ err))
  where err = text "cannot satisfy " <> pprFlag flag <>
                (if null reasons then Outputable.empty else text ": ") $$
              nest 4 (ppr_reasons $$
                      -- ToDo: this admonition seems a bit dodgy
                      text "(use -v for more information)")
        ppr_reasons = vcat (map ppr_reason reasons)
        ppr_reason (p, reason) =
            pprReason (ppr (installedPackageId p) <+> text "is") reason

pprFlag :: PackageFlag -> SDoc
pprFlag flag = case flag of
    IgnorePackage p -> text "-ignore-package " <> text p
    HidePackage p   -> text "-hide-package " <> text p
    ExposePackage a rns -> ppr_arg a <> ppr_rns rns
    TrustPackage p    -> text "-trust " <> text p
    DistrustPackage p -> text "-distrust " <> text p
  where ppr_arg arg = case arg of
                     PackageArg    p -> text "-package " <> text p
                     PackageIdArg  p -> text "-package-id " <> text p
                     PackageKeyArg p -> text "-package-key " <> text p
        ppr_rns Nothing = Outputable.empty
        ppr_rns (Just rns) = char '(' <> hsep (punctuate comma (map ppr_rn rns))
                                      <> char ')'
        ppr_rn (orig, new) | orig == new = text orig
                           | otherwise = text orig <+> text "as" <+> text new

-- -----------------------------------------------------------------------------
-- Wired-in packages

wired_in_pkgids :: [String]
wired_in_pkgids = map packageKeyString wiredInPackageKeys

findWiredInPackages
   :: DynFlags
   -> [PackageConfig]           -- database
   -> IO [PackageConfig]

findWiredInPackages dflags pkgs = do
  --
  -- Now we must find our wired-in packages, and rename them to
  -- their canonical names (eg. base-1.0 ==> base).
  --
  let
        matches :: PackageConfig -> String -> Bool
        pc `matches` pid = packageNameString pc == pid

        -- find which package corresponds to each wired-in package
        -- delete any other packages with the same name
        -- update the package and any dependencies to point to the new
        -- one.
        --
        -- When choosing which package to map to a wired-in package
        -- name, we pick the latest version (modern Cabal makes it difficult
        -- to install multiple versions of wired-in packages, however!)
        -- To override the default choice, -ignore-package could be used to
        -- hide newer versions.
        --
        findWiredInPackage :: [PackageConfig] -> String
                           -> IO (Maybe InstalledPackageId)
        findWiredInPackage pkgs wired_pkg =
           let all_ps = [ p | p <- pkgs, p `matches` wired_pkg ] in
           case all_ps of
                []   -> notfound
                many -> pick (head (sortByVersion many))
          where
                notfound = do
                          debugTraceMsg dflags 2 $
                            ptext (sLit "wired-in package ")
                                 <> text wired_pkg
                                 <> ptext (sLit " not found.")
                          return Nothing
                pick :: PackageConfig
                     -> IO (Maybe InstalledPackageId)
                pick pkg = do
                        debugTraceMsg dflags 2 $
                            ptext (sLit "wired-in package ")
                                 <> text wired_pkg
                                 <> ptext (sLit " mapped to ")
                                 <> ppr (installedPackageId pkg)
                        return (Just (installedPackageId pkg))


  mb_wired_in_ids <- mapM (findWiredInPackage pkgs) wired_in_pkgids
  let
        wired_in_ids = catMaybes mb_wired_in_ids

        -- this is old: we used to assume that if there were
        -- multiple versions of wired-in packages installed that
        -- they were mutually exclusive.  Now we're assuming that
        -- you have one "main" version of each wired-in package
        -- (the latest version), and the others are backward-compat
        -- wrappers that depend on this one.  e.g. base-4.0 is the
        -- latest, base-3.0 is a compat wrapper depending on base-4.0.
        {-
        deleteOtherWiredInPackages pkgs = filterOut bad pkgs
          where bad p = any (p `matches`) wired_in_pkgids
                      && package p `notElem` map fst wired_in_ids
        -}

        updateWiredInDependencies pkgs = map upd_pkg pkgs
          where upd_pkg pkg
                  | installedPackageId pkg `elem` wired_in_ids
                  = pkg {
                      packageKey = stringToPackageKey (packageNameString pkg)
                    }
                  | otherwise
                  = pkg

  return $ updateWiredInDependencies pkgs

-- ----------------------------------------------------------------------------

data UnusablePackageReason
  = IgnoredWithFlag
  | MissingDependencies [InstalledPackageId]
  | ShadowedBy InstalledPackageId

type UnusablePackages = Map InstalledPackageId
                            (PackageConfig, UnusablePackageReason)

pprReason :: SDoc -> UnusablePackageReason -> SDoc
pprReason pref reason = case reason of
  IgnoredWithFlag ->
      pref <+> ptext (sLit "ignored due to an -ignore-package flag")
  MissingDependencies deps ->
      pref <+>
      ptext (sLit "unusable due to missing or recursive dependencies:") $$
        nest 2 (hsep (map ppr deps))
  ShadowedBy ipid ->
      pref <+> ptext (sLit "shadowed by package ") <> ppr ipid

reportUnusable :: DynFlags -> UnusablePackages -> IO ()
reportUnusable dflags pkgs = mapM_ report (Map.toList pkgs)
  where
    report (ipid, (_, reason)) =
       debugTraceMsg dflags 2 $
         pprReason
           (ptext (sLit "package") <+>
            ppr ipid <+> text "is") reason

-- ----------------------------------------------------------------------------
--
-- Detect any packages that have missing dependencies, and also any
-- mutually-recursive groups of packages (loops in the package graph
-- are not allowed).  We do this by taking the least fixpoint of the
-- dependency graph, repeatedly adding packages whose dependencies are
-- satisfied until no more can be added.
--
findBroken :: [PackageConfig] -> UnusablePackages
findBroken pkgs = go [] Map.empty pkgs
 where
   go avail ipids not_avail =
     case partitionWith (depsAvailable ipids) not_avail of
        ([], not_avail) ->
            Map.fromList [ (installedPackageId p, (p, MissingDependencies deps))
                         | (p,deps) <- not_avail ]
        (new_avail, not_avail) ->
            go (new_avail ++ avail) new_ipids (map fst not_avail)
            where new_ipids = Map.insertList
                                [ (installedPackageId p, p) | p <- new_avail ]
                                ipids

   depsAvailable :: InstalledPackageIndex
                 -> PackageConfig
                 -> Either PackageConfig (PackageConfig, [InstalledPackageId])
   depsAvailable ipids pkg
        | null dangling = Left pkg
        | otherwise     = Right (pkg, dangling)
        where dangling = filter (not . (`Map.member` ipids)) (depends pkg)

-- -----------------------------------------------------------------------------
-- Eliminate shadowed packages, giving the user some feedback

-- later packages in the list should shadow earlier ones with the same
-- package name/version.  Additionally, a package may be preferred if
-- it is in the transitive closure of packages selected using -package-id
-- flags.
shadowPackages :: [PackageConfig] -> [InstalledPackageId] -> UnusablePackages
shadowPackages pkgs preferred
 = let (shadowed,_) = foldl check ([],emptyUFM) pkgs
   in  Map.fromList shadowed
 where
 check (shadowed,pkgmap) pkg
      | Just oldpkg <- lookupUFM pkgmap pkgid
      , let
            ipid_new = installedPackageId pkg
            ipid_old = installedPackageId oldpkg
        --
      , ipid_old /= ipid_new
      = if ipid_old `elem` preferred
           then ((ipid_new, (pkg, ShadowedBy ipid_old)) : shadowed, pkgmap)
           else ((ipid_old, (oldpkg, ShadowedBy ipid_new)) : shadowed, pkgmap')
      | otherwise
      = (shadowed, pkgmap')
      where
        pkgid = mkFastString (sourcePackageIdString pkg)
        pkgmap' = addToUFM pkgmap pkgid pkg

-- -----------------------------------------------------------------------------

ignorePackages :: [PackageFlag] -> [PackageConfig] -> UnusablePackages
ignorePackages flags pkgs = Map.fromList (concatMap doit flags)
  where
  doit (IgnorePackage str) =
     case partition (matchingStr str) pkgs of
         (ps, _) -> [ (installedPackageId p, (p, IgnoredWithFlag))
                    | p <- ps ]
        -- missing package is not an error for -ignore-package,
        -- because a common usage is to -ignore-package P as
        -- a preventative measure just in case P exists.
  doit _ = panic "ignorePackages"

-- -----------------------------------------------------------------------------

depClosure :: InstalledPackageIndex
           -> [InstalledPackageId]
           -> [InstalledPackageId]
depClosure index ipids = closure Map.empty ipids
  where
   closure set [] = Map.keys set
   closure set (ipid : ipids)
     | ipid `Map.member` set = closure set ipids
     | Just p <- Map.lookup ipid index = closure (Map.insert ipid p set)
                                                 (depends p ++ ipids)
     | otherwise = closure set ipids

-- -----------------------------------------------------------------------------
-- When all the command-line options are in, we can process our package
-- settings and populate the package state.

mkPackageState
    :: DynFlags
    -> [PackageConfig]          -- initial database
    -> [PackageKey]              -- preloaded packages
    -> PackageKey                -- this package
    -> IO (PackageState,
           [PackageKey],         -- new packages to preload
           PackageKey) -- this package, might be modified if the current
                      -- package is a wired-in package.

mkPackageState dflags pkgs0 preload0 this_package = do

{-
   Plan.

   1. P = transitive closure of packages selected by -package-id

   2. Apply shadowing.  When there are multiple packages with the same
      packageKey,
        * if one is in P, use that one
        * otherwise, use the one highest in the package stack
      [
       rationale: we cannot use two packages with the same packageKey
       in the same program, because packageKey is the symbol prefix.
       Hence we must select a consistent set of packages to use.  We have
       a default algorithm for doing this: packages higher in the stack
       shadow those lower down.  This default algorithm can be overriden
       by giving explicit -package-id flags; then we have to take these
       preferences into account when selecting which other packages are
       made available.

       Our simple algorithm throws away some solutions: there may be other
       consistent sets that would satisfy the -package flags, but it's
       not GHC's job to be doing constraint solving.
      ]

   3. remove packages selected by -ignore-package

   4. remove any packages with missing dependencies, or mutually recursive
      dependencies.

   5. report (with -v) any packages that were removed by steps 2-4

   6. apply flags to set exposed/hidden on the resulting packages
      - if any flag refers to a package which was removed by 2-4, then
        we can give an error message explaining why

   7. hide any packages which are superseded by later exposed packages
-}

  let
      flags = reverse (packageFlags dflags)

      -- pkgs0 with duplicate packages filtered out.  This is
      -- important: it is possible for a package in the global package
      -- DB to have the same IPID as a package in the user DB, and
      -- we want the latter to take precedence.  This is not the same
      -- as shadowing (below), since in this case the two packages
      -- have the same ABI and are interchangeable.
      --
      -- #4072: note that we must retain the ordering of the list here
      -- so that shadowing behaves as expected when we apply it later.
      pkgs0_unique = snd $ foldr del (Set.empty,[]) pkgs0
          where del p (s,ps)
                  | pid `Set.member` s = (s,ps)
                  | otherwise          = (Set.insert pid s, p:ps)
                  where pid = installedPackageId p
          -- XXX this is just a variant of nub

      ipid_map = Map.fromList [ (installedPackageId p, p) | p <- pkgs0 ]

      ipid_selected = depClosure ipid_map
                                 [ InstalledPackageId (mkFastString i)
                                 | ExposePackage (PackageIdArg i) _ <- flags ]

      (ignore_flags, other_flags) = partition is_ignore flags
      is_ignore IgnorePackage{} = True
      is_ignore _ = False

      shadowed = shadowPackages pkgs0_unique ipid_selected
      ignored  = ignorePackages ignore_flags pkgs0_unique

      isBroken = (`Map.member` (Map.union shadowed ignored)).installedPackageId
      pkgs0' = filter (not . isBroken) pkgs0_unique

      broken   = findBroken pkgs0'

      unusable = shadowed `Map.union` ignored `Map.union` broken
      pkgs1 = filter (not . (`Map.member` unusable) . installedPackageId) pkgs0'

  reportUnusable dflags unusable

  --
  -- Calculate the initial set of packages, prior to any package flags.
  -- This set contains the latest version of all valid (not unusable) packages,
  -- or is empty if we have -hide-all-packages
  --
  let preferLater pkg pkg' =
        case comparing packageVersion pkg pkg' of
            GT -> pkg
            _  -> pkg'
      calcInitial m pkg = addToUFM_C preferLater m (fsPackageName pkg) pkg
      initial = if gopt Opt_HideAllPackages dflags
                    then emptyUFM
                    else foldl' calcInitial emptyUFM pkgs1
      vis_map0 = foldUFM (\p vm ->
                            if exposed p
                               then addToUFM vm (calcKey p)
                                             (True, [], fsPackageName p)
                               else vm)
                         emptyUFM initial

  --
  -- Modify the package database according to the command-line flags
  -- (-package, -hide-package, -ignore-package, -hide-all-packages).
  -- This needs to know about the unusable packages, since if a user tries
  -- to enable an unusable package, we should let them know.
  --
  (pkgs2, vis_map) <- foldM (applyPackageFlag dflags unusable)
                            (pkgs1, vis_map0) other_flags

  --
  -- Sort out which packages are wired in. This has to be done last, since
  -- it modifies the package keys of wired in packages, but when we process
  -- package arguments we need to key against the old versions.
  --
  pkgs3 <- findWiredInPackages dflags pkgs2

  --
  -- Here we build up a set of the packages mentioned in -package
  -- flags on the command line; these are called the "preload"
  -- packages.  we link these packages in eagerly.  The preload set
  -- should contain at least rts & base, which is why we pretend that
  -- the command line contains -package rts & -package base.
  --
  let preload1 = [ installedPackageId p | f <- flags, p <- get_exposed f ]

      get_exposed (ExposePackage a _) = take 1 . sortByVersion
                                      . filter (matching a)
                                      $ pkgs2
      get_exposed _                 = []

  let pkg_db = extendPackageConfigMap emptyPackageConfigMap pkgs3

      ipid_map = Map.fromList [ (installedPackageId p, packageConfigId p)
                              | p <- pkgs3 ]

      lookupIPID ipid
         | Just pid <- Map.lookup ipid ipid_map = return pid
         | otherwise                            = missingPackageErr dflags ipid

  preload2 <- mapM lookupIPID preload1

  let
      -- add base & rts to the preload packages
      basicLinkedPackages
       | gopt Opt_AutoLinkPackages dflags
          = filter (flip elemUFM pkg_db)
                [basePackageKey, rtsPackageKey]
       | otherwise = []
      -- but in any case remove the current package from the set of
      -- preloaded packages so that base/rts does not end up in the
      -- set up preloaded package when we are just building it
      preload3 = nub $ filter (/= this_package)
                     $ (basicLinkedPackages ++ preload2)

  -- Close the preload packages with their dependencies
  dep_preload <- closeDeps dflags pkg_db ipid_map (zip preload3 (repeat Nothing))
  let new_dep_preload = filter (`notElem` preload0) dep_preload

  let pstate = PackageState{
    preloadPackages     = dep_preload,
    pkgIdMap            = pkg_db,
    moduleToPkgConf     = mkModuleToPkgConf dflags pkg_db ipid_map vis_map,
    moduleToPkgConfAll  = mkModuleToPkgConfAll dflags pkg_db ipid_map vis_map,
    installedPackageIdMap = ipid_map
    }
  return (pstate, new_dep_preload, this_package)


-- -----------------------------------------------------------------------------
-- | Makes the mapping from module to package info

-- | This function is generic; we instantiate it
mkModuleToPkgConfGeneric
  :: forall m e.
     -- Empty map, e.g. the initial state of the output
     m e
     -- How to create an entry in the map based on the calculated information
  -> (PackageKey -> ModuleName -> PackageConfig -> ModuleOrigin -> e)
     -- How to override the origin of an entry (used for renaming)
  -> (e -> ModuleOrigin -> e)
     -- How to incorporate a list of entries into the map
  -> (m e -> [(ModuleName, e)] -> m e)
  -- The proper arguments
  -> DynFlags
  -> PackageConfigMap
  -> InstalledPackageIdMap
  -> VisibilityMap
  -> m e
mkModuleToPkgConfGeneric emptyMap sing setOrigins addListTo
                         dflags pkg_db ipid_map vis_map =
    foldl' extend_modmap emptyMap (eltsUFM pkg_db)
 where
  extend_modmap modmap pkg = addListTo modmap theBindings
   where
    theBindings :: [(ModuleName, e)]
    theBindings | Just (b,rns,_) <- lookupUFM vis_map (packageConfigId pkg)
                              = newBindings b rns
                | otherwise   = newBindings False []

    newBindings :: Bool -> [(ModuleName, ModuleName)] -> [(ModuleName, e)]
    newBindings e rns  = es e ++ hiddens ++ map rnBinding rns

    rnBinding :: (ModuleName, ModuleName) -> (ModuleName, e)
    rnBinding (orig, new) = (new, setOrigins origEntry fromFlag)
     where origEntry = case lookupUFM esmap orig of
            Just r -> r
            Nothing -> throwGhcException (CmdLineError (showSDoc dflags
                        (text "package flag: could not find module name" <+>
                            ppr orig <+> text "in package" <+> ppr pk)))

    es :: Bool -> [(ModuleName, e)]
    es e =
     [(m, sing pk  m  pkg  (fromExposedModules e)) | m <- exposed_mods] ++
     [(m, sing pk' m' pkg' (fromReexportedModules e pkg))
     | ModuleExport {
         exportModuleName         = m,
         exportOriginalPackageId  = ipid',
         exportOriginalModuleName = m'
       } <- reexported_mods
     , let pk' = expectJust "mkModuleToPkgConf" (Map.lookup ipid' ipid_map)
           pkg' = pkg_lookup pk' ]

    esmap :: UniqFM e
    esmap = listToUFM (es False) -- parameter here doesn't matter, orig will
                                 -- be overwritten

    hiddens = [(m, sing pk m pkg ModHidden) | m <- hidden_mods]

    pk = packageConfigId pkg
    pkg_lookup = expectJust "mkModuleToPkgConf" . lookupPackage' pkg_db

    exposed_mods = exposedModules pkg
    reexported_mods = reexportedModules pkg
    hidden_mods = hiddenModules pkg

-- | This is a quick and efficient module map, which only contains an entry
-- if it is specified unambiguously.
mkModuleToPkgConf
  :: DynFlags
  -> PackageConfigMap
  -> InstalledPackageIdMap
  -> VisibilityMap
  -> ModuleNameMap SimpleModuleConf
mkModuleToPkgConf =
  mkModuleToPkgConfGeneric emptyMap sing setOrigins addListTo
    where emptyMap = emptyUFM
          sing pk m pkg = SModConf (mkModule pk m) pkg
          -- NB: don't put hidden entries in the map, they're not valid!
          addListTo m xs = addListToUFM_C merge m (filter isVisible xs)
          isVisible (_, SModConf _ _ o) = originVisible o
          isVisible (_, SModConfAmbiguous) = False
          merge (SModConf m pkg o) (SModConf m' _ o')
              | m == m' = SModConf m pkg (o `mappend` o')
              | otherwise = SModConfAmbiguous
          merge _ _ = SModConfAmbiguous
          setOrigins (SModConf m pkg _) os = SModConf m pkg os
          setOrigins SModConfAmbiguous _ = SModConfAmbiguous

-- | This is a slow and complete map, which includes information about
-- everything, including hidden modules
mkModuleToPkgConfAll
  :: DynFlags
  -> PackageConfigMap
  -> InstalledPackageIdMap
  -> VisibilityMap
  -> ModuleToPkgConfAll
mkModuleToPkgConfAll =
  mkModuleToPkgConfGeneric emptyMap sing setOrigins addListTo
    where emptyMap = Map.empty
          sing pk m _ = Map.singleton (mkModule pk m)
          addListTo = foldl' merge
          merge m (k, v) = Map.insertWith (Map.unionWith mappend) k v m
          setOrigins m os = fmap (const os) m

-- -----------------------------------------------------------------------------
-- Extracting information from the packages in scope

-- Many of these functions take a list of packages: in those cases,
-- the list is expected to contain the "dependent packages",
-- i.e. those packages that were found to be depended on by the
-- current module/program.  These can be auto or non-auto packages, it
-- doesn't really matter.  The list is always combined with the list
-- of preload (command-line) packages to determine which packages to
-- use.

-- | Find all the include directories in these and the preload packages
getPackageIncludePath :: DynFlags -> [PackageKey] -> IO [String]
getPackageIncludePath dflags pkgs =
  collectIncludeDirs `fmap` getPreloadPackagesAnd dflags pkgs

collectIncludeDirs :: [PackageConfig] -> [FilePath]
collectIncludeDirs ps = nub (filter notNull (concatMap includeDirs ps))

-- | Find all the library paths in these and the preload packages
getPackageLibraryPath :: DynFlags -> [PackageKey] -> IO [String]
getPackageLibraryPath dflags pkgs =
  collectLibraryPaths `fmap` getPreloadPackagesAnd dflags pkgs

collectLibraryPaths :: [PackageConfig] -> [FilePath]
collectLibraryPaths ps = nub (filter notNull (concatMap libraryDirs ps))

-- | Find all the link options in these and the preload packages,
-- returning (package hs lib options, extra library options, other flags)
getPackageLinkOpts :: DynFlags -> [PackageKey] -> IO ([String], [String], [String])
getPackageLinkOpts dflags pkgs =
  collectLinkOpts dflags `fmap` getPreloadPackagesAnd dflags pkgs

collectLinkOpts :: DynFlags -> [PackageConfig] -> ([String], [String], [String])
collectLinkOpts dflags ps =
    (
        concatMap (map ("-l" ++) . packageHsLibs dflags) ps,
        concatMap (map ("-l" ++) . extraLibraries) ps,
        concatMap ldOptions ps
    )

packageHsLibs :: DynFlags -> PackageConfig -> [String]
packageHsLibs dflags p = map (mkDynName . addSuffix) (hsLibraries p)
  where
        ways0 = ways dflags

        ways1 = filter (/= WayDyn) ways0
        -- the name of a shared library is libHSfoo-ghc<version>.so
        -- we leave out the _dyn, because it is superfluous

        -- debug RTS includes support for -eventlog
        ways2 | WayDebug `elem` ways1
              = filter (/= WayEventLog) ways1
              | otherwise
              = ways1

        tag     = mkBuildTag (filter (not . wayRTSOnly) ways2)
        rts_tag = mkBuildTag ways2

        mkDynName x
         | gopt Opt_Static dflags       = x
         | "HS" `isPrefixOf` x          = x ++ "-ghc" ++ cProjectVersion
           -- For non-Haskell libraries, we use the name "Cfoo". The .a
           -- file is libCfoo.a, and the .so is libfoo.so. That way the
           -- linker knows what we mean for the vanilla (-lCfoo) and dyn
           -- (-lfoo) ways. We therefore need to strip the 'C' off here.
         | Just x' <- stripPrefix "C" x = x'
         | otherwise
            = panic ("Don't understand library name " ++ x)

        addSuffix rts@"HSrts"    = rts       ++ (expandTag rts_tag)
        addSuffix other_lib      = other_lib ++ (expandTag tag)

        expandTag t | null t = ""
                    | otherwise = '_':t

-- | Find all the C-compiler options in these and the preload packages
getPackageExtraCcOpts :: DynFlags -> [PackageKey] -> IO [String]
getPackageExtraCcOpts dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap ccOptions ps)

-- | Find all the package framework paths in these and the preload packages
getPackageFrameworkPath  :: DynFlags -> [PackageKey] -> IO [String]
getPackageFrameworkPath dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (nub (filter notNull (concatMap frameworkDirs ps)))

-- | Find all the package frameworks in these and the preload packages
getPackageFrameworks  :: DynFlags -> [PackageKey] -> IO [String]
getPackageFrameworks dflags pkgs = do
  ps <- getPreloadPackagesAnd dflags pkgs
  return (concatMap frameworks ps)

-- -----------------------------------------------------------------------------
-- Package Utils

-- | Takes a 'ModuleName', and if the module is in any package returns
-- list of modules which take that name.
lookupModuleInAllPackages :: DynFlags
                          -> ModuleName
                          -> [(Module, PackageConfig)]
lookupModuleInAllPackages dflags m
  = case lookupModuleWithSuggestions dflags m Nothing of
      LookupFound a b -> [(a,b)]
      LookupMultiple rs -> map f rs
        where f (m,_) = (m, expectJust "lookupModule" (lookupPackage dflags
                                                         (modulePackageKey m)))
      _ -> []

-- | The result of performing a lookup
data LookupResult =
    -- | Found the module uniquely, nothing else to do
    LookupFound Module PackageConfig
    -- | Multiple modules with the same name in scope
  | LookupMultiple [(Module, ModuleOrigin)]
    -- | No modules found, but there were some hidden ones with
    -- an exact name match.  First is due to package hidden, second
    -- is due to module being hidden
  | LookupHidden [(Module, ModuleOrigin)] [(Module, ModuleOrigin)]
    -- | Nothing found, here are some suggested different names
  | LookupNotFound [ModuleSuggestion] -- suggestions

data ModuleSuggestion = SuggestVisible ModuleName Module ModuleOrigin
                      | SuggestHidden ModuleName Module ModuleOrigin

lookupModuleWithSuggestions :: DynFlags
                            -> ModuleName
                            -> Maybe FastString
                            -> LookupResult
lookupModuleWithSuggestions dflags m mb_pn
  = case lookupUFM (moduleToPkgConf pkg_state) m of
     Just (SModConf m pkg o) | matches mb_pn pkg o ->
        ASSERT( originVisible o ) LookupFound m pkg
     _ -> case Map.lookup m (moduleToPkgConfAll pkg_state) of
        Nothing -> LookupNotFound suggestions
        Just xs ->
          case foldl' classify ([],[],[]) (Map.toList xs) of
            ([], [], []) -> LookupNotFound suggestions
            -- NB: Yes, we have to check this case too, since package qualified
            -- imports could cause the main lookup to fail due to ambiguity,
            -- but the second lookup to succeed.
            (_, _, [(m, _)])             -> LookupFound m (mod_pkg m)
            (_, _, exposed@(_:_))        -> LookupMultiple exposed
            (hidden_pkg, hidden_mod, []) -> LookupHidden hidden_pkg hidden_mod
  where
    classify (hidden_pkg, hidden_mod, exposed) (m, origin0) =
      let origin = filterOrigin mb_pn (mod_pkg m) origin0
          x = (m, origin)
      in case origin of
          ModHidden                  -> (hidden_pkg,   x:hidden_mod, exposed)
          _ | originEmpty origin     -> (hidden_pkg,   hidden_mod,   exposed)
            | originVisible origin   -> (hidden_pkg,   hidden_mod,   x:exposed)
            | otherwise              -> (x:hidden_pkg, hidden_mod,   exposed)

    pkg_lookup = expectJust "lookupModuleWithSuggestions" . lookupPackage dflags
    pkg_state = pkgState dflags
    mod_pkg = pkg_lookup . modulePackageKey

    matches Nothing _ _ = True -- shortcut for efficiency
    matches mb_pn pkg o = originVisible (filterOrigin mb_pn pkg o)

    -- Filters out origins which are not associated with the given package
    -- qualifier.  No-op if there is no package qualifier.  Test if this
    -- excluded all origins with 'originEmpty'.
    filterOrigin :: Maybe FastString
                 -> PackageConfig
                 -> ModuleOrigin
                 -> ModuleOrigin
    filterOrigin Nothing _ o = o
    filterOrigin (Just pn) pkg o =
      case o of
          ModHidden -> if go pkg then ModHidden else mempty
          ModOrigin { fromOrigPackage = e, fromExposedReexport = res,
                      fromHiddenReexport = rhs }
            -> ModOrigin {
                  fromOrigPackage = if go pkg then e else Nothing
                , fromExposedReexport = filter go res
                , fromHiddenReexport = filter go rhs
                , fromPackageFlag = False -- always excluded
                }
      where go pkg = pn == fsPackageName pkg

    suggestions
      | gopt Opt_HelpfulErrors dflags =
           fuzzyLookup (moduleNameString m) all_mods
      | otherwise = []

    all_mods :: [(String, ModuleSuggestion)]     -- All modules
    all_mods = sortBy (comparing fst) $
        [ (moduleNameString m, suggestion)
        | (m, e) <- Map.toList (moduleToPkgConfAll (pkgState dflags))
        , suggestion <- map (getSuggestion m) (Map.toList e)
        ]
    getSuggestion name (mod, origin) =
        (if originVisible origin then SuggestVisible else SuggestHidden)
            name mod origin

listVisibleModuleNames :: DynFlags -> [ModuleName]
listVisibleModuleNames dflags =
    map fst (filter visible (Map.toList (moduleToPkgConfAll (pkgState dflags))))
  where visible (_, ms) = any originVisible (Map.elems ms)

-- | Find all the 'PackageConfig' in both the preload packages from 'DynFlags' and corresponding to the list of
-- 'PackageConfig's
getPreloadPackagesAnd :: DynFlags -> [PackageKey] -> IO [PackageConfig]
getPreloadPackagesAnd dflags pkgids =
  let
      state   = pkgState dflags
      pkg_map = pkgIdMap state
      ipid_map = installedPackageIdMap state
      preload = preloadPackages state
      pairs = zip pkgids (repeat Nothing)
  in do
  all_pkgs <- throwErr dflags (foldM (add_package pkg_map ipid_map) preload pairs)
  return (map (getPackageDetails dflags) all_pkgs)

-- Takes a list of packages, and returns the list with dependencies included,
-- in reverse dependency order (a package appears before those it depends on).
closeDeps :: DynFlags
          -> PackageConfigMap
          -> Map InstalledPackageId PackageKey
          -> [(PackageKey, Maybe PackageKey)]
          -> IO [PackageKey]
closeDeps dflags pkg_map ipid_map ps
    = throwErr dflags (closeDepsErr pkg_map ipid_map ps)

throwErr :: DynFlags -> MaybeErr MsgDoc a -> IO a
throwErr dflags m
              = case m of
                Failed e    -> throwGhcExceptionIO (CmdLineError (showSDoc dflags e))
                Succeeded r -> return r

closeDepsErr :: PackageConfigMap
             -> Map InstalledPackageId PackageKey
             -> [(PackageKey,Maybe PackageKey)]
             -> MaybeErr MsgDoc [PackageKey]
closeDepsErr pkg_map ipid_map ps = foldM (add_package pkg_map ipid_map) [] ps

-- internal helper
add_package :: PackageConfigMap
            -> Map InstalledPackageId PackageKey
            -> [PackageKey]
            -> (PackageKey,Maybe PackageKey)
            -> MaybeErr MsgDoc [PackageKey]
add_package pkg_db ipid_map ps (p, mb_parent)
  | p `elem` ps = return ps     -- Check if we've already added this package
  | otherwise =
      case lookupPackage' pkg_db p of
        Nothing -> Failed (missingPackageMsg p <>
                           missingDependencyMsg mb_parent)
        Just pkg -> do
           -- Add the package's dependents also
           ps' <- foldM add_package_ipid ps (depends pkg)
           return (p : ps')
          where
            add_package_ipid ps ipid
              | Just pid <- Map.lookup ipid ipid_map
              = add_package pkg_db ipid_map ps (pid, Just p)
              | otherwise
              = Failed (missingPackageMsg ipid
                          <> missingDependencyMsg mb_parent)

missingPackageErr :: Outputable pkgid => DynFlags -> pkgid -> IO a
missingPackageErr dflags p
    = throwGhcExceptionIO (CmdLineError (showSDoc dflags (missingPackageMsg p)))

missingPackageMsg :: Outputable pkgid => pkgid -> SDoc
missingPackageMsg p = ptext (sLit "unknown package:") <+> ppr p

missingDependencyMsg :: Maybe PackageKey -> SDoc
missingDependencyMsg Nothing = Outputable.empty
missingDependencyMsg (Just parent)
  = space <> parens (ptext (sLit "dependency of") <+> ftext (packageKeyFS parent))

-- -----------------------------------------------------------------------------

packageKeyPackageIdString :: DynFlags -> PackageKey -> String
packageKeyPackageIdString dflags pkg_key
    | pkg_key == mainPackageKey = "main"
    | otherwise = maybe "(unknown)"
                      sourcePackageIdString
                      (lookupPackage dflags pkg_key)

-- | Will the 'Name' come from a dynamically linked library?
isDllName :: DynFlags -> PackageKey -> Module -> Name -> Bool
-- Despite the "dll", I think this function just means that
-- the synbol comes from another dynamically-linked package,
-- and applies on all platforms, not just Windows
isDllName dflags _this_pkg this_mod name
  | gopt Opt_Static dflags = False
  | Just mod <- nameModule_maybe name
    -- Issue #8696 - when GHC is dynamically linked, it will attempt
    -- to load the dynamic dependencies of object files at compile
    -- time for things like QuasiQuotes or
    -- TemplateHaskell. Unfortunately, this interacts badly with
    -- intra-package linking, because we don't generate indirect
    -- (dynamic) symbols for intra-package calls. This means that if a
    -- module with an intra-package call is loaded without its
    -- dependencies, then GHC fails to link. This is the cause of #
    --
    -- In the mean time, always force dynamic indirections to be
    -- generated: when the module name isn't the module being
    -- compiled, references are dynamic.
    = if mod /= this_mod
      then True
      else case dllSplit dflags of
           Nothing -> False
           Just ss ->
               let findMod m = let modStr = moduleNameString (moduleName m)
                               in case find (modStr `Set.member`) ss of
                                  Just i -> i
                                  Nothing -> panic ("Can't find " ++ modStr ++ "in DLL split")
               in findMod mod /= findMod this_mod
       
  | otherwise = False  -- no, it is not even an external name

-- -----------------------------------------------------------------------------
-- Displaying packages

-- | Show (very verbose) package info
pprPackages :: DynFlags -> SDoc
pprPackages = pprPackagesWith pprPackageConfig

pprPackagesWith :: (PackageConfig -> SDoc) -> DynFlags -> SDoc
pprPackagesWith pprIPI dflags =
    vcat (intersperse (text "---") (map pprIPI (listPackageConfigMap dflags)))

-- | Show simplified package info.
--
-- The idea is to only print package id, and any information that might
-- be different from the package databases (exposure, trust)
pprPackagesSimple :: DynFlags -> SDoc
pprPackagesSimple = pprPackagesWith pprIPI
    where pprIPI ipi = let InstalledPackageId i = installedPackageId ipi
                           e = if exposed ipi then text "E" else text " "
                           t = if trusted ipi then text "T" else text " "
                       in e <> t <> text "  " <> ftext i

-- | Show the mapping of modules to where they come from.
pprModuleMap :: DynFlags -> SDoc
pprModuleMap dflags =
  vcat (map pprLine (Map.toList (moduleToPkgConfAll (pkgState dflags))))
    where
      pprLine (m,e) = ppr m $$ nest 50 (vcat (map (pprEntry m) (Map.toList e)))
      pprEntry m (m',o)
        | m == moduleName m' = ppr (modulePackageKey m') <+> parens (ppr o)
        | otherwise = ppr m' <+> parens (ppr o)

fsPackageName :: PackageConfig -> FastString
fsPackageName = mkFastString . packageNameString

\end{code}
