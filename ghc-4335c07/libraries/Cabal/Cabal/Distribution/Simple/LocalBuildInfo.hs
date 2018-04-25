{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.LocalBuildInfo
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Once a package has been configured we have resolved conditionals and
-- dependencies, configured the compiler and other needed external programs.
-- The 'LocalBuildInfo' is used to hold all this information. It holds the
-- install dirs, the compiler, the exact package dependencies, the configured
-- programs, the package database to use and a bunch of miscellaneous configure
-- flags. It gets saved and reloaded from a file (@dist\/setup-config@). It gets
-- passed in to very many subsequent build actions.

module Distribution.Simple.LocalBuildInfo (
        LocalBuildInfo(..),
        externalPackageDeps,
        localComponentId,
        localUnitId,
        localCompatPackageKey,

        -- * Buildable package components
        Component(..),
        ComponentName(..),
        defaultLibName,
        showComponentName,
        componentNameString,
        ComponentLocalBuildInfo(..),
        componentBuildDir,
        foldComponent,
        componentName,
        componentBuildInfo,
        componentBuildable,
        pkgComponents,
        pkgBuildableComponents,
        lookupComponent,
        getComponent,
        getComponentLocalBuildInfo,
        allComponentsInBuildOrder,
        componentsInBuildOrder,
        depLibraryPaths,
        allLibModules,

        withAllComponentsInBuildOrder,
        withComponentsInBuildOrder,
        withComponentsLBI,
        withLibLBI,
        withExeLBI,
        withBenchLBI,
        withTestLBI,
        enabledTestLBIs,
        enabledBenchLBIs,

        -- * Installation directories
        module Distribution.Simple.InstallDirs,
        absoluteInstallDirs, prefixRelativeInstallDirs,
        absoluteComponentInstallDirs, prefixRelativeComponentInstallDirs,
        substPathTemplate,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Component
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Types.ComponentName
import Distribution.Types.UnqualComponentName
import Distribution.Types.PackageDescription
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo

import Distribution.Simple.InstallDirs hiding (absoluteInstallDirs,
                                               prefixRelativeInstallDirs,
                                               substPathTemplate, )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.PackageDescription
import qualified Distribution.InstalledPackageInfo as Installed
import Distribution.Package
import Distribution.ModuleName
import Distribution.Simple.Compiler
import Distribution.Simple.PackageIndex
import Distribution.Simple.Utils
import Distribution.Text
import qualified Distribution.Compat.Graph as Graph

import Data.List (stripPrefix)
import System.FilePath
import qualified Data.Map as Map

import System.Directory (doesDirectoryExist, canonicalizePath)

-- -----------------------------------------------------------------------------
-- Configuration information of buildable components

componentBuildDir :: LocalBuildInfo -> ComponentLocalBuildInfo -> FilePath
-- For now, we assume that libraries/executables/test-suites/benchmarks
-- are only ever built once.  With Backpack, we need a special case for
-- libraries so that we can handle building them multiple times.
componentBuildDir lbi clbi
    = buildDir lbi </>
        case componentLocalName clbi of
            CLibName      ->
                if display (componentUnitId clbi) == display (componentComponentId clbi)
                    then ""
                    else display (componentUnitId clbi)
            CSubLibName s ->
                if display (componentUnitId clbi) == display (componentComponentId clbi)
                    then unUnqualComponentName s
                    else display (componentUnitId clbi)
            CFLibName s  -> unUnqualComponentName s
            CExeName s   -> unUnqualComponentName s
            CTestName s  -> unUnqualComponentName s
            CBenchName s -> unUnqualComponentName s

{-# DEPRECATED getComponentLocalBuildInfo "This function is not well-defined, because a 'ComponentName' does not uniquely identify a 'ComponentLocalBuildInfo'.  If you have a 'TargetInfo', you should use 'targetCLBI' to get the 'ComponentLocalBuildInfo'.  Otherwise, use 'componentNameTargets' to get all possible 'ComponentLocalBuildInfo's.  This will be removed in Cabal 2.2." #-}
getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi cname =
    case componentNameCLBIs lbi cname of
      [clbi] -> clbi
      [] ->
          error $ "internal error: there is no configuration data "
               ++ "for component " ++ show cname
      clbis ->
          error $ "internal error: the component name " ++ show cname
               ++ "is ambiguous.  Refers to: "
               ++ intercalate ", " (map (display . componentUnitId) clbis)

-- | Perform the action on each enabled 'library' in the package
-- description with the 'ComponentLocalBuildInfo'.
withLibLBI :: PackageDescription -> LocalBuildInfo
           -> (Library -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withLibLBI pkg lbi f =
    withAllTargetsInBuildOrder' pkg lbi $ \target ->
        case targetComponent target of
            CLib lib -> f lib (targetCLBI target)
            _ -> return ()

-- | Perform the action on each enabled 'Executable' in the package
-- description.  Extended version of 'withExe' that also gives corresponding
-- build info.
withExeLBI :: PackageDescription -> LocalBuildInfo
           -> (Executable -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withExeLBI pkg lbi f =
    withAllTargetsInBuildOrder' pkg lbi $ \target ->
        case targetComponent target of
            CExe exe -> f exe (targetCLBI target)
            _ -> return ()

-- | Perform the action on each enabled 'Benchmark' in the package
-- description.
withBenchLBI :: PackageDescription -> LocalBuildInfo
            -> (Benchmark -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withBenchLBI pkg lbi f =
    sequence_ [ f test clbi | (test, clbi) <- enabledBenchLBIs pkg lbi ]

withTestLBI :: PackageDescription -> LocalBuildInfo
            -> (TestSuite -> ComponentLocalBuildInfo -> IO ()) -> IO ()
withTestLBI pkg lbi f =
    sequence_ [ f test clbi | (test, clbi) <- enabledTestLBIs pkg lbi ]

enabledTestLBIs :: PackageDescription -> LocalBuildInfo
             -> [(TestSuite, ComponentLocalBuildInfo)]
enabledTestLBIs pkg lbi =
    [ (test, targetCLBI target)
    | target <- allTargetsInBuildOrder' pkg lbi
    , CTest test <- [targetComponent target] ]

enabledBenchLBIs :: PackageDescription -> LocalBuildInfo
             -> [(Benchmark, ComponentLocalBuildInfo)]
enabledBenchLBIs pkg lbi =
    [ (bench, targetCLBI target)
    | target <- allTargetsInBuildOrder' pkg lbi
    , CBench bench <- [targetComponent target] ]

{-# DEPRECATED withComponentsLBI "Use withAllComponentsInBuildOrder" #-}
withComponentsLBI :: PackageDescription -> LocalBuildInfo
                  -> (Component -> ComponentLocalBuildInfo -> IO ())
                  -> IO ()
withComponentsLBI = withAllComponentsInBuildOrder

-- | Perform the action on each buildable 'Library' or 'Executable' (Component)
-- in the PackageDescription, subject to the build order specified by the
-- 'compBuildOrder' field of the given 'LocalBuildInfo'
withAllComponentsInBuildOrder :: PackageDescription -> LocalBuildInfo
                              -> (Component -> ComponentLocalBuildInfo -> IO ())
                              -> IO ()
withAllComponentsInBuildOrder pkg lbi f =
    withAllTargetsInBuildOrder' pkg lbi $ \target ->
        f (targetComponent target) (targetCLBI target)

{-# DEPRECATED withComponentsInBuildOrder "You have got a 'TargetInfo' right? Use 'withNeededTargetsInBuildOrder' on the 'UnitId's you can 'nodeKey' out." #-}
withComponentsInBuildOrder :: PackageDescription -> LocalBuildInfo
                           -> [ComponentName]
                           -> (Component -> ComponentLocalBuildInfo -> IO ())
                           -> IO ()
withComponentsInBuildOrder pkg lbi cnames f =
    withNeededTargetsInBuildOrder' pkg lbi uids $ \target ->
        f (targetComponent target) (targetCLBI target)
  where uids = concatMap (componentNameToUnitIds lbi) cnames

allComponentsInBuildOrder :: LocalBuildInfo
                          -> [ComponentLocalBuildInfo]
allComponentsInBuildOrder lbi =
    Graph.topSort (componentGraph lbi)

-- | Private helper function for some of the deprecated implementations.
componentNameToUnitIds :: LocalBuildInfo -> ComponentName -> [UnitId]
componentNameToUnitIds lbi cname =
    case Map.lookup cname (componentNameMap lbi) of
        Just clbis -> map componentUnitId clbis
        Nothing -> error $ "componentNameToUnitIds " ++ display cname

{-# DEPRECATED componentsInBuildOrder "You've got 'TargetInfo' right? Use 'neededTargetsInBuildOrder' on the 'UnitId's you can 'nodeKey' out." #-}
componentsInBuildOrder :: LocalBuildInfo -> [ComponentName]
                       -> [ComponentLocalBuildInfo]
componentsInBuildOrder lbi cnames
    -- NB: use of localPkgDescr here is safe because we throw out the
    -- result immediately afterwards
    = map targetCLBI (neededTargetsInBuildOrder' (localPkgDescr lbi) lbi uids)
  where uids = concatMap (componentNameToUnitIds lbi) cnames

-- -----------------------------------------------------------------------------
-- A random function that has no business in this module

-- | Determine the directories containing the dynamic libraries of the
-- transitive dependencies of the component we are building.
--
-- When wanted, and possible, returns paths relative to the installDirs 'prefix'
depLibraryPaths :: Bool -- ^ Building for inplace?
                -> Bool -- ^ Generate prefix-relative library paths
                -> LocalBuildInfo
                -> ComponentLocalBuildInfo -- ^ Component that is being built
                -> NoCallStackIO [FilePath]
depLibraryPaths inplace relative lbi clbi = do
    let pkgDescr    = localPkgDescr lbi
        installDirs = absoluteComponentInstallDirs pkgDescr lbi (componentUnitId clbi) NoCopyDest
        executable  = case clbi of
                        ExeComponentLocalBuildInfo {} -> True
                        _                             -> False
        relDir | executable = bindir installDirs
               | otherwise  = libdir installDirs

    let -- TODO: this is kind of inefficient
        internalDeps = [ uid
                       | (uid, _) <- componentPackageDeps clbi
                       -- Test that it's internal
                       , sub_target <- allTargetsInBuildOrder' pkgDescr lbi
                       , componentUnitId (targetCLBI (sub_target)) == uid ]
        internalLibs = [ getLibDir (targetCLBI sub_target)
                       | sub_target <- neededTargetsInBuildOrder'
                                        pkgDescr lbi internalDeps ]
    {-
    -- This is better, but it doesn't work, because we may be passed a
    -- CLBI which doesn't actually exist, and was faked up when we
    -- were building a test suite/benchmark.  See #3599 for proposal
    -- to fix this.
    let internalCLBIs = filter ((/= componentUnitId clbi) . componentUnitId)
                      . map targetCLBI
                      $ neededTargetsInBuildOrder lbi [componentUnitId clbi]
        internalLibs = map getLibDir internalCLBIs
    -}
        getLibDir sub_clbi
          | inplace    = componentBuildDir lbi sub_clbi
          | otherwise  = dynlibdir (absoluteComponentInstallDirs pkgDescr lbi (componentUnitId sub_clbi) NoCopyDest)

    -- Why do we go through all the trouble of a hand-crafting
    -- internalLibs, when 'installedPkgs' actually contains the
    -- internal libraries?  The trouble is that 'installedPkgs'
    -- may contain *inplace* entries, which we must NOT use for
    -- not inplace 'depLibraryPaths' (e.g., for RPATH calculation).
    -- See #4025 for more details. This is all horrible but it
    -- is a moot point if you are using a per-component build,
    -- because you never have any internal libraries in this case;
    -- they're all external.
    let external_ipkgs = filter is_external (allPackages (installedPkgs lbi))
        is_external ipkg = not (installedUnitId ipkg `elem` internalDeps)
        -- First look for dynamic libraries in `dynamic-library-dirs`, and use
        -- `library-dirs` as a fall back.
        getDynDir pkg  = case Installed.libraryDynDirs pkg of
                           [] -> Installed.libraryDirs pkg
                           d  -> d
        allDepLibDirs  = concatMap getDynDir external_ipkgs

        allDepLibDirs' = internalLibs ++ allDepLibDirs
    allDepLibDirsC <- traverse canonicalizePathNoFail allDepLibDirs'

    let p                = prefix installDirs
        prefixRelative l = isJust (stripPrefix p l)
        libPaths
          | relative &&
            prefixRelative relDir = map (\l ->
                                          if prefixRelative l
                                             then shortRelativePath relDir l
                                             else l
                                        ) allDepLibDirsC
          | otherwise             = allDepLibDirsC

    return libPaths
  where
    -- 'canonicalizePath' fails on UNIX when the directory does not exists.
    -- So just don't canonicalize when it doesn't exist.
    canonicalizePathNoFail p = do
      exists <- doesDirectoryExist p
      if exists
         then canonicalizePath p
         else return p

-- | Get all module names that needed to be built by GHC; i.e., all
-- of these 'ModuleName's have interface files associated with them
-- that need to be installed.
allLibModules :: Library -> ComponentLocalBuildInfo -> [ModuleName]
allLibModules lib clbi =
    ordNub $
    explicitLibModules lib ++
    case clbi of
        LibComponentLocalBuildInfo { componentInstantiatedWith = insts } -> map fst insts
        _ -> []

-- -----------------------------------------------------------------------------
-- Wrappers for a couple functions from InstallDirs

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'absoluteComponentInstallDirs' instead.
absoluteInstallDirs :: PackageDescription -> LocalBuildInfo
                    -> CopyDest
                    -> InstallDirs FilePath
absoluteInstallDirs pkg lbi copydest =
    absoluteComponentInstallDirs pkg lbi (localUnitId lbi) copydest

-- | See 'InstallDirs.absoluteInstallDirs'.
absoluteComponentInstallDirs :: PackageDescription -> LocalBuildInfo
                             -> UnitId
                             -> CopyDest
                             -> InstallDirs FilePath
absoluteComponentInstallDirs pkg lbi uid copydest =
  InstallDirs.absoluteInstallDirs
    (packageId pkg)
    uid
    (compilerInfo (compiler lbi))
    copydest
    (hostPlatform lbi)
    (installDirTemplates lbi)

-- | Backwards compatibility function which computes the InstallDirs
-- assuming that @$libname@ points to the public library (or some fake
-- package identifier if there is no public library.)  IF AT ALL
-- POSSIBLE, please use 'prefixRelativeComponentInstallDirs' instead.
prefixRelativeInstallDirs :: PackageId -> LocalBuildInfo
                          -> InstallDirs (Maybe FilePath)
prefixRelativeInstallDirs pkg_descr lbi =
    prefixRelativeComponentInstallDirs pkg_descr lbi (localUnitId lbi)

-- |See 'InstallDirs.prefixRelativeInstallDirs'
prefixRelativeComponentInstallDirs :: PackageId -> LocalBuildInfo
                                   -> UnitId
                                   -> InstallDirs (Maybe FilePath)
prefixRelativeComponentInstallDirs pkg_descr lbi uid =
  InstallDirs.prefixRelativeInstallDirs
    (packageId pkg_descr)
    uid
    (compilerInfo (compiler lbi))
    (hostPlatform lbi)
    (installDirTemplates lbi)

substPathTemplate :: PackageId -> LocalBuildInfo
                  -> UnitId
                  -> PathTemplate -> FilePath
substPathTemplate pkgid lbi uid = fromPathTemplate
                                    . ( InstallDirs.substPathTemplate env )
    where env = initialPathTemplateEnv
                   pkgid
                   uid
                   (compilerInfo (compiler lbi))
                   (hostPlatform lbi)

