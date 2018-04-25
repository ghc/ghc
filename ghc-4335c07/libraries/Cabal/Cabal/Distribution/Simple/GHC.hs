{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.GHC
-- Copyright   :  Isaac Jones 2003-2007
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is a fairly large module. It contains most of the GHC-specific code for
-- configuring, building and installing packages. It also exports a function
-- for finding out what packages are already installed. Configuring involves
-- finding the @ghc@ and @ghc-pkg@ programs, finding what language extensions
-- this version of ghc supports and returning a 'Compiler' value.
--
-- 'getInstalledPackages' involves calling the @ghc-pkg@ program to find out
-- what packages are installed.
--
-- Building is somewhat complex as there is quite a bit of information to take
-- into account. We have to build libs and programs, possibly for profiling and
-- shared libs. We have to support building libraries that will be usable by
-- GHCi and also ghc's @-split-objs@ feature. We have to compile any C files
-- using ghc. Linking, especially for @split-objs@ is remarkably complex,
-- partly because there tend to be 1,000's of @.o@ files and this can often be
-- more than we can pass to the @ld@ or @ar@ programs in one go.
--
-- Installing for libs and exes involves finding the right files and copying
-- them to the right places. One of the more tricky things about this module is
-- remembering the layout of files in the build directory (which is not
-- explicitly documented) and thus what search dirs are used for various kinds
-- of files.

module Distribution.Simple.GHC (
        getGhcInfo,
        configure,
        getInstalledPackages,
        getInstalledPackagesMonitorFiles,
        getPackageDBContents,
        buildLib, buildFLib, buildExe,
        replLib, replFLib, replExe,
        startInterpreter,
        installLib, installFLib, installExe,
        libAbiHash,
        hcPkgInfo,
        registerPackage,
        componentGhcOptions,
        componentCcGhcOptions,
        getLibDir,
        isDynamic,
        getGlobalPackageDB,
        pkgRoot,
        -- * Constructing GHC environment files
        Internal.GhcEnvironmentFileEntry(..),
        Internal.simpleGhcEnvironmentFile,
        Internal.writeGhcEnvironmentFile,
        -- * Version-specific implementation quirks
        getImplInfo,
        GhcImplInfo(..)
 ) where

import Prelude ()
import Distribution.Compat.Prelude

import qualified Distribution.Simple.GHC.IPI642 as IPI642
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.GHC.ImplInfo
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.PackageDescription as PD
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
import Distribution.Simple.PackageIndex (InstalledPackageIndex)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentLocalBuildInfo
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Package
import qualified Distribution.ModuleName as ModuleName
import Distribution.ModuleName (ModuleName)
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin (runghcProgram)
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import qualified Distribution.Simple.Program.Ar    as Ar
import qualified Distribution.Simple.Program.Ld    as Ld
import qualified Distribution.Simple.Program.Strip as Strip
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Compiler hiding (Flag)
import Distribution.Version
import Distribution.System
import Distribution.Verbosity
import Distribution.Text
import Distribution.Types.ForeignLib
import Distribution.Types.ForeignLibType
import Distribution.Types.ForeignLibOption
import Distribution.Types.UnqualComponentName
import Distribution.Utils.NubList
import Language.Haskell.Extension

import Control.Monad (msum)
import Data.Char (isLower)
import qualified Data.Map as Map
import System.Directory
         ( doesFileExist, getAppUserDataDirectory, createDirectoryIfMissing
         , canonicalizePath, removeFile, renameFile )
import System.FilePath          ( (</>), (<.>), takeExtension
                                , takeDirectory, replaceExtension
                                ,isRelative )
import qualified System.Info
#ifndef mingw32_HOST_OS
import System.Posix (createSymbolicLink)
#endif /* mingw32_HOST_OS */

-- -----------------------------------------------------------------------------
-- Configuring

configure :: Verbosity -> Maybe FilePath -> Maybe FilePath
          -> ProgramDb
          -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity hcPath hcPkgPath conf0 = do

  (ghcProg, ghcVersion, progdb1) <-
    requireProgramVersion verbosity ghcProgram
      (orLaterVersion (mkVersion [6,11]))
      (userMaybeSpecifyPath "ghc" hcPath conf0)
  let implInfo = ghcVersionImplInfo ghcVersion

  -- This is slightly tricky, we have to configure ghc first, then we use the
  -- location of ghc to help find ghc-pkg in the case that the user did not
  -- specify the location of ghc-pkg directly:
  (ghcPkgProg, ghcPkgVersion, progdb2) <-
    requireProgramVersion verbosity ghcPkgProgram {
      programFindLocation = guessGhcPkgFromGhcPath ghcProg
    }
    anyVersion (userMaybeSpecifyPath "ghc-pkg" hcPkgPath progdb1)

  when (ghcVersion /= ghcPkgVersion) $ die' verbosity $
       "Version mismatch between ghc and ghc-pkg: "
    ++ programPath ghcProg ++ " is version " ++ display ghcVersion ++ " "
    ++ programPath ghcPkgProg ++ " is version " ++ display ghcPkgVersion

  -- Likewise we try to find the matching hsc2hs and haddock programs.
  let hsc2hsProgram' = hsc2hsProgram {
                           programFindLocation = guessHsc2hsFromGhcPath ghcProg
                       }
      haddockProgram' = haddockProgram {
                           programFindLocation = guessHaddockFromGhcPath ghcProg
                       }
      hpcProgram' = hpcProgram {
                        programFindLocation = guessHpcFromGhcPath ghcProg
                    }
      runghcProgram' = runghcProgram {
                        programFindLocation = guessRunghcFromGhcPath ghcProg
                    }
      progdb3 = addKnownProgram haddockProgram' $
              addKnownProgram hsc2hsProgram' $
              addKnownProgram hpcProgram' $
              addKnownProgram runghcProgram' progdb2

  languages  <- Internal.getLanguages verbosity implInfo ghcProg
  extensions0 <- Internal.getExtensions verbosity implInfo ghcProg

  ghcInfo <- Internal.getGhcInfo verbosity implInfo ghcProg
  let ghcInfoMap = Map.fromList ghcInfo
      extensions = -- workaround https://ghc.haskell.org/ticket/11214
                   filterExt JavaScriptFFI $
                   -- see 'filterExtTH' comment below
                   filterExtTH $ extensions0

      -- starting with GHC 8.0, `TemplateHaskell` will be omitted from
      -- `--supported-extensions` when it's not available.
      -- for older GHCs we can use the "Have interpreter" property to
      -- filter out `TemplateHaskell`
      filterExtTH | ghcVersion < mkVersion [8]
                   , Just "NO" <- Map.lookup "Have interpreter" ghcInfoMap
                   = filterExt TemplateHaskell
                  | otherwise = id

      filterExt ext = filter ((/= EnableExtension ext) . fst)

  let comp = Compiler {
        compilerId         = CompilerId GHC ghcVersion,
        compilerAbiTag     = NoAbiTag,
        compilerCompat     = [],
        compilerLanguages  = languages,
        compilerExtensions = extensions,
        compilerProperties = ghcInfoMap
      }
      compPlatform = Internal.targetPlatform ghcInfo
      -- configure gcc and ld
      progdb4 = Internal.configureToolchain implInfo ghcProg ghcInfoMap progdb3
  return (comp, compPlatform, progdb4)

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find
-- the corresponding tool; e.g. if the tool is ghc-pkg, we try looking
-- for a versioned or unversioned ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessToolFromGhcPath :: Program -> ConfiguredProgram
                     -> Verbosity -> ProgramSearchPath
                     -> IO (Maybe (FilePath, [FilePath]))
guessToolFromGhcPath tool ghcProg verbosity searchpath
  = do let toolname          = programName tool
           given_path        = programPath ghcProg
           given_dir         = takeDirectory given_path
       real_path <- canonicalizePath given_path
       let real_dir           = takeDirectory real_path
           versionSuffix path = takeVersionSuffix (dropExeExtension path)
           given_suf = versionSuffix given_path
           real_suf  = versionSuffix real_path
           guessNormal       dir = dir </> toolname <.> exeExtension
           guessGhcVersioned dir suf = dir </> (toolname ++ "-ghc" ++ suf)
                                           <.> exeExtension
           guessVersioned    dir suf = dir </> (toolname ++ suf)
                                           <.> exeExtension
           mkGuesses dir suf | null suf  = [guessNormal dir]
                             | otherwise = [guessGhcVersioned dir suf,
                                            guessVersioned dir suf,
                                            guessNormal dir]
           guesses = mkGuesses given_dir given_suf ++
                            if real_path == given_path
                                then []
                                else mkGuesses real_dir real_suf
       info verbosity $ "looking for tool " ++ toolname
         ++ " near compiler in " ++ given_dir
       debug verbosity $ "candidate locations: " ++ show guesses
       exists <- traverse doesFileExist guesses
       case [ file | (file, True) <- zip guesses exists ] of
                   -- If we can't find it near ghc, fall back to the usual
                   -- method.
         []     -> programFindLocation tool verbosity searchpath
         (fp:_) -> do info verbosity $ "found " ++ toolname ++ " in " ++ fp
                      let lookedAt = map fst
                                   . takeWhile (\(_file, exist) -> not exist)
                                   $ zip guesses exists
                      return (Just (fp, lookedAt))

  where takeVersionSuffix :: FilePath -> String
        takeVersionSuffix = takeWhileEndLE isSuffixChar

        isSuffixChar :: Char -> Bool
        isSuffixChar c = isDigit c || c == '.' || c == '-'

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding ghc-pkg, we try looking for both a versioned and unversioned
-- ghc-pkg in the same dir, that is:
--
-- > /usr/local/bin/ghc-pkg-ghc-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg-6.6.1(.exe)
-- > /usr/local/bin/ghc-pkg(.exe)
--
guessGhcPkgFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessGhcPkgFromGhcPath = guessToolFromGhcPath ghcPkgProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding hsc2hs, we try looking for both a versioned and unversioned
-- hsc2hs in the same dir, that is:
--
-- > /usr/local/bin/hsc2hs-ghc-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs-6.6.1(.exe)
-- > /usr/local/bin/hsc2hs(.exe)
--
guessHsc2hsFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessHsc2hsFromGhcPath = guessToolFromGhcPath hsc2hsProgram

-- | Given something like /usr/local/bin/ghc-6.6.1(.exe) we try and find a
-- corresponding haddock, we try looking for both a versioned and unversioned
-- haddock in the same dir, that is:
--
-- > /usr/local/bin/haddock-ghc-6.6.1(.exe)
-- > /usr/local/bin/haddock-6.6.1(.exe)
-- > /usr/local/bin/haddock(.exe)
--
guessHaddockFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessHaddockFromGhcPath = guessToolFromGhcPath haddockProgram

guessHpcFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessHpcFromGhcPath = guessToolFromGhcPath hpcProgram

guessRunghcFromGhcPath :: ConfiguredProgram
                       -> Verbosity -> ProgramSearchPath
                       -> IO (Maybe (FilePath, [FilePath]))
guessRunghcFromGhcPath = guessToolFromGhcPath runghcProgram


getGhcInfo :: Verbosity -> ConfiguredProgram -> IO [(String, String)]
getGhcInfo verbosity ghcProg = Internal.getGhcInfo verbosity implInfo ghcProg
  where
    Just version = programVersion ghcProg
    implInfo = ghcVersionImplInfo version

-- | Given a single package DB, return all installed packages.
getPackageDBContents :: Verbosity -> PackageDB -> ProgramDb
                        -> IO InstalledPackageIndex
getPackageDBContents verbosity packagedb progdb = do
  pkgss <- getInstalledPackages' verbosity [packagedb] progdb
  toPackageIndex verbosity pkgss progdb

-- | Given a package DB stack, return all installed packages.
getInstalledPackages :: Verbosity -> Compiler -> PackageDBStack
                     -> ProgramDb
                     -> IO InstalledPackageIndex
getInstalledPackages verbosity comp packagedbs progdb = do
  checkPackageDbEnvVar verbosity
  checkPackageDbStack verbosity comp packagedbs
  pkgss <- getInstalledPackages' verbosity packagedbs progdb
  index <- toPackageIndex verbosity pkgss progdb
  return $! hackRtsPackage index

  where
    hackRtsPackage index =
      case PackageIndex.lookupPackageName index (mkPackageName "rts") of
        [(_,[rts])]
           -> PackageIndex.insert (removeMingwIncludeDir rts) index
        _  -> index -- No (or multiple) ghc rts package is registered!!
                    -- Feh, whatever, the ghc test suite does some crazy stuff.

-- | Given a list of @(PackageDB, InstalledPackageInfo)@ pairs, produce a
-- @PackageIndex@. Helper function used by 'getPackageDBContents' and
-- 'getInstalledPackages'.
toPackageIndex :: Verbosity
               -> [(PackageDB, [InstalledPackageInfo])]
               -> ProgramDb
               -> IO InstalledPackageIndex
toPackageIndex verbosity pkgss progdb = do
  -- On Windows, various fields have $topdir/foo rather than full
  -- paths. We need to substitute the right value in so that when
  -- we, for example, call gcc, we have proper paths to give it.
  topDir <- getLibDir' verbosity ghcProg
  let indices = [ PackageIndex.fromList (map (Internal.substTopDir topDir) pkgs)
                | (_, pkgs) <- pkgss ]
  return $! mconcat indices

  where
    Just ghcProg = lookupProgram ghcProgram progdb

getLibDir :: Verbosity -> LocalBuildInfo -> IO FilePath
getLibDir verbosity lbi =
    dropWhileEndLE isSpace `fmap`
     getDbProgramOutput verbosity ghcProgram
     (withPrograms lbi) ["--print-libdir"]

getLibDir' :: Verbosity -> ConfiguredProgram -> IO FilePath
getLibDir' verbosity ghcProg =
    dropWhileEndLE isSpace `fmap`
     getProgramOutput verbosity ghcProg ["--print-libdir"]


-- | Return the 'FilePath' to the global GHC package database.
getGlobalPackageDB :: Verbosity -> ConfiguredProgram -> IO FilePath
getGlobalPackageDB verbosity ghcProg =
    dropWhileEndLE isSpace `fmap`
     getProgramOutput verbosity ghcProg ["--print-global-package-db"]

-- | Return the 'FilePath' to the per-user GHC package database.
getUserPackageDB :: Verbosity -> ConfiguredProgram -> Platform -> NoCallStackIO FilePath
getUserPackageDB _verbosity ghcProg platform = do
    -- It's rather annoying that we have to reconstruct this, because ghc
    -- hides this information from us otherwise. But for certain use cases
    -- like change monitoring it really can't remain hidden.
    appdir <- getAppUserDataDirectory "ghc"
    return (appdir </> platformAndVersion </> packageConfFileName)
  where
    platformAndVersion = Internal.ghcPlatformAndVersionString
                           platform ghcVersion
    packageConfFileName
      | ghcVersion >= mkVersion [6,12]   = "package.conf.d"
      | otherwise                        = "package.conf"
    Just ghcVersion = programVersion ghcProg

checkPackageDbEnvVar :: Verbosity -> IO ()
checkPackageDbEnvVar verbosity =
    Internal.checkPackageDbEnvVar verbosity "GHC" "GHC_PACKAGE_PATH"

checkPackageDbStack :: Verbosity -> Compiler -> PackageDBStack -> IO ()
checkPackageDbStack verbosity comp =
    if flagPackageConf implInfo
      then checkPackageDbStackPre76 verbosity
      else checkPackageDbStackPost76 verbosity
  where implInfo = ghcVersionImplInfo (compilerVersion comp)

checkPackageDbStackPost76 :: Verbosity -> PackageDBStack -> IO ()
checkPackageDbStackPost76 _ (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPost76 verbosity rest
  | GlobalPackageDB `elem` rest =
  die' verbosity $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"
checkPackageDbStackPost76 _ _ = return ()

checkPackageDbStackPre76 :: Verbosity -> PackageDBStack -> IO ()
checkPackageDbStackPre76 _ (GlobalPackageDB:rest)
  | GlobalPackageDB `notElem` rest = return ()
checkPackageDbStackPre76 verbosity rest
  | GlobalPackageDB `notElem` rest =
  die' verbosity $ "With current ghc versions the global package db is always used "
     ++ "and must be listed first. This ghc limitation is lifted in GHC 7.6,"
     ++ "see http://hackage.haskell.org/trac/ghc/ticket/5977"
checkPackageDbStackPre76 verbosity _ =
  die' verbosity $ "If the global package db is specified, it must be "
     ++ "specified first and cannot be specified multiple times"

-- GHC < 6.10 put "$topdir/include/mingw" in rts's installDirs. This
-- breaks when you want to use a different gcc, so we need to filter
-- it out.
removeMingwIncludeDir :: InstalledPackageInfo -> InstalledPackageInfo
removeMingwIncludeDir pkg =
    let ids = InstalledPackageInfo.includeDirs pkg
        ids' = filter (not . ("mingw" `isSuffixOf`)) ids
    in pkg { InstalledPackageInfo.includeDirs = ids' }

-- | Get the packages from specific PackageDBs, not cumulative.
--
getInstalledPackages' :: Verbosity -> [PackageDB] -> ProgramDb
                     -> IO [(PackageDB, [InstalledPackageInfo])]
getInstalledPackages' verbosity packagedbs progdb
  | ghcVersion >= mkVersion [6,9] =
  sequenceA
    [ do pkgs <- HcPkg.dump (hcPkgInfo progdb) verbosity packagedb
         return (packagedb, pkgs)
    | packagedb <- packagedbs ]

  where
    Just ghcProg    = lookupProgram ghcProgram progdb
    Just ghcVersion = programVersion ghcProg

getInstalledPackages' verbosity packagedbs progdb = do
    str <- getDbProgramOutput verbosity ghcPkgProgram progdb ["list"]
    let pkgFiles = [ init line | line <- lines str, last line == ':' ]
        dbFile packagedb = case (packagedb, pkgFiles) of
          (GlobalPackageDB, global:_)      -> return $ Just global
          (UserPackageDB,  _global:user:_) -> return $ Just user
          (UserPackageDB,  _global:_)      -> return $ Nothing
          (SpecificPackageDB specific, _)  -> return $ Just specific
          _ -> die' verbosity "cannot read ghc-pkg package listing"
    pkgFiles' <- traverse dbFile packagedbs
    sequenceA [ withFileContents file $ \content -> do
                  pkgs <- readPackages file content
                  return (db, pkgs)
              | (db , Just file) <- zip packagedbs pkgFiles' ]
  where
    -- Depending on the version of ghc we use a different type's Read
    -- instance to parse the package file and then convert.
    -- It's a bit yuck. But that's what we get for using Read/Show.
    readPackages
      | ghcVersion >= mkVersion [6,4,2]
      = \file content -> case reads content of
          [(pkgs, _)] -> return (map IPI642.toCurrent pkgs)
          _           -> failToRead file
      -- We dropped support for 6.4.2 and earlier.
      | otherwise
      = \file _ -> failToRead file
    Just ghcProg = lookupProgram ghcProgram progdb
    Just ghcVersion = programVersion ghcProg
    failToRead file = die' verbosity $ "cannot read ghc package database " ++ file

getInstalledPackagesMonitorFiles :: Verbosity -> Platform
                                 -> ProgramDb
                                 -> [PackageDB]
                                 -> IO [FilePath]
getInstalledPackagesMonitorFiles verbosity platform progdb =
    traverse getPackageDBPath
  where
    getPackageDBPath :: PackageDB -> IO FilePath
    getPackageDBPath GlobalPackageDB =
      selectMonitorFile =<< getGlobalPackageDB verbosity ghcProg

    getPackageDBPath UserPackageDB =
      selectMonitorFile =<< getUserPackageDB verbosity ghcProg platform

    getPackageDBPath (SpecificPackageDB path) = selectMonitorFile path

    -- GHC has old style file dbs, and new style directory dbs.
    -- Note that for dir style dbs, we only need to monitor the cache file, not
    -- the whole directory. The ghc program itself only reads the cache file
    -- so it's safe to only monitor this one file.
    selectMonitorFile path = do
      isFileStyle <- doesFileExist path
      if isFileStyle then return path
                     else return (path </> "package.cache")

    Just ghcProg = lookupProgram ghcProgram progdb


-- -----------------------------------------------------------------------------
-- Building a library

buildLib, replLib :: Verbosity          -> Cabal.Flag (Maybe Int)
                  -> PackageDescription -> LocalBuildInfo
                  -> Library            -> ComponentLocalBuildInfo -> IO ()
buildLib = buildOrReplLib False
replLib  = buildOrReplLib True

buildOrReplLib :: Bool -> Verbosity  -> Cabal.Flag (Maybe Int)
               -> PackageDescription -> LocalBuildInfo
               -> Library            -> ComponentLocalBuildInfo -> IO ()
buildOrReplLib forRepl verbosity numJobs pkg_descr lbi lib clbi = do
  let uid = componentUnitId clbi
      libTargetDir = componentBuildDir lbi clbi
      whenVanillaLib forceVanilla =
        when (forceVanilla || withVanillaLib lbi)
      whenProfLib = when (withProfLib lbi)
      whenSharedLib forceShared =
        when (forceShared || withSharedLib lbi)
      whenStaticLib forceStatic =
        when (forceStatic || withStaticLib lbi)
      whenGHCiLib = when (withGHCiLib lbi && withVanillaLib lbi)
      ifReplLib = when forRepl
      comp = compiler lbi
      ghcVersion = compilerVersion comp
      implInfo  = getImplInfo comp
      platform@(Platform _hostArch hostOS) = hostPlatform lbi
      has_code = not (componentIsIndefinite clbi)

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let runGhcProg = runGHC verbosity ghcProg comp platform

  libBi <- hackThreadedFlag verbosity
             comp (withProfLib lbi) (libBuildInfo lib)

  let isGhcDynamic        = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      doingTH = usesTemplateHaskellOrQQ libBi
      forceVanillaLib = doingTH && not isGhcDynamic
      forceSharedLib  = doingTH &&     isGhcDynamic
      -- TH always needs default libs, even when building for profiling

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = libCoverage lbi
      -- TODO: Historically HPC files have been put into a directory which
      -- has the package name.  I'm going to avoid changing this for
      -- now, but it would probably be better for this to be the
      -- component ID instead...
      pkg_name = display (PD.package pkg_descr)
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | forRepl = mempty  -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way pkg_name
        | otherwise = mempty

  createDirectoryIfMissingVerbose verbosity True libTargetDir
  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?
  let cLikeFiles  = fromNubListR $ toNubListR (cSources libBi) <> toNubListR (cxxSources libBi)
      cObjs       = map (`replaceExtension` objExtension) cLikeFiles
      baseOpts    = componentGhcOptions verbosity lbi libBi clbi libTargetDir
      vanillaOpts = baseOpts `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptNumJobs      = numJobs,
                      ghcOptInputModules = toNubListR $ allLibModules lib clbi,
                      ghcOptHPCDir       = hpcdir Hpc.Vanilla
                    }

      profOpts    = vanillaOpts `mappend` mempty {
                      ghcOptProfilingMode = toFlag True,
                      ghcOptProfilingAuto = Internal.profDetailLevelFlag True
                                              (withProfLibDetail lbi),
                      ghcOptHiSuffix      = toFlag "p_hi",
                      ghcOptObjSuffix     = toFlag "p_o",
                      ghcOptExtra         = toNubListR $ hcProfOptions GHC libBi,
                      ghcOptHPCDir        = hpcdir Hpc.Prof
                    }

      sharedOpts  = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                      ghcOptFPic        = toFlag True,
                      ghcOptHiSuffix    = toFlag "dyn_hi",
                      ghcOptObjSuffix   = toFlag "dyn_o",
                      ghcOptExtra       = toNubListR $ hcSharedOptions GHC libBi,
                      ghcOptHPCDir      = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions       = toNubListR $ PD.ldOptions libBi,
                      ghcOptLinkLibs          = toNubListR $ extraLibs libBi,
                      ghcOptLinkLibPath       = toNubListR $ extraLibDirs libBi,
                      ghcOptLinkFrameworks    = toNubListR $
                                                PD.frameworks libBi,
                      ghcOptLinkFrameworkDirs = toNubListR $
                                                PD.extraFrameworkDirs libBi,
                      ghcOptInputFiles     = toNubListR
                                             [libTargetDir </> x | x <- cObjs]
                   }
      replOpts    = vanillaOpts {
                      ghcOptExtra        = overNubListR
                                           Internal.filterGhciFlags $
                                           ghcOptExtra vanillaOpts,
                      ghcOptNumJobs      = mempty
                    }
                    `mappend` linkerOpts
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                    }

      vanillaSharedOpts = vanillaOpts `mappend` mempty {
                      ghcOptDynLinkMode  = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix  = toFlag "dyn_hi",
                      ghcOptDynObjSuffix = toFlag "dyn_o",
                      ghcOptHPCDir       = hpcdir Hpc.Dyn
                    }

  unless (forRepl || null (allLibModules lib clbi)) $
    do let vanilla = whenVanillaLib forceVanillaLib (runGhcProg vanillaOpts)
           shared  = whenSharedLib  forceSharedLib  (runGhcProg sharedOpts)
           useDynToo = dynamicTooSupported &&
                       (forceVanillaLib || withVanillaLib lbi) &&
                       (forceSharedLib  || withSharedLib  lbi) &&
                       null (hcSharedOptions GHC libBi)
       if not has_code
        then vanilla
        else
         if useDynToo
          then do
              runGhcProg vanillaSharedOpts
              case (hpcdir Hpc.Dyn, hpcdir Hpc.Vanilla) of
                (Cabal.Flag dynDir, Cabal.Flag vanillaDir) ->
                    -- When the vanilla and shared library builds are done
                    -- in one pass, only one set of HPC module interfaces
                    -- are generated. This set should suffice for both
                    -- static and dynamically linked executables. We copy
                    -- the modules interfaces so they are available under
                    -- both ways.
                    copyDirectoryRecursive verbosity dynDir vanillaDir
                _ -> return ()
          else if isGhcDynamic
            then do shared;  vanilla
            else do vanilla; shared
       whenProfLib (runGhcProg profOpts)

  -- build any C++ sources seperately
  unless (not has_code || null (cxxSources libBi)) $ do
    info verbosity "Building C++ Sources..."
    sequence_
      [ do let baseCxxOpts    = Internal.componentCxxGhcOptions verbosity implInfo
                               lbi libBi clbi libTargetDir filename
               vanillaCxxOpts = if isGhcDynamic
                                then baseCxxOpts { ghcOptFPic = toFlag True }
                                else baseCxxOpts
               profCxxOpts    = vanillaCxxOpts `mappend` mempty {
                                 ghcOptProfilingMode = toFlag True,
                                 ghcOptObjSuffix     = toFlag "p_o"
                               }
               sharedCxxOpts  = vanillaCxxOpts `mappend` mempty {
                                 ghcOptFPic        = toFlag True,
                                 ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                 ghcOptObjSuffix   = toFlag "dyn_o"
                               }
               odir           = fromFlag (ghcOptObjDir vanillaCxxOpts)
           createDirectoryIfMissingVerbose verbosity True odir
           let runGhcProgIfNeeded cxxOpts = do
                 needsRecomp <- checkNeedsRecompilation filename cxxOpts
                 when needsRecomp $ runGhcProg cxxOpts
           runGhcProgIfNeeded vanillaCxxOpts
           unless forRepl $
             whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCxxOpts)
           unless forRepl $ whenProfLib   (runGhcProgIfNeeded   profCxxOpts)
      | filename <- cxxSources libBi]

  when has_code . ifReplLib $ do
    when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
    ifReplLib (runGhcProg replOpts)

  -- build any C sources
  -- TODO: Add support for S and CMM files.
  unless (not has_code || null (cSources libBi)) $ do
    info verbosity "Building C Sources..."
    sequence_
      [ do let baseCcOpts    = Internal.componentCcGhcOptions verbosity implInfo
                               lbi libBi clbi libTargetDir filename
               vanillaCcOpts = if isGhcDynamic
                               -- Dynamic GHC requires C sources to be built
                               -- with -fPIC for REPL to work. See #2207.
                               then baseCcOpts { ghcOptFPic = toFlag True }
                               else baseCcOpts
               profCcOpts    = vanillaCcOpts `mappend` mempty {
                                 ghcOptProfilingMode = toFlag True,
                                 ghcOptObjSuffix     = toFlag "p_o"
                               }
               sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                 ghcOptFPic        = toFlag True,
                                 ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                                 ghcOptObjSuffix   = toFlag "dyn_o"
                               }
               odir          = fromFlag (ghcOptObjDir vanillaCcOpts)
           createDirectoryIfMissingVerbose verbosity True odir
           let runGhcProgIfNeeded ccOpts = do
                 needsRecomp <- checkNeedsRecompilation filename ccOpts
                 when needsRecomp $ runGhcProg ccOpts
           runGhcProgIfNeeded vanillaCcOpts
           unless forRepl $
             whenSharedLib forceSharedLib (runGhcProgIfNeeded sharedCcOpts)
           unless forRepl $ whenProfLib (runGhcProgIfNeeded profCcOpts)
      | filename <- cSources libBi]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.

  -- link:
  when has_code . unless forRepl $ do
    info verbosity "Linking..."
    let cProfObjs   = map (`replaceExtension` ("p_" ++ objExtension))
                      (cSources libBi ++ cxxSources libBi)
        cSharedObjs = map (`replaceExtension` ("dyn_" ++ objExtension))
                      (cSources libBi ++ cxxSources libBi)
        compiler_id = compilerId (compiler lbi)
        vanillaLibFilePath = libTargetDir </> mkLibName uid
        profileLibFilePath = libTargetDir </> mkProfLibName uid
        sharedLibFilePath  = libTargetDir </> mkSharedLibName compiler_id uid
        staticLibFilePath  = libTargetDir </> mkStaticLibName compiler_id uid
        ghciLibFilePath    = libTargetDir </> Internal.mkGHCiLibName uid
        libInstallPath = libdir $ absoluteComponentInstallDirs pkg_descr lbi uid NoCopyDest
        sharedLibInstallPath = libInstallPath </> mkSharedLibName compiler_id uid

    stubObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension [objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]
    stubProfObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension ["p_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]
    stubSharedObjs <- catMaybes <$> sequenceA
      [ findFileWithExtension ["dyn_" ++ objExtension] [libTargetDir]
          (ModuleName.toFilePath x ++"_stub")
      | ghcVersion < mkVersion [7,2] -- ghc-7.2+ does not make _stub.o files
      , x <- allLibModules lib clbi ]

    hObjs     <- Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir objExtension True
    hProfObjs <-
      if withProfLib lbi
              then Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir ("p_" ++ objExtension) True
              else return []
    hSharedObjs <-
      if withSharedLib lbi
              then Internal.getHaskellObjects implInfo lib lbi clbi
                      libTargetDir ("dyn_" ++ objExtension) False
              else return []

    unless (null hObjs && null cObjs && null stubObjs) $ do
      rpaths <- getRPaths lbi clbi

      let staticObjectFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          profObjectFiles =
                 hProfObjs
              ++ map (libTargetDir </>) cProfObjs
              ++ stubProfObjs
          ghciObjFiles =
                 hObjs
              ++ map (libTargetDir </>) cObjs
              ++ stubObjs
          dynamicObjectFiles =
                 hSharedObjs
              ++ map (libTargetDir </>) cSharedObjs
              ++ stubSharedObjs
          -- After the relocation lib is created we invoke ghc -shared
          -- with the dependencies spelled out as -package arguments
          -- and ghc invokes the linker with the proper library paths
          ghcSharedLinkArgs =
              mempty {
                ghcOptShared             = toFlag True,
                ghcOptDynLinkMode        = toFlag GhcDynamicOnly,
                ghcOptInputFiles         = toNubListR dynamicObjectFiles,
                ghcOptOutputFile         = toFlag sharedLibFilePath,
                ghcOptExtra              = toNubListR $
                                           hcSharedOptions GHC libBi,
                -- For dynamic libs, Mac OS/X needs to know the install location
                -- at build time. This only applies to GHC < 7.8 - see the
                -- discussion in #1660.
                ghcOptDylibName          = if hostOS == OSX
                                              && ghcVersion < mkVersion [7,8]
                                            then toFlag sharedLibInstallPath
                                            else mempty,
                ghcOptHideAllPackages    = toFlag True,
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptThisUnitId = case clbi of
                    LibComponentLocalBuildInfo { componentCompatPackageKey = pk }
                      -> toFlag pk
                    _ -> mempty,
                ghcOptThisComponentId = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts } ->
                        if null insts
                            then mempty
                            else toFlag (componentComponentId clbi)
                    _ -> mempty,
                ghcOptInstantiatedWith = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts }
                      -> insts
                    _ -> [],
                ghcOptPackages           = toNubListR $
                                           Internal.mkGhcOptPackages clbi ,
                ghcOptLinkLibs           = toNubListR $ extraLibs libBi,
                ghcOptLinkLibPath        = toNubListR $ extraLibDirs libBi,
                ghcOptLinkFrameworks     = toNubListR $ PD.frameworks libBi,
                ghcOptLinkFrameworkDirs  =
                  toNubListR $ PD.extraFrameworkDirs libBi,
                ghcOptRPaths             = rpaths
              }
          ghcStaticLinkArgs =
              mempty {
                ghcOptStaticLib          = toFlag True,
                ghcOptInputFiles         = toNubListR staticObjectFiles,
                ghcOptOutputFile         = toFlag staticLibFilePath,
                ghcOptExtra              = toNubListR $
                                           hcStaticOptions GHC libBi,
                ghcOptHideAllPackages    = toFlag True,
                ghcOptNoAutoLinkPackages = toFlag True,
                ghcOptPackageDBs         = withPackageDB lbi,
                ghcOptThisUnitId = case clbi of
                    LibComponentLocalBuildInfo { componentCompatPackageKey = pk }
                      -> toFlag pk
                    _ -> mempty,
                ghcOptThisComponentId = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts } ->
                        if null insts
                            then mempty
                            else toFlag (componentComponentId clbi)
                    _ -> mempty,
                ghcOptInstantiatedWith = case clbi of
                    LibComponentLocalBuildInfo { componentInstantiatedWith = insts }
                      -> insts
                    _ -> [],
                ghcOptPackages           = toNubListR $
                                           Internal.mkGhcOptPackages clbi ,
                ghcOptLinkLibs           = toNubListR $ extraLibs libBi,
                ghcOptLinkLibPath        = toNubListR $ extraLibDirs libBi
              }

      info verbosity (show (ghcOptPackages ghcSharedLinkArgs))

      whenVanillaLib False $
        Ar.createArLibArchive verbosity lbi vanillaLibFilePath staticObjectFiles

      whenProfLib $
        Ar.createArLibArchive verbosity lbi profileLibFilePath profObjectFiles

      whenGHCiLib $ do
        (ldProg, _) <- requireProgram verbosity ldProgram (withPrograms lbi)
        Ld.combineObjectFiles verbosity lbi ldProg
          ghciLibFilePath ghciObjFiles

      whenSharedLib False $
        runGhcProg ghcSharedLinkArgs

      whenStaticLib False $
        runGhcProg ghcStaticLinkArgs

-- | Start a REPL without loading any source files.
startInterpreter :: Verbosity -> ProgramDb -> Compiler -> Platform
                 -> PackageDBStack -> IO ()
startInterpreter verbosity progdb comp platform packageDBs = do
  let replOpts = mempty {
        ghcOptMode       = toFlag GhcModeInteractive,
        ghcOptPackageDBs = packageDBs
        }
  checkPackageDbStack verbosity comp packageDBs
  (ghcProg, _) <- requireProgram verbosity ghcProgram progdb
  runGHC verbosity ghcProg comp platform replOpts

-- -----------------------------------------------------------------------------
-- Building an executable or foreign library

-- | Build a foreign library
buildFLib, replFLib
  :: Verbosity          -> Cabal.Flag (Maybe Int)
  -> PackageDescription -> LocalBuildInfo
  -> ForeignLib         -> ComponentLocalBuildInfo -> IO ()
buildFLib v njobs pkg lbi = gbuild v njobs pkg lbi . GBuildFLib
replFLib  v njobs pkg lbi = gbuild v njobs pkg lbi . GReplFLib

-- | Build an executable with GHC.
--
buildExe, replExe
  :: Verbosity          -> Cabal.Flag (Maybe Int)
  -> PackageDescription -> LocalBuildInfo
  -> Executable         -> ComponentLocalBuildInfo -> IO ()
buildExe v njobs pkg lbi = gbuild v njobs pkg lbi . GBuildExe
replExe  v njobs pkg lbi = gbuild v njobs pkg lbi . GReplExe

-- | Building an executable, starting the REPL, and building foreign
-- libraries are all very similar and implemented in 'gbuild'. The
-- 'GBuildMode' distinguishes between the various kinds of operation.
data GBuildMode =
    GBuildExe  Executable
  | GReplExe   Executable
  | GBuildFLib ForeignLib
  | GReplFLib  ForeignLib

gbuildInfo :: GBuildMode -> BuildInfo
gbuildInfo (GBuildExe  exe)  = buildInfo exe
gbuildInfo (GReplExe   exe)  = buildInfo exe
gbuildInfo (GBuildFLib flib) = foreignLibBuildInfo flib
gbuildInfo (GReplFLib  flib) = foreignLibBuildInfo flib

gbuildName :: GBuildMode -> String
gbuildName (GBuildExe  exe)  = unUnqualComponentName $ exeName exe
gbuildName (GReplExe   exe)  = unUnqualComponentName $ exeName exe
gbuildName (GBuildFLib flib) = unUnqualComponentName $ foreignLibName flib
gbuildName (GReplFLib  flib) = unUnqualComponentName $ foreignLibName flib

gbuildTargetName :: LocalBuildInfo -> GBuildMode -> String
gbuildTargetName _lbi (GBuildExe  exe)  = exeTargetName exe
gbuildTargetName _lbi (GReplExe   exe)  = exeTargetName exe
gbuildTargetName  lbi (GBuildFLib flib) = flibTargetName lbi flib
gbuildTargetName  lbi (GReplFLib  flib) = flibTargetName lbi flib

exeTargetName :: Executable -> String
exeTargetName exe = unUnqualComponentName (exeName exe) `withExt` exeExtension

-- | Target name for a foreign library (the actual file name)
--
-- We do not use mkLibName and co here because the naming for foreign libraries
-- is slightly different (we don't use "_p" or compiler version suffices, and we
-- don't want the "lib" prefix on Windows).
--
-- TODO: We do use `dllExtension` and co here, but really that's wrong: they
-- use the OS used to build cabal to determine which extension to use, rather
-- than the target OS (but this is wrong elsewhere in Cabal as well).
flibTargetName :: LocalBuildInfo -> ForeignLib -> String
flibTargetName lbi flib =
    case (os, foreignLibType flib) of
      (Windows, ForeignLibNativeShared) -> nm <.> "dll"
      (Windows, ForeignLibNativeStatic) -> nm <.> "lib"
      (Linux,   ForeignLibNativeShared) -> "lib" ++ nm <.> versionedExt
      (_other,  ForeignLibNativeShared) -> "lib" ++ nm <.> dllExtension
      (_other,  ForeignLibNativeStatic) -> "lib" ++ nm <.> staticLibExtension
      (_any,    ForeignLibTypeUnknown)  -> cabalBug "unknown foreign lib type"
  where
    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

    os :: OS
    os = let (Platform _ os') = hostPlatform lbi
         in os'

    -- If a foreign lib foo has lib-version-info 5:1:2 or
    -- lib-version-linux 3.2.1, it should be built as libfoo.so.3.2.1
    -- Libtool's version-info data is translated into library versions in a
    -- nontrivial way: so refer to libtool documentation.
    versionedExt :: String
    versionedExt =
      let nums = foreignLibVersion flib os
      in foldl (<.>) "so" (map show nums)

-- | Name for the library when building.
--
-- If the `lib-version-info` field or the `lib-version-linux` field of
-- a foreign library target is set, we need to incorporate that
-- version into the SONAME field.
--
-- If a foreign library foo has lib-version-info 5:1:2, it should be
-- built as libfoo.so.3.2.1.  We want it to get soname libfoo.so.3.
-- However, GHC does not allow overriding soname by setting linker
-- options, as it sets a soname of its own (namely the output
-- filename), after the user-supplied linker options.  Hence, we have
-- to compile the library with the soname as its filename.  We rename
-- the compiled binary afterwards.
--
-- This method allows to adjust the name of the library at build time
-- such that the correct soname can be set.
flibBuildName :: LocalBuildInfo -> ForeignLib -> String
flibBuildName lbi flib
  -- On linux, if a foreign-library has version data, the first digit is used
  -- to produce the SONAME.
  | (os, foreignLibType flib) ==
    (Linux, ForeignLibNativeShared)
  = let nums = foreignLibVersion flib os
    in "lib" ++ nm <.> foldl (<.>) "so" (map show (take 1 nums))
  | otherwise = flibTargetName lbi flib
  where
    os :: OS
    os = let (Platform _ os') = hostPlatform lbi
         in os'

    nm :: String
    nm = unUnqualComponentName $ foreignLibName flib

gbuildIsRepl :: GBuildMode -> Bool
gbuildIsRepl (GBuildExe  _) = False
gbuildIsRepl (GReplExe   _) = True
gbuildIsRepl (GBuildFLib _) = False
gbuildIsRepl (GReplFLib  _) = True

gbuildNeedDynamic :: LocalBuildInfo -> GBuildMode -> Bool
gbuildNeedDynamic lbi bm =
    case bm of
      GBuildExe  _    -> withDynExe lbi
      GReplExe   _    -> withDynExe lbi
      GBuildFLib flib -> withDynFLib flib
      GReplFLib  flib -> withDynFLib flib
  where
    withDynFLib flib =
      case foreignLibType flib of
        ForeignLibNativeShared ->
          ForeignLibStandalone `notElem` foreignLibOptions flib
        ForeignLibNativeStatic ->
          False
        ForeignLibTypeUnknown  ->
          cabalBug "unknown foreign lib type"

gbuildModDefFiles :: GBuildMode -> [FilePath]
gbuildModDefFiles (GBuildExe _)     = []
gbuildModDefFiles (GReplExe  _)     = []
gbuildModDefFiles (GBuildFLib flib) = foreignLibModDefFile flib
gbuildModDefFiles (GReplFLib  flib) = foreignLibModDefFile flib

-- | "Main" module name when overridden by @ghc-options: -main-is ...@
-- or 'Nothing' if no @-main-is@ flag could be found.
--
-- In case of 'Nothing', 'Distribution.ModuleName.main' can be assumed.
exeMainModuleName :: Executable -> Maybe ModuleName
exeMainModuleName Executable{buildInfo = bnfo} =
    -- GHC honors the last occurence of a module name updated via -main-is
    --
    -- Moreover, -main-is when parsed left-to-right can update either
    -- the "Main" module name, or the "main" function name, or both,
    -- see also 'decodeMainIsArg'.
    msum $ reverse $ map decodeMainIsArg $ findIsMainArgs ghcopts
  where
    ghcopts = hcOptions GHC bnfo

    findIsMainArgs [] = []
    findIsMainArgs ("-main-is":arg:rest) = arg : findIsMainArgs rest
    findIsMainArgs (_:rest) = findIsMainArgs rest

-- | Decode argument to '-main-is'
--
-- Returns 'Nothing' if argument set only the function name.
--
-- This code has been stolen/refactored from GHC's DynFlags.setMainIs
-- function. The logic here is deliberately imperfect as it is
-- intended to be bug-compatible with GHC's parser. See discussion in
-- https://github.com/haskell/cabal/pull/4539#discussion_r118981753.
decodeMainIsArg :: String -> Maybe ModuleName
decodeMainIsArg arg
  | not (null main_fn) && isLower (head main_fn)
                        -- The arg looked like "Foo.Bar.baz"
  = Just (ModuleName.fromString main_mod)
  | isUpper (head arg)  -- The arg looked like "Foo" or "Foo.Bar"
  = Just (ModuleName.fromString arg)
  | otherwise           -- The arg looked like "baz"
  = Nothing
  where
    (main_mod, main_fn) = splitLongestPrefix arg (== '.')

    splitLongestPrefix :: String -> (Char -> Bool) -> (String,String)
    splitLongestPrefix str pred'
      | null r_pre = (str,           [])
      | otherwise  = (reverse (tail r_pre), reverse r_suf)
                           -- 'tail' drops the char satisfying 'pred'
      where (r_suf, r_pre) = break pred' (reverse str)

-- | Return C sources, GHC input files and GHC input modules
gbuildSources :: Verbosity
              -> Version -- ^ specVersion
              -> FilePath
              -> GBuildMode
              -> IO ([FilePath], [FilePath], [ModuleName])
gbuildSources verbosity specVer tmpDir bm =
    case bm of
      GBuildExe  exe  -> exeSources exe
      GReplExe   exe  -> exeSources exe
      GBuildFLib flib -> return $ flibSources flib
      GReplFLib  flib -> return $ flibSources flib
  where
    exeSources :: Executable -> IO ([FilePath], [FilePath], [ModuleName])
    exeSources exe@Executable{buildInfo = bnfo, modulePath = modPath} = do
      main <- findFile (tmpDir : hsSourceDirs bnfo) modPath
      let mainModName = fromMaybe ModuleName.main $ exeMainModuleName exe
          otherModNames = exeModules exe

      if isHaskell main
        then
          if specVer < mkVersion [2] && (mainModName `elem` otherModNames)
          then do
             -- The cabal manual clearly states that `other-modules` is
             -- intended for non-main modules.  However, there's at least one
             -- important package on Hackage (happy-1.19.5) which
             -- violates this. We workaround this here so that we don't
             -- invoke GHC with e.g.  'ghc --make Main src/Main.hs' which
             -- would result in GHC complaining about duplicate Main
             -- modules.
             --
             -- Finally, we only enable this workaround for
             -- specVersion < 2, as 'cabal-version:>=2.0' cabal files
             -- have no excuse anymore to keep doing it wrong... ;-)
             warn verbosity $ "Enabling workaround for Main module '"
                            ++ display mainModName
                            ++ "' listed in 'other-modules' illegally!"

             return   (cSources bnfo, [main],
                       filter (/= mainModName) (exeModules exe))

          else return (cSources bnfo, [main], exeModules exe)
        else return (main : cSources bnfo, [], exeModules exe)

    flibSources :: ForeignLib -> ([FilePath], [FilePath], [ModuleName])
    flibSources flib@ForeignLib{foreignLibBuildInfo = bnfo} =
      (cSources bnfo, [], foreignLibModules flib)

    isHaskell :: FilePath -> Bool
    isHaskell fp = elem (takeExtension fp) [".hs", ".lhs"]

-- | Generic build function. See comment for 'GBuildMode'.
gbuild :: Verbosity          -> Cabal.Flag (Maybe Int)
       -> PackageDescription -> LocalBuildInfo
       -> GBuildMode         -> ComponentLocalBuildInfo -> IO ()
gbuild verbosity numJobs pkg_descr lbi bm clbi = do
  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  let comp       = compiler lbi
      platform   = hostPlatform lbi
      implInfo   = getImplInfo comp
      runGhcProg = runGHC verbosity ghcProg comp platform

  bnfo <- hackThreadedFlag verbosity
            comp (withProfExe lbi) (gbuildInfo bm)

  -- the name that GHC really uses (e.g., with .exe on Windows for executables)
  let targetName = gbuildTargetName lbi bm
  let targetDir  = buildDir lbi </> (gbuildName bm)
  let tmpDir     = targetDir    </> (gbuildName bm ++ "-tmp")
  createDirectoryIfMissingVerbose verbosity True targetDir
  createDirectoryIfMissingVerbose verbosity True tmpDir

  -- TODO: do we need to put hs-boot files into place for mutually recursive
  -- modules?  FIX: what about exeName.hi-boot?

  -- Determine if program coverage should be enabled and if so, what
  -- '-hpcdir' should be.
  let isCoverageEnabled = exeCoverage lbi
      distPref = fromFlag $ configDistPref $ configFlags lbi
      hpcdir way
        | gbuildIsRepl bm   = mempty  -- HPC is not supported in ghci
        | isCoverageEnabled = toFlag $ Hpc.mixDir distPref way (gbuildName bm)
        | otherwise         = mempty

  rpaths <- getRPaths lbi clbi
  (cSrcs, inputFiles, inputModules) <- gbuildSources verbosity
                                       (specVersion pkg_descr) tmpDir bm

  let isGhcDynamic        = isDynamic comp
      dynamicTooSupported = supportsDynamicToo comp
      cObjs               = map (`replaceExtension` objExtension) cSrcs
      needDynamic         = gbuildNeedDynamic lbi bm
      needProfiling       = withProfExe lbi

  -- build executables
      baseOpts   = (componentGhcOptions verbosity lbi bnfo clbi tmpDir)
                    `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeMake,
                      ghcOptInputFiles   = toNubListR inputFiles,
                      ghcOptInputModules = toNubListR inputModules
                    }
      staticOpts = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticOnly,
                      ghcOptHPCDir         = hpcdir Hpc.Vanilla
                   }
      profOpts   = baseOpts `mappend` mempty {
                      ghcOptProfilingMode  = toFlag True,
                      ghcOptProfilingAuto  = Internal.profDetailLevelFlag False
                                             (withProfExeDetail lbi),
                      ghcOptHiSuffix       = toFlag "p_hi",
                      ghcOptObjSuffix      = toFlag "p_o",
                      ghcOptExtra          = toNubListR
                                             (hcProfOptions GHC bnfo),
                      ghcOptHPCDir         = hpcdir Hpc.Prof
                    }
      dynOpts    = baseOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcDynamicOnly,
                      -- TODO: Does it hurt to set -fPIC for executables?
                      ghcOptFPic           = toFlag True,
                      ghcOptHiSuffix       = toFlag "dyn_hi",
                      ghcOptObjSuffix      = toFlag "dyn_o",
                      ghcOptExtra          = toNubListR $
                                             hcSharedOptions GHC bnfo,
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      dynTooOpts = staticOpts `mappend` mempty {
                      ghcOptDynLinkMode    = toFlag GhcStaticAndDynamic,
                      ghcOptDynHiSuffix    = toFlag "dyn_hi",
                      ghcOptDynObjSuffix   = toFlag "dyn_o",
                      ghcOptHPCDir         = hpcdir Hpc.Dyn
                    }
      linkerOpts = mempty {
                      ghcOptLinkOptions       = toNubListR $ PD.ldOptions bnfo,
                      ghcOptLinkLibs          = toNubListR $ extraLibs bnfo,
                      ghcOptLinkLibPath       = toNubListR $ extraLibDirs bnfo,
                      ghcOptLinkFrameworks    = toNubListR $
                                                PD.frameworks bnfo,
                      ghcOptLinkFrameworkDirs = toNubListR $
                                                PD.extraFrameworkDirs bnfo,
                      ghcOptInputFiles     = toNubListR
                                             [tmpDir </> x | x <- cObjs]
                    }
      dynLinkerOpts = mempty {
                      ghcOptRPaths         = rpaths
                   }
      replOpts   = baseOpts {
                    ghcOptExtra            = overNubListR
                                             Internal.filterGhciFlags
                                             (ghcOptExtra baseOpts)
                   }
                   -- For a normal compile we do separate invocations of ghc for
                   -- compiling as for linking. But for repl we have to do just
                   -- the one invocation, so that one has to include all the
                   -- linker stuff too, like -l flags and any .o files from C
                   -- files etc.
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptMode         = toFlag GhcModeInteractive,
                      ghcOptOptimisation = toFlag GhcNoOptimisation
                     }
      commonOpts  | needProfiling = profOpts
                  | needDynamic   = dynOpts
                  | otherwise     = staticOpts
      compileOpts | useDynToo = dynTooOpts
                  | otherwise = commonOpts
      withStaticExe = not needProfiling && not needDynamic

      -- For building exe's that use TH with -prof or -dynamic we actually have
      -- to build twice, once without -prof/-dynamic and then again with
      -- -prof/-dynamic. This is because the code that TH needs to run at
      -- compile time needs to be the vanilla ABI so it can be loaded up and run
      -- by the compiler.
      -- With dynamic-by-default GHC the TH object files loaded at compile-time
      -- need to be .dyn_o instead of .o.
      doingTH = usesTemplateHaskellOrQQ bnfo
      -- Should we use -dynamic-too instead of compiling twice?
      useDynToo = dynamicTooSupported && isGhcDynamic
                  && doingTH && withStaticExe
                  && null (hcSharedOptions GHC bnfo)
      compileTHOpts | isGhcDynamic = dynOpts
                    | otherwise    = staticOpts
      compileForTH
        | gbuildIsRepl bm = False
        | useDynToo       = False
        | isGhcDynamic    = doingTH && (needProfiling || withStaticExe)
        | otherwise       = doingTH && (needProfiling || needDynamic)

   -- Build static/dynamic object files for TH, if needed.
  when compileForTH $
    runGhcProg compileTHOpts { ghcOptNoLink  = toFlag True
                             , ghcOptNumJobs = numJobs }

  -- Do not try to build anything if there are no input files.
  -- This can happen if the cabal file ends up with only cSrcs
  -- but no Haskell modules.
  unless ((null inputFiles && null inputModules)
          || gbuildIsRepl bm) $
    runGhcProg compileOpts { ghcOptNoLink  = toFlag True
                           , ghcOptNumJobs = numJobs }

  -- build any C sources
  unless (null cSrcs) $ do
   info verbosity "Building C Sources..."
   sequence_
     [ do let baseCcOpts    = Internal.componentCcGhcOptions verbosity implInfo
                              lbi bnfo clbi tmpDir filename
              vanillaCcOpts = if isGhcDynamic
                              -- Dynamic GHC requires C sources to be built
                              -- with -fPIC for REPL to work. See #2207.
                              then baseCcOpts { ghcOptFPic = toFlag True }
                              else baseCcOpts
              profCcOpts    = vanillaCcOpts `mappend` mempty {
                                ghcOptProfilingMode = toFlag True
                              }
              sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                                ghcOptFPic        = toFlag True,
                                ghcOptDynLinkMode = toFlag GhcDynamicOnly
                              }
              opts | needProfiling = profCcOpts
                   | needDynamic   = sharedCcOpts
                   | otherwise     = vanillaCcOpts
              odir = fromFlag (ghcOptObjDir opts)
          createDirectoryIfMissingVerbose verbosity True odir
          needsRecomp <- checkNeedsRecompilation filename opts
          when needsRecomp $
            runGhcProg opts
     | filename <- cSrcs ]

  -- TODO: problem here is we need the .c files built first, so we can load them
  -- with ghci, but .c files can depend on .h files generated by ghc by ffi
  -- exports.
  case bm of
    GReplExe  _ -> runGhcProg replOpts
    GReplFLib _ -> runGhcProg replOpts
    GBuildExe _ -> do
      let linkOpts = commonOpts
                   `mappend` linkerOpts
                   `mappend` mempty {
                      ghcOptLinkNoHsMain = toFlag (null inputFiles)
                     }
                   `mappend` (if withDynExe lbi then dynLinkerOpts else mempty)

      info verbosity "Linking..."
      -- Work around old GHCs not relinking in this
      -- situation, see #3294
      let target = targetDir </> targetName
      when (compilerVersion comp < mkVersion [7,7]) $ do
        e <- doesFileExist target
        when e (removeFile target)
      runGhcProg linkOpts { ghcOptOutputFile = toFlag target }
    GBuildFLib flib -> do
      let rtsInfo  = extractRtsInfo lbi
          linkOpts = case foreignLibType flib of
            ForeignLibNativeShared ->
                        commonOpts
              `mappend` linkerOpts
              `mappend` dynLinkerOpts
              `mappend` mempty {
                 ghcOptLinkNoHsMain    = toFlag True,
                 ghcOptShared          = toFlag True,
                 ghcOptLinkLibs        = toNubListR [
                                           if needDynamic
                                             then rtsDynamicLib rtsInfo
                                             else rtsStaticLib  rtsInfo
                                           ],
                 ghcOptLinkLibPath     = toNubListR $ rtsLibPaths rtsInfo,
                 ghcOptFPic            = toFlag True,
                 ghcOptLinkModDefFiles = toNubListR $ gbuildModDefFiles bm
                }
              -- See Note [RPATH]
              `mappend` ifNeedsRPathWorkaround lbi mempty {
                  ghcOptLinkOptions = toNubListR ["-Wl,--no-as-needed"]
                , ghcOptLinkLibs    = toNubListR ["ffi"]
                }
            ForeignLibNativeStatic ->
              -- this should be caught by buildFLib
              -- (and if we do implement tihs, we probably don't even want to call
              -- ghc here, but rather Ar.createArLibArchive or something)
              cabalBug "static libraries not yet implemented"
            ForeignLibTypeUnknown ->
              cabalBug "unknown foreign lib type"
      -- We build under a (potentially) different filename to set a
      -- soname on supported platforms.  See also the note for
      -- @flibBuildName@.
      info verbosity "Linking..."
      let buildName = flibBuildName lbi flib
      runGhcProg linkOpts { ghcOptOutputFile = toFlag (targetDir </> buildName) }
      renameFile (targetDir </> buildName) (targetDir </> targetName)

{-
Note [RPATH]
~~~~~~~~~~~~

Suppose that the dynamic library depends on `base`, but not (directly) on
`integer-gmp` (which, however, is a dependency of `base`). We will link the
library as

    gcc ... -lHSbase-4.7.0.2-ghc7.8.4 -lHSinteger-gmp-0.5.1.0-ghc7.8.4 ...

However, on systems (like Ubuntu) where the linker gets called with `-as-needed`
by default, the linker will notice that `integer-gmp` isn't actually a direct
dependency and hence omit the link.

Then when we attempt to link a C program against this dynamic library, the
_static_ linker will attempt to verify that all symbols can be resolved.  The
dynamic library itself does not require any symbols from `integer-gmp`, but
`base` does. In order to verify that the symbols used by `base` can be
resolved, the static linker needs to be able to _find_ integer-gmp.

Finding the `base` dependency is simple, because the dynamic elf header
(`readelf -d`) for the library that we have created looks something like

    (NEEDED) Shared library: [libHSbase-4.7.0.2-ghc7.8.4.so]
    (RPATH)  Library rpath: [/path/to/base-4.7.0.2:...]

However, when it comes to resolving the dependency on `integer-gmp`, it needs
to look at the dynamic header for `base`. On modern ghc (7.8 and higher) this
looks something like

    (NEEDED) Shared library: [libHSinteger-gmp-0.5.1.0-ghc7.8.4.so]
    (RPATH)  Library rpath: [$ORIGIN/../integer-gmp-0.5.1.0:...]

This specifies the location of `integer-gmp` _in terms of_ the location of base
(using the `$ORIGIN`) variable. But here's the crux: when the static linker
attempts to verify that all symbols can be resolved, [**IT DOES NOT RESOLVE
`$ORIGIN`**](http://stackoverflow.com/questions/6323603/ld-using-rpath-origin-inside-a-shared-library-recursive).
As a consequence, it will not be able to resolve the symbols and report the
missing symbols as errors, _even though the dynamic linker **would** be able to
resolve these symbols_. We can tell the static linker not to report these
errors by using `--unresolved-symbols=ignore-all` and all will be fine when we
run the program ([(indeed, this is what the gold linker
does)](https://sourceware.org/ml/binutils/2013-05/msg00038.html), but it makes
the resulting library more difficult to use.

Instead what we can do is make sure that the generated dynamic library has
explicit top-level dependencies on these libraries. This means that the static
linker knows where to find them, and when we have transitive dependencies on
the same libraries the linker will only load them once, so we avoid needing to
look at the `RPATH` of our dependencies. We can do this by passing
`--no-as-needed` to the linker, so that it doesn't omit any libraries.

Note that on older ghc (7.6 and before) the Haskell libraries don't have an
RPATH set at all, which makes it even more important that we make these
top-level dependencies.

Finally, we have to explicitly link against `libffi` for the same reason. For
newer ghc this _happens_ to be unnecessary on many systems because `libffi` is
a library which is not specific to GHC, and when the static linker verifies
that all symbols can be resolved it will find the `libffi` that is globally
installed (completely independent from ghc). Of course, this may well be the
_wrong_ version of `libffi`, but it's quite possible that symbol resolution
happens to work. This is of course the wrong approach, which is why we link
explicitly against `libffi` so that we will find the _right_ version of
`libffi`.
-}

-- | Do we need the RPATH workaround?
--
-- See Note [RPATH].
ifNeedsRPathWorkaround :: Monoid a => LocalBuildInfo -> a -> a
ifNeedsRPathWorkaround lbi a =
  case hostPlatform lbi of
    Platform _ Linux -> a
    _otherwise       -> mempty

data RtsInfo = RtsInfo {
    rtsDynamicLib :: FilePath
  , rtsStaticLib  :: FilePath
  , rtsLibPaths   :: [FilePath]
  }

-- | Extract (and compute) information about the RTS library
--
-- TODO: This hardcodes the name as @HSrts-ghc<version>@. I don't know if we can
-- find this information somewhere. We can lookup the 'hsLibraries' field of
-- 'InstalledPackageInfo' but it will tell us @["HSrts", "Cffi"]@, which
-- doesn't really help.
extractRtsInfo :: LocalBuildInfo -> RtsInfo
extractRtsInfo lbi =
    case PackageIndex.lookupPackageName (installedPkgs lbi) (mkPackageName "rts") of
      [(_, [rts])] -> aux rts
      _otherwise   -> error "No (or multiple) ghc rts package is registered"
  where
    aux :: InstalledPackageInfo -> RtsInfo
    aux rts = RtsInfo {
        rtsDynamicLib = "HSrts-ghc" ++ display ghcVersion
      , rtsStaticLib  = "HSrts"
      , rtsLibPaths   = InstalledPackageInfo.libraryDirs rts
      }
    ghcVersion :: Version
    ghcVersion = compilerVersion (compiler lbi)

-- | Returns True if the modification date of the given source file is newer than
-- the object file we last compiled for it, or if no object file exists yet.
checkNeedsRecompilation :: FilePath -> GhcOptions -> NoCallStackIO Bool
checkNeedsRecompilation filename opts = filename `moreRecentFile` oname
    where oname = getObjectFileName filename opts

-- | Finds the object file name of the given source file
getObjectFileName :: FilePath -> GhcOptions -> FilePath
getObjectFileName filename opts = oname
    where odir  = fromFlag (ghcOptObjDir opts)
          oext  = fromFlagOrDefault "o" (ghcOptObjSuffix opts)
          oname = odir </> replaceExtension filename oext

-- | Calculate the RPATHs for the component we are building.
--
-- Calculates relative RPATHs when 'relocatable' is set.
getRPaths :: LocalBuildInfo
          -> ComponentLocalBuildInfo -- ^ Component we are building
          -> NoCallStackIO (NubListR FilePath)
getRPaths lbi clbi | supportRPaths hostOS = do
    libraryPaths <- depLibraryPaths False (relocatable lbi) lbi clbi
    let hostPref = case hostOS of
                     OSX -> "@loader_path"
                     _   -> "$ORIGIN"
        relPath p = if isRelative p then hostPref </> p else p
        rpaths    = toNubListR (map relPath libraryPaths)
    return rpaths
  where
    (Platform _ hostOS) = hostPlatform lbi
    compid              = compilerId . compiler $ lbi

    -- The list of RPath-supported operating systems below reflects the
    -- platforms on which Cabal's RPATH handling is tested. It does _NOT_
    -- reflect whether the OS supports RPATH.

    -- E.g. when this comment was written, the *BSD operating systems were
    -- untested with regards to Cabal RPATH handling, and were hence set to
    -- 'False', while those operating systems themselves do support RPATH.
    supportRPaths Linux       = True
    supportRPaths Windows     = False
    supportRPaths OSX         = True
    supportRPaths FreeBSD     =
      case compid of
        CompilerId GHC ver | ver >= mkVersion [7,10,2] -> True
        _                                              -> False
    supportRPaths OpenBSD     = False
    supportRPaths NetBSD      = False
    supportRPaths DragonFly   = False
    supportRPaths Solaris     = False
    supportRPaths AIX         = False
    supportRPaths HPUX        = False
    supportRPaths IRIX        = False
    supportRPaths HaLVM       = False
    supportRPaths IOS         = False
    supportRPaths Android     = False
    supportRPaths Ghcjs       = False
    supportRPaths Hurd        = False
    supportRPaths (OtherOS _) = False
    -- Do _not_ add a default case so that we get a warning here when a new OS
    -- is added.

getRPaths _ _ = return mempty

-- | Filter the "-threaded" flag when profiling as it does not
--   work with ghc-6.8 and older.
hackThreadedFlag :: Verbosity -> Compiler -> Bool -> BuildInfo -> IO BuildInfo
hackThreadedFlag verbosity comp prof bi
  | not mustFilterThreaded = return bi
  | otherwise              = do
    warn verbosity $ "The ghc flag '-threaded' is not compatible with "
                  ++ "profiling in ghc-6.8 and older. It will be disabled."
    return bi { options = filterHcOptions (/= "-threaded") (options bi) }
  where
    mustFilterThreaded = prof && compilerVersion comp < mkVersion [6, 10]
                      && "-threaded" `elem` hcOptions GHC bi
    filterHcOptions p hcoptss =
      [ (hc, if hc == GHC then filter p opts else opts)
      | (hc, opts) <- hcoptss ]


-- | Extracts a String representing a hash of the ABI of a built
-- library.  It can fail if the library has not yet been built.
--
libAbiHash :: Verbosity -> PackageDescription -> LocalBuildInfo
           -> Library -> ComponentLocalBuildInfo -> IO String
libAbiHash verbosity _pkg_descr lbi lib clbi = do
  libBi <- hackThreadedFlag verbosity
             (compiler lbi) (withProfLib lbi) (libBuildInfo lib)
  let
      comp        = compiler lbi
      platform    = hostPlatform lbi
      vanillaArgs0 =
        (componentGhcOptions verbosity lbi libBi clbi (componentBuildDir lbi clbi))
        `mappend` mempty {
          ghcOptMode         = toFlag GhcModeAbiHash,
          ghcOptInputModules = toNubListR $ exposedModules lib
        }
      vanillaArgs =
          -- Package DBs unnecessary, and break ghc-cabal. See #3633
          -- BUT, put at least the global database so that 7.4 doesn't
          -- break.
          vanillaArgs0 { ghcOptPackageDBs = [GlobalPackageDB]
                       , ghcOptPackages = mempty }
      sharedArgs = vanillaArgs `mappend` mempty {
                       ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                       ghcOptFPic        = toFlag True,
                       ghcOptHiSuffix    = toFlag "dyn_hi",
                       ghcOptObjSuffix   = toFlag "dyn_o",
                       ghcOptExtra       = toNubListR $ hcSharedOptions GHC libBi
                   }
      profArgs   = vanillaArgs `mappend` mempty {
                     ghcOptProfilingMode = toFlag True,
                     ghcOptProfilingAuto = Internal.profDetailLevelFlag True
                                             (withProfLibDetail lbi),
                     ghcOptHiSuffix      = toFlag "p_hi",
                     ghcOptObjSuffix     = toFlag "p_o",
                     ghcOptExtra         = toNubListR $ hcProfOptions GHC libBi
                   }
      ghcArgs
        | withVanillaLib lbi = vanillaArgs
        | withSharedLib lbi = sharedArgs
        | withProfLib lbi = profArgs
        | otherwise = error "libAbiHash: Can't find an enabled library way"

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi)
  hash <- getProgramInvocationOutput verbosity
          (ghcInvocation ghcProg comp platform ghcArgs)
  return (takeWhile (not . isSpace) hash)

componentGhcOptions :: Verbosity -> LocalBuildInfo
                    -> BuildInfo -> ComponentLocalBuildInfo -> FilePath
                    -> GhcOptions
componentGhcOptions verbosity lbi =
  Internal.componentGhcOptions verbosity implInfo lbi
  where
    comp     = compiler lbi
    implInfo = getImplInfo comp

componentCcGhcOptions :: Verbosity -> LocalBuildInfo
                      -> BuildInfo -> ComponentLocalBuildInfo
                      -> FilePath -> FilePath
                      -> GhcOptions
componentCcGhcOptions verbosity lbi =
    Internal.componentCcGhcOptions verbosity implInfo lbi
  where
    comp     = compiler lbi
    implInfo = getImplInfo comp

-- -----------------------------------------------------------------------------
-- Installing

-- |Install executables for GHC.
installExe :: Verbosity
           -> LocalBuildInfo
           -> FilePath -- ^Where to copy the files to
           -> FilePath  -- ^Build location
           -> (FilePath, FilePath)  -- ^Executable (prefix,suffix)
           -> PackageDescription
           -> Executable
           -> IO ()
installExe verbosity lbi binDir buildPref
  (progprefix, progsuffix) _pkg exe = do
  createDirectoryIfMissingVerbose verbosity True binDir
  let exeName' = unUnqualComponentName $ exeName exe
      exeFileName = exeTargetName exe
      fixedExeBaseName = progprefix ++ exeName' ++ progsuffix
      installBinary dest = do
          installExecutableFile verbosity
            (buildPref </> exeName' </> exeFileName)
            (dest <.> exeExtension)
          when (stripExes lbi) $
            Strip.stripExe verbosity (hostPlatform lbi) (withPrograms lbi)
                           (dest <.> exeExtension)
  installBinary (binDir </> fixedExeBaseName)

-- |Install foreign library for GHC.
installFLib :: Verbosity
            -> LocalBuildInfo
            -> FilePath  -- ^install location
            -> FilePath  -- ^Build location
            -> PackageDescription
            -> ForeignLib
            -> IO ()
installFLib verbosity lbi targetDir builtDir _pkg flib =
    install (foreignLibIsShared flib)
            builtDir
            targetDir
            (flibTargetName lbi flib)
  where
    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name
      createDirectoryIfMissingVerbose verbosity True targetDir
      -- TODO: Should we strip? (stripLibs lbi)
      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst
      -- Now install appropriate symlinks if library is versioned
      let (Platform _ os) = hostPlatform lbi
      when (not (null (foreignLibVersion flib os))) $ do
          when (os /= Linux) $ die' verbosity
            -- It should be impossible to get here.
            "Can't install foreign-library symlink on non-Linux OS"
#ifndef mingw32_HOST_OS
          -- 'createSymbolicLink file1 file2' creates a symbolic link
          -- named 'file2' which points to the file 'file1'.
          -- Note that we do want a symlink to 'name' rather than
          -- 'dst', because the symlink will be relative to the
          -- directory it's created in.
          -- Finally, we first create the symlinks in a temporary
          -- directory and then rename to simulate 'ln --force'.
          withTempDirectory verbosity dstDir nm $ \tmpDir -> do
              let link1 = flibBuildName lbi flib
                  link2 = "lib" ++ nm <.> "so"
              createSymbolicLink name (tmpDir </> link1)
              renameFile (tmpDir </> link1) (dstDir </> link1)
              createSymbolicLink name (tmpDir </> link2)
              renameFile (tmpDir </> link2) (dstDir </> link2)
        where
          nm :: String
          nm = unUnqualComponentName $ foreignLibName flib
#endif /* mingw32_HOST_OS */


-- |Install for ghc, .hi, .a and, if --with-ghci given, .o
installLib    :: Verbosity
              -> LocalBuildInfo
              -> FilePath  -- ^install location
              -> FilePath  -- ^install location for dynamic libraries
              -> FilePath  -- ^Build location
              -> PackageDescription
              -> Library
              -> ComponentLocalBuildInfo
              -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir _builtDir _pkg lib clbi = do
  -- copy .hi files over:
  whenVanilla $ copyModuleFiles "hi"
  whenProf    $ copyModuleFiles "p_hi"
  whenShared  $ copyModuleFiles "dyn_hi"

  -- copy the built library files over:
  whenHasCode $ do
    whenVanilla $ do
      sequence_ [ installOrdinary builtDir targetDir       (mkGenericStaticLibName (l ++ f))
                | l <- getHSLibraryName (componentUnitId clbi):(extraBundledLibs (libBuildInfo lib))
                , f <- "":extraLibFlavours (libBuildInfo lib)
                ]
    whenProf    $ installOrdinary builtDir targetDir       profileLibName
    whenGHCi    $ installOrdinary builtDir targetDir       ghciLibName
    whenShared  $ installShared   builtDir dynlibTargetDir sharedLibName

  where
    builtDir = componentBuildDir lbi clbi

    install isShared srcDir dstDir name = do
      let src = srcDir </> name
          dst = dstDir </> name

      createDirectoryIfMissingVerbose verbosity True dstDir

      if isShared
        then installExecutableFile verbosity src dst
        else installOrdinaryFile   verbosity src dst

      when (stripLibs lbi) $ Strip.stripLib verbosity
                             (hostPlatform lbi) (withPrograms lbi) dst

    installOrdinary = install False
    installShared   = install True

    copyModuleFiles ext =
      findModuleFiles [builtDir] [ext] (allLibModules lib clbi)
      >>= installOrdinaryFiles verbosity targetDir

    compiler_id = compilerId (compiler lbi)
    uid = componentUnitId clbi
    profileLibName = mkProfLibName          uid
    ghciLibName    = Internal.mkGHCiLibName uid
    sharedLibName  = (mkSharedLibName compiler_id) uid

    hasLib    = not $ null (allLibModules lib clbi)
                   && null (cSources (libBuildInfo lib))
    has_code = not (componentIsIndefinite clbi)
    whenHasCode = when has_code
    whenVanilla = when (hasLib && withVanillaLib lbi)
    whenProf    = when (hasLib && withProfLib    lbi && has_code)
    whenGHCi    = when (hasLib && withGHCiLib    lbi && has_code)
    whenShared  = when (hasLib && withSharedLib  lbi && has_code)

-- -----------------------------------------------------------------------------
-- Registering

hcPkgInfo :: ProgramDb -> HcPkg.HcPkgInfo
hcPkgInfo progdb = HcPkg.HcPkgInfo { HcPkg.hcPkgProgram    = ghcPkgProg
                                   , HcPkg.noPkgDbStack    = v < [6,9]
                                   , HcPkg.noVerboseFlag   = v < [6,11]
                                   , HcPkg.flagPackageConf = v < [7,5]
                                   , HcPkg.supportsDirDbs  = v >= [6,8]
                                   , HcPkg.requiresDirDbs  = v >= [7,10]
                                   , HcPkg.nativeMultiInstance  = v >= [7,10]
                                   , HcPkg.recacheMultiInstance = v >= [6,12]
                                   , HcPkg.suppressFilesCheck   = v >= [6,6]
                                   }
  where
    v               = versionNumbers ver
    Just ghcPkgProg = lookupProgram ghcPkgProgram progdb
    Just ver        = programVersion ghcPkgProg

registerPackage
  :: Verbosity
  -> ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> HcPkg.RegisterOptions
  -> IO ()
registerPackage verbosity progdb packageDbs installedPkgInfo registerOptions =
    HcPkg.register (hcPkgInfo progdb) verbosity packageDbs
                   installedPkgInfo registerOptions

pkgRoot :: Verbosity -> LocalBuildInfo -> PackageDB -> IO FilePath
pkgRoot verbosity lbi = pkgRoot'
   where
    pkgRoot' GlobalPackageDB =
      let Just ghcProg = lookupProgram ghcProgram (withPrograms lbi)
      in  fmap takeDirectory (getGlobalPackageDB verbosity ghcProg)
    pkgRoot' UserPackageDB = do
      appDir <- getAppUserDataDirectory "ghc"
      let ver      = compilerVersion (compiler lbi)
          subdir   = System.Info.arch ++ '-':System.Info.os
                     ++ '-':display ver
          rootDir  = appDir </> subdir
      -- We must create the root directory for the user package database if it
      -- does not yet exists. Otherwise '${pkgroot}' will resolve to a
      -- directory at the time of 'ghc-pkg register', and registration will
      -- fail.
      createDirectoryIfMissing True rootDir
      return rootDir
    pkgRoot' (SpecificPackageDB fp) = return (takeDirectory fp)

-- -----------------------------------------------------------------------------
-- Utils

isDynamic :: Compiler -> Bool
isDynamic = Internal.ghcLookupProperty "GHC Dynamic"

supportsDynamicToo :: Compiler -> Bool
supportsDynamicToo = Internal.ghcLookupProperty "Support dynamic-too"

withExt :: FilePath -> String -> FilePath
withExt fp ext = fp <.> if takeExtension fp /= ('.':ext) then ext else ""
