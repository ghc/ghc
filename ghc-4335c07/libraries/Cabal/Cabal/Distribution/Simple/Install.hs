{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Install
-- Copyright   :  Isaac Jones 2003-2004
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This is the entry point into installing a built package. Performs the
-- \"@.\/setup install@\" and \"@.\/setup copy@\" actions. It moves files into
-- place based on the prefix argument. It does the generic bits and then calls
-- compiler-specific functions to do the rest.

module Distribution.Simple.Install (
        install,
  ) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.TargetInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.ForeignLib
import Distribution.Types.PackageDescription
import Distribution.Types.UnqualComponentName
import Distribution.Types.ExecutableScope

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.BuildPaths (haddockName, haddockPref)
import Distribution.Simple.Utils
         ( createDirectoryIfMissingVerbose
         , installDirectoryContents, installOrdinaryFile, isInSearchPath
         , die', info, noticeNoWrap, warn, matchDirFileGlob )
import Distribution.Simple.Compiler
         ( CompilerFlavor(..), compilerFlavor )
import Distribution.Simple.Setup
         ( CopyFlags(..), fromFlag, HaddockTarget(ForDevelopment) )
import Distribution.Simple.BuildTarget

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.GHCJS as GHCJS
import qualified Distribution.Simple.JHC   as JHC
import qualified Distribution.Simple.LHC   as LHC
import qualified Distribution.Simple.UHC   as UHC
import qualified Distribution.Simple.HaskellSuite as HaskellSuite
import Distribution.Compat.Graph (IsNode(..))

import System.Directory
         ( doesDirectoryExist, doesFileExist )
import System.FilePath
         ( takeFileName, takeDirectory, (</>), isRelative )

import Distribution.Verbosity
import Distribution.Text
         ( display )

-- |Perform the \"@.\/setup install@\" and \"@.\/setup copy@\"
-- actions.  Move files into place based on the prefix argument.
--
-- This does NOT register libraries, you should call 'register'
-- to do that.

install :: PackageDescription -- ^information from the .cabal file
        -> LocalBuildInfo -- ^information from the configure step
        -> CopyFlags -- ^flags sent to copy or install
        -> IO ()
install pkg_descr lbi flags = do
  checkHasLibsOrExes
  targets <- readTargetInfos verbosity pkg_descr lbi (copyArgs flags)

  copyPackage verbosity pkg_descr lbi distPref copydest

  -- It's not necessary to do these in build-order, but it's harmless
  withNeededTargetsInBuildOrder' pkg_descr lbi (map nodeKey targets) $ \target ->
    let comp = targetComponent target
        clbi = targetCLBI target
    in copyComponent verbosity pkg_descr lbi comp clbi copydest
 where
  distPref  = fromFlag (copyDistPref flags)
  verbosity = fromFlag (copyVerbosity flags)
  copydest  = fromFlag (copyDest flags)

  checkHasLibsOrExes =
    unless (hasLibs pkg_descr || hasForeignLibs pkg_descr || hasExes pkg_descr) $
      die' verbosity "No executables and no library found. Nothing to do."

-- | Copy package global files.
copyPackage :: Verbosity -> PackageDescription
            -> LocalBuildInfo -> FilePath -> CopyDest -> IO ()
copyPackage verbosity pkg_descr lbi distPref copydest = do
  let -- This is a bit of a hack, to handle files which are not
      -- per-component (data files and Haddock files.)
      InstallDirs {
         datadir    = dataPref,
         -- NB: The situation with Haddock is a bit delicate.  On the
         -- one hand, the easiest to understand Haddock documentation
         -- path is pkgname-0.1, which means it's per-package (not
         -- per-component).  But this means that it's impossible to
         -- install Haddock documentation for internal libraries.  We'll
         -- keep this constraint for now; this means you can't use
         -- Cabal to Haddock internal libraries.  This does not seem
         -- like a big problem.
         docdir     = docPref,
         htmldir    = htmlPref,
         haddockdir = interfacePref}
             -- Notice use of 'absoluteInstallDirs' (not the
             -- per-component variant).  This means for non-library
             -- packages we'll just pick a nondescriptive foo-0.1
             = absoluteInstallDirs pkg_descr lbi copydest

  -- Install (package-global) data files
  installDataFiles verbosity pkg_descr dataPref

  -- Install (package-global) Haddock files
  -- TODO: these should be done per-library
  docExists <- doesDirectoryExist $ haddockPref ForDevelopment distPref pkg_descr
  info verbosity ("directory " ++ haddockPref ForDevelopment distPref pkg_descr ++
                  " does exist: " ++ show docExists)

  -- TODO: this is a bit questionable, Haddock files really should
  -- be per library (when there are convenience libraries.)
  when docExists $ do
      createDirectoryIfMissingVerbose verbosity True htmlPref
      installDirectoryContents verbosity
          (haddockPref ForDevelopment distPref pkg_descr) htmlPref
      -- setPermissionsRecursive [Read] htmlPref
      -- The haddock interface file actually already got installed
      -- in the recursive copy, but now we install it where we actually
      -- want it to be (normally the same place). We could remove the
      -- copy in htmlPref first.
      let haddockInterfaceFileSrc  = haddockPref ForDevelopment distPref pkg_descr
                                                   </> haddockName pkg_descr
          haddockInterfaceFileDest = interfacePref </> haddockName pkg_descr
      -- We only generate the haddock interface file for libs, So if the
      -- package consists only of executables there will not be one:
      exists <- doesFileExist haddockInterfaceFileSrc
      when exists $ do
        createDirectoryIfMissingVerbose verbosity True interfacePref
        installOrdinaryFile verbosity haddockInterfaceFileSrc
                                      haddockInterfaceFileDest

  let lfiles = licenseFiles pkg_descr
  unless (null lfiles) $ do
    createDirectoryIfMissingVerbose verbosity True docPref
    sequence_
      [ installOrdinaryFile verbosity lfile (docPref </> takeFileName lfile)
      | lfile <- lfiles ]

-- | Copy files associated with a component.
copyComponent :: Verbosity -> PackageDescription
              -> LocalBuildInfo -> Component -> ComponentLocalBuildInfo
              -> CopyDest
              -> IO ()
copyComponent verbosity pkg_descr lbi (CLib lib) clbi copydest = do
    let InstallDirs{
            libdir = libPref,
            dynlibdir = dynlibPref,
            includedir = incPref
            } = absoluteComponentInstallDirs pkg_descr lbi (componentUnitId clbi) copydest
        buildPref = componentBuildDir lbi clbi

    case libName lib of
        Nothing -> noticeNoWrap verbosity ("Installing library in " ++ libPref)
        Just n -> noticeNoWrap verbosity ("Installing internal library " ++ display n ++ " in " ++ libPref)

    -- install include files for all compilers - they may be needed to compile
    -- haskell files (using the CPP extension)
    installIncludeFiles verbosity lib buildPref incPref

    case compilerFlavor (compiler lbi) of
      GHC   -> GHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      GHCJS -> GHCJS.installLib verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      LHC   -> LHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      JHC   -> JHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      UHC   -> UHC.installLib   verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      HaskellSuite _ -> HaskellSuite.installLib
                                verbosity lbi libPref dynlibPref buildPref pkg_descr lib clbi
      _ -> die' verbosity $ "installing with "
              ++ display (compilerFlavor (compiler lbi))
              ++ " is not implemented"

copyComponent verbosity pkg_descr lbi (CFLib flib) clbi copydest = do
    let InstallDirs{
            flibdir = flibPref
            } = absoluteComponentInstallDirs pkg_descr lbi (componentUnitId clbi) copydest
        buildPref = componentBuildDir lbi clbi

    noticeNoWrap verbosity ("Installing foreign library " ++ unUnqualComponentName (foreignLibName flib) ++ " in " ++ flibPref)

    case compilerFlavor (compiler lbi) of
      GHC   -> GHC.installFLib   verbosity lbi flibPref buildPref pkg_descr flib
      _ -> die' verbosity $ "installing foreign lib with "
              ++ display (compilerFlavor (compiler lbi))
              ++ " is not implemented"

copyComponent verbosity pkg_descr lbi (CExe exe) clbi copydest = do
    let installDirs = absoluteComponentInstallDirs pkg_descr lbi (componentUnitId clbi) copydest
        -- the installers know how to find the actual location of the
        -- binaries
        buildPref = buildDir lbi
        uid = componentUnitId clbi
        pkgid = packageId pkg_descr
        binPref | ExecutablePrivate <- exeScope exe = libexecdir installDirs
                | otherwise = bindir installDirs
        progPrefixPref = substPathTemplate pkgid lbi uid (progPrefix lbi)
        progSuffixPref = substPathTemplate pkgid lbi uid (progSuffix lbi)
        progFix = (progPrefixPref, progSuffixPref)
    noticeNoWrap verbosity ("Installing executable " ++ display (exeName exe)
                      ++ " in " ++ binPref)
    inPath <- isInSearchPath binPref
    when (not inPath) $
      warn verbosity ("The directory " ++ binPref
                      ++ " is not in the system search path.")
    case compilerFlavor (compiler lbi) of
      GHC   -> GHC.installExe   verbosity lbi binPref buildPref progFix pkg_descr exe
      GHCJS -> GHCJS.installExe verbosity lbi binPref buildPref progFix pkg_descr exe
      LHC   -> LHC.installExe   verbosity lbi binPref buildPref progFix pkg_descr exe
      JHC   -> JHC.installExe   verbosity     binPref buildPref progFix pkg_descr exe
      UHC   -> return ()
      HaskellSuite {} -> return ()
      _ -> die' verbosity $ "installing with "
              ++ display (compilerFlavor (compiler lbi))
              ++ " is not implemented"

-- Nothing to do for benchmark/testsuite
copyComponent _ _ _ (CBench _) _ _ = return ()
copyComponent _ _ _ (CTest _) _ _ = return ()

-- | Install the files listed in data-files
--
installDataFiles :: Verbosity -> PackageDescription -> FilePath -> IO ()
installDataFiles verbosity pkg_descr destDataDir =
  flip traverse_ (dataFiles pkg_descr) $ \ file -> do
    let srcDataDir = dataDir pkg_descr
    files <- matchDirFileGlob srcDataDir file
    let dir = takeDirectory file
    createDirectoryIfMissingVerbose verbosity True (destDataDir </> dir)
    sequence_ [ installOrdinaryFile verbosity (srcDataDir  </> file')
                                              (destDataDir </> file')
              | file' <- files ]

-- | Install the files listed in install-includes for a library
--
installIncludeFiles :: Verbosity -> Library -> FilePath -> FilePath -> IO ()
installIncludeFiles verbosity lib buildPref destIncludeDir = do
    let relincdirs = "." : filter isRelative (includeDirs lbi)
        lbi = libBuildInfo lib
        incdirs = relincdirs ++ [ buildPref </> dir | dir <- relincdirs ]
    incs <- traverse (findInc incdirs) (installIncludes lbi)
    sequence_
      [ do createDirectoryIfMissingVerbose verbosity True destDir
           installOrdinaryFile verbosity srcFile destFile
      | (relFile, srcFile) <- incs
      , let destFile = destIncludeDir </> relFile
            destDir  = takeDirectory destFile ]
  where

   findInc []         file = die' verbosity ("can't find include file " ++ file)
   findInc (dir:dirs) file = do
     let path = dir </> file
     exists <- doesFileExist path
     if exists then return (file, path) else findInc dirs file
