{-# LANGUAGE FlexibleContexts #-}
module Rules.Install (installRules) where

import Hadrian.Oracles.DirectoryContents

import Base
import Expression hiding (builder)
import Target
import Context
import Settings
import Settings.Path
import Util
import GHC
import Rules
import Rules.Generate
import Rules.Libffi
import Rules.Wrappers
import Settings.Packages.Rts
import Oracles.Config.Setting
import Oracles.Dependencies

import qualified System.Directory as IO

{- | Install the built binaries etc. to the @destDir ++ prefix@.

The installation prefix is usually @/usr/local@ on a Unix system.
The resulting tree structure is organized under @destDir ++ prefix@ as follows:

* @bin@: executable wrapper scripts, installed by 'installBins', e.g. @ghc@.

* @lib/ghc-<version>/bin@: executable binaries/scripts,
  installed by 'installLibExecs' and 'installLibExecScripts'.

* @lib/ghc-<version>/include@: headers etc., installed by 'installIncludes'.

* @lib/ghc-<version>/<pkg-name>@: built packages, e.g. @base@, installed
  by 'installPackages'.

* @lib/ghc-<version>/settings@ etc.: other files in @lib@ directory,
  installed by 'installCommonLibs'.

XXX (izgzhen): Do we need @INSTALL_OPTS@ in the make scripts?
-}
installRules :: Rules ()
installRules = do
    "install" ~> do
        installIncludes
        installPackageConf
        installCommonLibs
        installLibExecs
        installLibExecScripts
        installBins
        installPackages

-- | Binaries to install.
installBinPkgs :: [Package]
installBinPkgs = [ghc, ghcPkg, ghcSplit, hp2ps, hpc, hsc2hs, runGhc, unlit]

getLibExecDir :: Action FilePath
getLibExecDir = (-/- "bin") <$> installGhcLibDir

-- ref: ghc.mk
-- | Install executable scripts to @prefix/lib/bin@.
installLibExecScripts :: Action ()
installLibExecScripts = do
    libExecDir <- getLibExecDir
    installDirectory (destDir ++ libExecDir)
    forM_ libExecScripts $ \script -> do
        installScript script (destDir ++ libExecDir)
  where
    libExecScripts :: [FilePath]
    libExecScripts = [ghcSplitPath]

-- ref: ghc.mk
-- | Install executable binaries to @prefix/lib/bin@.
installLibExecs :: Action ()
installLibExecs = do
    libExecDir <- getLibExecDir
    installDirectory (destDir ++ libExecDir)
    forM_ installBinPkgs $ \pkg -> do
        withLatestBuildStage pkg $ \stage -> do
            let context = programContext stage pkg
                bin     = inplaceLibBinPath -/- programName context <.> exe
            installProgram bin (destDir ++ libExecDir)
            when (pkg == ghc) $ do
                moveFile (destDir ++ libExecDir -/- programName context <.> exe)
                         (destDir ++ libExecDir -/- "ghc" <.> exe)

-- ref: ghc.mk
-- | Install executable wrapper scripts to @prefix/bin@.
installBins :: Action ()
installBins = do
    binDir <- setting InstallBinDir
    libDir <- installGhcLibDir
    installDirectory (destDir ++ binDir)
    win <- windowsHost
    when win $
        copyDirectoryContents matchAll (destDir ++ libDir -/- "bin") (destDir ++ binDir)
    unless win $ forM_ installBinPkgs $ \pkg ->
        withLatestBuildStage pkg $ \stage -> do
            let context = programContext stage pkg
            version <- setting ProjectVersion
            -- Name of the binary file
            let binName = if pkg == ghc
                          then "ghc-" ++ version <.> exe
                          else programName context ++ "-" ++ version <.> exe
            -- Name of the symbolic link
            let symName = if pkg == ghc
                          then "ghc" <.> exe
                          else programName context <.> exe
            case lookup context installWrappers of
                Nothing -> return ()
                Just wrapper -> do
                    contents <- interpretInContext context $
                        wrapper (WrappedBinary (destDir ++ libDir) symName)
                    let wrapperPath = destDir ++ binDir -/- binName
                    writeFileChanged wrapperPath contents
                    makeExecutable wrapperPath
                    unlessM windowsHost $
                        linkSymbolic (destDir ++ binDir -/- binName)
                                     (destDir ++ binDir -/- symName)

withLatestBuildStage :: Package -> (Stage -> Action ()) -> Action ()
withLatestBuildStage pkg m = do
  maybeStage <- latestBuildStage pkg
  case maybeStage of
      Just stage -> m stage
      Nothing    -> return ()

-- ref: rules/manual-package-conf.mk
-- TODO: Should we use a temporary file instead of pkgConfInstallPath?
-- | Install @package.conf.install@ for each package. Note that it will be
-- recreated each time.
installPackageConf :: Action ()
installPackageConf = do
    let context = vanillaContext Stage0 rts
    liftIO $ IO.createDirectoryIfMissing True (takeDirectory pkgConfInstallPath)
    build $ target context HsCpp [ pkgPath rts -/- "package.conf.in" ]
                                 [ pkgConfInstallPath <.> "raw" ]
    Stdout content <- cmd "grep" [ "-v", "^#pragma GCC"
                                 , pkgConfInstallPath <.> "raw" ]
    withTempFile $ \tmp -> do
        liftIO $ writeFile tmp content
        Stdout result <- cmd "sed" [ "-e", "s/\"\"//g", "-e", "s/:[   ]*,/: /g", tmp ]
        liftIO $ writeFile pkgConfInstallPath result

-- ref: ghc.mk
-- | Install packages to @prefix/lib@.
installPackages :: Action ()
installPackages = do
    need [pkgConfInstallPath]

    ghcLibDir <- installGhcLibDir
    binDir    <- setting InstallBinDir

    -- Install package.conf
    let installedPackageConf = destDir ++ ghcLibDir -/- "package.conf.d"
    installDirectory (destDir ++ ghcLibDir)
    removeDirectory installedPackageConf
    installDirectory installedPackageConf

    -- Install RTS
    let rtsDir = destDir ++ ghcLibDir -/- "rts"
    installDirectory rtsDir
    ways    <- interpretInContext (vanillaContext Stage1 rts) getRtsWays
    rtsLibs <- mapM pkgLibraryFile $ map (Context Stage1 rts) ways
    ffiLibs <- sequence $ map rtsLibffiLibrary ways

    -- TODO: Add dynamic ones
    forM_ (rtsLibs ++ ffiLibs) $ \lib -> installData [lib] rtsDir

    -- HACK (issue #327)
    let ghcBootPlatformHeader =
            buildPath (vanillaContext Stage1 compiler) -/- "ghc_boot_platform.h"

    copyFile ghcBootPlatformHeader (pkgPath compiler -/- "ghc_boot_platform.h")

    activePackages <- filterM ((isJust <$>) . latestBuildStage)
                              (knownPackages \\ [rts, libffi])

    installLibPkgs <- topsortPackages (filter isLibrary activePackages)

    forM_ installLibPkgs $ \pkg@Package{..} -> do
        when (isLibrary pkg) $
            withLatestBuildStage pkg $ \stage -> do
                let context = vanillaContext stage pkg
                top <- topDirectory
                let installDistDir = top -/- buildPath context
                need =<< packageTargets stage pkg
                docDir <- installDocDir
                ghclibDir <- installGhcLibDir

                -- Copy over packages
                strip <- stripCmdPath
                ways  <- interpretInContext context getLibraryWays
                let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal" <.> exe -- HACK?
                need [ghcCabalInplace]

                let cabalFile = pkgCabalFile pkg

                pkgConf <- pkgConfFile context
                need [cabalFile, pkgConf] -- TODO: check if need pkgConf

                -- HACK (#318): copy stuff back to the place favored by ghc-cabal
                quietly $ copyDirectoryContents (Not excluded)
                    installDistDir (installDistDir -/- "build")

                whenM (isSpecified HsColour) $
                    build $ target context GhcCabalHsColour [cabalFile] []

                pref <- setting InstallPrefix
                unit $ cmd ghcCabalInplace [ "copy"
                                           , pkgPath
                                           , installDistDir
                                           , strip
                                           , destDir
                                           , pref
                                           , ghclibDir
                                           , docDir -/- "html/libraries"
                                           , intercalate " " (map show ways) ]

    -- Register packages
    let installedGhcPkgReal = destDir ++ binDir -/- "ghc-pkg" <.> exe
        installedGhcReal    = destDir ++ binDir -/- "ghc"     <.> exe
    -- TODO: Extend GhcPkg builder args to support --global-package-db
    unit $ cmd installedGhcPkgReal [ "--force", "--global-package-db"
                                   , installedPackageConf, "update"
                                   , pkgConfInstallPath ]

    forM_ installLibPkgs $ \pkg@Package{..} -> do
        when (isLibrary pkg) $
            withLatestBuildStage pkg $ \stage -> do
                let context = vanillaContext stage pkg
                top <- topDirectory
                let installDistDir = top -/- buildPath context
                -- TODO: better reference to the built inplace binary path
                let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal"
                pref   <- setting InstallPrefix
                docDir <- installDocDir
                r      <- relocatableBuild
                unit $ cmd ghcCabalInplace
                    [ "register"
                    , pkgPath
                    , installDistDir
                    , installedGhcReal
                    , installedGhcPkgReal
                    , destDir ++ ghcLibDir
                    , destDir
                    , destDir ++ pref
                    , destDir ++ ghcLibDir
                    , destDir ++ docDir -/- "html/libraries"
                    , if r then "YES" else "NO" ]

    confs <- getDirectoryContents installedPackageConf
    forM_ confs (\f -> createData $ installedPackageConf -/- f)
    unit $ cmd installedGhcPkgReal [ "--force", "--global-package-db"
                                   , installedPackageConf, "recache" ]
  where
    createData f = unit $ cmd "chmod" [ "644", f ]
    excluded = Or [ Test "//haddock-prologue.txt"
                  , Test "//package-data.mk"
                  , Test "//setup-config"
                  , Test "//inplace-pkg-config"
                  , Test "//build" ]

-- ref: ghc.mk
-- | Install settings etc. files to @prefix/lib@.
installCommonLibs :: Action ()
installCommonLibs = do
    ghcLibDir <- installGhcLibDir
    installLibsTo inplaceLibCopyTargets (destDir ++ ghcLibDir)

-- ref: ghc.mk
-- | Install library files to some path.
installLibsTo :: [FilePath] -> FilePath -> Action ()
installLibsTo libs dir = do
    installDirectory dir
    forM_ libs $ \lib -> do
       case takeExtension lib of
           ".a" -> do
               let out = dir -/- takeFileName lib
               installData [out] dir
               let context = vanillaContext Stage0 $ topLevel (PackageName "")
               -- TODO: Get rid of meaningless context for certain builder like ranlib
               build $ target context Ranlib [out] [out]
           _ -> installData [lib] dir

-- ref: includes/ghc.mk
-- | All header files are in includes/{one of these subdirectories}.
includeHSubdirs :: [FilePath]
includeHSubdirs = [".", "rts", "rts/prof", "rts/storage", "stg"]

-- ref: includes/ghc.mk
-- | Install header files to @prefix/lib/ghc-<version>/include@.
installIncludes ::Action ()
installIncludes = do
    ghclibDir <- installGhcLibDir
    let ghcheaderDir = ghclibDir -/- "include"
    installDirectory (destDir ++ ghcheaderDir)
    forM_ includeHSubdirs $ \dir -> do
        installDirectory (destDir ++ ghcheaderDir -/- dir)
        headers <- getDirectoryFiles ("includes" -/- dir) ["*.h"]
        installHeader (map (("includes" -/- dir) -/-) headers)
                      (destDir ++ ghcheaderDir -/- dir ++ "/")
    installHeader (includesDependencies ++
                   [generatedPath -/- "DerivedConstants.h"] ++
                   libffiDependencies)
                  (destDir ++ ghcheaderDir ++ "/")
  where
    installHeader = installData -- they share same arguments
