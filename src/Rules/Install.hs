{-# LANGUAGE FlexibleContexts #-}
module Rules.Install (installRules) where

import Base
import Target
import Context
import Predicate hiding (builder)
import Settings
import Settings.Path
import Util
import GHC
import Rules
import Rules.Wrappers (WrappedBinary(..), installWrappers)
import Rules.Libffi
import Rules.Generate
import Settings.Packages.Rts
import Oracles.Config.Setting
import Oracles.Dependencies (sortPkgsByDep)
import Oracles.Path

import qualified System.Directory as IO

{- | Install the built binaries etc. to the @destDir ++ prefix@.

The installation prefix is usually like @/usr/local@ on Unix system.
The resulting tree structure is organized under @destDir ++ prefix@, like below

* @bin@: executable wrapper scripts, installed by 'installBins', e.g. @ghc@
* @lib/ghc-<version>/bin@: executable binaries/scripts,
  installed by 'installLibExecs' and 'installLibExecScripts'
* @lib/ghc-<version>/include@: headers etc., installed by 'installIncludes'
* @lib/ghc-<version>/<pkg-name>@: built packages, installed by 'installPackages',
  e.g. @base@
* @lib/ghc-<version>/settings@ etc.: other files in @lib@ path, installed by
  'installCommonLibs'

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

getLibExecDir :: Action FilePath
getLibExecDir = (-/- "bin") <$> installGhcLibDir

-- | Install executable scripts to @prefix/lib/bin@
-- ref: ghc.mk
installLibExecScripts :: Action ()
installLibExecScripts = do
    libExecDir <- getLibExecDir
    installDirectory (destDir ++ libExecDir)
    forM_ libExecScripts $ \script -> do
        installScript script (destDir ++ libExecDir)
  where
    libExecScripts :: [FilePath]
    libExecScripts = [ ghcSplitPath ]


-- | Install executable binaries to @prefix/lib/bin@
-- ref: ghc.mk
installLibExecs :: Action ()
installLibExecs = do
    libExecDir <- getLibExecDir
    installDirectory (destDir ++ libExecDir)
    forM_ installBinPkgs $ \pkg -> do
        withLatestBuildStage pkg $ \stg -> do
            let context = programContext stg pkg
            let bin = inplaceLibBinPath -/- programName context <.> exe
            installProgram bin (destDir ++ libExecDir)
            when (pkg == ghc) $ do
                moveFile (destDir ++ libExecDir -/- programName context <.> exe)
                         (destDir ++ libExecDir -/- "ghc" <.> exe)

-- | Binaries to install
installBinPkgs :: [Package]
installBinPkgs =
    [ ghc, ghcPkg, ghcSplit, hp2ps
    , hpc, hsc2hs, runGhc, unlit ]

-- | Install executable wrapper scripts to @prefix/bin@
-- ref: ghc.mk
installBins :: Action ()
installBins = do
    binDir <- setting InstallBinDir
    installDirectory (destDir ++ binDir)
    forM_ installBinPkgs $ \pkg ->
        withLatestBuildStage pkg $ \stg -> do
            let context = programContext stg pkg
            version <- setting ProjectVersion
            -- binary's name
            let binName = if pkg == ghc
                          then "ghc-" ++ version <.> exe
                          else programName context ++ "-" ++ version <.> exe
            -- symbolic link's name
            let symName = if pkg == ghc
                          then "ghc" <.> exe
                          else programName context <.> exe
            case lookup context installWrappers of
                Nothing -> return ()
                Just wrapper -> do
                    libDir <- installGhcLibDir
                    contents <- interpretInContext context $
                                    wrapper
                                    (WrappedBinary (destDir ++ libDir) symName)
                    let wrapperPath = destDir ++ binDir -/- binName
                    writeFileChanged wrapperPath contents
                    makeExecutable wrapperPath
                    unlessM windowsHost $
                        linkSymbolic (destDir ++ binDir -/- binName)
                                     (destDir ++ binDir -/- symName)

withLatestBuildStage :: Package -> (Stage -> Action ()) -> Action ()
withLatestBuildStage pkg m = do
  stg' <- latestBuildStage pkg
  case stg' of
      Nothing -> return ()
      Just stg -> m stg

-- | Install @package.conf.install@ for each package
-- Note that each time it will be recreated
-- ref: rules/manual-package-conf.mk
installPackageConf :: Action ()
installPackageConf = do
    let context = vanillaContext Stage0 rts
    liftIO $ IO.createDirectoryIfMissing True (takeDirectory pkgConfInstallPath)
    build $ Target context HsCpp [ pkgPath rts -/- "package.conf.in" ]
                                 [ pkgConfInstallPath <.> "raw" ]
    Stdout content <- cmd "grep" [ "-v", "^#pragma GCC"
                                 , pkgConfInstallPath <.> "raw" ]
    withTempFile $ \tmp -> do
        liftIO $ writeFile tmp content
        Stdout content' <- cmd "sed" [ "-e", "s/\"\"//g", "-e", "s/:[   ]*,/: /g", tmp ]
        liftIO $ writeFile pkgConfInstallPath content'

-- | Install packages to @prefix/lib@
-- ref: ghc.mk
installPackages :: Action ()
installPackages = do
    need [ pkgConfInstallPath ]

    ghcLibDir <- installGhcLibDir
    binDir <- setting InstallBinDir

    -- Install package.conf
    let installedPackageConf = destDir ++ ghcLibDir -/- "package.conf.d"
    installDirectory (destDir ++ ghcLibDir)
    removeDirectory installedPackageConf
    installDirectory installedPackageConf

    -- Install RTS
    let rtsDir = destDir ++ ghcLibDir -/- "rts"
    installDirectory rtsDir
    ways <- interpretInContext (vanillaContext Stage1 rts) getRtsWays
    rtsLibs <- mapM pkgLibraryFile $ map (Context Stage1 rts) ways
    ffiLibs <- sequence $ map rtsLibffiLibrary ways

    -- TODO: Add dynamic ones
    forM_ (rtsLibs ++ ffiLibs) $ \lib -> installData [lib] rtsDir

    -- HACK (issue #327)
    let ghcBootPlatformHeader =
          buildPath (vanillaContext Stage1 compiler) -/-
          "ghc_boot_platform.h"

    copyFile ghcBootPlatformHeader (pkgPath compiler -/- "ghc_boot_platform.h")

    activePackages <- filterM ((isJust <$>) . latestBuildStage)
                              (knownPackages \\ [rts, libffi])

    installLibPkgs <- sortPkgsByDep (filter isLibrary activePackages)

    forM_ installLibPkgs $ \pkg@Package{..} -> do
        when (isLibrary pkg) $
            withLatestBuildStage pkg $ \stg -> do
                let context = vanillaContext stg pkg
                top <- interpretInContext context getTopDirectory
                let installDistDir = top -/- buildPath context
                buildPackage stg pkg
                docDir <- installDocDir
                ghclibDir <- installGhcLibDir

                -- Copy over packages

                strip <- stripCmdPath context
                ways <- interpretInContext context getLibraryWays
                let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal" -- HACK?
                need [ ghcCabalInplace ]

                let cabalFile = pkgCabalFile pkg
                -- HsColour sources
                -- QUESTION: what is the output of GhcCabalHsColour?
                whenM (isSpecified HsColour) $ do
                    top <- interpretInContext context getTopDirectory
                    let installDistDir = top -/- buildPath context
                    -- HACK: copy stuff back to the place favored by ghc-cabal
                    quietly $ copyDirectoryContents (Not excluded)
                                  installDistDir (installDistDir -/- "build")

                pkgConf <- pkgConfFile context
                need [ cabalFile, pkgConf ] -- TODO: check if need pkgConf
                build $ Target context GhcCabalHsColour [cabalFile] []

                -- HACK (#318): copy stuff back to the place favored by ghc-cabal
                quietly $ copyDirectoryContents (Not excluded)
                            installDistDir (installDistDir -/- "build")
                pref <- setting InstallPrefix
                unit $ cmd ghcCabalInplace
                           [ "copy"
                           , pkgPath
                           , installDistDir
                           , strip
                           , destDir
                           , pref
                           , ghclibDir
                           , docDir -/- "html/libraries"
                           , intercalate "  " (map show ways) ]

    -- Register packages
    let installedGhcPkgReal = destDir ++ binDir -/- "ghc-pkg" <.> exe
    let installedGhcReal = destDir ++ binDir -/- "ghc" <.> exe
    -- TODO: Extend GhcPkg builder args to support --global-package-db
    unit $ cmd installedGhcPkgReal
               [ "--force", "--global-package-db"
               , installedPackageConf, "update"
               , pkgConfInstallPath ]

    forM_ installLibPkgs $ \pkg@Package{..} -> do
        when (isLibrary pkg) $
            withLatestBuildStage pkg $ \stg -> do
                let context = vanillaContext stg pkg
                top <- interpretInContext context getTopDirectory
                let installDistDir = top -/- buildPath context
                -- TODO: better reference to the built inplace binary path
                let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal"
                pref <- setting InstallPrefix
                docDir <- installDocDir
                r <- relocatableBuild
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
    unit $ cmd installedGhcPkgReal
               [ "--force", "--global-package-db"
               , installedPackageConf, "recache" ]
  where
    createData f = unit $ cmd "chmod" [ "644", f ]
    excluded = Or
        [ Test "//haddock-prologue.txt"
        , Test "//package-data.mk"
        , Test "//setup-config"
        , Test "//inplace-pkg-config"
        , Test "//build" ]

-- | Install settings etc. files to @prefix/lib@
-- ref: ghc.mk
installCommonLibs :: Action ()
installCommonLibs = do
    ghcLibDir <- installGhcLibDir
    installLibsTo inplaceLibCopyTargets (destDir ++ ghcLibDir)

-- | Install library files to some path
-- ref: ghc.mk
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
               build $ Target context Ranlib [out] [out]
           _ -> installData [lib] dir

-- | All header files are in includes/{one of these subdirectories}
-- ref: includes/ghc.mk
includeHSubdirs :: [FilePath]
includeHSubdirs = [ ".", "rts", "rts/prof", "rts/storage", "stg" ]

-- | Install header files to @prefix/lib/ghc-<version>/include@
-- ref: includes/ghc.mk
installIncludes ::Action ()
installIncludes = do
    ghclibDir <- installGhcLibDir
    let ghcheaderDir = ghclibDir -/- "include"
    installDirectory (destDir ++ ghcheaderDir)
    forM_ includeHSubdirs $ \d -> do
        installDirectory (destDir ++ ghcheaderDir -/- d)
        headers <- getDirectoryFiles ("includes" -/- d) ["*.h"]
        installHeader (map (("includes" -/- d) -/-) headers)
                      (destDir ++ ghcheaderDir -/- d ++ "/")
    installHeader (includesDependencies ++
                   [generatedPath -/- "DerivedConstants.h"] ++
                   libffiDependencies)
                  (destDir ++ ghcheaderDir ++ "/")
  where
    installHeader = installData -- they share same arguments
