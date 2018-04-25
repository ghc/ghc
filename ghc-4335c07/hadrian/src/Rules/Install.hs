module Rules.Install (installRules) where

import Hadrian.Oracles.DirectoryContents
import qualified System.Directory as IO

import Base
import Expression
import Oracles.Setting
import Rules
import Rules.Generate
import Rules.Libffi
import Rules.Wrappers
import Settings
import Settings.Packages.Rts
import Target
import Utilities

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
installRules =
    "install" ~> do
        installIncludes
        installPackageConf
        installCommonLibs
        installLibExecs
        installLibExecScripts
        installBins
        installPackages
        installDocs

-- TODO: Get rid of hard-coded list.
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
    destDir <- getDestDir
    installDirectory (destDir ++ libExecDir)
    forM_ libExecScripts $ \script -> installScript script (destDir ++ libExecDir)
  where
    libExecScripts :: [FilePath]
    libExecScripts = [ghcSplitPath]

-- ref: ghc.mk
-- | Install executable binaries to @prefix/lib/bin@.
installLibExecs :: Action ()
installLibExecs = do
    libExecDir <- getLibExecDir
    destDir <- getDestDir
    installDirectory (destDir ++ libExecDir)
    forM_ installBinPkgs $ \pkg ->
        withInstallStage pkg $ \stage -> do
            context <- programContext stage pkg
            let bin = inplaceLibBinPath -/- programName context <.> exe
            installProgram bin (destDir ++ libExecDir)
            when (pkg == ghc) $
                moveFile (destDir ++ libExecDir -/- programName context <.> exe)
                         (destDir ++ libExecDir -/- "ghc" <.> exe)

-- ref: ghc.mk
-- | Install executable wrapper scripts to @prefix/bin@.
installBins :: Action ()
installBins = do
    binDir <- setting InstallBinDir
    libDir <- installGhcLibDir
    destDir <- getDestDir
    installDirectory (destDir ++ binDir)
    win <- windowsHost
    when win $
        copyDirectoryContents matchAll (destDir ++ libDir -/- "bin") (destDir ++ binDir)
    unless win $ forM_ installBinPkgs $ \pkg ->
        withInstallStage pkg $ \stage -> do
            context <- programContext stage pkg
            version <- setting ProjectVersion
            -- Name of the binary file
            let binName | pkg == ghc = "ghc-" ++ version <.> exe
                        | otherwise  = programName context ++ "-" ++ version <.> exe
            -- Name of the symbolic link
            let symName | pkg == ghc = "ghc" <.> exe
                        | otherwise  = programName context <.> exe
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

-- | Perform an action depending on the install stage or do nothing if the
-- package is not installed.
withInstallStage :: Package -> (Stage -> Action ()) -> Action ()
withInstallStage pkg m = do
    maybeStage <- installStage pkg
    case maybeStage of { Just stage -> m stage; Nothing -> return () }

pkgConfInstallPath :: Action FilePath
pkgConfInstallPath = buildPath (vanillaContext Stage0 rts) <&> (-/- "package.conf.install")

-- ref: rules/manual-package-conf.mk
-- TODO: Should we use a temporary file instead of pkgConfInstallPath?
-- | Install @package.conf.install@ for each package. Note that it will be
-- recreated each time.
installPackageConf :: Action ()
installPackageConf = do
    let context = vanillaContext Stage0 rts
    confPath <- pkgConfInstallPath
    liftIO $ IO.createDirectoryIfMissing True (takeDirectory confPath)
    build $ target context HsCpp [ pkgPath rts -/- "package.conf.in" ]
                                 [ confPath <.> "raw" ]
    Stdout content <- cmd "grep" [ "-v", "^#pragma GCC"
                                 , confPath <.> "raw" ]
    withTempFile $ \tmp -> do
        liftIO $ writeFile tmp content
        Stdout result <- cmd "sed" [ "-e", "s/\"\"//g", "-e", "s/:[   ]*,/: /g", tmp ]
        liftIO $ writeFile confPath result

-- ref: ghc.mk
-- | Install packages to @prefix/lib@.
installPackages :: Action ()
installPackages = do
    confPath <- pkgConfInstallPath
    need [confPath]

    ghcLibDir <- installGhcLibDir
    binDir    <- setting InstallBinDir
    destDir   <- getDestDir

    -- Install package.conf
    let installedPackageConf = destDir ++ ghcLibDir -/- "package.conf.d"
    installDirectory (destDir ++ ghcLibDir)
    removeDirectory installedPackageConf
    installDirectory installedPackageConf

    -- Install RTS
    let rtsDir = destDir ++ ghcLibDir -/- "rts"
    installDirectory rtsDir
    ways    <- interpretInContext (vanillaContext Stage1 rts) getRtsWays
    rtsLibs <- mapM (pkgLibraryFile . Context Stage1 rts) ways
    ffiLibs <- mapM rtsLibffiLibrary ways

    -- TODO: Add dynamic libraries.
    forM_ (rtsLibs ++ ffiLibs) $ \lib -> installData [lib] rtsDir

    -- TODO: Remove this hack required for @ghc-cabal copy@.
    -- See https://github.com/snowleopard/hadrian/issues/327.
    ghcBootPlatformHeader <-
        buildPath (vanillaContext Stage1 compiler) <&> (-/- "ghc_boot_platform.h")
    copyFile ghcBootPlatformHeader (pkgPath compiler -/- "ghc_boot_platform.h")

    installPackages <- filterM ((isJust <$>) . installStage)
                               (knownPackages \\ [rts, libffi])

    installLibPkgs <- topsortPackages (filter isLibrary installPackages)

    -- TODO: Figure out what is the root cause of the missing ghc-gmp.h error.
    copyFile (pkgPath integerGmp -/- "gmp/ghc-gmp.h") (pkgPath integerGmp -/- "ghc-gmp.h")

    forM_ installLibPkgs $ \pkg ->
        case pkgCabalFile pkg of
            Nothing -> error $ "Non-Haskell project in installLibPkgs" ++ show pkg
            Just cabalFile -> withInstallStage pkg $ \stage -> do
                let context = vanillaContext stage pkg
                top <- topDirectory
                installDistDir <- buildPath context
                let absInstallDistDir = top -/- installDistDir

                need =<< packageTargets True stage pkg
                docDir <- installDocDir
                ghclibDir <- installGhcLibDir

                -- Copy over packages
                strip <- stripCmdPath
                ways  <- interpretInContext context getLibraryWays
                -- TODO: Remove hard-coded @ghc-cabal@ path.
                let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal" <.> exe
                need [ghcCabalInplace]

                pkgConf <- pkgConfFile context
                need [cabalFile, pkgConf] -- TODO: Check if we need 'pkgConf'.

                -- TODO: Drop redundant copies required by @ghc-cabal@.
                -- See https://github.com/snowleopard/hadrian/issues/318.
                quietly $ copyDirectoryContentsUntracked (Not excluded)
                    installDistDir (installDistDir -/- "build")

                pref <- setting InstallPrefix
                unit $ cmd ghcCabalInplace [ "copy"
                                           , pkgPath pkg
                                           , absInstallDistDir
                                           , strip
                                           , destDir
                                           , pref
                                           , ghclibDir
                                           , docDir -/- "html/libraries"
                                           , unwords (map show ways) ]

    -- Register packages
    let installedGhcPkgReal = destDir ++ binDir -/- "ghc-pkg" <.> exe
        installedGhcReal    = destDir ++ binDir -/- "ghc"     <.> exe
    -- TODO: Extend GhcPkg builder args to support --global-package-db
    unit $ cmd installedGhcPkgReal [ "--force", "--global-package-db"
                                   , installedPackageConf, "update"
                                   , confPath ]

    forM_ installLibPkgs $ \pkg ->
        withInstallStage pkg $ \stage -> do
            let context = vanillaContext stage pkg
            top <- topDirectory
            installDistDir <- (top -/-) <$> buildPath context
            -- TODO: better reference to the built inplace binary path
            let ghcCabalInplace = inplaceBinPath -/- "ghc-cabal"
            pref   <- setting InstallPrefix
            docDir <- installDocDir
            r      <- relocatableBuild
            unit $ cmd ghcCabalInplace
                [ "register"
                , pkgPath pkg
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
    destDir   <- getDestDir
    installLibsTo inplaceLibCopyTargets (destDir ++ ghcLibDir)

-- ref: ghc.mk
-- | Install library files to some path.
installLibsTo :: [FilePath] -> FilePath -> Action ()
installLibsTo libs dir = do
    installDirectory dir
    forM_ libs $ \lib -> case takeExtension lib of
        ".a" -> do
            let out = dir -/- takeFileName lib
            installData [out] dir
            runBuilder Ranlib [out] [out] [out]
        _ -> installData [lib] dir

-- ref: includes/ghc.mk
-- | All header files are in includes/{one of these subdirectories}.
includeHSubdirs :: [FilePath]
includeHSubdirs = [".", "rts", "rts/prof", "rts/storage", "stg"]

-- ref: includes/ghc.mk
-- | Install header files to @prefix/lib/ghc-<version>/include@.
installIncludes :: Action ()
installIncludes = do
    ghclibDir <- installGhcLibDir
    destDir   <- getDestDir
    let ghcheaderDir = ghclibDir -/- "include"
    installDirectory (destDir ++ ghcheaderDir)
    forM_ includeHSubdirs $ \dir -> do
        installDirectory (destDir ++ ghcheaderDir -/- dir)
        headers <- getDirectoryFiles ("includes" -/- dir) ["*.h"]
        installHeader (map (("includes" -/- dir) -/-) headers)
                      (destDir ++ ghcheaderDir -/- dir ++ "/")
    root    <- buildRoot
    rtsPath <- rtsBuildPath
    installHeader (fmap (root -/-) includesDependencies ++
                   [root -/- generatedDir -/- "DerivedConstants.h"] ++
                   fmap (rtsPath -/-) libffiDependencies)
                  (destDir ++ ghcheaderDir ++ "/")
  where
    installHeader = installData -- they share same arguments

-- ref: ghc.mk
-- | Install documentation to @prefix/share/doc/ghc-<version>@.
installDocs :: Action ()
installDocs = do
    destDir <- getDestDir
    docDir  <- installDocDir
    root    <- buildRoot
    installDirectory (destDir ++ docDir)

    let usersGuide = root -/- "docs/pdfs/users_guide.pdf"
    whenM (doesFileExist usersGuide) $
        installData [usersGuide] (destDir ++ docDir)

    let htmlDocDir = destDir ++ docDir -/- "html"
    installDirectory htmlDocDir
    installData ["docs/index.html"] htmlDocDir

    forM_ ["Haddock", "libraries", "users_guide"] $ \dirname -> do
        let dir = root -/- "docs/html" -/- dirname
        whenM (doesDirectoryExist dir) $ copyDirectory dir htmlDocDir
