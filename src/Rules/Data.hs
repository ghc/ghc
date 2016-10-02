module Rules.Data (buildPackageData) where

import Base
import Context
import Expression
import GHC
import Oracles.Config.Setting
import Oracles.Dependencies
import Rules.Actions
import Rules.Generate
import Rules.Libffi
import Settings.Paths
import Target
import UserSettings

-- | Build @package-data.mk@ by using ghc-cabal utility to process .cabal files.
buildPackageData :: Context -> Rules ()
buildPackageData context@Context {..} = do
    let cabalFile = pkgCabalFile package
        configure = pkgPath package -/- "configure"
        dataFile  = pkgDataFile context
        oldPath   = pkgPath package -/- contextDirectory context -- TODO: remove, #113
        inTreeMk  = oldPath -/- takeFileName dataFile -- TODO: remove, #113

    inTreeMk %> \mk -> do
        -- Make sure all generated dependencies are in place before proceeding.
        orderOnly =<< interpretInContext context generatedDependencies

        -- GhcCabal may run the configure script, so we depend on it.
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- Before we configure a package its dependencies need to be registered.
        need =<< mapM pkgConfFile =<< contextDependencies context

        need [cabalFile]
        build $ Target context GhcCabal [cabalFile] [mk]

    -- TODO: Get rid of this, see #113.
    dataFile %> \mk -> do
        -- TODO: This is a hack. Add a proper support for autogen directory
        -- structure of the new Cabal (probably only after #113).
        let oldBuild
                | isLibrary package   = oldPath -/- "build"
                | package == ghc      = oldPath -/- "build/ghc"
                | package == hpcBin   = oldPath -/- "build/hpc"
                | package == iservBin = oldPath -/- "build/iserv"
                | otherwise           = oldPath -/- "build" -/- pkgNameString package
        copyFile inTreeMk mk
        autogenFiles <- getDirectoryFiles oldBuild ["autogen/*"]
        createDirectory $ buildPath context -/- "autogen"
        forM_ autogenFiles $ \file' -> do
            let file = unifyPath file'
            copyFile (oldBuild -/- file) (buildPath context -/- file)
        let haddockPrologue = "haddock-prologue.txt"
        copyFile (oldPath -/- haddockPrologue) (buildPath context -/- haddockPrologue)
        postProcessPackageData context mk

    -- TODO: PROGNAME was $(CrossCompilePrefix)hp2ps.
    priority 2.0 $ do
        when (package == hp2ps) $ dataFile %> \mk -> do
            orderOnly =<< interpretInContext context generatedDependencies
            let prefix = fixKey (buildPath context) ++ "_"
                cSrcs  = [ "AreaBelow.c", "Curves.c", "Error.c", "Main.c"
                         , "Reorder.c", "TopTwenty.c", "AuxFile.c"
                         , "Deviation.c", "HpFile.c", "Marks.c", "Scale.c"
                         , "TraceElement.c", "Axes.c", "Dimensions.c", "Key.c"
                         , "PsFile.c", "Shade.c", "Utilities.c" ]
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = hp2ps"
                    , "C_SRCS = " ++ unwords cSrcs
                    , "DEP_EXTRA_LIBS = m"
                    , "CC_OPTS = -I" ++ generatedPath ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated " ++ mk

        when (package == unlit) $ dataFile %> \mk -> do
            orderOnly =<< interpretInContext context generatedDependencies
            let prefix   = fixKey (buildPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = unlit"
                    , "C_SRCS = unlit.c"
                    , "SYNOPSIS = Literate script filter." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated " ++ mk

        when (package == touchy) $ dataFile %> \mk -> do
            orderOnly =<< interpretInContext context generatedDependencies
            let prefix   = fixKey (buildPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = touchy"
                    , "C_SRCS = touchy.c" ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated " ++ mk

        -- Bootstrapping `ghcCabal`: although `ghcCabal` is a proper cabal
        -- package, we cannot generate the corresponding `package-data.mk` file
        -- by running by running `ghcCabal`, because it has not yet been built.
        when (package == ghcCabal && stage == Stage0) $ dataFile %> \mk -> do
            orderOnly =<< interpretInContext context generatedDependencies
            let prefix   = fixKey (buildPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = ghc-cabal"
                    , "MODULES = Main"
                    , "SYNOPSIS = Bootstrapped ghc-cabal utility."
                    , "HS_SRC_DIRS = ." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated " ++ mk

        when (package == rts && stage == Stage1) $ do
            dataFile %> \mk -> do
                orderOnly =<< interpretInContext context generatedDependencies
                windows <- windowsHost
                let prefix = fixKey (buildPath context) ++ "_"
                    dirs   = [ ".", "hooks", "sm", "eventlog", "linker" ]
                          ++ [ if windows then "win32" else "posix" ]
                -- TODO: Adding cmm/S sources to C_SRCS is a hack -- refactor.
                cSrcs   <- map unifyPath <$>
                           getDirectoryFiles (pkgPath package) (map (-/- "*.c") dirs)
                cmmSrcs <- getDirectoryFiles (pkgPath package) ["*.cmm"]
                buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
                buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
                let extraSrcs = [ "AdjustorAsm.S" | buildAdjustor   ]
                             ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
                             ++ [ rtsBuildPath -/- "AutoApply.cmm"  ]
                             ++ [ rtsBuildPath -/- "sm/Evac_thr.c"  ]
                             ++ [ rtsBuildPath -/- "sm/Scav_thr.c"  ]
                let contents = unlines $ map (prefix++)
                        [ "C_SRCS = "  ++ unwords (cSrcs ++ cmmSrcs ++ extraSrcs)
                        , "CC_OPTS = -I" ++ generatedPath
                        , "COMPONENT_ID = rts" ]
                writeFileChanged mk contents
                putSuccess $ "| Successfully generated " ++ mk

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- For example, get rid of
-- libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...
-- Reason: we don't need them and we can't parse them.
-- 2) Replace '/' and '\' with '_' before '='
-- For example libraries/deepseq/dist-install_VERSION = 1.4.0.0
-- is replaced by libraries_deepseq_dist-install_VERSION = 1.4.0.0
-- Reason: Shake's built-in makefile parser doesn't recognise slashes
postProcessPackageData :: Context -> FilePath -> Action ()
postProcessPackageData context@Context {..} file = fixFile file fixPackageData
  where
    fixPackageData = unlines . map processLine . filter (not . null) . filter ('$' `notElem`) . lines
    processLine line = fixKey fixedPrefix ++ suffix
      where
        (prefix, suffix) = break (== '=') line
        -- Change package/path/targetDir to takeDirectory file
        -- This is a temporary hack until we get rid of ghc-cabal
        fixedPrefix = takeDirectory file ++ drop len prefix
        len         = length (pkgPath package -/- contextDirectory context)

-- TODO: Remove, see #113.
fixKey :: String -> String
fixKey = replaceSeparators '_'
