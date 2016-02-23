module Rules.Data (buildPackageData) where

import qualified System.Directory as IO

import Base
import Context
import Expression
import GHC
import Oracles.Config.Setting
import Oracles.PackageDeps
import Rules.Actions
import Rules.Generate
import Rules.Libffi
import Settings
import Settings.Builders.Common
import Target

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Context -> Rules ()
buildPackageData context @ Context {..} = do
    let cabalFile = pkgCabalFile package
        configure = pkgPath package -/- "configure"
        dataFile  = pkgDataFile context
        oldPath   = pkgPath package -/- contextDirectory context -- TODO: remove, #113

    [dataFile, oldPath -/- "package-data.mk"] &%> \_ -> do
        -- The first thing we do with any package is make sure all generated
        -- dependencies are in place before proceeding.
        orderOnly $ generatedDependencies stage package

        -- GhcCabal may run the configure script, so we depend on it
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- Before we configure a package its dependencies need to be registered
        deps <- packageDeps package
        pkgs <- interpretInContext context getPackages
        let depPkgs = matchPackageNames (sort pkgs) deps
        need =<< traverse (pkgConfFile . vanillaContext stage) depPkgs

        -- TODO: get rid of this, see #113
        let inTreeMk = oldPath -/- takeFileName dataFile

        need [cabalFile]
        build $ Target context GhcCabal [cabalFile] [inTreeMk]

        -- TODO: get rid of this, see #113
        liftIO $ IO.copyFile inTreeMk dataFile
        autogenFiles <- getDirectoryFiles oldPath ["build/autogen/*"]
        createDirectory $ contextPath context -/- "build/autogen"
        forM_ autogenFiles $ \file -> do
            copyFile (oldPath -/- file) (contextPath context -/- file)
        let haddockPrologue = "haddock-prologue.txt"
        copyFile (oldPath -/- haddockPrologue) (contextPath context -/- haddockPrologue)

        postProcessPackageData context dataFile

    -- TODO: PROGNAME was $(CrossCompilePrefix)hp2ps
    priority 2.0 $ do
        when (package == hp2ps) $ dataFile %> \mk -> do
            includes <- interpretInContext context $ fromDiffExpr includesArgs
            let prefix = fixKey (contextPath context) ++ "_"
                cSrcs  = [ "AreaBelow.c", "Curves.c", "Error.c", "Main.c"
                         , "Reorder.c", "TopTwenty.c", "AuxFile.c"
                         , "Deviation.c", "HpFile.c", "Marks.c", "Scale.c"
                         , "TraceElement.c", "Axes.c", "Dimensions.c", "Key.c"
                         , "PsFile.c", "Shade.c", "Utilities.c" ]
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = hp2ps"
                    , "C_SRCS = " ++ unwords cSrcs
                    , "DEP_EXTRA_LIBS = m"
                    , "CC_OPTS = " ++ unwords includes ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (package == unlit) $ dataFile %> \mk -> do
            let prefix   = fixKey (contextPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = unlit"
                    , "C_SRCS = unlit.c"
                    , "SYNOPSIS = Literate script filter." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (package == touchy) $ dataFile %> \mk -> do
            let prefix   = fixKey (contextPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = touchy"
                    , "C_SRCS = touchy.c" ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        -- Bootstrapping `ghcCabal`: although `ghcCabal` is a proper cabal
        -- package, we cannot generate the corresponding `package-data.mk` file
        -- by running by running `ghcCabal`, because it has not yet been built.
        when (package == ghcCabal && stage == Stage0) $ dataFile %> \mk -> do
            let prefix   = fixKey (contextPath context) ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = ghc-cabal"
                    , "MODULES = Main"
                    , "SYNOPSIS = Bootstrapped ghc-cabal utility."
                    , "HS_SRC_DIRS = ." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (package == rts && stage == Stage1) $ do
            dataFile %> \mk -> do
                orderOnly $ generatedDependencies stage package
                windows <- windowsHost
                let prefix = fixKey (contextPath context) ++ "_"
                    dirs   = [ ".", "hooks", "sm", "eventlog" ]
                          ++ [ "posix" | not windows ]
                          ++ [ "win32" |     windows ]
                -- TODO: rts/dist/build/sm/Evac_thr.c, rts/dist/build/sm/Scav_thr.c
                -- TODO: adding cmm/S sources to C_SRCS is a hack; rethink after #18
                cSrcs    <- getDirectoryFiles (pkgPath package) (map (-/- "*.c") dirs)
                cmmSrcs  <- getDirectoryFiles (pkgPath package) ["*.cmm"]
                buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
                buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
                let sSrcs = [ "AdjustorAsm.S" | buildAdjustor   ]
                         ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
                    extraSrcs = [ rtsBuildPath -/- "AutoApply.cmm" ]
                includes <- interpretInContext context $ fromDiffExpr includesArgs
                let contents = unlines $ map (prefix++)
                        [ "C_SRCS = "
                          ++ unwords (cSrcs ++ cmmSrcs ++ sSrcs ++ extraSrcs)
                        , "CC_OPTS = "  ++ unwords includes
                        , "COMPONENT_ID = rts" ]
                writeFileChanged mk contents
                putSuccess $ "| Successfully generated '" ++ mk ++ "'."

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
postProcessPackageData context @ Context {..} file = fixFile file fixPackageData
  where
    fixPackageData = unlines . map processLine . filter (not . null) . filter ('$' `notElem`) . lines
    processLine line = fixKey fixedPrefix ++ suffix
      where
        (prefix, suffix) = break (== '=') line
        -- Change package/path/targetDir to takeDirectory file
        -- This is a temporary hack until we get rid of ghc-cabal
        fixedPrefix = takeDirectory file ++ drop len prefix
        len         = length (pkgPath package -/- contextDirectory context)

-- TODO: remove, see #113
fixKey :: String -> String
fixKey = replaceSeparators '_'
