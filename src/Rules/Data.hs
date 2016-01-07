module Rules.Data (buildPackageData) where

import Base
import Expression
import GHC
import Oracles
import Predicates (registerPackage)
import Rules.Actions
import Rules.Generate
import Rules.Resources
import Settings
import Settings.Builders.Common
import Settings.Packages.Rts

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Resources -> PartialTarget -> Rules ()
buildPackageData rs target @ (PartialTarget stage pkg) = do
    let cabalFile = pkgCabalFile pkg
        configure = pkgPath pkg -/- "configure"
        dataFile  = pkgDataFile stage pkg

    dataFile %> \mk -> do
        -- The first thing we do with any package is make sure all generated
        -- dependencies are in place before proceeding.
        orderOnly $ generatedDependencies stage pkg

        -- GhcCabal may run the configure script, so we depend on it
        -- We don't know who built the configure script from configure.ac
        whenM (doesFileExist $ configure <.> "ac") $ need [configure]

        -- We configure packages in the order of their dependencies
        deps <- packageDeps pkg
        pkgs <- interpretPartial target getPackages
        let depPkgs = matchPackageNames (sort pkgs) deps
        orderOnly $ map (pkgDataFile stage) depPkgs

        need [cabalFile]
        buildWithResources [(resGhcCabal rs, 1)] $
            fullTarget target GhcCabal [cabalFile] [mk]

        -- ghc-pkg produces inplace-pkg-config when run on packages with
        -- library components only
        when (isLibrary pkg) .
            whenM (interpretPartial target registerPackage) .
                buildWithResources [(resGhcPkg rs, 1)] $
                    fullTarget target (GhcPkg stage) [cabalFile] []

        postProcessPackageData dataFile

    -- TODO: PROGNAME was $(CrossCompilePrefix)hp2ps
    priority 2.0 $ do
        when (pkg == hp2ps) $ dataFile %> \mk -> do
            includes <- interpretPartial target $ fromDiffExpr includesArgs
            let prefix = "utils_hp2ps_" ++ stageString stage ++ "_"
                cSrcs  = [ "AreaBelow.c", "Curves.c", "Error.c", "Main.c"
                         , "Reorder.c", "TopTwenty.c", "AuxFile.c"
                         , "Deviation.c", "HpFile.c", "Marks.c", "Scale.c"
                         , "TraceElement.c", "Axes.c", "Dimensions.c", "Key.c"
                         , "PsFile.c", "Shade.c", "Utilities.c" ]
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = hp2ps"
                    , "C_SRCS = " ++ unwords cSrcs
                    , "INSTALL = YES"
                    , "INSTALL_INPLACE = YES"
                    , "DEP_EXTRA_LIBS = m"
                    , "CC_OPTS = " ++ unwords includes ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (pkg == touchy) $ dataFile %> \mk -> do
            let prefix = "utils_touchy_" ++ stageString stage ++ "_"
                contents = unlines $ map (prefix++)
                    [ "PROGNAME = touchy"
                    , "C_SRCS = touchy.c" ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        -- Bootstrapping `ghcCabal`: although `ghcCabal` is a proper cabal
        -- package, we cannot generate the corresponding `package-data.mk` file
        -- by running by running `ghcCabal`, because it has not yet been built.
        when (pkg == ghcCabal && stage == Stage0) $ dataFile %> \mk -> do
            let contents = unlines
                    [ "utils_ghc-cabal_stage0_PROGNAME = ghc-cabal"
                    , "utils_ghc-cabal_stage0_MODULES = Main"
                    , "utils_ghc-cabal_stage0_SYNOPSIS = Bootstrapped ghc-cabal utility."
                    , "utils_ghc-cabal_stage0_HS_SRC_DIRS = ." ]
            writeFileChanged mk contents
            putSuccess $ "| Successfully generated '" ++ mk ++ "'."

        when (pkg == rts && stage == Stage1) $ do
            dataFile %> \mk -> do
                windows <- windowsHost
                let prefix = "rts_" ++ stageString stage ++ "_"
                    dirs   = [ ".", "hooks", "sm", "eventlog" ]
                          ++ [ "posix" | not windows ]
                          ++ [ "win32" |     windows ]
                -- TODO: rts/dist/build/sm/Evac_thr.c, rts/dist/build/sm/Scav_thr.c
                -- TODO: adding cmm/S sources to C_SRCS is a hack; rethink after #18
                cSrcs    <- getDirectoryFiles (pkgPath pkg) (map (-/- "*.c") dirs)
                cmmSrcs  <- getDirectoryFiles (pkgPath pkg) ["*.cmm"]
                buildAdjustor   <- anyTargetArch ["i386", "powerpc", "powerpc64"]
                buildStgCRunAsm <- anyTargetArch ["powerpc64le"]
                let sSrcs = [ "AdjustorAsm.S" | buildAdjustor   ]
                         ++ [ "StgCRunAsm.S"  | buildStgCRunAsm ]
                    extraSrcs = [ targetDirectory Stage1 rts -/- "build/AutoApply.cmm" ]
                includes <- interpretPartial target $ fromDiffExpr includesArgs
                let contents = unlines $ map (prefix++)
                        [ "C_SRCS = "
                          ++ unwords (cSrcs ++ cmmSrcs ++ sSrcs ++ extraSrcs)
                        , "CC_OPTS = "  ++ unwords includes
                        , "COMPONENT_ID = " ++ "rts" ]
                writeFileChanged mk contents
                putSuccess $ "| Successfully generated '" ++ mk ++ "'."

                need [rtsConf]
                buildWithResources [(resGhcPkg rs, 1)] $
                    fullTarget target (GhcPkg stage) [rtsConf] []

            rtsConf %> \_ -> do
                orderOnly $ generatedDependencies stage pkg
                need [ rtsConfIn ]
                build $ fullTarget target HsCpp [rtsConfIn] [rtsConf]

                let fixRtsConf = unlines
                               . map
                               ( replace "\"\"" ""
                               . replace "rts/dist/build" "rts/stage1/build" )
                               . filter (not . null)
                               . lines

                fixFile rtsConf fixRtsConf

-- Prepare a given 'packaga-data.mk' file for parsing by readConfigFile:
-- 1) Drop lines containing '$'
-- For example, get rid of
-- libraries/Win32_dist-install_CMM_SRCS  := $(addprefix cbits/,$(notdir ...
-- Reason: we don't need them and we can't parse them.
-- 2) Replace '/' and '\' with '_' before '='
-- For example libraries/deepseq/dist-install_VERSION = 1.4.0.0
-- is replaced by libraries_deepseq_dist-install_VERSION = 1.4.0.0
-- Reason: Shake's built-in makefile parser doesn't recognise slashes
postProcessPackageData :: FilePath -> Action ()
postProcessPackageData file = fixFile file fixPackageData
  where
    fixPackageData = unlines . map processLine . filter ('$' `notElem`) . lines
    processLine line = replaceSeparators '_' prefix ++ suffix
      where
        (prefix, suffix) = break (== '=') line
