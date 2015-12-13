module Rules.Data (buildPackageData) where

import Expression
import GHC (deriveConstants, genapply, genprimopcode)
import Oracles
import Predicates (registerPackage)
import Rules.Actions
import Rules.Resources
import Settings

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Resources -> PartialTarget -> Rules ()
buildPackageData rs target @ (PartialTarget stage pkg) = do
    let path      = targetPath stage pkg
        cabalFile = pkgCabalFile pkg
        configure = pkgPath pkg -/- "configure"

    fmap (path -/-)
        [ "package-data.mk"
        , "haddock-prologue.txt"
        , "setup-config"
        , "build" -/- "autogen" -/- "cabal_macros.h"
        -- TODO: Is this needed? Also check out Paths_cpsa.hs.
        -- , "build" -/- "autogen" -/- ("Paths_" ++ name) <.> "hs"
        ] &%> \outs -> do
            -- GhcCabal may run the configure script, so we depend on it
            -- We don't know who built the configure script from configure.ac
            whenM (doesFileExist $ configure <.> "ac") $ need [configure]

            -- We configure packages in the order of their dependencies
            deps <- packageDeps pkg
            pkgs <- interpretPartial target getPackages
            let cmp p name = compare (pkgName p) name
                depPkgs    = intersectOrd cmp (sort pkgs) deps
            need [ targetPath stage p -/- "package-data.mk" | p <- depPkgs ]

            need [cabalFile]
            buildWithResources [(ghcCabal rs, 1)] $
                fullTarget target GhcCabal [cabalFile] outs

            -- TODO: find out of ghc-cabal can be concurrent with ghc-pkg
            when (isLibrary pkg) .
                whenM (interpretPartial target registerPackage) .
                buildWithResources [(ghcPkg rs, 1)] $
                fullTarget target (GhcPkg stage) [cabalFile] outs

            postProcessPackageData $ path -/- "package-data.mk"

    -- TODO: Track dependency on this generated file
    -- TODO: Use a cabal file instead of manual hacks?
    priority 2.0 $
        when (pkg == deriveConstants) $ path -/- "package-data.mk" %> \mk -> do
        let contents = unlines
                [ "utils_deriveConstants_dist-boot_MODULES = DeriveConstants"
                , "utils_deriveConstants_dist-boot_PROGNAME = deriveConstants"
                , "utils_deriveConstants_dist-boot_HS_SRC_DIRS = ."
                , "utils_deriveConstants_dist-boot_INSTALL_INPLACE = YES"
                , "utils_deriveConstants_dist-boot_HC_OPTS = -package process -package containers" ]
        writeFileChanged mk contents

    priority 2.0 $
        when (pkg == genapply) $ path -/- "package-data.mk" %> \mk -> do
        ghcUnreg <- flag GhcUnregisterised
        let hcOpts = "-package pretty" ++ if ghcUnreg then " -DNO_REGS" else ""
            contents = unlines
                [ "utils_genapply_dist-boot_MODULES = GenApply"
                , "utils_genapply_dist-boot_PROGNAME = genapply"
                , "utils_genapply_dist-boot_HS_SRC_DIRS = ."
                , "utils_genapply_dist-boot_INSTALL_INPLACE = YES"
                , "utils_genapply_dist-boot_HC_OPTS = " ++ hcOpts ]
        writeFileChanged mk contents

    priority 2.0 $
        when (pkg == genprimopcode) $ path -/- "package-data.mk" %> \mk -> do
        let contents = unlines
                [ "utils_genprimopcode_dist-boot_MODULES = Lexer Main ParserM Parser Syntax"
                , "utils_genprimopcode_dist-boot_PROGNAME = genprimopcode"
                , "utils_genprimopcode_dist-boot_HS_SRC_DIRS = ."
                , "utils_genprimopcode_dist-boot_INSTALL_INPLACE = YES"
                , "utils_genprimopcode_dist-boot_HC_OPTS = -package array" ]
        writeFileChanged mk contents

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
postProcessPackageData file = do
    contents <- fmap (filter ('$' `notElem`) . lines) . liftIO $ readFile file
    length contents `seq` writeFileLines file $ map processLine contents
      where
        processLine line = replaceSeparators '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line
