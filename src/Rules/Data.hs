module Rules.Data (buildPackageData) where

import Base
import Util
import Target (PartialTarget (..), fullTarget)
import Package
import Builder
import Expression
import Predicates (registerPackage)
import Oracles.PackageDeps
import Settings.Packages
import Settings.TargetDirectory
import Rules.Actions
import Rules.Resources

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Resources -> PartialTarget -> Rules ()
buildPackageData rs target @ (PartialTarget stage pkg) = do
    let path      = targetPath stage pkg
        cabalFile = pkgCabalFile pkg
        configure = pkgPath pkg -/- "configure"

    fmap (path -/-)
        [ "package-data.mk"
        , "haddock-prologue.txt"
        , "inplace-pkg-config"
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
            whenM (interpretPartial target registerPackage) .
                buildWithResources [(ghcPkg rs, 1)] $
                fullTarget target (GhcPkg stage) [cabalFile] outs

            postProcessPackageData $ path -/- "package-data.mk"

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
    pkgData <- fmap (filter ('$' `notElem`) . lines) . liftIO $ readFile file
    length pkgData `seq` writeFileLines file $ map processLine pkgData
      where
        processLine line = replaceSeparators '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line
