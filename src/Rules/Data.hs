module Rules.Data (buildPackageData) where

import Base
import Util
import Package
import Builder
import Switches
import Expression
import qualified Target
import Settings.TargetDirectory
import Rules.Actions
import Control.Applicative
import Control.Monad.Extra

-- TODO: Add ordering between packages? (see ghc.mk)
-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: StagePackageTarget -> Rules ()
buildPackageData target = do
    let stage     = Target.stage target
        pkg       = Target.package target
        path      = targetPath stage pkg
        cabal     = pkgPath pkg -/- pkgCabal pkg
        configure = pkgPath pkg -/- "configure"

    -- We do not allow parallel invokations of ghc-pkg (they don't work)
    ghcPkg <- newResource "ghc-pkg" 1

    (path -/-) <$>
        [ "package-data.mk"
        , "haddock-prologue.txt"
        , "inplace-pkg-config"
        , "setup-config"
        , "build" -/- "autogen" -/- "cabal_macros.h"
        -- TODO: Is this needed? Also check out Paths_cpsa.hs.
        -- , "build" -/- "autogen" -/- ("Paths_" ++ name) <.> "hs"
        ] &%> \files -> do
            -- GhcCabal may run the configure script, so we depend on it
            -- We don't know who built the configure script from configure.ac
            whenM (doesFileExist $ configure <.> "ac") $ need [configure]
            buildWithResources [(ghcPkg, 1)] $ -- GhcCabal calls ghc-pkg too
                fullTarget target [cabal] GhcCabal files
            whenM (interpretExpr target registerPackage) .
                buildWithResources [(ghcPkg, 1)] $
                fullTarget target [cabal] (GhcPkg stage) files
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
    pkgData <- (filter ('$' `notElem`) . lines) <$> liftIO (readFile file)
    length pkgData `seq` writeFileLines file $ map processLine pkgData
      where
        processLine line = replaceSeparators '_' prefix ++ suffix
          where
            (prefix, suffix) = break (== '=') line
