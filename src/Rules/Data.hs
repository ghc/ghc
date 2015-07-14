{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Rules.Data (
    cabalArgs, ghcPkgArgs, buildPackageData
    ) where

import Base
import Package
import Expression hiding (when, liftIO)
import Oracles.Flag (when)
import Oracles.Builder
import Settings.GhcPkg
import Settings.GhcCabal
import Settings.TargetDirectory
import Rules.Util
import Util
import Ways

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Target -> Rules ()
buildPackageData target =
    let stage = getStage target
        pkg   = getPackage target
        path  = targetPath stage pkg
    in
    (path </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        let configure = pkgPath pkg </> "configure"
            -- TODO: 1) how to automate this? 2) handle multiple files?
            newTarget = target { getFile = path </> "package-data.mk"
                               , getWay = vanilla } -- TODO: think
        -- GhcCabal will run the configure script, so we depend on it
        need [pkgPath pkg </> pkgCabal pkg]
        -- We still don't know who built the configure script from configure.ac
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        build $ newTarget { getBuilder = GhcCabal }
        -- TODO: when (registerPackage settings) $
        build $ newTarget { getBuilder = GhcPkg stage }
        postProcessPackageData $ path </> "package-data.mk"

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
