module Rules.Data (
    cabalSettings, ghcPkgSettings, buildPackageData
    ) where

import Base hiding (arg, args, Args)
import Package
import Expression hiding (when, liftIO)
import Oracles.Flag (when)
import Oracles.Builder
import Targets
import Settings
import Util

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

-- this is a positional argument, hence:
-- * if it is empty, we need to emit one empty string argument
-- * otherwise, we must collapse it into one space-separated string

-- Build package-data.mk by using GhcCabal to process pkgCabal file
buildPackageData :: Environment -> Rules ()
buildPackageData env =
    let stage = getStage env
        pkg   = getPackage env
        dir   = pkgPath pkg </> targetDirectory stage pkg
    in
    (dir </>) <$>
    [ "package-data.mk"
    , "haddock-prologue.txt"
    , "inplace-pkg-config"
    , "setup-config"
    , "build" </> "autogen" </> "cabal_macros.h"
    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
    ] &%> \_ -> do
        let configure = pkgPath pkg </> "configure"
        -- GhcCabal will run the configure script, so we depend on it
        need [pkgPath pkg </> pkgCabal pkg]
        -- We still don't know who built the configure script from configure.ac
        when (doesFileExist $ configure <.> "ac") $ need [configure]
        run' env GhcCabal
        -- TODO: when (registerPackage settings) $
        run' env (GhcPkg stage)
        postProcessPackageData $ dir </> "package-data.mk"

run' :: Environment -> Builder -> Action ()
run' env builder = do
    args <- interpret (env {getBuilder = builder}) $ fromDiff settings
    putColoured Green (show args)
    run builder args

--buildRule :: Package -> TodoItem -> Rules ()
--buildRule pkg @ (Package name path cabal _) todo @ (stage, dist, settings) =
--    let pathDist  = path </> dist
--        cabalPath = path </> cabal
--        configure = path </> "configure"
--    in
--    -- All these files are produced by a single run of GhcCabal
--    (pathDist </>) <$>
--    [ "package-data.mk"
--    , "haddock-prologue.txt"
--    , "inplace-pkg-config"
--    , "setup-config"
--    , "build" </> "autogen" </> "cabal_macros.h"
--    -- TODO: Is this needed? Also check out Paths_cpsa.hs.
--    -- , "build" </> "autogen" </> ("Paths_" ++ name) <.> "hs"
--    ] &%> \_ -> do
--        need [cabalPath]
--        when (doesFileExist $ configure <.> "ac") $ need [configure]
--        -- GhcCabal will run the configure script, so we depend on it
--        -- We still don't know who build the configure script from configure.ac
--        run GhcCabal $ cabalArgs pkg todo
--        when (registerPackage settings) $
--            run (GhcPkg stage) $ ghcPkgArgs pkg todo
--        postProcessPackageData $ pathDist </> "package-data.mk"

-- buildSettings = + builder Gcc ? ccSettings

-- builder Gcc ? "-tricky-flag"
