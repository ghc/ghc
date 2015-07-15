module Settings.Util (
    -- Primitive settings elements
    arg, argPath, argM,
    argConfig, argStagedConfig, argConfigList, argStagedConfigList,
    appendCcArgs,
    -- argBuilderPath, argStagedBuilderPath,
    -- argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    -- argIncludeDirs, argDepIncludeDirs,
    -- argConcat, argConcatPath, argConcatSpace,
    -- argPairs, argPrefix, argPrefixPath,
    -- argPackageConstraints,
    ) where

import Base
import Util
import Builder
import Oracles.Base
import Expression

-- A single argument.
arg :: String -> Args
arg = append . return

-- A single path argument. The path gets unified.
argPath :: String -> Args
argPath = append . return . unifyPath

argM :: Action String -> Args
argM = appendM . fmap return

argConfig :: String -> Args
argConfig = appendM . fmap return . askConfig

argConfigList :: String -> Args
argConfigList = appendM . fmap words . askConfig

stagedKey :: Stage -> String -> String
stagedKey stage key = key ++ "-stage" ++ show stage

argStagedConfig :: String -> Args
argStagedConfig key = do
    stage <- asks getStage
    argConfig (stagedKey stage key)

argStagedConfigList :: String -> Args
argStagedConfigList key = do
    stage <- asks getStage
    argConfigList (stagedKey stage key)

-- Pass arguments to Gcc and corresponding lists of sub-arguments of GhcCabal
appendCcArgs :: [String] -> Args
appendCcArgs xs = do
    stage <- asks getStage
    mconcat [ builder (Gcc stage) ? append xs
            , builder GhcCabal    ? appendSub "--configure-option=CFLAGS" xs
            , builder GhcCabal    ? appendSub "--gcc-options" xs ]

-- packageData :: Arity -> String -> Args
-- packageData arity key =
--     return $ EnvironmentParameter $ PackageData arity key Nothing Nothing

-- -- Accessing key value pairs from package-data.mk files
-- argPackageKey :: Args
-- argPackageKey = packageData Single "PACKAGE_KEY"

-- argPackageDeps :: Args
-- argPackageDeps = packageData Multiple "DEPS"

-- argPackageDepKeys :: Args
-- argPackageDepKeys = packageData Multiple "DEP_KEYS"

-- argSrcDirs :: Args
-- argSrcDirs = packageData Multiple "HS_SRC_DIRS"

-- argIncludeDirs :: Args
-- argIncludeDirs = packageData Multiple "INCLUDE_DIRS"

-- argDepIncludeDirs :: Args
-- argDepIncludeDirs = packageData Multiple "DEP_INCLUDE_DIRS_SINGLE_QUOTED"

-- argPackageConstraints :: Packages -> Args
-- argPackageConstraints = return . EnvironmentParameter . PackageConstraints

-- -- Concatenate arguments: arg1 ++ arg2 ++ ...
-- argConcat :: Args -> Args
-- argConcat = return . Fold Concat

-- -- </>-concatenate arguments: arg1 </> arg2 </> ...
-- argConcatPath :: Args -> Args
-- argConcatPath = return . Fold ConcatPath

-- -- Concatene arguments (space separated): arg1 ++ " " ++ arg2 ++ ...
-- argConcatSpace :: Args -> Args
-- argConcatSpace = return . Fold ConcatSpace

-- -- An ordered list of pairs of arguments: prefix |> arg1, prefix |> arg2, ...
-- argPairs :: String -> Args -> Args
-- argPairs prefix settings = settings >>= (arg prefix |>) . return

-- -- An ordered list of prefixed arguments: prefix ++ arg1, prefix ++ arg2, ...
-- argPrefix :: String -> Args -> Args
-- argPrefix prefix = fmap (Fold Concat . (arg prefix |>) . return)

-- -- An ordered list of prefixed arguments: prefix </> arg1, prefix </> arg2, ...
-- argPrefixPath :: String -> Args -> Args
-- argPrefixPath prefix = fmap (Fold ConcatPath . (arg prefix |>) . return)

-- TODO: do '-ticky' in all debug ways?
-- wayHcArgs :: Way -> Args
-- wayHcArgs (Way _ units) = args
--     [ if (Dynamic    `elem` units)
--       then args ["-fPIC", "-dynamic"]
--       else arg "-static"
--     , when (Threaded   `elem` units) $ arg "-optc-DTHREADED_RTS"
--     , when (Debug      `elem` units) $ arg "-optc-DDEBUG"
--     , when (Profiling  `elem` units) $ arg "-prof"
--     , when (Logging    `elem` units) $ arg "-eventlog"
--     , when (Parallel   `elem` units) $ arg "-parallel"
--     , when (GranSim    `elem` units) $ arg "-gransim"
--     , when (units == [Debug] || units == [Debug, Dynamic]) $
--       args ["-ticky", "-DTICKY_TICKY"] ]
