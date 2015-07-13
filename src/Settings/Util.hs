{-# LANGUAGE NoImplicitPrelude #-}

module Settings.Util (
    -- Primitive settings elements
    arg, argM, args, argWith,
    argConfig, argStagedConfig, argConfigList, argStagedConfigList,
    ccArgs,
    -- argBuilderPath, argStagedBuilderPath,
    -- argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    -- argIncludeDirs, argDepIncludeDirs,
    -- argConcat, argConcatPath, argConcatSpace,
    -- argPairs, argPrefix, argPrefixPath,
    -- argPackageConstraints,
    ) where

import Base hiding (Args, arg, args)
import Oracles hiding (not)
import Expression

-- A single argument
arg :: String -> Settings
arg = append . return

argM :: Action String -> Settings
argM = appendM . fmap return

-- A list of arguments
args :: [String] -> Settings
args = append

argWith :: Builder -> Settings
argWith = argM . with

argConfig :: String -> Settings
argConfig = appendM . fmap return . askConfig

argConfigList :: String -> Settings
argConfigList = appendM . fmap words . askConfig

stagedKey :: Stage -> String -> String
stagedKey stage key = key ++ "-stage" ++ show stage

argStagedConfig :: String -> Settings
argStagedConfig key = do
    stage <- asks getStage
    argConfig (stagedKey stage key)

argStagedConfigList :: String -> Settings
argStagedConfigList key = do
    stage <- asks getStage
    argConfigList (stagedKey stage key)

-- Pass arguments to Gcc and corresponding lists of sub-arguments of GhcCabal
ccArgs :: [String] -> Settings
ccArgs xs = do
    stage <- asks getStage
    mconcat [ builder (Gcc stage) ? args xs
            , builder GhcCabal    ? appendSub "--configure-option=CFLAGS" xs
            , builder GhcCabal    ? appendSub "--gcc-options" xs ]




-- packageData :: Arity -> String -> Settings
-- packageData arity key =
--     return $ EnvironmentParameter $ PackageData arity key Nothing Nothing

-- -- Accessing key value pairs from package-data.mk files
-- argPackageKey :: Settings
-- argPackageKey = packageData Single "PACKAGE_KEY"

-- argPackageDeps :: Settings
-- argPackageDeps = packageData Multiple "DEPS"

-- argPackageDepKeys :: Settings
-- argPackageDepKeys = packageData Multiple "DEP_KEYS"

-- argSrcDirs :: Settings
-- argSrcDirs = packageData Multiple "HS_SRC_DIRS"

-- argIncludeDirs :: Settings
-- argIncludeDirs = packageData Multiple "INCLUDE_DIRS"

-- argDepIncludeDirs :: Settings
-- argDepIncludeDirs = packageData Multiple "DEP_INCLUDE_DIRS_SINGLE_QUOTED"

-- argPackageConstraints :: Packages -> Settings
-- argPackageConstraints = return . EnvironmentParameter . PackageConstraints

-- -- Concatenate arguments: arg1 ++ arg2 ++ ...
-- argConcat :: Settings -> Settings
-- argConcat = return . Fold Concat

-- -- </>-concatenate arguments: arg1 </> arg2 </> ...
-- argConcatPath :: Settings -> Settings
-- argConcatPath = return . Fold ConcatPath

-- -- Concatene arguments (space separated): arg1 ++ " " ++ arg2 ++ ...
-- argConcatSpace :: Settings -> Settings
-- argConcatSpace = return . Fold ConcatSpace

-- -- An ordered list of pairs of arguments: prefix |> arg1, prefix |> arg2, ...
-- argPairs :: String -> Settings -> Settings
-- argPairs prefix settings = settings >>= (arg prefix |>) . return

-- -- An ordered list of prefixed arguments: prefix ++ arg1, prefix ++ arg2, ...
-- argPrefix :: String -> Settings -> Settings
-- argPrefix prefix = fmap (Fold Concat . (arg prefix |>) . return)

-- -- An ordered list of prefixed arguments: prefix </> arg1, prefix </> arg2, ...
-- argPrefixPath :: String -> Settings -> Settings
-- argPrefixPath prefix = fmap (Fold ConcatPath . (arg prefix |>) . return)
