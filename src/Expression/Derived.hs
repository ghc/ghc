{-# LANGUAGE NoImplicitPrelude #-}

module Expression.Derived (
    Settings,

    -- Constructing build predicates
    packages, package,
    builders, builder, stagedBuilder,
    stages, stage, notStage,
    ways, way, files, file,
    configValues, config, configYes, configNo, configNonEmpty,

    -- Primitive settings elements
    arg, args, argPath, argsOrdered, argBuildPath, argBuildDir,
    argInput, argOutput,
    argConfig, argStagedConfig, argConfigList, argStagedConfigList,
    argBuilderPath, argStagedBuilderPath,
    argWithBuilder, argWithStagedBuilder,
    argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    argIncludeDirs, argDepIncludeDirs,
    argConcat, argConcatPath, argConcatSpace,
    argPairs, argPrefix, argPrefixPath,
    argPackageConstraints
    ) where

import Base hiding (Args, arg, args)
import Ways
import Util
import Package (Package)
import Oracles.Builder
import Expression.PG
import Expression.Settings
import Expression.BuildPredicate
import Expression.BuildExpression

-- Auxiliary function for multiway disjunction
alternatives :: (a -> BuildVariable) -> [a] -> BuildPredicate
alternatives f = foldr (||) false . map (variable . f)

-- Basic GHC build predicates
packages :: [Package] -> BuildPredicate
packages = alternatives PackageVariable

builders :: [Builder] -> BuildPredicate
builders = alternatives BuilderVariable

stages :: [Stage] -> BuildPredicate
stages = alternatives StageVariable

ways :: [Way] -> BuildPredicate
ways = alternatives WayVariable

files :: [FilePattern] -> BuildPredicate
files = alternatives FileVariable

configValues :: String -> [String] -> BuildPredicate
configValues key = alternatives (ConfigVariable key)

package :: Package -> BuildPredicate
package p = packages [p]

builder :: Builder -> BuildPredicate
builder b = builders [b]

stagedBuilder :: (Stage -> Builder) -> BuildPredicate
stagedBuilder s2b = builders $ map s2b [Stage0 ..]

stage :: Stage -> BuildPredicate
stage s = stages [s]

notStage :: Stage -> BuildPredicate
notStage = not . variable . StageVariable

way :: Way -> BuildPredicate
way w = ways [w]

file :: FilePattern -> BuildPredicate
file f = files [f]

config :: String -> String -> BuildPredicate
config key value = configValues key [value]

configYes :: String -> BuildPredicate
configYes key = configValues key ["YES"]

configNo :: String -> BuildPredicate
configNo key = configValues key ["NO" ]

configNonEmpty :: String -> BuildPredicate
configNonEmpty key = not $ configValues key [""]

-- A single argument
arg :: String -> Settings
arg = return . Plain

-- A single FilePath argument
argPath :: FilePath -> Settings
argPath = return . Plain . unifyPath

-- A set of arguments (unordered)
args :: [String] -> Settings
args = msum . map arg

-- An (ordered) list of arguments
argsOrdered :: [String] -> Settings
argsOrdered = mproduct . map arg

argBuildPath :: Settings
argBuildPath = return $ BuildParameter $ PackagePath

argBuildDir :: Settings
argBuildDir = return $ BuildParameter $ BuildDir

argInput :: Settings
argInput = return $ BuildParameter $ Input

argOutput :: Settings
argOutput = return $ BuildParameter $ Output

argConfig :: String -> Settings
argConfig = return . EnvironmentParameter . Config Single

argConfigList :: String -> Settings
argConfigList = return . EnvironmentParameter . Config Multiple

stagedKey :: Stage -> String -> String
stagedKey stage key = key ++ "-stage" ++ show stage

argStagedConfig :: String -> Settings
argStagedConfig key =
    msum $ map (\s -> stage s ? argConfig (stagedKey s key)) [Stage0 ..]

argStagedConfigList :: String -> Settings
argStagedConfigList key =
    msum $ map (\s -> stage s ? argConfigList (stagedKey s key)) [Stage0 ..]

-- evaluates to the path to a given builder
argBuilderPath :: Builder -> Settings
argBuilderPath = return . EnvironmentParameter . BuilderPath

-- as above but takes current stage into account
argStagedBuilderPath :: (Stage -> Builder) -> Settings
argStagedBuilderPath f =
    msum $ map (\s -> stage s ? argBuilderPath (f s)) [Stage0 ..]

-- evaluates to 'with-builder=path/to/builder' for a given builder
argWithBuilder :: Builder -> Settings
argWithBuilder builder =
    argPrefix (withBuilderKey builder) (argBuilderPath builder)

-- as above but takes current stage into account
argWithStagedBuilder :: (Stage -> Builder) -> Settings
argWithStagedBuilder f =
    msum $ map (\s -> stage s ? argWithBuilder (f s)) [Stage0 ..]

packageData :: Arity -> String -> Settings
packageData arity key =
    return $ EnvironmentParameter $ PackageData arity key Nothing Nothing

-- Accessing key value pairs from package-data.mk files
argPackageKey :: Settings
argPackageKey = packageData Single "PACKAGE_KEY"

argPackageDeps :: Settings
argPackageDeps = packageData Multiple "DEPS"

argPackageDepKeys :: Settings
argPackageDepKeys = packageData Multiple "DEP_KEYS"

argSrcDirs :: Settings
argSrcDirs = packageData Multiple "HS_SRC_DIRS"

argIncludeDirs :: Settings
argIncludeDirs = packageData Multiple "INCLUDE_DIRS"

argDepIncludeDirs :: Settings
argDepIncludeDirs = packageData Multiple "DEP_INCLUDE_DIRS_SINGLE_QUOTED"

argPackageConstraints :: Packages -> Settings
argPackageConstraints = return . EnvironmentParameter . PackageConstraints

-- Concatenate arguments: arg1 ++ arg2 ++ ...
argConcat :: Settings -> Settings
argConcat = return . Fold Concat

-- </>-concatenate arguments: arg1 </> arg2 </> ...
argConcatPath :: Settings -> Settings
argConcatPath = return . Fold ConcatPath

-- Concatene arguments (space separated): arg1 ++ " " ++ arg2 ++ ...
argConcatSpace :: Settings -> Settings
argConcatSpace = return . Fold ConcatSpace

-- An ordered list of pairs of arguments: prefix |> arg1, prefix |> arg2, ...
argPairs :: String -> Settings -> Settings
argPairs prefix settings = settings >>= (arg prefix |>) . return

-- An ordered list of prefixed arguments: prefix ++ arg1, prefix ++ arg2, ...
argPrefix :: String -> Settings -> Settings
argPrefix prefix = fmap (Fold Concat . (arg prefix |>) . return)

-- An ordered list of prefixed arguments: prefix </> arg1, prefix </> arg2, ...
argPrefixPath :: String -> Settings -> Settings
argPrefixPath prefix = fmap (Fold ConcatPath . (arg prefix |>) . return)
