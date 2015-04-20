{-# LANGUAGE FlexibleInstances #-}

module Expression.Args (
    Args (..), BuildParameter (..), EnvironmentParameter (..),
    Arity (..), Combine (..),
    Settings,
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

import Base hiding (arg, args, Args)
import Util
import Oracles.Builder
import Expression.Build

-- Settings comprise the following primitive elements
data Args
    = Plain String                              -- e.g. "-O2"
    | BuildParameter BuildParameter             -- e.g. build path
    | EnvironmentParameter EnvironmentParameter -- e.g. host OS
    | Fold Combine Settings                     -- e.g. ccSettings
    deriving (Show, Eq)

-- Build parameters to be determined during the build process
data BuildParameter
    = PackagePath -- path to the current package, e.g. "libraries/deepseq"
    | BuildDir    -- build directory, e.g. "dist-install"
    | Input       -- input file(s), e.g. "src.hs"
    | Output      -- output file(s), e.g. ["src.o", "src.hi"]
    deriving (Show, Eq)

-- Environment parameters to be determined using oracles
data EnvironmentParameter
    = BuilderPath Builder                -- look up path to a Builder
    | Config Arity String                -- look up configuration flag(s)
    | PackageData                        -- look up package-data.mk flag(s)
      {
        pdArity       :: Arity,          -- arity of value (Single or Multiple)
        pdKey         :: String,         -- key to look up, e.g. "PACKAGE_KEY"
        pdPackagePath :: Maybe FilePath, -- path to the current package
        pdBuildDir    :: Maybe FilePath  -- build directory
      }
    | PackageConstraints Packages        -- package version constraints
    deriving (Show, Eq)

-- Method for combining settings elements in Fold Combine Settings
data Combine = Id            -- Keep given settings as is
             | Concat        -- Concatenate: a ++ b
             | ConcatPath    -- </>-concatenate: a </> b
             | ConcatSpace   -- concatenate with a space: a ++ " " ++ b
             deriving (Show, Eq)

data Arity = Single   -- expands to a single argument
           | Multiple -- expands to a list of arguments
           deriving (Show, Eq)

type Settings = BuildExpression Args

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
