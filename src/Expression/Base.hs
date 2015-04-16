{-# LANGUAGE FlexibleInstances #-}

module Expression.Base (
    module Expression.Build,
    module Expression.Predicate,
    (?), (??), whenExists,
    Args (..), -- hide?
    Settings,
    Packages,
    FilePaths,
    Ways,
    project,
    arg, args, argsOrdered, argBuildPath, argBuildDir,
    argInput, argOutput,
    argConfig, argStagedConfig, argBuilderPath, argStagedBuilderPath,
    argWithBuilder, argWithStagedBuilder,
    argPackageKey, argPackageDeps, argPackageDepKeys, argSrcDirs,
    argIncludeDirs, argDepIncludeDirs,
    argConcat, argConcatPath, argConcatSpace, argPairs, argPrefix,
    argBootPkgConstraints,
    setPackage, setBuilder, setBuilderFamily, setStage, setWay,
    setFile, setConfig
    ) where

import Base hiding (arg, args, Args)
import Ways
import Package (Package)
import Oracles.Builder
import Expression.PG
import Expression.Predicate
import Expression.Build

-- Settings can be built out of the following primitive elements
data Args
    = Plain String           -- a plain old string argument: e.g., "-O2"
    | BuildPath              -- evaluates to build path: "libraries/base"
    | BuildDir               -- evaluates to build directory: "dist-install"
    | Input                  -- evaluates to input file(s): "src.c"
    | Output                 -- evaluates to output file(s): "src.o"
    | Config String          -- evaluates to the value of a given config key
    | BuilderPath Builder    -- evaluates to the path to a given builder
    | PackageData String     -- looks up value a given key in package-data.mk
    | BootPkgConstraints     -- evaluates to boot package constraints
    | Fold Combine Settings  -- fold settings using a given combine method

data Combine = Concat        -- Concatenate: a ++ b
             | ConcatPath    -- </>-concatenate: a </> b
             | ConcatSpace   -- concatenate with a space: a ++ " " ++ b

type Ways      = BuildExpression Way
type Settings  = BuildExpression Args
type Packages  = BuildExpression Package
type FilePaths = BuildExpression FilePath

-- A single argument
arg :: String -> Settings
arg = return . Plain

-- A set of arguments (unordered)
args :: [String] -> Settings
args = msum . map arg

-- An (ordered) list of arguments
argsOrdered :: [String] -> Settings
argsOrdered = mproduct . map arg

argBuildPath :: Settings
argBuildPath = return BuildPath

argBuildDir :: Settings
argBuildDir = return BuildDir

argInput :: Settings
argInput = return Input

argOutput :: Settings
argOutput = return Output

argConfig :: String -> Settings
argConfig = return . Config

argStagedConfig :: String -> Settings
argStagedConfig key =
    msum $ map (\s -> stage s ? argConfig (stagedKey s)) [Stage0 ..]
  where
    stagedKey :: Stage -> String
    stagedKey stage = key ++ "-stage" ++ show stage

argBuilderPath :: Builder -> Settings
argBuilderPath = return . BuilderPath

-- evaluates to the path to a given builder, taking current stage into account
argStagedBuilderPath :: (Stage -> Builder) -> Settings
argStagedBuilderPath f =
    msum $ map (\s -> stage s ? argBuilderPath (f s)) [Stage0 ..]

argWithBuilder :: Builder -> Settings
argWithBuilder builder =
    let key = case builder of
            Ar       -> "--with-ar="
            Ld       -> "--with-ld="
            Gcc _    -> "--with-gcc="
            Ghc _    -> "--with-ghc="
            Alex     -> "--with-alex="
            Happy    -> "--with-happy="
            GhcPkg _ -> "--with-ghc-pkg="
            HsColour -> "--with-hscolour="
    in
    argPrefix key (argBuilderPath builder)

argWithStagedBuilder :: (Stage -> Builder) -> Settings
argWithStagedBuilder f =
    msum $ map (\s -> stage s ? argWithBuilder (f s)) [Stage0 ..]


-- Accessing key value pairs from package-data.mk files
argPackageKey :: Settings
argPackageKey = return $ PackageData "PACKAGE_KEY"

argPackageDeps :: Settings
argPackageDeps = return $ PackageData "DEPS"

argPackageDepKeys :: Settings
argPackageDepKeys = return $ PackageData "DEP_KEYS"

argSrcDirs :: Settings
argSrcDirs = return $ PackageData "HS_SRC_DIRS"

argIncludeDirs :: Settings
argIncludeDirs = return $ PackageData "INCLUDE_DIRS"

argDepIncludeDirs :: Settings
argDepIncludeDirs = return $ PackageData "DEP_INCLUDE_DIRS_SINGLE_QUOTED"

argBootPkgConstraints :: Settings
argBootPkgConstraints = return BootPkgConstraints

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
argPaths :: String -> Settings -> Settings
argPaths prefix = fmap (Fold ConcatPath . (arg prefix |>) . return)

-- Partially evaluate Settings using a truth-teller (compute a 'projection')
project :: (BuildVariable -> Maybe Bool) -> Settings -> Settings
project _ Epsilon = Epsilon
project t (Vertex v) = Vertex v -- TODO: go deeper
project t (Overlay   l r) = Overlay   (project  t l) (project t r)
project t (Sequence  l r) = Sequence  (project  t l) (project t r)
project t (Condition l r) = Condition (evaluate t l) (project t r)

-- Partial evaluation of settings

setPackage :: Package -> Settings -> Settings
setPackage = project . matchPackage

setBuilder :: Builder -> Settings -> Settings
setBuilder = project . matchBuilder

setBuilderFamily :: (Stage -> Builder) -> Settings -> Settings
setBuilderFamily = project . matchBuilderFamily

setStage :: Stage -> Settings -> Settings
setStage = project . matchStage

setWay :: Way -> Settings -> Settings
setWay = project . matchWay

setFile :: FilePath -> Settings -> Settings
setFile = project . matchFile

setConfig :: String -> String -> Settings -> Settings
setConfig key = project . matchConfig key

--type ArgsTeller = Args -> Maybe [String]

--fromPlain :: ArgsTeller
--fromPlain (Plain list) = Just list
--fromPlain _            = Nothing

--tellArgs :: ArgsTeller -> Args -> Args
--tellArgs t a = case t a of
--    Just list -> Plain list
--    Nothing   -> a
