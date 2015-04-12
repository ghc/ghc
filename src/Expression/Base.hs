{-# LANGUAGE FlexibleInstances #-}

module Expression.Base (
    module Expression.Build,
    module Expression.Predicate,
    (?), (??), whenExists,
    Args (..), -- hide?
    Settings,
    Ways,
    remove, project,
    arg, args, argsOrdered, argPairs, argBuildPath, argBuildDist,
    argConfig, argBuilderPath, argStagedBuilderPath,
    argPackageKey, argPackageDeps, argPackageDepKeys,
    argComplex, argPath, argBootPkgConstraints,
    setPackage, setBuilder, setBuilderFamily, setStage, setWay,
    setFile, setConfig
    ) where

import Base hiding (arg, args, Args)
import Ways
import Package.Base (Package)
import Oracles.Builder
import Expression.PG
import Expression.Predicate
import Expression.Build

-- Settings can be built out of the following primitive elements
data Args
    = Plain [String]          -- an (ordered) list of arguments: ["-i", "dir"]
    | Pairs String [String]   -- resolves to a list of pairs: "-i dir1 -i dir2"
    | BuildPath               -- evaluates to build path: libraries/base
    | BuildDist               -- evaluates to build subdirectory: dist-install
    | Config String           -- evaluates to the value of a given config key
    | BuilderPath Builder     -- evaluates to the path to a given builder
    | PackageKey String       -- looks up "PACKAGE_KEY" in package-data.mk
    | PackageDeps String      -- looks up "DEPS"        in package-data.mk
    | PackageDepKeys String   -- looks up "DEP_KEYS"    in package-data.mk
    | BootPkgConstraints      -- evaluates to boot package constraints
    | Complex String Settings -- joins a prefix with settings: "CFLAGS=..."
    | Path String Settings    -- as above but joins settings elements with </>

type Settings = BuildExpression Args
type Ways     = BuildExpression Way

-- A single argument
arg :: String -> Settings
arg s = Vertex $ Plain [s]

-- A set of arguments (unordered)
args :: [String] -> Settings
args = mconcat . map arg

-- An (ordered) list of arguments
argsOrdered :: [String] -> Settings
argsOrdered = Vertex . Plain

-- An (ordered) list of pair of arguments: [prefix, arg1, prefix, arg2, ...]
argPairs :: String -> [String] -> Settings
argPairs prefix = Vertex . Pairs prefix

argBuildDist :: Settings
argBuildPath = Vertex $ BuildPath

argBuildPath :: Settings
argBuildDist = Vertex $ BuildDist

argConfig :: String -> Settings
argConfig = Vertex . Config

argBuilderPath :: Builder -> Settings
argBuilderPath = Vertex . BuilderPath

-- evaluates to the path to a given builder, taking current stage into account
argStagedBuilderPath :: (Stage -> Builder) -> Settings
argStagedBuilderPath f =
    mconcat $ map (\s -> stage s ? argBuilderPath (f s)) [Stage0 ..]

argPackageKey :: String -> Settings
argPackageKey = Vertex . PackageKey

argPackageDeps :: String -> Settings
argPackageDeps = Vertex . PackageDeps

argPackageDepKeys :: String -> Settings
argPackageDepKeys = Vertex . PackageDepKeys

argBootPkgConstraints :: Settings
argBootPkgConstraints = Vertex $ BootPkgConstraints

argComplex :: String -> Settings -> Settings
argComplex prefix = Vertex . Complex prefix

argPath :: String -> Settings -> Settings
argPath prefix = Vertex . Path prefix

-- Partially evaluate Settings using a truth-teller (compute a 'projection')
project :: (BuildVariable -> Maybe Bool) -> Settings -> Settings
project _ Epsilon = Epsilon
project t (Vertex v) = case v of
    Complex l r -> argComplex l (project t r)
    Path    l r -> argPath    l (project t r)
    _           -> Vertex v
project t (Overlay   l r) = Overlay   (project  t l) (project t r)
project t (Sequence  l r) = Sequence  (project  t l) (project t r)
project t (Condition l r) = Condition (evaluate t l) (project t r)

-- Removes a given argument list from settings
remove :: [String] -> Settings -> Settings
remove _ Epsilon = Epsilon
remove as v @ (Vertex (Plain bs))
    | as == bs  = Epsilon
    | otherwise = v
remove _ v @ (Vertex _)   = v
remove as (Overlay   l r) = Overlay   (remove as l) (remove as r)
remove as (Sequence  l r) = Sequence  (remove as l) (remove as r)
remove as (Condition x r) = Condition x             (remove as r)

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
