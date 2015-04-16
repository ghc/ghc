{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, TypeFamilies #-}

module Expression.Build (
    BuildVariable (..),
    BuildPredicate (..),
    BuildExpression (..),
    evaluate, tellTruth,
    linearise, (|>), msum, mproduct, fromList, fromOrderedList,
    packages, package, matchPackage,
    builders, builder, matchBuilder, matchBuilderFamily,
    stages, stage, notStage, matchStage,
    ways, way, matchWay,
    files, file, matchFile,
    configValues, config, configYes, configNo, configNonEmpty, matchConfig,
    supportsPackageKey, targetPlatforms, targetPlatform,
    targetOss, targetOs, targetArchs, dynamicGhcPrograms, ghcWithInterpreter,
    platformSupportsSharedLibs, crossCompiling,
    gccIsClang, gccLt46, windowsHost
    ) where

import Control.Applicative
import Base
import Ways
import Package (Package)
import Oracles.Builder
import Expression.PG

-- Build variables that can be used in build predicates
data BuildVariable = PackageVariable Package
                   | BuilderVariable Builder
                   | StageVariable   Stage
                   | WayVariable     Way
                   | FileVariable    FilePattern
                   | ConfigVariable  String String -- from config files

-- A datatype for build predicates
data BuildPredicate
    = Evaluated Bool                    -- Evaluated predicate
    | Unevaluated BuildVariable         -- To be evaluated later
    | Not BuildPredicate                -- Negation
    | And BuildPredicate BuildPredicate -- Conjunction
    | Or  BuildPredicate BuildPredicate -- Disjunction

instance Predicate BuildPredicate where
    type Variable BuildPredicate = BuildVariable
    variable = Unevaluated
    true     = Evaluated True
    false    = Evaluated False
    not      = Not
    (&&)     = And
    (||)     = Or

alternatives :: Predicate a => (b -> Variable a) -> [b] -> a
alternatives f = foldr (||) false . map (variable . f)

type BuildExpression v = PG BuildPredicate v

-- Partially evaluate a BuildPredicate with a truth-teller function
-- that takes a BuildVariable and returns a Maybe Bool, where Nothing
-- is returned if the argument cannot be evaluated.
evaluate :: (BuildVariable -> Maybe Bool) -> BuildPredicate -> BuildPredicate
evaluate _ p @ (Evaluated _) = p
evaluate t p @ (Unevaluated q) = case t q of
    Just bool -> Evaluated bool
    Nothing   -> p
evaluate t (Not p  ) = Not (evaluate t p)
evaluate t (And p q) = And (evaluate t p) (evaluate t q)
evaluate t (Or  p q) = Or  (evaluate t p) (evaluate t q)

-- Attempt to fully evaluate a predicate (a truth-teller function!). Returns
-- Nothing if the predicate cannot be evaluated due to remaining unevaluated
-- variables.
tellTruth :: BuildPredicate -> Maybe Bool
tellTruth (Evaluated bool) = Just bool
tellTruth (Unevaluated _)  = Nothing
tellTruth (Not p)          = not <$> tellTruth p
tellTruth (And p q)
    | (p' == Just False) || (q' == Just False) = Just False
    | (p' == Just True ) && (q' == Just True ) = Just True
    | otherwise                                = Nothing
  where
    p' = tellTruth p
    q' = tellTruth q
tellTruth (Or p q)
    | (p' == Just True ) || (q' == Just True ) = Just True
    | (p' == Just False) && (q' == Just False) = Just False
    | otherwise                                = Nothing
  where
    p' = tellTruth p
    q' = tellTruth q

-- Linearise a build expression into a list. Returns Nothing if the given
-- expression cannot be uniquely evaluated due to remaining variables.
-- Overlay subexpressions are linearised in arbitrary order.
linearise :: BuildExpression v -> Maybe [v]
linearise Epsilon         = Just []
linearise (Vertex v)      = Just [v]
linearise (Overlay   p q) = (++) <$> linearise p <*> linearise q -- TODO: union
linearise (Sequence  p q) = (++) <$> linearise p <*> linearise q
linearise (Condition x q) = case tellTruth x of
    Just True  -> linearise q
    Just False -> Just []
    Nothing    -> Nothing

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

stage :: Stage -> BuildPredicate
stage s = stages [s]

notStage :: Stage -> BuildPredicate
notStage = not . Unevaluated . StageVariable

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

-- Truth-tellers for partial evaluation

matchPackage :: Package -> BuildVariable -> Maybe Bool
matchPackage p (PackageVariable p') = Just $ p == p'
matchPackage _ _                    = Nothing

matchBuilder :: Builder -> BuildVariable -> Maybe Bool
matchBuilder b (BuilderVariable b') = Just $ b == b'
matchBuilder _ _                    = Nothing

matchBuilderFamily :: (Stage -> Builder) -> BuildVariable -> Maybe Bool
matchBuilderFamily f (BuilderVariable b) = Just $ b `elem` map f [Stage0 ..]
matchBuilderFamily _ _                   = Nothing

matchStage :: Stage -> BuildVariable -> Maybe Bool
matchStage s (StageVariable s') = Just $ s == s'
matchStage _ _                  = Nothing

matchWay :: Way -> BuildVariable -> Maybe Bool
matchWay w (WayVariable w') = Just $ w == w'
matchWay _ _                = Nothing

matchFile :: FilePath -> BuildVariable -> Maybe Bool
matchFile file (FileVariable pattern) = Just $ pattern ?== file
matchFile _ _                     = Nothing

matchConfig :: String -> String -> BuildVariable -> Maybe Bool
matchConfig key value (ConfigVariable key' value')
    | key == key'   = Just $ value == value'
    | otherwise     = Nothing
matchKeyValue _ _ _ = Nothing

-- Derived predicates

supportsPackageKey :: BuildPredicate
supportsPackageKey = configYes "supports-package-key"

targetPlatforms :: [String] -> BuildPredicate
targetPlatforms = configValues "target-platform-full"

targetPlatform :: String -> BuildPredicate
targetPlatform s = targetPlatforms [s]

targetOss :: [String] -> BuildPredicate
targetOss = configValues "target-os"

targetOs :: String -> BuildPredicate
targetOs s = targetOss [s]

targetArchs :: [String] -> BuildPredicate
targetArchs = configValues "target-arch"

solarisBrokenShld :: BuildPredicate
solarisBrokenShld = configYes "solaris-broken-shld"

platformSupportsSharedLibs :: BuildPredicate
platformSupportsSharedLibs =
    not (targetPlatforms [ "powerpc-unknown-linux"
                         , "x86_64-unknown-mingw32"
                         , "i386-unknown-mingw32" ]
        ||
        solarisBrokenShld && targetPlatform "i386-unknown-solaris2")

dynamicGhcPrograms :: BuildPredicate
dynamicGhcPrograms = configYes "dynamic-ghc-programs"

ghcWithInterpreter :: BuildPredicate
ghcWithInterpreter =
    targetOss [ "mingw32", "cygwin32", "linux", "solaris2"
              , "freebsd", "dragonfly", "netbsd", "openbsd"
              , "darwin", "kfreebsdgnu" ]
    &&
    targetArchs ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

crossCompiling :: BuildPredicate
crossCompiling = configYes "cross-compiling"

gccIsClang :: BuildPredicate
gccIsClang = configYes "gcc-is-clang"

gccLt46 :: BuildPredicate
gccLt46 = configYes "gcc-lt-46"

windowsHost :: BuildPredicate
windowsHost = configValues "host-os-cpp" ["mingw32", "cygwin32"]
