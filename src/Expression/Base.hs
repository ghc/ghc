{-# LANGUAGE FlexibleInstances #-}

module Expression.Base (
    Guard,
    Settings,
    ArgList (..),
    Ways, targetWays,
    remove,
    opt, opts, subSettings, optBuildPath, optBuildDist, optPath,
    optBootPkgConstraints,
    packages, package, setPackage,
    builders, builder, setBuilder, setBuilderFamily,
    stages, stage, notStage, setStage,
    ways, way, setWay,
    files, file, setFile,
    optKeyValue, optBuilder, optStagedBuilder,
    crossCompiling,
    keyValues, keyValue, keyYes, keyNo, keyNonEmpty, setKeyValue,
    packageKey, packageDeps, packageDepKeys,
    supportsPackageKey, targetPlatforms, targetPlatform,
    targetOss, targetArchs,
    dynamicGhcPrograms, ghcWithInterpreter
    ) where

import Base
import Ways
import Package.Base (Package)
import Oracles.Builder
import Expression.PG
import Expression.Predicate
import Expression.PGPredicate
-- import Expression.ArgList

data BuildParameter = WhenPackage  Package
                    | WhenBuilder  Builder
                    | WhenStage    Stage
                    | WhenWay      Way
                    | WhenFile     FilePattern
                    | WhenKeyValue String String -- from config files

type Guard = Predicate BuildParameter

type Matcher = TruthTeller BuildParameter

type Expression v = PGPredicate BuildParameter v

type Settings = Expression ArgList

type Ways = Expression Way

data ArgList = Plain [String]
             | Complex String Settings
             | Path String Settings
             | BuildPath
             | BuildDist
             | BootPkgConstraints
             | KeyValue String
             | BuilderPath Builder
             | PackageKey String
             | PackageDeps String
             | PackageDepKeys String

plain :: String -> ArgList
plain s = Plain [s]

subSettings :: String -> Settings -> Settings
subSettings prefix = Vertex . Complex prefix

type ArgsTeller = ArgList -> Maybe [String]

-- Monoid instance for args-tellers (asks them one by one)
instance Monoid ArgsTeller where
    mempty        = const Nothing
    p `mappend` q = \a -> getFirst $ First (p a) <> First (q a)

fromPlain :: ArgsTeller
fromPlain (Plain list) = Just list
fromPlain _            = Nothing

tellArgs :: ArgsTeller -> ArgList -> ArgList
tellArgs t a = case t a of
    Just list -> Plain list
    Nothing   -> a

-- TODO: move to Targets.hs
targetWays :: Ways
targetWays = Vertex vanilla
    <> notStage Stage0 ? Vertex profiling
    <> platformSupportsSharedLibs ? Vertex dynamic

-- TODO: rename to 'arg' and args'
opt :: String -> Settings
opt = Vertex . plain

opts :: [String] -> Settings
opts = mconcat . map opt

optBuildPath, optBuildDist, optBootPkgConstraints :: Settings
optBuildPath          = Vertex $ BuildPath
optBuildDist          = Vertex $ BuildDist
optBootPkgConstraints = Vertex $ BootPkgConstraints

optPath :: String -> Settings -> Settings
optPath prefix = Vertex . Path prefix

---- Extract all plain and unconditional arguments.
---- Overlay subexpressions are evaluated in arbitrary order.
--plainArgs :: PGPredicate p v -> [v]
--plainArgs Epsilon         = []
--plainArgs (Vertex (Plain args)) = args
--plainArgs (Vertex _)            = []
--plainArgs (Overlay   l r) = (++) <$> plainArgs l <*> plainArgs r -- TODO: union
--plainArgs (Sequence  l r) = (++) <$> plainArgs l <*> plainArgs r
--plainArgs (Condition x r) = case tellTruth x of
--    Just True -> plainArgs r
--    _         -> []

-- Partially evaluate Settings using a truth-teller (compute a 'projection')
project :: Matcher -> Settings -> Settings
project _ Epsilon         = Epsilon
project m (Vertex v)  = case v of
    Complex l r -> Vertex $ Complex l (project m r)
    _           -> Vertex v
project m (Overlay   l r) = Overlay   (project  m l) (project m r)
project m (Sequence  l r) = Sequence  (project  m l) (project m r)
project m (Condition l r) = Condition (evaluate m l) (project m r)

remove :: [String] -> Settings -> Settings
remove _ Epsilon         = Epsilon
remove a v @ (Vertex (Plain b))
    | a == b    = Epsilon
    | otherwise = v
remove _ v @ (Vertex _)  = v
remove a (Overlay   l r) = Overlay   (remove a l) (remove a r)
remove a (Sequence  l r) = Sequence  (remove a l) (remove a r)
remove a (Condition x r) = Condition x            (remove a r)


alternatives :: (a -> BuildParameter) -> [a] -> Guard
alternatives p = multiOr . map (Parameter . p)

-- Basic GHC build guards

packages :: [Package] -> Guard
packages = alternatives WhenPackage

builders :: [Builder] -> Guard
builders = alternatives WhenBuilder

stages :: [Stage] -> Guard
stages = alternatives WhenStage

ways :: [Way] -> Guard
ways = alternatives WhenWay

files :: [FilePattern] -> Guard
files = alternatives WhenFile

keyValues :: String -> [String] -> Guard
keyValues key = alternatives (WhenKeyValue key)

package :: Package -> Guard
package p = packages [p]

builder :: Builder -> Guard
builder b = builders [b]

stage :: Stage -> Guard
stage s = stages [s]

notStage :: Stage -> Guard
notStage = Not . Parameter . WhenStage

way :: Way -> Guard
way w = ways [w]

file :: FilePattern -> Guard
file f = files [f]

keyValue :: String -> String -> Guard
keyValue key value = keyValues key [value]

keyYes, keyNo, keyNonEmpty :: String -> Guard
keyYes      key = keyValues key ["YES"]
keyNo       key = keyValues key ["NO" ]
keyNonEmpty key = Not $ keyValues key [""]

-- Partial evaluation of settings

setPackage :: Package -> Settings -> Settings
setPackage = project . matchPackage

setBuilder :: Builder -> Settings -> Settings
setBuilder = project . matchBuilder

setBuilderFamily :: (Stage -> Builder) -> Settings -> Settings
setBuilderFamily f = mconcat $ map (setBuilder . f) [Stage0 ..]

setStage :: Stage -> Settings -> Settings
setStage = project . matchStage

setWay :: Way -> Settings -> Settings
setWay = project . matchWay

setFile :: FilePath -> Settings -> Settings
setFile = project . matchFile

setKeyValue :: String -> String -> Settings -> Settings
setKeyValue key = project . matchKeyValue key

-- Truth-tellers for partial evaluation

matchPackage :: Package -> Matcher
matchPackage p (WhenPackage p') = Just $ p == p'
matchPackage _ _                = Nothing

matchBuilder :: Builder -> Matcher
matchBuilder b (WhenBuilder b') = Just $ b == b'
matchBuilder _ _                = Nothing

matchStage :: Stage -> Matcher
matchStage s (WhenStage s') = Just $ s == s'
matchStage _ _              = Nothing

matchWay :: Way -> Matcher
matchWay w (WhenWay w') = Just $ w == w'
matchWay _ _            = Nothing

matchFile :: FilePath -> Matcher
matchFile file (WhenFile pattern) = Just $ pattern ?== file
matchFile _ _                     = Nothing

matchKeyValue :: String -> String -> Matcher
matchKeyValue key value (WhenKeyValue key' value')
    | key == key' = Just $ value == value'
    | otherwise   = Nothing
matchKeyValue _ _ _ = Nothing

-- Argument templates

optKeyValue :: String -> Settings
optKeyValue = Vertex . KeyValue

optBuilder :: Builder -> Settings
optBuilder = Vertex . BuilderPath

optStagedBuilder :: (Stage -> Builder) -> Settings
optStagedBuilder f =
    mconcat $ map (\s -> builder (f s) ? optBuilder (f s)) [Stage0 ..]

packageKey :: String -> Settings
packageKey = Vertex . PackageKey

packageDeps :: String -> Settings
packageDeps = Vertex . PackageDeps

packageDepKeys :: String -> Settings
packageDepKeys = Vertex . PackageDepKeys

-- Derived guards

supportsPackageKey :: Guard
supportsPackageKey = keyYes "supports-package-key"

targetPlatforms :: [String] -> Guard
targetPlatforms = keyValues "target-platform-full"

targetPlatform :: String -> Guard
targetPlatform s = targetPlatforms [s]

targetOss :: [String] -> Guard
targetOss = keyValues "target-os"

targetArchs :: [String] -> Guard
targetArchs = keyValues "target-arch"

solarisBrokenShld :: Guard
solarisBrokenShld = keyYes "solaris-broken-shld"

platformSupportsSharedLibs :: Guard
platformSupportsSharedLibs =
    Not $ (targetPlatforms [ "powerpc-unknown-linux"
                           , "x86_64-unknown-mingw32"
                           , "i386-unknown-mingw32" ]
           `Or`
           solarisBrokenShld `And` targetPlatform "i386-unknown-solaris2")

dynamicGhcPrograms :: Guard
dynamicGhcPrograms = keyYes "dynamic-ghc-programs"

ghcWithInterpreter :: Guard
ghcWithInterpreter =
    targetOss [ "mingw32", "cygwin32", "linux", "solaris2"
              , "freebsd", "dragonfly", "netbsd", "openbsd"
              , "darwin", "kfreebsdgnu" ]
    `And`
    targetArchs ["i386", "x86_64", "powerpc", "sparc", "sparc64", "arm"]

crossCompiling :: Guard
crossCompiling = keyYes "cross-compiling"

