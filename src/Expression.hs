{-# LANGUAGE FlexibleInstances #-}

module Expression (
    Guard,
    Settings,
    module Expression.ArgList,
    module Expression.Predicate,
    opts, fence, (?),
    packages, package, setPackage,
    builders, builder, setBuilder,
    stages, stage, notStage, setStage,
    ways, way, setWay,
    files, file, setFile,
    keyValues, keyValue, keyYes, keyNo, setKeyValue,
    packageKey, packageDeps, packageDepKeys
    ) where

import Base
import Ways
import Package.Base (Package)
import Oracles.Builder
import Expression.PG
import Expression.Predicate
import Expression.ArgList

data BuildParameter = WhenPackage  Package
                    | WhenBuilder  Builder
                    | WhenStage    Stage
                    | WhenWay      Way
                    | WhenFile     FilePattern
                    | WhenKeyValue String String -- from config files

type Guard = Predicate BuildParameter

instance Monoid Guard where
    mempty = Evaluated True
    mappend = And

type Settings = PG Guard ArgList

opts :: [String] -> Settings
opts = mconcat . map (\s -> Vertex $ Plain [s])

fence :: Settings -> Settings -> Settings
fence = Sequence

(?) :: Guard -> Settings -> Settings
(?) = Condition

infixl 7 ?

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

keyYes, keyNo :: String -> Guard
keyYes key = keyValues key ["YES"]
keyNo  key = keyValues key ["NO" ]

-- Partial evaluation of settings

setPackage :: Package -> Settings -> Settings
setPackage = project . matchPackage

setBuilder :: Builder -> Settings -> Settings
setBuilder = project . matchBuilder

setStage :: Stage -> Settings -> Settings
setStage = project . matchStage

setWay :: Way -> Settings -> Settings
setWay = project . matchWay

setFile :: FilePath -> Settings -> Settings
setFile = project . matchFile

setKeyValue :: String -> String -> Settings -> Settings
setKeyValue key = project . matchKeyValue key

-- Truth-tellers for partial evaluation

type Matcher = TruthTeller BuildParameter

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

packageKey :: String -> Settings
packageKey = Vertex . PackageKey

packageDeps :: String -> Settings
packageDeps = Vertex . PackageDeps

packageDepKeys :: String -> Settings
packageDepKeys = Vertex . PackageDepKeys
