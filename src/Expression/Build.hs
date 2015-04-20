{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, TypeFamilies #-}

module Expression.Build (
    BuildVariable (..),
    BuildPredicate (..),
    BuildExpression,
    Ways, Packages, TargetDirs,
    (?), (??), whenExists, support,
    (|>), msum, mproduct, fromList, fromOrderedList,
    packages, package,
    builders, builder, stagedBuilder,
    stages, stage, notStage,
    ways, way, files, file,
    configValues, config, configYes, configNo, configNonEmpty
    ) where

import Base
import Ways
import Oracles.Builder
import Package (Package)
import Expression.PG

-- Build variables that can be used in build predicates
data BuildVariable = PackageVariable Package
                   | BuilderVariable Builder
                   | StageVariable   Stage
                   | WayVariable     Way
                   | FileVariable    FilePattern
                   | ConfigVariable  String String -- from config files
                   deriving (Show, Eq)

-- A datatype for build predicates
data BuildPredicate
    = Evaluated Bool                    -- Evaluated predicate
    | Unevaluated BuildVariable         -- To be evaluated later
    | Not BuildPredicate                -- Negation
    | And BuildPredicate BuildPredicate -- Conjunction
    | Or  BuildPredicate BuildPredicate -- Disjunction
    deriving Eq -- TODO: create a proper Eq instance (use BDDs?)

instance Show BuildPredicate where
    showsPrec _ (Evaluated bool) = shows bool
    showsPrec _ (Unevaluated var) = shows var

    showsPrec d (Or p q) =
        showParen (d > 0) $ shows p . showString " \\/ " . shows q

    showsPrec d (And p q) =
        showParen (d > 1) $ showsPrec 1 p . showString " /\\ " . showsPrec 1 q

    showsPrec d (Not p) = showChar '!' . showsPrec 2 p

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

type Ways       = BuildExpression Way
type Packages   = BuildExpression Package
type TargetDirs = BuildExpression TargetDir

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
