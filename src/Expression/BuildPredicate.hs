{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}

module Expression.BuildPredicate (
    BuildVariable (..),
    BuildPredicate (..)
    ) where

import Base
import Ways
import Oracles.Builder
import Package (Package)
import Expression.Predicate

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

instance Predicate BuildPredicate where
    type Variable BuildPredicate = BuildVariable
    variable = Unevaluated
    true     = Evaluated True
    false    = Evaluated False
    not      = Not
    (&&)     = And
    (||)     = Or

instance Show BuildPredicate where
    showsPrec _ (Evaluated bool) = shows bool
    showsPrec _ (Unevaluated var) = shows var

    showsPrec d (Or p q) =
        showParen (d > 0) $ shows p . showString " \\/ " . shows q

    showsPrec d (And p q) =
        showParen (d > 1) $ showsPrec 1 p . showString " /\\ " . showsPrec 1 q

    showsPrec d (Not p) = showChar '!' . showsPrec 2 p
