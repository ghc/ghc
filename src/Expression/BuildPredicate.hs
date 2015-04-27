{-# LANGUAGE NoImplicitPrelude, TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Expression.BuildPredicate (
    BuildVariable (..),
    BuildPredicate, rewrite
    ) where

import Base
import Ways
import Oracles.Builder
import Package (Package)
import Expression.Project
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

-- A (fold like) rewrite of a PG according to given instructions
rewrite :: (Bool                             -> r) -- how to rewrite Booleans
        -> (BuildVariable                    -> r) -- how to rewrite variables
        -> (BuildPredicate                   -> r) -- how to rewrite Not's
        -> (BuildPredicate -> BuildPredicate -> r) -- how to rewrite And's
        -> (BuildPredicate -> BuildPredicate -> r) -- how to rewrite Or's
        -> BuildPredicate                          -- BuildPredicate to rewrite
        -> r                                       -- result
rewrite fb fv fn fa fo p = case p of
    Evaluated   b -> fb b
    Unevaluated v -> fv v
    Not         q -> fn   q
    And       p q -> fa p q
    Or        p q -> fo p q

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

eval :: (BuildVariable -> BuildPredicate) -> BuildPredicate -> BuildPredicate
eval f = rewrite Evaluated f (Not . eval f) fa fo
  where
    fa p q = And (eval f p) (eval f q)
    fo p q = Or  (eval f p) (eval f q)

instance Project Package BuildPredicate where
    project p = eval f
      where
        f (PackageVariable p') = Evaluated $ p == p'
        f var                  = Unevaluated var

instance Project Builder BuildPredicate where
    project b = eval f
      where
        f (BuilderVariable b') = Evaluated $ b == b'
        f var                  = Unevaluated var

instance Project (Stage -> Builder) BuildPredicate where
    project s2b = eval f
      where
        f (BuilderVariable b) = Evaluated $ b `elem` map s2b [Stage0 ..]
        f var                 = Unevaluated var

instance Project Way BuildPredicate where
    project w = eval f
      where
        f (WayVariable w') = Evaluated $ w == w'
        f var              = Unevaluated var

instance Project Stage BuildPredicate where
    project s = eval f
      where
        f (StageVariable s') = Evaluated $ s == s'
        f var                = Unevaluated var

instance Project FilePath BuildPredicate where
    project f = eval g
      where
        g (FileVariable f') = Evaluated $ f == f'
        g var               = Unevaluated var

-- TargetDirs do not appear in build predicates
instance Project TargetDir BuildPredicate where
