{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Expression.BuildExpression (
    BuildExpression,
    Ways, Packages, TargetDirs,

    -- reexport from Expression.PG:
    bimap, (|>), (?), (??), whenExists, support,
    msum, mproduct,
    fromList, fromOrderedList
    ) where

import Base
import Ways
import Package (Package)
import Oracles.Builder
import Expression.PG
import Expression.Project
import Expression.BuildPredicate

type BuildExpression v = PG BuildPredicate v

type Ways       = BuildExpression Way
type Packages   = BuildExpression Package
type TargetDirs = BuildExpression TargetDir

-- Projecting a build expression requires examining all predicates and vertices
instance (Project Package v, Project Package BuildPredicate)
    => Project Package (BuildExpression v) where
    project p = bimap (project p) (project p)

instance (Project Builder v, Project Builder BuildPredicate)
    => Project Builder (BuildExpression v) where
    project b = bimap (project b) (project b)

instance (Project (Stage -> Builder) v,
    Project (Stage -> Builder) BuildPredicate)
    => Project (Stage -> Builder) (BuildExpression v) where
    project s2b = bimap (project s2b) (project s2b)

instance (Project Stage v, Project Stage BuildPredicate)
    => Project Stage (BuildExpression v) where
    project s = bimap (project s) (project s)

instance (Project TargetDir v, Project TargetDir BuildPredicate)
    => Project TargetDir (BuildExpression v) where
    project d = bimap (project d) (project d)

instance (Project Way v, Project Way BuildPredicate)
    => Project Way (BuildExpression v) where
    project w = bimap (project w) (project w)

instance (Project FilePath v, Project FilePath BuildPredicate)
    => Project FilePath (BuildExpression v) where
    project f = bimap (project f) (project f)
