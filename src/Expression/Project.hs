{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}

module Expression.Project (
    Project (..)
    ) where

import Base hiding (Args)
import Package
import Ways
import Oracles.Builder
import Expression.PG
import Expression.Settings
import Expression.BuildPredicate
import Expression.BuildExpression

-- Projecting (partially evaluating) values of type b by setting the
-- parameters of type a
class Project a b where
    project :: a -> b -> b
    project = const id

-- Project a build predicate recursively through Not, And and Or
pmap :: Project a BuildPredicate => a -> BuildPredicate -> BuildPredicate
pmap a (Not p  ) = Not (project a p)
pmap a (And p q) = And (project a p) (project a q)
pmap a (Or  p q) = Or  (project a p) (project a q)
pmap _ p         = p

instance Project Package BuildPredicate where
    project pkg (Unevaluated (PackageVariable pkg')) = Evaluated $ pkg == pkg'
    project pkg p = pmap pkg p

instance Project Builder BuildPredicate where
    project b (Unevaluated (BuilderVariable b')) = Evaluated $ b == b'
    project b p = pmap b p

instance Project (Stage -> Builder) BuildPredicate where
    project s2b (Unevaluated (BuilderVariable b)) =
        Evaluated $ b `elem` map s2b [Stage0 ..]
    project s2b p = pmap s2b p

instance Project Way BuildPredicate where
    project w (Unevaluated (WayVariable w')) = Evaluated $ w == w'
    project w p = pmap w p

instance Project Stage BuildPredicate where
    project s (Unevaluated (StageVariable s')) = Evaluated $ s == s'
    project s p = pmap s p

instance Project FilePath BuildPredicate where
    project f (Unevaluated (FileVariable p)) = Evaluated $ p ?== f
    project f p = pmap f p

-- TargetDirs do not appear in build predicates
instance Project TargetDir BuildPredicate where

-- Nothing to project in expressions containing FilePaths, Packages or Ways
instance Project a TargetDir where
instance Project a Package where
instance Project a Way where

-- Projecting on Way, Stage, Builder, FilePath and staged Builder is trivial:
-- only (Fold Combine Settings) and (EnvironmentParameter PackageConstraints)
-- can be affected (more specifically, the predicates contained in them).
-- This is handled with 'amap'.
amap :: (Project a Settings, Project a Packages) => a -> Args -> Args
amap p (Fold combine settings) = Fold combine (project p settings)
amap p (EnvironmentParameter (PackageConstraints ps)) =
    EnvironmentParameter $ PackageConstraints $ project p ps
amap _ a = a

instance Project Way Args where
    project = amap

instance Project Stage Args where
    project = amap

instance Project Builder Args where
    project = amap

instance Project FilePath Args where
    project = amap

instance Project (Stage -> Builder) Args where
    project = amap

-- Projecting on Package and TargetDir is more interesting.
instance Project Package Args where
    project p (BuildParameter PackagePath) = Plain $ pkgPath p
    project p (EnvironmentParameter pd @ (PackageData _ _ _ _)) =
        EnvironmentParameter $ pd { pdPackagePath = Just $ pkgPath p }
    project p a = amap p a

instance Project TargetDir Args where
    project (TargetDir d) (BuildParameter BuildDir) = Plain d
    project (TargetDir d) (EnvironmentParameter pd @ (PackageData _ _ _ _)) =
        EnvironmentParameter $ pd { pdBuildDir = Just d }
    project d a = amap d a

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

-- Composing projections
instance (Project a z, Project b z) => Project (a, b) z where
    project (p, q) = project p . project q

instance (Project a z, Project b z, Project c z) => Project (a, b, c) z where
    project (p, q, r) = project p . project q . project r
