{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Expression.Project (
    Project (..)
    ) where

import Base hiding (Args)
import Ways
import Package

-- Projecting (partially evaluating) values of type b by setting the
-- parameters of type a
class Project a b where
    project :: a -> b -> b
    project = const id

-- Nothing to project in expressions containing FilePaths, Packages or Ways
instance Project a TargetDir where
instance Project a Package where
instance Project a Way where

-- Composing projections
instance (Project a z, Project b z) => Project (a, b) z where
    project (p, q) = project p . project q

instance (Project a z, Project b z, Project c z) => Project (a, b, c) z where
    project (p, q, r) = project p . project q . project r
