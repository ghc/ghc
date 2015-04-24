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
import Expression.PG
import Expression.BuildPredicate

type BuildExpression v = PG BuildPredicate v

type Ways       = BuildExpression Way
type Packages   = BuildExpression Package
type TargetDirs = BuildExpression TargetDir
