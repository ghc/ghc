module Distribution.Solver.Modular.Index
    ( Index
    , PInfo(..)
    , defaultQualifyOptions
    , mkIndex
    ) where

import Data.List as L
import Data.Map as M
import Prelude hiding (pi)

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree

-- | An index contains information about package instances. This is a nested
-- dictionary. Package names are mapped to instances, which in turn is mapped
-- to info.
type Index = Map PN (Map I PInfo)

-- | Info associated with a package instance.
-- Currently, dependencies, flags and failure reasons.
-- Packages that have a failure reason recorded for them are disabled
-- globally, for reasons external to the solver. We currently use this
-- for shadowing which essentially is a GHC limitation, and for
-- installed packages that are broken.
data PInfo = PInfo (FlaggedDeps PN) FlagInfo (Maybe FailReason)

mkIndex :: [(PN, I, PInfo)] -> Index
mkIndex xs = M.map M.fromList (groupMap (L.map (\ (pn, i, pi) -> (pn, (i, pi))) xs))

groupMap :: Ord a => [(a, b)] -> Map a [b]
groupMap xs = M.fromListWith (flip (++)) (L.map (\ (x, y) -> (x, [y])) xs)

defaultQualifyOptions :: Index -> QualifyOptions
defaultQualifyOptions idx = QO {
      qoBaseShim         = or [ dep == base
                              | -- Find all versions of base ..
                                Just is <- [M.lookup base idx]
                                -- .. which are installed ..
                              , (I _ver (Inst _), PInfo deps _flagNfo _fr) <- M.toList is
                                -- .. and flatten all their dependencies ..
                              , (LDep _ (Dep _is_exe dep _ci), _comp) <- flattenFlaggedDeps deps
                              ]
    , qoSetupIndependent = True
    }
  where
    base = mkPackageName "base"
