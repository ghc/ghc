module Distribution.Solver.Modular.Assignment
    ( Assignment(..)
    , PAssignment
    , FAssignment
    , SAssignment
    , toCPs
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude hiding (pi)

import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Maybe

import Distribution.PackageDescription (FlagAssignment, mkFlagAssignment) -- from Cabal

import Distribution.Solver.Types.ComponentDeps (ComponentDeps, Component)
import qualified Distribution.Solver.Types.ComponentDeps as CD
import Distribution.Solver.Types.OptionalStanza
import Distribution.Solver.Types.PackagePath

import Distribution.Solver.Modular.Configured
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.LabeledGraph
import Distribution.Solver.Modular.Package

-- | A (partial) package assignment. Qualified package names
-- are associated with instances.
type PAssignment    = Map QPN I

type FAssignment    = Map QFN Bool
type SAssignment    = Map QSN Bool

-- | A (partial) assignment of variables.
data Assignment = A PAssignment FAssignment SAssignment
  deriving (Show, Eq)

-- | Delivers an ordered list of fully configured packages.
--
-- TODO: This function is (sort of) ok. However, there's an open bug
-- w.r.t. unqualification. There might be several different instances
-- of one package version chosen by the solver, which will lead to
-- clashes.
toCPs :: Assignment -> RevDepMap -> [CP QPN]
toCPs (A pa fa sa) rdm =
  let
    -- get hold of the graph
    g   :: Graph Component
    vm  :: Vertex -> ((), QPN, [(Component, QPN)])
    cvm :: QPN -> Maybe Vertex
    -- Note that the RevDepMap contains duplicate dependencies. Therefore the nub.
    (g, vm, cvm) = graphFromEdges (L.map (\ (x, xs) -> ((), x, nub xs))
                                  (M.toList rdm))
    tg :: Graph Component
    tg = transposeG g
    -- Topsort the dependency graph, yielding a list of pkgs in the right order.
    -- The graph will still contain all the installed packages, and it might
    -- contain duplicates, because several variables might actually resolve to
    -- the same package in the presence of qualified package names.
    ps :: [PI QPN]
    ps = L.map ((\ (_, x, _) -> PI x (pa M.! x)) . vm) $
         topSort g
    -- Determine the flags per package, by walking over and regrouping the
    -- complete flag assignment by package.
    fapp :: Map QPN FlagAssignment
    fapp = M.fromListWith mappend $
           L.map (\ ((FN qpn fn), b) -> (qpn, mkFlagAssignment [(fn, b)])) $
           M.toList $
           fa
    -- Stanzas per package.
    sapp :: Map QPN [OptionalStanza]
    sapp = M.fromListWith (++) $
           L.map (\ ((SN qpn sn), b) -> (qpn, if b then [sn] else [])) $
           M.toList $
           sa
    -- Dependencies per package.
    depp :: QPN -> [(Component, PI QPN)]
    depp qpn = let v :: Vertex
                   v   = fromJust (cvm qpn)
                   dvs :: [(Component, Vertex)]
                   dvs = tg A.! v
               in L.map (\ (comp, dv) -> case vm dv of (_, x, _) -> (comp, PI x (pa M.! x))) dvs
    -- Translated to PackageDeps
    depp' :: QPN -> ComponentDeps [PI QPN]
    depp' = CD.fromList . L.map (\(comp, d) -> (comp, [d])) . depp
  in
    L.map (\ pi@(PI qpn _) -> CP pi
                                 (M.findWithDefault mempty qpn fapp)
                                 (M.findWithDefault mempty qpn sapp)
                                 (depp' qpn))
          ps
