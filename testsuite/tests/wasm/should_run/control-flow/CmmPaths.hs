{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module CmmPaths
  ( cmmPaths
  , cmmExits
  )
where

-- Enumerates paths through a CmmGraph.  Paths can then
-- be used to determine a sequence of observations, which
-- is eventually converted into a sequence of Booleans
-- and used to test a translation.

import Prelude hiding (succ)

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label.NonDet
import GHC.Cmm.Switch

import GHC.Utils.Panic

import ActionsAndObservations
import ControlTestMonad

type CmmPath = [Event Stmt Expr]

-- | Return all paths that start in the entry node
-- and contain at most one repeated node.

cmmPaths :: CmmGraph -> [CmmPath]
cmmPaths g = map reverse $ pathsPrefixed (g_entry g) [] setEmpty
  where pathsPrefixed :: Label -> CmmPath -> LabelSet -> [CmmPath]
            -- ^ returns a list of all _short_ paths that begin with (block : prefix),
            -- where a short path is one that contains at most one repeated label,
            -- which must be the last one on the path (and so at the head of the list).
            -- Precondition: `visited == setFromList prefix`.
        pathsPrefixed lbl prefix visited = prefix' : extensions
          where prefix' = action lbl : prefix
                visited' = setInsert lbl visited
                extensions = if setMember lbl visited then [prefix']
                             else concatMap extend (cmmExits $ blockLabeled lbl)
                extend (Nothing, lbl) = pathsPrefixed lbl prefix' visited'
                extend (Just event, lbl) = pathsPrefixed lbl (event : prefix') visited'


        action lbl = Action (stmt lbl (middle $ blockLabeled lbl))
        blockLabeled lbl = mapFindWithDefault (panic "missing block") lbl blockmap

        middle block = m
            where (_, m, _) = blockSplit block

        CmmGraph { g_graph = GMany NothingO blockmap NothingO } = g

-- | Returns the successors of the given nodes, associating each
-- successor with the event/observation (if any) that causes the
-- computation to transfer control to that successor.

cmmExits :: CmmBlock -> [(Maybe (Event Stmt Expr), Label)]
cmmExits b =
  let thisExp e = expr (entryLabel b) e
  in
    case lastNode b of
      CmmBranch l -> [(Nothing, l)]
      CmmCondBranch e t f _ -> [(Just $ Predicate (thisExp e) True, t),
                                (Just $ Predicate (thisExp e) False, f)]
      CmmSwitch e targets ->
          let (lo, hi) = switchTargetsRange targets
              dests = switchTargetsCases targets
              other = switchTargetsDefault targets
              caseExit (j, lbl) = (Just $ Switch (thisExp e) (lo, hi + 1) j, lbl)
              defaultExits = case other of
                               Nothing -> []
                               Just lbl -> [(Just $ Switch (thisExp e) (lo, hi + 1) defarg, lbl)]
              defarg = try lo
                  where try i | i == hi = i
                              | i `elem` caseArgs = try (i + 1)
                              | otherwise = i
                        caseArgs = map fst dests
              labelOf i = case [lbl | (j, lbl) <- dests, j == i]
                          of [lbl] -> lbl
                             [] -> case other of
                                     Just lbl -> lbl
                                     Nothing -> panic "GHC.Tests.CmmPaths.exit: no default"
                             (_ : _ : _) -> panic "GHC.Tests.CmmPaths.exit: too many matches"
          in  if hi - lo < 10 then
                [(Just $ Switch (thisExp e) (lo, hi + 1) i, labelOf i) | i <- [lo..hi]]
              else
                  -- as some switch statements go from minBound :: Int to maxBound :: Int
                defaultExits ++ map caseExit dests

      CmmCall { cml_cont = Just l } -> [(Nothing, l)]
      CmmCall { cml_cont = Nothing } -> []
      CmmForeignCall { succ = l } -> [(Nothing, l)]
