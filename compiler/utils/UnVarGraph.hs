{-

Copyright (c) 2014 Joachim Breitner

A data structure for undirected graphs of variables
(or in plain terms: Sets of unordered pairs of numbers)


This is very specifically tailored for the use in CallArity. In particular it
stores the graph as a union of complete and complete bipartite graph, which
would be very expensive to store as sets of edges or as adjanceny lists.

It does not normalize the graphs. This means that g `unionUnVarGraph` g is
equal to g, but twice as expensive and large.

-}
module UnVarGraph
    ( UnVarSet
    , emptyUnVarSet, mkUnVarSet, varEnvDom, unionUnVarSet, unionUnVarSets
    , delUnVarSet
    , elemUnVarSet, isEmptyUnVarSet
    , UnVarGraph
    , emptyUnVarGraph
    , unionUnVarGraph, unionUnVarGraphs
    , completeGraph, completeBipartiteGraph
    , neighbors
    , delNode
    ) where

import Id
import VarEnv
import UniqFM
import Outputable
import Data.List
import Bag
import Unique

import qualified Data.IntSet as S

-- We need a type for sets of variables (UnVarSet).
-- We do not use VarSet, because for that we need to have the actual variable
-- at hand, and we do not have that when we turn the domain of a VarEnv into a UnVarSet.
-- Therefore, use a IntSet directly (which is likely also a bit more efficient).

-- Set of uniques, i.e. for adjancet nodes
newtype UnVarSet = UnVarSet (S.IntSet)
    deriving Eq

k :: Var -> Int
k v = getKey (getUnique v)

emptyUnVarSet :: UnVarSet
emptyUnVarSet = UnVarSet S.empty

elemUnVarSet :: Var -> UnVarSet -> Bool
elemUnVarSet v (UnVarSet s) = k v `S.member` s


isEmptyUnVarSet :: UnVarSet -> Bool
isEmptyUnVarSet (UnVarSet s) = S.null s

delUnVarSet :: UnVarSet -> Var -> UnVarSet
delUnVarSet (UnVarSet s) v = UnVarSet $ k v `S.delete` s

mkUnVarSet :: [Var] -> UnVarSet
mkUnVarSet vs = UnVarSet $ S.fromList $ map k vs

varEnvDom :: VarEnv a -> UnVarSet
varEnvDom ae = UnVarSet $ ufmToSet_Directly ae

unionUnVarSet :: UnVarSet -> UnVarSet -> UnVarSet
unionUnVarSet (UnVarSet set1) (UnVarSet set2) = UnVarSet (set1 `S.union` set2)

unionUnVarSets :: [UnVarSet] -> UnVarSet
unionUnVarSets = foldr unionUnVarSet emptyUnVarSet

instance Outputable UnVarSet where
    ppr (UnVarSet s) = braces $
        hcat $ punctuate comma [ ppr (getUnique i) | i <- S.toList s]


-- The graph type. A list of complete bipartite graphs
data Gen = CBPG UnVarSet UnVarSet -- complete bipartite
         | CG   UnVarSet          -- complete
newtype UnVarGraph = UnVarGraph (Bag Gen)

emptyUnVarGraph :: UnVarGraph
emptyUnVarGraph = UnVarGraph emptyBag

unionUnVarGraph :: UnVarGraph -> UnVarGraph -> UnVarGraph
{-
Premature optimisation, it seems.
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s1 == s3 && s2 == s4
    = pprTrace "unionUnVarGraph fired" empty $
      completeGraph (s1 `unionUnVarSet` s2)
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s2 == s3 && s1 == s4
    = pprTrace "unionUnVarGraph fired2" empty $
      completeGraph (s1 `unionUnVarSet` s2)
-}
unionUnVarGraph (UnVarGraph g1) (UnVarGraph g2)
    = -- pprTrace "unionUnVarGraph" (ppr (length g1, length g2)) $
      UnVarGraph (g1 `unionBags` g2)

unionUnVarGraphs :: [UnVarGraph] -> UnVarGraph
unionUnVarGraphs = foldl' unionUnVarGraph emptyUnVarGraph

-- completeBipartiteGraph A B = { {a,b} | a ∈ A, b ∈ B }
completeBipartiteGraph :: UnVarSet -> UnVarSet -> UnVarGraph
completeBipartiteGraph s1 s2 = prune $ UnVarGraph $ unitBag $ CBPG s1 s2

completeGraph :: UnVarSet -> UnVarGraph
completeGraph s = prune $ UnVarGraph $ unitBag $ CG s

neighbors :: UnVarGraph -> Var -> UnVarSet
neighbors (UnVarGraph g) v = unionUnVarSets $ concatMap go $ bagToList g
  where go (CG s)       = (if v `elemUnVarSet` s then [s] else [])
        go (CBPG s1 s2) = (if v `elemUnVarSet` s1 then [s2] else []) ++
                          (if v `elemUnVarSet` s2 then [s1] else [])

delNode :: UnVarGraph -> Var -> UnVarGraph
delNode (UnVarGraph g) v = prune $ UnVarGraph $ mapBag go g
  where go (CG s)       = CG (s `delUnVarSet` v)
        go (CBPG s1 s2) = CBPG (s1 `delUnVarSet` v) (s2 `delUnVarSet` v)

prune :: UnVarGraph -> UnVarGraph
prune (UnVarGraph g) = UnVarGraph $ filterBag go g
  where go (CG s)       = not (isEmptyUnVarSet s)
        go (CBPG s1 s2) = not (isEmptyUnVarSet s1) && not (isEmptyUnVarSet s2)

instance Outputable Gen where
    ppr (CG s)       = ppr s  <> char '²'
    ppr (CBPG s1 s2) = ppr s1 <+> char 'x' <+> ppr s2
instance Outputable UnVarGraph where
    ppr (UnVarGraph g) = ppr g
