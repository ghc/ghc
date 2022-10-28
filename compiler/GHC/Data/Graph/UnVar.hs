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
module GHC.Data.Graph.UnVar
    ( UnVarGraph
    , emptyUnVarGraph
    , unionUnVarGraph, unionUnVarGraphs
    , completeGraph, completeBipartiteGraph
    , neighbors
    , hasLoopAt
    , delNode
    ) where

import GHC.Prelude

import GHC.Types.Id
import GHC.Utils.Outputable
import GHC.Types.Var.Set
import GHC.Types.Unique.SlimSet

data UnVarGraph = CBPG  !VarSlimSet !VarSlimSet -- ^ complete bipartite graph
                | CG    !VarSlimSet           -- ^ complete graph
                | Union UnVarGraph UnVarGraph
                | Del   !VarSlimSet UnVarGraph

emptyUnVarGraph :: UnVarGraph
emptyUnVarGraph = CG emptyUniqSlimSet

unionUnVarGraph :: UnVarGraph -> UnVarGraph -> UnVarGraph
{-
Premature optimisation, it seems.
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s1 == s3 && s2 == s4
    = pprTrace "unionUnVarGraph fired" empty $
      completeGraph (s1 `unionUniqSlimSet` s2)
unionUnVarGraph (UnVarGraph [CBPG s1 s2]) (UnVarGraph [CG s3, CG s4])
    | s2 == s3 && s1 == s4
    = pprTrace "unionUnVarGraph fired2" empty $
      completeGraph (s1 `unionUniqSlimSet` s2)
-}
unionUnVarGraph a b
  | is_null a = b
  | is_null b = a
  | otherwise = Union a b

unionUnVarGraphs :: [UnVarGraph] -> UnVarGraph
unionUnVarGraphs = foldl' unionUnVarGraph emptyUnVarGraph

-- completeBipartiteGraph A B = { {a,b} | a ∈ A, b ∈ B }
completeBipartiteGraph :: VarSlimSet -> VarSlimSet -> UnVarGraph
completeBipartiteGraph s1 s2 = prune $ CBPG s1 s2

completeGraph :: VarSlimSet -> UnVarGraph
completeGraph s = prune $ CG s

-- (v' ∈ neighbors G v) <=> v--v' ∈ G
neighbors :: UnVarGraph -> Var -> VarSlimSet
neighbors = go
  where
    go (Del d g) v
      | v `elemUniqSlimSet` d = emptyUniqSlimSet
      | otherwise          = go g v `minusUniqSlimSet` d
    go (Union g1 g2) v     = go g1 v `unionUniqSlimSet` go g2 v
    go (CG s) v            = if v `elemUniqSlimSet` s then s else emptyUniqSlimSet
    go (CBPG s1 s2) v      = (if v `elemUniqSlimSet` s1 then s2 else emptyUniqSlimSet) `unionUniqSlimSet`
                             (if v `elemUniqSlimSet` s2 then s1 else emptyUniqSlimSet)

-- hasLoopAt G v <=> v--v ∈ G
hasLoopAt :: UnVarGraph -> Var -> Bool
hasLoopAt = go
  where
    go (Del d g) v
      | v `elemUniqSlimSet` d  = False
      | otherwise           = go g v
    go (Union g1 g2) v      = go g1 v || go g2 v
    go (CG s) v             = v `elemUniqSlimSet` s
    go (CBPG s1 s2) v       = v `elemUniqSlimSet` s1 && v `elemUniqSlimSet` s2

delNode :: UnVarGraph -> Var -> UnVarGraph
delNode (Del d g) v = Del (extendUniqSlimSet v d) g
delNode g         v
  | is_null g       = emptyUnVarGraph
  | otherwise       = Del (mkUniqSlimSet [v]) g

-- | Resolves all `Del`, by pushing them in, and simplifies `∅ ∪ … = …`
prune :: UnVarGraph -> UnVarGraph
prune = go emptyUniqSlimSet
  where
    go :: VarSlimSet -> UnVarGraph -> UnVarGraph
    go dels (Del dels' g) = go (dels `unionUniqSlimSet` dels') g
    go dels (Union g1 g2)
      | is_null g1' = g2'
      | is_null g2' = g1'
      | otherwise   = Union g1' g2'
      where
        g1' = go dels g1
        g2' = go dels g2
    go dels (CG s)        = CG (s `minusUniqSlimSet` dels)
    go dels (CBPG s1 s2)  = CBPG (s1 `minusUniqSlimSet` dels) (s2 `minusUniqSlimSet` dels)

-- | Shallow empty check.
is_null :: UnVarGraph -> Bool
is_null (CBPG s1 s2)  = isEmptyUniqSlimSet s1 || isEmptyUniqSlimSet s2
is_null (CG   s)      = isEmptyUniqSlimSet s
is_null _             = False

instance Outputable UnVarGraph where
    ppr (Del d g) = text "Del" <+> ppr (sizeUniqSlimSet d) <+> parens (ppr g)
    ppr (Union a b) = text "Union" <+> parens (ppr a) <+> parens (ppr b)
    ppr (CG s) = text "CG" <+> ppr (sizeUniqSlimSet s)
    ppr (CBPG a b) = text "CBPG" <+> ppr (sizeUniqSlimSet a) <+> ppr (sizeUniqSlimSet b)
