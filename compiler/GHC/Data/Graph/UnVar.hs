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
    ( UnVarSet
    , emptyUnVarSet, mkUnVarSet, unionUnVarSet, unionUnVarSets
    , extendUnVarSet, extendUnVarSetList, delUnVarSet, delUnVarSetList
    , elemUnVarSet, isEmptyUnVarSet
    , UnVarGraph
    , emptyUnVarGraph
    , unionUnVarGraph, unionUnVarGraphs
    , completeGraph, completeBipartiteGraph
    , neighbors
    , hasLoopAt
    , delNode
    , domUFMUnVarSet
    ) where

import GHC.Prelude

import GHC.Types.Unique.FM( UniqFM, ufmToSet_Directly )
import GHC.Types.Var
import GHC.Utils.Outputable
import GHC.Types.Unique
import GHC.Word

import qualified GHC.Data.Word64Set as S

-- We need a type for sets of variables (UnVarSet).
-- We do not use VarSet, because for that we need to have the actual variable
-- at hand, and we do not have that when we turn the domain of a VarEnv into a UnVarSet.
-- Therefore, use a IntSet directly (which is likely also a bit more efficient).

-- Set of uniques, i.e. for adjacent nodes
newtype UnVarSet = UnVarSet S.Word64Set
    deriving Eq

k :: Var -> Word64
k v = getKey (getUnique v)

domUFMUnVarSet :: UniqFM key elt -> UnVarSet
domUFMUnVarSet ae = UnVarSet $ ufmToSet_Directly ae

emptyUnVarSet :: UnVarSet
emptyUnVarSet = UnVarSet S.empty

elemUnVarSet :: Var -> UnVarSet -> Bool
elemUnVarSet v (UnVarSet s) = k v `S.member` s


isEmptyUnVarSet :: UnVarSet -> Bool
isEmptyUnVarSet (UnVarSet s) = S.null s

delUnVarSet :: UnVarSet -> Var -> UnVarSet
delUnVarSet (UnVarSet s) v = UnVarSet $ k v `S.delete` s

delUnVarSetList :: UnVarSet -> [Var] -> UnVarSet
delUnVarSetList s vs = s `minusUnVarSet` mkUnVarSet vs

minusUnVarSet :: UnVarSet -> UnVarSet -> UnVarSet
minusUnVarSet (UnVarSet s) (UnVarSet s') = UnVarSet $ s `S.difference` s'

sizeUnVarSet :: UnVarSet -> Int
sizeUnVarSet (UnVarSet s) = S.size s

mkUnVarSet :: [Var] -> UnVarSet
mkUnVarSet vs = UnVarSet $ S.fromList $ map k vs

extendUnVarSet :: Var -> UnVarSet -> UnVarSet
extendUnVarSet v (UnVarSet s) = UnVarSet $ S.insert (k v) s

extendUnVarSetList :: [Var] -> UnVarSet -> UnVarSet
extendUnVarSetList vs s = s `unionUnVarSet` mkUnVarSet vs

unionUnVarSet :: UnVarSet -> UnVarSet -> UnVarSet
unionUnVarSet (UnVarSet set1) (UnVarSet set2) = UnVarSet (set1 `S.union` set2)

unionUnVarSets :: [UnVarSet] -> UnVarSet
unionUnVarSets = foldl' (flip unionUnVarSet) emptyUnVarSet

instance Outputable UnVarSet where
    ppr (UnVarSet s) = braces $
        hcat $ punctuate comma [ ppr (mkUniqueGrimily i) | i <- S.toList s]

data UnVarGraph = CBPG  !UnVarSet !UnVarSet -- ^ complete bipartite graph
                | CG    !UnVarSet           -- ^ complete graph
                | Union UnVarGraph UnVarGraph
                | Del   !UnVarSet UnVarGraph

emptyUnVarGraph :: UnVarGraph
emptyUnVarGraph = CG emptyUnVarSet

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
unionUnVarGraph a b
  | is_null a = b
  | is_null b = a
  | otherwise = Union a b

unionUnVarGraphs :: [UnVarGraph] -> UnVarGraph
unionUnVarGraphs = foldl' unionUnVarGraph emptyUnVarGraph

-- completeBipartiteGraph A B = { {a,b} | a ∈ A, b ∈ B }
completeBipartiteGraph :: UnVarSet -> UnVarSet -> UnVarGraph
completeBipartiteGraph s1 s2 = prune $ CBPG s1 s2

completeGraph :: UnVarSet -> UnVarGraph
completeGraph s = prune $ CG s

-- (v' ∈ neighbors G v) <=> v--v' ∈ G
neighbors :: UnVarGraph -> Var -> UnVarSet
neighbors = go
  where
    go (Del d g) v
      | v `elemUnVarSet` d = emptyUnVarSet
      | otherwise          = go g v `minusUnVarSet` d
    go (Union g1 g2) v     = go g1 v `unionUnVarSet` go g2 v
    go (CG s) v            = if v `elemUnVarSet` s then s else emptyUnVarSet
    go (CBPG s1 s2) v      = (if v `elemUnVarSet` s1 then s2 else emptyUnVarSet) `unionUnVarSet`
                             (if v `elemUnVarSet` s2 then s1 else emptyUnVarSet)

-- hasLoopAt G v <=> v--v ∈ G
hasLoopAt :: UnVarGraph -> Var -> Bool
hasLoopAt = go
  where
    go (Del d g) v
      | v `elemUnVarSet` d  = False
      | otherwise           = go g v
    go (Union g1 g2) v      = go g1 v || go g2 v
    go (CG s) v             = v `elemUnVarSet` s
    go (CBPG s1 s2) v       = v `elemUnVarSet` s1 && v `elemUnVarSet` s2

delNode :: UnVarGraph -> Var -> UnVarGraph
delNode (Del d g) v = Del (extendUnVarSet v d) g
delNode g         v
  | is_null g       = emptyUnVarGraph
  | otherwise       = Del (mkUnVarSet [v]) g

-- | Resolves all `Del`, by pushing them in, and simplifies `∅ ∪ … = …`
prune :: UnVarGraph -> UnVarGraph
prune = go emptyUnVarSet
  where
    go :: UnVarSet -> UnVarGraph -> UnVarGraph
    go dels (Del dels' g) = go (dels `unionUnVarSet` dels') g
    go dels (Union g1 g2)
      | is_null g1' = g2'
      | is_null g2' = g1'
      | otherwise   = Union g1' g2'
      where
        g1' = go dels g1
        g2' = go dels g2
    go dels (CG s)        = CG (s `minusUnVarSet` dels)
    go dels (CBPG s1 s2)  = CBPG (s1 `minusUnVarSet` dels) (s2 `minusUnVarSet` dels)

-- | Shallow empty check.
is_null :: UnVarGraph -> Bool
is_null (CBPG s1 s2)  = isEmptyUnVarSet s1 || isEmptyUnVarSet s2
is_null (CG   s)      = isEmptyUnVarSet s
is_null _             = False

instance Outputable UnVarGraph where
    ppr (Del d g) = text "Del" <+> ppr (sizeUnVarSet d) <+> parens (ppr g)
    ppr (Union a b) = text "Union" <+> parens (ppr a) <+> parens (ppr b)
    ppr (CG s) = text "CG" <+> ppr (sizeUnVarSet s)
    ppr (CBPG a b) = text "CBPG" <+> ppr (sizeUnVarSet a) <+> ppr (sizeUnVarSet b)
