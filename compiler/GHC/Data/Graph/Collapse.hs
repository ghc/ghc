{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHC.Data.Graph.Collapse
  ( PureSupernode(..)
  , Supernode(..)
  , collapseInductiveGraph
  , VizCollapseMonad(..)
  , NullCollapseViz(..)
  , runNullCollapse
  , MonadUniqDSM(..)
  )
where

import GHC.Prelude

import Control.Exception
import Control.Monad
import Data.List (delete, union, insert, intersect)
import Data.Semigroup

import GHC.Cmm.Dataflow.Label
import GHC.Data.Graph.Inductive.Graph
import GHC.Types.Unique.DSM
import GHC.Utils.Panic hiding (assert)


{-|
Module      : GHC.Data.Graph.Collapse
Description : Implement the "collapsing" algorithm Hecht and Ullman

A control-flow graph is reducible if and only if it is collapsible
according to the definition of Hecht and Ullman (1972).   This module
implements the collapsing algorithm of Hecht and Ullman, and if it
encounters a graph that is not collapsible, it splits nodes until the
graph is fully collapsed.  It then reports what nodes (if any) had to
be split in order to collapse the graph.  The information is used
upstream to node-split Cmm graphs.

The module uses the inductive graph representation cloned from the
Functional Graph Library (Hackage package `fgl`, modules
`GHC.Data.Graph.Inductive.*`.)

-}

-- Full reference to paper: Matthew S. Hecht and Jeffrey D. Ullman
-- (1972).  Flow Graph Reducibility. SIAM J. Comput., 1(2), 188–202.
-- https://doi.org/10.1137/0201014


------------------ Graph-splitting monad -----------------------

-- | If you want to visualize the graph-collapsing algorithm, create
-- an instance of monad `VizCollapseMonad`.  Each step in the
-- algorithm is announced to the monad as a side effect.  If you don't
-- care about visualization, you would use the `NullCollapseViz`
-- monad, in which these operations are no-ops.

class (Monad m) => MonadUniqDSM m where
  liftUniqDSM :: UniqDSM a -> m a

class (MonadUniqDSM m, Graph gr, Supernode s m) => VizCollapseMonad m gr s where
  consumeByInGraph :: Node -> Node -> gr s () -> m ()
  splitGraphAt :: gr s () -> LNode s -> m ()
  finalGraph :: gr s () -> m ()

-- | The identity monad as a `VizCollapseMonad`.  Use this monad when
-- you want efficiency in graph collapse.
newtype NullCollapseViz a = NullCollapseViz { unNCV :: UniqDSM a }
  deriving (Functor, Applicative, Monad, MonadGetUnique)

instance MonadUniqDSM NullCollapseViz where
  liftUniqDSM = NullCollapseViz

instance (Graph gr, Supernode s NullCollapseViz) =>
    VizCollapseMonad NullCollapseViz gr s where
  consumeByInGraph _ _ _ = return ()
  splitGraphAt _ _ = return ()
  finalGraph _ = return ()

runNullCollapse :: NullCollapseViz a -> UniqDSM a
runNullCollapse = unNCV


------------------ Utility functions on graphs -----------------------


-- | Tell if a `Node` has a single predecessor.
singlePred :: Graph gr => gr a b -> Node -> Bool
singlePred gr n
    | ([_], _, _, _) <- context gr n = True
    | otherwise = False

-- | Use this function to extract information about a `Node` that you
-- know is in a `Graph`.  It's like `match` from `Graph`, but it must
-- succeed.
forceMatch :: (Graph gr)
           => Node -> gr s b -> (Context s b, gr s b)
forceMatch node g = case match node g of (Just c, g') -> (c, g')
                                         _ -> panicDump node g
 where panicDump :: Graph gr => Node -> gr s b -> any
       panicDump k _g =
         panic $ "GHC.Data.Graph.Collapse failed to match node " ++ show k

-- | Rewrite the label of a given node.
updateNode :: DynGraph gr => (s -> s) -> Node -> gr s b -> gr s b
updateNode relabel node g = (preds, n, relabel this, succs) & g'
    where ((preds, n, this, succs), g') = forceMatch node g


-- | Test if a graph has but a single node.
singletonGraph :: Graph gr => gr a b -> Bool
singletonGraph g = case labNodes g of [_] -> True
                                      _ -> False


----------------  Supernodes ------------------------------------

-- | A "supernode" stands for a collection of one or more nodes (basic
-- blocks) that have been coalesced by the Hecht-Ullman algorithm.
-- A collection in a supernode constitutes a /reducible/ subgraph of a
-- control-flow graph.  (When an entire control-flow graph is collapsed
-- to a single supernode, the flow graph is reducible.)
--
-- The idea of node splitting is to collapse a control-flow graph down
-- to a single supernode, then materialize (``inflate'') the reducible
-- equivalent graph from that supernode.  The `Supernode` class
-- defines only the methods needed to collapse; rematerialization is
-- the responsibility of the client.
--
-- During the Hecht-Ullman algorithm, every supernode has a unique
-- entry point, which is given by `superLabel`.  But this invariant is
-- not guaranteed by the class methods and is not a law of the class.
-- The `mapLabels` function rewrites all labels that appear in a
-- supernode (both definitions and uses).  The `freshen` function
-- replaces every appearance of a /defined/ label with a fresh label.
-- (Appearances include both definitions and uses.)
--
-- Laws:
-- @
--    superLabel (n <> n') == superLabel n
--    blocks (n <> n') == blocks n `union` blocks n'
--    mapLabels f (n <> n') = mapLabels f n <> mapLabels f n'
--    mapLabels id == id
--    mapLabels (f . g) == mapLabels f . mapLabels g
-- @
--
-- (We expect `freshen` to distribute over `<>`, but because of
-- the fresh names involved, formulating a precise law is a bit
-- challenging.)

class (Semigroup node) => PureSupernode node where
  superLabel :: node -> Label
  mapLabels :: (Label -> Label) -> (node -> node)

class (MonadGetUnique m, PureSupernode node) => Supernode node m where
  freshen :: node -> m node

  -- ghost method
  -- blocks :: node -> Set Block

------------------ Functions specific to the algorithm -----------------------

-- | Merge two nodes, return new graph plus list of nodes that newly have a single
-- predecessor.  This function implements transformation $T_2$ from
-- the Hecht and Ullman paper (merge the node into its unique
-- predecessor).  It then also removes self-edges (transformation $T_1$ from
-- the Hecht and Ullman paper).  There is no need for a separate
-- implementation of $T_1$.
--
-- `consumeBy v u g` returns the graph that results when node v is
-- consumed by node u in graph g.  Both v and u are replaced with a new node u'
-- with these properties:
--
--    LABELS(u') = LABELS(u) `union` LABELS(v)
--    SUCC(u') = SUCC(u) `union` SUCC(v) - { u }
--    every node that previously points to u now points to u'
--
-- It also returns a list of nodes in the result graph that
-- are *newly* single-predecessor nodes.

consumeBy :: (DynGraph gr, PureSupernode s)
          => Node -> Node -> gr s () -> (gr s (), [Node])
consumeBy toNode fromNode g =
    assert (toPreds == [((), fromNode)]) $
    (newGraph, newCandidates)
  where ((toPreds,   _, to,   toSuccs),   g')  = forceMatch toNode   g
        ((fromPreds, _, from, fromSuccs), g'') = forceMatch fromNode g'
        context = ( fromPreds -- by construction, can't have `toNode`
                  , fromNode
                  , from <> to
                  , delete ((), fromNode) toSuccs `union` fromSuccs
                  )
        newGraph = context & g''
        newCandidates = filter (singlePred newGraph) changedNodes
        changedNodes = fromNode `insert` map snd (toSuccs `intersect` fromSuccs)

-- | Split a given node.  The node is replaced with a collection of replicas,
-- one for each predecessor.  After the split, every predecessor
-- points to a unique replica.
split :: forall gr s b m . (DynGraph gr, Supernode s m)
      => Node -> gr s b -> m (gr s b)
split node g = assert (isMultiple preds) $ foldM addReplica g' newNodes
  where ((preds, _, this, succs), g') = forceMatch node g
        newNodes :: [((b, Node), Node)]
        newNodes = zip preds [maxNode+1..]
        (_, maxNode) = nodeRange g
        thisLabel = superLabel this
        addReplica :: gr s b -> ((b, Node), Node) -> m (gr s b)
        addReplica g ((b, pred), newNode) = do
            newSuper <- freshen this
            return $ add newSuper
          where add newSuper =
                  updateNode (thisLabel `replacedWith` superLabel newSuper) pred $
                  ([(b, pred)], newNode, newSuper, succs) & g

replacedWith :: PureSupernode s => Label -> Label -> s -> s
replacedWith old new = mapLabels (\l -> if l == old then new else l)


-- | Does a list have more than one element? (in constant time).
isMultiple :: [a] -> Bool
isMultiple [] = False
isMultiple [_] = False
isMultiple (_:_:_) = True

-- | Find a candidate for splitting by finding a node that has multiple predecessors.

anySplittable :: forall gr a b . Graph gr => gr a b -> LNode a
anySplittable g = case splittable of
                    n : _ -> n
                    [] -> panic "anySplittable found no splittable nodes"
  where splittable = filter (isMultiple . pre g . fst) $ labNodes g
        splittable :: [LNode a]


------------------ The collapsing algorithm -----------------------

-- | Using the algorithm of Hecht and Ullman (1972), collapse a graph
-- into a single node, splitting nodes as needed.  Record
-- visualization events in monad `m`.
collapseInductiveGraph :: (DynGraph gr, Supernode s m, VizCollapseMonad m gr s)
                       => gr s () -> m (gr s ())
collapseInductiveGraph g = drain g worklist
  where worklist :: [[Node]] -- nodes with exactly one predecessor
        worklist = [filter (singlePred g) $ nodes g]

        drain g [] = if singletonGraph g then finalGraph g >> return g
                     else let (n, super) = anySplittable g
                          in  do splitGraphAt g (n, super)
                                 collapseInductiveGraph =<< split n g
        drain g ([]:nss) = drain g nss
        drain g ((n:ns):nss) = let (g', ns') = consumeBy n (theUniquePred n) g
                               in  do consumeByInGraph n (theUniquePred n) g
                                      drain g' (ns':ns:nss)
         where theUniquePred n
                 | ([(_, p)], _, _, _) <- context g n = p
                 | otherwise =
                     panic "node claimed to have a unique predecessor; it doesn't"
