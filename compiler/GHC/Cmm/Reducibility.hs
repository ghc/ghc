{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : GHC.Cmm.Reducibility
Description : Tell if a `CmmGraph` is reducible, or make it so

Test a Cmm control-flow graph for reducibility.  And provide a
function that, when given an arbitrary control-flow graph, returns an
equivalent, reducible control-flow graph.  The equivalent graph is
obtained by "splitting" (copying) nodes of the original graph.
The resulting equivalent graph has the same dynamic behavior as the
original, but it is larger.

Documentation uses the language of control-flow analysis, in which a
basic block is called a "node."  These "nodes" are `CmmBlock`s or
equivalent; they have nothing to do with a `CmmNode`.

For more on reducibility and related analyses and algorithms, see
Note [Reducibility resources]
-}

module GHC.Cmm.Reducibility
  ( Reducibility(..)
  , reducibility

  , asReducible
  )
where

import GHC.Prelude hiding (splitAt, succ)

import Control.Monad
import Data.List (nub)
import Data.Maybe
import Data.Semigroup
import qualified Data.Sequence as Seq

import GHC.Cmm
import GHC.Cmm.BlockId
import GHC.Cmm.Dataflow
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dominators
import GHC.Cmm.Dataflow.Graph hiding (addBlock)
import GHC.Cmm.Dataflow.Label
import GHC.Data.Graph.Collapse
import GHC.Data.Graph.Inductive.Graph
import GHC.Data.Graph.Inductive.PatriciaTree
import GHC.Types.Unique.DSM
import GHC.Utils.Panic

-- | Represents the result of a reducibility analysis.
data Reducibility = Reducible | Irreducible
  deriving (Eq, Show)

-- | Given a graph, say whether the graph is reducible.  The graph must
-- be bundled with a dominator analysis and a reverse postorder
-- numbering, as these results are needed to perform the test.

reducibility :: NonLocal node
             => GraphWithDominators node
             -> Reducibility
reducibility gwd =
    if all goodBlock blockmap then Reducible else Irreducible
  where goodBlock b = all (goodEdge (entryLabel b)) (successors b)
        goodEdge from to = rpnum to > rpnum from || to `dominates` from
        rpnum = gwdRPNumber gwd
        blockmap = graphMap $ gwd_graph gwd
        dominators = gwdDominatorsOf gwd
        dominates lbl blockname =
            lbl == blockname || dominatorsMember lbl (dominators blockname)

-- | Given a graph, return an equivalent reducible graph, by
-- "splitting" (copying) nodes if necessary.  The input
-- graph must be bundled with a dominator analysis and a reverse
-- postorder numbering.  The computation is monadic because when a
-- node is split, the new copy needs a fresh label.
--
-- Use this function whenever a downstream algorithm needs a reducible
-- control-flow graph.

asReducible :: GraphWithDominators CmmNode
            -> UniqDSM (GraphWithDominators CmmNode)
asReducible gwd = case reducibility gwd of
                    Reducible -> return gwd
                    Irreducible -> assertReducible <$> nodeSplit gwd

assertReducible :: GraphWithDominators CmmNode -> GraphWithDominators CmmNode
assertReducible gwd = case reducibility gwd of
                        Reducible -> gwd
                        Irreducible -> panic "result not reducible"

----------------------------------------------------------------

-- | Split one or more nodes of the given graph, which must be
-- irreducible.

nodeSplit :: GraphWithDominators CmmNode
          -> UniqDSM (GraphWithDominators CmmNode)
nodeSplit gwd =
    graphWithDominators <$> inflate (g_entry g) <$> runNullCollapse collapsed
  where g = gwd_graph gwd
        collapsed :: NullCollapseViz (Gr CmmSuper ())
        collapsed = collapseInductiveGraph (cgraphOfCmm g)

type CGraph = Gr CmmSuper ()

-- | Turn a collapsed supernode back into a control-flow graph
inflate :: Label -> CGraph -> CmmGraph
inflate entry cg = CmmGraph entry graph
  where graph = GMany NothingO body NothingO
        body :: LabelMap CmmBlock
        body = foldl (\map block -> mapInsert (entryLabel block) block map) mapEmpty $
               blocks super
        super = case labNodes cg of
                  [(_, s)] -> s
                  _ -> panic "graph given to `inflate` is not singleton"


-- | Convert a `CmmGraph` into an inductive graph.
-- (The function coalesces duplicate edges into a single edge.)
cgraphOfCmm :: CmmGraph -> CGraph
cgraphOfCmm g = foldl' addSuccEdges (mkGraph cnodes []) blocks
   where blocks = zip [0..] $ revPostorderFrom (graphMap g) (g_entry g)
         cnodes = [(k, super block) | (k, block) <- blocks]
          where super block = Nodes (entryLabel block) (Seq.singleton block)
         labelNumber = \lbl -> fromJust $ mapLookup lbl numbers
             where numbers :: LabelMap Int
                   numbers = mapFromList $ map swap blocks
                   swap (k, block) = (entryLabel block, k)
         addSuccEdges :: CGraph -> (Node, CmmBlock) -> CGraph
         addSuccEdges graph (k, block) =
             insEdges [(k, labelNumber lbl, ()) | lbl <- nub $ successors block] graph
{-
Note [Reducibility resources]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*Flow Analysis of Computer Programs.* Matthew S. Hecht North Holland, 1977.
Available to borrow from archive.org.

Matthew S. Hecht and Jeffrey D. Ullman (1972).
Flow Graph Reducibility. SIAM J. Comput., 1(2), 188–202.
https://doi.org/10.1137/0201014

Johan Janssen and Henk Corporaal. 1997. Making graphs reducible with
controlled node splitting. ACM TOPLAS 19, 6 (Nov. 1997),
1031–1052. DOI:https://doi.org/10.1145/267959.269971

Sebastian Unger and Frank Mueller. 2002. Handling irreducible loops:
optimized node splitting versus DJ-graphs. ACM TOPLAS 24, 4 (July
2002), 299–333. https://doi.org/10.1145/567097.567098.  (This one
contains the most detailed account of how the Hecht/Ullman algorithm
is used to modify an actual control-flow graph.  But still not much detail.)

https://rgrig.blogspot.com/2009/10/dtfloatleftclearleft-summary-of-some.html
 (Nice summary of useful facts)

-}



type Seq = Seq.Seq

-- | A "supernode" contains a single-entry, multiple-exit, reducible subgraph.
-- The entry point is the given label, and the block with that label
-- dominates all the other blocks in the supernode.  When an entire
-- graph is collapsed into a single supernode, the graph is reducible.
-- More detail can be found in "GHC.Data.Graph.Collapse".

data CmmSuper
    = Nodes { label :: Label
            , blocks :: Seq CmmBlock
            }

instance Semigroup CmmSuper where
  s <> s' = Nodes (label s) (blocks s <> blocks s')

instance PureSupernode CmmSuper where
  superLabel = label
  mapLabels = changeLabels

instance Supernode CmmSuper NullCollapseViz where
  freshen s = liftUniqDSM $ relabel s


-- | Return all labels defined within a supernode.
definedLabels :: CmmSuper -> Seq Label
definedLabels = fmap entryLabel . blocks



-- | Map the given function over every use and definition of a label
-- in the given supernode.
changeLabels :: (Label -> Label) -> (CmmSuper -> CmmSuper)
changeLabels f (Nodes l blocks) = Nodes (f l) (fmap (changeBlockLabels f) blocks)

-- | Map the given function over every use and definition of a label
-- in the given block.
changeBlockLabels :: (Label -> Label) -> CmmBlock -> CmmBlock
changeBlockLabels f block = blockJoin entry' middle exit'
  where (entry, middle, exit) = blockSplit block
        entry' = let CmmEntry l scope = entry
                 in  CmmEntry (f l) scope
        exit' = case exit of
                  -- unclear why mapSuccessors doesn't touch these
                  CmmCall { cml_cont = Just l } -> exit { cml_cont = Just (f l) }
                  CmmForeignCall { succ = l } -> exit { succ = f l }
                  _ -> mapSuccessors f exit


-- | Within the given supernode, replace every defined label (and all
-- of its uses) with a fresh label.

relabel :: CmmSuper -> UniqDSM CmmSuper
relabel node = do
     finite_map <- foldM addPair mapEmpty $ definedLabels node
     return $ changeLabels (labelChanger finite_map) node
  where addPair :: LabelMap Label -> Label -> UniqDSM (LabelMap Label)
        addPair map old = do new <- newBlockId
                             return $ mapInsert old new map
        labelChanger :: LabelMap Label -> (Label -> Label)
        labelChanger mapping = \lbl -> mapFindWithDefault lbl lbl mapping
