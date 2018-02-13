{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Hoopl.Graph
    ( Body
    , Graph
    , Graph'(..)
    , NonLocal(..)
    , addBlock
    , bodyList
    , emptyBody
    , labelsDefined
    , mapGraph
    , mapGraphBlocks
    , postorder_dfs_from
    ) where


import GhcPrelude

import Hoopl.Label
import Hoopl.Block
import Hoopl.Collections

-- | A (possibly empty) collection of closed/closed blocks
type Body n = LabelMap (Block n C C)

-- | @Body@ abstracted over @block@
type Body' block (n :: * -> * -> *) = LabelMap (block n C C)

-------------------------------
-- | Gives access to the anchor points for
-- nonlocal edges as well as the edges themselves
class NonLocal thing where
  entryLabel :: thing C x -> Label   -- ^ The label of a first node or block
  successors :: thing e C -> [Label] -- ^ Gives control-flow successors

instance NonLocal n => NonLocal (Block n) where
  entryLabel (BlockCO f _)   = entryLabel f
  entryLabel (BlockCC f _ _) = entryLabel f

  successors (BlockOC   _ n) = successors n
  successors (BlockCC _ _ n) = successors n


emptyBody :: Body' block n
emptyBody = mapEmpty

bodyList :: Body' block n -> [(Label,block n C C)]
bodyList body = mapToList body

addBlock :: NonLocal thing
         => thing C C -> LabelMap (thing C C)
         -> LabelMap (thing C C)
addBlock b body
  | mapMember lbl body = error $ "duplicate label " ++ show lbl ++ " in graph"
  | otherwise          = mapInsert lbl b body
  where lbl = entryLabel b


-- ---------------------------------------------------------------------------
-- Graph

-- | A control-flow graph, which may take any of four shapes (O/O,
-- O/C, C/O, C/C).  A graph open at the entry has a single,
-- distinguished, anonymous entry point; if a graph is closed at the
-- entry, its entry point(s) are supplied by a context.
type Graph = Graph' Block

-- | @Graph'@ is abstracted over the block type, so that we can build
-- graphs of annotated blocks for example (Compiler.Hoopl.Dataflow
-- needs this).
data Graph' block (n :: * -> * -> *) e x where
  GNil  :: Graph' block n O O
  GUnit :: block n O O -> Graph' block n O O
  GMany :: MaybeO e (block n O C)
        -> Body' block n
        -> MaybeO x (block n C O)
        -> Graph' block n e x


-- -----------------------------------------------------------------------------
-- Mapping over graphs

-- | Maps over all nodes in a graph.
mapGraph :: (forall e x. n e x -> n' e x) -> Graph n e x -> Graph n' e x
mapGraph f = mapGraphBlocks (mapBlock f)

-- | Function 'mapGraphBlocks' enables a change of representation of blocks,
-- nodes, or both.  It lifts a polymorphic block transform into a polymorphic
-- graph transform.  When the block representation stabilizes, a similar
-- function should be provided for blocks.
mapGraphBlocks :: forall block n block' n' e x .
                  (forall e x . block n e x -> block' n' e x)
               -> (Graph' block n e x -> Graph' block' n' e x)

mapGraphBlocks f = map
  where map :: Graph' block n e x -> Graph' block' n' e x
        map GNil = GNil
        map (GUnit b) = GUnit (f b)
        map (GMany e b x) = GMany (fmap f e) (mapMap f b) (fmap f x)

-- -----------------------------------------------------------------------------
-- Extracting Labels from graphs

labelsDefined :: forall block n e x . NonLocal (block n) => Graph' block n e x
              -> LabelSet
labelsDefined GNil      = setEmpty
labelsDefined (GUnit{}) = setEmpty
labelsDefined (GMany _ body x) = mapFoldlWithKey addEntry (exitLabel x) body
  where addEntry :: forall a. LabelSet -> ElemOf LabelSet -> a -> LabelSet
        addEntry labels label _ = setInsert label labels
        exitLabel :: MaybeO x (block n C O) -> LabelSet
        exitLabel NothingO  = setEmpty
        exitLabel (JustO b) = setSingleton (entryLabel b)


----------------------------------------------------------------

class LabelsPtr l where
  targetLabels :: l -> [Label]

instance NonLocal n => LabelsPtr (n e C) where
  targetLabels n = successors n

instance LabelsPtr Label where
  targetLabels l = [l]

instance LabelsPtr LabelSet where
  targetLabels = setElems

instance LabelsPtr l => LabelsPtr [l] where
  targetLabels = concatMap targetLabels

-- | This is the most important traversal over this data structure.  It drops
-- unreachable code and puts blocks in an order that is good for solving forward
-- dataflow problems quickly.  The reverse order is good for solving backward
-- dataflow problems quickly.  The forward order is also reasonably good for
-- emitting instructions, except that it will not usually exploit Forrest
-- Baskett's trick of eliminating the unconditional branch from a loop.  For
-- that you would need a more serious analysis, probably based on dominators, to
-- identify loop headers.
--
-- The ubiquity of 'postorder_dfs' is one reason for the ubiquity of the 'LGraph'
-- representation, when for most purposes the plain 'Graph' representation is
-- more mathematically elegant (but results in more complicated code).
--
-- Here's an easy way to go wrong!  Consider
-- @
--      A -> [B,C]
--      B -> D
--      C -> D
-- @
-- Then ordinary dfs would give [A,B,D,C] which has a back ref from C to D.
-- Better to get [A,B,C,D]


-- | Traversal: 'postorder_dfs' returns a list of blocks reachable
-- from the entry of enterable graph. The entry and exit are *not* included.
-- The list has the following property:
--
--      Say a "back reference" exists if one of a block's
--      control-flow successors precedes it in the output list
--
--      Then there are as few back references as possible
--
-- The output is suitable for use in
-- a forward dataflow problem.  For a backward problem, simply reverse
-- the list.  ('postorder_dfs' is sufficiently tricky to implement that
-- one doesn't want to try and maintain both forward and backward
-- versions.)

postorder_dfs_from_except :: forall block e . (NonLocal block, LabelsPtr e)
                          => LabelMap (block C C) -> e -> LabelSet -> [block C C]
postorder_dfs_from_except blocks b visited =
 vchildren (get_children b) (\acc _visited -> acc) [] visited
 where
   vnode :: block C C -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vnode block cont acc visited =
        if setMember id visited then
            cont acc visited
        else
            let cont' acc visited = cont (block:acc) visited in
            vchildren (get_children block) cont' acc (setInsert id visited)
      where id = entryLabel block
   vchildren :: forall a. [block C C] -> ([block C C] -> LabelSet -> a) -> [block C C] -> LabelSet -> a
   vchildren bs cont acc visited = next bs acc visited
      where next children acc visited =
                case children of []     -> cont acc visited
                                 (b:bs) -> vnode b (next bs) acc visited
   get_children :: forall l. LabelsPtr l => l -> [block C C]
   get_children block = foldr add_id [] $ targetLabels block
   add_id id rst = case lookupFact id blocks of
                      Just b -> b : rst
                      Nothing -> rst

postorder_dfs_from
    :: (NonLocal block, LabelsPtr b) => LabelMap (block C C) -> b -> [block C C]
postorder_dfs_from blocks b = postorder_dfs_from_except blocks b setEmpty
