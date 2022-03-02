{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.Cmm.Dataflow.Graph
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
    , revPostorderFrom
    ) where


import GHC.Prelude
import GHC.Utils.Misc

import GHC.Cmm.Dataflow.Label
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Collections

import Data.Kind

-- | A (possibly empty) collection of closed/closed blocks
type Body n = LabelMap (Block n C C)

-- | @Body@ abstracted over @block@
type Body' block (n :: Extensibility -> Extensibility -> Type) = LabelMap (block n C C)

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

addBlock
    :: (NonLocal block, HasDebugCallStack)
    => block C C -> LabelMap (block C C) -> LabelMap (block C C)
addBlock block body = mapAlter add lbl body
  where
    lbl = entryLabel block
    add Nothing = Just block
    add _ = error $ "duplicate label " ++ show lbl ++ " in graph"


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
data Graph' block (n :: Extensibility -> Extensibility -> Type) e x where
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

-- | Returns a list of blocks reachable from the provided Labels in the reverse
-- postorder.
--
-- This is the most important traversal over this data structure.  It drops
-- unreachable code and puts blocks in an order that is good for solving forward
-- dataflow problems quickly.  The reverse order is good for solving backward
-- dataflow problems quickly.  The forward order is also reasonably good for
-- emitting instructions, except that it will not usually exploit Forrest
-- Baskett's trick of eliminating the unconditional branch from a loop.  For
-- that you would need a more serious analysis, probably based on dominators, to
-- identify loop headers.
--
-- For forward analyses we want reverse postorder visitation, consider:
-- @
--      A -> [B,C]
--      B -> D
--      C -> D
-- @
-- Postorder: [D, C, B, A] (or [D, B, C, A])
-- Reverse postorder: [A, B, C, D] (or [A, C, B, D])
-- This matters for, e.g., forward analysis, because we want to analyze *both*
-- B and C before we analyze D.
revPostorderFrom
  :: forall block.  (NonLocal block)
  => LabelMap (block C C) -> Label -> [block C C]
revPostorderFrom graph start = go start_worklist setEmpty []
  where
    start_worklist = lookup_for_descend start Nil

    -- To compute the postorder we need to "visit" a block (mark as done) *after*
    -- visiting all its successors. So we need to know whether we already
    -- processed all successors of each block (and @NonLocal@ allows arbitrary
    -- many successors). So we use an explicit stack with an extra bit
    -- of information:
    -- - @ConsTodo@ means to explore the block if it wasn't visited before
    -- - @ConsMark@ means that all successors were already done and we can add
    --   the block to the result.
    --
    -- NOTE: We add blocks to the result list in postorder, but we *prepend*
    -- them (i.e., we use @(:)@), which means that the final list is in reverse
    -- postorder.
    go :: DfsStack (block C C) -> LabelSet -> [block C C] -> [block C C]
    go Nil                      !_           !result = result
    go (ConsMark block rest)    !wip_or_done !result =
        go rest wip_or_done (block : result)
    go (ConsTodo block rest)    !wip_or_done !result
        | entryLabel block `setMember` wip_or_done = go rest wip_or_done result
        | otherwise =
            let new_worklist =
                    foldr lookup_for_descend
                          (ConsMark block rest)
                          (successors block)
            in go new_worklist (setInsert (entryLabel block) wip_or_done) result

    lookup_for_descend :: Label -> DfsStack (block C C) -> DfsStack (block C C)
    lookup_for_descend label wl
      | Just b <- mapLookup label graph = ConsTodo b wl
      | otherwise =
           error $ "Label that doesn't have a block?! " ++ show label

data DfsStack a = ConsTodo a (DfsStack a) | ConsMark a (DfsStack a) | Nil
