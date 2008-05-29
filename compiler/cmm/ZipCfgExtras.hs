{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

-- This module contains code related to the zipcfg representation.
-- The code either has been used or has been thought to be useful
-- within the Quick C-- compiler, but as yet no use has been found for
-- it within GHC.  This module should therefore be considered to be
-- full of code that need not be maintained.  Should a function in
-- this module prove useful, it should not be exported, but rather
-- should be migrated back into ZipCfg (or possibly ZipCfgUtil), where
-- it can be maintained.

module ZipCfgExtras
  ()
where
import BlockId
import Maybes
import Panic
import ZipCfg

import Prelude hiding (zip, unzip, last)


exit    :: LGraph m l -> FGraph m l         -- focus on edge into default exit node 
                                            -- (fails if there isn't one)
focusp  :: (Block m l -> Bool) -> LGraph m l -> Maybe (FGraph m l)
                                      -- focus on start of block satisfying predicate
unfocus :: FGraph m l -> LGraph m l            -- lose focus 

-- | We can insert a single-entry, single-exit subgraph at
-- the current focus.
-- The new focus can be at either the entry edge or the exit edge.

{-
splice_focus_entry :: FGraph m l -> LGraph m l -> FGraph m l
splice_focus_exit  :: FGraph m l -> LGraph m l -> FGraph m l
-}

_unused :: ()
_unused = all `seq` ()
    where all = ( exit, focusp, unfocus {- , splice_focus_entry, splice_focus_exit -}
                , foldM_fwd_block (\_ a -> Just a)
                )

unfocus (FGraph e bz bs) = LGraph e (insertBlock (zip bz) bs)

focusp p (LGraph entry blocks) =
    fmap (\(b, bs) -> FGraph entry (unzip b) bs) (splitp_blocks p blocks)

exit g@(LGraph eid _) = FGraph eid (ZBlock h (ZLast l)) others
    where FGraph _ b others = focusp is_exit g `orElse` panic "no exit in flow graph"
          (h, l) = goto_end b


{-
splice_focus_entry (FGraph eid (ZBlock head tail) blocks) g =
  let (tail', g') = splice_tail g tail in
  FGraph eid (ZBlock head tail') (plusUFM (lg_blocks g') blocks)

splice_focus_exit (FGraph eid (ZBlock head tail) blocks) g =
  let (g', head') = splice_head head g in
  FGraph eid (ZBlock head' tail) (plusUFM (lg_blocks g') blocks)
-}

-- | iterate from first to last
foldM_fwd_block ::
  Monad m => (BlockId -> a -> m a) -> (mid -> a -> m a) -> (ZLast l -> a -> m a) ->
             Block mid l -> a -> m a
foldM_fwd_block first middle last (Block id t) z = do { z <- first id z; tail t z }
    where tail (ZTail m t) z = do { z <- middle m z; tail t z }
          tail (ZLast l)   z = last l z

splitp_blocks :: (Block m l -> Bool) -> BlockEnv (Block m l) ->
                 Maybe (Block m l, BlockEnv (Block m l))
splitp_blocks = undefined -- implemented in ZipCfg but not exported
is_exit :: Block m l -> Bool
is_exit = undefined -- implemented in ZipCfg but not exported
