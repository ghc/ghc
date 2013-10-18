{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module CmmContFlowOpt
    ( cmmCfgOpts
    , cmmCfgOptsProc
    , removeUnreachableBlocksProc
    , removeUnreachableBlocks
    , replaceLabels
    )
where

import Hoopl
import BlockId
import Cmm
import CmmUtils
import Maybes

import Control.Monad
import Prelude hiding (succ, unzip, zip)

-----------------------------------------------------------------------------
--
-- Control-flow optimisations
--
-----------------------------------------------------------------------------

cmmCfgOpts :: Bool -> CmmGraph -> CmmGraph
cmmCfgOpts split g = fst (blockConcat split g)

cmmCfgOptsProc :: Bool -> CmmDecl -> CmmDecl
cmmCfgOptsProc split (CmmProc info lbl live g) = CmmProc info' lbl live g'
    where (g', env) = blockConcat split g
          info' = info{ info_tbls = new_info_tbls }
          new_info_tbls = mapFromList (map upd_info (mapToList (info_tbls info)))

          -- If we changed any labels, then we have to update the info tables
          -- too, except for the top-level info table because that might be
          -- referred to by other procs.
          upd_info (k,info)
             | Just k' <- mapLookup k env
             = (k', if k' == g_entry g'
                       then info
                       else info{ cit_lbl = infoTblLbl k' })
             | otherwise
             = (k,info)

cmmCfgOptsProc _ top = top


-----------------------------------------------------------------------------
--
-- Block concatenation
--
-----------------------------------------------------------------------------

-- This optimisation does three things:
--
--   - If a block finishes with an unconditional branch, then we may
--     be able to duplicate the block it points to and remove the
--     branch.  We do this if either
--        a) the destination block is small (e.g. just another branch), or
--        b) this is the only jump to this particular destination block.
--
--   - If a block finishes in a call whose continuation block is a
--     goto, then we can shortcut the destination, making the
--     destination of the goto into the continuation.  E.g.
--             call g returns to L    ==>     call g returns to M
--          L: goto M                      M: ...blah...
--          M: ...blah...
--     (but see Note [shortcut call returns])
--
--   - Remove any unreachable blocks from the graph.  This is a side
--     effect of starting with a postorder DFS traversal of the graph

-- Both transformations are improved by working from the end of the
-- graph towards the beginning, because we may be able to perform many
-- shortcuts in one go.

-- We need to walk over the blocks from the end back to the
-- beginning.  We are going to maintain the "current" graph
-- (BlockEnv CmmBlock) as we go, and also a mapping from BlockId
-- to BlockId, representing continuation labels that we have
-- renamed.  This latter mapping is important because we might
-- shortcut a CmmCall continuation.  For example:
--
--    Sp[0] = L
--    call g returns to L
--
--    L: goto M
--
--    M: ...
--
-- So when we shortcut the L block, we need to replace not only
-- the continuation of the call, but also references to L in the
-- code (e.g. the assignment Sp[0] = L).  So we keep track of
-- which labels we have renamed and apply the mapping at the end
-- with replaceLabels.

blockConcat :: Bool -> CmmGraph -> (CmmGraph, BlockEnv BlockId)
blockConcat splitting_procs g@CmmGraph { g_entry = entry_id }
  = (replaceLabels shortcut_map $ ofBlockMap new_entry new_blocks, shortcut_map')
  where
     -- we might be able to shortcut the entry BlockId itself.
     -- remember to update the shortcut_map', since we also have to
     -- update the info_tbls mapping now.
     (new_entry, shortcut_map')
       | Just entry_blk <- mapLookup entry_id new_blocks
       , Just dest      <- canShortcut entry_blk
       = (dest, mapInsert entry_id dest shortcut_map)
       | otherwise
       = (entry_id, shortcut_map)

     blocks = postorderDfs g
     blockmap = foldr addBlock emptyBody blocks
      -- the initial blockmap is constructed from the postorderDfs result,
      -- so that we automatically throw away unreachable blocks.

     (new_blocks, shortcut_map) =
           foldr maybe_concat (blockmap, mapEmpty) blocks

     maybe_concat :: CmmBlock
                  -> (BlockEnv CmmBlock, BlockEnv BlockId)
                  -> (BlockEnv CmmBlock, BlockEnv BlockId)
     maybe_concat block (blocks, shortcut_map)
        | CmmBranch b' <- last
        , Just blk' <- mapLookup b' blocks
        , shouldConcatWith b' blk'
        = (mapInsert bid (splice head blk') blocks, shortcut_map)

        -- calls: if we can shortcut the continuation label, then
        -- we must *also* remember to substitute for the label in the
        -- code, because we will push it somewhere.
        | splitting_procs -- Note [shortcut call returns]
        , Just b'   <- callContinuation_maybe last
        , Just blk' <- mapLookup b' blocks
        , Just dest <- canShortcut blk'
        = (blocks, mapInsert b' dest shortcut_map)
           -- replaceLabels will substitute dest for b' everywhere, later

        -- non-calls: see if we can shortcut any of the successors,
        -- and check whether we should invert the conditional
        | Nothing <- callContinuation_maybe last
        = ( mapInsert bid (blockJoinTail head swapcond_last) blocks
          , shortcut_map )

        | otherwise
        = (blocks, shortcut_map)
        where
          (head, last) = blockSplitTail block
          bid = entryLabel block

          shortcut_last = mapSuccessors shortcut last
            where
              shortcut l =
                 case mapLookup l blocks of
                   Just b | Just dest <- canShortcut b  -> dest
                   _otherwise -> l

          -- for a conditional, we invert the conditional if that
          -- would make it more likely that the branch-not-taken case
          -- becomes a fallthrough.  This helps the native codegen a
          -- little bit, and probably has no effect on LLVM.  It's
          -- convenient to do it here, where we have the information
          -- about predecessors.
          --
          swapcond_last
            | CmmCondBranch cond t f <- shortcut_last
            , numPreds f > 1
            , numPreds t == 1
            , Just cond' <- maybeInvertCmmExpr cond
            = CmmCondBranch cond' f t

            | otherwise
            = shortcut_last


     shouldConcatWith b block
       | okToDuplicate block = True  -- short enough to duplicate
       | numPreds b == 1     = True  -- only one predecessor: go for it
       | otherwise           = False

     numPreds bid = mapLookup bid backEdges `orElse` 0

     canShortcut :: CmmBlock -> Maybe BlockId
     canShortcut block
       | (_, middle, CmmBranch dest) <- blockSplit block
       , isEmptyBlock middle
       = Just dest
       | otherwise
       = Nothing

     backEdges :: BlockEnv Int -- number of predecessors for each block
     backEdges = mapInsertWith (+) entry_id 1 $ -- add 1 for the entry id
                   predMap blocks

     splice :: Block CmmNode C O -> CmmBlock -> CmmBlock
     splice head rest = head `blockAppend` snd (blockSplitHead rest)


callContinuation_maybe :: CmmNode O C -> Maybe BlockId
callContinuation_maybe (CmmCall { cml_cont = Just b }) = Just b
callContinuation_maybe (CmmForeignCall { succ = b })   = Just b
callContinuation_maybe _ = Nothing

okToDuplicate :: CmmBlock -> Bool
okToDuplicate block
  = case blockSplit block of
      (_, m, CmmBranch _) -> isEmptyBlock m
      -- cheap and cheerful; we might expand this in the future to
      -- e.g. spot blocks that represent a single instruction or two.
      -- Be careful: a CmmCall can be more than one instruction, it
      -- has a CmmExpr inside it.
      _otherwise -> False


{-  Note [shortcut call returns]

Consider this code that you might get from a recursive let-no-escape:

      goto L1
     L1:
      if (Hp > HpLim) then L2 else L3
     L2:
      call stg_gc_noregs returns to L4
     L4:
      goto L1
     L3:
      ...
      goto L1

Then the control-flow optimiser shortcuts L4.  But that turns L1
into the call-return proc point, and every iteration of the loop
has to shuffle variables to and from the stack.  So we must *not*
shortcut L4.

Moreover not shortcutting call returns is probably fine.  If L4 can
concat with its branch target then it will still do so.  And we
save some compile time because we don't have to traverse all the
code in replaceLabels.

However, we probably do want to do this if we are splitting proc
points, because L1 will be a proc-point anyway, so merging it with L4
reduces the number of proc points.  Unfortunately recursive
let-no-escapes won't generate very good code with proc-point splitting
on - we should probably compile them to explicitly use the native
calling convention instead.
-}

------------------------------------------------------------------------
-- Map over the CmmGraph, replacing each label with its mapping in the
-- supplied BlockEnv.

replaceLabels :: BlockEnv BlockId -> CmmGraph -> CmmGraph
replaceLabels env g
  | mapNull env = g
  | otherwise   = replace_eid $ mapGraphNodes1 txnode g
   where
     replace_eid g = g {g_entry = lookup (g_entry g)}
     lookup id = mapLookup id env `orElse` id

     txnode :: CmmNode e x -> CmmNode e x
     txnode (CmmBranch bid)         = CmmBranch (lookup bid)
     txnode (CmmCondBranch p t f)   = mkCmmCondBranch (exp p) (lookup t) (lookup f)
     txnode (CmmSwitch e arms)      = CmmSwitch (exp e) (map (liftM lookup) arms)
     txnode (CmmCall t k rg a res r) = CmmCall (exp t) (liftM lookup k) rg a res r
     txnode fc@CmmForeignCall{}     = fc{ args = map exp (args fc)
                                        , succ = lookup (succ fc) }
     txnode other                   = mapExpDeep exp other

     exp :: CmmExpr -> CmmExpr
     exp (CmmLit (CmmBlock bid))                = CmmLit (CmmBlock (lookup bid))
     exp (CmmStackSlot (Young id) i) = CmmStackSlot (Young (lookup id)) i
     exp e                                      = e

mkCmmCondBranch :: CmmExpr -> Label -> Label -> CmmNode O C
mkCmmCondBranch p t f = if t == f then CmmBranch t else CmmCondBranch p t f

----------------------------------------------------------------
-- Build a map from a block to its set of predecessors. Very useful.

predMap :: [CmmBlock] -> BlockEnv Int
predMap blocks = foldr add_preds mapEmpty blocks -- find the back edges
  where
    add_preds block env = foldr add env (successors block)
      where add lbl env = mapInsertWith (+) lbl 1 env

-----------------------------------------------------------------------------
--
-- Removing unreachable blocks

removeUnreachableBlocksProc :: CmmDecl -> CmmDecl
removeUnreachableBlocksProc (CmmProc info lbl live g)
   = CmmProc info lbl live (removeUnreachableBlocks g)

removeUnreachableBlocks :: CmmGraph -> CmmGraph
removeUnreachableBlocks g
  | length blocks < mapSize (toBlockMap g) = ofBlockList (g_entry g) blocks
  | otherwise = g
  where blocks = postorderDfs g
