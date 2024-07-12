{-# LANGUAGE GADTs #-}
module GHC.Cmm.ContFlowOpt
    ( cmmCfgOpts
    , cmmCfgOptsProc
    , removeUnreachableBlocksProc
    , replaceLabels
    )
where

import GHC.Prelude hiding (succ, unzip, zip)

import GHC.Cmm.Dataflow.Block hiding (blockConcat)
import GHC.Cmm.Dataflow.Graph
import qualified GHC.Cmm.Dataflow.Label as Det
import qualified GHC.Cmm.Dataflow.Label.NonDet as NonDet
import GHC.Cmm.BlockId
import GHC.Cmm
import GHC.Cmm.Utils
import GHC.Cmm.Switch (mapSwitchTargets, switchTargetsToList)
import GHC.Data.Maybe
import GHC.Platform
import GHC.Utils.Misc
import GHC.Utils.Outputable
import GHC.Utils.Panic

import Control.Monad


-- Note [What is shortcutting]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider this Cmm code:
--
-- L1: ...
--     goto L2;
-- L2: goto L3;
-- L3: ...
--
-- Here L2 is an empty block and contains only an unconditional branch
-- to L3. In this situation any block that jumps to L2 can jump
-- directly to L3:
--
-- L1: ...
--     goto L3;
-- L2: goto L3;
-- L3: ...
--
-- In this situation we say that we shortcut L2 to L3. One of
-- consequences of shortcutting is that some blocks of code may become
-- unreachable (in the example above this is true for L2).


-- Note [Control-flow optimisations]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- This optimisation does three things:
--
--   - If a block finishes in an unconditional branch to another block
--     and that is the only jump to that block we concatenate the
--     destination block at the end of the current one.
--
--   - If a block finishes in a call whose continuation block is a
--     goto, then we can shortcut the destination, making the
--     continuation block the destination of the goto - but see Note
--     [Shortcut call returns].
--
--   - For any block that is not a call we try to shortcut the
--     destination(s). Additionally, if a block ends with a
--     conditional branch we try to invert the condition.
--
-- Blocks are processed using postorder DFS traversal. A side effect
-- of determining traversal order with a graph search is elimination
-- of any blocks that are unreachable.
--
-- Transformations are improved by working from the end of the graph
-- towards the beginning, because we may be able to perform many
-- shortcuts in one go.


-- Note [Shortcut call returns]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We are going to maintain the "current" graph (LabelMap CmmBlock) as
-- we go, and also a mapping from BlockId to BlockId, representing
-- continuation labels that we have renamed.  This latter mapping is
-- important because we might shortcut a CmmCall continuation.  For
-- example:
--
--    Sp[0] = L
--    call g returns to L
--    L: goto M
--    M: ...
--
-- So when we shortcut the L block, we need to replace not only
-- the continuation of the call, but also references to L in the
-- code (e.g. the assignment Sp[0] = L):
--
--    Sp[0] = M
--    call g returns to M
--    M: ...
--
-- So we keep track of which labels we have renamed and apply the mapping
-- at the end with replaceLabels.


-- Note [Shortcut call returns and proc-points]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Consider this code that you might get from a recursive
-- let-no-escape:
--
--       goto L1
--      L1:
--       if (Hp > HpLim) then L2 else L3
--      L2:
--       call stg_gc_noregs returns to L4
--      L4:
--       goto L1
--      L3:
--       ...
--       goto L1
--
-- Then the control-flow optimiser shortcuts L4.  But that turns L1
-- into the call-return proc point, and every iteration of the loop
-- has to shuffle variables to and from the stack.  So we must *not*
-- shortcut L4.
--
-- Moreover not shortcutting call returns is probably fine.  If L4 can
-- concat with its branch target then it will still do so.  And we
-- save some compile time because we don't have to traverse all the
-- code in replaceLabels.
--
-- However, we probably do want to do this if we are splitting proc
-- points, because L1 will be a proc-point anyway, so merging it with
-- L4 reduces the number of proc points.  Unfortunately recursive
-- let-no-escapes won't generate very good code with proc-point
-- splitting on - we should probably compile them to explicitly use
-- the native calling convention instead.

cmmCfgOpts :: Bool -> CmmGraph -> CmmGraph
cmmCfgOpts split g = fst (blockConcat split g)

cmmCfgOptsProc :: Bool -> CmmDecl -> CmmDecl
cmmCfgOptsProc split (CmmProc info lbl live g) = CmmProc info' lbl live g'
    where (g', env) = blockConcat split g
          info' = info{ info_tbls = new_info_tbls }
          new_info_tbls = Det.mapFromList (map upd_info (Det.mapToList (info_tbls info)))

          -- If we changed any labels, then we have to update the info tables
          -- too, except for the top-level info table because that might be
          -- referred to by other procs.
          upd_info (k,info)
             | Just k' <- NonDet.mapLookup k env
             = (k', if k' == g_entry g'
                       then info
                       else info{ cit_lbl = infoTblLbl k' })
             | otherwise
             = (k,info)
cmmCfgOptsProc _ top = top


blockConcat :: Bool -> CmmGraph -> (CmmGraph, NonDet.LabelMap BlockId)
blockConcat splitting_procs g@CmmGraph { g_entry = entry_id }
  = (replaceLabels shortcut_map $ ofBlockMap new_entry new_blocks, shortcut_map')
  where
     -- We might be able to shortcut the entry BlockId itself.
     -- Remember to update the shortcut_map, since we also have to
     -- update the info_tbls mapping now.
     (new_entry, shortcut_map')
       | Just entry_blk <- Det.mapLookup entry_id new_blocks
       , Just dest      <- canShortcut entry_blk
       = (dest, NonDet.mapInsert entry_id dest shortcut_map)
       | otherwise
       = (entry_id, shortcut_map)

     -- blocks are sorted in reverse postorder, but we want to go from the exit
     -- towards beginning, so we use foldr below.
     blocks = revPostorder g
     blockmap = foldl' (flip addBlock) emptyBody blocks

     -- Accumulator contains three components:
     --  * map of blocks in a graph
     --  * map of shortcut labels. See Note [Shortcut call returns]
     --  * map containing number of predecessors for each block. We discard
     --    it after we process all blocks.
     (new_blocks, shortcut_map, _) =
           foldr maybe_concat (blockmap, NonDet.mapEmpty, initialBackEdges) blocks

     -- Map of predecessors for initial graph. We increase number of
     -- predecessors for entry block by one to denote that it is
     -- target of a jump, even if no block in the current graph jumps
     -- to it.
     initialBackEdges = incPreds entry_id (predMap blocks)

     maybe_concat :: CmmBlock
                  -> (Det.LabelMap CmmBlock, NonDet.LabelMap BlockId, NonDet.LabelMap Int)
                  -> (Det.LabelMap CmmBlock, NonDet.LabelMap BlockId, NonDet.LabelMap Int)
     maybe_concat block (!blocks, !shortcut_map, !backEdges)
        -- If:
        --   (1) current block ends with unconditional branch to b' and
        --   (2) it has exactly one predecessor (namely, current block)
        --
        -- Then:
        --   (1) append b' block at the end of current block
        --   (2) remove b' from the map of blocks
        --   (3) remove information about b' from predecessors map
        --
        -- Since we know that the block has only one predecessor we call
        -- mapDelete directly instead of calling decPreds.
        --
        -- Note that we always maintain an up-to-date list of predecessors, so
        -- we can ignore the contents of shortcut_map
        | CmmBranch b' <- last
        , hasOnePredecessor b'
        , Just blk' <- Det.mapLookup b' blocks
        = let bid' = entryLabel blk'
          in ( Det.mapDelete bid' $ Det.mapInsert bid (splice head blk') blocks
             , shortcut_map
             , NonDet.mapDelete b' backEdges )

        -- If:
        --   (1) we are splitting proc points (see Note
        --       [Shortcut call returns and proc-points]) and
        --   (2) current block is a CmmCall or CmmForeignCall with
        --       continuation b' and
        --   (3) we can shortcut that continuation to dest
        -- Then:
        --   (1) we change continuation to point to b'
        --   (2) create mapping from b' to dest
        --   (3) increase number of predecessors of dest by 1
        --   (4) decrease number of predecessors of b' by 1
        --
        -- Later we will use replaceLabels to substitute all occurrences of b'
        -- with dest.
        | splitting_procs
        , Just b'   <- callContinuation_maybe last
        , Just blk' <- Det.mapLookup b' blocks
        , Just dest <- canShortcut blk'
        = ( Det.mapInsert bid (blockJoinTail head (update_cont dest)) blocks
          , NonDet.mapInsert b' dest shortcut_map
          , decPreds b' $ incPreds dest backEdges )

        -- If:
        --   (1) a block does not end with a call
        -- Then:
        --   (1) if it ends with a conditional attempt to invert the
        --       conditional
        --   (2) attempt to shortcut all destination blocks
        --   (3) if new successors of a block are different from the old ones
        --       update the of predecessors accordingly
        --
        -- A special case of this is a situation when a block ends with an
        -- unconditional jump to a block that can be shortcut.
        | Nothing <- callContinuation_maybe last
        = let oldSuccs = successors last
              newSuccs = successors rewrite_last
          in ( Det.mapInsert bid (blockJoinTail head rewrite_last) blocks
             , shortcut_map
             , if oldSuccs == newSuccs
               then backEdges
               else foldr incPreds (foldr decPreds backEdges oldSuccs) newSuccs )

        -- Otherwise don't do anything
        | otherwise
        = ( blocks, shortcut_map, backEdges )
        where
          (head, last) = blockSplitTail block
          bid = entryLabel block

          -- Changes continuation of a call to a specified label
          update_cont dest =
              case last of
                CmmCall{}        -> last { cml_cont = Just dest }
                CmmForeignCall{} -> last { succ = dest }
                _                -> panic "Can't shortcut continuation."

          -- Attempts to shortcut successors of last node
          shortcut_last = mapSuccessors shortcut last
            where
              shortcut l =
                 case Det.mapLookup l blocks of
                   Just b | Just dest <- canShortcut b -> dest
                   _otherwise -> l

          rewrite_last
            -- Sometimes we can get rid of the conditional completely.
            | CmmCondBranch _cond t f _l <- shortcut_last
            , t == f
            = CmmBranch t

            -- See Note [Invert Cmm conditionals]
            | CmmCondBranch cond t f l <- shortcut_last
            , hasOnePredecessor t -- inverting will make t a fallthrough
            , likelyTrue l || (numPreds f > 1)
            , Just cond' <- maybeInvertCmmExpr cond
            = CmmCondBranch cond' f t (invertLikeliness l)

            -- If all jump destinations of a switch go to the
            -- same target eliminate the switch.
            | CmmSwitch _expr targets <- shortcut_last
            , (t:ts) <- switchTargetsToList targets
            , all (== t) ts
            = CmmBranch t

            | otherwise
            = shortcut_last

          likelyTrue (Just True)   = True
          likelyTrue _             = False

          invertLikeliness :: Maybe Bool -> Maybe Bool
          invertLikeliness         = fmap not

          -- Number of predecessors for a block
          numPreds bid = NonDet.mapLookup bid backEdges `orElse` 0

          hasOnePredecessor b = numPreds b == 1

{-
  Note [Invert Cmm conditionals]
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  The native code generator always produces jumps to the true branch.
  Falling through to the false branch is however faster. So we try to
  arrange for that to happen.
  This means we invert the condition if:
  * The likely path will become a fallthrough.
  * We can't guarantee a fallthrough for the false branch but for the
    true branch.

  In some cases it's faster to avoid inverting when the false branch is likely.
  However determining when that is the case is neither easy nor cheap so for
  now we always invert as this produces smaller binaries and code that is
  equally fast on average. (On an i7-6700K)

  TODO:
  There is also the edge case when both branches have multiple predecessors.
  In this case we could assume that we will end up with a jump for BOTH
  branches. In this case it might be best to put the likely path in the true
  branch especially if there are large numbers of predecessors as this saves
  us the jump that's not taken. However I haven't tested this and as of early
  2018 we almost never generate cmm where this would apply.
-}

-- Functions for incrementing and decrementing number of predecessors. If
-- decrementing would set the predecessor count to 0, we remove entry from the
-- map.
-- Invariant: if a block has no predecessors it should be dropped from the
-- graph because it is unreachable. maybe_concat is constructed to maintain
-- that invariant, but calling replaceLabels may introduce unreachable blocks.
-- We rely on subsequent passes in the Cmm pipeline to remove unreachable
-- blocks.
incPreds, decPreds :: BlockId -> NonDet.LabelMap Int -> NonDet.LabelMap Int
incPreds bid edges = NonDet.mapInsertWith (+) bid 1 edges
decPreds bid edges = case NonDet.mapLookup bid edges of
                       Just preds | preds > 1 -> NonDet.mapInsert bid (preds - 1) edges
                       Just _                 -> NonDet.mapDelete bid edges
                       _                      -> edges


-- Checks if a block consists only of "goto dest". If it does than we return
-- "Just dest" label. See Note [What is shortcutting]
canShortcut :: CmmBlock -> Maybe BlockId
canShortcut block
    | (_, middle, CmmBranch dest) <- blockSplit block
    , all dont_care $ blockToList middle
    = Just dest
    | otherwise
    = Nothing
    where dont_care CmmComment{} = True
          dont_care CmmTick{}    = True
          dont_care _other       = False

-- Concatenates two blocks. First one is assumed to be open on exit, the second
-- is assumed to be closed on entry (i.e. it has a label attached to it, which
-- the splice function removes by calling snd on result of blockSplitHead).
splice :: Block CmmNode C O -> CmmBlock -> CmmBlock
splice head rest = entry `blockJoinHead` code0 `blockAppend` code1
  where (CmmEntry lbl sc0, code0) = blockSplitHead head
        (CmmEntry _   sc1, code1) = blockSplitHead rest
        entry = CmmEntry lbl (combineTickScopes sc0 sc1)

-- If node is a call with continuation call return Just label of that
-- continuation. Otherwise return Nothing.
callContinuation_maybe :: CmmNode O C -> Maybe BlockId
callContinuation_maybe (CmmCall { cml_cont = Just b }) = Just b
callContinuation_maybe (CmmForeignCall { succ = b })   = Just b
callContinuation_maybe _ = Nothing


-- Map over the CmmGraph, replacing each label with its mapping in the
-- supplied LabelMap.
replaceLabels :: NonDet.LabelMap BlockId -> CmmGraph -> CmmGraph
replaceLabels env g
  | NonDet.mapNull env = g
  | otherwise   = replace_eid $ mapGraphNodes1 txnode g
   where
     replace_eid g = g {g_entry = lookup (g_entry g)}
     lookup id = NonDet.mapLookup id env `orElse` id

     txnode :: CmmNode e x -> CmmNode e x
     txnode (CmmBranch bid) = CmmBranch (lookup bid)
     txnode (CmmCondBranch p t f l) =
       mkCmmCondBranch (exp p) (lookup t) (lookup f) l
     txnode (CmmSwitch e ids) =
       CmmSwitch (exp e) (mapSwitchTargets lookup ids)
     txnode (CmmCall t k rg a res r) =
       CmmCall (exp t) (liftM lookup k) rg a res r
     txnode fc@CmmForeignCall{} =
       fc{ args = map exp (args fc), succ = lookup (succ fc) }
     txnode other = mapExpDeep exp other

     exp :: CmmExpr -> CmmExpr
     exp (CmmLit (CmmBlock bid))                = CmmLit (CmmBlock (lookup bid))
     exp (CmmStackSlot (Young id) i) = CmmStackSlot (Young (lookup id)) i
     exp e                                      = e

mkCmmCondBranch :: CmmExpr -> NonDet.Label -> NonDet.Label -> Maybe Bool -> CmmNode O C
mkCmmCondBranch p t f l =
  if t == f then CmmBranch t else CmmCondBranch p t f l

-- Build a map from a block to its set of predecessors.
predMap :: [CmmBlock] -> NonDet.LabelMap Int
predMap blocks = foldr add_preds NonDet.mapEmpty blocks
  where
    add_preds block env = foldr add env (successors block)
      where add lbl env = NonDet.mapInsertWith (+) lbl 1 env

-- Remove unreachable blocks from procs
removeUnreachableBlocksProc :: Platform -> CmmDecl -> CmmDecl
removeUnreachableBlocksProc _ proc@(CmmProc info lbl live g)
   | used_blocks `lengthLessThan` Det.mapSize (toBlockMap g)
   = CmmProc info' lbl live g'
   | otherwise
   = proc
   where
     g'    = ofBlockList (g_entry g) used_blocks
     info' = info { info_tbls = keep_used (info_tbls info) }
             -- Remove any info_tbls for unreachable

     keep_used :: Det.LabelMap CmmInfoTable -> Det.LabelMap CmmInfoTable
     keep_used bs = Det.mapFoldlWithKey keep Det.mapEmpty bs

     keep :: Det.LabelMap CmmInfoTable -> Det.Label -> CmmInfoTable -> Det.LabelMap CmmInfoTable
     keep env l i | l `NonDet.setMember` used_lbls = Det.mapInsert l i env
                  | otherwise               = env

     used_blocks :: [CmmBlock]
     used_blocks = revPostorder g

     used_lbls :: NonDet.LabelSet
     used_lbls = NonDet.setFromList $ map entryLabel used_blocks
removeUnreachableBlocksProc platform data'@(CmmData _ _) =
    pprPanic "removeUnreachableBlocksProc: passed data declaration instead of procedure" (pdoc platform data')
