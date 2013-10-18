{-# LANGUAGE RecordWildCards, GADTs #-}
module CmmLayoutStack (
       cmmLayoutStack, setInfoTableStackMap
  ) where

import StgCmmUtils      ( callerSaveVolatileRegs ) -- XXX layering violation
import StgCmmForeign    ( saveThreadState, loadThreadState ) -- XXX layering violation

import BasicTypes
import Cmm
import CmmInfo
import BlockId
import CLabel
import CmmUtils
import MkGraph
import ForeignCall
import CmmLive
import CmmProcPoint
import SMRep
import Hoopl
import UniqSupply
import Maybes
import UniqFM
import Util

import DynFlags
import FastString
import Outputable
import qualified Data.Set as Set
import Control.Monad.Fix
import Data.Array as Array
import Data.Bits
import Data.List (nub)
import Control.Monad (liftM)

#include "HsVersions.h"


data StackSlot = Occupied | Empty
     -- Occupied: a return address or part of an update frame

instance Outputable StackSlot where
  ppr Occupied = ptext (sLit "XXX")
  ppr Empty    = ptext (sLit "---")

-- All stack locations are expressed as positive byte offsets from the
-- "base", which is defined to be the address above the return address
-- on the stack on entry to this CmmProc.
--
-- Lower addresses have higher StackLocs.
--
type StackLoc = ByteOff

{-
 A StackMap describes the stack at any given point.  At a continuation
 it has a particular layout, like this:

         |             | <- base
         |-------------|
         |     ret0    | <- base + 8
         |-------------|
         .  upd frame  . <- base + sm_ret_off
         |-------------|
         |             |
         .    vars     .
         . (live/dead) .
         |             | <- base + sm_sp - sm_args
         |-------------|
         |    ret1     |
         .  ret vals   . <- base + sm_sp    (<--- Sp points here)
         |-------------|

Why do we include the final return address (ret0) in our stack map?  I
have absolutely no idea, but it seems to be done that way consistently
in the rest of the code generator, so I played along here. --SDM

Note that we will be constructing an info table for the continuation
(ret1), which needs to describe the stack down to, but not including,
the update frame (or ret0, if there is no update frame).
-}

data StackMap = StackMap
 {  sm_sp   :: StackLoc
       -- ^ the offset of Sp relative to the base on entry
       -- to this block.
 ,  sm_args :: ByteOff
       -- ^ the number of bytes of arguments in the area for this block
       -- Defn: the offset of young(L) relative to the base is given by
       -- (sm_sp - sm_args) of the StackMap for block L.
 ,  sm_ret_off :: ByteOff
       -- ^ Number of words of stack that we do not describe with an info
       -- table, because it contains an update frame.
 ,  sm_regs :: UniqFM (LocalReg,StackLoc)
       -- ^ regs on the stack
 }

instance Outputable StackMap where
  ppr StackMap{..} =
     text "Sp = " <> int sm_sp $$
     text "sm_args = " <> int sm_args $$
     text "sm_ret_off = " <> int sm_ret_off $$
     text "sm_regs = " <> ppr (eltsUFM sm_regs)


cmmLayoutStack :: DynFlags -> ProcPointSet -> ByteOff -> CmmGraph
               -> UniqSM (CmmGraph, BlockEnv StackMap)
cmmLayoutStack dflags procpoints entry_args
               graph0@(CmmGraph { g_entry = entry })
  = do
    -- pprTrace "cmmLayoutStack" (ppr entry_args) $ return ()

    -- We need liveness info.  We could do removeDeadAssignments at
    -- the same time, but it buys nothing over doing cmmSink later,
    -- and costs a lot more than just cmmLocalLiveness.
    -- (graph, liveness) <- removeDeadAssignments graph0
    let (graph, liveness) = (graph0, cmmLocalLiveness dflags graph0)

    -- pprTrace "liveness" (ppr liveness) $ return ()
    let blocks = postorderDfs graph

    (final_stackmaps, _final_high_sp, new_blocks) <-
          mfix $ \ ~(rec_stackmaps, rec_high_sp, _new_blocks) ->
            layout dflags procpoints liveness entry entry_args
                   rec_stackmaps rec_high_sp blocks

    new_blocks' <- mapM (lowerSafeForeignCall dflags) new_blocks

    -- pprTrace ("Sp HWM") (ppr _final_high_sp) $ return ()
    return (ofBlockList entry new_blocks', final_stackmaps)



layout :: DynFlags
       -> BlockSet                      -- proc points
       -> BlockEnv CmmLocalLive         -- liveness
       -> BlockId                       -- entry
       -> ByteOff                       -- stack args on entry

       -> BlockEnv StackMap             -- [final] stack maps
       -> ByteOff                       -- [final] Sp high water mark

       -> [CmmBlock]                    -- [in] blocks

       -> UniqSM
          ( BlockEnv StackMap           -- [out] stack maps
          , ByteOff                     -- [out] Sp high water mark
          , [CmmBlock]                  -- [out] new blocks
          )

layout dflags procpoints liveness entry entry_args final_stackmaps final_sp_high blocks
  = go blocks init_stackmap entry_args []
  where
    (updfr, cont_info)  = collectContInfo blocks

    init_stackmap = mapSingleton entry StackMap{ sm_sp   = entry_args
                                               , sm_args = entry_args
                                               , sm_ret_off = updfr
                                               , sm_regs = emptyUFM
                                               }

    go [] acc_stackmaps acc_hwm acc_blocks
      = return (acc_stackmaps, acc_hwm, acc_blocks)

    go (b0 : bs) acc_stackmaps acc_hwm acc_blocks
      = do
       let (entry0@(CmmEntry entry_lbl), middle0, last0) = blockSplit b0

       let stack0@StackMap { sm_sp = sp0 }
               = mapFindWithDefault
                     (pprPanic "no stack map for" (ppr entry_lbl))
                     entry_lbl acc_stackmaps

       -- pprTrace "layout" (ppr entry_lbl <+> ppr stack0) $ return ()

       -- (a) Update the stack map to include the effects of
       --     assignments in this block
       let stack1 = foldBlockNodesF (procMiddle acc_stackmaps) middle0 stack0

       -- (b) Insert assignments to reload all the live variables if this
       --     block is a proc point
       let middle1 = if entry_lbl `setMember` procpoints
                        then foldr blockCons middle0 (insertReloads stack0)
                        else middle0

       -- (c) Look at the last node and if we are making a call or
       --     jumping to a proc point, we must save the live
       --     variables, adjust Sp, and construct the StackMaps for
       --     each of the successor blocks.  See handleLastNode for
       --     details.
       (middle2, sp_off, last1, fixup_blocks, out)
           <- handleLastNode dflags procpoints liveness cont_info
                             acc_stackmaps stack1 middle0 last0

       -- pprTrace "layout(out)" (ppr out) $ return ()

       -- (d) Manifest Sp: run over the nodes in the block and replace
       --     CmmStackSlot with CmmLoad from Sp with a concrete offset.
       --
       -- our block:
       --    middle1          -- the original middle nodes
       --    middle2          -- live variable saves from handleLastNode
       --    Sp = Sp + sp_off -- Sp adjustment goes here
       --    last1            -- the last node
       --
       let middle_pre = blockToList $ foldl blockSnoc middle1 middle2

           final_blocks = manifestSp dflags final_stackmaps stack0 sp0 final_sp_high entry0
                              middle_pre sp_off last1 fixup_blocks

           acc_stackmaps' = mapUnion acc_stackmaps out

           -- If this block jumps to the GC, then we do not take its
           -- stack usage into account for the high-water mark.
           -- Otherwise, if the only stack usage is in the stack-check
           -- failure block itself, we will do a redundant stack
           -- check.  The stack has a buffer designed to accommodate
           -- the largest amount of stack needed for calling the GC.
           --
           this_sp_hwm | isGcJump last0 = 0
                       | otherwise      = sp0 - sp_off

           hwm' = maximum (acc_hwm : this_sp_hwm : map sm_sp (mapElems out))

       go bs acc_stackmaps' hwm' (final_blocks ++ acc_blocks)


-- -----------------------------------------------------------------------------

-- Not foolproof, but GCFun is the culprit we most want to catch
isGcJump :: CmmNode O C -> Bool
isGcJump (CmmCall { cml_target = CmmReg (CmmGlobal l) })
  = l == GCFun || l == GCEnter1
isGcJump _something_else = False

-- -----------------------------------------------------------------------------

-- This doesn't seem right somehow.  We need to find out whether this
-- proc will push some update frame material at some point, so that we
-- can avoid using that area of the stack for spilling.  The
-- updfr_space field of the CmmProc *should* tell us, but it doesn't
-- (I think maybe it gets filled in later when we do proc-point
-- splitting).
--
-- So we'll just take the max of all the cml_ret_offs.  This could be
-- unnecessarily pessimistic, but probably not in the code we
-- generate.

collectContInfo :: [CmmBlock] -> (ByteOff, BlockEnv ByteOff)
collectContInfo blocks
  = (maximum ret_offs, mapFromList (catMaybes mb_argss))
 where
  (mb_argss, ret_offs) = mapAndUnzip get_cont blocks

  get_cont :: Block CmmNode x C -> (Maybe (Label, ByteOff), ByteOff)
  get_cont b =
     case lastNode b of
        CmmCall { cml_cont = Just l, .. }
           -> (Just (l, cml_ret_args), cml_ret_off)
        CmmForeignCall { .. }
           -> (Just (succ, ret_args), ret_off)
        _other -> (Nothing, 0)


-- -----------------------------------------------------------------------------
-- Updating the StackMap from middle nodes

-- Look for loads from stack slots, and update the StackMap.  This is
-- purely for optimisation reasons, so that we can avoid saving a
-- variable back to a different stack slot if it is already on the
-- stack.
--
-- This happens a lot: for example when function arguments are passed
-- on the stack and need to be immediately saved across a call, we
-- want to just leave them where they are on the stack.
--
procMiddle :: BlockEnv StackMap -> CmmNode e x -> StackMap -> StackMap
procMiddle stackmaps node sm
  = case node of
     CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot area off) _)
       -> sm { sm_regs = addToUFM (sm_regs sm) r (r,loc) }
        where loc = getStackLoc area off stackmaps
     CmmAssign (CmmLocal r) _other
       -> sm { sm_regs = delFromUFM (sm_regs sm) r }
     _other
       -> sm

getStackLoc :: Area -> ByteOff -> BlockEnv StackMap -> StackLoc
getStackLoc Old       n _         = n
getStackLoc (Young l) n stackmaps =
  case mapLookup l stackmaps of
    Nothing -> pprPanic "getStackLoc" (ppr l)
    Just sm -> sm_sp sm - sm_args sm + n


-- -----------------------------------------------------------------------------
-- Handling stack allocation for a last node

-- We take a single last node and turn it into:
--
--    C1 (some statements)
--    Sp = Sp + N
--    C2 (some more statements)
--    call f()          -- the actual last node
--
-- plus possibly some more blocks (we may have to add some fixup code
-- between the last node and the continuation).
--
-- C1: is the code for saving the variables across this last node onto
-- the stack, if the continuation is a call or jumps to a proc point.
--
-- C2: if the last node is a safe foreign call, we have to inject some
-- extra code that goes *after* the Sp adjustment.

handleLastNode
   :: DynFlags -> ProcPointSet -> BlockEnv CmmLocalLive -> BlockEnv ByteOff
   -> BlockEnv StackMap -> StackMap
   -> Block CmmNode O O
   -> CmmNode O C
   -> UniqSM
      ( [CmmNode O O]      -- nodes to go *before* the Sp adjustment
      , ByteOff            -- amount to adjust Sp
      , CmmNode O C        -- new last node
      , [CmmBlock]         -- new blocks
      , BlockEnv StackMap  -- stackmaps for the continuations
      )

handleLastNode dflags procpoints liveness cont_info stackmaps
               stack0@StackMap { sm_sp = sp0 } middle last
 = case last of
    --  At each return / tail call,
    --  adjust Sp to point to the last argument pushed, which
    --  is cml_args, after popping any other junk from the stack.
    CmmCall{ cml_cont = Nothing, .. } -> do
      let sp_off = sp0 - cml_args
      return ([], sp_off, last, [], mapEmpty)

    --  At each CmmCall with a continuation:
    CmmCall{ cml_cont = Just cont_lbl, .. } ->
       return $ lastCall cont_lbl cml_args cml_ret_args cml_ret_off

    CmmForeignCall{ succ = cont_lbl, .. } -> do
       return $ lastCall cont_lbl (wORD_SIZE dflags) ret_args ret_off
            -- one word of args: the return address

    CmmBranch{..}     ->  handleBranches
    CmmCondBranch{..} ->  handleBranches
    CmmSwitch{..}     ->  handleBranches

  where
     -- Calls and ForeignCalls are handled the same way:
     lastCall :: BlockId -> ByteOff -> ByteOff -> ByteOff
              -> ( [CmmNode O O]
                 , ByteOff
                 , CmmNode O C
                 , [CmmBlock]
                 , BlockEnv StackMap
                 )
     lastCall lbl cml_args cml_ret_args cml_ret_off
      =  ( assignments
         , spOffsetForCall sp0 cont_stack cml_args
         , last
         , [] -- no new blocks
         , mapSingleton lbl cont_stack )
      where
         (assignments, cont_stack) = prepareStack lbl cml_ret_args cml_ret_off


     prepareStack lbl cml_ret_args cml_ret_off
       | Just cont_stack <- mapLookup lbl stackmaps
             -- If we have already seen this continuation before, then
             -- we just have to make the stack look the same:
       = (fixupStack stack0 cont_stack, cont_stack)
             -- Otherwise, we have to allocate the stack frame
       | otherwise
       = (save_assignments, new_cont_stack)
       where
        (new_cont_stack, save_assignments)
           = setupStackFrame dflags lbl liveness cml_ret_off cml_ret_args stack0


     -- For other last nodes (branches), if any of the targets is a
     -- proc point, we have to set up the stack to match what the proc
     -- point is expecting.
     --
     handleBranches :: UniqSM ( [CmmNode O O]
                                , ByteOff
                                , CmmNode O C
                                , [CmmBlock]
                                , BlockEnv StackMap )

     handleBranches
         -- Note [diamond proc point]
       | Just l <- futureContinuation middle
       , (nub $ filter (`setMember` procpoints) $ successors last) == [l]
       = do
         let cont_args = mapFindWithDefault 0 l cont_info
             (assigs, cont_stack) = prepareStack l cont_args (sm_ret_off stack0)
             out = mapFromList [ (l', cont_stack)
                               | l' <- successors last ]
         return ( assigs
                , spOffsetForCall sp0 cont_stack (wORD_SIZE dflags)
                , last
                , []
                , out)

        | otherwise = do
          pps <- mapM handleBranch (successors last)
          let lbl_map :: LabelMap Label
              lbl_map = mapFromList [ (l,tmp) | (l,tmp,_,_) <- pps ]
              fix_lbl l = mapFindWithDefault l l lbl_map
          return ( []
                 , 0
                 , mapSuccessors fix_lbl last
                 , concat [ blk | (_,_,_,blk) <- pps ]
                 , mapFromList [ (l, sm) | (l,_,sm,_) <- pps ] )

     -- For each successor of this block
     handleBranch :: BlockId -> UniqSM (BlockId, BlockId, StackMap, [CmmBlock])
     handleBranch l
        --   (a) if the successor already has a stackmap, we need to
        --       shuffle the current stack to make it look the same.
        --       We have to insert a new block to make this happen.
        | Just stack2 <- mapLookup l stackmaps
        = do
             let assigs = fixupStack stack0 stack2
             (tmp_lbl, block) <- makeFixupBlock dflags sp0 l stack2 assigs
             return (l, tmp_lbl, stack2, block)

        --   (b) if the successor is a proc point, save everything
        --       on the stack.
        | l `setMember` procpoints
        = do
             let cont_args = mapFindWithDefault 0 l cont_info
                 (stack2, assigs) =
                      --pprTrace "first visit to proc point"
                      --             (ppr l <+> ppr stack1) $
                      setupStackFrame dflags l liveness (sm_ret_off stack0)
                                                       cont_args stack0
             --
             (tmp_lbl, block) <- makeFixupBlock dflags sp0 l stack2 assigs
             return (l, tmp_lbl, stack2, block)

        --   (c) otherwise, the current StackMap is the StackMap for
        --       the continuation.  But we must remember to remove any
        --       variables from the StackMap that are *not* live at
        --       the destination, because this StackMap might be used
        --       by fixupStack if this is a join point.
        | otherwise = return (l, l, stack1, [])
        where live = mapFindWithDefault (panic "handleBranch") l liveness
              stack1 = stack0 { sm_regs = filterUFM is_live (sm_regs stack0) }
              is_live (r,_) = r `elemRegSet` live


makeFixupBlock :: DynFlags -> ByteOff -> Label -> StackMap -> [CmmNode O O]
               -> UniqSM (Label, [CmmBlock])
makeFixupBlock dflags sp0 l stack assigs
  | null assigs && sp0 == sm_sp stack = return (l, [])
  | otherwise = do
    tmp_lbl <- liftM mkBlockId $ getUniqueM
    let sp_off = sp0 - sm_sp stack
        block = blockJoin (CmmEntry tmp_lbl)
                          (maybeAddSpAdj dflags sp_off (blockFromList assigs))
                          (CmmBranch l)
    return (tmp_lbl, [block])


-- Sp is currently pointing to current_sp,
-- we want it to point to
--    (sm_sp cont_stack - sm_args cont_stack + args)
-- so the difference is
--    sp0 - (sm_sp cont_stack - sm_args cont_stack + args)
spOffsetForCall :: ByteOff -> StackMap -> ByteOff -> ByteOff
spOffsetForCall current_sp cont_stack args
  = current_sp - (sm_sp cont_stack - sm_args cont_stack + args)


-- | create a sequence of assignments to establish the new StackMap,
-- given the old StackMap.
fixupStack :: StackMap -> StackMap -> [CmmNode O O]
fixupStack old_stack new_stack = concatMap move new_locs
 where
     old_map  = sm_regs old_stack
     new_locs = stackSlotRegs new_stack

     move (r,n)
       | Just (_,m) <- lookupUFM old_map r, n == m = []
       | otherwise = [CmmStore (CmmStackSlot Old n)
                               (CmmReg (CmmLocal r))]



setupStackFrame
             :: DynFlags
             -> BlockId                 -- label of continuation
             -> BlockEnv CmmLocalLive   -- liveness
             -> ByteOff      -- updfr
             -> ByteOff      -- bytes of return values on stack
             -> StackMap     -- current StackMap
             -> (StackMap, [CmmNode O O])

setupStackFrame dflags lbl liveness updfr_off ret_args stack0
  = (cont_stack, assignments)
  where
      -- get the set of LocalRegs live in the continuation
      live = mapFindWithDefault Set.empty lbl liveness

      -- the stack from the base to updfr_off is off-limits.
      -- our new stack frame contains:
      --   * saved live variables
      --   * the return address [young(C) + 8]
      --   * the args for the call,
      --     which are replaced by the return values at the return
      --     point.

      -- everything up to updfr_off is off-limits
      -- stack1 contains updfr_off, plus everything we need to save
      (stack1, assignments) = allocate dflags updfr_off live stack0

      -- And the Sp at the continuation is:
      --   sm_sp stack1 + ret_args
      cont_stack = stack1{ sm_sp = sm_sp stack1 + ret_args
                         , sm_args = ret_args
                         , sm_ret_off = updfr_off
                         }


-- -----------------------------------------------------------------------------
-- Note [diamond proc point]
--
-- This special case looks for the pattern we get from a typical
-- tagged case expression:
--
--    Sp[young(L1)] = L1
--    if (R1 & 7) != 0 goto L1 else goto L2
--  L2:
--    call [R1] returns to L1
--  L1: live: {y}
--    x = R1
--
-- If we let the generic case handle this, we get
--
--    Sp[-16] = L1
--    if (R1 & 7) != 0 goto L1a else goto L2
--  L2:
--    Sp[-8] = y
--    Sp = Sp - 16
--    call [R1] returns to L1
--  L1a:
--    Sp[-8] = y
--    Sp = Sp - 16
--    goto L1
--  L1:
--    x = R1
--
-- The code for saving the live vars is duplicated in each branch, and
-- furthermore there is an extra jump in the fast path (assuming L1 is
-- a proc point, which it probably is if there is a heap check).
--
-- So to fix this we want to set up the stack frame before the
-- conditional jump.  How do we know when to do this, and when it is
-- safe?  The basic idea is, when we see the assignment
--
--   Sp[young(L)] = L
--
-- we know that
--   * we are definitely heading for L
--   * there can be no more reads from another stack area, because young(L)
--     overlaps with it.
--
-- We don't necessarily know that everything live at L is live now
-- (some might be assigned between here and the jump to L).  So we
-- simplify and only do the optimisation when we see
--
--   (1) a block containing an assignment of a return address L
--   (2) ending in a branch where one (and only) continuation goes to L,
--       and no other continuations go to proc points.
--
-- then we allocate the stack frame for L at the end of the block,
-- before the branch.
--
-- We could generalise (2), but that would make it a bit more
-- complicated to handle, and this currently catches the common case.

futureContinuation :: Block CmmNode O O -> Maybe BlockId
futureContinuation middle = foldBlockNodesB f middle Nothing
   where f :: CmmNode a b -> Maybe BlockId -> Maybe BlockId
         f (CmmStore (CmmStackSlot (Young l) _) (CmmLit (CmmBlock _))) _
               = Just l
         f _ r = r

-- -----------------------------------------------------------------------------
-- Saving live registers

-- | Given a set of live registers and a StackMap, save all the registers
-- on the stack and return the new StackMap and the assignments to do
-- the saving.
--
allocate :: DynFlags -> ByteOff -> LocalRegSet -> StackMap
         -> (StackMap, [CmmNode O O])
allocate dflags ret_off live stackmap@StackMap{ sm_sp = sp0
                                              , sm_regs = regs0 }
 =
  -- pprTrace "allocate" (ppr live $$ ppr stackmap) $

   -- we only have to save regs that are not already in a slot
   let to_save = filter (not . (`elemUFM` regs0)) (Set.elems live)
       regs1   = filterUFM (\(r,_) -> elemRegSet r live) regs0
   in

   -- make a map of the stack
   let stack = reverse $ Array.elems $
               accumArray (\_ x -> x) Empty (1, toWords dflags (max sp0 ret_off)) $
                 ret_words ++ live_words
            where ret_words =
                   [ (x, Occupied)
                   | x <- [ 1 .. toWords dflags ret_off] ]
                  live_words =
                   [ (toWords dflags x, Occupied)
                   | (r,off) <- eltsUFM regs1,
                     let w = localRegBytes dflags r,
                     x <- [ off, off - wORD_SIZE dflags .. off - w + 1] ]
   in

   -- Pass over the stack: find slots to save all the new live variables,
   -- choosing the oldest slots first (hence a foldr).
   let
       save slot ([], stack, n, assigs, regs) -- no more regs to save
          = ([], slot:stack, plusW dflags n 1, assigs, regs)
       save slot (to_save, stack, n, assigs, regs)
          = case slot of
               Occupied ->  (to_save, Occupied:stack, plusW dflags n 1, assigs, regs)
               Empty
                 | Just (stack', r, to_save') <-
                       select_save to_save (slot:stack)
                 -> let assig = CmmStore (CmmStackSlot Old n')
                                         (CmmReg (CmmLocal r))
                        n' = plusW dflags n 1
                   in
                        (to_save', stack', n', assig : assigs, (r,(r,n')):regs)

                 | otherwise
                 -> (to_save, slot:stack, plusW dflags n 1, assigs, regs)

       -- we should do better here: right now we'll fit the smallest first,
       -- but it would make more sense to fit the biggest first.
       select_save :: [LocalReg] -> [StackSlot]
                   -> Maybe ([StackSlot], LocalReg, [LocalReg])
       select_save regs stack = go regs []
         where go []     _no_fit = Nothing
               go (r:rs) no_fit
                 | Just rest <- dropEmpty words stack
                 = Just (replicate words Occupied ++ rest, r, rs++no_fit)
                 | otherwise
                 = go rs (r:no_fit)
                 where words = localRegWords dflags r

       -- fill in empty slots as much as possible
       (still_to_save, save_stack, n, save_assigs, save_regs)
          = foldr save (to_save, [], 0, [], []) stack

       -- push any remaining live vars on the stack
       (push_sp, push_assigs, push_regs)
          = foldr push (n, [], []) still_to_save
          where
              push r (n, assigs, regs)
                = (n', assig : assigs, (r,(r,n')) : regs)
                where
                  n' = n + localRegBytes dflags r
                  assig = CmmStore (CmmStackSlot Old n')
                                   (CmmReg (CmmLocal r))

       trim_sp
          | not (null push_regs) = push_sp
          | otherwise
          = plusW dflags n (- length (takeWhile isEmpty save_stack))

       final_regs = regs1 `addListToUFM` push_regs
                          `addListToUFM` save_regs

   in
  -- XXX should be an assert
   if ( n /= max sp0 ret_off ) then pprPanic "allocate" (ppr n <+> ppr sp0 <+> ppr ret_off) else

   if (trim_sp .&. (wORD_SIZE dflags - 1)) /= 0  then pprPanic "allocate2" (ppr trim_sp <+> ppr final_regs <+> ppr push_sp) else

   ( stackmap { sm_regs = final_regs , sm_sp = trim_sp }
   , push_assigs ++ save_assigs )


-- -----------------------------------------------------------------------------
-- Manifesting Sp

-- | Manifest Sp: turn all the CmmStackSlots into CmmLoads from Sp.  The
-- block looks like this:
--
--    middle_pre       -- the middle nodes
--    Sp = Sp + sp_off -- Sp adjustment goes here
--    last             -- the last node
--
-- And we have some extra blocks too (that don't contain Sp adjustments)
--
-- The adjustment for middle_pre will be different from that for
-- middle_post, because the Sp adjustment intervenes.
--
manifestSp
   :: DynFlags
   -> BlockEnv StackMap  -- StackMaps for other blocks
   -> StackMap           -- StackMap for this block
   -> ByteOff            -- Sp on entry to the block
   -> ByteOff            -- SpHigh
   -> CmmNode C O        -- first node
   -> [CmmNode O O]      -- middle
   -> ByteOff            -- sp_off
   -> CmmNode O C        -- last node
   -> [CmmBlock]         -- new blocks
   -> [CmmBlock]         -- final blocks with Sp manifest

manifestSp dflags stackmaps stack0 sp0 sp_high
           first middle_pre sp_off last fixup_blocks
  = final_block : fixup_blocks'
  where
    area_off = getAreaOff stackmaps

    adj_pre_sp, adj_post_sp :: CmmNode e x -> CmmNode e x
    adj_pre_sp  = mapExpDeep (areaToSp dflags sp0            sp_high area_off)
    adj_post_sp = mapExpDeep (areaToSp dflags (sp0 - sp_off) sp_high area_off)

    final_middle = maybeAddSpAdj dflags sp_off $
                   blockFromList $
                   map adj_pre_sp $
                   elimStackStores stack0 stackmaps area_off $
                   middle_pre

    final_last    = optStackCheck (adj_post_sp last)

    final_block   = blockJoin first final_middle final_last

    fixup_blocks' = map (mapBlock3' (id, adj_post_sp, id)) fixup_blocks


getAreaOff :: BlockEnv StackMap -> (Area -> StackLoc)
getAreaOff _ Old = 0
getAreaOff stackmaps (Young l) =
  case mapLookup l stackmaps of
    Just sm -> sm_sp sm - sm_args sm
    Nothing -> pprPanic "getAreaOff" (ppr l)


maybeAddSpAdj :: DynFlags -> ByteOff -> Block CmmNode O O -> Block CmmNode O O
maybeAddSpAdj _      0      block = block
maybeAddSpAdj dflags sp_off block
   = block `blockSnoc` CmmAssign spReg (cmmOffset dflags (CmmReg spReg) sp_off)


{-
Sp(L) is the Sp offset on entry to block L relative to the base of the
OLD area.

SpArgs(L) is the size of the young area for L, i.e. the number of
arguments.

 - in block L, each reference to [old + N] turns into
   [Sp + Sp(L) - N]

 - in block L, each reference to [young(L') + N] turns into
   [Sp + Sp(L) - Sp(L') + SpArgs(L') - N]

 - be careful with the last node of each block: Sp has already been adjusted
   to be Sp + Sp(L) - Sp(L')
-}

areaToSp :: DynFlags -> ByteOff -> ByteOff -> (Area -> StackLoc) -> CmmExpr -> CmmExpr
areaToSp dflags sp_old _sp_hwm area_off (CmmStackSlot area n) =
  cmmOffset dflags (CmmReg spReg) (sp_old - area_off area - n)
areaToSp dflags _ sp_hwm _ (CmmLit CmmHighStackMark) = mkIntExpr dflags sp_hwm
areaToSp dflags _ _ _ (CmmMachOp (MO_U_Lt _)  -- Note [Always false stack check]
                          [CmmMachOp (MO_Sub _)
                                  [ CmmRegOff (CmmGlobal Sp) x_off
                                  , CmmLit (CmmInt y_lit _)],
                           CmmReg (CmmGlobal SpLim)])
                              | fromIntegral x_off >= y_lit = zeroExpr dflags
areaToSp _ _ _ _ other = other

-- Note [Always false stack check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We can optimise stack checks of the form
--
--   if ((Sp + x) - y < SpLim) then .. else ..
--
-- where are non-negative integer byte offsets.  Since we know that
-- SpLim <= Sp (remember the stack grows downwards), this test must
-- yield False if (x >= y), so we can rewrite the comparison to False.
-- A subsequent sinking pass will later drop the dead code.
-- Optimising this away depends on knowing that SpLim <= Sp, so it is
-- really the job of the stack layout algorithm, hence we do it now.

optStackCheck :: CmmNode O C -> CmmNode O C
optStackCheck n = -- Note [null stack check]
 case n of
   CmmCondBranch (CmmLit (CmmInt 0 _)) _true false -> CmmBranch false
   other -> other


-- -----------------------------------------------------------------------------

-- | Eliminate stores of the form
--
--    Sp[area+n] = r
--
-- when we know that r is already in the same slot as Sp[area+n].  We
-- could do this in a later optimisation pass, but that would involve
-- a separate analysis and we already have the information to hand
-- here.  It helps clean up some extra stack stores in common cases.
--
-- Note that we may have to modify the StackMap as we walk through the
-- code using procMiddle, since an assignment to a variable in the
-- StackMap will invalidate its mapping there.
--
elimStackStores :: StackMap
                -> BlockEnv StackMap
                -> (Area -> ByteOff)
                -> [CmmNode O O]
                -> [CmmNode O O]
elimStackStores stackmap stackmaps area_off nodes
  = go stackmap nodes
  where
    go _stackmap [] = []
    go stackmap (n:ns)
     = case n of
         CmmStore (CmmStackSlot area m) (CmmReg (CmmLocal r))
            | Just (_,off) <- lookupUFM (sm_regs stackmap) r
            , area_off area + m == off
            -> -- pprTrace "eliminated a node!" (ppr r) $
               go stackmap ns
         _otherwise
            -> n : go (procMiddle stackmaps n stackmap) ns


-- -----------------------------------------------------------------------------
-- Update info tables to include stack liveness


setInfoTableStackMap :: DynFlags -> BlockEnv StackMap -> CmmDecl -> CmmDecl
setInfoTableStackMap dflags stackmaps (CmmProc top_info@TopInfo{..} l v g)
  = CmmProc top_info{ info_tbls = mapMapWithKey fix_info info_tbls } l v g
  where
    fix_info lbl info_tbl@CmmInfoTable{ cit_rep = StackRep _ } =
       info_tbl { cit_rep = StackRep (get_liveness lbl) }
    fix_info _ other = other

    get_liveness :: BlockId -> Liveness
    get_liveness lbl
      = case mapLookup lbl stackmaps of
          Nothing -> pprPanic "setInfoTableStackMap" (ppr lbl <+> ppr info_tbls)
          Just sm -> stackMapToLiveness dflags sm

setInfoTableStackMap _ _ d = d


stackMapToLiveness :: DynFlags -> StackMap -> Liveness
stackMapToLiveness dflags StackMap{..} =
   reverse $ Array.elems $
        accumArray (\_ x -> x) True (toWords dflags sm_ret_off + 1,
                                     toWords dflags (sm_sp - sm_args)) live_words
   where
     live_words =  [ (toWords dflags off, False)
                   | (r,off) <- eltsUFM sm_regs, isGcPtrType (localRegType r) ]


-- -----------------------------------------------------------------------------
-- Lowering safe foreign calls

{-
Note [Lower safe foreign calls]

We start with

   Sp[young(L1)] = L1
 ,-----------------------
 | r1 = foo(x,y,z) returns to L1
 '-----------------------
 L1:
   R1 = r1 -- copyIn, inserted by mkSafeCall
   ...

the stack layout algorithm will arrange to save and reload everything
live across the call.  Our job now is to expand the call so we get

   Sp[young(L1)] = L1
 ,-----------------------
 | SAVE_THREAD_STATE()
 | token = suspendThread(BaseReg, interruptible)
 | r = foo(x,y,z)
 | BaseReg = resumeThread(token)
 | LOAD_THREAD_STATE()
 | R1 = r  -- copyOut
 | jump Sp[0]
 '-----------------------
 L1:
   r = R1 -- copyIn, inserted by mkSafeCall
   ...

Note the copyOut, which saves the results in the places that L1 is
expecting them (see Note {safe foreign call convention]). Note also
that safe foreign call is replace by an unsafe one in the Cmm graph.
-}

lowerSafeForeignCall :: DynFlags -> CmmBlock -> UniqSM CmmBlock
lowerSafeForeignCall dflags block
  | (entry, middle, CmmForeignCall { .. }) <- blockSplit block
  = do
    -- Both 'id' and 'new_base' are KindNonPtr because they're
    -- RTS-only objects and are not subject to garbage collection
    id <- newTemp (bWord dflags)
    new_base <- newTemp (cmmRegType dflags (CmmGlobal BaseReg))
    let (caller_save, caller_load) = callerSaveVolatileRegs dflags
    load_tso <- newTemp (gcWord dflags)
    load_stack <- newTemp (gcWord dflags)
    let suspend = saveThreadState dflags <*>
                  caller_save <*>
                  mkMiddle (callSuspendThread dflags id intrbl)
        midCall = mkUnsafeCall tgt res args
        resume  = mkMiddle (callResumeThread new_base id) <*>
                  -- Assign the result to BaseReg: we
                  -- might now have a different Capability!
                  mkAssign (CmmGlobal BaseReg) (CmmReg (CmmLocal new_base)) <*>
                  caller_load <*>
                  loadThreadState dflags load_tso load_stack

        (_, regs, copyout) =
             copyOutOflow dflags NativeReturn Jump (Young succ)
                            (map (CmmReg . CmmLocal) res)
                            ret_off []

        -- NB. after resumeThread returns, the top-of-stack probably contains
        -- the stack frame for succ, but it might not: if the current thread
        -- received an exception during the call, then the stack might be
        -- different.  Hence we continue by jumping to the top stack frame,
        -- not by jumping to succ.
        jump = CmmCall { cml_target    = entryCode dflags $
                                         CmmLoad (CmmReg spReg) (bWord dflags)
                       , cml_cont      = Just succ
                       , cml_args_regs = regs
                       , cml_args      = widthInBytes (wordWidth dflags)
                       , cml_ret_args  = ret_args
                       , cml_ret_off   = ret_off }

    graph' <- lgraphOfAGraph $ suspend <*>
                               midCall <*>
                               resume  <*>
                               copyout <*>
                               mkLast jump

    case toBlockList graph' of
      [one] -> let (_, middle', last) = blockSplit one
               in return (blockJoin entry (middle `blockAppend` middle') last)
      _ -> panic "lowerSafeForeignCall0"

  -- Block doesn't end in a safe foreign call:
  | otherwise = return block


foreignLbl :: FastString -> CmmExpr
foreignLbl name = CmmLit (CmmLabel (mkForeignLabel name Nothing ForeignLabelInExternalPackage IsFunction))

newTemp :: CmmType -> UniqSM LocalReg
newTemp rep = getUniqueM >>= \u -> return (LocalReg u rep)

callSuspendThread :: DynFlags -> LocalReg -> Bool -> CmmNode O O
callSuspendThread dflags id intrbl =
  CmmUnsafeForeignCall
       (ForeignTarget (foreignLbl (fsLit "suspendThread"))
        (ForeignConvention CCallConv [AddrHint, NoHint] [AddrHint] CmmMayReturn))
       [id] [CmmReg (CmmGlobal BaseReg), mkIntExpr dflags (fromEnum intrbl)]

callResumeThread :: LocalReg -> LocalReg -> CmmNode O O
callResumeThread new_base id =
  CmmUnsafeForeignCall
       (ForeignTarget (foreignLbl (fsLit "resumeThread"))
            (ForeignConvention CCallConv [AddrHint] [AddrHint] CmmMayReturn))
       [new_base] [CmmReg (CmmLocal id)]

-- -----------------------------------------------------------------------------

plusW :: DynFlags -> ByteOff -> WordOff -> ByteOff
plusW dflags b w = b + w * wORD_SIZE dflags

dropEmpty :: WordOff -> [StackSlot] -> Maybe [StackSlot]
dropEmpty 0 ss           = Just ss
dropEmpty n (Empty : ss) = dropEmpty (n-1) ss
dropEmpty _ _            = Nothing

isEmpty :: StackSlot -> Bool
isEmpty Empty = True
isEmpty _ = False

localRegBytes :: DynFlags -> LocalReg -> ByteOff
localRegBytes dflags r
    = roundUpToWords dflags (widthInBytes (typeWidth (localRegType r)))

localRegWords :: DynFlags -> LocalReg -> WordOff
localRegWords dflags = toWords dflags . localRegBytes dflags

toWords :: DynFlags -> ByteOff -> WordOff
toWords dflags x = x `quot` wORD_SIZE dflags


insertReloads :: StackMap -> [CmmNode O O]
insertReloads stackmap =
   [ CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot Old sp)
                                     (localRegType r))
   | (r,sp) <- stackSlotRegs stackmap
   ]


stackSlotRegs :: StackMap -> [(LocalReg, StackLoc)]
stackSlotRegs sm = eltsUFM (sm_regs sm)
