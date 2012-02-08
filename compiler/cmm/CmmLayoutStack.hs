{-# LANGUAGE RecordWildCards, GADTs #-}
module CmmLayoutStack (
       cmmLayoutStack, setInfoTableStackMap
  ) where

import Cmm
import BlockId
import CmmUtils
import CmmLive
import CmmProcPoint
import SMRep
import Hoopl
import OptimizationFuel
import Constants
import UniqSupply
import Maybes
import UniqFM
import Util

import FastString
import Outputable
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.Fix
import Data.Array as Array

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


cmmLayoutStack :: ProcPointSet -> ByteOff -> CmmGraph
               -> FuelUniqSM (CmmGraph, BlockEnv StackMap)
cmmLayoutStack procpoints entry_args
               graph@(CmmGraph { g_entry = entry })
  = do
    pprTrace "cmmLayoutStack" (ppr entry_args) $ return ()
    liveness <- cmmLiveness graph
    pprTrace "liveness" (ppr liveness) $ return ()
    let blocks = postorderDfs graph

    (final_stackmaps, final_high_sp, new_blocks) <- liftUniq $
          mfix $ \ ~(rec_stackmaps, rec_high_sp, _new_blocks) ->
            layout procpoints liveness entry entry_args
                   rec_stackmaps rec_high_sp blocks

    pprTrace ("Sp HWM") (ppr final_high_sp) $
       return (ofBlockList entry new_blocks, final_stackmaps)



layout :: BlockSet                      -- proc points
       -> BlockEnv CmmLive              -- liveness
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

layout procpoints liveness entry entry_args final_stackmaps final_hwm blocks
  = go blocks init_stackmap entry_args []
  where
    sp_high = final_hwm - entry_args
        -- The stack check value is adjusted by the Sp offset on
        -- entry to the proc, which is entry_args.  We are
        -- assuming that we only do a stack check at the
        -- beginning of a proc, and we don't modify Sp before the
        -- check.

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
    
       pprTrace "layout" (ppr entry_lbl <+> ppr stack0) $ return ()
    
       -- Update the stack map to include the effects of assignments
       -- in this block
       let stack1 = foldBlockNodesF (procMiddle acc_stackmaps) middle0 stack0
    
       -- Insert assignments to reload all the live variables if this
       -- is a proc point
       let middle1 = if entry_lbl `setMember` procpoints
                        then foldr blockCons middle0 (insertReloads stack0)
                        else middle0
    
       -- Look at the last node and if we are making a call or jumping to
       -- a proc point, we must save the live variables, adjust Sp, and
       -- construct the StackMaps for each of the successor blocks.
       -- See handleLastNode for details.
       (saves, out, sp_off, last1, fixup_blocks)
           <- handleLastNode procpoints liveness cont_info
                             acc_stackmaps stack1 last0
    
       let hwm'    = maximum (acc_hwm : map sm_sp (mapElems out))
           middle2 = maybeAddSpAdj sp_off $ foldl blockSnoc middle1 saves
    
           -- manifest Sp: turn all CmmStackSlots into actual loads
           fiddle_middle = mapExpDeep (areaToSp sp0 sp_high final_stackmaps)
           fiddle_last   = mapExpDeep (areaToSp (sp0 - sp_off) sp_high
                                                final_stackmaps)
    
           stackmaps' = mapUnion acc_stackmaps out
           newblock   = blockJoin entry0 middle2 last1
           newblock'  = blockMapNodes3 (id, fiddle_middle, fiddle_last) newblock
           fixup_blocks' = map (blockMapNodes3 (id, fiddle_middle, id))
                               fixup_blocks
    
       pprTrace "layout(out)" (ppr out) $ return ()
    
       go bs stackmaps' hwm' (newblock' : fixup_blocks' ++ acc_blocks)


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

  get_cont b =
     case lastNode b of
        CmmCall { cml_cont = Just l, .. }
           -> (Just (l, cml_ret_args), cml_ret_off)
        CmmForeignCall { .. }
           -> (Just (succ, 0), updfr) -- ??
        _other -> (Nothing, 0)


maybeAddSpAdj :: ByteOff -> Block CmmNode O O -> Block CmmNode O O
maybeAddSpAdj 0      block = block
maybeAddSpAdj sp_off block
   = block `blockSnoc` CmmAssign spReg (cmmOffset (CmmReg spReg) sp_off)


procMiddle :: BlockEnv StackMap -> CmmNode e x -> StackMap -> StackMap
procMiddle stackmaps node sm
  = case node of
     CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot area off) t)
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

handleLastNode
   :: ProcPointSet -> BlockEnv CmmLive -> BlockEnv ByteOff
   -> BlockEnv StackMap -> StackMap
   -> CmmNode O C
   -> UniqSM
      ( [CmmNode O O]      -- assignments to save live variables
      , BlockEnv StackMap  -- stackmaps for the continuations
      , ByteOff            -- amount to adjust Sp before the jump
      , CmmNode O C        -- new last node
      , [CmmBlock]         -- new blocks
      )

handleLastNode procpoints liveness cont_info stackmaps
               stack0@StackMap { sm_sp = sp0 } last
 = case last of
    --  At each return / tail call,
    --  adjust Sp to point to the last argument pushed, which
    --  is cml_args, after popping any other junk from the stack.
    CmmCall{ cml_cont = Nothing, .. } -> do
      let sp_off = sp0 - cml_args
      return ([], mapEmpty, sp_off, last, [])

    --  At each CmmCall with a continuation:
    CmmCall{ cml_cont = Just cont_lbl, .. }
      -- If we have already seen this continuation before, then
      -- we just have to make the stack look the same:
      | Just cont_stack <- mapLookup cont_lbl stackmaps
      ->
         return ( fixupStack stack0 cont_stack
                , stackmaps
                , sp0 - sm_sp cont_stack
                , last
                , [] )

      -- a continuation we haven't seen before:
      -- allocate the stack frame for it.
      | otherwise -> do

      -- get the set of LocalRegs live in the continuation
      let target_live = mapFindWithDefault Set.empty cont_lbl
                            liveness

      -- the stack from the base to cml_ret_off is off-limits.
      -- our new stack frame contains:
      --   * saved live variables
      --   * the return address [young(C) + 8]
      --   * the args for the call,
      --     which are replaced by the return values at the return
      --     point.

      -- everything up to cml_ret_off is off-limits: mark it Occupied
      -- stack2 contains cml_ret_off, plus everything we need to save
          (stack2, assigs) = allocate cml_ret_off target_live stack0

      -- Sp is currently pointing to sp0,
      -- we want it to point to (sm_sp stack2 + cml_args)
      -- so the difference is sp0 - (sm_sp stack2 + cml_args)
          sp_off = sp0 - (sm_sp stack2 + cml_args)

      -- And the Sp at the continuation is:
      --   sm_sp stack2 + cml_ret_args
          cont_stack = stack2{ sm_sp = sm_sp stack2 + cml_ret_args
                             , sm_args = cml_ret_args
                             , sm_ret_off = cml_ret_off
                             }

      -- emit the necessary assignments of LocalRegs to stack slots
      -- emit an Sp adjustment, taking into account the call area
      --
      return ( assigs
             , mapSingleton cont_lbl cont_stack
             , sp_off
             , last
             , [] -- no new blocks
             )

    CmmBranch{..}     ->  handleProcPoints
    CmmCondBranch{..} ->  handleProcPoints
    CmmSwitch{..}     ->  handleProcPoints

  where
     handleProcPoints :: UniqSM ( [CmmNode O O]
                                , BlockEnv StackMap
                                , ByteOff
                                , CmmNode O C
                                , [CmmBlock] )

     handleProcPoints = do
          pps <- mapM handleProcPoint (successors last)
          let lbl_map :: LabelMap Label
              lbl_map = mapFromList [ (l,tmp) | (l,tmp,_,_) <- pps ]
              fix_lbl l = mapLookup l lbl_map `orElse` l
          return ( []
                 , mapFromList [ (l, sm) | (l,_,sm,_) <- pps ]
                 , 0
                 , mapSuccessors fix_lbl last
                 , concat [ blk | (_,_,_,blk) <- pps ] )

     -- For each proc point that is a successor of this block, we need to
     --   (a) if the proc point already has a stackmap, we need to
     --       shuffle the current stack to make it look the same.
     --       We have to insert a new block to make this happen.
     --   (b) otherwise, call "allocate live stack0" to make the
     --       stack map for the proc point
     handleProcPoint :: BlockId
                     -> UniqSM (BlockId, BlockId, StackMap, [CmmBlock])
     handleProcPoint l
        | not (l `setMember` procpoints) = return (l, l, stack0, [])
        | otherwise = do
           tmp <- getUniqueM
           let tmp_lbl = mkBlockId tmp
               (assigs, stack3) = case mapLookup l stackmaps of
                  Just pp_sm -> (fixupStack stack0 pp_sm, pp_sm)
                  Nothing    -> pprTrace "first visit to proc point" (ppr l <+> ppr live $$ ppr stack1) $ (assigs, stack2)
                    where
                     live = mapFindWithDefault Set.empty l liveness
                     (stack1, assigs) = allocate (sm_ret_off stack0) live stack0
                     cont_args = mapFindWithDefault 0 l cont_info
                     stack2 = stack1 { sm_sp   = sm_sp stack1 + cont_args
                                     , sm_args = cont_args
                                     }

               sp_off = sp0 - sm_sp stack3

               block = blockJoin
                          (CmmEntry tmp_lbl)
                          (maybeAddSpAdj sp_off (blockFromList assigs))
                          (CmmBranch l)
           --
           return (l, tmp_lbl, stack3, [block])


     passthrough :: BlockEnv StackMap
     passthrough = mapFromList (zip (successors last) (repeat stack0))


-- | create a sequence of assignments to establish the new StackMap,
-- given the old StackMap.
fixupStack :: StackMap -> StackMap -> [CmmNode O O]
fixupStack old_stack new_stack = concatMap move new_locs
 where
     old_map :: Map LocalReg ByteOff
     old_map  = Map.fromList (stackSlotRegs old_stack)
     new_locs = stackSlotRegs new_stack

     move (r,n)
       | Just m <- Map.lookup r old_map, n == m = []
       | otherwise = [CmmStore (CmmStackSlot Old n)
                               (CmmReg (CmmLocal r))]

-- -----------------------------------------------------------------------------
-- Updating references to CallAreas

{-
After running layout, we need to update all the references to stack areas.

Sp(L) is the Sp offset on entry to block L relative to the base of the
OLD area.

SpArgs(L) is the size of the young area for L, i.e. the number of
arguments.

 - in block L, each reference to (OldArea[N]) turns into
   [Sp + Sp(L) - N]

 - in block L, each reference to (Young(L')[N]) turns into
   [Sp + Sp(L) - Sp(L') + SpArgs(L') - N]

 - be careful with the last node of each block: Sp has already been adjusted
   to be Sp + Sp(L) - Sp(L')
-}

areaToSp :: ByteOff -> ByteOff -> BlockEnv StackMap -> CmmExpr -> CmmExpr
areaToSp sp_old _sp_hwm stackmaps (CmmStackSlot area n) =
  cmmOffset (CmmReg spReg) (sp_old - area_off - n)
  where
    area_off = case area of
                 Old -> 0
                 Young l ->
                    case mapLookup l stackmaps of
                       Just sm -> sm_sp sm - sm_args sm
                       Nothing -> pprPanic "areaToSp(2)" (ppr l)
areaToSp _ sp_hwm _ (CmmLit CmmHighStackMark) = CmmLit (mkIntCLit sp_hwm)
areaToSp _ _ _ other = other


-- -----------------------------------------------------------------------------
-- Saving live registers

-- | Given a set of live registers and a StackMap, save all the registers
-- on the stack and return the new StackMap and the assignments to do
-- the saving.
--
allocate :: ByteOff -> RegSet -> StackMap -> (StackMap, [CmmNode O O])
allocate ret_off live stackmap@StackMap{ sm_sp = sp0
                                       , sm_regs = regs0 }
 =
  pprTrace "allocate" (ppr live $$ ppr stackmap) $

   -- we only have to save regs that are not already in a slot
   let to_save = filter (not . (`elemUFM` regs0)) (Set.elems live)
       regs1   = filterUFM (\(r,_) -> elemRegSet r live) regs0
   in

   -- make a map of the stack
   let stack = reverse $ Array.elems $
               accumArray (\_ x -> x) Empty (1, toWords (max sp0 ret_off)) $
                 ret_words ++ live_words
            where ret_words =
                   [ (x, Occupied)
                   | x <- [ 1 .. toWords ret_off] ]
                  live_words =
                   [ (toWords x, Occupied)
                   | (r,off) <- eltsUFM regs1,
                     let w = localRegBytes r,
                     x <- [ off, off-wORD_SIZE .. off - w + 1] ]
   in

   -- Pass over the stack: find slots to save all the new live variables,
   -- choosing the oldest slots first (hence a foldr).
   let
       save slot ([], stack, n, assigs, regs) -- no more regs to save
          = ([], slot:stack, n `plusW` 1, assigs, regs)
       save slot (to_save, stack, n, assigs, regs)
          = case slot of
               Occupied ->  (to_save, Occupied:stack, n `plusW` 1, assigs, regs)
               Empty
                 | Just (stack', r, to_save') <-
                       select_save to_save (slot:stack)
                 -> let assig = CmmStore (CmmStackSlot Old n')
                                         (CmmReg (CmmLocal r))
                        n' = n `plusW` 1
                   in
                        (to_save', stack', n', assig : assigs, (r,(r,n')):regs)

                 | otherwise
                 -> (to_save, slot:stack, n `plusW` 1, assigs, regs)

       -- we should do better here: right now we'll fit the smallest first,
       -- but it would make more sense to fit the biggest first.
       select_save :: [LocalReg] -> [StackSlot]
                   -> Maybe ([StackSlot], LocalReg, [LocalReg])
       select_save regs stack = go regs []
         where go []     no_fit = Nothing
               go (r:rs) no_fit
                 | Just rest <- dropEmpty words stack
                 = Just (replicate words Occupied ++ rest, r, rs++no_fit)
                 | otherwise
                 = go rs (r:no_fit)
                 where words = localRegWords r

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
                  w  = typeWidth (localRegType r)
                  n' = n + widthInBytes w
                  assig = CmmStore (CmmStackSlot Old n')
                                   (CmmReg (CmmLocal r))

       trim_sp
          | not (null push_regs) = push_sp
          | otherwise
          = case break notEmpty save_stack of
              (empties, rest) -> n `plusW` (- length empties)

       final_regs = regs1 `addListToUFM` push_regs
                          `addListToUFM` save_regs

   in
  -- XXX should be an assert
   if ( n /= max sp0 ret_off ) then pprPanic "allocate" (ppr n <+> ppr sp0 <+> ppr ret_off) else

   ( stackmap { sm_regs = final_regs , sm_sp = trim_sp }
   , push_assigs ++ save_assigs )


-- -----------------------------------------------------------------------------
-- Update info tables to include stack liveness


setInfoTableStackMap :: BlockEnv StackMap -> CmmDecl -> CmmDecl
setInfoTableStackMap stackmaps
    (CmmProc top_info@TopInfo{..} l g@CmmGraph{g_entry = eid})
  = CmmProc top_info{ info_tbl = fix_info info_tbl } l g
  where
    fix_info info_tbl@CmmInfoTable{ cit_rep = StackRep _ } =
       info_tbl { cit_rep = StackRep (get_liveness eid) }
    fix_info other = other

    get_liveness :: BlockId -> Liveness
    get_liveness lbl
      = case mapLookup lbl stackmaps of
          Nothing -> pprPanic "setInfoTableStackMap" (ppr lbl)
          Just sm -> stackMapToLiveness sm

stackMapToLiveness :: StackMap -> Liveness
stackMapToLiveness StackMap{..} =
   reverse $ Array.elems $
        accumArray (\_ x -> x) True (toWords sm_ret_off + 1,
                                     toWords (sm_sp - sm_args)) live_words
   where
     live_words =  [ (toWords off, False)
                   | (r,off) <- eltsUFM sm_regs, isGcPtrType (localRegType r) ]


-- -----------------------------------------------------------------------------

plusW :: ByteOff -> WordOff -> ByteOff
plusW b w = b + w * wORD_SIZE

dropEmpty :: WordOff -> [StackSlot] -> Maybe [StackSlot]
dropEmpty 0 ss           = Just ss
dropEmpty n (Empty : ss) = dropEmpty (n-1) ss
dropEmpty n _            = Nothing

pushEmpty :: ByteOff -> [StackSlot] -> [StackSlot]
pushEmpty n stack = replicate (toWords n) Empty ++ stack

notEmpty :: StackSlot -> Bool
notEmpty Empty = False
notEmpty _ = True

localRegBytes :: LocalReg -> ByteOff
localRegBytes r = widthInBytes (typeWidth (localRegType r))

localRegWords :: LocalReg -> WordOff
localRegWords = toWords . localRegBytes

toWords :: ByteOff -> WordOff
toWords x = x `quot` wORD_SIZE


insertReloads :: StackMap -> [CmmNode O O]
insertReloads stackmap =
   [ CmmAssign (CmmLocal r) (CmmLoad (CmmStackSlot Old sp)
                                     (localRegType r))
   | (r,sp) <- stackSlotRegs stackmap
   ]


stackSlotRegs :: StackMap -> [(LocalReg, StackLoc)]
stackSlotRegs sm = eltsUFM (sm_regs sm)
