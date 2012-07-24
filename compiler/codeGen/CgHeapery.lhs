%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[CgHeapery]{Heap management functions}

\begin{code}
module CgHeapery (
        initHeapUsage, getVirtHp, setVirtHp, setRealHp,
        getHpRelOffset, hpRel,

        funEntryChecks, thunkEntryChecks,
        altHeapCheck, unbxTupleHeapCheck,
        hpChkGen, hpChkNodePointsAssignSp0,
        stkChkGen, stkChkNodePoints,

        layOutDynConstr, layOutStaticConstr,
        mkVirtHeapOffsets, mkStaticClosureFields, mkStaticClosure,

        allocDynClosure, emitSetDynHdr
    ) where

#include "HsVersions.h"

import StgSyn
import CLabel
import CgUtils
import CgMonad
import CgProf
import CgTicky
import CgParallel
import CgStackery
import CgCallConv
import ClosureInfo
import SMRep

import OldCmm
import OldCmmUtils
import Id
import DataCon
import TyCon
import CostCentre
import Util
import Module
import Constants
import Outputable
import DynFlags
import FastString

import Data.List
import Data.Maybe (fromMaybe)
\end{code}


%************************************************************************
%*                                                                      *
\subsection[CgUsages-heapery]{Monad things for fiddling with heap usage}
%*                                                                      *
%************************************************************************

The heap always grows upwards, so hpRel is easy

\begin{code}
hpRel :: VirtualHpOffset        -- virtual offset of Hp
      -> VirtualHpOffset        -- virtual offset of The Thing
      -> WordOff                -- integer word offset
hpRel hp off = off - hp
\end{code}

@initHeapUsage@ applies a function to the amount of heap that it uses.
It initialises the heap usage to zeros, and passes on an unchanged
heap usage.

It is usually a prelude to performing a GC check, so everything must
be in a tidy and consistent state.

rje: Note the slightly suble fixed point behaviour needed here

\begin{code}
initHeapUsage :: (VirtualHpOffset -> Code) -> Code
initHeapUsage fcode
  = do  { orig_hp_usage <- getHpUsage
        ; setHpUsage initHpUsage
        ; fixC_(\heap_usage2 -> do
                { fcode (heapHWM heap_usage2)
                ; getHpUsage })
        ; setHpUsage orig_hp_usage }

setVirtHp :: VirtualHpOffset -> Code
setVirtHp new_virtHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {virtHp = new_virtHp}) }

getVirtHp :: FCode VirtualHpOffset
getVirtHp
  = do  { hp_usage <- getHpUsage
        ; return (virtHp hp_usage) }

setRealHp ::  VirtualHpOffset -> Code
setRealHp new_realHp
  = do  { hp_usage <- getHpUsage
        ; setHpUsage (hp_usage {realHp = new_realHp}) }

getHpRelOffset :: VirtualHpOffset -> FCode CmmExpr
getHpRelOffset virtual_offset
  = do  { hp_usg <- getHpUsage
        ; return (cmmRegOffW hpReg (hpRel (realHp hp_usg) virtual_offset)) }
\end{code}


%************************************************************************
%*                                                                      *
                Layout of heap objects
%*                                                                      *
%************************************************************************

\begin{code}
layOutDynConstr, layOutStaticConstr
        :: DynFlags
        -> DataCon
        -> [(CgRep,a)]
        -> (ClosureInfo,
            [(a,VirtualHpOffset)])

layOutDynConstr    = layOutConstr False
layOutStaticConstr = layOutConstr True

layOutConstr :: Bool -> DynFlags -> DataCon -> [(CgRep, a)]
             -> (ClosureInfo, [(a, VirtualHpOffset)])
layOutConstr is_static dflags data_con args
   = (mkConInfo dflags is_static data_con tot_wds ptr_wds,
      things_w_offsets)
  where
    (tot_wds,            --  #ptr_wds + #nonptr_wds
     ptr_wds,            --  #ptr_wds
     things_w_offsets) = mkVirtHeapOffsets dflags False{-not a thunk-} args
\end{code}

@mkVirtHeapOffsets@ always returns boxed things with smaller offsets
than the unboxed things, and furthermore, the offsets in the result
list

\begin{code}
mkVirtHeapOffsets
          :: DynFlags
          -> Bool               -- True <=> is a thunk
          -> [(CgRep,a)]        -- Things to make offsets for
          -> (WordOff,          -- _Total_ number of words allocated
              WordOff,          -- Number of words allocated for *pointers*
              [(a, VirtualHpOffset)])
                                -- Things with their offsets from start of
                                --  object in order of increasing offset

-- First in list gets lowest offset, which is initial offset + 1.

mkVirtHeapOffsets dflags is_thunk things
  = let non_void_things               = filterOut (isVoidArg . fst) things
        (ptrs, non_ptrs)              = separateByPtrFollowness non_void_things
        (wds_of_ptrs, ptrs_w_offsets) = mapAccumL computeOffset 0 ptrs
        (tot_wds, non_ptrs_w_offsets) = mapAccumL computeOffset wds_of_ptrs non_ptrs
    in
    (tot_wds, wds_of_ptrs, ptrs_w_offsets ++ non_ptrs_w_offsets)
  where
    hdr_size    | is_thunk   = thunkHdrSize dflags
                | otherwise  = fixedHdrSize dflags

    computeOffset wds_so_far (rep, thing)
      = (wds_so_far + cgRepSizeW rep, (thing, hdr_size + wds_so_far))
\end{code}


%************************************************************************
%*                                                                      *
                Lay out a static closure
%*                                                                      *
%************************************************************************

Make a static closure, adding on any extra padding needed for CAFs,
and adding a static link field if necessary.

\begin{code}
mkStaticClosureFields
        :: DynFlags
        -> ClosureInfo
        -> CostCentreStack
        -> Bool                 -- Has CAF refs
        -> [CmmLit]             -- Payload
        -> [CmmLit]             -- The full closure
mkStaticClosureFields dflags cl_info ccs caf_refs payload
  = mkStaticClosure dflags info_lbl ccs payload padding_wds
        static_link_field saved_info_field
  where
    info_lbl = infoTableLabelFromCI cl_info

    -- CAFs must have consistent layout, regardless of whether they
    -- are actually updatable or not.  The layout of a CAF is:
    --
    --        3 saved_info
    --        2 static_link
    --        1 indirectee
    --        0 info ptr
    --
    -- the static_link and saved_info fields must always be in the same
    -- place.  So we use closureNeedsUpdSpace rather than
    -- closureUpdReqd here:

    is_caf = closureNeedsUpdSpace cl_info

    padding_wds
        | not is_caf = []
        | otherwise  = ASSERT(null payload) [mkIntCLit 0]

    static_link_field
        | is_caf || staticClosureNeedsLink cl_info = [static_link_value]
        | otherwise                                = []

    saved_info_field
        | is_caf     = [mkIntCLit 0]
        | otherwise  = []

        -- for a static constructor which has NoCafRefs, we set the
        -- static link field to a non-zero value so the garbage
        -- collector will ignore it.
    static_link_value
        | caf_refs      = mkIntCLit 0
        | otherwise     = mkIntCLit 1

mkStaticClosure :: DynFlags -> CLabel -> CostCentreStack -> [CmmLit]
  -> [CmmLit] -> [CmmLit] -> [CmmLit] -> [CmmLit]
mkStaticClosure dflags info_lbl ccs payload padding_wds static_link_field saved_info_field
  =  [CmmLabel info_lbl]
  ++ variable_header_words
  ++ concatMap padLitToWord payload
  ++ padding_wds
  ++ static_link_field
  ++ saved_info_field
  where
    variable_header_words
        =  staticGranHdr
        ++ staticParHdr
        ++ staticProfHdr dflags ccs
        ++ staticTickyHdr

padLitToWord :: CmmLit -> [CmmLit]
padLitToWord lit = lit : padding pad_length
  where width = typeWidth (cmmLitType lit)
        pad_length = wORD_SIZE - widthInBytes width :: Int

        padding n | n <= 0 = []
                  | n `rem` 2 /= 0 = CmmInt 0 W8  : padding (n-1)
                  | n `rem` 4 /= 0 = CmmInt 0 W16 : padding (n-2)
                  | n `rem` 8 /= 0 = CmmInt 0 W32 : padding (n-4)
                  | otherwise      = CmmInt 0 W64 : padding (n-8)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[CgHeapery-heap-overflow]{Heap overflow checking}
%*                                                                      *
%************************************************************************

The new code  for heapChecks. For GrAnSim the code for doing a heap check
and doing a context switch has been separated. Especially, the HEAP_CHK
macro only performs a heap check. THREAD_CONTEXT_SWITCH should be used for
doing a context switch. GRAN_FETCH_AND_RESCHEDULE must be put at the
beginning of every slow entry code in order to simulate the fetching of
closures. If fetching is necessary (i.e. current closure is not local) then
an automatic context switch is done.

--------------------------------------------------------------
A heap/stack check at a function or thunk entry point.

\begin{code}
funEntryChecks :: ClosureInfo -> CmmStmts -> Maybe [GlobalReg] -> Code -> Code
funEntryChecks cl_info reg_save_code live code
  = hpStkCheck cl_info True reg_save_code live code

thunkEntryChecks :: ClosureInfo -> Code -> Code
thunkEntryChecks cl_info code
  = hpStkCheck cl_info False noStmts (Just [node]) code

hpStkCheck :: ClosureInfo       -- Function closure
           -> Bool              -- Is a function? (not a thunk)
           -> CmmStmts          -- Register saves
           -> Maybe [GlobalReg] -- Live registers
           -> Code
           -> Code

hpStkCheck cl_info is_fun reg_save_code live code
  =  getFinalStackHW    $ \ spHw -> do
        { sp <- getRealSp
        ; let stk_words = spHw - sp
        ; initHeapUsage $ \ hpHw  -> do
            {   -- Emit heap checks, but be sure to do it lazily so
                -- that the conditionals on hpHw don't cause a black hole
              codeOnly $ do

                dflags <- getDynFlags

                let (node_asst, full_live)
                        | nodeMustPointToIt dflags (closureLFInfo cl_info)
                        = (noStmts, live)
                        | otherwise
                        = (oneStmt (CmmAssign nodeReg (CmmLit (CmmLabel closure_lbl)))
                          ,Just $ node : fromMaybe [] live)
                        -- Strictly speaking, we should tag node here.  But if
                        -- node doesn't point to the closure, the code for the closure
                        -- cannot depend on the value of R1 anyway, so we're safe.

                    full_save_code = node_asst `plusStmts` reg_save_code

                do_checks stk_words hpHw full_save_code rts_label full_live
                tickyAllocHeap hpHw
            ; setRealHp hpHw
            ; code }
        }
  where
    closure_lbl = closureLabelFromCI cl_info


    rts_label | is_fun    = CmmReg (CmmGlobal GCFun)
                                -- Function entry point
              | otherwise = CmmReg (CmmGlobal GCEnter1)
                                -- Thunk or case return
        -- In the thunk/case-return case, R1 points to a closure
        -- which should be (re)-entered after GC
\end{code}

Heap checks in a case alternative are nice and easy, provided this is
a bog-standard algebraic case.  We have in our hand:

       * one return address, on the stack,
       * one return value, in Node.

the canned code for this heap check failure just pushes Node on the
stack, saying 'EnterGHC' to return.  The scheduler will return by
entering the top value on the stack, which in turn will return through
the return address, getting us back to where we were.  This is
therefore only valid if the return value is *lifted* (just being
boxed isn't good enough).

For primitive returns, we have an unlifted value in some register
(either R1 or FloatReg1 or DblReg1).  This means using specialised
heap-check code for these cases.

\begin{code}
altHeapCheck
    :: AltType  -- PolyAlt, PrimAlt, AlgAlt, but *not* UbxTupAlt
                --      (Unboxed tuples are dealt with by ubxTupleHeapCheck)
    -> Code     -- Continuation
    -> Code
altHeapCheck alt_type code
  = initHeapUsage $ \ hpHw -> do
        { codeOnly $ do
             { do_checks 0 {- no stack chk -} hpHw
                         noStmts {- nothign to save -}
                         rts_label live
             ; tickyAllocHeap hpHw }
        ; setRealHp hpHw
        ; code }
  where
    (rts_label, live) = gc_info alt_type

    mkL l = CmmLit . CmmLabel $ mkCmmCodeLabel rtsPackageId (fsLit l)

    gc_info PolyAlt = (mkL "stg_gc_unpt_r1" , Just [node])

        -- Do *not* enter R1 after a heap check in
        -- a polymorphic case.  It might be a function
        -- and the entry code for a function (currently)
        -- applies it
        --
        -- However R1 is guaranteed to be a pointer

    gc_info (AlgAlt _) = (stg_gc_enter1, Just [node])
        -- Enter R1 after the heap check; it's a pointer

    gc_info (PrimAlt tc)
      = case primRepToCgRep (tyConPrimRep tc) of
          VoidArg   -> (mkL "stg_gc_noregs", Just [])
          FloatArg  -> (mkL "stg_gc_f1", Just [FloatReg 1])
          DoubleArg -> (mkL "stg_gc_d1", Just [DoubleReg 1])
          LongArg   -> (mkL "stg_gc_l1", Just [LongReg 1])
                                -- R1 is boxed but unlifted:
          PtrArg    -> (mkL "stg_gc_unpt_r1", Just [node])
                                -- R1 is unboxed:
          NonPtrArg -> (mkL "stg_gc_unbx_r1", Just [node])

    gc_info (UbxTupAlt _) = panic "altHeapCheck"
\end{code}


Unboxed tuple alternatives and let-no-escapes (the two most annoying
constructs to generate code for!)  For unboxed tuple returns, there
are an arbitrary number of possibly unboxed return values, some of
which will be in registers, and the others will be on the stack.  We
always organise the stack-resident fields into pointers &
non-pointers, and pass the number of each to the heap check code.

\begin{code}
unbxTupleHeapCheck
        :: [(Id, GlobalReg)]    -- Live registers
        -> WordOff              -- no. of stack slots containing ptrs
        -> WordOff              -- no. of stack slots containing nonptrs
        -> CmmStmts             -- code to insert in the failure path
        -> Code
        -> Code

unbxTupleHeapCheck regs ptrs nptrs fail_code code
  -- We can't manage more than 255 pointers/non-pointers
  -- in a generic heap check.
  | ptrs > 255 || nptrs > 255 = panic "altHeapCheck"
  | otherwise
  = initHeapUsage $ \ hpHw -> do
        { codeOnly $ do { do_checks 0 {- no stack check -} hpHw
                                    full_fail_code rts_label live
                        ; tickyAllocHeap hpHw }
        ; setRealHp hpHw
        ; code }
  where
    full_fail_code  = fail_code `plusStmts` oneStmt assign_liveness
    assign_liveness = CmmAssign (CmmGlobal (VanillaReg 9 VNonGcPtr))    -- Ho ho ho!
                                (CmmLit (mkWordCLit liveness))
    liveness        = mkRegLiveness regs ptrs nptrs
    live            = Just $ map snd regs
    rts_label       = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "stg_gc_ut")))

\end{code}


%************************************************************************
%*                                                                      *
                Heap/Stack Checks.
%*                                                                      *
%************************************************************************

When failing a check, we save a return address on the stack and
jump to a pre-compiled code fragment that saves the live registers
and returns to the scheduler.

The return address in most cases will be the beginning of the basic
block in which the check resides, since we need to perform the check
again on re-entry because someone else might have stolen the resource
in the meantime.

\begin{code}
do_checks :: WordOff           -- Stack headroom
          -> WordOff           -- Heap  headroom
          -> CmmStmts          -- Assignments to perform on failure
          -> CmmExpr           -- Rts address to jump to on failure
          -> Maybe [GlobalReg] -- Live registers
          -> Code
do_checks 0 0 _ _ _ = nopC

do_checks _ hp _ _ _
  | hp > bLOCKS_PER_MBLOCK * bLOCK_SIZE_W
  = sorry (unlines [
            "Trying to allocate more than " ++ show (bLOCKS_PER_MBLOCK * bLOCK_SIZE) ++ " bytes.",
            "",
            "See: http://hackage.haskell.org/trac/ghc/ticket/4505",
            "Suggestion: read data from a file instead of having large static data",
            "structures in the code."])

do_checks stk hp reg_save_code rts_lbl live
  = do_checks' (CmmLit (mkIntCLit (stk*wORD_SIZE)))
               (CmmLit (mkIntCLit (hp*wORD_SIZE)))
         (stk /= 0) (hp /= 0) reg_save_code rts_lbl live

-- The offsets are now in *bytes*
do_checks' :: CmmExpr -> CmmExpr -> Bool -> Bool -> CmmStmts -> CmmExpr
           -> Maybe [GlobalReg] -> Code
do_checks' stk_expr hp_expr stk_nonzero hp_nonzero reg_save_code rts_lbl live
  = do  { doGranAllocate hp_expr

        -- The failure block: this saves the registers and jumps to
        -- the appropriate RTS stub.
        ; exit_blk_id <- forkLabelledCode $ do {
                        ; emitStmts reg_save_code
                        ; stmtC (CmmJump rts_lbl live) }

        -- In the case of a heap-check failure, we must also set
        -- HpAlloc.  NB. HpAlloc is *only* set if Hp has been
        -- incremented by the heap check, it must not be set in the
        -- event that a stack check failed, because the RTS stub will
        -- retreat Hp by HpAlloc.
        ; hp_blk_id <- if hp_nonzero
                          then forkLabelledCode $ do
                                  stmtC (CmmAssign (CmmGlobal HpAlloc) hp_expr)
                                  stmtC (CmmBranch exit_blk_id)
                          else return exit_blk_id

        -- Check for stack overflow *FIRST*; otherwise
        -- we might bumping Hp and then failing stack oflo
        ; whenC stk_nonzero
                (stmtC (CmmCondBranch stk_oflo exit_blk_id))

        ; whenC hp_nonzero
                (stmtsC [CmmAssign hpReg
                                (cmmOffsetExprB (CmmReg hpReg) hp_expr),
                        CmmCondBranch hp_oflo hp_blk_id])
                -- Bump heap pointer, and test for heap exhaustion
                -- Note that we don't move the heap pointer unless the
                -- stack check succeeds.  Otherwise we might end up
                -- with slop at the end of the current block, which can
                -- confuse the LDV profiler.
    }
  where
        -- Stk overflow if (Sp - stk_bytes < SpLim)
    stk_oflo = CmmMachOp mo_wordULt
                  [CmmMachOp mo_wordSub [CmmReg spReg, stk_expr],
                   CmmReg (CmmGlobal SpLim)]

        -- Hp overflow if (Hp > HpLim)
        -- (Hp has been incremented by now)
        -- HpLim points to the LAST WORD of valid allocation space.
    hp_oflo = CmmMachOp mo_wordUGt
                  [CmmReg hpReg, CmmReg (CmmGlobal HpLim)]
\end{code}

%************************************************************************
%*                                                                      *
     Generic Heap/Stack Checks - used in the RTS
%*                                                                      *
%************************************************************************

\begin{code}
hpChkGen :: CmmExpr -> CmmExpr -> CmmExpr -> Code
hpChkGen bytes liveness reentry
  = do_checks' (CmmLit (mkIntCLit 0)) bytes False True assigns
          stg_gc_gen (Just activeStgRegs)
  where
    assigns = mkStmts [ mk_vanilla_assignment 9 liveness,
                        mk_vanilla_assignment 10 reentry ]

-- a heap check where R1 points to the closure to enter on return, and
-- we want to assign to Sp[0] on failure (used in AutoApply.cmm:BUILD_PAP).
hpChkNodePointsAssignSp0 :: CmmExpr -> CmmExpr -> Code
hpChkNodePointsAssignSp0 bytes sp0
  = do_checks' (CmmLit (mkIntCLit 0)) bytes False True assign
          stg_gc_enter1 (Just [node])
  where assign = oneStmt (CmmStore (CmmReg spReg) sp0)

stkChkGen :: CmmExpr -> CmmExpr -> CmmExpr -> Code
stkChkGen bytes liveness reentry
  = do_checks' bytes (CmmLit (mkIntCLit 0)) True False assigns
          stg_gc_gen (Just activeStgRegs)
  where
    assigns = mkStmts [ mk_vanilla_assignment 9 liveness,
                        mk_vanilla_assignment 10 reentry ]

mk_vanilla_assignment :: Int -> CmmExpr -> CmmStmt
mk_vanilla_assignment n e
  = CmmAssign (CmmGlobal (VanillaReg n (vgcFlag (cmmExprType e)))) e

stkChkNodePoints :: CmmExpr -> Code
stkChkNodePoints bytes
  = do_checks' bytes (CmmLit (mkIntCLit 0)) True False noStmts
          stg_gc_enter1 (Just [node])

stg_gc_gen :: CmmExpr
stg_gc_gen = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit "stg_gc_gen")))
stg_gc_enter1 :: CmmExpr
stg_gc_enter1 = CmmReg (CmmGlobal GCEnter1)
\end{code}

%************************************************************************
%*                                                                      *
\subsection[initClosure]{Initialise a dynamic closure}
%*                                                                      *
%************************************************************************

@allocDynClosure@ puts the thing in the heap, and modifies the virtual Hp
to account for this.

\begin{code}
allocDynClosure
        :: ClosureInfo
        -> CmmExpr              -- Cost Centre to stick in the object
        -> CmmExpr              -- Cost Centre to blame for this alloc
                                -- (usually the same; sometimes "OVERHEAD")

        -> [(CmmExpr, VirtualHpOffset)] -- Offsets from start of the object
                                        -- ie Info ptr has offset zero.
        -> FCode VirtualHpOffset        -- Returns virt offset of object

allocDynClosure cl_info use_cc _blame_cc amodes_with_offsets
  = do  { virt_hp <- getVirtHp

        -- FIND THE OFFSET OF THE INFO-PTR WORD
        ; dflags <- getDynFlags
        ; let   info_offset = virt_hp + 1
                -- info_offset is the VirtualHpOffset of the first
                -- word of the new object
                -- Remember, virtHp points to last allocated word,
                -- ie 1 *before* the info-ptr word of new object.

                info_ptr = CmmLit (CmmLabel (infoTableLabelFromCI cl_info))
                hdr_w_offsets = initDynHdr dflags info_ptr use_cc `zip` [0..]

        -- SAY WHAT WE ARE ABOUT TO DO
        ; profDynAlloc cl_info use_cc
        ; tickyDynAlloc cl_info

        -- ALLOCATE THE OBJECT
        ; base <- getHpRelOffset info_offset
        ; hpStore base (hdr_w_offsets ++ amodes_with_offsets)

        -- BUMP THE VIRTUAL HEAP POINTER
        ; setVirtHp (virt_hp + closureSize dflags cl_info)

        -- RETURN PTR TO START OF OBJECT
        ; returnFC info_offset }


initDynHdr :: DynFlags
           -> CmmExpr
           -> CmmExpr           -- Cost centre to put in object
           -> [CmmExpr]
initDynHdr dflags info_ptr cc
  =  [info_ptr]
        -- ToDo: Gransim stuff
        -- ToDo: Parallel stuff
  ++ dynProfHdr dflags cc
        -- No ticky header

hpStore :: CmmExpr -> [(CmmExpr, VirtualHpOffset)] -> Code
-- Store the item (expr,off) in base[off]
hpStore base es
  = stmtsC [ CmmStore (cmmOffsetW base off) val
           | (val, off) <- es ]

emitSetDynHdr :: CmmExpr -> CmmExpr -> CmmExpr -> Code
emitSetDynHdr base info_ptr ccs
  = do dflags <- getDynFlags
       hpStore base (zip (initDynHdr dflags info_ptr ccs) [0..])
\end{code}
