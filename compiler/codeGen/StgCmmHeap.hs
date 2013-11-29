-----------------------------------------------------------------------------
--
-- Stg to C--: heap management functions
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmHeap (
        getVirtHp, setVirtHp, setRealHp,
        getHpRelOffset, hpRel,

        entryHeapCheck, altHeapCheck, noEscapeHeapCheck, altHeapCheckReturnsTo,
        heapStackCheckGen,
        entryHeapCheck',

        mkStaticClosureFields, mkStaticClosure,

        allocDynClosure, allocDynClosureCmm,
        emitSetDynHdr
    ) where

#include "HsVersions.h"

import StgSyn
import CLabel
import StgCmmLayout
import StgCmmUtils
import StgCmmMonad
import StgCmmProf (profDynAlloc, dynProfHdr, staticProfHdr)
import StgCmmTicky
import StgCmmClosure
import StgCmmEnv

import MkGraph

import Hoopl
import SMRep
import Cmm
import CmmUtils
import CostCentre
import IdInfo( CafInfo(..), mayHaveCafRefs )
import Id ( Id )
import Module
import DynFlags
import FastString( mkFastString, fsLit )

import Control.Monad (when)
import Data.Maybe (isJust)

-----------------------------------------------------------
--              Initialise dynamic heap objects
-----------------------------------------------------------

allocDynClosure
        :: Maybe Id
        -> CmmInfoTable
        -> LambdaFormInfo
        -> CmmExpr              -- Cost Centre to stick in the object
        -> CmmExpr              -- Cost Centre to blame for this alloc
                                -- (usually the same; sometimes "OVERHEAD")

        -> [(NonVoid StgArg, VirtualHpOffset)]  -- Offsets from start of object
                                                -- ie Info ptr has offset zero.
                                                -- No void args in here
        -> FCode CmmExpr -- returns Hp+n

allocDynClosureCmm
        :: Maybe Id -> CmmInfoTable -> LambdaFormInfo -> CmmExpr -> CmmExpr
        -> [(CmmExpr, ByteOff)]
        -> FCode CmmExpr -- returns Hp+n

-- allocDynClosure allocates the thing in the heap,
-- and modifies the virtual Hp to account for this.
-- The second return value is the graph that sets the value of the
-- returned LocalReg, which should point to the closure after executing
-- the graph.

-- allocDynClosure returns an (Hp+8) CmmExpr, and hence the result is
-- only valid until Hp is changed.  The caller should assign the
-- result to a LocalReg if it is required to remain live.
--
-- The reason we don't assign it to a LocalReg here is that the caller
-- is often about to call regIdInfo, which immediately assigns the
-- result of allocDynClosure to a new temp in order to add the tag.
-- So by not generating a LocalReg here we avoid a common source of
-- new temporaries and save some compile time.  This can be quite
-- significant - see test T4801.


allocDynClosure mb_id info_tbl lf_info use_cc _blame_cc args_w_offsets
  = do  { let (args, offsets) = unzip args_w_offsets
        ; cmm_args <- mapM getArgAmode args     -- No void args
        ; allocDynClosureCmm mb_id info_tbl lf_info
                             use_cc _blame_cc (zip cmm_args offsets)
        }

allocDynClosureCmm mb_id info_tbl lf_info use_cc _blame_cc amodes_w_offsets
  = do  { virt_hp <- getVirtHp

        -- SAY WHAT WE ARE ABOUT TO DO
        ; let rep = cit_rep info_tbl
        ; tickyDynAlloc mb_id rep lf_info
        ; profDynAlloc rep use_cc

        -- FIND THE OFFSET OF THE INFO-PTR WORD
        ; let   info_offset = virt_hp + 1
                -- info_offset is the VirtualHpOffset of the first
                -- word of the new object
                -- Remember, virtHp points to last allocated word,
                -- ie 1 *before* the info-ptr word of new object.

                info_ptr = CmmLit (CmmLabel (cit_lbl info_tbl))

        -- ALLOCATE THE OBJECT
        ; base <- getHpRelOffset info_offset
        ; emitComment $ mkFastString "allocDynClosure"
        ; emitSetDynHdr base info_ptr  use_cc
        ; let (cmm_args, offsets) = unzip amodes_w_offsets
        ; hpStore base cmm_args offsets

        -- BUMP THE VIRTUAL HEAP POINTER
        ; dflags <- getDynFlags
        ; setVirtHp (virt_hp + heapClosureSize dflags rep)

        ; getHpRelOffset info_offset
        }

emitSetDynHdr :: CmmExpr -> CmmExpr -> CmmExpr -> FCode ()
emitSetDynHdr base info_ptr ccs
  = do dflags <- getDynFlags
       hpStore base (header dflags) [0, wORD_SIZE dflags ..]
  where
    header :: DynFlags -> [CmmExpr]
    header dflags = [info_ptr] ++ dynProfHdr dflags ccs
        -- ToDof: Parallel stuff
        -- No ticky header

hpStore :: CmmExpr -> [CmmExpr] -> [ByteOff] -> FCode ()
-- Store the item (expr,off) in base[off]
hpStore base vals offs
  = do dflags <- getDynFlags
       let mk_store val off = mkStore (cmmOffsetB dflags base off) val
       emit (catAGraphs (zipWith mk_store vals offs))


-----------------------------------------------------------
--              Layout of static closures
-----------------------------------------------------------

-- Make a static closure, adding on any extra padding needed for CAFs,
-- and adding a static link field if necessary.

mkStaticClosureFields
        :: DynFlags
        -> CmmInfoTable
        -> CostCentreStack
        -> CafInfo
        -> [CmmLit]             -- Payload
        -> [CmmLit]             -- The full closure
mkStaticClosureFields dflags info_tbl ccs caf_refs payload
  = mkStaticClosure dflags info_lbl ccs payload padding
        static_link_field saved_info_field
  where
    info_lbl = cit_lbl info_tbl

    -- CAFs must have consistent layout, regardless of whether they
    -- are actually updatable or not.  The layout of a CAF is:
    --
    --        3 saved_info
    --        2 static_link
    --        1 indirectee
    --        0 info ptr
    --
    -- the static_link and saved_info fields must always be in the
    -- same place.  So we use isThunkRep rather than closureUpdReqd
    -- here:

    is_caf = isThunkRep (cit_rep info_tbl)

    padding
        | is_caf && null payload = [mkIntCLit dflags 0]
        | otherwise = []

    static_link_field
        | is_caf || staticClosureNeedsLink (mayHaveCafRefs caf_refs) info_tbl
        = [static_link_value]
        | otherwise
        = []

    saved_info_field
        | is_caf     = [mkIntCLit dflags 0]
        | otherwise  = []

        -- For a static constructor which has NoCafRefs, we set the
        -- static link field to a non-zero value so the garbage
        -- collector will ignore it.
    static_link_value
        | mayHaveCafRefs caf_refs  = mkIntCLit dflags 0
        | otherwise                = mkIntCLit dflags 1  -- No CAF refs


mkStaticClosure :: DynFlags -> CLabel -> CostCentreStack -> [CmmLit]
  -> [CmmLit] -> [CmmLit] -> [CmmLit] -> [CmmLit]
mkStaticClosure dflags info_lbl ccs payload padding static_link_field saved_info_field
  =  [CmmLabel info_lbl]
  ++ staticProfHdr dflags ccs
  ++ concatMap (padLitToWord dflags) payload
  ++ padding
  ++ static_link_field
  ++ saved_info_field

-- JD: Simon had ellided this padding, but without it the C back end asserts
-- failure. Maybe it's a bad assertion, and this padding is indeed unnecessary?
padLitToWord :: DynFlags -> CmmLit -> [CmmLit]
padLitToWord dflags lit = lit : padding pad_length
  where width = typeWidth (cmmLitType dflags lit)
        pad_length = wORD_SIZE dflags - widthInBytes width :: Int

        padding n | n <= 0 = []
                  | n `rem` 2 /= 0 = CmmInt 0 W8  : padding (n-1)
                  | n `rem` 4 /= 0 = CmmInt 0 W16 : padding (n-2)
                  | n `rem` 8 /= 0 = CmmInt 0 W32 : padding (n-4)
                  | otherwise      = CmmInt 0 W64 : padding (n-8)

-----------------------------------------------------------
--              Heap overflow checking
-----------------------------------------------------------

{- Note [Heap checks]
   ~~~~~~~~~~~~~~~~~~
Heap checks come in various forms.  We provide the following entry
points to the runtime system, all of which use the native C-- entry
convention.

  * gc() performs garbage collection and returns
    nothing to its caller

  * A series of canned entry points like
        r = gc_1p( r )
    where r is a pointer.  This performs gc, and
    then returns its argument r to its caller.

  * A series of canned entry points like
        gcfun_2p( f, x, y )
    where f is a function closure of arity 2
    This performs garbage collection, keeping alive the
    three argument ptrs, and then tail-calls f(x,y)

These are used in the following circumstances

* entryHeapCheck: Function entry
    (a) With a canned GC entry sequence
        f( f_clo, x:ptr, y:ptr ) {
             Hp = Hp+8
             if Hp > HpLim goto L
             ...
          L: HpAlloc = 8
             jump gcfun_2p( f_clo, x, y ) }
     Note the tail call to the garbage collector;
     it should do no register shuffling

    (b) No canned sequence
        f( f_clo, x:ptr, y:ptr, ...etc... ) {
          T: Hp = Hp+8
             if Hp > HpLim goto L
             ...
          L: HpAlloc = 8
             call gc()  -- Needs an info table
             goto T }

* altHeapCheck: Immediately following an eval
  Started as
        case f x y of r { (p,q) -> rhs }
  (a) With a canned sequence for the results of f
       (which is the very common case since
       all boxed cases return just one pointer
           ...
           r = f( x, y )
        K:      -- K needs an info table
           Hp = Hp+8
           if Hp > HpLim goto L
           ...code for rhs...

        L: r = gc_1p( r )
           goto K }

        Here, the info table needed by the call
        to gc_1p should be the *same* as the
        one for the call to f; the C-- optimiser
        spots this sharing opportunity)

   (b) No canned sequence for results of f
       Note second info table
           ...
           (r1,r2,r3) = call f( x, y )
        K:
           Hp = Hp+8
           if Hp > HpLim goto L
           ...code for rhs...

        L: call gc()    -- Extra info table here
           goto K

* generalHeapCheck: Anywhere else
  e.g. entry to thunk
       case branch *not* following eval,
       or let-no-escape
  Exactly the same as the previous case:

        K:      -- K needs an info table
           Hp = Hp+8
           if Hp > HpLim goto L
           ...

        L: call gc()
           goto K
-}

--------------------------------------------------------------
-- A heap/stack check at a function or thunk entry point.

entryHeapCheck :: ClosureInfo
               -> Maybe LocalReg -- Function (closure environment)
               -> Int            -- Arity -- not same as len args b/c of voids
               -> [LocalReg]     -- Non-void args (empty for thunk)
               -> FCode ()
               -> FCode ()

entryHeapCheck cl_info nodeSet arity args code
  = entryHeapCheck' is_fastf node arity args code
  where
    node = case nodeSet of
              Just r  -> CmmReg (CmmLocal r)
              Nothing -> CmmLit (CmmLabel $ staticClosureLabel cl_info)

    is_fastf = case closureFunInfo cl_info of
                 Just (_, ArgGen _) -> False
                 _otherwise         -> True

-- | lower-level version for CmmParse
entryHeapCheck' :: Bool           -- is a known function pattern
                -> CmmExpr        -- expression for the closure pointer
                -> Int            -- Arity -- not same as len args b/c of voids
                -> [LocalReg]     -- Non-void args (empty for thunk)
                -> FCode ()
                -> FCode ()
entryHeapCheck' is_fastf node arity args code
  = do dflags <- getDynFlags
       let is_thunk = arity == 0

           args' = map (CmmReg . CmmLocal) args
           stg_gc_fun    = CmmReg (CmmGlobal GCFun)
           stg_gc_enter1 = CmmReg (CmmGlobal GCEnter1)

           {- Thunks:          jump stg_gc_enter_1

              Function (fast): call (NativeNode) stg_gc_fun(fun, args)

              Function (slow): call (slow) stg_gc_fun(fun, args)
           -}
           gc_call upd
               | is_thunk
                 = mkJump dflags NativeNodeCall stg_gc_enter1 [node] upd

               | is_fastf
                 = mkJump dflags NativeNodeCall stg_gc_fun (node : args') upd

               | otherwise
                 = mkJump dflags Slow stg_gc_fun (node : args') upd

       updfr_sz <- getUpdFrameOff

       loop_id <- newLabelC
       emitLabel loop_id
       heapCheck True True (gc_call updfr_sz <*> mkBranch loop_id) code

-- ------------------------------------------------------------
-- A heap/stack check in a case alternative


-- If there are multiple alts and we need to GC, but don't have a
-- continuation already (the scrut was simple), then we should
-- pre-generate the continuation.  (if there are multiple alts it is
-- always a canned GC point).

-- altHeapCheck:
-- If we have a return continuation,
--   then if it is a canned GC pattern,
--           then we do mkJumpReturnsTo
--           else we do a normal call to stg_gc_noregs
--   else if it is a canned GC pattern,
--           then generate the continuation and do mkCallReturnsTo
--           else we do a normal call to stg_gc_noregs

altHeapCheck :: [LocalReg] -> FCode a -> FCode a
altHeapCheck regs code = altOrNoEscapeHeapCheck False regs code

altOrNoEscapeHeapCheck :: Bool -> [LocalReg] -> FCode a -> FCode a
altOrNoEscapeHeapCheck checkYield regs code = do
    dflags <- getDynFlags
    case cannedGCEntryPoint dflags regs of
      Nothing -> genericGC checkYield code
      Just gc -> do
        lret <- newLabelC
        let (off, _, copyin) = copyInOflow dflags NativeReturn (Young lret) regs []
        lcont <- newLabelC
        emitOutOfLine lret (copyin <*> mkBranch lcont)
        emitLabel lcont
        cannedGCReturnsTo checkYield False gc regs lret off code

altHeapCheckReturnsTo :: [LocalReg] -> Label -> ByteOff -> FCode a -> FCode a
altHeapCheckReturnsTo regs lret off code
  = do dflags <- getDynFlags
       case cannedGCEntryPoint dflags regs of
           Nothing -> genericGC False code
           Just gc -> cannedGCReturnsTo False True gc regs lret off code

-- noEscapeHeapCheck is implemented identically to altHeapCheck (which
-- is more efficient), but cannot be optimized away in the non-allocating
-- case because it may occur in a loop
noEscapeHeapCheck :: [LocalReg] -> FCode a -> FCode a
noEscapeHeapCheck regs code = altOrNoEscapeHeapCheck True regs code

cannedGCReturnsTo :: Bool -> Bool -> CmmExpr -> [LocalReg] -> Label -> ByteOff
                  -> FCode a
                  -> FCode a
cannedGCReturnsTo checkYield cont_on_stack gc regs lret off code
  = do dflags <- getDynFlags
       updfr_sz <- getUpdFrameOff
       heapCheck False checkYield (gc_call dflags gc updfr_sz) code
  where
    reg_exprs = map (CmmReg . CmmLocal) regs
      -- Note [stg_gc arguments]

      -- NB. we use the NativeReturn convention for passing arguments
      -- to the canned heap-check routines, because we are in a case
      -- alternative and hence the [LocalReg] was passed to us in the
      -- NativeReturn convention.
    gc_call dflags label sp
      | cont_on_stack
      = mkJumpReturnsTo dflags label NativeReturn reg_exprs lret off sp
      | otherwise
      = mkCallReturnsTo dflags label NativeReturn reg_exprs lret off sp []

genericGC :: Bool -> FCode a -> FCode a
genericGC checkYield code
  = do updfr_sz <- getUpdFrameOff
       lretry <- newLabelC
       emitLabel lretry
       call <- mkCall generic_gc (GC, GC) [] [] updfr_sz []
       heapCheck False checkYield (call <*> mkBranch lretry) code

cannedGCEntryPoint :: DynFlags -> [LocalReg] -> Maybe CmmExpr
cannedGCEntryPoint dflags regs
  = case map localRegType regs of
      []  -> Just (mkGcLabel "stg_gc_noregs")
      [ty]
          | isGcPtrType ty -> Just (mkGcLabel "stg_gc_unpt_r1")
          | isFloatType ty -> case width of
                                  W32       -> Just (mkGcLabel "stg_gc_f1")
                                  W64       -> Just (mkGcLabel "stg_gc_d1")
                                  _         -> Nothing

          | width == wordWidth dflags -> Just (mkGcLabel "stg_gc_unbx_r1")
          | width == W64              -> Just (mkGcLabel "stg_gc_l1")
          | otherwise                 -> Nothing
          where
              width = typeWidth ty
      [ty1,ty2]
          |  isGcPtrType ty1
          && isGcPtrType ty2 -> Just (mkGcLabel "stg_gc_pp")
      [ty1,ty2,ty3]
          |  isGcPtrType ty1
          && isGcPtrType ty2
          && isGcPtrType ty3 -> Just (mkGcLabel "stg_gc_ppp")
      [ty1,ty2,ty3,ty4]
          |  isGcPtrType ty1
          && isGcPtrType ty2
          && isGcPtrType ty3
          && isGcPtrType ty4 -> Just (mkGcLabel "stg_gc_pppp")
      _otherwise -> Nothing

-- Note [stg_gc arguments]
-- It might seem that we could avoid passing the arguments to the
-- stg_gc function, because they are already in the right registers.
-- While this is usually the case, it isn't always.  Sometimes the
-- code generator has cleverly avoided the eval in a case, e.g. in
-- ffi/should_run/4221.hs we found
--
--   case a_r1mb of z
--     FunPtr x y -> ...
--
-- where a_r1mb is bound a top-level constructor, and is known to be
-- evaluated.  The codegen just assigns x, y and z, and continues;
-- R1 is never assigned.
--
-- So we'll have to rely on optimisations to eliminatethese
-- assignments where possible.


-- | The generic GC procedure; no params, no results
generic_gc :: CmmExpr
generic_gc = mkGcLabel "stg_gc_noregs"

-- | Create a CLabel for calling a garbage collector entry point
mkGcLabel :: String -> CmmExpr
mkGcLabel s = CmmLit (CmmLabel (mkCmmCodeLabel rtsPackageId (fsLit s)))

-------------------------------
heapCheck :: Bool -> Bool -> CmmAGraph -> FCode a -> FCode a
heapCheck checkStack checkYield do_gc code
  = getHeapUsage $ \ hpHw ->
    -- Emit heap checks, but be sure to do it lazily so
    -- that the conditionals on hpHw don't cause a black hole
    do  { dflags <- getDynFlags
        ; let mb_alloc_bytes
                 | hpHw > 0  = Just (mkIntExpr dflags (hpHw * (wORD_SIZE dflags)))
                 | otherwise = Nothing
              stk_hwm | checkStack = Just (CmmLit CmmHighStackMark)
                      | otherwise  = Nothing
        ; codeOnly $ do_checks stk_hwm checkYield mb_alloc_bytes do_gc
        ; tickyAllocHeap True hpHw
        ; setRealHp hpHw
        ; code }

heapStackCheckGen :: Maybe CmmExpr -> Maybe CmmExpr -> FCode ()
heapStackCheckGen stk_hwm mb_bytes
  = do updfr_sz <- getUpdFrameOff
       lretry <- newLabelC
       emitLabel lretry
       call <- mkCall generic_gc (GC, GC) [] [] updfr_sz []
       do_checks stk_hwm False mb_bytes (call <*> mkBranch lretry)

-- Note [Single stack check]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- When compiling a function we can determine how much stack space it
-- will use. We therefore need to perform only a single stack check at
-- the beginning of a function to see if we have enough stack space.
--
-- The check boils down to comparing Sp-N with SpLim, where N is the
-- amount of stack space needed (see Note [Stack usage] below).  *BUT*
-- at this stage of the pipeline we are not supposed to refer to Sp
-- itself, because the stack is not yet manifest, so we don't quite
-- know where Sp pointing.

-- So instead of referring directly to Sp - as we used to do in the
-- past - the code generator uses (old + 0) in the stack check. That
-- is the address of the first word of the old area, so if we add N
-- we'll get the address of highest used word.
--
-- This makes the check robust.  For example, while we need to perform
-- only one stack check for each function, we could in theory place
-- more stack checks later in the function. They would be redundant,
-- but not incorrect (in a sense that they should not change program
-- behaviour). We need to make sure however that a stack check
-- inserted after incrementing the stack pointer checks for a
-- respectively smaller stack space. This would not be the case if the
-- code generator produced direct references to Sp. By referencing
-- (old + 0) we make sure that we always check for a correct amount of
-- stack: when converting (old + 0) to Sp the stack layout phase takes
-- into account changes already made to stack pointer. The idea for
-- this change came from observations made while debugging #8275.

-- Note [Stack usage]
-- ~~~~~~~~~~~~~~~~~~
-- At the moment we convert from STG to Cmm we don't know N, the
-- number of bytes of stack that the function will use, so we use a
-- special late-bound CmmLit, namely
--       CmmHighStackMark
-- to stand for the number of bytes needed. When the stack is made
-- manifest, the number of bytes needed is calculated, and used to
-- replace occurrences of CmmHighStackMark
--
-- The (Maybe CmmExpr) passed to do_checks is usually
--     Just (CmmLit CmmHighStackMark)
-- but can also (in certain hand-written RTS functions)
--     Just (CmmLit 8)  or some other fixed valuet
-- If it is Nothing, we don't generate a stack check at all.

do_checks :: Maybe CmmExpr    -- Should we check the stack?
                              -- See Note [Stack usage]
          -> Bool             -- Should we check for preemption?
          -> Maybe CmmExpr    -- Heap headroom (bytes)
          -> CmmAGraph        -- What to do on failure
          -> FCode ()
do_checks mb_stk_hwm checkYield mb_alloc_lit do_gc = do
  dflags <- getDynFlags
  gc_id <- newLabelC

  let
    Just alloc_lit = mb_alloc_lit

    bump_hp   = cmmOffsetExprB dflags (CmmReg hpReg) alloc_lit

    -- Sp overflow if ((old + 0) - CmmHighStack < SpLim)
    -- At the beginning of a function old + 0 = Sp
    -- See Note [Single stack check]
    sp_oflo sp_hwm =
         CmmMachOp (mo_wordULt dflags)
                  [CmmMachOp (MO_Sub (typeWidth (cmmRegType dflags spReg)))
                             [CmmStackSlot Old 0, sp_hwm],
                   CmmReg spLimReg]

    -- Hp overflow if (Hp > HpLim)
    -- (Hp has been incremented by now)
    -- HpLim points to the LAST WORD of valid allocation space.
    hp_oflo = CmmMachOp (mo_wordUGt dflags)
                  [CmmReg hpReg, CmmReg (CmmGlobal HpLim)]

    alloc_n = mkAssign (CmmGlobal HpAlloc) alloc_lit

  case mb_stk_hwm of
    Nothing -> return ()
    Just stk_hwm -> tickyStackCheck >> (emit =<< mkCmmIfGoto (sp_oflo stk_hwm) gc_id)

  -- Emit new label that might potentially be a header
  -- of a self-recursive tail call.
  -- See Note [Self-recursive loop header].
  self_loop_info <- getSelfLoop
  case self_loop_info of
    Just (_, loop_header_id, _)
        | checkYield && isJust mb_stk_hwm -> emitLabel loop_header_id
    _otherwise -> return ()

  if (isJust mb_alloc_lit)
    then do
     tickyHeapCheck
     emitAssign hpReg bump_hp
     emit =<< mkCmmIfThen hp_oflo (alloc_n <*> mkBranch gc_id)
    else do
      when (checkYield && not (gopt Opt_OmitYields dflags)) $ do
         -- Yielding if HpLim == 0
         let yielding = CmmMachOp (mo_wordEq dflags)
                                  [CmmReg (CmmGlobal HpLim),
                                   CmmLit (zeroCLit dflags)]
         emit =<< mkCmmIfGoto yielding gc_id

  emitOutOfLine gc_id $
     do_gc -- this is expected to jump back somewhere

                -- Test for stack pointer exhaustion, then
                -- bump heap pointer, and test for heap exhaustion
                -- Note that we don't move the heap pointer unless the
                -- stack check succeeds.  Otherwise we might end up
                -- with slop at the end of the current block, which can
                -- confuse the LDV profiler.

-- Note [Self-recursive loop header]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Self-recursive loop header is required by loopification optimization (See
-- Note [Self-recursive tail calls] in StgCmmExpr). We emit it if:
--
--  1. There is information about self-loop in the FCode environment. We don't
--     check the binder (first component of the self_loop_info) because we are
--     certain that if the self-loop info is present then we are compiling the
--     binder body. Reason: the only possible way to get here with the
--     self_loop_info present is from closureCodeBody.
--
--  2. checkYield && isJust mb_stk_hwm. checkYield tells us that it is possible
--     to preempt the heap check (see #367 for motivation behind this check). It
--     is True for heap checks placed at the entry to a function and
--     let-no-escape heap checks but false for other heap checks (eg. in case
--     alternatives or created from hand-written high-level Cmm). The second
--     check (isJust mb_stk_hwm) is true for heap checks at the entry to a
--     function and some heap checks created in hand-written Cmm. Otherwise it
--     is Nothing. In other words the only situation when both conditions are
--     true is when compiling stack and heap checks at the entry to a
--     function. This is the only situation when we want to emit a self-loop
--     label.
