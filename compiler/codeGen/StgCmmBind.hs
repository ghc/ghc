-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: bindings
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module StgCmmBind (
        cgTopRhsClosure,
        cgBind,
        emitBlackHoleCode,
        pushUpdateFrame, emitUpdateFrame
  ) where

#include "HsVersions.h"

import StgCmmExpr
import StgCmmMonad
import StgCmmEnv
import StgCmmCon
import StgCmmHeap
import StgCmmProf (curCCS, ldvEnterClosure, enterCostCentreFun, enterCostCentreThunk,
                   initUpdFrameProf, costCentreFrom)
import StgCmmTicky
import StgCmmLayout
import StgCmmUtils
import StgCmmClosure
import StgCmmForeign    (emitPrimCall)

import MkGraph
import CoreSyn          ( AltCon(..) )
import SMRep
import Cmm
import CmmInfo
import CmmUtils
import CLabel
import StgSyn
import CostCentre
import Id
import IdInfo
import Name
import Module
import ListSetOps
import Util
import BasicTypes
import Outputable
import FastString
import Maybes
import DynFlags

import Control.Monad

------------------------------------------------------------------------
--              Top-level bindings
------------------------------------------------------------------------

-- For closures bound at top level, allocate in static space.
-- They should have no free variables.

cgTopRhsClosure :: DynFlags
                -> RecFlag              -- member of a recursive group?
                -> Id
                -> CostCentreStack      -- Optional cost centre annotation
                -> StgBinderInfo
                -> UpdateFlag
                -> [Id]                 -- Args
                -> StgExpr
                -> (CgIdInfo, FCode ())

cgTopRhsClosure dflags rec id ccs _ upd_flag args body =
  let closure_label = mkLocalClosureLabel (idName id) (idCafInfo id)
      cg_id_info    = litIdInfo dflags id lf_info (CmmLabel closure_label)
      lf_info       = mkClosureLFInfo dflags id TopLevel [] upd_flag args
  in (cg_id_info, gen_code dflags lf_info closure_label)
  where
  -- special case for a indirection (f = g).  We create an IND_STATIC
  -- closure pointing directly to the indirectee.  This is exactly
  -- what the CAF will eventually evaluate to anyway, we're just
  -- shortcutting the whole process, and generating a lot less code
  -- (#7308)
  --
  -- Note: we omit the optimisation when this binding is part of a
  -- recursive group, because the optimisation would inhibit the black
  -- hole detection from working in that case.  Test
  -- concurrent/should_run/4030 fails, for instance.
  --
  gen_code dflags _ closure_label
    | StgApp f [] <- body, null args, isNonRec rec
    = do
         cg_info <- getCgIdInfo f
         let closure_rep   = mkStaticClosureFields dflags
                                    indStaticInfoTable ccs MayHaveCafRefs
                                    [unLit (idInfoToAmode cg_info)]
         emitDataLits closure_label closure_rep
         return ()

  gen_code dflags lf_info closure_label
   = do {     -- LAY OUT THE OBJECT
          let name = idName id
        ; mod_name <- getModuleName
        ; let descr         = closureDescription dflags mod_name name
              closure_info  = mkClosureInfo dflags True id lf_info 0 0 descr

              caffy         = idCafInfo id
              info_tbl      = mkCmmInfo closure_info -- XXX short-cut
              closure_rep   = mkStaticClosureFields dflags info_tbl ccs caffy []

                 -- BUILD THE OBJECT, AND GENERATE INFO TABLE (IF NECESSARY)
        ; emitDataLits closure_label closure_rep
        ; let fv_details :: [(NonVoid Id, VirtualHpOffset)]
              (_, _, fv_details) = mkVirtHeapOffsets dflags (isLFThunk lf_info)
                                               (addIdReps [])
        -- Don't drop the non-void args until the closure info has been made
        ; forkClosureBody (closureCodeBody True id closure_info ccs
                                (nonVoidIds args) (length args) body fv_details)

        ; return () }

  unLit (CmmLit l) = l
  unLit _ = panic "unLit"

------------------------------------------------------------------------
--              Non-top-level bindings
------------------------------------------------------------------------

cgBind :: StgBinding -> FCode ()
cgBind (StgNonRec name rhs)
  = do  { (info, fcode) <- cgRhs name rhs
        ; addBindC info
        ; init <- fcode
        ; emit init }
        -- init cannot be used in body, so slightly better to sink it eagerly

cgBind (StgRec pairs)
  = do  {  r <- sequence $ unzipWith cgRhs pairs
        ;  let (id_infos, fcodes) = unzip r
        ;  addBindsC id_infos
        ;  (inits, body) <- getCodeR $ sequence fcodes
        ;  emit (catAGraphs inits <*> body) }

{- Note [cgBind rec]

   Recursive let-bindings are tricky.
   Consider the following pseudocode:

     let x = \_ ->  ... y ...
         y = \_ ->  ... z ...
         z = \_ ->  ... x ...
     in ...

   For each binding, we need to allocate a closure, and each closure must
   capture the address of the other closures.
   We want to generate the following C-- code:
     // Initialization Code
     x = hp - 24; // heap address of x's closure
     y = hp - 40; // heap address of x's closure
     z = hp - 64; // heap address of x's closure
     // allocate and initialize x
     m[hp-8]   = ...
     m[hp-16]  = y       // the closure for x captures y
     m[hp-24] = x_info;
     // allocate and initialize y
     m[hp-32] = z;       // the closure for y captures z
     m[hp-40] = y_info;
     // allocate and initialize z
     ...

   For each closure, we must generate not only the code to allocate and
   initialize the closure itself, but also some initialization Code that
   sets a variable holding the closure pointer.

   We could generate a pair of the (init code, body code), but since
   the bindings are recursive we also have to initialise the
   environment with the CgIdInfo for all the bindings before compiling
   anything.  So we do this in 3 stages:

     1. collect all the CgIdInfos and initialise the environment
     2. compile each binding into (init, body) code
     3. emit all the inits, and then all the bodies

   We'd rather not have separate functions to do steps 1 and 2 for
   each binding, since in pratice they share a lot of code.  So we
   have just one function, cgRhs, that returns a pair of the CgIdInfo
   for step 1, and a monadic computation to generate the code in step
   2.

   The alternative to separating things in this way is to use a
   fixpoint.  That's what we used to do, but it introduces a
   maintenance nightmare because there is a subtle dependency on not
   being too strict everywhere.  Doing things this way means that the
   FCode monad can be strict, for example.
 -}

cgRhs :: Id
      -> StgRhs
      -> FCode (
                 CgIdInfo         -- The info for this binding
               , FCode CmmAGraph  -- A computation which will generate the
                                  -- code for the binding, and return an
                                  -- assignent of the form "x = Hp - n"
                                  -- (see above)
               )

cgRhs id (StgRhsCon cc con args)
  = withNewTickyCounterThunk False (idName id) $ -- False for "not static"
    buildDynCon id True cc con args

{- See Note [GC recovery] in compiler/codeGen/StgCmmClosure.hs -}
cgRhs name (StgRhsClosure cc bi fvs upd_flag _srt args body)
  = do dflags <- getDynFlags
       mkRhsClosure dflags name cc bi (nonVoidIds fvs) upd_flag args body

------------------------------------------------------------------------
--              Non-constructor right hand sides
------------------------------------------------------------------------

mkRhsClosure :: DynFlags -> Id -> CostCentreStack -> StgBinderInfo
             -> [NonVoid Id]                    -- Free vars
             -> UpdateFlag
             -> [Id]                            -- Args
             -> StgExpr
             -> FCode (CgIdInfo, FCode CmmAGraph)

{- mkRhsClosure looks for two special forms of the right-hand side:
        a) selector thunks
        b) AP thunks

If neither happens, it just calls mkClosureLFInfo.  You might think
that mkClosureLFInfo should do all this, but it seems wrong for the
latter to look at the structure of an expression

Note [Selectors]
~~~~~~~~~~~~~~~~
We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
         case the_fv of
           con a_1 ... a_n -> a_i

Note [Ap thunks]
~~~~~~~~~~~~~~~~
A more generic AP thunk of the form

        x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.

-}

---------- Note [Selectors] ------------------
mkRhsClosure    dflags bndr _cc _bi
                [NonVoid the_fv]                -- Just one free var
                upd_flag                -- Updatable thunk
                []                      -- A thunk
                (StgCase (StgApp scrutinee [{-no args-}])
                      _ _ _ _   -- ignore uniq, etc.
                      (AlgAlt _)
                      [(DataAlt _, params, _use_mask,
                            (StgApp selectee [{-no args-}]))])
  |  the_fv == scrutinee                -- Scrutinee is the only free variable
  && maybeToBool maybe_offset           -- Selectee is a component of the tuple
  && offset_into_int <= mAX_SPEC_SELECTEE_SIZE dflags -- Offset is small enough
  = -- NOT TRUE: ASSERT(is_single_constructor)
    -- The simplifier may have statically determined that the single alternative
    -- is the only possible case and eliminated the others, even if there are
    -- other constructors in the datatype.  It's still ok to make a selector
    -- thunk in this case, because we *know* which constructor the scrutinee
    -- will evaluate to.
    --
    -- srt is discarded; it must be empty
    cgRhsStdThunk bndr lf_info [StgVarArg the_fv]
  where
    lf_info               = mkSelectorLFInfo bndr offset_into_int
                                 (isUpdatable upd_flag)
    (_, _, params_w_offsets) = mkVirtConstrOffsets dflags (addIdReps params)
                               -- Just want the layout
    maybe_offset          = assocMaybe params_w_offsets (NonVoid selectee)
    Just the_offset       = maybe_offset
    offset_into_int       = the_offset - fixedHdrSize dflags

---------- Note [Ap thunks] ------------------
mkRhsClosure    dflags bndr _cc _bi
                fvs
                upd_flag
                []                      -- No args; a thunk
                (StgApp fun_id args)

  | args `lengthIs` (arity-1)
        && all (isGcPtrRep . idPrimRep . unsafe_stripNV) fvs
        && isUpdatable upd_flag
        && arity <= mAX_SPEC_AP_SIZE dflags
        && not (gopt Opt_SccProfilingOn dflags)
                                  -- not when profiling: we don't want to
                                  -- lose information about this particular
                                  -- thunk (e.g. its type) (#949)

                   -- Ha! an Ap thunk
  = cgRhsStdThunk bndr lf_info payload

  where
        lf_info = mkApLFInfo bndr upd_flag arity
        -- the payload has to be in the correct order, hence we can't
        -- just use the fvs.
        payload = StgVarArg fun_id : args
        arity   = length fvs

---------- Default case ------------------
mkRhsClosure dflags bndr cc _ fvs upd_flag args body
  = do  { let lf_info = mkClosureLFInfo dflags bndr NotTopLevel fvs upd_flag args
        ; (id_info, reg) <- rhsIdInfo bndr lf_info
        ; return (id_info, gen_code lf_info reg) }
 where
 gen_code lf_info reg
  = do  {       -- LAY OUT THE OBJECT
        -- If the binder is itself a free variable, then don't store
        -- it in the closure.  Instead, just bind it to Node on entry.
        -- NB we can be sure that Node will point to it, because we
        -- haven't told mkClosureLFInfo about this; so if the binder
        -- _was_ a free var of its RHS, mkClosureLFInfo thinks it *is*
        -- stored in the closure itself, so it will make sure that
        -- Node points to it...
        ; let
                is_elem      = isIn "cgRhsClosure"
                bndr_is_a_fv = (NonVoid bndr) `is_elem` fvs
                reduced_fvs | bndr_is_a_fv = fvs `minusList` [NonVoid bndr]
                            | otherwise    = fvs


        -- MAKE CLOSURE INFO FOR THIS CLOSURE
        ; mod_name <- getModuleName
        ; dflags <- getDynFlags
        ; let   name  = idName bndr
                descr = closureDescription dflags mod_name name
                fv_details :: [(NonVoid Id, VirtualHpOffset)]
                (tot_wds, ptr_wds, fv_details)
                   = mkVirtHeapOffsets dflags (isLFThunk lf_info)
                                       (addIdReps (map unsafe_stripNV reduced_fvs))
                closure_info = mkClosureInfo dflags False       -- Not static
                                             bndr lf_info tot_wds ptr_wds
                                             descr

        -- BUILD ITS INFO TABLE AND CODE
        ; forkClosureBody $
                -- forkClosureBody: (a) ensure that bindings in here are not seen elsewhere
                --                  (b) ignore Sequel from context; use empty Sequel
                -- And compile the body
                closureCodeBody False bndr closure_info cc (nonVoidIds args)
                                (length args) body fv_details

        -- BUILD THE OBJECT
--      ; (use_cc, blame_cc) <- chooseDynCostCentres cc args body
        ; let use_cc = curCCS; blame_cc = curCCS
        ; emit (mkComment $ mkFastString "calling allocDynClosure")
        ; let toVarArg (NonVoid a, off) = (NonVoid (StgVarArg a), off)
        ; let info_tbl = mkCmmInfo closure_info
        ; hp_plus_n <- allocDynClosure (Just bndr) info_tbl lf_info use_cc blame_cc
                                         (map toVarArg fv_details)

        -- RETURN
        ; return (mkRhsInit dflags reg lf_info hp_plus_n) }

-------------------------
cgRhsStdThunk
        :: Id
        -> LambdaFormInfo
        -> [StgArg]             -- payload
        -> FCode (CgIdInfo, FCode CmmAGraph)

cgRhsStdThunk bndr lf_info payload
 = do  { (id_info, reg) <- rhsIdInfo bndr lf_info
       ; return (id_info, gen_code reg)
       }
 where
 gen_code reg  -- AHA!  A STANDARD-FORM THUNK
  = withNewTickyCounterStdThunk False (idName bndr) $ -- False for "not static"
    do
  {     -- LAY OUT THE OBJECT
    mod_name <- getModuleName
  ; dflags <- getDynFlags
  ; let (tot_wds, ptr_wds, payload_w_offsets)
            = mkVirtHeapOffsets dflags (isLFThunk lf_info) (addArgReps payload)

        descr = closureDescription dflags mod_name (idName bndr)
        closure_info = mkClosureInfo dflags False       -- Not static
                                     bndr lf_info tot_wds ptr_wds
                                     descr

--  ; (use_cc, blame_cc) <- chooseDynCostCentres cc [{- no args-}] body
  ; let use_cc = curCCS; blame_cc = curCCS

  ; tickyEnterStdThunk closure_info

        -- BUILD THE OBJECT
  ; let info_tbl = mkCmmInfo closure_info
  ; hp_plus_n <- allocDynClosure (Just bndr) info_tbl lf_info
                                   use_cc blame_cc payload_w_offsets

        -- RETURN
  ; return (mkRhsInit dflags reg lf_info hp_plus_n) }


mkClosureLFInfo :: DynFlags
                -> Id           -- The binder
                -> TopLevelFlag -- True of top level
                -> [NonVoid Id] -- Free vars
                -> UpdateFlag   -- Update flag
                -> [Id]         -- Args
                -> LambdaFormInfo
mkClosureLFInfo dflags bndr top fvs upd_flag args
  | null args =
        mkLFThunk (idType bndr) top (map unsafe_stripNV fvs) upd_flag
  | otherwise =
        mkLFReEntrant top (map unsafe_stripNV fvs) args (mkArgDescr dflags args)


------------------------------------------------------------------------
--              The code for closures
------------------------------------------------------------------------

closureCodeBody :: Bool            -- whether this is a top-level binding
                -> Id              -- the closure's name
                -> ClosureInfo     -- Lots of information about this closure
                -> CostCentreStack -- Optional cost centre attached to closure
                -> [NonVoid Id]    -- incoming args to the closure
                -> Int             -- arity, including void args
                -> StgExpr
                -> [(NonVoid Id, VirtualHpOffset)] -- the closure's free vars
                -> FCode ()

{- There are two main cases for the code for closures.

* If there are *no arguments*, then the closure is a thunk, and not in
  normal form. So it should set up an update frame (if it is
  shared). NB: Thunks cannot have a primitive type!

* If there is *at least one* argument, then this closure is in
  normal form, so there is no need to set up an update frame.

  The Macros for GrAnSim are produced at the beginning of the
  argSatisfactionCheck (by calling fetchAndReschedule).
  There info if Node points to closure is available. -- HWL -}

closureCodeBody top_lvl bndr cl_info cc _args arity body fv_details
  | arity == 0 -- No args i.e. thunk
  = withNewTickyCounterThunk (isStaticClosure cl_info) (closureName cl_info) $
    emitClosureProcAndInfoTable top_lvl bndr lf_info info_tbl [] $
      \(_, node, _) -> thunkCode cl_info fv_details cc node arity body
   where
     lf_info  = closureLFInfo cl_info
     info_tbl = mkCmmInfo cl_info

closureCodeBody top_lvl bndr cl_info cc args arity body fv_details
  = -- Note: args may be [], if all args are Void
    withNewTickyCounterFun (closureName cl_info) args $ do {

        ; let
             lf_info  = closureLFInfo cl_info
             info_tbl = mkCmmInfo cl_info

        -- Emit the main entry code
        ; emitClosureProcAndInfoTable top_lvl bndr lf_info info_tbl args $
            \(_offset, node, arg_regs) -> do
                -- Emit slow-entry code (for entering a closure through a PAP)
                { mkSlowEntryCode bndr cl_info arg_regs

                ; dflags <- getDynFlags
                ; let node_points = nodeMustPointToIt dflags lf_info
                      node' = if node_points then Just node else Nothing
                ; when node_points (ldvEnterClosure cl_info)
                -- Emit new label that might potentially be a header
                -- of a self-recursive tail call. See Note
                -- [Self-recursive tail calls] in StgCmmExpr
                ; loop_header_id <- newLabelC
                ; emitLabel loop_header_id
                -- Extend reader monad with information that
                -- self-recursive tail calls can be optimized into local
                -- jumps
                ; withSelfLoop (bndr, loop_header_id, arg_regs) $ do
                {
                -- Main payload
                ; entryHeapCheck cl_info node' arity arg_regs $ do
                { -- ticky after heap check to avoid double counting
                  tickyEnterFun cl_info
                ; enterCostCentreFun cc
                    (CmmMachOp (mo_wordSub dflags)
                         [ CmmReg (CmmLocal node) -- not nodeReg, see #8275
                         , mkIntExpr dflags (funTag dflags cl_info) ])
                ; fv_bindings <- mapM bind_fv fv_details
                -- Load free vars out of closure *after*
                -- heap check, to reduce live vars over check
                ; when node_points $ load_fvs node lf_info fv_bindings
                ; void $ cgExpr body
                }}}

  }

-- A function closure pointer may be tagged, so we
-- must take it into account when accessing the free variables.
bind_fv :: (NonVoid Id, VirtualHpOffset) -> FCode (LocalReg, WordOff)
bind_fv (id, off) = do { reg <- rebindToReg id; return (reg, off) }

load_fvs :: LocalReg -> LambdaFormInfo -> [(LocalReg, WordOff)] -> FCode ()
load_fvs node lf_info = mapM_ (\ (reg, off) ->
   do dflags <- getDynFlags
      let tag = lfDynTag dflags lf_info
      emit $ mkTaggedObjectLoad dflags reg node off tag)

-----------------------------------------
-- The "slow entry" code for a function.  This entry point takes its
-- arguments on the stack.  It loads the arguments into registers
-- according to the calling convention, and jumps to the function's
-- normal entry point.  The function's closure is assumed to be in
-- R1/node.
--
-- The slow entry point is used for unknown calls: eg. stg_PAP_entry

mkSlowEntryCode :: Id -> ClosureInfo -> [LocalReg] -> FCode ()
-- If this function doesn't have a specialised ArgDescr, we need
-- to generate the function's arg bitmap and slow-entry code.
-- Here, we emit the slow-entry code.
mkSlowEntryCode bndr cl_info arg_regs -- function closure is already in `Node'
  | Just (_, ArgGen _) <- closureFunInfo cl_info
  = do dflags <- getDynFlags
       let node = idToReg dflags (NonVoid bndr)
           slow_lbl = closureSlowEntryLabel  cl_info
           fast_lbl = closureLocalEntryLabel dflags cl_info
           -- mkDirectJump does not clobber `Node' containing function closure
           jump = mkJump dflags NativeNodeCall
                                (mkLblExpr fast_lbl)
                                (map (CmmReg . CmmLocal) (node : arg_regs))
                                (initUpdFrameOff dflags)
       emitProcWithConvention Slow Nothing slow_lbl (node : arg_regs) jump
  | otherwise = return ()

-----------------------------------------
thunkCode :: ClosureInfo -> [(NonVoid Id, VirtualHpOffset)] -> CostCentreStack
          -> LocalReg -> Int -> StgExpr -> FCode ()
thunkCode cl_info fv_details _cc node arity body
  = do { dflags <- getDynFlags
       ; let node_points = nodeMustPointToIt dflags (closureLFInfo cl_info)
             node'       = if node_points then Just node else Nothing
        ; ldvEnterClosure cl_info -- NB: Node always points when profiling

        -- Heap overflow check
        ; entryHeapCheck cl_info node' arity [] $ do
        { -- Overwrite with black hole if necessary
          -- but *after* the heap-overflow check
        ; tickyEnterThunk cl_info
        ; when (blackHoleOnEntry cl_info && node_points)
                (blackHoleIt node)

          -- Push update frame
        ; setupUpdate cl_info node $
            -- We only enter cc after setting up update so
            -- that cc of enclosing scope will be recorded
            -- in update frame CAF/DICT functions will be
            -- subsumed by this enclosing cc
            do { tickyEnterThunk cl_info
               ; enterCostCentreThunk (CmmReg nodeReg)
               ; let lf_info = closureLFInfo cl_info
               ; fv_bindings <- mapM bind_fv fv_details
               ; load_fvs node lf_info fv_bindings
               ; void $ cgExpr body }}}


------------------------------------------------------------------------
--              Update and black-hole wrappers
------------------------------------------------------------------------

blackHoleIt :: LocalReg -> FCode ()
-- Only called for closures with no args
-- Node points to the closure
blackHoleIt node_reg
  = emitBlackHoleCode (CmmReg (CmmLocal node_reg))

emitBlackHoleCode :: CmmExpr -> FCode ()
emitBlackHoleCode node = do
  dflags <- getDynFlags

  -- Eager blackholing is normally disabled, but can be turned on with
  -- -feager-blackholing.  When it is on, we replace the info pointer
  -- of the thunk with stg_EAGER_BLACKHOLE_info on entry.

  -- If we wanted to do eager blackholing with slop filling, we'd need
  -- to do it at the *end* of a basic block, otherwise we overwrite
  -- the free variables in the thunk that we still need.  We have a
  -- patch for this from Andy Cheadle, but not incorporated yet. --SDM
  -- [6/2004]
  --
  -- Previously, eager blackholing was enabled when ticky-ticky was
  -- on. But it didn't work, and it wasn't strictly necessary to bring
  -- back minimal ticky-ticky, so now EAGER_BLACKHOLING is
  -- unconditionally disabled. -- krc 1/2007

  -- Note the eager-blackholing check is here rather than in blackHoleOnEntry,
  -- because emitBlackHoleCode is called from CmmParse.

  let  eager_blackholing =  not (gopt Opt_SccProfilingOn dflags)
                         && gopt Opt_EagerBlackHoling dflags
             -- Profiling needs slop filling (to support LDV
             -- profiling), so currently eager blackholing doesn't
             -- work with profiling.

  when eager_blackholing $ do
    emitStore (cmmOffsetW dflags node (fixedHdrSize dflags))
                  (CmmReg (CmmGlobal CurrentTSO))
    emitPrimCall [] MO_WriteBarrier []
    emitStore node (CmmReg (CmmGlobal EagerBlackholeInfo))

setupUpdate :: ClosureInfo -> LocalReg -> FCode () -> FCode ()
        -- Nota Bene: this function does not change Node (even if it's a CAF),
        -- so that the cost centre in the original closure can still be
        -- extracted by a subsequent enterCostCentre
setupUpdate closure_info node body
  | not (lfUpdatable (closureLFInfo closure_info))
  = body

  | not (isStaticClosure closure_info)
  = if not (closureUpdReqd closure_info)
      then do tickyUpdateFrameOmitted; body
      else do
          tickyPushUpdateFrame
          dflags <- getDynFlags
          let
              bh = blackHoleOnEntry closure_info &&
                   not (gopt Opt_SccProfilingOn dflags) &&
                   gopt Opt_EagerBlackHoling dflags

              lbl | bh        = mkBHUpdInfoLabel
                  | otherwise = mkUpdInfoLabel

          pushUpdateFrame lbl (CmmReg (CmmLocal node)) body

  | otherwise   -- A static closure
  = do  { tickyUpdateBhCaf closure_info

        ; if closureUpdReqd closure_info
          then do       -- Blackhole the (updatable) CAF:
                { upd_closure <- link_caf node True
                ; pushUpdateFrame mkBHUpdInfoLabel upd_closure body }
          else do {tickyUpdateFrameOmitted; body}
    }

-----------------------------------------------------------------------------
-- Setting up update frames

-- Push the update frame on the stack in the Entry area,
-- leaving room for the return address that is already
-- at the old end of the area.
--
pushUpdateFrame :: CLabel -> CmmExpr -> FCode () -> FCode ()
pushUpdateFrame lbl updatee body
  = do
       updfr  <- getUpdFrameOff
       dflags <- getDynFlags
       let
           hdr         = fixedHdrSize dflags * wORD_SIZE dflags
           frame       = updfr + hdr + sIZEOF_StgUpdateFrame_NoHdr dflags
       --
       emitUpdateFrame dflags (CmmStackSlot Old frame) lbl updatee
       withUpdFrameOff frame body

emitUpdateFrame :: DynFlags -> CmmExpr -> CLabel -> CmmExpr -> FCode ()
emitUpdateFrame dflags frame lbl updatee = do
  let
           hdr         = fixedHdrSize dflags * wORD_SIZE dflags
           off_updatee = hdr + oFFSET_StgUpdateFrame_updatee dflags
  --
  emitStore frame (mkLblExpr lbl)
  emitStore (cmmOffset dflags frame off_updatee) updatee
  initUpdFrameProf frame

-----------------------------------------------------------------------------
-- Entering a CAF
--
-- When a CAF is first entered, it creates a black hole in the heap,
-- and updates itself with an indirection to this new black hole.
--
-- We update the CAF with an indirection to a newly-allocated black
-- hole in the heap.  We also set the blocking queue on the newly
-- allocated black hole to be empty.
--
-- Why do we make a black hole in the heap when we enter a CAF?
--
--     - for a  generational garbage collector, which needs a fast
--       test for whether an updatee is in an old generation or not
--
--     - for the parallel system, which can implement updates more
--       easily if the updatee is always in the heap. (allegedly).
--
-- When debugging, we maintain a separate CAF list so we can tell when
-- a CAF has been garbage collected.

-- newCAF must be called before the itbl ptr is overwritten, since
-- newCAF records the old itbl ptr in order to do CAF reverting
-- (which Hugs needs to do in order that combined mode works right.)
--

-- ToDo [Feb 04]  This entire link_caf nonsense could all be moved
-- into the "newCAF" RTS procedure, which we call anyway, including
-- the allocation of the black-hole indirection closure.
-- That way, code size would fall, the CAF-handling code would
-- be closer together, and the compiler wouldn't need to know
-- about off_indirectee etc.

link_caf :: LocalReg           -- pointer to the closure
         -> Bool               -- True <=> updatable, False <=> single-entry
         -> FCode CmmExpr      -- Returns amode for closure to be updated
-- To update a CAF we must allocate a black hole, link the CAF onto the
-- CAF list, then update the CAF to point to the fresh black hole.
-- This function returns the address of the black hole, so it can be
-- updated with the new value when available.  The reason for all of this
-- is that we only want to update dynamic heap objects, not static ones,
-- so that generational GC is easier.
link_caf node _is_upd = do
  { dflags <- getDynFlags
    -- Alloc black hole specifying CC_HDR(Node) as the cost centre
  ; let use_cc   = costCentreFrom dflags (CmmReg nodeReg)
        blame_cc = use_cc
        tso      = CmmReg (CmmGlobal CurrentTSO)

  ; hp_rel <- allocDynClosureCmm Nothing cafBlackHoleInfoTable mkLFBlackHole
                                         use_cc blame_cc [(tso,fixedHdrSize dflags)]
        -- small optimisation: we duplicate the hp_rel expression in
        -- both the newCAF call and the value returned below.
        -- If we instead used allocDynClosureReg which assigns it to a reg,
        -- then the reg is live across the newCAF call and gets spilled,
        -- which is stupid.  Really we should have an optimisation pass to
        -- fix this, but we don't yet. --SDM

        -- Call the RTS function newCAF to add the CAF to the CafList
        -- so that the garbage collector can find them
        -- This must be done *before* the info table pointer is overwritten,
        -- because the old info table ptr is needed for reversion
  ; ret <- newTemp (bWord dflags)
  ; emitRtsCallGen [(ret,NoHint)] (mkForeignLabel (fsLit "newCAF") Nothing ForeignLabelInExternalPackage IsFunction)
      [ (CmmReg (CmmGlobal BaseReg),  AddrHint),
        (CmmReg (CmmLocal node), AddrHint),
        (hp_rel, AddrHint) ]
      False

  -- see Note [atomic CAF entry] in rts/sm/Storage.c
  ; updfr  <- getUpdFrameOff
  ; emit =<< mkCmmIfThen
      (CmmMachOp (mo_wordEq dflags) [ CmmReg (CmmLocal ret), CmmLit (zeroCLit dflags)])
        -- re-enter the CAF
       (let target = entryCode dflags (closureInfoPtr dflags (CmmReg (CmmLocal node))) in
        mkJump dflags NativeNodeCall target [] updfr)

  ; return hp_rel }

------------------------------------------------------------------------
--              Profiling
------------------------------------------------------------------------

-- For "global" data constructors the description is simply occurrence
-- name of the data constructor itself.  Otherwise it is determined by
-- @closureDescription@ from the let binding information.

closureDescription :: DynFlags
           -> Module            -- Module
                   -> Name              -- Id of closure binding
                   -> String
        -- Not called for StgRhsCon which have global info tables built in
        -- CgConTbls.lhs with a description generated from the data constructor
closureDescription dflags mod_name name
  = showSDocDump dflags (char '<' <>
                    (if isExternalName name
                      then ppr name -- ppr will include the module name prefix
                      else pprModule mod_name <> char '.' <> ppr name) <>
                    char '>')
   -- showSDocDump, because we want to see the unique on the Name.

