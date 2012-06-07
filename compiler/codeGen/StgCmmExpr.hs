-----------------------------------------------------------------------------
--
-- Stg to C-- code generation: expressions
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module StgCmmExpr ( cgExpr ) where

#define FAST_STRING_NOT_NEEDED
#include "HsVersions.h"

import {-# SOURCE #-} StgCmmBind ( cgBind )

import StgCmmMonad
import StgCmmHeap
import StgCmmEnv
import StgCmmCon
import StgCmmProf
import StgCmmLayout
import StgCmmPrim
import StgCmmHpc
import StgCmmTicky
import StgCmmUtils
import StgCmmClosure

import StgSyn

import MkGraph
import BlockId
import Cmm
import CoreSyn
import DataCon
import ForeignCall
import Id
import PrimOp
import TyCon
import Type
import CostCentre	( CostCentreStack, currentCCS )
import Control.Monad (when)
import Maybes
import Util
import FastString
import Outputable
import UniqSupply

------------------------------------------------------------------------
--		cgExpr: the main function
------------------------------------------------------------------------

cgExpr	:: StgExpr -> FCode ()

cgExpr (StgApp fun args)     = cgIdApp fun args

{- seq# a s ==> a -}
cgExpr (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _res_ty) =
  cgIdApp a []

cgExpr (StgOpApp op args ty) = cgOpApp op args ty
cgExpr (StgConApp con args)  = cgConApp con args
cgExpr (StgSCC cc tick push expr) = do { emitSetCCC cc tick push; cgExpr expr }
cgExpr (StgTick m n expr) = do { emit (mkTickBox m n); cgExpr expr }
cgExpr (StgLit lit)       = do cmm_lit <- cgLit lit
                               emitReturn [CmmLit cmm_lit]

cgExpr (StgLet binds expr)             = do { cgBind binds;     cgExpr expr }
cgExpr (StgLetNoEscape _ _ binds expr) =
  do { us <- newUniqSupply
     ; let join_id = mkBlockId (uniqFromSupply us)
     ; cgLneBinds join_id binds
     ; cgExpr expr 
     ; emit $ mkLabel join_id}

cgExpr (StgCase expr _live_vars _save_vars bndr srt alt_type alts) =
  cgCase expr bndr srt alt_type alts

cgExpr (StgLam {}) = panic "cgExpr: StgLam"

------------------------------------------------------------------------
--		Let no escape
------------------------------------------------------------------------

{- Generating code for a let-no-escape binding, aka join point is very
very similar to what we do for a case expression.  The duality is
between
	let-no-escape x = b
	in e
and
	case e of ... -> b

That is, the RHS of 'x' (ie 'b') will execute *later*, just like
the alternative of the case; it needs to be compiled in an environment
in which all volatile bindings are forgotten, and the free vars are
bound only to stable things like stack locations..  The 'e' part will
execute *next*, just like the scrutinee of a case. -}

-------------------------
cgLneBinds :: BlockId -> StgBinding -> FCode ()
cgLneBinds join_id (StgNonRec bndr rhs)
  = do  { local_cc <- saveCurrentCostCentre
                -- See Note [Saving the current cost centre]
        ; info <- cgLetNoEscapeRhs join_id local_cc bndr rhs 
        ; addBindC (cg_id info) info }

cgLneBinds join_id (StgRec pairs)
  = do  { local_cc <- saveCurrentCostCentre
        ; new_bindings <- fixC (\ new_bindings -> do
                { addBindsC new_bindings
                ; listFCs [ cgLetNoEscapeRhs join_id local_cc b e 
                          | (b,e) <- pairs ] })
        ; addBindsC new_bindings }


-------------------------
cgLetNoEscapeRhs
    :: BlockId          -- join point for successor of let-no-escape
    -> Maybe LocalReg	-- Saved cost centre
    -> Id
    -> StgRhs
    -> FCode CgIdInfo

cgLetNoEscapeRhs join_id local_cc bndr rhs =
  do { (info, rhs_body) <- getCodeR $ cgLetNoEscapeRhsBody local_cc bndr rhs 
     ; let (bid, _) = expectJust "cgLetNoEscapeRhs" $ maybeLetNoEscape info
     ; emit (outOfLine $ mkLabel bid <*> rhs_body <*> mkBranch join_id)
     ; return info
     }

cgLetNoEscapeRhsBody
    :: Maybe LocalReg	-- Saved cost centre
    -> Id
    -> StgRhs
    -> FCode CgIdInfo
cgLetNoEscapeRhsBody local_cc bndr (StgRhsClosure cc _bi _ _upd _ args body)
  = cgLetNoEscapeClosure bndr local_cc cc (nonVoidIds args) body
cgLetNoEscapeRhsBody local_cc bndr (StgRhsCon cc con args)
  = cgLetNoEscapeClosure bndr local_cc cc [] (StgConApp con args)
	-- For a constructor RHS we want to generate a single chunk of 
	-- code which can be jumped to from many places, which will 
	-- return the constructor. It's easy; just behave as if it 
	-- was an StgRhsClosure with a ConApp inside!

-------------------------
cgLetNoEscapeClosure
	:: Id			-- binder
	-> Maybe LocalReg	-- Slot for saved current cost centre
	-> CostCentreStack   	-- XXX: *** NOT USED *** why not?
	-> [NonVoid Id]		-- Args (as in \ args -> body)
    	-> StgExpr		-- Body (as in above)
	-> FCode CgIdInfo

cgLetNoEscapeClosure bndr cc_slot _unused_cc args body
  = do  { arg_regs <- forkProc $ do	
		{ restoreCurrentCostCentre cc_slot
		; arg_regs <- bindArgsToRegs args
		; altHeapCheck arg_regs (cgExpr body)
			-- Using altHeapCheck just reduces
			-- instructions to save on stack
		; return arg_regs }
	; return $ lneIdInfo bndr arg_regs}


------------------------------------------------------------------------
--		Case expressions
------------------------------------------------------------------------

{- Note [Compiling case expressions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It is quite interesting to decide whether to put a heap-check at the
start of each alternative.  Of course we certainly have to do so if
the case forces an evaluation, or if there is a primitive op which can
trigger GC.

A more interesting situation is this (a Plan-B situation)

	!P!;
	...P...
	case x# of
	  0#      -> !Q!; ...Q...
	  default -> !R!; ...R...

where !x! indicates a possible heap-check point. The heap checks
in the alternatives *can* be omitted, in which case the topmost
heapcheck will take their worst case into account.

In favour of omitting !Q!, !R!:

 - *May* save a heap overflow test,
   if ...P... allocates anything.  

 - We can use relative addressing from a single Hp to 
   get at all the closures so allocated.

 - No need to save volatile vars etc across heap checks
   in !Q!, !R!

Against omitting !Q!, !R!

  - May put a heap-check into the inner loop.  Suppose 
	the main loop is P -> R -> P -> R...
	Q is the loop exit, and only it does allocation.
    This only hurts us if P does no allocation.  If P allocates,
    then there is a heap check in the inner loop anyway.

  - May do more allocation than reqd.  This sometimes bites us
    badly.  For example, nfib (ha!) allocates about 30\% more space if the
    worst-casing is done, because many many calls to nfib are leaf calls
    which don't need to allocate anything. 

    We can un-allocate, but that costs an instruction

Neither problem hurts us if there is only one alternative.

Suppose the inner loop is P->R->P->R etc.  Then here is
how many heap checks we get in the *inner loop* under various
conditions

  Alooc	  Heap check in branches (!Q!, !R!)?
  P Q R	     yes     no (absorb to !P!)
--------------------------------------
  n n n	     0		0
  n y n	     0		1
  n . y	     1		1
  y . y	     2		1
  y . n	     1		1

Best choices: absorb heap checks from Q and R into !P! iff
  a) P itself does some allocation
or
  b) P does allocation, or there is exactly one alternative

We adopt (b) because that is more likely to put the heap check at the
entry to a function, when not many things are live.  After a bunch of
single-branch cases, we may have lots of things live

Hence: two basic plans for

	case e of r { alts }

------ Plan A: the general case ---------

	...save current cost centre...

	...code for e, 
	   with sequel (SetLocals r)

        ...restore current cost centre...
	...code for alts...
	...alts do their own heap checks

------ Plan B: special case when ---------
  (i)  e does not allocate or call GC
  (ii) either upstream code performs allocation
       or there is just one alternative

  Then heap allocation in the (single) case branch
  is absorbed by the upstream check.
  Very common example: primops on unboxed values

	...code for e,
	   with sequel (SetLocals r)...

	...code for alts...
	...no heap check...
-}



-------------------------------------
data GcPlan
  = GcInAlts 		-- Put a GC check at the start the case alternatives,
	[LocalReg] 	-- which binds these registers
	SRT		-- using this SRT
  | NoGcInAlts		-- The scrutinee is a primitive value, or a call to a
			-- primitive op which does no GC.  Absorb the allocation
			-- of the case alternative(s) into the upstream check

-------------------------------------
-- See Note [case on Bool]
cgCase :: StgExpr -> Id -> SRT -> AltType -> [StgAlt] -> FCode ()
{-
cgCase (OpApp ) bndr srt AlgAlt [(DataAlt flase, a2]
  | isBoolTy (idType bndr)
  , isDeadBndr bndr
  = 
-}

  -- Note [ticket #3132]: we might be looking at a case of a lifted Id
  -- that was cast to an unlifted type.  The Id will always be bottom,
  -- but we don't want the code generator to fall over here.  If we
  -- just emit an assignment here, the assignment will be
  -- type-incorrect Cmm.  Hence, we emit the usual enter/return code,
  -- (and because bottom must be untagged, it will be entered and the
  -- program will crash).
  -- The Sequel is a type-correct assignment, albeit bogus.
  -- The (dead) continuation loops; it would be better to invoke some kind
  -- of panic function here.
  --
  -- However, we also want to allow an assignment to be generated
  -- in the case when the types are compatible, because this allows
  -- some slightly-dodgy but occasionally-useful casts to be used,
  -- such as in RtClosureInspect where we cast an HValue to a MutVar#
  -- so we can print out the contents of the MutVar#.  If we generate
  -- code that enters the HValue, then we'll get a runtime panic, because
  -- the HValue really is a MutVar#.  The types are compatible though,
  -- so we can just generate an assignment.
cgCase (StgApp v []) bndr _ alt_type@(PrimAlt _) alts
  | isUnLiftedType (idType v)
  || reps_compatible
  = -- assignment suffices for unlifted types
    do { when (not reps_compatible) $
           panic "cgCase: reps do not match, perhaps a dodgy unsafeCoerce?"
       ; v_info <- getCgIdInfo v
       ; emit (mkAssign (CmmLocal (idToReg (NonVoid bndr))) (idInfoToAmode v_info))
       ; _ <- bindArgsToRegs [NonVoid bndr]
       ; cgAlts NoGcInAlts (NonVoid bndr) alt_type alts }
  where
    reps_compatible = idPrimRep v == idPrimRep bndr

cgCase scrut@(StgApp v []) _ _ (PrimAlt _) _ 
  = -- fail at run-time, not compile-time
    do { mb_cc <- maybeSaveCostCentre True
       ; withSequel (AssignTo [idToReg (NonVoid v)] False) (cgExpr scrut)
       ; restoreCurrentCostCentre mb_cc
       ; emit $ mkComment $ mkFastString "should be unreachable code"
       ; emit $ withFreshLabel "l" (\l -> mkLabel l <*> mkBranch l)}

{-
case seq# a s of v
  (# s', a' #) -> e

==>

case a of v
  (# s', a' #) -> e

(taking advantage of the fact that the return convention for (# State#, a #)
is the same as the return convention for just 'a')
-}
cgCase (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) bndr srt alt_type alts
  = -- handle seq#, same return convention as vanilla 'a'.
    cgCase (StgApp a []) bndr srt alt_type alts

cgCase scrut bndr srt alt_type alts 
  = -- the general case
    do { up_hp_usg <- getVirtHp        -- Upstream heap usage
       ; let ret_bndrs = chooseReturnBndrs bndr alt_type alts
             alt_regs  = map idToReg ret_bndrs
             simple_scrut = isSimpleScrut scrut alt_type
             gcInAlts | not simple_scrut = True
                      | isSingleton alts = False
                      | up_hp_usg > 0    = False
                      | otherwise        = True
             gc_plan = if gcInAlts then GcInAlts alt_regs srt else NoGcInAlts

       ; mb_cc <- maybeSaveCostCentre simple_scrut
       ; withSequel (AssignTo alt_regs gcInAlts) (cgExpr scrut)
       ; restoreCurrentCostCentre mb_cc

  -- JD: We need Note: [Better Alt Heap Checks]
       ; _ <- bindArgsToRegs ret_bndrs
       ; cgAlts gc_plan (NonVoid bndr) alt_type alts }

-----------------
maybeSaveCostCentre :: Bool -> FCode (Maybe LocalReg)
maybeSaveCostCentre simple_scrut
  | simple_scrut = saveCurrentCostCentre
  | otherwise    = return Nothing


-----------------
isSimpleScrut :: StgExpr -> AltType -> Bool
-- Simple scrutinee, does not block or allocate; hence safe to amalgamate
-- heap usage from alternatives into the stuff before the case
-- NB: if you get this wrong, and claim that the expression doesn't allocate
--     when it does, you'll deeply mess up allocation
isSimpleScrut (StgOpApp op _ _) _          = isSimpleOp op
isSimpleScrut (StgLit _)       _           = True	-- case 1# of { 0# -> ..; ... }
isSimpleScrut (StgApp _ [])    (PrimAlt _) = True	-- case x# of { 0# -> ..; ... }
isSimpleScrut _		       _           = False

isSimpleOp :: StgOp -> Bool
-- True iff the op cannot block or allocate
isSimpleOp (StgFCallOp (CCall (CCallSpec _ _ safe)) _) = not (playSafe safe)
isSimpleOp (StgPrimOp op)      			       = not (primOpOutOfLine op)
isSimpleOp (StgPrimCallOp _)                           = False

-----------------
chooseReturnBndrs :: Id -> AltType -> [StgAlt] -> [NonVoid Id]
-- These are the binders of a case that are assigned
-- by the evaluation of the scrutinee
-- Only non-void ones come back
chooseReturnBndrs bndr (PrimAlt _) _alts
  = nonVoidIds [bndr]

chooseReturnBndrs _bndr (UbxTupAlt _) [(_, ids, _, _)]
  = nonVoidIds ids	-- 'bndr' is not assigned!

chooseReturnBndrs bndr (AlgAlt _) _alts
  = nonVoidIds [bndr]	-- Only 'bndr' is assigned

chooseReturnBndrs bndr PolyAlt _alts
  = nonVoidIds [bndr]	-- Only 'bndr' is assigned

chooseReturnBndrs _ _ _ = panic "chooseReturnBndrs"
	-- UbxTupALt has only one alternative

-------------------------------------
cgAlts :: GcPlan -> NonVoid Id -> AltType -> [StgAlt] -> FCode ()
-- At this point the result of the case are in the binders
cgAlts gc_plan _bndr PolyAlt [(_, _, _, rhs)]
  = maybeAltHeapCheck gc_plan (cgExpr rhs)
  
cgAlts gc_plan _bndr (UbxTupAlt _) [(_, _, _, rhs)]
  = maybeAltHeapCheck gc_plan (cgExpr rhs)
	-- Here bndrs are *already* in scope, so don't rebind them

cgAlts gc_plan bndr (PrimAlt _) alts
  = do	{ tagged_cmms <- cgAltRhss gc_plan bndr alts

	; let bndr_reg = CmmLocal (idToReg bndr)
	      (DEFAULT,deflt) = head tagged_cmms
		-- PrimAlts always have a DEFAULT case
		-- and it always comes first

	      tagged_cmms' = [(lit,code) 
			     | (LitAlt lit, code) <- tagged_cmms]
	; emit (mkCmmLitSwitch (CmmReg bndr_reg) tagged_cmms' deflt) }

cgAlts gc_plan bndr (AlgAlt tycon) alts
  = do	{ tagged_cmms <- cgAltRhss gc_plan bndr alts
	
	; let fam_sz   = tyConFamilySize tycon
	      bndr_reg = CmmLocal (idToReg bndr)
	      mb_deflt = case tagged_cmms of
			   ((DEFAULT,rhs) : _) -> Just rhs
			   _other	       -> Nothing
		-- DEFAULT is always first, if present

	      branches = [ (dataConTagZ con, cmm) 
	   	         | (DataAlt con, cmm) <- tagged_cmms ]

                    -- Is the constructor tag in the node reg?
        ; if isSmallFamily fam_sz
	  then let	-- Yes, bndr_reg has constr. tag in ls bits
                   tag_expr = cmmConstrTag1 (CmmReg bndr_reg)
                   branches' = [(tag+1,branch) | (tag,branch) <- branches]
                in
	        emitSwitch tag_expr branches' mb_deflt 1 fam_sz

	   else 	-- No, get tag from info table
                let -- Note that ptr _always_ has tag 1
                    -- when the family size is big enough
                    untagged_ptr = cmmRegOffB bndr_reg (-1)
                    tag_expr = getConstrTag (untagged_ptr)
		 in
		 emitSwitch tag_expr branches mb_deflt 0 (fam_sz - 1) }

cgAlts _ _ _ _ = panic "cgAlts"
	-- UbxTupAlt and PolyAlt have only one alternative

-------------------
cgAltRhss :: GcPlan -> NonVoid Id -> [StgAlt] -> FCode [(AltCon, CmmAGraph)]
cgAltRhss gc_plan bndr alts
  = forkAlts (map cg_alt alts)
  where
    base_reg = idToReg bndr
    cg_alt :: StgAlt -> FCode (AltCon, CmmAGraph)
    cg_alt (con, bndrs, _uses, rhs)
      = getCodeR		  $
	maybeAltHeapCheck gc_plan $
	do { _ <- bindConArgs con base_reg bndrs
	   ; cgExpr rhs
	   ; return con }

maybeAltHeapCheck :: GcPlan -> FCode a -> FCode a
maybeAltHeapCheck NoGcInAlts        code = code
maybeAltHeapCheck (GcInAlts regs _) code = altHeapCheck regs code

-----------------------------------------------------------------------------
-- 	Tail calls
-----------------------------------------------------------------------------

cgConApp :: DataCon -> [StgArg] -> FCode ()
cgConApp con stg_args
  | isUnboxedTupleCon con	-- Unboxed tuple: assign and return
  = do { arg_exprs <- getNonVoidArgAmodes stg_args
       ; tickyUnboxedTupleReturn (length arg_exprs)
       ; emitReturn arg_exprs }

  | otherwise	--  Boxed constructors; allocate and return
  = ASSERT( stg_args `lengthIs` dataConRepRepArity con )
    do	{ (idinfo, init) <- buildDynCon (dataConWorkId con) currentCCS con stg_args
	   	-- The first "con" says that the name bound to this closure is
		-- is "con", which is a bit of a fudge, but it only affects profiling

        ; emit init
	; emitReturn [idInfoToAmode idinfo] }


cgIdApp :: Id -> [StgArg] -> FCode ()
cgIdApp fun_id [] | isVoidId fun_id = emitReturn []
cgIdApp fun_id args
  = do 	{ fun_info <- getCgIdInfo fun_id
        ; case maybeLetNoEscape fun_info of
            Just (blk_id, lne_regs) -> cgLneJump blk_id lne_regs args
            Nothing -> cgTailCall fun_id fun_info args }

cgLneJump :: BlockId -> [LocalReg] -> [StgArg] -> FCode ()
cgLneJump blk_id lne_regs args	-- Join point; discard sequel
  = do	{ cmm_args <- getNonVoidArgAmodes args
      	; emit (mkMultiAssign lne_regs cmm_args
		<*> mkBranch blk_id) }
    
cgTailCall :: Id -> CgIdInfo -> [StgArg] -> FCode ()
cgTailCall fun_id fun_info args = do
    dflags <- getDynFlags
    case (getCallMethod dflags fun_name (idCafInfo fun_id) lf_info (length args)) of

	    -- A value in WHNF, so we can just return it.
      	ReturnIt -> emitReturn [fun]	-- ToDo: does ReturnIt guarantee tagged?
    
      	EnterIt -> ASSERT( null args )	-- Discarding arguments
      		do { let fun' = CmmLoad fun (cmmExprType fun)
                   ; [ret,call] <- forkAlts [
      			getCode $ emitReturn [fun],	-- Is tagged; no need to untag
      			getCode $ do -- emit (mkAssign nodeReg fun)
                         emitCall (NativeNodeCall, NativeReturn)
                                  (entryCode fun') [fun]]  -- Not tagged
      		   ; emit (mkCmmIfThenElse (cmmIsTagged fun) ret call) }

      	SlowCall -> do 	    -- A slow function call via the RTS apply routines
      		{ tickySlowCall lf_info args
                ; emit $ mkComment $ mkFastString "slowCall"
      		; slowCall fun args }
    
      	-- A direct function call (possibly with some left-over arguments)
      	DirectEntry lbl arity -> do
		{ tickyDirectCall arity args
 		; if node_points then
                    do emit $ mkComment $ mkFastString "directEntry"
                       emit (mkAssign nodeReg fun)
                       directCall lbl arity args
		  else do emit $ mkComment $ mkFastString "directEntry else"
                          directCall lbl arity args }

	JumpToIt {} -> panic "cgTailCall"	-- ???

  where
    fun_name 	= idName            fun_id
    fun         = idInfoToAmode     fun_info
    lf_info     = cgIdInfoLF        fun_info
    node_points = nodeMustPointToIt lf_info


{- Note [case on Bool]
   ~~~~~~~~~~~~~~~~~~~
A case on a Boolean value does two things:
  1. It looks up the Boolean in a closure table and assigns the
     result to the binder.
  2. It branches to the True or False case through analysis
     of the closure assigned to the binder.
But the indirection through the closure table is unnecessary
if the assignment to the binder will be dead code (use isDeadBndr).

The following example illustrates how badly the code turns out:
  STG:
    case <=## [ww_s7Hx y_s7HD] of wild2_sbH8 {
      GHC.Types.False -> <true  code> // sbH8 dead
      GHC.Types.True  -> <false code> // sbH8 dead
    };
  Cmm:
    _s7HD::F64 = F64[_sbH7::I64 + 7];  // MidAssign
    _ccsW::I64 = %MO_F_Le_W64(_s7Hx::F64, _s7HD::F64);  // MidAssign
    // emitReturn  // MidComment
    _sbH8::I64 = I64[ghczmprim_GHCziBool_Bool_closure_tbl + (_ccsW::I64 << 3)];  // MidAssign
    _ccsX::I64 = _sbH8::I64 & 7;  // MidAssign
    if (_ccsX::I64 >= 2) goto ccsH; else goto ccsI;  // LastCondBranch

The assignments to _sbH8 and _ccsX are completely unnecessary.
Instead, we should branch based on the value of _ccsW.
-}

{- Note [Better Alt Heap Checks]
If two function calls can share a return point, then they will also
get the same info table. Therefore, it's worth our effort to make
those opportunities appear as frequently as possible.

Here are a few examples of how it should work:

  STG:
    case f x of
      True  -> <True code -- including allocation>
      False -> <False code>
  Cmm:
      r = call f(x) returns to L;
   L:
      if r & 7 >= 2 goto L1 else goto L2;
   L1:
      if Hp > HpLim then
        r = gc(r);
        goto L;
      <True code -- including allocation>
   L2:
      <False code>
Note that the code following both the call to f(x) and the code to gc(r)
should be the same, which will allow the common blockifier to discover
that they are the same. Therefore, both function calls will return to the same
block, and they will use the same info table.        

Here's an example of the Cmm code we want from a primOp.
The primOp doesn't produce an info table for us to reuse, but that's okay:
we should still generate the same code:
  STG:
    case f x of
      0 -> <0-case code -- including allocation>
      _ -> <default-case code>
  Cmm:
      r = a +# b;
   L:
      if r == 0 then goto L1 else goto L2;
   L1:
      if Hp > HpLim then
        r = gc(r);
        goto L;
      <0-case code -- including allocation>
   L2:
      <default-case code>
-}

