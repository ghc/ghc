%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgCase.lhs,v 1.66 2003/07/22 16:11:26 simonmar Exp $
%
%********************************************************
%*							*
\section[CgCase]{Converting @StgCase@ expressions}
%*							*
%********************************************************

\begin{code}
module CgCase (	cgCase, saveVolatileVarsAndRegs, 
		mkRetDirectTarget, restoreCurrentCostCentre
	) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr  ( cgExpr )

import CgMonad
import StgSyn
import AbsCSyn

import AbsCUtils	( mkAbstractCs, mkAbsCStmts, mkAlgAltsCSwitch, getAmodeRep )
import CgBindery	( getVolatileRegs, getArgAmodes,
			  bindNewToReg, bindNewToTemp,
			  getCAddrModeAndInfo,
			  rebindToStack, getCAddrMode, getCAddrModeIfVolatile,
			  buildContLivenessMask, nukeDeadBindings,
			)
import CgCon		( bindConArgs, bindUnboxedTupleComponents )
import CgHeapery	( altHeapCheck, unbxTupleHeapCheck )
import CgRetConv	( dataReturnConvPrim, ctrlReturnConvAlg,
			  CtrlReturnConvention(..)
			)
import CgStackery	( allocPrimStack, allocStackTop,
			  deAllocStackTop, freeStackSlots, dataStackSlots
			)
import CgTailCall	( performTailCall )
import CgUsages		( getSpRelOffset )
import CLabel		( mkVecTblLabel, mkClosureTblLabel,
			  mkDefaultLabel, mkAltLabel, mkReturnInfoLabel
			)
import ClosureInfo	( mkLFArgument )
import CmdLineOpts	( opt_SccProfilingOn )
import Id		( Id, idName, isDeadBinder )
import DataCon		( dataConTag, fIRST_TAG, ConTag )
import VarSet		( varSetElems )
import CoreSyn		( AltCon(..) )
import PrimOp		( primOpOutOfLine, PrimOp(..) )
import PrimRep		( getPrimRepSize, retPrimRepSize, PrimRep(..)
			)
import TyCon		( TyCon, isEnumerationTyCon, tyConPrimRep	)
import Unique           ( Unique, Uniquable(..), newTagUnique )
import Util		( only )
import List		( sortBy )
import Outputable
\end{code}

\begin{code}
data GCFlag
  = GCMayHappen	-- The scrutinee may involve GC, so everything must be
		-- tidy before the code for the scrutinee.

  | NoGC	-- The scrutinee is a primitive value, or a call to a
		-- primitive op which does no GC.  Hence the case can
		-- be done inline, without tidying up first.
\end{code}

It is quite interesting to decide whether to put a heap-check
at the start of each alternative.  Of course we certainly have
to do so if the case forces an evaluation, or if there is a primitive
op which can trigger GC.

A more interesting situation is this:

 \begin{verbatim}
	!A!;
	...A...
	case x# of
	  0#      -> !B!; ...B...
	  default -> !C!; ...C...
 \end{verbatim}

where \tr{!x!} indicates a possible heap-check point. The heap checks
in the alternatives {\em can} be omitted, in which case the topmost
heapcheck will take their worst case into account.

In favour of omitting \tr{!B!}, \tr{!C!}:

 - {\em May} save a heap overflow test,
	if ...A... allocates anything.  The other advantage
	of this is that we can use relative addressing
	from a single Hp to get at all the closures so allocated.

 - No need to save volatile vars etc across the case

Against:

  - May do more allocation than reqd.  This sometimes bites us
	badly.  For example, nfib (ha!)  allocates about 30\% more space if the
	worst-casing is done, because many many calls to nfib are leaf calls
	which don't need to allocate anything.

	This never hurts us if there is only one alternative.

\begin{code}
cgCase	:: StgExpr
	-> StgLiveVars
	-> StgLiveVars
	-> Id
	-> SRT
	-> AltType
	-> [StgAlt]
	-> Code
\end{code}

Special case #1: case of literal.

\begin{code}
cgCase (StgLit lit) live_in_whole_case live_in_alts bndr srt 
       alt_type@(PrimAlt tycon) alts 
  = bindNewToTemp bndr			`thenFC` \ tmp_amode ->
    absC (CAssign tmp_amode (CLit lit))	`thenC`
    cgPrimAlts NoGC tmp_amode alts alt_type
\end{code}

Special case #2: scrutinising a primitive-typed variable.	No
evaluation required.  We don't save volatile variables, nor do we do a
heap-check in the alternatives.	 Instead, the heap usage of the
alternatives is worst-cased and passed upstream.  This can result in
allocating more heap than strictly necessary, but it will sometimes
eliminate a heap check altogether.

\begin{code}
cgCase (StgApp v []) live_in_whole_case live_in_alts bndr srt
       alt_type@(PrimAlt tycon) alts

  = -- Careful! we can't just bind the default binder to the same thing
    -- as the scrutinee, since it might be a stack location, and having
    -- two bindings pointing at the same stack locn doesn't work (it
    -- confuses nukeDeadBindings).  Hence, use a new temp.
    getCAddrMode v			`thenFC` \ amode ->
    bindNewToTemp bndr			`thenFC` \ tmp_amode ->
    absC (CAssign tmp_amode amode)	`thenC`
    cgPrimAlts NoGC tmp_amode alts alt_type
\end{code}	

Special case #3: inline PrimOps.

\begin{code}
cgCase (StgOpApp op@(StgPrimOp primop) args _) 
       live_in_whole_case live_in_alts bndr srt alt_type alts
  | not (primOpOutOfLine primop)
  =	-- Get amodes for the arguments and results
    getArgAmodes args			`thenFC` \ arg_amodes ->
    getVolatileRegs live_in_alts        `thenFC` \ vol_regs ->

    case alt_type of 
      PrimAlt tycon	-- PRIMITIVE ALTS
	-> bindNewToTemp bndr					`thenFC` \ tmp_amode ->
	   absC (COpStmt [tmp_amode] op arg_amodes vol_regs)	`thenC` 
			 -- Note: no liveness arg
	   cgPrimAlts NoGC tmp_amode alts alt_type

      UbxTupAlt tycon 	-- UNBOXED TUPLE ALTS
	-> 	-- No heap check, no yield, just get in there and do it.
		-- NB: the case binder isn't bound to anything; 
		--     it has a unboxed tuple type
	   mapFCs bindNewToTemp res_ids				`thenFC` \ res_tmps ->
	   absC (COpStmt res_tmps op arg_amodes vol_regs) 	`thenC`
	   cgExpr rhs
	where
	   [(_, res_ids, _, rhs)] = alts

      AlgAlt tycon 	-- ENUMERATION TYPE RETURN
	-> ASSERT( isEnumerationTyCon tycon )
	   do_enum_primop primop		`thenFC` \ tag_amode ->

	 	-- Bind the default binder if necessary
		-- (avoiding it avoids the assignment)
		-- The deadness info is set by StgVarInfo
	   (if (isDeadBinder bndr)
		then nopC
		else bindNewToTemp bndr 	`thenFC` \ tmp_amode ->
		     absC (CAssign tmp_amode (tagToClosure tycon tag_amode))
	   )					`thenC`

		-- Compile the alts
	   cgAlgAlts NoGC (getUnique bndr) 
	   	     Nothing{-cc_slot-} False{-no semi-tagging-}
		     (AlgAlt tycon) alts 	`thenFC` \ tagged_alts ->

		-- Do the switch
	   absC (mkAlgAltsCSwitch tag_amode tagged_alts)
	where
	   do_enum_primop :: PrimOp -> FCode CAddrMode	-- Returns amode for result
	   do_enum_primop TagToEnumOp	-- No code!
	      = returnFC (only arg_amodes)

	   do_enum_primop primop
 	      = absC (COpStmt [tag_amode] op arg_amodes vol_regs)	`thenC`
		returnFC tag_amode
	      where			
		tag_amode = CTemp (newTagUnique (getUnique bndr) 'C') IntRep
			-- Being a bit short of uniques for temporary variables here, 
			-- we use newTagUnique to generate a new unique from the case 
			-- binder.  The case binder's unique will presumably have 
			-- the 'c' tag (generated by CoreToStg), so we just change 
			-- its tag to 'C' (for 'case') to ensure it doesn't clash with 
			-- anything else.
			-- We can't use the unique from the case binder, becaus e
			-- this is used to hold the actual result closure
			-- (via the call to bindNewToTemp)

      other -> pprPanic "cgCase: case of primop has strange alt type" (ppr alt_type)
\end{code}

TODO: Case-of-case of primop can probably be done inline too (but
maybe better to translate it out beforehand).  See
ghc/lib/misc/PackedString.lhs for examples where this crops up (with
4.02).

Special case: scrutinising a non-primitive variable.
This can be done a little better than the general case, because
we can reuse/trim the stack slot holding the variable (if it is in one).

\begin{code}
cgCase (StgApp fun args)
	live_in_whole_case live_in_alts bndr srt alt_type alts
  = getCAddrModeAndInfo fun		`thenFC` \ (fun', fun_amode, lf_info) ->
    getArgAmodes args			`thenFC` \ arg_amodes ->

	-- Nuking dead bindings *before* calculating the saves is the
	-- value-add here.  We might end up freeing up some slots currently
	-- occupied by variables only required for the call.
	-- NOTE: we need to look up the variables used in the call before
	-- doing this, because some of them may not be in the environment
	-- afterward.
    nukeDeadBindings live_in_alts	`thenC`
    saveVolatileVarsAndRegs live_in_alts
    	    	    	`thenFC` \ (save_assts, alts_eob_info, maybe_cc_slot) ->

    forkEval alts_eob_info 
	( allocStackTop retPrimRepSize
	 `thenFC` \_ -> nopC )
	( deAllocStackTop retPrimRepSize `thenFC` \_ ->
	  cgEvalAlts maybe_cc_slot bndr srt alt_type alts ) 
					 `thenFC` \ scrut_eob_info ->

    setEndOfBlockInfo (maybeReserveSeqFrame alt_type scrut_eob_info)	$
    performTailCall fun' fun_amode lf_info arg_amodes save_assts
\end{code}

Note about return addresses: we *always* push a return address, even
if because of an optimisation we end up jumping direct to the return
code (not through the address itself).  The alternatives always assume
that the return address is on the stack.  The return address is
required in case the alternative performs a heap check, since it
encodes the liveness of the slots in the activation record.

On entry to the case alternative, we can re-use the slot containing
the return address immediately after the heap check.  That's what the
deAllocStackTop call is doing above.

Finally, here is the general case.

\begin{code}
cgCase expr live_in_whole_case live_in_alts bndr srt alt_type alts
  =	-- Figure out what volatile variables to save
    nukeDeadBindings live_in_whole_case	`thenC`
    
    saveVolatileVarsAndRegs live_in_alts
    	    	    	`thenFC` \ (save_assts, alts_eob_info, maybe_cc_slot) ->

    -- Save those variables right now!
    absC save_assts 	    	    	`thenC`

    -- generate code for the alts
    forkEval alts_eob_info
    	(nukeDeadBindings live_in_alts `thenC` 
	 allocStackTop retPrimRepSize   -- space for retn address 
	 `thenFC` \_ -> nopC
	 )
	(deAllocStackTop retPrimRepSize `thenFC` \_ ->
	 cgEvalAlts maybe_cc_slot bndr srt alt_type alts) `thenFC` \ scrut_eob_info ->

    setEndOfBlockInfo (maybeReserveSeqFrame alt_type scrut_eob_info)	$
    cgExpr expr
\end{code}

There's a lot of machinery going on behind the scenes to manage the
stack pointer here.  forkEval takes the virtual Sp and free list from
the first argument, and turns that into the *real* Sp for the second
argument.  It also uses this virtual Sp as the args-Sp in the EOB info
returned, so that the scrutinee will trim the real Sp back to the
right place before doing whatever it does.  
  --SDM (who just spent an hour figuring this out, and didn't want to 
	 forget it).

Why don't we push the return address just before evaluating the
scrutinee?  Because the slot reserved for the return address might
contain something useful, so we wait until performing a tail call or
return before pushing the return address (see
CgTailCall.pushReturnAddress).  

This also means that the environment doesn't need to know about the
free stack slot for the return address (for generating bitmaps),
because we don't reserve it until just before the eval.

TODO!!  Problem: however, we have to save the current cost centre
stack somewhere, because at the eval point the current CCS might be
different.  So we pick a free stack slot and save CCCS in it.  The
problem with this is that this slot isn't recorded as free/unboxed in
the environment, so a case expression in the scrutinee will have the
wrong bitmap attached.  Fortunately we don't ever seem to see
case-of-case at the back end.  One solution might be to shift the
saved CCS to the correct place in the activation record just before
the jump.
	--SDM

(one consequence of the above is that activation records on the stack
don't follow the layout of closures when we're profiling.  The CCS
could be anywhere within the record).

\begin{code}
maybeReserveSeqFrame PolyAlt (EndOfBlockInfo args_sp (CaseAlts amode stuff _))
   = EndOfBlockInfo (args_sp + retPrimRepSize) (CaseAlts amode stuff True)
maybeReserveSeqFrame other scrut_eob_info = scrut_eob_info
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-alts]{Alternatives}
%*									*
%************************************************************************

@cgEvalAlts@ returns an addressing mode for a continuation for the
alternatives of a @case@, used in a context when there
is some evaluation to be done.

\begin{code}
cgEvalAlts :: Maybe VirtualSpOffset	-- Offset of cost-centre to be restored, if any
	   -> Id
	   -> SRT			-- SRT for the continuation
	   -> AltType
	   -> [StgAlt]
	   -> FCode Sequel	-- Any addr modes inside are guaranteed
				-- to be a label so that we can duplicate it 
				-- without risk of duplicating code

cgEvalAlts cc_slot bndr srt (UbxTupAlt _) [(con,args,_,rhs)]
  =	-- Unboxed tuple case
	-- By now, the simplifier should have have turned it
	-- into 	case e of (# a,b #) -> e
	-- There shouldn't be a 
	--		case e of DEFAULT -> e
    ASSERT2( case con of { DataAlt _ -> True; other -> False },
	     text "cgEvalAlts: dodgy case of unboxed tuple type" )
    
    forkAbsC (	-- forkAbsC for the RHS, so that the envt is
		-- not changed for the mkRetDirect call
	bindUnboxedTupleComponents args		`thenFC` \ (live_regs, ptrs, nptrs, _) ->
		-- restore the CC *after* binding the tuple components, so that we
		-- get the stack offset of the saved CC right.
  	restoreCurrentCostCentre cc_slot True	`thenC` 
		-- Generate a heap check if necessary
  	unbxTupleHeapCheck live_regs ptrs nptrs AbsCNop (
		-- And finally the code for the alternative
  	cgExpr rhs
    ))						`thenFC` \ abs_c ->
    mkRetDirectTarget bndr abs_c srt		`thenFC` \ lbl ->
    returnFC (CaseAlts lbl Nothing False)

cgEvalAlts cc_slot bndr srt alt_type@(PrimAlt tycon) alts
  = forkAbsC (	-- forkAbsC for the RHS, so that the envt is
		-- not changed for the mkRetDirect call
  	restoreCurrentCostCentre cc_slot True		`thenC` 
	bindNewToReg bndr reg (mkLFArgument bndr)	`thenC`
	cgPrimAlts GCMayHappen (CReg reg) alts alt_type
    ) 						`thenFC` \ abs_c ->
    mkRetDirectTarget bndr abs_c srt		`thenFC` \ lbl ->
    returnFC (CaseAlts lbl Nothing False)
  where
    reg  = dataReturnConvPrim kind
    kind = tyConPrimRep tycon

cgEvalAlts cc_slot bndr srt alt_type alts
  = 	-- Algebraic and polymorphic case
	-- Bind the default binder
    bindNewToReg bndr node (mkLFArgument bndr) `thenC`

	-- Generate sequel info for use downstream
	-- At the moment, we only do it if the type is vector-returnable.
	-- Reason: if not, then it costs extra to label the
	-- alternatives, because we'd get return code like:
	--
	--	switch TagReg { 0 : JMP(alt_1); 1 : JMP(alt_2) ..etc }
	--
	-- which is worse than having the alt code in the switch statement

    let	ret_conv = case alt_type of
			AlgAlt tc -> ctrlReturnConvAlg tc
			PolyAlt   -> UnvectoredReturn 0

	use_labelled_alts = case ret_conv of
				VectoredReturn _ -> True
				_	    	 -> False

	semi_tagged_stuff = cgSemiTaggedAlts use_labelled_alts bndr alts

    in
    cgAlgAlts GCMayHappen (getUnique bndr) 
	      cc_slot use_labelled_alts
	      alt_type alts 			`thenFC` \ tagged_alt_absCs ->

    mkRetVecTarget bndr tagged_alt_absCs 
		   srt ret_conv 		`thenFC` \ return_vec ->

    returnFC (CaseAlts return_vec semi_tagged_stuff False)
\end{code}


HWL comment on {\em GrAnSim\/}  (adding GRAN_YIELDs for context switch): If
we  do  an inlining of the  case  no separate  functions  for returning are
created, so we don't have to generate a GRAN_YIELD in that case.  This info
must be  propagated  to cgAlgAltRhs (where the  GRAN_YIELD  macro might  be
emitted). Hence, the new Bool arg to cgAlgAltRhs.

%************************************************************************
%*									*
\subsection[CgCase-alg-alts]{Algebraic alternatives}
%*									*
%************************************************************************

In @cgAlgAlts@, none of the binders in the alternatives are
assumed to be yet bound.

HWL comment on {\em GrAnSim\/} (adding GRAN_YIELDs for context switch): The
last   arg of  cgAlgAlts  indicates  if we  want  a context   switch at the
beginning of  each alternative. Normally we  want that. The  only exception
are inlined alternatives.

\begin{code}
cgAlgAlts :: GCFlag
       -> Unique
       -> Maybe VirtualSpOffset
       -> Bool				-- True <=> branches must be labelled
					-- 	(used for semi-tagging)
       -> AltType			-- ** AlgAlt or PolyAlt only **
       -> [StgAlt]			-- The alternatives
       -> FCode [(AltCon, AbstractC)]	-- The branches

cgAlgAlts gc_flag uniq restore_cc must_label_branches alt_type alts
  = forkAlts [ cgAlgAlt gc_flag uniq restore_cc must_label_branches alt_type alt
	     | alt <- alts]

cgAlgAlt :: GCFlag
      	 -> Unique -> Maybe VirtualSpOffset -> Bool	-- turgid state
      	 -> AltType					-- ** AlgAlt or PolyAlt only **
      	 -> StgAlt
      	 -> FCode (AltCon, AbstractC)

cgAlgAlt gc_flag uniq cc_slot must_label_branch
         alt_type (con, args, use_mask, rhs)
  = getAbsC (bind_con_args con args		`thenFC` \ _ ->
    	     restoreCurrentCostCentre cc_slot True	`thenC`
	     maybeAltHeapCheck gc_flag alt_type (cgExpr rhs)
    ) 						`thenFC` \ abs_c -> 
    let
	final_abs_c | must_label_branch = CCodeBlock lbl abs_c
		    | otherwise	        = abs_c
    in
    returnFC (con, final_abs_c)
  where
    lbl = case con of
	    DataAlt dc -> mkAltLabel uniq (dataConTag dc)
	    DEFAULT    -> mkDefaultLabel uniq
	    other      -> pprPanic "cgAlgAlt" (ppr con)

    bind_con_args DEFAULT      args = nopC
    bind_con_args (DataAlt dc) args = bindConArgs dc args
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-semi-tagged-alts]{The code to deal with sem-tagging}
%*									*
%************************************************************************

Turgid-but-non-monadic code to conjure up the required info from
algebraic case alternatives for semi-tagging.

\begin{code}
cgSemiTaggedAlts :: Bool 	-- True <=> use semitagging: each alt will be labelled
		 -> Id 
		 -> [StgAlt]
		 -> SemiTaggingStuff

cgSemiTaggedAlts False binder alts
  = Nothing
cgSemiTaggedAlts True binder alts
  = Just ([st_alt con args | (DataAlt con, args, _, _) <- alts],
	  case head alts of
	    (DEFAULT, _, _, _) -> Just st_deflt
	    other	       -> Nothing)
  where
    uniq = getUnique binder

    st_deflt = (binder,
	        (CCallProfCtrMacro FSLIT("RET_SEMI_BY_DEFAULT") [], -- ToDo: monadise?
	         mkDefaultLabel uniq))

    st_alt con args	-- Ha!  Nothing to do; Node already points to the thing
      =	 (con_tag,
	   (CCallProfCtrMacro FSLIT("RET_SEMI_IN_HEAP") -- ToDo: monadise?
		[mkIntCLit (length args)], -- how big the thing in the heap is
	     join_label)
	    )
      where
	con_tag	   = dataConTag con
	join_label = mkAltLabel uniq con_tag


tagToClosure :: TyCon -> CAddrMode -> CAddrMode
-- Primops returning an enumeration type (notably Bool)
-- actually return an index into
-- the table of closures for the enumeration type
tagToClosure tycon tag_amode
  = CVal (CIndex closure_tbl tag_amode PtrRep) PtrRep
  where
    closure_tbl = CLbl (mkClosureTblLabel tycon) PtrRep
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-prim-alts]{Primitive alternatives}
%*									*
%************************************************************************

@cgPrimAlts@ generates suitable a @CSwitch@
for dealing with the alternatives of a primitive @case@, given an
addressing mode for the thing to scrutinise.  It also keeps track of
the maximum stack depth encountered down any branch.

As usual, no binders in the alternatives are yet bound.

\begin{code}
cgPrimAlts :: GCFlag
	   -> CAddrMode	-- Scrutinee
	   -> [StgAlt]	-- Alternatives
	   -> AltType	
	   -> Code
-- INVARIANT: the default binder is already bound
cgPrimAlts gc_flag scrutinee alts alt_type
  = forkAlts (map (cgPrimAlt gc_flag alt_type) alts)	`thenFC` \ tagged_absCs ->
    let
	((DEFAULT, deflt_absC) : others) = tagged_absCs		-- There is always a default
	alt_absCs = [(lit,rhs) | (LitAlt lit, rhs) <- others]
    in
    absC (CSwitch scrutinee alt_absCs deflt_absC)
	-- CSwitch does sensible things with one or zero alternatives

cgPrimAlt :: GCFlag
	  -> AltType
	  -> StgAlt			-- The alternative
	  -> FCode (AltCon, AbstractC)	-- Its compiled form

cgPrimAlt gc_flag alt_type (con, [], [], rhs)
  = ASSERT( case con of { DEFAULT -> True; LitAlt _ -> True; other -> False } )
    getAbsC (maybeAltHeapCheck gc_flag alt_type (cgExpr rhs))	`thenFC` \ abs_c ->
    returnFC (con, abs_c)
\end{code}


%************************************************************************
%*									*
\subsection[CgCase-tidy]{Code for tidying up prior to an eval}
%*									*
%************************************************************************

\begin{code}
maybeAltHeapCheck 
	:: GCFlag 
	-> AltType	-- PolyAlt, PrimAlt, AlgAlt, but *not* UbxTupAlt
	-> Code		-- Continuation
	-> Code
maybeAltHeapCheck NoGC	      _        code = code
maybeAltHeapCheck GCMayHappen alt_type code 
  = 	-- HWL: maybe need yield here
	-- yield [node] True	-- XXX live regs wrong
    altHeapCheck alt_type code

saveVolatileVarsAndRegs
    :: StgLiveVars                    -- Vars which should be made safe
    -> FCode (AbstractC,              -- Assignments to do the saves
	      EndOfBlockInfo,	      -- sequel for the alts
              Maybe VirtualSpOffset)  -- Slot for current cost centre

saveVolatileVarsAndRegs vars
  = saveVolatileVars vars       `thenFC` \ var_saves ->
    saveCurrentCostCentre 	`thenFC` \ (maybe_cc_slot, cc_save) ->
    getEndOfBlockInfo           `thenFC` \ eob_info ->
    returnFC (mkAbstractCs [var_saves, cc_save],
	      eob_info,
	      maybe_cc_slot)


saveVolatileVars :: StgLiveVars		-- Vars which should be made safe
		 -> FCode AbstractC	-- Assignments to to the saves

saveVolatileVars vars
  = save_em (varSetElems vars)
  where
    save_em [] = returnFC AbsCNop

    save_em (var:vars)
      = getCAddrModeIfVolatile var `thenFC` \ v ->
	case v of
	    Nothing	    -> save_em vars -- Non-volatile, so carry on


	    Just vol_amode  ->	-- Aha! It's volatile
			       save_var var vol_amode 	`thenFC` \ abs_c ->
			       save_em vars		`thenFC` \ abs_cs ->
			       returnFC (abs_c `mkAbsCStmts` abs_cs)

    save_var var vol_amode
      = allocPrimStack (getPrimRepSize kind) 	`thenFC` \ slot ->
	rebindToStack var slot	    	`thenC`
	getSpRelOffset slot		`thenFC` \ sp_rel ->
	returnFC (CAssign (CVal sp_rel kind) vol_amode)
      where
	kind = getAmodeRep vol_amode
\end{code}

---------------------------------------------------------------------------

When we save the current cost centre (which is done for lexical
scoping), we allocate a free stack location, and return (a)~the
virtual offset of the location, to pass on to the alternatives, and
(b)~the assignment to do the save (just as for @saveVolatileVars@).

\begin{code}
saveCurrentCostCentre ::
	FCode (Maybe VirtualSpOffset,	-- Where we decide to store it
	       AbstractC)		-- Assignment to save it

saveCurrentCostCentre
  = if not opt_SccProfilingOn then
	returnFC (Nothing, AbsCNop)
    else
	allocPrimStack (getPrimRepSize CostCentreRep) `thenFC` \ slot ->
	dataStackSlots [slot]			      `thenC`
	getSpRelOffset slot   		     	      `thenFC` \ sp_rel ->
	returnFC (Just slot,
		  CAssign (CVal sp_rel CostCentreRep) (CReg CurCostCentre))

-- Sometimes we don't free the slot containing the cost centre after restoring it
-- (see CgLetNoEscape.cgLetNoEscapeBody).
restoreCurrentCostCentre :: Maybe VirtualSpOffset -> Bool -> Code
restoreCurrentCostCentre Nothing     _freeit = nopC
restoreCurrentCostCentre (Just slot) freeit
 = getSpRelOffset slot				     `thenFC` \ sp_rel ->
   (if freeit then freeStackSlots [slot] else nopC)  `thenC`
   absC (CCallProfCCMacro FSLIT("RESTORE_CCCS") [CVal sp_rel CostCentreRep])
    -- we use the RESTORE_CCCS macro, rather than just
    -- assigning into CurCostCentre, in case RESTORE_CCCS
    -- has some sanity-checking in it.
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-return-vec]{Building a return vector}
%*									*
%************************************************************************

Build a return vector, and return a suitable label addressing
mode for it.

\begin{code}
mkRetDirectTarget :: Id 		-- Used for labelling only
		  -> AbstractC 		-- Return code
		  -> SRT		-- Live CAFs in return code
		  -> FCode CAddrMode	-- Emit the labelled return block, 
					-- and return its label
mkRetDirectTarget bndr abs_c srt
  = buildContLivenessMask bndr				`thenFC` \ liveness ->
    getSRTInfo name srt					`thenFC` \ srt_info -> 
    absC (CRetDirect uniq abs_c srt_info liveness)	`thenC`
    return lbl
  where
    name = idName bndr
    uniq = getUnique name
    lbl  = CLbl (mkReturnInfoLabel uniq) RetRep
\end{code}

\begin{code}
mkRetVecTarget :: Id			-- Just for its unique
	       -> [(AltCon, AbstractC)] -- Branch codes
	       -> SRT			-- Continuation's SRT
	       -> CtrlReturnConvention
	       -> FCode CAddrMode

mkRetVecTarget bndr tagged_alt_absCs srt (UnvectoredReturn 0)
  = ASSERT( null other_alts )
    mkRetDirectTarget bndr deflt_absC srt
  where
    ((DEFAULT, deflt_absC) : other_alts) = tagged_alt_absCs

mkRetVecTarget bndr tagged_alt_absCs srt (UnvectoredReturn n)
  = mkRetDirectTarget bndr switch_absC srt
  where
         -- Find the tag explicitly rather than using tag_reg for now.
	 -- on architectures with lots of regs the tag will be loaded
	 -- into tag_reg by the code doing the returning.
    tag = CMacroExpr WordRep GET_TAG [CVal (nodeRel 0) DataPtrRep]
    switch_absC = mkAlgAltsCSwitch tag tagged_alt_absCs
	  

mkRetVecTarget bndr tagged_alt_absCs srt (VectoredReturn table_size)
  = buildContLivenessMask bndr  `thenFC` \ liveness ->
    getSRTInfo name srt		`thenFC` \ srt_info ->
    let 
	ret_vector = CRetVector vtbl_lbl vector_table srt_info liveness
    in
    absC (mkAbstractCs alts_absCs `mkAbsCStmts` ret_vector)	`thenC`
		 -- Alts come first, because we don't want to declare all the symbols

    return (CLbl vtbl_lbl DataPtrRep)
  where
    tags 	 = [fIRST_TAG .. (table_size+fIRST_TAG-1)]
    vector_table = map mk_vector_entry tags
    alts_absCs   = map snd (sortBy cmp tagged_alt_absCs)
			-- The sort is unnecessary; just there for now
			-- to make the new order the same as the old
    (DEFAULT,_) `cmp` (DEFAULT,_) = EQ
    (DEFAULT,_) `cmp` _	  = GT
    (DataAlt d1,_) `cmp` (DataAlt d2,_) = dataConTag d1 `compare` dataConTag d2
    (DataAlt d1,_) `cmp` (DEFAULT, _)   = LT
	-- Others impossible

    name       = idName bndr
    uniq       = getUnique name 
    vtbl_lbl   = mkVecTblLabel uniq

    deflt_lbl :: CAddrMode
    deflt_lbl = case tagged_alt_absCs of
		   (DEFAULT, abs_c) : _ -> get_block_label abs_c
		   other 		-> mkIntCLit 0
			-- 'other' case: the simplifier might have eliminated a case
			-- 		  so we may have e.g. case xs of 
			--					 [] -> e
			-- In that situation the default should never be taken, 
			-- so we just use '0' (=> seg fault if used)

    mk_vector_entry :: ConTag -> CAddrMode
    mk_vector_entry tag
      = case [ absC | (DataAlt d, absC) <- tagged_alt_absCs, dataConTag d == tag ] of
		-- The comprehension neatly, and correctly, ignores the DEFAULT
	     []      -> deflt_lbl
	     [abs_c] -> get_block_label abs_c
	     _       -> panic "mkReturnVector: too many"

    get_block_label (CCodeBlock lbl _) = CLbl lbl CodePtrRep
\end{code}
