%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgCase.lhs,v 1.38 2000/03/23 17:45:19 simonpj Exp $
%
%********************************************************
%*							*
\section[CgCase]{Converting @StgCase@ expressions}
%*							*
%********************************************************

\begin{code}
module CgCase (	cgCase, saveVolatileVarsAndRegs, restoreCurrentCostCentre
	) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr  ( cgExpr )

import CgMonad
import StgSyn
import AbsCSyn

import AbsCUtils	( mkAbstractCs, mkAbsCStmts, mkAlgAltsCSwitch,
			  getAmodeRep, nonemptyAbsC
			)
import CgUpdate		( reserveSeqFrame )
import CgBindery	( getVolatileRegs, getArgAmodes, getArgAmode,
			  bindNewToReg, bindNewToTemp,
			  bindNewPrimToAmode,
			  rebindToStack, getCAddrMode,
			  getCAddrModeAndInfo, getCAddrModeIfVolatile,
			  buildContLivenessMask, nukeDeadBindings,
			)
import CgCon		( bindConArgs, bindUnboxedTupleComponents )
import CgHeapery	( altHeapCheck, yield )
import CgRetConv	( dataReturnConvPrim, ctrlReturnConvAlg,
			  CtrlReturnConvention(..)
			)
import CgStackery	( allocPrimStack, allocStackTop,
			  deAllocStackTop, freeStackSlots, dataStackSlots
			)
import CgTailCall	( tailCallFun )
import CgUsages		( getSpRelOffset, getRealSp )
import CLabel		( CLabel, mkVecTblLabel, mkReturnPtLabel, 
			  mkDefaultLabel, mkAltLabel, mkReturnInfoLabel,
			  mkErrorStdEntryLabel, mkClosureTblLabel
			)
import ClosureInfo	( mkLFArgument )
import CmdLineOpts	( opt_SccProfilingOn, opt_GranMacros )
import CostCentre	( CostCentre )
import Id		( Id, idPrimRep, isDeadBinder )
import DataCon		( DataCon, dataConTag, fIRST_TAG, ConTag,
			  isUnboxedTupleCon )
import VarSet		( varSetElems )
import Literal		( Literal )
import PrimOp		( primOpOutOfLine, PrimOp(..) )
import PrimRep		( getPrimRepSize, retPrimRepSize, PrimRep(..)
			)
import TyCon		( TyCon, isEnumerationTyCon, isUnboxedTupleTyCon,
			  isNewTyCon, isAlgTyCon, isFunTyCon, isPrimTyCon,
			  tyConDataCons, tyConFamilySize )
import Type		( Type, typePrimRep, splitAlgTyConApp, 
			  splitTyConApp_maybe, repType )
import Unique           ( Unique, Uniquable(..), mkPseudoUnique1 )
import Maybes		( maybeToBool )
import Util
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
	-> StgCaseAlts
	-> Code
\end{code}

Special case #1:  PrimOps returning enumeration types.

For enumeration types, we invent a temporary (builtin-unique 1) to
hold the tag, and cross our fingers that this doesn't clash with
anything else.  Builtin-unique 0 is used for a similar reason when
compiling enumerated-type primops in CgExpr.lhs.  We can't use the
unique from the case binder, because this is used to hold the actual
closure (when the case binder is live, that is).

There is an extra special case for

	case tagToEnum# x of
		...

which generates no code for the primop, unless x is used in the
alternatives (in which case we lookup the tag in the relevant closure
table to get the closure).

Being a bit short of uniques for temporary variables here, we use
mkPseudoUnique1 to generate a temporary for the tag.  We can't use
mkBuiltinUnique, because that occasionally clashes with some
temporaries generated for _ccall_GC, amongst others (see CgExpr.lhs).

\begin{code}
cgCase (StgPrimApp op args res_ty)
         live_in_whole_case live_in_alts bndr srt (StgAlgAlts ty alts deflt)
  | isEnumerationTyCon tycon
  = getArgAmodes args `thenFC` \ arg_amodes ->

    let tag_amode = case op of 
			TagToEnumOp -> only arg_amodes
			_ -> CTemp (mkPseudoUnique1{-see above-} 1) IntRep

	closure = CVal (CIndex (CLbl (mkClosureTblLabel tycon) PtrRep) tag_amode PtrRep) PtrRep
    in

    case op of {
	TagToEnumOp -> nopC;  -- no code!

	_ -> 	-- Perform the operation
	       getVolatileRegs live_in_alts     `thenFC` \ vol_regs ->

 	       absC (COpStmt [tag_amode] op
		 arg_amodes -- note: no liveness arg
		 vol_regs)
    } 						`thenC`

 	-- bind the default binder if necessary
	-- The deadness info is set by StgVarInfo
    (if (isDeadBinder bndr)
	then nopC
	else bindNewToTemp bndr 		`thenFC` \ bndr_amode ->
	     absC (CAssign bndr_amode closure))
						`thenC`

	-- compile the alts
    cgAlgAlts NoGC uniq Nothing{-cc_slot-} False{-no semi-tagging-}
		False{-not poly case-} alts deflt
                False{-don't emit yield-}  	`thenFC` \ (tagged_alts, deflt_c) ->

	-- Do the switch
    absC (mkAlgAltsCSwitch tag_amode tagged_alts deflt_c)

   where
	(Just (tycon,_)) = splitTyConApp_maybe res_ty
	uniq = getUnique bndr
\end{code}

Special case #2: inline PrimOps.

\begin{code}
cgCase (StgPrimApp op args res_ty) 
	live_in_whole_case live_in_alts bndr srt alts
  | not (primOpOutOfLine op)
  =
	-- Get amodes for the arguments and results
    getArgAmodes args			`thenFC` \ arg_amodes ->
    let
	result_amodes = getPrimAppResultAmodes (getUnique bndr) alts
    in
	-- Perform the operation
    getVolatileRegs live_in_alts        `thenFC` \ vol_regs ->

    absC (COpStmt result_amodes op
		 arg_amodes -- note: no liveness arg
		 vol_regs) 		`thenC`

	-- Scrutinise the result
    cgInlineAlts bndr alts
\end{code}

TODO: Case-of-case of primop can probably be done inline too (but
maybe better to translate it out beforehand).  See
ghc/lib/misc/PackedString.lhs for examples where this crops up (with
4.02).

Another special case: scrutinising a primitive-typed variable.	No
evaluation required.  We don't save volatile variables, nor do we do a
heap-check in the alternatives.	 Instead, the heap usage of the
alternatives is worst-cased and passed upstream.  This can result in
allocating more heap than strictly necessary, but it will sometimes
eliminate a heap check altogether.

\begin{code}
cgCase (StgApp v []) live_in_whole_case live_in_alts bndr srt
			(StgPrimAlts ty alts deflt)

  = 
    getCAddrMode v		`thenFC` \amode ->

    {- 
       Careful! we can't just bind the default binder to the same thing
       as the scrutinee, since it might be a stack location, and having
       two bindings pointing at the same stack locn doesn't work (it
       confuses nukeDeadBindings).  Hence, use a new temp.
    -}
    bindNewToTemp bndr  		`thenFC`  \deflt_amode ->
    absC (CAssign deflt_amode amode)	`thenC`

    cgPrimAlts NoGC amode alts deflt []
\end{code}

Special case: scrutinising a non-primitive variable.
This can be done a little better than the general case, because
we can reuse/trim the stack slot holding the variable (if it is in one).

\begin{code}
cgCase (StgApp fun args)
	live_in_whole_case live_in_alts bndr srt alts@(StgAlgAlts ty _ _)
  =
    getCAddrModeAndInfo fun		`thenFC` \ (fun_amode, lf_info) ->
    getArgAmodes args			`thenFC` \ arg_amodes ->

	-- Squish the environment
    nukeDeadBindings live_in_alts	`thenC`
    saveVolatileVarsAndRegs live_in_alts
    	    	    	`thenFC` \ (save_assts, alts_eob_info, maybe_cc_slot) ->

    allocStackTop retPrimRepSize	`thenFC` \_ ->

    forkEval alts_eob_info nopC (
	        deAllocStackTop retPrimRepSize `thenFC` \_ ->
		cgEvalAlts maybe_cc_slot bndr srt alts) 
					 `thenFC` \ scrut_eob_info ->

    let real_scrut_eob_info =
		if not_con_ty
 	   		then reserveSeqFrame scrut_eob_info
	   		else scrut_eob_info
    in

    setEndOfBlockInfo real_scrut_eob_info (
      tailCallFun fun fun_amode lf_info arg_amodes save_assts
      )

  where
     not_con_ty = case (getScrutineeTyCon ty) of
      			Just _ -> False
		        other  -> True
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
cgCase expr live_in_whole_case live_in_alts bndr srt alts
  =	-- Figure out what volatile variables to save
    nukeDeadBindings live_in_whole_case	`thenC`
    
    saveVolatileVarsAndRegs live_in_alts
    	    	    	`thenFC` \ (save_assts, alts_eob_info, maybe_cc_slot) ->

    -- Save those variables right now!
    absC save_assts 	    	    	`thenC`

    -- generate code for the alts
    forkEval alts_eob_info
    	(
	 nukeDeadBindings live_in_alts `thenC` 
	 allocStackTop retPrimRepSize   -- space for retn address 
	 `thenFC` \_ -> nopC
	 )
	(deAllocStackTop retPrimRepSize `thenFC` \_ ->
	 cgEvalAlts maybe_cc_slot bndr srt alts) `thenFC` \ scrut_eob_info ->

    let real_scrut_eob_info =
		if not_con_ty
 	   		then reserveSeqFrame scrut_eob_info
	   		else scrut_eob_info
    in

    setEndOfBlockInfo real_scrut_eob_info (cgExpr expr)

  where
     not_con_ty = case (getScrutineeTyCon (alts_ty alts)) of
      			Just _ -> False
		        other  -> True
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
alts_ty (StgAlgAlts ty _ _) = ty
alts_ty (StgPrimAlts ty _ _) = ty
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-primops]{Primitive applications}
%*									*
%************************************************************************

Get result amodes for a primitive operation, in the case wher GC can't happen.
The  amodes are returned in canonical order, ready for the prim-op!

	Alg case: temporaries named as in the alternatives,
		  plus (CTemp u) for the tag (if needed)
	Prim case: (CTemp u)

This is all disgusting, because these amodes must be consistent with those
invented by CgAlgAlts.

\begin{code}
getPrimAppResultAmodes
	:: Unique
	-> StgCaseAlts
	-> [CAddrMode]

getPrimAppResultAmodes uniq (StgAlgAlts ty alts some_default)

  | isUnboxedTupleTyCon tycon = 
	case alts of 
	    [(con, args, use_mask, rhs)] -> 
		[ CTemp (getUnique arg) (idPrimRep arg) | arg <- args ]
	    _ -> panic "getPrimAppResultAmodes: case of unboxed tuple has multiple branches"

  | otherwise = panic ("getPrimAppResultAmodes: case of primop has strange type: " ++ showSDoc (ppr ty))

  where (tycon, _, _) = splitAlgTyConApp ty

-- The situation is simpler for primitive results, because there is only
-- one!

getPrimAppResultAmodes uniq (StgPrimAlts ty _ _)
  = [CTemp uniq (typePrimRep ty)]
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
	   -> StgCaseAlts
	   -> FCode Sequel	-- Any addr modes inside are guaranteed
				-- to be a label so that we can duplicate it 
				-- without risk of duplicating code

cgEvalAlts cc_slot bndr srt alts
  = 	
    let uniq = getUnique bndr in

    buildContLivenessMask uniq	        `thenFC` \ liveness_mask ->

    case alts of

      -- algebraic alts ...
      (StgAlgAlts ty alts deflt) ->

	   -- bind the default binder (it covers all the alternatives)
    	bindNewToReg bndr node mkLFArgument	 `thenC`

	-- Generate sequel info for use downstream
	-- At the moment, we only do it if the type is vector-returnable.
	-- Reason: if not, then it costs extra to label the
	-- alternatives, because we'd get return code like:
	--
	--	switch TagReg { 0 : JMP(alt_1); 1 : JMP(alt_2) ..etc }
	--
	-- which is worse than having the alt code in the switch statement

    	let	tycon_info   	= getScrutineeTyCon ty
		is_alg		= maybeToBool tycon_info
		Just spec_tycon = tycon_info
    	in

    	-- deal with the unboxed tuple case
    	if is_alg && isUnboxedTupleTyCon spec_tycon then
	    case alts of 
         	[alt] -> let lbl = mkReturnInfoLabel uniq in
			 cgUnboxedTupleAlt uniq cc_slot True alt
				`thenFC` \ abs_c ->
		  	 getSRTLabel `thenFC` \srt_label -> 
		  	 absC (CRetDirect uniq abs_c (srt_label, srt) 
					liveness_mask) `thenC`
		  	returnFC (CaseAlts (CLbl lbl RetRep) Nothing)
	 	_ -> panic "cgEvalAlts: dodgy case of unboxed tuple type"

    	-- normal algebraic (or polymorphic) case alternatives
    	else let
		ret_conv | is_alg    = ctrlReturnConvAlg spec_tycon
			 | otherwise = UnvectoredReturn 0

		use_labelled_alts = case ret_conv of
	      				VectoredReturn _ -> True
	      				_	    	 -> False

		semi_tagged_stuff
    	  	   = if use_labelled_alts then
			cgSemiTaggedAlts bndr alts deflt -- Just <something>
	    	     else
			Nothing -- no semi-tagging info

      	in
      	cgAlgAlts GCMayHappen uniq cc_slot use_labelled_alts (not is_alg) 
		alts deflt True	`thenFC` \ (tagged_alt_absCs, deflt_absC) ->

      	mkReturnVector uniq tagged_alt_absCs deflt_absC srt liveness_mask 
		ret_conv  `thenFC` \ return_vec ->

      	returnFC (CaseAlts return_vec semi_tagged_stuff)

      -- primitive alts...
      (StgPrimAlts ty alts deflt) ->

	-- Restore the cost centre
	restoreCurrentCostCentre cc_slot 	`thenFC` \ cc_restore ->

    	-- Generate the switch
    	getAbsC (cgPrimEvalAlts bndr ty alts deflt)  	`thenFC` \ abs_c ->

    	-- Generate the labelled block, starting with restore-cost-centre
    	getSRTLabel 					`thenFC` \srt_label ->
    	absC (CRetDirect uniq (cc_restore `mkAbsCStmts` abs_c) 
			(srt_label,srt) liveness_mask)	`thenC`

	-- Return an amode for the block
    	returnFC (CaseAlts (CLbl (mkReturnInfoLabel uniq) RetRep) Nothing)
\end{code}


\begin{code}
cgInlineAlts :: Id
    	     -> StgCaseAlts
    	     -> Code
\end{code}

HWL comment on {\em GrAnSim\/}  (adding GRAN_YIELDs for context switch): If
we  do  an inlining of the  case  no separate  functions  for returning are
created, so we don't have to generate a GRAN_YIELD in that case.  This info
must be  propagated  to cgAlgAltRhs (where the  GRAN_YIELD  macro might  be
emitted). Hence, the new Bool arg to cgAlgAltRhs.

First case: primitive op returns an unboxed tuple.

\begin{code}
cgInlineAlts bndr (StgAlgAlts ty [alt@(con,args,use_mask,rhs)] StgNoDefault)
  | isUnboxedTupleCon con
  = -- no heap check, no yield, just get in there and do it.
    mapFCs bindNewToTemp args `thenFC` \ _ ->
    cgExpr rhs

  | otherwise
  = panic "cgInlineAlts: single alternative, not an unboxed tuple"
\end{code}

Third (real) case: primitive result type.

\begin{code}
cgInlineAlts bndr (StgPrimAlts ty alts deflt)
  = cgPrimInlineAlts bndr ty alts deflt
\end{code}

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
	  -> Bool				-- True <=> polymorphic case
	  -> [(DataCon, [Id], [Bool], StgExpr)]	-- The alternatives
	  -> StgCaseDefault			-- The default
          -> Bool                               -- Context switch at alts?
	  -> FCode ([(ConTag, AbstractC)],	-- The branches
		    AbstractC			-- The default case
	     )

cgAlgAlts gc_flag uniq restore_cc must_label_branches is_fun alts deflt
          emit_yield{-should a yield macro be emitted?-}

  = forkAlts (map (cgAlgAlt gc_flag uniq restore_cc must_label_branches emit_yield) alts)
	     (cgAlgDefault gc_flag is_fun uniq restore_cc must_label_branches deflt emit_yield)
\end{code}

\begin{code}
cgAlgDefault :: GCFlag
	     -> Bool 			-- could be a function-typed result?
	     -> Unique -> Maybe VirtualSpOffset -> Bool -- turgid state...
	     -> StgCaseDefault		-- input
	     -> Bool
	     -> FCode AbstractC		-- output

cgAlgDefault gc_flag is_fun uniq cc_slot must_label_branch StgNoDefault _
  = returnFC AbsCNop

cgAlgDefault gc_flag is_fun uniq cc_slot must_label_branch
	     (StgBindDefault rhs)
          emit_yield{-should a yield macro be emitted?-}

  = 	-- We have arranged that Node points to the thing
    restoreCurrentCostCentre cc_slot `thenFC` \restore_cc ->
    getAbsC (absC restore_cc `thenC`
             -- HWL: maybe need yield here
             --(if emit_yield
             --   then yield [node] True
             --   else absC AbsCNop)                            `thenC`     
	     possibleHeapCheck gc_flag is_fun [node] [] Nothing (cgExpr rhs)
	-- Node is live, but doesn't need to point at the thing itself;
	-- it's ok for Node to point to an indirection or FETCH_ME
	-- Hence no need to re-enter Node.
    )					`thenFC` \ abs_c ->

    let
	final_abs_c | must_label_branch = CCodeBlock lbl abs_c
		    | otherwise	        = abs_c
    in
    returnFC final_abs_c
  where
    lbl = mkDefaultLabel uniq

-- HWL comment on GrAnSim: GRAN_YIELDs needed; emitted in cgAlgAltRhs

cgAlgAlt :: GCFlag
	 -> Unique -> Maybe VirtualSpOffset -> Bool	-- turgid state
	 -> Bool                               -- Context switch at alts?
	 -> (DataCon, [Id], [Bool], StgExpr)
	 -> FCode (ConTag, AbstractC)

cgAlgAlt gc_flag uniq cc_slot must_label_branch 
         emit_yield{-should a yield macro be emitted?-}
         (con, args, use_mask, rhs)
  = 
    restoreCurrentCostCentre cc_slot `thenFC` \restore_cc ->
    getAbsC (absC restore_cc `thenC`
             -- HWL: maybe need yield here
    	     -- (if emit_yield
      	     --    then yield [node] True		-- XXX live regs wrong
      	     --    else absC AbsCNop)                               `thenC`    
    	     (case gc_flag of
		NoGC   	    -> mapFCs bindNewToTemp args `thenFC` \_ -> nopC
    		GCMayHappen -> bindConArgs con args
    	     )	`thenC`
    	     possibleHeapCheck gc_flag False [node] [] Nothing (
	     cgExpr rhs)
            ) `thenFC` \ abs_c -> 
    let
	final_abs_c | must_label_branch = CCodeBlock lbl abs_c
		    | otherwise	        = abs_c
    in
    returnFC (tag, final_abs_c)
  where
    tag	= dataConTag con
    lbl = mkAltLabel uniq tag

cgUnboxedTupleAlt
	:: Unique			-- unique for label of the alternative
	-> Maybe VirtualSpOffset	-- Restore cost centre
	-> Bool				-- ctxt switch
	-> (DataCon, [Id], [Bool], StgExpr) -- alternative
	-> FCode AbstractC

cgUnboxedTupleAlt lbl cc_slot emit_yield (con,args,use_mask,rhs)
  = getAbsC (
  	bindUnboxedTupleComponents args 
  		      `thenFC` \ (live_regs,tags,stack_res) ->

        restoreCurrentCostCentre cc_slot `thenFC` \restore_cc ->
	absC restore_cc `thenC`

        -- HWL: maybe need yield here
  	-- (if emit_yield
  	--    then yield live_regs True		-- XXX live regs wrong?
  	--    else absC AbsCNop)                         `thenC`     
  	let 
	      -- ToDo: could maybe use Nothing here if stack_res is False
	      -- since the heap-check can just return to the top of the 
	      -- stack.
  	      ret_addr = Just lbl
  	in

	-- free up stack slots containing tags,
	freeStackSlots (map fst tags) 		`thenC`

	-- generate a heap check if necessary
  	possibleHeapCheck GCMayHappen False live_regs tags ret_addr (

	-- and finally the code for the alternative
  	cgExpr rhs)
    )
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-semi-tagged-alts]{The code to deal with sem-tagging}
%*									*
%************************************************************************

Turgid-but-non-monadic code to conjure up the required info from
algebraic case alternatives for semi-tagging.

\begin{code}
cgSemiTaggedAlts :: Id
		 -> [(DataCon, [Id], [Bool], StgExpr)]
		 -> GenStgCaseDefault Id Id
		 -> SemiTaggingStuff

cgSemiTaggedAlts binder alts deflt
  = Just (map st_alt alts, st_deflt deflt)
  where
    uniq        = getUnique binder

    st_deflt StgNoDefault = Nothing

    st_deflt (StgBindDefault _)
      = Just (Just binder,
	      (CCallProfCtrMacro SLIT("RET_SEMI_BY_DEFAULT") [], -- ToDo: monadise?
	       mkDefaultLabel uniq)
	     )

    st_alt (con, args, use_mask, _)
      =  -- Ha!  Nothing to do; Node already points to the thing
	 (con_tag,
	   (CCallProfCtrMacro SLIT("RET_SEMI_IN_HEAP") -- ToDo: monadise?
		[mkIntCLit (length args)], -- how big the thing in the heap is
	     join_label)
	    )
      where
	con_tag	    = dataConTag con
	join_label  = mkAltLabel uniq con_tag
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-prim-alts]{Primitive alternatives}
%*									*
%************************************************************************

@cgPrimEvalAlts@ and @cgPrimInlineAlts@ generate suitable @CSwitch@es
for dealing with the alternatives of a primitive @case@, given an
addressing mode for the thing to scrutinise.  It also keeps track of
the maximum stack depth encountered down any branch.

As usual, no binders in the alternatives are yet bound.

\begin{code}
cgPrimInlineAlts bndr ty alts deflt
  = cgPrimAltsWithDefault bndr NoGC (CTemp uniq kind) alts deflt []
  where
	uniq = getUnique bndr
	kind = typePrimRep ty

cgPrimEvalAlts bndr ty alts deflt
  = cgPrimAltsWithDefault bndr GCMayHappen (CReg reg) alts deflt [reg]
  where
	reg  = WARN( case kind of { PtrRep -> True; other -> False }, text "cgPrimEE" <+> ppr bndr <+> ppr ty  )
	       dataReturnConvPrim kind
	kind = typePrimRep ty

cgPrimAltsWithDefault bndr gc_flag scrutinee alts deflt regs
  = 	-- first bind the default if necessary
    bindNewPrimToAmode bndr scrutinee	 	`thenC`
    cgPrimAlts gc_flag scrutinee alts deflt regs

cgPrimAlts gc_flag scrutinee alts deflt regs
  = forkAlts (map (cgPrimAlt gc_flag regs) alts)
	     (cgPrimDefault gc_flag regs deflt) 
					`thenFC` \ (alt_absCs, deflt_absC) ->

    absC (CSwitch scrutinee alt_absCs deflt_absC)
	-- CSwitch does sensible things with one or zero alternatives


cgPrimAlt :: GCFlag
	  -> [MagicId]		   	-- live registers
	  -> (Literal, StgExpr)		-- The alternative
	  -> FCode (Literal, AbstractC) -- Its compiled form

cgPrimAlt gc_flag regs (lit, rhs)
  = getAbsC rhs_code	 `thenFC` \ absC ->
    returnFC (lit,absC)
  where
    rhs_code = possibleHeapCheck gc_flag False regs [] Nothing (cgExpr rhs)

cgPrimDefault :: GCFlag
	      -> [MagicId]		-- live registers
	      -> StgCaseDefault
	      -> FCode AbstractC

cgPrimDefault gc_flag regs StgNoDefault
  = panic "cgPrimDefault: No default in prim case"

cgPrimDefault gc_flag regs (StgBindDefault rhs)
  = getAbsC (possibleHeapCheck gc_flag False regs [] Nothing (cgExpr rhs))
\end{code}


%************************************************************************
%*									*
\subsection[CgCase-tidy]{Code for tidying up prior to an eval}
%*									*
%************************************************************************

\begin{code}
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


saveVolatileVars :: StgLiveVars	-- Vars which should be made safe
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

restoreCurrentCostCentre :: Maybe VirtualSpOffset -> FCode AbstractC
restoreCurrentCostCentre Nothing = returnFC AbsCNop
restoreCurrentCostCentre (Just slot)
 = getSpRelOffset slot				 `thenFC` \ sp_rel ->
   freeStackSlots [slot]			 `thenC`
   returnFC (CCallProfCCMacro SLIT("RESTORE_CCCS") [CVal sp_rel CostCentreRep])
    -- we use the RESTORE_CCCS macro, rather than just
    -- assigning into CurCostCentre, in case RESTORE_CCC
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
mkReturnVector :: Unique
	       -> [(ConTag, AbstractC)] -- Branch codes
	       -> AbstractC		-- Default case
	       -> SRT			-- continuation's SRT
	       -> Liveness		-- stack liveness
	       -> CtrlReturnConvention
	       -> FCode CAddrMode

mkReturnVector uniq tagged_alt_absCs deflt_absC srt liveness ret_conv
  = getSRTLabel `thenFC` \srt_label ->
    let
     srt_info = (srt_label, srt)

     (return_vec_amode, vtbl_body) = case ret_conv of {

	-- might be a polymorphic case...
      UnvectoredReturn 0 ->
	ASSERT(null tagged_alt_absCs)
    	(CLbl ret_label RetRep,
	 absC (CRetDirect uniq deflt_absC (srt_label, srt) liveness));

      UnvectoredReturn n ->
        -- find the tag explicitly rather than using tag_reg for now.
	-- on architectures with lots of regs the tag will be loaded
	-- into tag_reg by the code doing the returning.
        let
	  tag = CMacroExpr WordRep GET_TAG [CVal (nodeRel 0) DataPtrRep]
        in
    	(CLbl ret_label RetRep,
	 absC (CRetDirect uniq 
			    (mkAlgAltsCSwitch tag tagged_alt_absCs deflt_absC)
			    (srt_label, srt)
			    liveness));

      VectoredReturn table_size ->
	let
	  (vector_table, alts_absC) = 
	    unzip (map mk_vector_entry [fIRST_TAG .. (table_size+fIRST_TAG-1)])

	  ret_vector = CRetVector vtbl_label
			  vector_table
			  (srt_label, srt) liveness
	in
    	(CLbl vtbl_label DataPtrRep, 
	 -- alts come first, because we don't want to declare all the symbols
	 absC (mkAbstractCs (mkAbstractCs alts_absC : [deflt_absC,ret_vector]))
	)

    } in
    vtbl_body    	    	    	    	    	    `thenC`
    returnFC return_vec_amode
    -- )
  where

    vtbl_label = mkVecTblLabel uniq
    ret_label = mkReturnInfoLabel uniq

    deflt_lbl = 
	case nonemptyAbsC deflt_absC of
		 -- the simplifier might have eliminated a case
	   Nothing -> CLbl mkErrorStdEntryLabel CodePtrRep 
 	   Just absC@(CCodeBlock lbl _) -> CLbl lbl CodePtrRep

    mk_vector_entry :: ConTag -> (CAddrMode, AbstractC)
    mk_vector_entry tag
      = case [ absC | (t, absC) <- tagged_alt_absCs, t == tag ] of
	     []     -> (deflt_lbl, AbsCNop)
	     [absC@(CCodeBlock lbl _)] -> (CLbl lbl CodePtrRep,absC)
	     _      -> panic "mkReturnVector: too many"
\end{code}

%************************************************************************
%*									*
\subsection[CgCase-utils]{Utilities for handling case expressions}
%*									*
%************************************************************************

@possibleHeapCheck@ tests a flag passed in to decide whether to do a
heap check or not.  These heap checks are always in a case
alternative, so we use altHeapCheck.

\begin{code}
possibleHeapCheck 
	:: GCFlag 
	-> Bool				--  True <=> algebraic case
	-> [MagicId]			--  live registers
	-> [(VirtualSpOffset,Int)]	--  stack slots to tag
	-> Maybe Unique			--  return address unique
	-> Code				--  continuation
	-> Code

possibleHeapCheck GCMayHappen is_alg regs tags lbl code 
  = altHeapCheck is_alg regs tags AbsCNop lbl code
possibleHeapCheck NoGC	_ _ tags lbl code 
  = code
\end{code}

\begin{code}
getScrutineeTyCon :: Type -> Maybe TyCon
getScrutineeTyCon ty =
   case splitTyConApp_maybe (repType ty) of
	Nothing -> Nothing
	Just (tc,_) -> 
		if isFunTyCon tc  then Nothing else     -- not interested in funs
		if isPrimTyCon tc then Just tc else	-- return primitive tycons
			-- otherwise (algebraic tycons) check the no. of constructors
		Just tc
\end{code}
