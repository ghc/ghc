%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgCase.lhs,v 1.74 2005/03/31 10:16:34 simonmar Exp $
%
%********************************************************
%*							*
\section[CgCase]{Converting @StgCase@ expressions}
%*							*
%********************************************************

\begin{code}
module CgCase (	cgCase, saveVolatileVarsAndRegs, 
		restoreCurrentCostCentre
	) where

#include "HsVersions.h"

import {-# SOURCE #-} CgExpr  ( cgExpr )

import CgMonad
import StgSyn
import CgBindery	( getArgAmodes,
			  bindNewToReg, bindNewToTemp,
			  getCgIdInfo, getArgAmode,
			  rebindToStack, getCAddrModeIfVolatile,
			  nukeDeadBindings, idInfoToAmode
			)
import CgCon		( bindConArgs, bindUnboxedTupleComponents )
import CgHeapery	( altHeapCheck, unbxTupleHeapCheck )
import CgCallConv	( dataReturnConvPrim, ctrlReturnConvAlg,
			  CtrlReturnConvention(..)
			)
import CgStackery	( allocPrimStack, allocStackTop, getSpRelOffset,
			  deAllocStackTop, freeStackSlots
			)
import CgTailCall	( performTailCall )
import CgPrimOp		( cgPrimOp )
import CgForeignCall	( cgForeignCall )
import CgUtils		( newTemp, cgLit, emitLitSwitch, emitSwitch,
			  tagToClosure )
import CgProf		( curCCS, curCCSAddr )
import CgInfoTbls	( emitDirectReturnTarget, emitAlgReturnTarget, 
			  dataConTagZ )
import SMRep		( CgRep(..), retAddrSizeW, nonVoidArg, isVoidArg,
			  idCgRep, tyConCgRep, typeHint )
import CmmUtils		( CmmStmts, noStmts, oneStmt, plusStmts )
import Cmm
import MachOp		( wordRep )
import ClosureInfo	( mkLFArgument )
import StaticFlags	( opt_SccProfilingOn )
import Id		( Id, idName, isDeadBinder, idType )
import ForeignCall	( ForeignCall(..), CCallSpec(..), playSafe )
import VarSet		( varSetElems )
import CoreSyn		( AltCon(..) )
import PrimOp		( PrimOp(..), primOpOutOfLine )
import TyCon		( isEnumerationTyCon, tyConFamilySize )
import Util		( isSingleton )
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
  = do	{ tmp_reg <- bindNewToTemp bndr
	; cm_lit <- cgLit lit
	; stmtC (CmmAssign tmp_reg (CmmLit cm_lit))
	; cgPrimAlts NoGC alt_type tmp_reg alts }
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
  = do	{ -- Careful! we can't just bind the default binder to the same thing
	  -- as the scrutinee, since it might be a stack location, and having
	  -- two bindings pointing at the same stack locn doesn't work (it
	  -- confuses nukeDeadBindings).  Hence, use a new temp.
	  v_info <- getCgIdInfo v
	; amode <- idInfoToAmode v_info
	; tmp_reg <- bindNewToTemp bndr
	; stmtC (CmmAssign tmp_reg amode)
	; cgPrimAlts NoGC alt_type tmp_reg alts }
\end{code}

Special case #3: inline PrimOps and foreign calls.

\begin{code}
cgCase (StgOpApp op@(StgPrimOp primop) args _) 
       live_in_whole_case live_in_alts bndr srt alt_type alts
  | not (primOpOutOfLine primop)
  = cgInlinePrimOp primop args bndr alt_type live_in_alts alts
\end{code}

TODO: Case-of-case of primop can probably be done inline too (but
maybe better to translate it out beforehand).  See
ghc/lib/misc/PackedString.lhs for examples where this crops up (with
4.02).

Special case #4: inline foreign calls: an unsafe foreign call can be done
right here, just like an inline primop.

\begin{code}
cgCase (StgOpApp op@(StgFCallOp fcall _) args _) 
       live_in_whole_case live_in_alts bndr srt alt_type alts
  | unsafe_foreign_call
  = ASSERT( isSingleton alts )
    do	--  *must* be an unboxed tuple alt.
	-- exactly like the cgInlinePrimOp case for unboxed tuple alts..
	{ res_tmps <- mapFCs bindNewToTemp non_void_res_ids
	; let res_hints = map (typeHint.idType) non_void_res_ids
	; cgForeignCall (zip res_tmps res_hints) fcall args live_in_alts
	; cgExpr rhs }
  where
   (_, res_ids, _, rhs) = head alts
   non_void_res_ids = filter (nonVoidArg . idCgRep) res_ids

   unsafe_foreign_call
	 = case fcall of
	 	CCall (CCallSpec _ _ s) -> not (playSafe s)
		_other			-> False				
\end{code}

Special case: scrutinising a non-primitive variable.
This can be done a little better than the general case, because
we can reuse/trim the stack slot holding the variable (if it is in one).

\begin{code}
cgCase (StgApp fun args)
	live_in_whole_case live_in_alts bndr srt alt_type alts
  = do	{ fun_info <- getCgIdInfo fun
	; arg_amodes <- getArgAmodes args

	-- Nuking dead bindings *before* calculating the saves is the
	-- value-add here.  We might end up freeing up some slots currently
	-- occupied by variables only required for the call.
	-- NOTE: we need to look up the variables used in the call before
	-- doing this, because some of them may not be in the environment
	-- afterward.
	; nukeDeadBindings live_in_alts	
	; (save_assts, alts_eob_info, maybe_cc_slot)
		<- saveVolatileVarsAndRegs live_in_alts

	; scrut_eob_info
	    <- forkEval alts_eob_info 
			(allocStackTop retAddrSizeW >> nopC)
			(do { deAllocStackTop retAddrSizeW
			    ; cgEvalAlts maybe_cc_slot bndr srt alt_type alts })

	; setEndOfBlockInfo (maybeReserveSeqFrame alt_type scrut_eob_info)
			    (performTailCall fun_info arg_amodes save_assts) }
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
  = do	{	-- Figure out what volatile variables to save
	  nukeDeadBindings live_in_whole_case
    
	; (save_assts, alts_eob_info, maybe_cc_slot)
		<- saveVolatileVarsAndRegs live_in_alts

	     -- Save those variables right now!
	; emitStmts save_assts

	    -- generate code for the alts
	; scrut_eob_info
	       <- forkEval alts_eob_info
		      	   (do 	{ nukeDeadBindings live_in_alts
				; allocStackTop retAddrSizeW   -- space for retn address 
				; nopC })
			   (do	{ deAllocStackTop retAddrSizeW
				; cgEvalAlts maybe_cc_slot bndr srt alt_type alts })

	; setEndOfBlockInfo (maybeReserveSeqFrame alt_type scrut_eob_info)
			    (cgExpr expr)
    }
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
different.  So we pick a free stack slot and save CCCS in it.  One
consequence of this is that activation records on the stack don't
follow the layout of closures when we're profiling.  The CCS could be
anywhere within the record).

\begin{code}
maybeReserveSeqFrame PolyAlt (EndOfBlockInfo args_sp (CaseAlts amode stuff bndr _))
   = EndOfBlockInfo (args_sp + retAddrSizeW) (CaseAlts amode stuff bndr True)
maybeReserveSeqFrame other scrut_eob_info = scrut_eob_info
\end{code}


%************************************************************************
%*									*
		Inline primops
%*									*
%************************************************************************

\begin{code}
cgInlinePrimOp primop args bndr (PrimAlt tycon) live_in_alts alts
  | isVoidArg (idCgRep bndr)
  = ASSERT( con == DEFAULT && isSingleton alts && null bs )
    do	{ 	-- VOID RESULT; just sequencing, 
		-- so get in there and do it
	  cgPrimOp [] primop args live_in_alts
	; cgExpr rhs }
  where
    (con,bs,_,rhs) = head alts

cgInlinePrimOp primop args bndr (PrimAlt tycon) live_in_alts alts
  = do	{ 	-- PRIMITIVE ALTS, with non-void result
	  tmp_reg <- bindNewToTemp bndr
	; cgPrimOp [tmp_reg] primop args live_in_alts
	; cgPrimAlts NoGC (PrimAlt tycon) tmp_reg alts }

cgInlinePrimOp primop args bndr (UbxTupAlt tycon) live_in_alts alts
  = ASSERT( isSingleton alts )
    do	{  	-- UNBOXED TUPLE ALTS
	 	-- No heap check, no yield, just get in there and do it.
		-- NB: the case binder isn't bound to anything; 
		--     it has a unboxed tuple type
	  
	  res_tmps <- mapFCs bindNewToTemp non_void_res_ids
	; cgPrimOp res_tmps primop args live_in_alts
	; cgExpr rhs }
  where
   (_, res_ids, _, rhs) = head alts
   non_void_res_ids = filter (nonVoidArg . idCgRep) res_ids

cgInlinePrimOp primop args bndr (AlgAlt tycon) live_in_alts alts
  = do 	{ 	-- ENUMERATION TYPE RETURN
		-- Typical: case a ># b of { True -> ..; False -> .. }
		-- The primop itself returns an index into the table of
		-- closures for the enumeration type.
	   tag_amode <- ASSERT( isEnumerationTyCon tycon )
			do_enum_primop primop

	 	-- Bind the default binder if necessary
		-- (avoiding it avoids the assignment)
		-- The deadness info is set by StgVarInfo
	; dflags <- getDynFlags
	; whenC (not (isDeadBinder bndr))
		(do { tmp_reg <- bindNewToTemp bndr
		    ; stmtC (CmmAssign tmp_reg (tagToClosure dflags tycon tag_amode)) })

		-- Compile the alts
	; (branches, mb_deflt) <- cgAlgAlts NoGC Nothing{-cc_slot-}
				   	    (AlgAlt tycon) alts

		-- Do the switch
	; emitSwitch tag_amode branches mb_deflt 0 (tyConFamilySize tycon - 1)
	}
  where

    do_enum_primop :: PrimOp -> FCode CmmExpr	-- Returns amode for result
    do_enum_primop TagToEnumOp	-- No code!
       | [arg] <- args = do
         (_,e) <- getArgAmode arg
	 return e
    do_enum_primop primop
      = do tmp <- newTemp wordRep
	   cgPrimOp [tmp] primop args live_in_alts
    	   returnFC (CmmReg tmp)

cgInlinePrimOp primop arg_amodes bndr PolyAlt live_in_alts alts
  = pprPanic "cgCase: case of primop has polymorphic type" (ppr bndr)
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

cgEvalAlts cc_slot bndr srt alt_type@(PrimAlt tycon) alts
  = do	{ let   rep = tyConCgRep tycon
		reg = dataReturnConvPrim rep	-- Bottom for voidRep

	; abs_c <- forkProc $ do
		{ 	-- Bind the case binder, except if it's void
			-- (reg is bottom in that case)
		  whenC (nonVoidArg rep) $
		  bindNewToReg bndr reg (mkLFArgument bndr)
		; restoreCurrentCostCentre cc_slot True
		; cgPrimAlts GCMayHappen alt_type reg alts }

	; lbl <- emitDirectReturnTarget (idName bndr) abs_c srt
	; returnFC (CaseAlts lbl Nothing bndr False) }

cgEvalAlts cc_slot bndr srt (UbxTupAlt _) [(con,args,_,rhs)]
  =	-- Unboxed tuple case
	-- By now, the simplifier should have have turned it
	-- into 	case e of (# a,b #) -> e
	-- There shouldn't be a 
	--		case e of DEFAULT -> e
    ASSERT2( case con of { DataAlt _ -> True; other -> False },
	     text "cgEvalAlts: dodgy case of unboxed tuple type" )
    do	{ 	-- forkAbsC for the RHS, so that the envt is
		-- not changed for the emitDirectReturn call
	  abs_c <- forkProc $ do 
		{ (live_regs, ptrs, nptrs, _) <- bindUnboxedTupleComponents args
			-- Restore the CC *after* binding the tuple components, 
			-- so that we get the stack offset of the saved CC right.
		; restoreCurrentCostCentre cc_slot True
			-- Generate a heap check if necessary
			-- and finally the code for the alternative
		; unbxTupleHeapCheck live_regs ptrs nptrs noStmts
				     (cgExpr rhs) }
	; lbl <- emitDirectReturnTarget (idName bndr) abs_c srt
	; returnFC (CaseAlts lbl Nothing bndr False) }

cgEvalAlts cc_slot bndr srt alt_type alts
  = 	-- Algebraic and polymorphic case
    do	{	-- Bind the default binder
	  bindNewToReg bndr nodeReg (mkLFArgument bndr)

	-- Generate sequel info for use downstream
	-- At the moment, we only do it if the type is vector-returnable.
	-- Reason: if not, then it costs extra to label the
	-- alternatives, because we'd get return code like:
	--
	--	switch TagReg { 0 : JMP(alt_1); 1 : JMP(alt_2) ..etc }
	--
	-- which is worse than having the alt code in the switch statement

	; (alts, mb_deflt) <- cgAlgAlts GCMayHappen cc_slot alt_type alts

	; (lbl, branches) <- emitAlgReturnTarget (idName bndr) 
				alts mb_deflt srt ret_conv

	; returnFC (CaseAlts lbl branches bndr False) }
  where
    ret_conv = case alt_type of
    		AlgAlt tc -> ctrlReturnConvAlg tc
    		PolyAlt   -> UnvectoredReturn 0
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
       -> Maybe VirtualSpOffset
       -> AltType				--  ** AlgAlt or PolyAlt only **
       -> [StgAlt]				-- The alternatives
       -> FCode ( [(ConTagZ, CgStmts)], -- The branches
		  Maybe CgStmts )	-- The default case

cgAlgAlts gc_flag cc_slot alt_type alts
  = do alts <- forkAlts [ cgAlgAlt gc_flag cc_slot alt_type alt | alt <- alts]
       let
	    mb_deflt = case alts of -- DEFAULT is always first, if present
			 ((DEFAULT,blks) : _) -> Just blks
			 other		      -> Nothing

	    branches = [(dataConTagZ con, blks) 
	   	       | (DataAlt con, blks) <- alts]
       -- in
       return (branches, mb_deflt)


cgAlgAlt :: GCFlag
      	 -> Maybe VirtualSpOffset	-- Turgid state
      	 -> AltType			--  ** AlgAlt or PolyAlt only **
      	 -> StgAlt
      	 -> FCode (AltCon, CgStmts)

cgAlgAlt gc_flag cc_slot alt_type (con, args, use_mask, rhs)
  = do	{ abs_c <- getCgStmts $ do
		{ bind_con_args con args
		; restoreCurrentCostCentre cc_slot True
		; maybeAltHeapCheck gc_flag alt_type (cgExpr rhs) }
	; return (con, abs_c) }
  where
    bind_con_args DEFAULT      args = nopC
    bind_con_args (DataAlt dc) args = bindConArgs dc args
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
	   -> AltType	-- Always PrimAlt, but passed to maybeAltHeapCheck
	   -> CmmReg	-- Scrutinee
	   -> [StgAlt]	-- Alternatives
	   -> Code
-- NB: cgPrimAlts emits code that does the case analysis.
-- It's often used in inline situations, rather than to genearte
-- a labelled return point.  That's why its interface is a little
-- different to cgAlgAlts
--
-- INVARIANT: the default binder is already bound
cgPrimAlts gc_flag alt_type scrutinee alts
  = do	{ tagged_absCs <- forkAlts (map (cgPrimAlt gc_flag alt_type) alts)
	; let ((DEFAULT, deflt_absC) : others) = tagged_absCs	-- There is always a default
	      alt_absCs = [(lit,rhs) | (LitAlt lit, rhs) <- others]
 	; emitLitSwitch (CmmReg scrutinee) alt_absCs deflt_absC }

cgPrimAlt :: GCFlag
	  -> AltType
	  -> StgAlt				-- The alternative
	  -> FCode (AltCon, CgStmts)	-- Its compiled form

cgPrimAlt gc_flag alt_type (con, [], [], rhs)
  = ASSERT( case con of { DEFAULT -> True; LitAlt _ -> True; other -> False } )
    do	{ abs_c <- getCgStmts (maybeAltHeapCheck gc_flag alt_type (cgExpr rhs)) 
	; returnFC (con, abs_c) }
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
maybeAltHeapCheck GCMayHappen alt_type code = altHeapCheck alt_type code

saveVolatileVarsAndRegs
    :: StgLiveVars                    -- Vars which should be made safe
    -> FCode (CmmStmts,  	      -- Assignments to do the saves
	      EndOfBlockInfo,	      -- sequel for the alts
              Maybe VirtualSpOffset)  -- Slot for current cost centre

saveVolatileVarsAndRegs vars
  = do	{ var_saves <- saveVolatileVars vars
	; (maybe_cc_slot, cc_save) <- saveCurrentCostCentre
	; eob_info <- getEndOfBlockInfo
	; returnFC (var_saves `plusStmts` cc_save,
		    eob_info,
		    maybe_cc_slot) }


saveVolatileVars :: StgLiveVars		-- Vars which should be made safe
		 -> FCode CmmStmts	-- Assignments to to the saves

saveVolatileVars vars
  = do	{ stmts_s <- mapFCs save_it (varSetElems vars)
	; return (foldr plusStmts noStmts stmts_s) }
  where
    save_it var
      = do { v <- getCAddrModeIfVolatile var
	   ; case v of
		Nothing	        -> return noStmts 	   -- Non-volatile
		Just vol_amode  -> save_var var vol_amode  -- Aha! It's volatile
	}

    save_var var vol_amode
      = do { slot <- allocPrimStack (idCgRep var)
	   ; rebindToStack var slot
	   ; sp_rel <- getSpRelOffset slot
	   ; returnFC (oneStmt (CmmStore sp_rel vol_amode)) }
\end{code}

---------------------------------------------------------------------------

When we save the current cost centre (which is done for lexical
scoping), we allocate a free stack location, and return (a)~the
virtual offset of the location, to pass on to the alternatives, and
(b)~the assignment to do the save (just as for @saveVolatileVars@).

\begin{code}
saveCurrentCostCentre ::
	FCode (Maybe VirtualSpOffset,	-- Where we decide to store it
	       CmmStmts)		-- Assignment to save it

saveCurrentCostCentre
  | not opt_SccProfilingOn 
  = returnFC (Nothing, noStmts)
  | otherwise
  = do	{ slot <- allocPrimStack PtrArg
	; sp_rel <- getSpRelOffset slot
	; returnFC (Just slot,
		    oneStmt (CmmStore sp_rel curCCS)) }

-- Sometimes we don't free the slot containing the cost centre after restoring it
-- (see CgLetNoEscape.cgLetNoEscapeBody).
restoreCurrentCostCentre :: Maybe VirtualSpOffset -> Bool -> Code
restoreCurrentCostCentre Nothing     _freeit = nopC
restoreCurrentCostCentre (Just slot) freeit
 = do 	{ sp_rel <- getSpRelOffset slot
	; whenC freeit (freeStackSlots [slot])
	; stmtC (CmmStore curCCSAddr (CmmLoad sp_rel wordRep)) }
\end{code}

