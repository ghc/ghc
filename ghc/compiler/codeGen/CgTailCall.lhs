%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
%********************************************************
%*							*
\section[CgTailCall]{Tail calls: converting @StgApps@}
%*							*
%********************************************************

\begin{code}
#include "HsVersions.h"

module CgTailCall (
	cgTailCall,
	performReturn,
	mkStaticAlgReturnCode, mkDynamicAlgReturnCode,
	mkPrimReturnCode,

	tailCallBusiness
    ) where

IMP_Ubiq(){-uitous-}

import CgMonad
import AbsCSyn

import AbsCUtils	( mkAbstractCs, mkAbsCStmts, getAmodeRep )
import CgBindery	( getArgAmodes, getCAddrMode, getCAddrModeAndInfo )
import CgRetConv	( dataReturnConvPrim, dataReturnConvAlg,
			  ctrlReturnConvAlg, CtrlReturnConvention(..),
			  DataReturnConvention(..)
			)
import CgStackery	( adjustRealSps, mkStkAmodes )
import CgUsages		( getSpARelOffset )
import CLabel		( mkStdUpdCodePtrVecLabel, mkConUpdCodePtrVecLabel, CLabel )
import ClosureInfo	( nodeMustPointToIt,
			  getEntryConvention, EntryConvention(..),
			  LambdaFormInfo
			)
import CmdLineOpts	( opt_DoSemiTagging )
import HeapOffs		( zeroOff, SYN_IE(VirtualSpAOffset) )
import Id		( idType, dataConTyCon, dataConTag,
			  fIRST_TAG, SYN_IE(Id)
			)
import Literal		( mkMachInt )
import Maybes		( assocMaybe )
import PrimRep		( PrimRep(..) )
import StgSyn		( SYN_IE(StgArg), GenStgArg(..), SYN_IE(StgLiveVars) )
import Type		( isPrimType )
import TyCon            ( TyCon )
import Util		( zipWithEqual, panic, assertPanic )
\end{code}

%************************************************************************
%*									*
\subsection[tailcall-doc]{Documentation}
%*									*
%************************************************************************

\begin{code}
cgTailCall :: StgArg -> [StgArg] -> StgLiveVars -> Code
\end{code}

Here's the code we generate for a tail call.  (NB there may be no
arguments, in which case this boils down to just entering a variable.)

\begin{itemize}
\item	Adjust the stack ptr to \tr{tailSp + #args}.
\item	Put args in the top locations of the resulting stack.
\item	Make Node point to the function closure.
\item	Enter the function closure.
\end{itemize}

Things to be careful about:
\begin{itemize}
\item	Don't overwrite stack locations before you have finished with
	them (remember you need the function and the as-yet-unmoved
	arguments).
\item	Preferably, generate no code to replace x by x on the stack (a
	common situation in tail-recursion).
\item	Adjust the stack high water mark appropriately.
\end{itemize}

\begin{code}
cgTailCall (StgConArg con) args live_vars
  = panic "cgTailCall StgConArg"	-- Only occur in argument positions
\end{code}

Literals are similar to constructors; they return by putting
themselves in an appropriate register and returning to the address on
top of the B stack.

\begin{code}
cgTailCall (StgLitArg lit) [] live_vars
  = performPrimReturn (CLit lit) live_vars
\end{code}

Treat unboxed locals exactly like literals (above) except use the addr
mode for the local instead of (CLit lit) in the assignment.

Case for unboxed @Ids@ first:
\begin{code}
cgTailCall atom@(StgVarArg fun) [] live_vars
  | isPrimType (idType fun)
  = getCAddrMode fun `thenFC` \ amode ->
    performPrimReturn amode live_vars
\end{code}

The general case (@fun@ is boxed):
\begin{code}
cgTailCall (StgVarArg fun) args live_vars = performTailCall fun args live_vars
\end{code}

%************************************************************************
%*									*
\subsection[return-and-tail-call]{Return and tail call}
%*									*
%************************************************************************

ADR-HACK

  A quick bit of hacking to try to solve my void#-leaking blues...

  I think I'm getting bitten by this stuff because code like

  \begin{pseudocode}
	  case ds.s12 :: IoWorld of {
	      -- lvs: [ds.s12]; rhs lvs: []; uniq: c0
	    IoWorld ds.s13# -> ds.s13#;
	  } :: Universe#
  \end{pseudocode}

  causes me to try to allocate a register to return the result in.  The
  hope is that the following will avoid such problems (and that Will
  will do this in a cleaner way when he hits the same problem).

KCAH-RDA

\begin{code}
performPrimReturn :: CAddrMode	-- The thing to return
		  -> StgLiveVars
		  -> Code

performPrimReturn amode live_vars
  = let
	kind = getAmodeRep amode
	ret_reg = dataReturnConvPrim kind

	assign_possibly = case kind of
	  VoidRep -> AbsCNop
	  kind -> (CAssign (CReg ret_reg) amode)
    in
    performReturn assign_possibly mkPrimReturnCode live_vars

mkPrimReturnCode :: Sequel -> Code
mkPrimReturnCode (UpdateCode _)	= panic "mkPrimReturnCode: Upd"
mkPrimReturnCode sequel		= sequelToAmode sequel	`thenFC` \ dest_amode ->
				  absC (CReturn dest_amode DirectReturn)
				  -- Direct, no vectoring

-- All constructor arguments in registers; Node and InfoPtr are set.
-- All that remains is
--	(a) to set TagReg, if necessary
--	(b) to set InfoPtr to the info ptr, if necessary
--	(c) to do the right sort of jump.

mkStaticAlgReturnCode :: Id		-- The constructor
		      -> Maybe CLabel	-- The info ptr, if it isn't already set
		      -> Sequel		-- where to return to
		      -> Code

mkStaticAlgReturnCode con maybe_info_lbl sequel
  =	-- Generate profiling code if necessary
    (case return_convention of
	VectoredReturn sz -> profCtrC SLIT("VEC_RETURN") [mkIntCLit sz]
	other		  -> nopC
    )					`thenC`

	-- Set tag if necessary
	-- This is done by a macro, because if we are short of registers
	-- we don't set TagReg; instead the continuation gets the tag
	-- by indexing off the info ptr
    (case return_convention of

	UnvectoredReturn no_of_constrs
	 | no_of_constrs > 1
		-> absC (CMacroStmt SET_TAG [mkIntCLit zero_indexed_tag])

	other	-> nopC
    )					`thenC`

	-- Generate the right jump or return
    (case sequel of
	UpdateCode _ ->	-- Ha!	We know the constructor,
			-- so we can go direct to the correct
			-- update code for that constructor

				-- Set the info pointer, and jump
			set_info_ptr		`thenC`
    			absC (CJump (CLbl update_label CodePtrRep))

	CaseAlts _ (Just (alts, _)) ->	-- Ho! We know the constructor so
					-- we can go right to the alternative

			-- No need to set info ptr when returning to a
			-- known join point. After all, the code at
			-- the destination knows what constructor it
			-- is going to handle.

			case assocMaybe alts tag of
			   Just (alt_absC, join_lbl) -> absC (CJump (CLbl join_lbl CodePtrRep))
			   Nothing		     -> panic "mkStaticAlgReturnCode: default"
				-- The Nothing case should never happen; it's the subject
				-- of a wad of special-case code in cgReturnCon

	other ->	-- OnStack, or (CaseAlts) ret_amode Nothing)
			-- Set the info pointer, and jump
		    set_info_ptr		`thenC`
		    sequelToAmode sequel	`thenFC` \ ret_amode ->
		    absC (CReturn ret_amode return_info)
    )

  where
    tag		      = dataConTag   con
    tycon	      = dataConTyCon con
    return_convention = ctrlReturnConvAlg tycon
    zero_indexed_tag  = tag - fIRST_TAG	      -- Adjust tag to be zero-indexed
					      -- cf AbsCUtils.mkAlgAltsCSwitch

    update_label
      = case (dataReturnConvAlg con) of
	  ReturnInHeap   -> mkStdUpdCodePtrVecLabel tycon tag
	  ReturnInRegs _ -> mkConUpdCodePtrVecLabel tycon tag

    return_info = case return_convention of
			UnvectoredReturn _ -> DirectReturn
			VectoredReturn _   -> StaticVectoredReturn zero_indexed_tag

    set_info_ptr = case maybe_info_lbl of
			Nothing	      -> nopC
			Just info_lbl -> absC (CAssign (CReg infoptr) (CLbl info_lbl DataPtrRep))


mkDynamicAlgReturnCode :: TyCon -> CAddrMode -> Sequel -> Code

mkDynamicAlgReturnCode tycon dyn_tag sequel
  = case ctrlReturnConvAlg tycon of
	VectoredReturn sz ->

		profCtrC SLIT("VEC_RETURN") [mkIntCLit sz] `thenC`
		sequelToAmode sequel		`thenFC` \ ret_addr ->
		absC (CReturn ret_addr (DynamicVectoredReturn dyn_tag))

	UnvectoredReturn no_of_constrs ->

		-- Set tag if necessary
		-- This is done by a macro, because if we are short of registers
		-- we don't set TagReg; instead the continuation gets the tag
		-- by indexing off the info ptr
		(if no_of_constrs > 1 then
			absC (CMacroStmt SET_TAG [dyn_tag])
		else
			nopC
		)			`thenC`


		sequelToAmode sequel		`thenFC` \ ret_addr ->
		-- Generate the right jump or return
		absC (CReturn ret_addr DirectReturn)
\end{code}

\begin{code}
performReturn :: AbstractC	    -- Simultaneous assignments to perform
	      -> (Sequel -> Code)   -- The code to execute to actually do
				    -- the return, given an addressing mode
				    -- for the return address
	      -> StgLiveVars
	      -> Code

performReturn sim_assts finish_code live_vars
  = getEndOfBlockInfo	`thenFC` \ (EndOfBlockInfo args_spa args_spb sequel) ->

	-- Do the simultaneous assignments,
    doSimAssts args_spa live_vars sim_assts	`thenC`

	-- Adjust stack pointers
    adjustRealSps args_spa args_spb	`thenC`

	-- Do the return
    finish_code sequel		-- "sequel" is `robust' in that it doesn't
				-- depend on stk-ptr values
\end{code}

\begin{code}
performTailCall :: Id			-- Function
		-> [StgArg]	-- Args
		-> StgLiveVars
		-> Code

performTailCall fun args live_vars
  =	-- Get all the info we have about the function and args and go on to
	-- the business end
    getCAddrModeAndInfo fun	`thenFC` \ (fun_amode, lf_info) ->
    getArgAmodes args		`thenFC` \ arg_amodes ->

    tailCallBusiness
		fun fun_amode lf_info arg_amodes
		live_vars AbsCNop {- No pending assignments -}


tailCallBusiness :: Id -> CAddrMode	-- Function and its amode
		 -> LambdaFormInfo	-- Info about the function
		 -> [CAddrMode]		-- Arguments
		 -> StgLiveVars	-- Live in continuation

		 -> AbstractC		-- Pending simultaneous assignments
					-- *** GUARANTEED to contain only stack assignments.
					--     In ptic, we don't need to look in here to
					--     discover all live regs

		 -> Code

tailCallBusiness fun fun_amode lf_info arg_amodes live_vars pending_assts
  = nodeMustPointToIt lf_info			`thenFC` \ node_points ->
    getEntryConvention fun lf_info
	(map getAmodeRep arg_amodes)		`thenFC` \ entry_conv ->

    getEndOfBlockInfo	`thenFC` \ (EndOfBlockInfo args_spa args_spb sequel) ->

    let
	node_asst
	  = if node_points then
		CAssign (CReg node) fun_amode
	    else
		AbsCNop

	(arg_regs, finish_code)
	  = case entry_conv of
	      ViaNode			  ->
		([],
		     mkAbstractCs [
			CCallProfCtrMacro SLIT("ENT_VIA_NODE") [],
			CJump (CMacroExpr CodePtrRep ENTRY_CODE [(CMacroExpr DataPtrRep INFO_PTR [CReg node])])
		     ])
	      StdEntry lbl Nothing	  -> ([], CJump (CLbl lbl CodePtrRep))
	      StdEntry lbl (Just itbl)	  -> ([], CAssign (CReg infoptr) (CLbl itbl DataPtrRep)
						     `mkAbsCStmts`
						  CJump (CLbl lbl CodePtrRep))
	      DirectEntry lbl arity regs  ->
		(regs,	 CJump (CLbl lbl CodePtrRep))

	no_of_args = length arg_amodes

	(reg_arg_amodes, stk_arg_amodes) = splitAt (length arg_regs) arg_amodes
	    -- We get some stk_arg_amodes if (a) no regs, or (b) args beyond arity

	reg_arg_assts
	  = mkAbstractCs (zipWithEqual "assign_to_reg2" assign_to_reg arg_regs reg_arg_amodes)

	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode
    in
    case fun_amode of
      CJoinPoint join_spa join_spb ->  -- Ha!  A let-no-escape thingy

	  ASSERT(not (args_spa > join_spa) || (args_spb > join_spb))
	      -- If ASSERTion fails: Oops: the join point has *lower*
	      -- stack ptrs than the continuation Note that we take
	      -- the SpB point without the return address here.	 The
	      -- return address is put on by the let-no-escapey thing
	      -- when it finishes.

	  mkStkAmodes join_spa join_spb stk_arg_amodes
		      `thenFC` \ (final_spa, final_spb, stk_arg_assts) ->

		-- Do the simultaneous assignments,
	  doSimAssts join_spa live_vars
		(mkAbstractCs [pending_assts, reg_arg_assts, stk_arg_assts])
			`thenC`

		-- Adjust stack ptrs
	  adjustRealSps final_spa final_spb	`thenC`

		-- Jump to join point
	  absC finish_code

      _ -> -- else: not a let-no-escape (the common case)

		-- Make instruction to save return address
	    loadRetAddrIntoRetReg sequel	`thenFC` \ ret_asst ->

	    mkStkAmodes args_spa args_spb stk_arg_amodes
						`thenFC`
			    \ (final_spa, final_spb, stk_arg_assts) ->

		-- The B-stack space for the pushed return addess, with any args pushed
		-- on top, is recorded in final_spb.

		-- Do the simultaneous assignments,
	    doSimAssts args_spa live_vars
		(mkAbstractCs [pending_assts, node_asst, ret_asst,
			       reg_arg_assts, stk_arg_assts])
						`thenC`

		-- Final adjustment of stack pointers
	    adjustRealSps final_spa final_spb	`thenC`

		-- Now decide about semi-tagging
	    let
		semi_tagging_on = opt_DoSemiTagging
	    in
	    case (semi_tagging_on, arg_amodes, node_points, sequel) of

	--
	-- *************** The semi-tagging case ***************
	--
	      (	  True,		   [],		True,	     CaseAlts _ (Just (st_alts, maybe_deflt_join_details))) ->

		-- Whoppee!  Semi-tagging rules OK!
		-- (a) semi-tagging is switched on
		-- (b) there are no arguments,
		-- (c) Node points to the closure
		-- (d) we have a case-alternative sequel with
		--	some visible alternatives

		-- Why is test (c) necessary?
		-- Usually Node will point to it at this point, because we're
		-- scrutinsing something which is either a thunk or a
		-- constructor.
		-- But not always!  The example I came across is when we have
		-- a top-level Double:
		--	lit.3 = D# 3.000
		--	... (case lit.3 of ...) ...
		-- Here, lit.3 is built as a re-entrant thing, which you must enter.
		-- (OK, the simplifier should have eliminated this, but it's
		--  easy to deal with the case anyway.)
		let
		    join_details_to_code (load_regs_and_profiling_code, join_lbl)
			= load_regs_and_profiling_code		`mkAbsCStmts`
			  CJump (CLbl join_lbl CodePtrRep)

		    semi_tagged_alts = [ (mkMachInt (toInteger (tag - fIRST_TAG)),
					  join_details_to_code join_details)
				       | (tag, join_details) <- st_alts
				       ]

		    enter_jump
		      -- Enter Node (we know infoptr will have the info ptr in it)!
		      = mkAbstractCs [
			CCallProfCtrMacro SLIT("RET_SEMI_FAILED")
					[CMacroExpr IntRep INFO_TAG [CReg infoptr]],
			CJump (CMacroExpr CodePtrRep ENTRY_CODE [CReg infoptr]) ]
		in
			-- Final switch
		absC (mkAbstractCs [
			    CAssign (CReg infoptr)
				    (CVal (NodeRel zeroOff) DataPtrRep),

			    case maybe_deflt_join_details of
				Nothing ->
				    CSwitch (CMacroExpr IntRep INFO_TAG [CReg infoptr])
					(semi_tagged_alts)
					(enter_jump)
				Just (_, details) ->
				    CSwitch (CMacroExpr IntRep EVAL_TAG [CReg infoptr])
				     [(mkMachInt 0, enter_jump)]
				     (CSwitch
					 (CMacroExpr IntRep INFO_TAG [CReg infoptr])
					 (semi_tagged_alts)
					 (join_details_to_code details))
		])

	--
	-- *************** The non-semi-tagging case ***************
	--
	      other -> absC finish_code
\end{code}

\begin{code}
loadRetAddrIntoRetReg :: Sequel -> FCode AbstractC

loadRetAddrIntoRetReg InRetReg
  = returnFC AbsCNop  -- Return address already there

loadRetAddrIntoRetReg sequel
  = sequelToAmode sequel      `thenFC` \ amode ->
    returnFC (CAssign (CReg RetReg) amode)

\end{code}

%************************************************************************
%*									*
\subsection[doSimAssts]{@doSimAssts@}
%*									*
%************************************************************************

@doSimAssts@ happens at the end of every block of code.
They are separate because we sometimes do some jiggery-pokery in between.

\begin{code}
doSimAssts :: VirtualSpAOffset	-- tail_spa: SpA as seen by continuation
	   -> StgLiveVars	-- Live in continuation
	   -> AbstractC
	   -> Code

doSimAssts tail_spa live_vars sim_assts
  =	-- Do the simultaneous assignments
    absC (CSimultaneous sim_assts)	`thenC`

	-- Stub any unstubbed slots; the only live variables are indicated in
	-- the end-of-block info in the monad
    nukeDeadBindings live_vars		`thenC`
    getUnstubbedAStackSlots tail_spa	`thenFC` \ a_slots ->
	-- Passing in tail_spa here should actually be redundant, because
	-- the stack should be trimmed (by nukeDeadBindings) to
	-- exactly the tail_spa position anyhow.

	-- Emit code to stub dead regs; this only generates actual
	-- machine instructions in in the DEBUG version
	-- *** NOT DONE YET ***

    (if (null a_slots)
     then nopC
     else profCtrC SLIT("A_STK_STUB") [mkIntCLit (length a_slots)]	`thenC`
	  mapCs stub_A_slot a_slots
    )
  where
    stub_A_slot :: VirtualSpAOffset -> Code
    stub_A_slot offset = getSpARelOffset offset		`thenFC` \ spa_rel ->
			 absC (CAssign	(CVal spa_rel PtrRep)
					(CReg StkStubReg))
\end{code}
