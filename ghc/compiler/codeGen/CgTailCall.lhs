%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgTailCall.lhs,v 1.22 1999/06/22 08:00:00 simonpj Exp $
%
%********************************************************
%*							*
\section[CgTailCall]{Tail calls: converting @StgApps@}
%*							*
%********************************************************

\begin{code}
module CgTailCall (
	cgTailCall,
	performReturn, performPrimReturn,
	mkStaticAlgReturnCode, mkDynamicAlgReturnCode,
	mkUnboxedTupleReturnCode, returnUnboxedTuple,
	mkPrimReturnCode,

	tailCallFun,
	tailCallPrimOp,
	doTailCall,

	pushReturnAddress
    ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn
import PprAbsC		( pprAmode )

import AbsCUtils	( mkAbstractCs, mkAbsCStmts, getAmodeRep )
import CgBindery	( getArgAmodes, getCAddrMode, getCAddrModeAndInfo )
import CgRetConv	( dataReturnConvPrim,
			  ctrlReturnConvAlg, CtrlReturnConvention(..),
			  assignAllRegs, assignRegs
			)
import CgStackery	( mkTaggedStkAmodes, adjustStackHW )
import CgUsages		( getSpRelOffset, adjustSpAndHp )
import CgUpdate		( pushSeqFrame )
import CLabel		( mkUpdInfoLabel, mkRtsPrimOpLabel )
import ClosureInfo	( nodeMustPointToIt,
			  getEntryConvention, EntryConvention(..),
			  LambdaFormInfo
			)
import CmdLineOpts	( opt_DoSemiTagging )
import Id		( Id, idType, idName )
import DataCon		( DataCon, dataConTyCon, dataConTag, fIRST_TAG )
import Const		( mkMachInt )
import Maybes		( assocMaybe, maybeToBool )
import PrimRep		( PrimRep(..) )
import StgSyn		( StgArg, GenStgArg(..) )
import Type		( isUnLiftedType )
import TyCon            ( TyCon )
import PrimOp		( PrimOp )
import Util		( zipWithEqual )
import Outputable
import Panic		( panic, assertPanic )
\end{code}

%************************************************************************
%*									*
\subsection[tailcall-doc]{Documentation}
%*									*
%************************************************************************

\begin{code}
cgTailCall :: Id -> [StgArg] -> Code
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

Treat unboxed locals exactly like literals (above) except use the addr
mode for the local instead of (CLit lit) in the assignment.

Case for unboxed @Ids@ first:
\begin{code}
cgTailCall fun []
  | isUnLiftedType (idType fun)
  = getCAddrMode fun		`thenFC` \ amode ->
    performPrimReturn (ppr fun) amode
\end{code}

The general case (@fun@ is boxed):
\begin{code}
cgTailCall fun args = performTailCall fun args
\end{code}

%************************************************************************
%*									*
\subsection[return-and-tail-call]{Return and tail call}
%*									*
%************************************************************************

\begin{code}
performPrimReturn :: SDoc	-- Just for debugging (sigh)
		  -> CAddrMode	-- The thing to return
		  -> Code

performPrimReturn doc amode
  = let
	kind = getAmodeRep amode
	ret_reg = WARN( case kind of { PtrRep -> True; other -> False }, text "primRet" <+> doc <+> pprAmode amode )
		  dataReturnConvPrim kind

	assign_possibly = case kind of
	  VoidRep -> AbsCNop
	  kind -> (CAssign (CReg ret_reg) amode)
    in
    performReturn assign_possibly (mkPrimReturnCode doc)

mkPrimReturnCode :: SDoc 		-- Debugging only
		 -> Sequel
		 -> Code
mkPrimReturnCode doc UpdateCode	= pprPanic "mkPrimReturnCode: Upd" doc
mkPrimReturnCode doc sequel	= sequelToAmode sequel	`thenFC` \ dest_amode ->
				  absC (CReturn dest_amode DirectReturn)
				  -- Direct, no vectoring

-- Constructor is built on the heap; Node is set.
-- All that remains is
--	(a) to set TagReg, if necessary
--	(c) to do the right sort of jump.

mkStaticAlgReturnCode :: DataCon	-- The constructor
		      -> Sequel		-- where to return to
		      -> Code

mkStaticAlgReturnCode con sequel
  =	-- Generate profiling code if necessary
    (case return_convention of
	VectoredReturn sz -> profCtrC SLIT("TICK_VEC_RETURN") [mkIntCLit sz]
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
	UpdateCode ->	-- Ha!  We can go direct to the update code,
			-- (making sure to jump to the *correct* update
			--  code.)
    			absC (CReturn (CLbl mkUpdInfoLabel CodePtrRep)
				      return_info)

	CaseAlts _ (Just (alts, _)) ->	-- Ho! We know the constructor so
					-- we can go right to the alternative

		case assocMaybe alts tag of
		   Just (alt_absC, join_lbl) -> 
			absC (CJump (CLbl join_lbl CodePtrRep))
		   Nothing -> panic "mkStaticAlgReturnCode: default"
				-- The Nothing case should never happen; 
				-- it's the subject of a wad of special-case 
				-- code in cgReturnCon

	-- can't be a SeqFrame, because we're returning a constructor

	other ->	-- OnStack, or (CaseAlts ret_amode Nothing)
		    sequelToAmode sequel	`thenFC` \ ret_amode ->
		    absC (CReturn ret_amode return_info)
    )

  where
    tag		      = dataConTag   con
    tycon	      = dataConTyCon con
    return_convention = ctrlReturnConvAlg tycon
    zero_indexed_tag  = tag - fIRST_TAG	      -- Adjust tag to be zero-indexed
					      -- cf AbsCUtils.mkAlgAltsCSwitch

    return_info = 
       case return_convention of
		UnvectoredReturn _ -> DirectReturn
		VectoredReturn _   -> StaticVectoredReturn zero_indexed_tag

mkUnboxedTupleReturnCode :: Sequel -> Code
mkUnboxedTupleReturnCode sequel
    = case sequel of
	-- can't update with an unboxed tuple!
	UpdateCode -> panic "mkUnboxedTupleReturnCode"

	CaseAlts _ (Just ([(_,(alt_absC,join_lbl))], _)) ->
			absC (CJump (CLbl join_lbl CodePtrRep))

	-- can't be a SeqFrame

	other ->	-- OnStack, or (CaseAlts ret_amode something)
		    sequelToAmode sequel	`thenFC` \ ret_amode ->
		    absC (CReturn ret_amode DirectReturn)

-- This function is used by PrimOps that return enumerated types (i.e.
-- all the comparison operators).

mkDynamicAlgReturnCode :: TyCon -> CAddrMode -> Sequel -> Code

mkDynamicAlgReturnCode tycon dyn_tag sequel
  = case ctrlReturnConvAlg tycon of
	VectoredReturn sz ->

		profCtrC SLIT("TICK_VEC_RETURN") [mkIntCLit sz] `thenC`
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
	      -> Code

-- this is just a special case of doTailCall, later.
performReturn sim_assts finish_code
  = getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->

	-- Do the simultaneous assignments,
    doSimAssts sim_assts	 	`thenC`

	-- push a return address if necessary
	-- (after the assignments above, in case we clobber a live
	--  stack location)
    pushReturnAddress eob		`thenC`

	-- Adjust Sp/Hp
    adjustSpAndHp args_sp		`thenC`

	-- Do the return
    finish_code sequel		-- "sequel" is `robust' in that it doesn't
				-- depend on stk-ptr values
\end{code}

Returning unboxed tuples.  This is mainly to support _ccall_GC_, where
we want to do things in a slightly different order to normal:

		- push return address
		- adjust stack pointer
		- r = call(args...)
		- assign regs for unboxed tuple (usually just R1 = r)
		- return to continuation

The return address (i.e. stack frame) must be on the stack before
doing the call in case the call ends up in the garbage collector.

Sadly, the information about the continuation is lost after we push it
(in order to avoid pushing it again), so we end up doing a needless
indirect jump (ToDo).

\begin{code}
returnUnboxedTuple :: [CAddrMode] -> Code -> Code
returnUnboxedTuple amodes before_jump
  = getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->

	-- push a return address if necessary
    pushReturnAddress eob		`thenC`
    setEndOfBlockInfo (EndOfBlockInfo args_sp (OnStack args_sp)) (

	-- Adjust Sp/Hp
    adjustSpAndHp args_sp		`thenC`

    before_jump				`thenC`

    let (ret_regs, leftovers) = assignRegs [] (map getAmodeRep amodes)
    in

    profCtrC SLIT("TICK_RET_UNBOXED_TUP") [mkIntCLit (length amodes)] `thenC`

    doTailCall amodes ret_regs
		mkUnboxedTupleReturnCode
		(length leftovers)  {- fast args arity -}
		AbsCNop {-no pending assigments-}
		Nothing {-not a let-no-escape-}
		False   {-node doesn't point-}
     )
\end{code}

\begin{code}
performTailCall :: Id		-- Function
		-> [StgArg]	-- Args
		-> Code

performTailCall fun args
  =	-- Get all the info we have about the function and args and go on to
	-- the business end
    getCAddrModeAndInfo fun	`thenFC` \ (fun_amode, lf_info) ->
    getArgAmodes args		`thenFC` \ arg_amodes ->

    tailCallFun
		fun fun_amode lf_info arg_amodes
		AbsCNop {- No pending assignments -}


-- generating code for a tail call to a function (or closure)

tailCallFun :: Id -> CAddrMode	-- Function and its amode
		 -> LambdaFormInfo	-- Info about the function
		 -> [CAddrMode]		-- Arguments

		 -> AbstractC		-- Pending simultaneous assignments
					-- *** GUARANTEED to contain only stack 
					-- assignments.

					-- In ptic, we don't need to look in 
					-- here to discover all live regs

		 -> Code

tailCallFun fun fun_amode lf_info arg_amodes pending_assts
  = nodeMustPointToIt lf_info			`thenFC` \ node_points ->
    getEntryConvention (idName fun) lf_info
	(map getAmodeRep arg_amodes)		`thenFC` \ entry_conv ->
    let
	node_asst
	  = if node_points then
		CAssign (CReg node) fun_amode
	    else
		AbsCNop

	(arg_regs, finish_code, arity)
	  = case entry_conv of
	      ViaNode ->
		([],
		     profCtrC SLIT("TICK_ENT_VIA_NODE") [] `thenC`
		     absC (CJump (CMacroExpr CodePtrRep ENTRY_CODE 
			        [CVal (nodeRel 0) DataPtrRep]))
		     , 0)
	      StdEntry lbl -> ([], absC (CJump (CLbl lbl CodePtrRep)), 0)
	      DirectEntry lbl arity regs  ->
		(regs,	 absC (CJump (CLbl lbl CodePtrRep)), 
	         arity - length regs)

	-- set up for a let-no-escape if necessary
	join_sp = case fun_amode of
			CJoinPoint sp -> Just sp
			other         -> Nothing
    in
    doTailCall arg_amodes arg_regs (const finish_code) arity
		(mkAbstractCs [node_asst,pending_assts]) join_sp node_points


-- this generic tail call code is used for both function calls and returns.

doTailCall 
	:: [CAddrMode] 			-- args to pass to function
	-> [MagicId]			-- registers to use
	-> (Sequel->Code)		-- code to perform jump
	-> Int				-- number of "fast" stack arguments
	-> AbstractC			-- pending assignments
	-> Maybe VirtualSpOffset	-- sp offset to trim stack to: 
					-- USED iff destination is a let-no-escape
	-> Bool				-- node points to the closure to enter
	-> Code

doTailCall arg_amodes arg_regs finish_code arity pending_assts
		maybe_join_sp node_points
  = getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->

    let
	no_of_args = length arg_amodes

	(reg_arg_amodes, stk_arg_amodes) = splitAt (length arg_regs) arg_amodes
	    -- We get some stk_arg_amodes if (a) no regs, or 
	    --				     (b) args beyond arity

	reg_arg_assts
	  = mkAbstractCs (zipWithEqual "assign_to_reg2" 
				assign_to_reg arg_regs reg_arg_amodes)

	assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

	join_sp = case maybe_join_sp of
			Just sp -> ASSERT(not (args_sp > sp)) sp
	      -- If ASSERTion fails: Oops: the join point has *lower*
	      -- stack ptrs than the continuation Note that we take
	      -- the Sp point without the return address here.	 The
	      -- return address is put on by the let-no-escapey thing
	      -- when it finishes.
			Nothing -> args_sp

	(fast_stk_amodes, tagged_stk_amodes) = 
		splitAt arity stk_arg_amodes
    in
	-- We can omit tags on the arguments passed to the fast entry point, 
	-- but we have to be careful to fill in the tags on any *extra*
	-- arguments we're about to push on the stack.

	mkTaggedStkAmodes join_sp tagged_stk_amodes `thenFC`
			    \ (fast_sp, tagged_arg_assts, tag_assts) ->

	mkTaggedStkAmodes fast_sp fast_stk_amodes `thenFC`
			    \ (final_sp, fast_arg_assts, _) ->

	-- adjust the high-water mark if necessary
	adjustStackHW final_sp	`thenC`

		-- The stack space for the pushed return addess, 
		-- with any args pushed on top, is recorded in final_sp.
	
		-- Do the simultaneous assignments,
	doSimAssts (mkAbstractCs [pending_assts,
			          reg_arg_assts, 
				  fast_arg_assts, 
				  tagged_arg_assts,
			          tag_assts])	`thenC`
	
		-- push a return address if necessary
		-- (after the assignments above, in case we clobber a live
		--  stack location)

		-- DONT push the return address when we're about
		-- to jump to a let-no-escape: the final tail call
		-- in the let-no-escape will do this.
	(if (maybeToBool maybe_join_sp)
		then nopC
		else pushReturnAddress eob)		`thenC`

		-- Final adjustment of Sp/Hp
	adjustSpAndHp final_sp		`thenC`
	
		-- Now decide about semi-tagging
	let
		semi_tagging_on = opt_DoSemiTagging
	in
	case (semi_tagging_on, arg_amodes, node_points, sequel) of

	--
	-- *************** The semi-tagging case ***************
	--
	{- XXX leave this out for now.
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

		    semi_tagged_alts = [ (mkMachInt (fromInt (tag - fIRST_TAG)),
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
		-}

	--
	-- *************** The non-semi-tagging case ***************
	--
	      other -> finish_code sequel
\end{code}

%************************************************************************
%*									*
\subsection[tailCallPrimOp]{@tailCallPrimOp@}
%*									*
%************************************************************************

\begin{code}
tailCallPrimOp :: PrimOp -> [StgArg] -> Code
tailCallPrimOp op args =
    -- we're going to perform a normal-looking tail call, 
    -- except that *all* the arguments will be in registers.
    getArgAmodes args		`thenFC` \ arg_amodes ->
    let (arg_regs, leftovers) = assignAllRegs [] (map getAmodeRep arg_amodes)
    in
    ASSERT(null leftovers) -- no stack-resident args
    doTailCall arg_amodes arg_regs 
	(const (absC (CJump (CLbl (mkRtsPrimOpLabel op) CodePtrRep))))
	0       {- arity shouldn't matter, all args in regs -}
	AbsCNop {- no pending assignments -}
	Nothing {- not a let-no-escape -}
	False   {- node doesn't point -}
\end{code}

%************************************************************************
%*									*
\subsection[doSimAssts]{@doSimAssts@}
%*									*
%************************************************************************

@doSimAssts@ happens at the end of every block of code.
They are separate because we sometimes do some jiggery-pokery in between.

\begin{code}
doSimAssts :: AbstractC -> Code

doSimAssts sim_assts
  = absC (CSimultaneous sim_assts)
\end{code}

%************************************************************************
%*									*
\subsection[retAddr]{@Return Addresses@}
%*									*
%************************************************************************

We always push the return address just before performing a tail call
or return.  The reason we leave it until then is because the stack
slot that the return address is to go into might contain something
useful.

If the end of block info is CaseAlts, then we're in the scrutinee of a
case expression and the return address is still to be pushed.

There are cases where it doesn't look necessary to push the return
address: for example, just before doing a return to a known
continuation.  However, the continuation will expect to find the
return address on the stack in case it needs to do a heap check.

\begin{code}
pushReturnAddress :: EndOfBlockInfo -> Code
pushReturnAddress (EndOfBlockInfo args_sp sequel@(CaseAlts amode _)) =
    getSpRelOffset args_sp			 `thenFC` \ sp_rel ->
    absC (CAssign (CVal sp_rel RetRep) amode)
pushReturnAddress (EndOfBlockInfo args_sp sequel@(SeqFrame amode _)) =
    pushSeqFrame args_sp			 `thenFC` \ ret_sp ->
    getSpRelOffset ret_sp			 `thenFC` \ sp_rel ->
    absC (CAssign (CVal sp_rel RetRep) amode)
pushReturnAddress _ = nopC
\end{code}
