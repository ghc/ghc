%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgTailCall.lhs,v 1.38 2003/06/02 13:27:34 simonpj Exp $
%
%********************************************************
%*							*
\section[CgTailCall]{Tail calls: converting @StgApps@}
%*							*
%********************************************************

\begin{code}
module CgTailCall (
	cgTailCall, performTailCall,
	performReturn, performPrimReturn,
	mkStaticAlgReturnCode, mkDynamicAlgReturnCode,
	returnUnboxedTuple, ccallReturnUnboxedTuple,
	mkPrimReturnCode,
	tailCallPrimOp,

	pushReturnAddress
    ) where

#include "HsVersions.h"

import CgMonad
import CgBindery	( getArgAmodes, getCAddrMode, getCAddrModeAndInfo )
import CgRetConv
import CgStackery
import CgUsages		( getSpRelOffset, adjustSpAndHp )
import ClosureInfo

import AbsCUtils	( mkAbstractCs, getAmodeRep )
import AbsCSyn
import CLabel		( mkRtsPrimOpLabel, mkSeqInfoLabel )

import Id		( Id, idType, idName )
import DataCon		( DataCon, dataConTyCon, dataConTag, fIRST_TAG )
import PrimRep		( PrimRep(..) )
import StgSyn		( StgArg )
import Type		( isUnLiftedType )
import Name		( Name )
import TyCon            ( TyCon )
import PrimOp		( PrimOp )
import Util		( zipWithEqual, splitAtList )
import ListSetOps	( assocMaybe )
import PrimRep		( isFollowableRep )
import Outputable
import Panic		( panic, assertPanic )

import List		( partition )

-----------------------------------------------------------------------------
-- Tail Calls

cgTailCall :: Id -> [StgArg] -> Code

-- Here's the code we generate for a tail call.  (NB there may be no
-- arguments, in which case this boils down to just entering a variable.)
-- 
--    *	Put args in the top locations of the stack.
--    *	Adjust the stack ptr
--    *	Make R1 point to the function closure if necessary.
--    *	Perform the call.
--
-- Things to be careful about:
--
--    *	Don't overwrite stack locations before you have finished with
-- 	them (remember you need the function and the as-yet-unmoved
-- 	arguments).
--    *	Preferably, generate no code to replace x by x on the stack (a
-- 	common situation in tail-recursion).
--    *	Adjust the stack high water mark appropriately.
-- 
-- Treat unboxed locals exactly like literals (above) except use the addr
-- mode for the local instead of (CLit lit) in the assignment.

-- Case for unboxed returns first:
cgTailCall fun []
  | isUnLiftedType (idType fun)
  = getCAddrMode fun		`thenFC` \ amode ->
    performPrimReturn (ppr fun) amode

-- The general case (@fun@ is boxed):
cgTailCall fun args
  = getCAddrModeAndInfo fun		`thenFC` \ (fun', fun_amode, lf_info) ->
    getArgAmodes args			`thenFC` \ arg_amodes ->
    performTailCall fun' fun_amode lf_info arg_amodes AbsCNop


-- -----------------------------------------------------------------------------
-- The guts of a tail-call

performTailCall 
	:: Id		-- function
	-> CAddrMode	-- function amode
	-> LambdaFormInfo
	-> [CAddrMode]
	-> AbstractC	-- Pending simultaneous assignments
			-- *** GUARANTEED to contain only stack assignments.
	-> Code

performTailCall fun fun_amode lf_info arg_amodes pending_assts =
    nodeMustPointToIt lf_info		`thenFC` \ node_points ->
    let
	-- assign to node if necessary
	node_asst
	   | node_points = CAssign (CReg node) fun_amode
	   | otherwise   = AbsCNop
    in
  
    getEndOfBlockInfo `thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->	

    let
	-- set up for a let-no-escape if necessary
	join_sp = case fun_amode of
			CJoinPoint sp -> sp
			other         -> args_sp
    in

    -- decide how to code the tail-call: which registers assignments to make,
    -- what args to push on the stack, and how to make the jump
    constructTailCall (idName fun) lf_info arg_amodes join_sp
	node_points fun_amode sequel 
		`thenFC` \ (final_sp, arg_assts, jump_code) ->

    let sim_assts = mkAbstractCs [node_asst,
			      	  pending_assts,
			      	  arg_assts]

	is_lne = case fun_amode of { CJoinPoint _ -> True; _ -> False }
    in

    doFinalJump final_sp sim_assts is_lne (const jump_code)


-- Figure out how to do a particular tail-call.

constructTailCall
	:: Name
	-> LambdaFormInfo
	-> [CAddrMode]
	-> VirtualSpOffset		-- Sp at which to make the call
	-> Bool				-- node points to the fun closure?
	-> CAddrMode			-- addressing mode of the function
	-> Sequel			-- the sequel, in case we need it
	-> FCode (
		VirtualSpOffset,	-- Sp after pushing the args
		AbstractC,		-- assignments
		Code			-- code to do the jump
	   )
		
constructTailCall name lf_info arg_amodes sp node_points fun_amode sequel =

    getEntryConvention name lf_info (map getAmodeRep arg_amodes)
		`thenFC` \ entry_conv ->

    case entry_conv of
	EnterIt -> returnFC (sp, AbsCNop, code)
	  where code = profCtrC FSLIT("TICK_ENT_VIA_NODE") [] `thenC`
		       absC (CJump (CMacroExpr CodePtrRep ENTRY_CODE 
		       		[CVal (nodeRel 0) DataPtrRep]))

	-- A function, but we have zero arguments.  It is already in WHNF,
	-- so we can just return it.
	ReturnIt -> returnFC (sp, asst, code)
	  where -- if node doesn't already point to the closure, we have to
		-- load it up.
		asst | node_points = AbsCNop
		     | otherwise   = CAssign (CReg node) fun_amode

		code = sequelToAmode sequel	`thenFC` \ dest_amode ->
		       absC (CReturn dest_amode DirectReturn)

	JumpToIt lbl -> returnFC (sp, AbsCNop, code)
	  where code = absC (CJump (CLbl lbl CodePtrRep))

	-- a slow function call via the RTS apply routines
	SlowCall -> 
		let (apply_fn, new_amodes) = constructSlowCall arg_amodes

		    	-- if node doesn't already point to the closure, 
			-- we have to load it up.
		    node_asst | node_points = AbsCNop
		              | otherwise   = CAssign (CReg node) fun_amode
		in

		-- Fill in all the arguments on the stack
		mkStkAmodes sp new_amodes `thenFC` 
			\ (final_sp, stk_assts) ->

		returnFC
		  (final_sp + 1,   -- add one, because the stg_ap functions
				   -- expect there to be a free slot on the stk
		   mkAbstractCs [node_asst, stk_assts],
		   absC (CJump apply_fn)
		  )

	-- A direct function call (possibly with some left-over arguments)
	DirectEntry lbl arity regs

	   -- A let-no-escape is slightly different, because we
	   -- arrange the stack arguments into pointers and non-pointers
	   -- to make the heap check easier.  The tail-call sequence
	   -- is very similar to returning an unboxed tuple, so we
	   -- share some code.
	   | is_let_no_escape ->
	    pushUnboxedTuple sp arg_amodes   `thenFC` \ (final_sp, assts) ->
	    returnFC (final_sp, assts, absC (CJump (CLbl lbl CodePtrRep)))


	   -- A normal fast call
	   | otherwise ->
 	   let
		-- first chunk of args go in registers
		(reg_arg_amodes, stk_arg_amodes) = 
		    splitAtList regs arg_amodes

		-- the rest of this function's args go straight on the stack
		(stk_args, extra_stk_args) = 
		    splitAt (arity - length regs) stk_arg_amodes

		-- any "extra" arguments are placed in frames on the
		-- stack after the other arguments.
		slow_stk_args = slowArgs extra_stk_args

		reg_assts
	  	    = mkAbstractCs (zipWithEqual "assign_to_reg2" 
					assign_to_reg regs reg_arg_amodes)

	    in
	    mkStkAmodes sp (stk_args ++ slow_stk_args) `thenFC` 
			\ (final_sp, stk_assts) ->

	    returnFC
		(final_sp,
		 mkAbstractCs [reg_assts, stk_assts],
		 absC (CJump (CLbl lbl CodePtrRep))
		)

       where is_let_no_escape = case fun_amode of
					CJoinPoint _ -> True
					_ -> False

-- -----------------------------------------------------------------------------
-- The final clean-up before we do a jump at the end of a basic block.
-- This code is shared by tail-calls and returns.

doFinalJump :: VirtualSpOffset -> AbstractC -> Bool -> (Sequel -> Code) -> Code 
doFinalJump final_sp sim_assts is_let_no_escape jump_code =

    -- adjust the high-water mark if necessary
    adjustStackHW final_sp	`thenC`

    -- Do the simultaneous assignments,
    absC (CSimultaneous sim_assts) `thenC`

	-- push a return address if necessary (after the assignments
	-- above, in case we clobber a live stack location)
	--
	-- DONT push the return address when we're about to jump to a
	-- let-no-escape: the final tail call in the let-no-escape
	-- will do this.
    getEndOfBlockInfo `thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->
    (if is_let_no_escape then nopC
			 else pushReturnAddress eob)	`thenC`

    -- Final adjustment of Sp/Hp
    adjustSpAndHp final_sp		`thenC`

    -- and do the jump
    jump_code sequel

-- -----------------------------------------------------------------------------
-- A general return (just a special case of doFinalJump, above)

performReturn :: AbstractC	    -- Simultaneous assignments to perform
	      -> (Sequel -> Code)   -- The code to execute to actually do
				    -- the return, given an addressing mode
				    -- for the return address
	      -> Code

performReturn sim_assts finish_code
  = getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->
    doFinalJump args_sp sim_assts False{-not a LNE-} finish_code

-- -----------------------------------------------------------------------------
-- Primitive Returns

-- Just load the return value into the right register, and return.

performPrimReturn :: SDoc	-- Just for debugging (sigh)
		  -> CAddrMode	-- The thing to return
		  -> Code

performPrimReturn doc amode
  = let
	kind = getAmodeRep amode
	ret_reg = dataReturnConvPrim kind

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

-- -----------------------------------------------------------------------------
-- Algebraic constructor returns

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
	VectoredReturn sz -> profCtrC FSLIT("TICK_VEC_RETURN") [mkIntCLit sz]
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
	CaseAlts _ (Just (alts, _)) False -> -- Ho! We know the constructor so
					-- we can go right to the alternative

		case assocMaybe alts tag of
		   Just (alt_absC, join_lbl) -> 
			absC (CJump (CLbl join_lbl CodePtrRep))
		   Nothing -> panic "mkStaticAlgReturnCode: default"
				-- The Nothing case should never happen; 
				-- it's the subject of a wad of special-case 
				-- code in cgReturnCon

	other ->	-- OnStack, or (CaseAlts ret_amode Nothing),
			-- or UpdateCode.
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


-- -----------------------------------------------------------------------------
-- Returning an enumerated type from a PrimOp

-- This function is used by PrimOps that return enumerated types (i.e.
-- all the comparison operators).

mkDynamicAlgReturnCode :: TyCon -> CAddrMode -> Sequel -> Code

mkDynamicAlgReturnCode tycon dyn_tag sequel
  = case ctrlReturnConvAlg tycon of
	VectoredReturn sz ->

		profCtrC FSLIT("TICK_VEC_RETURN") [mkIntCLit sz] `thenC`
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


-- ---------------------------------------------------------------------------
-- Unboxed tuple returns

-- These are a bit like a normal tail call, except that:
--
--   - The tail-call target is an info table on the stack
--
--   - We separate stack arguments into pointers and non-pointers,
--     to make it easier to leave things in a sane state for a heap check.
--     This is OK because we can never partially-apply an unboxed tuple,
--     unlike a function.  The same technique is used when calling
--     let-no-escape functions, because they also can't be partially
--     applied.

returnUnboxedTuple :: [CAddrMode] -> Code
returnUnboxedTuple amodes =
    getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->

    profCtrC FSLIT("TICK_RET_UNBOXED_TUP") [mkIntCLit (length amodes)] `thenC`

    pushUnboxedTuple args_sp amodes `thenFC` \ (final_sp, assts) ->
    doFinalJump final_sp assts False{-not a LNE-} mkUnboxedTupleReturnCode


pushUnboxedTuple
	:: VirtualSpOffset		-- Sp at which to start pushing
	-> [CAddrMode]			-- amodes of the components
	-> FCode (VirtualSpOffset,	-- final Sp
		  AbstractC)		-- assignments (regs+stack)

pushUnboxedTuple sp amodes =
    let
        (arg_regs, _leftovers) = assignRegs [] (map getAmodeRep amodes)

	(reg_arg_amodes, stk_arg_amodes) = splitAtList arg_regs amodes

	-- separate the rest of the args into pointers and non-pointers
	( ptr_args, nptr_args ) = 
	   partition (isFollowableRep . getAmodeRep) stk_arg_amodes

	reg_arg_assts
	  = mkAbstractCs (zipWithEqual "assign_to_reg2" 
				assign_to_reg arg_regs reg_arg_amodes)
    in

    -- push ptrs, then nonptrs, on the stack
    mkStkAmodes sp ptr_args       `thenFC` \ (ptr_sp,  ptr_assts) ->
    mkStkAmodes ptr_sp  nptr_args `thenFC` \ (final_sp, nptr_assts) ->

    returnFC (final_sp, 
	      mkAbstractCs [reg_arg_assts, ptr_assts, nptr_assts])
    
		  

mkUnboxedTupleReturnCode :: Sequel -> Code
mkUnboxedTupleReturnCode sequel
    = case sequel of
	-- can't update with an unboxed tuple!
	UpdateCode -> panic "mkUnboxedTupleReturnCode"

	CaseAlts _ (Just ([(_,(alt_absC,join_lbl))], _)) False ->
			absC (CJump (CLbl join_lbl CodePtrRep))

	other ->	-- OnStack, or (CaseAlts ret_amode something)
		    sequelToAmode sequel	`thenFC` \ ret_amode ->
		    absC (CReturn ret_amode DirectReturn)

-- -----------------------------------------------------------------------------
-- Returning unboxed tuples.  This is mainly to support _ccall_GC_, where
-- we want to do things in a slightly different order to normal:
-- 
-- 		- push return address
-- 		- adjust stack pointer
-- 		- r = call(args...)
-- 		- assign regs for unboxed tuple (usually just R1 = r)
-- 		- return to continuation
-- 
-- The return address (i.e. stack frame) must be on the stack before
-- doing the call in case the call ends up in the garbage collector.
-- 
-- Sadly, the information about the continuation is lost after we push it
-- (in order to avoid pushing it again), so we end up doing a needless
-- indirect jump (ToDo).

ccallReturnUnboxedTuple :: [CAddrMode] -> Code -> Code
ccallReturnUnboxedTuple amodes before_jump
  = getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->

	-- push a return address if necessary
    pushReturnAddress eob		`thenC`
    setEndOfBlockInfo (EndOfBlockInfo args_sp (OnStack args_sp)) (

	-- Adjust Sp/Hp
    adjustSpAndHp args_sp		`thenC`

    before_jump				`thenC`
  
    returnUnboxedTuple amodes
  )

-- -----------------------------------------------------------------------------
-- Calling an out-of-line primop

tailCallPrimOp :: PrimOp -> [StgArg] -> Code
tailCallPrimOp op args =
    -- we're going to perform a normal-looking tail call, 
    -- except that *all* the arguments will be in registers.
    getArgAmodes args		`thenFC` \ arg_amodes ->
    let (arg_regs, leftovers) = assignAllRegs [] (map getAmodeRep arg_amodes)

	reg_arg_assts
	  = mkAbstractCs (zipWithEqual "assign_to_reg2" 
				assign_to_reg arg_regs arg_amodes)

	jump_to_primop = 
	   absC (CJump (CLbl (mkRtsPrimOpLabel op) CodePtrRep))
    in

    ASSERT(null leftovers) -- no stack-resident args

    getEndOfBlockInfo	`thenFC` \ eob@(EndOfBlockInfo args_sp sequel) ->
    doFinalJump args_sp reg_arg_assts False{-not a LNE-} (const jump_to_primop)

-- -----------------------------------------------------------------------------
-- Return Addresses

-- | We always push the return address just before performing a tail call
-- or return.  The reason we leave it until then is because the stack
-- slot that the return address is to go into might contain something
-- useful.
-- 
-- If the end of block info is 'CaseAlts', then we're in the scrutinee of a
-- case expression and the return address is still to be pushed.
-- 
-- There are cases where it doesn't look necessary to push the return
-- address: for example, just before doing a return to a known
-- continuation.  However, the continuation will expect to find the
-- return address on the stack in case it needs to do a heap check.

pushReturnAddress :: EndOfBlockInfo -> Code

pushReturnAddress (EndOfBlockInfo args_sp sequel@(CaseAlts amode _ False)) =
    getSpRelOffset args_sp			 `thenFC` \ sp_rel ->
    absC (CAssign (CVal sp_rel RetRep) amode)

-- For a polymorphic case, we have two return addresses to push: the case
-- return, and stg_seq_frame_info which turns a possible vectored return
-- into a direct one.
pushReturnAddress (EndOfBlockInfo args_sp sequel@(CaseAlts amode _ True)) =
    getSpRelOffset (args_sp-1)			 `thenFC` \ sp_rel ->
    absC (CAssign (CVal sp_rel RetRep) amode)	 `thenC`
    getSpRelOffset args_sp			 `thenFC` \ sp_rel ->
    absC (CAssign (CVal sp_rel RetRep) (CLbl mkSeqInfoLabel RetRep))
pushReturnAddress _ = nopC

-- -----------------------------------------------------------------------------
-- Misc.

assign_to_reg reg_id amode = CAssign (CReg reg_id) amode

\end{code}
