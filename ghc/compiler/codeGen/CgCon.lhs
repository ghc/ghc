%
% (c) The GRASP Project, Glasgow University, 1992-1998
%
\section[CgCon]{Code generation for constructors}

This module provides the support code for @StgToAbstractC@ to deal
with {\em constructors} on the RHSs of let(rec)s.  See also
@CgClosure@, which deals with closures.

\begin{code}
module CgCon (
	cgTopRhsCon, buildDynCon,
	bindConArgs, bindUnboxedTupleComponents,
	cgReturnDataCon
    ) where

#include "HsVersions.h"

import CgMonad
import AbsCSyn
import StgSyn

import AbsCUtils	( getAmodeRep )
import CgBindery	( getArgAmodes, bindNewToNode,
			  bindArgsToRegs, newTempAmodeAndIdInfo,
			  idInfoToAmode, stableAmodeIdInfo,
			  heapIdInfo, CgIdInfo, bindNewToStack
			)
import CgStackery	( mkTaggedVirtStkOffsets, freeStackSlots )
import CgUsages		( getRealSp, getVirtSp, setRealAndVirtualSp )
import CgClosure	( cgTopRhsClosure )
import CgRetConv	( assignRegs )
import Constants	( mAX_INTLIKE, mIN_INTLIKE )
import CgHeapery	( allocDynClosure )
import CgTailCall	( performReturn, mkStaticAlgReturnCode, doTailCall,
			  mkUnboxedTupleReturnCode )
import CLabel		( mkClosureLabel, mkStaticClosureLabel )
import ClosureInfo	( mkClosureLFInfo, mkConLFInfo, mkLFArgument,
			  layOutDynCon, layOutDynClosure,
			  layOutStaticClosure
			)
import CostCentre	( currentOrSubsumedCCS, dontCareCCS, CostCentreStack,
			  currentCCS )
import DataCon		( DataCon, dataConName, dataConTag, dataConTyCon,
			  isUnboxedTupleCon )
import MkId		( mkDataConId )
import Id		( Id, idName, idType, idPrimRep )
import Const		( Con(..), Literal(..) )
import PrelInfo		( maybeCharLikeCon, maybeIntLikeCon )
import PrimRep		( PrimRep(..) )
import BasicTypes	( TopLevelFlag(..) )
import Util
import Panic		( assertPanic )
\end{code}

%************************************************************************
%*									*
\subsection[toplevel-constructors]{Top-level constructors}
%*									*
%************************************************************************

\begin{code}
cgTopRhsCon :: Id		-- Name of thing bound to this RHS
	    -> DataCon		-- Id
	    -> [StgArg]		-- Args
	    -> Bool		-- All zero-size args (see buildDynCon)
	    -> FCode (Id, CgIdInfo)
\end{code}

Special Case: Constructors some of whose arguments are of \tr{Double#}
type, {\em or} which are ``lit lits'' (which are given \tr{Addr#}
type).

These ones have to be compiled as re-entrant thunks rather than
closures, because we can't figure out a way to persuade C to allow us
to initialise a static closure with Doubles!  Thus, for \tr{x = 2.0}
(defaults to Double), we get:

\begin{verbatim}
-- The STG syntax:
    Main.x = MkDouble [2.0##]

-- C Code:

-- closure:
    SET_STATIC_HDR(Main_x_closure,Main_x_static,CC_DATA,,EXTDATA_RO)
    };
-- its *own* info table:
    STATIC_INFO_TABLE(Main_x,Main_x_entry,,,,EXTFUN,???,":MkDouble","Double");
-- with its *own* entry code:
    STGFUN(Main_x_entry) {
	P_ u1701;
	RetDouble1=2.0;
	u1701=(P_)*SpB;
	SpB=SpB-1;
	JMP_(u1701[0]);
    }
\end{verbatim}

The above has the down side that each floating-point constant will end
up with its own info table (rather than sharing the MkFloat/MkDouble
ones).  On the plus side, however, it does return a value (\tr{2.0})
{\em straight away}.

Here, then is the implementation: just pretend it's a non-updatable
thunk.  That is, instead of

	x = D# 3.455#

pretend we've seen

	x = [] \n [] -> D# 3.455#

\begin{code}
top_ccc = mkCCostCentreStack dontCareCCS -- because it's static data

cgTopRhsCon bndr con args all_zero_size_args
  | any isLitLitArg args
  = cgTopRhsClosure bndr dontCareCCS NoStgBinderInfo NoSRT [] body lf_info
  where
    body    = StgCon (DataCon con) args rhs_ty
    lf_info = mkClosureLFInfo bndr TopLevel [] ReEntrant []
    rhs_ty  = idType bndr
\end{code}

OK, so now we have the general case.

\begin{code}
cgTopRhsCon id con args all_zero_size_args
  = (
	-- LAY IT OUT
    getArgAmodes args		`thenFC` \ amodes ->

    let
	(closure_info, amodes_w_offsets)
	  = layOutStaticClosure name getAmodeRep amodes lf_info
    in

	-- BUILD THE OBJECT
    absC (CStaticClosure
	    closure_label		-- Labelled with the name on lhs of defn
	    closure_info		-- Closure is static
	    top_ccc
	    (map fst amodes_w_offsets)) -- Sorted into ptrs first, then nonptrs

    ) `thenC`

	-- RETURN
    returnFC (id, stableAmodeIdInfo id (CLbl closure_label PtrRep) lf_info)
  where
    con_tycon	    = dataConTyCon   con
    lf_info	    = mkConLFInfo    con
    closure_label   = mkClosureLabel name
    name            = idName id
\end{code}

%************************************************************************
%*									*
%* non-top-level constructors						*
%*									*
%************************************************************************
\subsection[code-for-constructors]{The code for constructors}

\begin{code}
buildDynCon :: Id		-- Name of the thing to which this constr will
				-- be bound
	    -> CostCentreStack	-- Where to grab cost centre from;
				-- current CCS if currentOrSubsumedCCS
	    -> DataCon		-- The data constructor
	    -> [CAddrMode]	-- Its args
	    -> Bool		-- True <=> all args (if any) are
				-- of "zero size" (i.e., VoidRep);
				-- The reason we don't just look at the
				-- args is that we may be in a "knot", and
				-- premature looking at the args will cause
				-- the compiler to black-hole!
	    -> FCode CgIdInfo	-- Return details about how to find it
\end{code}

First we deal with the case of zero-arity constructors.  Now, they
will probably be unfolded, so we don't expect to see this case much,
if at all, but it does no harm, and sets the scene for characters.

In the case of zero-arity constructors, or, more accurately, those
which have exclusively size-zero (VoidRep) args, we generate no code
at all.

\begin{code}
buildDynCon binder cc con args all_zero_size_args@True
  = returnFC (stableAmodeIdInfo binder
				(CLbl (mkStaticClosureLabel (dataConName con)) PtrRep)
    				(mkConLFInfo con))
\end{code}

Now for @Char@-like closures.  We generate an assignment of the
address of the closure to a temporary.  It would be possible simply to
generate no code, and record the addressing mode in the environment,
but we'd have to be careful if the argument wasn't a constant --- so
for simplicity we just always asssign to a temporary.

Last special case: @Int@-like closures.  We only special-case the
situation in which the argument is a literal in the range
@mIN_INTLIKE@..@mAX_INTLILKE@.  NB: for @Char@-like closures we can
work with any old argument, but for @Int@-like ones the argument has
to be a literal.  Reason: @Char@ like closures have an argument type
which is guaranteed in range.

Because of this, we use can safely return an addressing mode.

\begin{code}
buildDynCon binder cc con [arg_amode] all_zero_size_args@False

  | maybeCharLikeCon con
  = absC (CAssign temp_amode (CCharLike arg_amode))	`thenC`
    returnFC temp_id_info

  | maybeIntLikeCon con && in_range_int_lit arg_amode
  = returnFC (stableAmodeIdInfo binder (CIntLike arg_amode) (mkConLFInfo con))
  where
    (temp_amode, temp_id_info) = newTempAmodeAndIdInfo binder (mkConLFInfo con)

    in_range_int_lit (CLit (MachInt val _)) = val <= mAX_INTLIKE && val >= mIN_INTLIKE
    in_range_int_lit other_amode	    = False

    tycon = dataConTyCon con
\end{code}

Now the general case.

\begin{code}
buildDynCon binder ccs con args all_zero_size_args@False
  = allocDynClosure closure_info use_cc blame_cc amodes_w_offsets `thenFC` \ hp_off ->
    returnFC (heapIdInfo binder hp_off lf_info)
  where
    (closure_info, amodes_w_offsets)
      = layOutDynClosure (idName binder) getAmodeRep args lf_info
    lf_info = mkConLFInfo con

    use_cc	-- cost-centre to stick in the object
      = if currentOrSubsumedCCS ccs
	then CReg CurCostCentre
	else mkCCostCentreStack ccs

    blame_cc = use_cc -- cost-centre on which to blame the alloc (same)
\end{code}


%************************************************************************
%*									*
%* constructor-related utility function:				*
%*		bindConArgs is called from cgAlt of a case		*
%*									*
%************************************************************************
\subsection[constructor-utilities]{@bindConArgs@: constructor-related utility}

@bindConArgs@ $con args$ augments the environment with bindings for the
binders $args$, assuming that we have just returned from a @case@ which
found a $con$.

\begin{code}
bindConArgs 
	:: DataCon -> [Id]		-- Constructor and args
	-> Code

bindConArgs con args
  = ASSERT(not (isUnboxedTupleCon con))
    mapCs bind_arg args_w_offsets
   where
     bind_arg (arg, offset) = bindNewToNode arg offset mkLFArgument
     (_, args_w_offsets) = layOutDynCon con idPrimRep args
\end{code}

Unboxed tuples are handled slightly differently - the object is
returned in registers and on the stack instead of the heap.

\begin{code}
bindUnboxedTupleComponents
	:: [Id]					-- args
	-> FCode ([MagicId], 			-- regs assigned
		  [(VirtualSpOffset,Int)], 	-- tag slots
		  Bool)				-- any components on stack?

bindUnboxedTupleComponents args
 =  -- Assign as many components as possible to registers
    let (arg_regs, leftovers) = assignRegs [] (map idPrimRep args)
	(reg_args, stk_args) = splitAt (length arg_regs) args
    in

    -- Allocate the rest on the stack (ToDo: separate out pointers)
    getVirtSp `thenFC` \ vsp ->
    getRealSp `thenFC` \ rsp ->
    let (top_sp, stk_offsets, tags) = 
		mkTaggedVirtStkOffsets rsp idPrimRep stk_args
    in

    -- The stack pointer points to the last stack-allocated component
    setRealAndVirtualSp top_sp  		`thenC`

    -- need to explicitly free any empty slots we just jumped over
    (if vsp < rsp then freeStackSlots [vsp+1 .. rsp] else nopC) `thenC`

    bindArgsToRegs reg_args arg_regs 		`thenC`
    mapCs bindNewToStack stk_offsets 		`thenC`
    returnFC (arg_regs,tags, not (null stk_offsets))
\end{code}

%************************************************************************
%*									*
\subsubsection[CgRetConv-cgReturnDataCon]{Actually generate code for a constructor return}
%*									*
%************************************************************************


Note: it's the responsibility of the @cgReturnDataCon@ caller to be
sure the @amodes@ passed don't conflict with each other.
\begin{code}
cgReturnDataCon :: DataCon -> [CAddrMode] -> Bool -> Code

cgReturnDataCon con amodes all_zero_size_args
  = getEndOfBlockInfo	`thenFC` \ (EndOfBlockInfo args_sp sequel) ->

    case sequel of

      CaseAlts _ (Just (alts, Just (maybe_deflt_binder, (_,deflt_lbl))))
	| not (dataConTag con `is_elem` map fst alts)
	->
		-- Special case!  We're returning a constructor to the default case
		-- of an enclosing case.  For example:
		--
		--	case (case e of (a,b) -> C a b) of
		--	  D x -> ...
		--	  y   -> ...<returning here!>...
		--
		-- In this case,
		--	if the default is a non-bind-default (ie does not use y),
		--	then we should simply jump to the default join point;
		--
		--	if the default is a bind-default (ie does use y), we
		--	should return the constructor in the heap,
		--      pointed to by Node.

		case maybe_deflt_binder of
		  Just binder ->
			ASSERT(not (isUnboxedTupleCon con))
			buildDynCon binder currentCCS con amodes all_zero_size_args
								`thenFC` \ idinfo ->
		  	profCtrC SLIT("TICK_RET_NEW") [mkIntCLit (length amodes)] `thenC`
			idInfoToAmode PtrRep idinfo		`thenFC` \ amode ->
			performReturn (move_to_reg amode node)  jump_to_join_point

		  Nothing ->
			performReturn AbsCNop {- No reg assts -} jump_to_join_point
	where
	  is_elem = isIn "cgReturnDataCon"
	  jump_to_join_point sequel = absC (CJump (CLbl deflt_lbl CodePtrRep))
		-- Ignore the sequel: we've already looked at it above

      other_sequel	-- The usual case

	  | isUnboxedTupleCon con ->
			-- Return unboxed tuple in registers
		  let (ret_regs, leftovers) = 
			 assignRegs [] (map getAmodeRep amodes)
		  in
		  profCtrC SLIT("TICK_RET_UNBOXED_TUP") 
				[mkIntCLit (length amodes)] `thenC`

		  doTailCall amodes ret_regs 
			mkUnboxedTupleReturnCode
			(length leftovers)  {- fast args arity -}
			AbsCNop {-no pending assigments-}
			Nothing {-not a let-no-escape-}
			False   {-node doesn't point-}
		
          | otherwise ->
			-- BUILD THE OBJECT IN THE HEAP
			-- The first "con" says that the name bound to this
			-- closure is "con", which is a bit of a fudge, but it only
			-- affects profiling

			-- This Id is also used to get a unique for a
			-- temporary variable, if the closure is a CHARLIKE.
			-- funilly enough, this makes the unique always come
			-- out as '54' :-)
		  buildDynCon (mkDataConId con) currentCCS 
			con amodes all_zero_size_args
							`thenFC` \ idinfo ->
		  idInfoToAmode PtrRep idinfo		`thenFC` \ amode ->


			-- RETURN
		  profCtrC SLIT("TICK_RET_NEW") [mkIntCLit (length amodes)] `thenC`
		  -- could use doTailCall here.
		  performReturn (move_to_reg amode node) 
			(mkStaticAlgReturnCode con)

  where
    con_name = dataConName con

    move_to_reg :: CAddrMode -> MagicId -> AbstractC
    move_to_reg src_amode dest_reg = CAssign (CReg dest_reg) src_amode
\end{code}
