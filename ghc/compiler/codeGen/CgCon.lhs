%
% (c) The GRASP Project, Glasgow University, 1992-1996
%
\section[CgCon]{Code generation for constructors}

This module provides the support code for @StgToAbstractC@ to deal
with {\em constructors} on the RHSs of let(rec)s.  See also
@CgClosure@, which deals with closures.

\begin{code}
#include "HsVersions.h"

module CgCon (
	cgTopRhsCon, buildDynCon,
	bindConArgs,
	cgReturnDataCon
    ) where

IMP_Ubiq(){-uitous-}

import CgMonad
import AbsCSyn
import StgSyn

import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CgBindery	( getArgAmodes, bindNewToNode,
			  bindArgsToRegs, newTempAmodeAndIdInfo,
			  idInfoToAmode, stableAmodeIdInfo,
			  heapIdInfo, CgIdInfo
			)
import CgClosure	( cgTopRhsClosure )
import Constants	( mAX_INTLIKE, mIN_INTLIKE )
import CgHeapery	( allocDynClosure )
import CgRetConv	( dataReturnConvAlg, DataReturnConvention(..) )
import CgTailCall	( performReturn, mkStaticAlgReturnCode )
import CLabel		( mkClosureLabel, mkStaticClosureLabel,
			  mkConInfoTableLabel, mkPhantomInfoTableLabel
			)
import ClosureInfo	( mkClosureLFInfo, mkConLFInfo, mkLFArgument,
			  layOutDynCon, layOutDynClosure,
			  layOutStaticClosure
			)
import CostCentre	( currentOrSubsumedCosts, useCurrentCostCentre,
			  dontCareCostCentre, CostCentre
			)
import Id		( idPrimRep, dataConTag, dataConTyCon,
			  isDataCon, SYN_IE(DataCon),
			  emptyIdSet, SYN_IE(Id)
			)
import Literal		( Literal(..) )
import Maybes		( maybeToBool )
import PrelInfo		( maybeCharLikeTyCon, maybeIntLikeTyCon )
import PrimRep		( isFloatingRep, PrimRep(..) )
import TyCon		( TyCon{-instance Uniquable-} )
import Util		( isIn, zipWithEqual, panic, assertPanic )
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

Special Case:
Constructors some of whose arguments are of \tr{Float#} or
\tr{Double#} type, {\em or} which are ``lit lits'' (which are given
\tr{Addr#} type).

These ones have to be compiled as re-entrant thunks rather than closures,
because we can't figure out a way to persuade C to allow us to initialise a
static closure with Floats and Doubles!
Thus, for \tr{x = 2.0} (defaults to Double), we get:

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

	x = F# 3.455#

pretend we've seen

	x = [] \n [] -> F# 3.455#

\begin{code}
top_cc  = dontCareCostCentre -- out here to avoid a cgTopRhsCon CAF (sigh)
top_ccc = mkCCostCentre dontCareCostCentre -- because it's static data

cgTopRhsCon name con args all_zero_size_args
  |  any (isFloatingRep . getArgPrimRep) args
  || any isLitLitArg args
  = cgTopRhsClosure name top_cc NoStgBinderInfo [] body lf_info
  where
    body = StgCon con args emptyIdSet{-emptyLiveVarSet-}
    lf_info = mkClosureLFInfo True {- Top level -} [] ReEntrant []
\end{code}

OK, so now we have the general case.

\begin{code}
cgTopRhsCon name con args all_zero_size_args
  = (
    ASSERT(isDataCon con)

	-- LAY IT OUT
    getArgAmodes args		`thenFC` \ amodes ->

    let
	(closure_info, amodes_w_offsets)
	  = layOutStaticClosure name getAmodeRep amodes lf_info
    in
	-- HWL: In 0.22 there was a heap check in here that had to be changed.
	--      CHECK if having no heap check is ok for GrAnSim here!!!

	-- BUILD THE OBJECT
    absC (CStaticClosure
	    closure_label			-- Labelled with the name on lhs of defn
	    closure_info			-- Closure is static
	    top_ccc
	    (map fst amodes_w_offsets))		-- Sorted into ptrs first, then nonptrs

    ) `thenC`

	-- RETURN
    returnFC (name, stableAmodeIdInfo name (CLbl closure_label PtrRep) lf_info)
  where
    con_tycon	    = dataConTyCon   con
    lf_info	    = mkConLFInfo    con
    closure_label   = mkClosureLabel name
\end{code}

The general case is:
\begin{verbatim}
-- code:
    data Foo = MkFoo
    x = MkFoo

-- STG code:
STG syntax:
    Main.x = Main.MkFoo []

-- interesting parts of the C Code:

-- closure for "x":
    SET_STATIC_HDR(Main_x_closure,Main_MkFoo_static,CC_DATA,,EXTDATA_RO)
    };
-- entry code for "x":
    STGFUN(Main_x_entry) {
	Node=(W_)(Main_x_closure);
	STGJUMP(Main_MkFoo_entry);
    }
\end{verbatim}

Observe: (1)~We create a static closure for \tr{x}, {\em reusing} the
regular \tr{MkFoo} info-table and entry code.  (2)~However: the
\tr{MkFoo} code expects Node to be set, but the caller of \tr{x_entry}
will not have set it.  Therefore, the whole point of \tr{x_entry} is
to set node (and then call the shared \tr{MkFoo} entry code).

Special Case:
For top-level Int/Char constants. We get entry-code fragments of the form:

\begin{verbatim}
-- code:
    y = 1

-- entry code for "y":
    STGFUN(Main_y_entry) {
	Node=(W_)(Main_y_closure);
	STGJUMP(I#_entry);
    }
\end{verbatim}

This is pretty tiresome: we {\em know} what the constant is---we'd
rather just return it.  We end up with something that's a hybrid
between the Float/Double and general cases: (a)~like Floats/Doubles,
the entry-code returns the value immediately; (b)~like the general
case, we share the data-constructor's std info table.  So, what we
want is:
\begin{verbatim}
-- code:
    z = 1

-- STG code:
STG syntax:
    Main.z = I# [1#]

-- interesting parts of the C Code:

-- closure for "z" (shares I# info table):
    SET_STATIC_HDR(Main_z_closure,I#_static,CC_DATA,,EXTDATA_RO)
    };
-- entry code for "z" (do the business directly):
    STGFUN(Main_z_entry) {
    	P_ u1702;
	Ret1=1;
	u1702=(P_)*SpB;
	SpB=SpB-1;
	JMP_(u1702[0]);
    }
\end{verbatim}

This blob used to be in cgTopRhsCon, but I don't see how we can jump
direct to the named code for a constructor; any external entries will
be via Node.  Generating all this extra code is a real waste for big
static data structures.  So I've nuked it.  SLPJ Sept 94

%************************************************************************
%*									*
%* non-top-level constructors						*
%*									*
%************************************************************************
\subsection[code-for-constructors]{The code for constructors}

\begin{code}
buildDynCon :: Id		-- Name of the thing to which this constr will
				-- be bound
	    -> CostCentre	-- Where to grab cost centre from;
				-- current CC if currentOrSubsumedCosts
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
  = ASSERT(isDataCon con)
    returnFC (stableAmodeIdInfo binder
				(CLbl (mkStaticClosureLabel con) PtrRep)
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

  | maybeToBool (maybeCharLikeTyCon tycon)
  = ASSERT(isDataCon con)
    absC (CAssign temp_amode (CCharLike arg_amode))	`thenC`
    returnFC temp_id_info

  | maybeToBool (maybeIntLikeTyCon tycon) && in_range_int_lit arg_amode
  = ASSERT(isDataCon con)
    returnFC (stableAmodeIdInfo binder (CIntLike arg_amode) (mkConLFInfo con))
  where
    tycon = dataConTyCon con
    (temp_amode, temp_id_info) = newTempAmodeAndIdInfo binder (mkConLFInfo con)

    in_range_int_lit (CLit (MachInt val _)) = val <= mAX_INTLIKE && val >= mIN_INTLIKE
    in_range_int_lit other_amode	    = False
\end{code}

Now the general case.

\begin{code}
buildDynCon binder cc con args all_zero_size_args@False
  = ASSERT(isDataCon con)
    allocDynClosure closure_info use_cc blame_cc amodes_w_offsets `thenFC` \ hp_off ->
    returnFC (heapIdInfo binder hp_off (mkConLFInfo con))
  where
    (closure_info, amodes_w_offsets)
      = layOutDynClosure binder getAmodeRep args (mkConLFInfo con)

    use_cc	-- cost-centre to stick in the object
      = if currentOrSubsumedCosts cc
	then CReg CurCostCentre
	else mkCCostCentre cc

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
bindConArgs :: DataCon -> [Id] -> Code
bindConArgs con args
  = ASSERT(isDataCon con)
    case (dataReturnConvAlg con) of
      ReturnInRegs rs  -> bindArgsToRegs args rs
      ReturnInHeap     ->
	  let
	      (_, args_w_offsets) = layOutDynCon con idPrimRep args
	  in
	  mapCs bind_arg args_w_offsets
   where
     bind_arg (arg, offset) = bindNewToNode arg offset mkLFArgument
\end{code}


%************************************************************************
%*									*
\subsubsection[CgRetConv-cgReturnDataCon]{Actually generate code for a constructor return}
%*									*
%************************************************************************


Note: it's the responsibility of the @cgReturnDataCon@ caller to be
sure the @amodes@ passed don't conflict with each other.
\begin{code}
cgReturnDataCon :: DataCon -> [CAddrMode] -> Bool -> StgLiveVars -> Code

cgReturnDataCon con amodes all_zero_size_args live_vars
  = ASSERT(isDataCon con)
    getEndOfBlockInfo	`thenFC` \ (EndOfBlockInfo args_spa args_spb sequel) ->

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
		--	should return the constructor IN THE HEAP, pointed to by Node,
		--	**regardless** of the return convention of the constructor C.

		case maybe_deflt_binder of
		  Just binder ->
			buildDynCon binder useCurrentCostCentre con amodes all_zero_size_args
								`thenFC` \ idinfo ->
			idInfoToAmode PtrRep idinfo		`thenFC` \ amode ->
			performReturn (move_to_reg amode node)  jump_to_join_point live_vars

		  Nothing ->
			performReturn AbsCNop {- No reg assts -} jump_to_join_point live_vars
	where
	  is_elem = isIn "cgReturnDataCon"
	  jump_to_join_point sequel = absC (CJump (CLbl deflt_lbl CodePtrRep))
		-- Ignore the sequel: we've already looked at it above

      other_sequel ->	-- The usual case
	    case (dataReturnConvAlg con) of

	      ReturnInHeap	    ->
			-- BUILD THE OBJECT IN THE HEAP
			-- The first "con" says that the name bound to this
			-- closure is "con", which is a bit of a fudge, but it only
			-- affects profiling (ToDo?)
		  buildDynCon con useCurrentCostCentre con amodes all_zero_size_args
							`thenFC` \ idinfo ->
		  idInfoToAmode PtrRep idinfo		`thenFC` \ amode ->

			-- MAKE NODE POINT TO IT
		  let reg_assts = move_to_reg amode node
		      info_lbl  = mkConInfoTableLabel con
		  in

			-- RETURN
		  profCtrC SLIT("RET_NEW_IN_HEAP") [mkIntCLit (length amodes)] `thenC`

		  performReturn reg_assts (mkStaticAlgReturnCode con (Just info_lbl)) live_vars

	      ReturnInRegs regs  ->
	    	  let
		      reg_assts = mkAbstractCs (zipWithEqual "move_to_reg" move_to_reg amodes regs)
		      info_lbl  = mkPhantomInfoTableLabel con
		  in
		  profCtrC SLIT("RET_NEW_IN_REGS") [mkIntCLit (length amodes)] `thenC`

	    	  performReturn reg_assts (mkStaticAlgReturnCode con (Just info_lbl)) live_vars
  where
    move_to_reg :: CAddrMode -> MagicId -> AbstractC
    move_to_reg src_amode dest_reg = CAssign (CReg dest_reg) src_amode
\end{code}
