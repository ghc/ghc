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
			  bindArgsToRegs, 
			  idInfoToAmode, stableAmodeIdInfo,
			  heapIdInfo, CgIdInfo, bindNewToStack
			)
import CgStackery	( mkVirtStkOffsets, freeStackSlots, updateFrameSize )
import CgUsages		( getRealSp, getVirtSp, setRealAndVirtualSp,
			  getSpRelOffset )
import CgRetConv	( assignRegs )
import Constants	( mAX_INTLIKE, mIN_INTLIKE, mAX_CHARLIKE, mIN_CHARLIKE,
			  mIN_UPD_SIZE )
import CgHeapery	( allocDynClosure )
import CgTailCall	( performReturn, mkStaticAlgReturnCode,
			  returnUnboxedTuple )
import CLabel		( mkClosureLabel )
import ClosureInfo	( mkConLFInfo, mkLFArgument, layOutDynConstr, 
			  layOutStaticConstr, closureSize, mkStaticClosure
			)
import CostCentre	( currentOrSubsumedCCS, dontCareCCS, CostCentreStack,
			  currentCCS )
import DataCon		( DataCon, dataConTag, 
			  isUnboxedTupleCon, isNullaryDataCon, dataConWorkId, 
			  dataConName, dataConRepArity
			)
import Id		( Id, idName, idPrimRep, isDeadBinder )
import Literal		( Literal(..) )
import PrelInfo		( maybeCharLikeCon, maybeIntLikeCon )
import PrimRep		( PrimRep(..), isFollowableRep )
import Unique		( Uniquable(..) )
import Util
import Outputable

import List		( partition )
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
	    -> FCode (Id, CgIdInfo)
cgTopRhsCon id con args
  = ASSERT( not (isDllConApp con args) )
    ASSERT( args `lengthIs` dataConRepArity con )

	-- LAY IT OUT
    getArgAmodes args		`thenFC` \ amodes ->

    let
	name          = idName id
	lf_info	      = mkConLFInfo con
    	closure_label = mkClosureLabel name
	(closure_info, amodes_w_offsets) 
		= layOutStaticConstr con getAmodeRep amodes
	caffy = any stgArgHasCafRefs args
    in

	-- BUILD THE OBJECT
    absC (mkStaticClosure
	    closure_label
	    closure_info
	    dontCareCCS			-- because it's static data
	    (map fst amodes_w_offsets)  -- Sorted into ptrs first, then nonptrs
	    caffy			-- has CAF refs
	  )					`thenC`
		-- NOTE: can't use idCafInfo instead of nonEmptySRT above,
		-- because top-level constructors that were floated by
		-- CorePrep don't have CafInfo attached.  The SRT is more
		-- reliable.

	-- RETURN
    returnFC (id, stableAmodeIdInfo id (CLbl closure_label PtrRep) lf_info)
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
	    -> FCode CgIdInfo	-- Return details about how to find it

-- We used to pass a boolean indicating whether all the
-- args were of size zero, so we could use a static
-- construtor; but I concluded that it just isn't worth it.
-- Now I/O uses unboxed tuples there just aren't any constructors
-- with all size-zero args.
--
-- The reason for having a separate argument, rather than looking at
-- the addr modes of the args is that we may be in a "knot", and
-- premature looking at the args will cause the compiler to black-hole!
\end{code}

First we deal with the case of zero-arity constructors.  Now, they
will probably be unfolded, so we don't expect to see this case much,
if at all, but it does no harm, and sets the scene for characters.

In the case of zero-arity constructors, or, more accurately, those
which have exclusively size-zero (VoidRep) args, we generate no code
at all.

\begin{code}
buildDynCon binder cc con []
  = returnFC (stableAmodeIdInfo binder
				(CLbl (mkClosureLabel (dataConName con)) PtrRep)
    				(mkConLFInfo con))
\end{code}

The following three paragraphs about @Char@-like and @Int@-like
closures are obsolete, but I don't understand the details well enough
to properly word them, sorry. I've changed the treatment of @Char@s to
be analogous to @Int@s: only a subset is preallocated, because @Char@
has now 31 bits. Only literals are handled here. -- Qrczak

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
buildDynCon binder cc con [arg_amode]
  | maybeIntLikeCon con && in_range_int_lit arg_amode
  = returnFC (stableAmodeIdInfo binder (CIntLike arg_amode) (mkConLFInfo con))
  where
    in_range_int_lit (CLit (MachInt val)) = val <= mAX_INTLIKE && val >= mIN_INTLIKE
    in_range_int_lit _other_amode	  = False

buildDynCon binder cc con [arg_amode]
  | maybeCharLikeCon con && in_range_char_lit arg_amode
  = returnFC (stableAmodeIdInfo binder (CCharLike arg_amode) (mkConLFInfo con))
  where
    in_range_char_lit (CLit (MachChar val)) = val <= mAX_CHARLIKE && val >= mIN_CHARLIKE
    in_range_char_lit _other_amode	    = False
\end{code}

Now the general case.

\begin{code}
buildDynCon binder ccs con args
  = allocDynClosure closure_info use_cc blame_cc amodes_w_offsets `thenFC` \ hp_off ->
    returnFC (heapIdInfo binder hp_off lf_info)
  where
    lf_info = mkConLFInfo con

    (closure_info, amodes_w_offsets) = layOutDynConstr con getAmodeRep args

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
     bind_arg (arg, offset) = bindNewToNode arg offset (mkLFArgument arg)
     (_, args_w_offsets)    = layOutDynConstr con idPrimRep args
\end{code}

Unboxed tuples are handled slightly differently - the object is
returned in registers and on the stack instead of the heap.

\begin{code}
bindUnboxedTupleComponents
	:: [Id]				-- Aargs
	-> FCode ([MagicId], 		-- Regs assigned
		  Int,			-- Number of pointer stack slots
		  Int,			-- Number of non-pointer stack slots
		  VirtualSpOffset)	-- Offset of return address slot
					-- (= realSP on entry)

bindUnboxedTupleComponents args
 =      -- Assign as many components as possible to registers
    let (arg_regs, _leftovers) = assignRegs [] (map idPrimRep args)
	(reg_args, stk_args)   = splitAtList arg_regs args

	-- separate the rest of the args into pointers and non-pointers
	(ptr_args, nptr_args) = 
	   partition (isFollowableRep . idPrimRep) stk_args
    in
  
    -- Allocate the rest on the stack
    -- The real SP points to the return address, above which any 
    -- leftover unboxed-tuple components will be allocated
    getVirtSp `thenFC` \ vsp ->
    getRealSp `thenFC` \ rsp ->
    let 
	(ptr_sp,  ptr_offsets)  = mkVirtStkOffsets rsp    idPrimRep ptr_args
	(nptr_sp, nptr_offsets) = mkVirtStkOffsets ptr_sp idPrimRep nptr_args
        ptrs  = ptr_sp - rsp
	nptrs = nptr_sp - ptr_sp
    in

    -- The stack pointer points to the last stack-allocated component
    setRealAndVirtualSp nptr_sp  		`thenC`

    -- We have just allocated slots starting at real SP + 1, and set the new
    -- virtual SP to the topmost allocated slot.  
    -- If the virtual SP started *below* the real SP, we've just jumped over
    -- some slots that won't be in the free-list, so put them there
    -- This commonly happens because we've freed the return-address slot
    -- (trimming back the virtual SP), but the real SP still points to that slot
    freeStackSlots [vsp+1,vsp+2 .. rsp]		`thenC`

    bindArgsToRegs reg_args arg_regs 		`thenC`
    mapCs bindNewToStack ptr_offsets 		`thenC`
    mapCs bindNewToStack nptr_offsets 		`thenC`

    returnFC (arg_regs, ptrs, nptrs, rsp)
\end{code}

%************************************************************************
%*									*
\subsubsection[CgRetConv-cgReturnDataCon]{Actually generate code for a constructor return}
%*									*
%************************************************************************


Note: it's the responsibility of the @cgReturnDataCon@ caller to be
sure the @amodes@ passed don't conflict with each other.
\begin{code}
cgReturnDataCon :: DataCon -> [CAddrMode] -> Code

cgReturnDataCon con amodes
  = ASSERT( amodes `lengthIs` dataConRepArity con )
    getEndOfBlockInfo	`thenFC` \ (EndOfBlockInfo args_sp sequel) ->

    case sequel of

      CaseAlts _ (Just (alts, Just (deflt_bndr, (_,deflt_lbl)))) False
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

		if isDeadBinder deflt_bndr
		then performReturn AbsCNop {- No reg assts -} jump_to_join_point
		else build_it_then jump_to_join_point
	where
	  is_elem = isIn "cgReturnDataCon"
	  jump_to_join_point sequel = absC (CJump (CLbl deflt_lbl CodePtrRep))
		-- Ignore the sequel: we've already looked at it above

      other_sequel	-- The usual case
	  | isUnboxedTupleCon con -> returnUnboxedTuple amodes
          | otherwise ->	     build_it_then (mkStaticAlgReturnCode con)

  where
    move_to_reg :: CAddrMode -> MagicId -> AbstractC
    move_to_reg src_amode dest_reg = CAssign (CReg dest_reg) src_amode

    build_it_then return =
		-- BUILD THE OBJECT IN THE HEAP
		-- The first "con" says that the name bound to this
		-- closure is "con", which is a bit of a fudge, but it only
		-- affects profiling

		-- This Id is also used to get a unique for a
		-- temporary variable, if the closure is a CHARLIKE.
		-- funnily enough, this makes the unique always come
		-- out as '54' :-)
	  buildDynCon (dataConWorkId con) currentCCS con amodes	`thenFC` \ idinfo ->
	  idInfoToAmode PtrRep idinfo				`thenFC` \ amode ->


		-- RETURN
	  profCtrC FSLIT("TICK_RET_NEW") [mkIntCLit (length amodes)] `thenC`
	  -- could use doTailCall here.
	  performReturn (move_to_reg amode node) return
\end{code}
