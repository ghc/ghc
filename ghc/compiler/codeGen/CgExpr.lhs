%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
% $Id: CgExpr.lhs,v 1.56 2003/07/02 13:19:28 simonpj Exp $
%
%********************************************************
%*							*
\section[CgExpr]{Converting @StgExpr@s}
%*							*
%********************************************************

\begin{code}
module CgExpr ( cgExpr ) where

#include "HsVersions.h"

import Constants	( mAX_SPEC_SELECTEE_SIZE, mAX_SPEC_AP_SIZE )
import StgSyn
import CgMonad
import AbsCSyn
import AbsCUtils	( mkAbstractCs, getAmodeRep )
import CLabel		( mkClosureTblLabel )

import SMRep		( fixedHdrSize )
import CoreSyn		( AltCon(..) )
import CgBindery	( getArgAmodes, getArgAmode, CgIdInfo, 
			  nukeDeadBindings, addBindC, addBindsC )
import CgCase		( cgCase, saveVolatileVarsAndRegs )
import CgClosure	( cgRhsClosure, cgStdRhsClosure )
import CgCon		( buildDynCon, cgReturnDataCon )
import CgLetNoEscape	( cgLetNoEscapeClosure )
import CgRetConv	( dataReturnConvPrim )
import CgTailCall	( cgTailCall, performReturn, performPrimReturn,
			  mkDynamicAlgReturnCode, mkPrimReturnCode,
			  tailCallPrimOp, ccallReturnUnboxedTuple
			)
import ClosureInfo	( mkClosureLFInfo, mkSelectorLFInfo,
			  mkApLFInfo, layOutDynConstr )
import CostCentre	( sccAbleCostCentre, isSccCountCostCentre )
import Id		( idPrimRep, Id )
import VarSet
import PrimOp		( primOpOutOfLine, getPrimOpResultInfo, 
			  PrimOp(..), PrimOpResultInfo(..) )
import TysPrim		( foreignObjPrimTyCon, arrayPrimTyCon, 
			  byteArrayPrimTyCon, mutableByteArrayPrimTyCon,
			  mutableArrayPrimTyCon )
import PrimRep		( PrimRep(..), isFollowableRep )
import TyCon		( isUnboxedTupleTyCon, isEnumerationTyCon )
import Type		( Type, typePrimRep, tyConAppArgs, tyConAppTyCon, repType )
import Maybes		( maybeToBool )
import ListSetOps	( assocMaybe )
import Unique		( mkBuiltinUnique )
import BasicTypes	( TopLevelFlag(..), RecFlag(..) )
import Util             ( lengthIs )
import Outputable
\end{code}

This module provides the support code for @StgToAbstractC@ to deal
with STG {\em expressions}.  See also @CgClosure@, which deals
with closures, and @CgCon@, which deals with constructors.

\begin{code}
cgExpr	:: StgExpr		-- input
	-> Code			-- output
\end{code}

%********************************************************
%*							*
%*		Tail calls				*
%*							*
%********************************************************

``Applications'' mean {\em tail calls}, a service provided by module
@CgTailCall@.  This includes literals, which show up as
@(STGApp (StgLitArg 42) [])@.

\begin{code}
cgExpr (StgApp fun args) = cgTailCall fun args
\end{code}

%********************************************************
%*							*
%*		STG ConApps  (for inline versions)	*
%*							*
%********************************************************

\begin{code}
cgExpr (StgConApp con args)
  = getArgAmodes args `thenFC` \ amodes ->
    cgReturnDataCon con amodes
\end{code}

Literals are similar to constructors; they return by putting
themselves in an appropriate register and returning to the address on
top of the stack.

\begin{code}
cgExpr (StgLit lit)
  = performPrimReturn (text "literal" <+> ppr lit) (CLit lit)
\end{code}


%********************************************************
%*							*
%*		STG PrimApps  (unboxed primitive ops)	*
%*							*
%********************************************************

Here is where we insert real live machine instructions.

NOTE about _ccall_GC_:

A _ccall_GC_ is treated as an out-of-line primop (returns True
for primOpOutOfLine) so that when we see the call in case context
	case (ccall ...) of { ... }
we get a proper stack frame on the stack when we perform it.  When we
get in a tail-call position, however, we need to actually perform the
call, so we treat it as an inline primop.

\begin{code}
cgExpr (StgOpApp op@(StgFCallOp _ _) args res_ty)
  = primRetUnboxedTuple op args res_ty

-- tagToEnum# is special: we need to pull the constructor out of the table,
-- and perform an appropriate return.

cgExpr (StgOpApp (StgPrimOp TagToEnumOp) [arg] res_ty) 
  = ASSERT(isEnumerationTyCon tycon)
    getArgAmode arg `thenFC` \amode ->
	-- save the tag in a temporary in case amode overlaps
	-- with node.
    absC (CAssign dyn_tag amode)	`thenC`
    performReturn (
		CAssign (CReg node) 
			(CVal (CIndex
		          (CLbl (mkClosureTblLabel tycon) PtrRep)
		          dyn_tag PtrRep) PtrRep))
	    (\ sequel -> mkDynamicAlgReturnCode tycon dyn_tag sequel)
   where
        dyn_tag = CTemp (mkBuiltinUnique 0) IntRep
		-- The '0' is just to get a random spare temp
	  --
	  -- if you're reading this code in the attempt to figure
	  -- out why the compiler panic'ed here, it is probably because
	  -- you used tagToEnum# in a non-monomorphic setting, e.g., 
	  --         intToTg :: Enum a => Int -> a ; intToTg (I# x#) = tagToEnum# x#
          --
	  -- That won't work.
          --
	tycon = tyConAppTyCon res_ty


cgExpr x@(StgOpApp op@(StgPrimOp primop) args res_ty)
  | primOpOutOfLine primop 
  = tailCallPrimOp primop args

  | otherwise
  = getArgAmodes args	`thenFC` \ arg_amodes ->

    case (getPrimOpResultInfo primop) of

	ReturnsPrim kind ->
	    let result_amode = CReg (dataReturnConvPrim kind) in
	    performReturn 
	      (COpStmt [result_amode] op arg_amodes [{-no vol_regs-}])
	      (mkPrimReturnCode (text "primapp)" <+> ppr x))
			  
	-- otherwise, must be returning an enumerated type (eg. Bool).
	-- we've only got the tag in R2, so we have to load the constructor
	-- itself into R1.

	ReturnsAlg tycon
	    | isUnboxedTupleTyCon tycon -> primRetUnboxedTuple op args res_ty

	    | isEnumerationTyCon  tycon ->
	     	performReturn
	      	     (COpStmt [dyn_tag] op arg_amodes [{-no vol_regs-}])
			  (\ sequel -> 
			  absC (CAssign (CReg node) closure_lbl) `thenC`
			  mkDynamicAlgReturnCode tycon dyn_tag sequel)

            where
	       -- Pull a unique out of thin air to put the tag in.  
	       -- It shouldn't matter if this overlaps with anything - we're
	       -- about to return anyway.
	       dyn_tag = CTemp (mkBuiltinUnique 0) IntRep

	       closure_lbl = CVal (CIndex
			       (CLbl (mkClosureTblLabel tycon) PtrRep)
			       dyn_tag PtrRep) PtrRep

\end{code}

%********************************************************
%*							*
%*		Case expressions			*
%*							*
%********************************************************
Case-expression conversion is complicated enough to have its own
module, @CgCase@.
\begin{code}

cgExpr (StgCase expr live_vars save_vars bndr srt alt_type alts)
  = cgCase expr live_vars save_vars bndr srt alt_type alts
\end{code}


%********************************************************
%*							*
%* 		Let and letrec				*
%*							*
%********************************************************
\subsection[let-and-letrec-codegen]{Converting @StgLet@ and @StgLetrec@}

\begin{code}
cgExpr (StgLet (StgNonRec name rhs) expr)
  = cgRhs name rhs	`thenFC` \ (name, info) ->
    addBindC name info 	`thenC`
    cgExpr expr

cgExpr (StgLet (StgRec pairs) expr)
  = fixC (\ new_bindings -> addBindsC new_bindings `thenC`
			    listFCs [ cgRhs b e | (b,e) <- pairs ]
    ) `thenFC` \ new_bindings ->

    addBindsC new_bindings `thenC`
    cgExpr expr
\end{code}

\begin{code}
cgExpr (StgLetNoEscape live_in_whole_let live_in_rhss bindings body)
  =    	-- Figure out what volatile variables to save
    nukeDeadBindings live_in_whole_let	`thenC`
    saveVolatileVarsAndRegs live_in_rhss
    	    `thenFC` \ (save_assts, rhs_eob_info, maybe_cc_slot) ->

	-- Save those variables right now!
    absC save_assts				`thenC`

	-- Produce code for the rhss
	-- and add suitable bindings to the environment
    cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot bindings `thenC`

	-- Do the body
    setEndOfBlockInfo rhs_eob_info (cgExpr body)
\end{code}


%********************************************************
%*							*
%*		SCC Expressions				*
%*							*
%********************************************************

SCC expressions are treated specially. They set the current cost
centre.
\begin{code}
cgExpr (StgSCC cc expr)
  = ASSERT(sccAbleCostCentre cc)
    costCentresC
	FSLIT("SET_CCC")
	[mkCCostCentre cc, mkIntCLit (if isSccCountCostCentre cc then 1 else 0)]
    `thenC`
    cgExpr expr
\end{code}

ToDo: counting of dict sccs ...

%********************************************************
%*							*
%*		Non-top-level bindings			*
%*							*
%********************************************************
\subsection[non-top-level-bindings]{Converting non-top-level bindings}

We rely on the support code in @CgCon@ (to do constructors) and
in @CgClosure@ (to do closures).

\begin{code}
cgRhs :: Id -> StgRhs -> FCode (Id, CgIdInfo)
	-- the Id is passed along so a binding can be set up

cgRhs name (StgRhsCon maybe_cc con args)
  = getArgAmodes args				`thenFC` \ amodes ->
    buildDynCon name maybe_cc con amodes	`thenFC` \ idinfo ->
    returnFC (name, idinfo)

cgRhs name (StgRhsClosure cc bi fvs upd_flag srt args body)
  = mkRhsClosure name cc bi srt fvs upd_flag args body
\end{code}

mkRhsClosure looks for two special forms of the right-hand side:
	a) selector thunks.
	b) AP thunks

If neither happens, it just calls mkClosureLFInfo.  You might think
that mkClosureLFInfo should do all this, but it seems wrong for the
latter to look at the structure of an expression

Selectors
~~~~~~~~~
We look at the body of the closure to see if it's a selector---turgid,
but nothing deep.  We are looking for a closure of {\em exactly} the
form:

...  = [the_fv] \ u [] ->
	 case the_fv of
	   con a_1 ... a_n -> a_i


\begin{code}
mkRhsClosure	bndr cc	bi srt
		[the_fv]   		-- Just one free var
		upd_flag		-- Updatable thunk
		[]			-- A thunk
		body@(StgCase (StgApp scrutinee [{-no args-}])
		      _ _ _ _   -- ignore uniq, etc.
		      (AlgAlt tycon)
		      [(DataAlt con, params, use_mask,
			    (StgApp selectee [{-no args-}]))])
  |  the_fv == scrutinee		-- Scrutinee is the only free variable
  && maybeToBool maybe_offset		-- Selectee is a component of the tuple
  && offset_into_int <= mAX_SPEC_SELECTEE_SIZE	-- Offset is small enough
  = -- NOT TRUE: ASSERT(is_single_constructor)
    -- The simplifier may have statically determined that the single alternative
    -- is the only possible case and eliminated the others, even if there are
    -- other constructors in the datatype.  It's still ok to make a selector
    -- thunk in this case, because we *know* which constructor the scrutinee
    -- will evaluate to.
    cgStdRhsClosure bndr cc bi [the_fv] [] body lf_info [StgVarArg the_fv]
  where
    lf_info 		  = mkSelectorLFInfo bndr offset_into_int (isUpdatable upd_flag)
    (_, params_w_offsets) = layOutDynConstr con idPrimRep params
				-- Just want the layout
    maybe_offset	  = assocMaybe params_w_offsets selectee
    Just the_offset 	  = maybe_offset
    offset_into_int       = the_offset - fixedHdrSize
\end{code}

Ap thunks
~~~~~~~~~

A more generic AP thunk of the form

	x = [ x_1...x_n ] \.. [] -> x_1 ... x_n

A set of these is compiled statically into the RTS, so we just use
those.  We could extend the idea to thunks where some of the x_i are
global ids (and hence not free variables), but this would entail
generating a larger thunk.  It might be an option for non-optimising
compilation, though.

We only generate an Ap thunk if all the free variables are pointers,
for semi-obvious reasons.

\begin{code}
mkRhsClosure 	bndr cc bi srt
		fvs
		upd_flag
		[]			-- No args; a thunk
		body@(StgApp fun_id args)

  | args `lengthIs` (arity-1)
 	&& all isFollowableRep (map idPrimRep fvs) 
 	&& isUpdatable upd_flag
 	&& arity <= mAX_SPEC_AP_SIZE 

 		   -- Ha! an Ap thunk
	= cgStdRhsClosure bndr cc bi fvs [] body lf_info payload

   where
	lf_info = mkApLFInfo bndr upd_flag arity
	-- the payload has to be in the correct order, hence we can't
 	-- just use the fvs.
	payload    = StgVarArg fun_id : args
	arity 	   = length fvs
\end{code}

The default case
~~~~~~~~~~~~~~~~
\begin{code}
mkRhsClosure bndr cc bi srt fvs upd_flag args body
  = cgRhsClosure bndr cc bi srt fvs args body lf_info
  where
    lf_info = mkClosureLFInfo bndr NotTopLevel fvs upd_flag args
\end{code}


%********************************************************
%*							*
%*		Let-no-escape bindings
%*							*
%********************************************************
\begin{code}
cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot 
	(StgNonRec binder rhs)
  = cgLetNoEscapeRhs live_in_rhss rhs_eob_info maybe_cc_slot 	
		     NonRecursive binder rhs 
    	    	    	    	`thenFC` \ (binder, info) ->
    addBindC binder info

cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot (StgRec pairs)
  = fixC (\ new_bindings ->
		addBindsC new_bindings 	`thenC`
		listFCs [ cgLetNoEscapeRhs full_live_in_rhss 
				rhs_eob_info maybe_cc_slot Recursive b e 
			| (b,e) <- pairs ]
    ) `thenFC` \ new_bindings ->

    addBindsC new_bindings
  where
    -- We add the binders to the live-in-rhss set so that we don't
    -- delete the bindings for the binder from the environment!
    full_live_in_rhss = live_in_rhss `unionVarSet` (mkVarSet [b | (b,r) <- pairs])

cgLetNoEscapeRhs
    :: StgLiveVars	-- Live in rhss
    -> EndOfBlockInfo
    -> Maybe VirtualSpOffset
    -> RecFlag
    -> Id
    -> StgRhs
    -> FCode (Id, CgIdInfo)

cgLetNoEscapeRhs full_live_in_rhss rhs_eob_info maybe_cc_slot rec binder
		 (StgRhsClosure cc bi _ upd_flag srt args body)
  = -- We could check the update flag, but currently we don't switch it off
    -- for let-no-escaped things, so we omit the check too!
    -- case upd_flag of
    --     Updatable -> panic "cgLetNoEscapeRhs"	-- Nothing to update!
    --     other     -> cgLetNoEscapeClosure binder cc bi live_in_whole_let live_in_rhss args body
    cgLetNoEscapeClosure binder cc bi srt full_live_in_rhss rhs_eob_info
	maybe_cc_slot rec args body

-- For a constructor RHS we want to generate a single chunk of code which
-- can be jumped to from many places, which will return the constructor.
-- It's easy; just behave as if it was an StgRhsClosure with a ConApp inside!
cgLetNoEscapeRhs full_live_in_rhss rhs_eob_info maybe_cc_slot rec binder
    	    	 (StgRhsCon cc con args)
  = cgLetNoEscapeClosure binder cc noBinderInfo{-safe-} NoSRT
			 full_live_in_rhss rhs_eob_info maybe_cc_slot rec
	[] 	--No args; the binder is data structure, not a function
	(StgConApp con args)
\end{code}

Little helper for primitives that return unboxed tuples.


\begin{code}
primRetUnboxedTuple :: StgOp -> [StgArg] -> Type -> Code
primRetUnboxedTuple op args res_ty
  = getArgAmodes args	    `thenFC` \ arg_amodes1 ->
    {-
      For a foreign call, we might need to fiddle with some of the args:
      for example, when passing a ByteArray#, we pass a ptr to the goods
      rather than the heap object.
    -}
    let 
	arg_amodes
	  | StgFCallOp{} <- op = zipWith shimFCallArg args arg_amodes1
	  | otherwise          = arg_amodes1
    in
    {-
      put all the arguments in temporaries so they don't get stomped when
      we push the return address.
    -}
    let
      n_args		  = length args
      arg_uniqs	          = map mkBuiltinUnique [0 .. n_args-1]
      arg_reps		  = map getAmodeRep arg_amodes
      arg_temps		  = zipWith CTemp arg_uniqs arg_reps
    in
    absC (mkAbstractCs (zipWith CAssign arg_temps arg_amodes)) `thenC`
    {-
      allocate some temporaries for the return values.
    -}
    let
      ty_args     = tyConAppArgs (repType res_ty)
      prim_reps   = map typePrimRep ty_args
      temp_uniqs  = map mkBuiltinUnique [ n_args .. n_args + length ty_args - 1]
      temp_amodes = zipWith CTemp temp_uniqs prim_reps
    in
    ccallReturnUnboxedTuple temp_amodes 	
	(absC (COpStmt temp_amodes op arg_temps []))


shimFCallArg arg amode
  | tycon == foreignObjPrimTyCon
	= CMacroExpr AddrRep ForeignObj_CLOSURE_DATA [amode]
  | tycon == arrayPrimTyCon || tycon == mutableArrayPrimTyCon
	= CMacroExpr PtrRep PTRS_ARR_CTS [amode]
  | tycon == byteArrayPrimTyCon || tycon == mutableByteArrayPrimTyCon
	= CMacroExpr AddrRep BYTE_ARR_CTS [amode]
  | otherwise = amode
  where	
	-- should be a tycon app, since this is a foreign call
	tycon = tyConAppTyCon (repType (stgArgType arg))
\end{code}
