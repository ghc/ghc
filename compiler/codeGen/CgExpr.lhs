%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

\begin{code}
module CgExpr ( cgExpr ) where

#include "HsVersions.h"

import Constants
import StgSyn
import CgMonad

import SMRep
import CoreSyn
import CgProf
import CgHeapery
import CgBindery
import CgCase
import CgClosure
import CgCon
import CgLetNoEscape
import CgCallConv
import CgTailCall
import CgInfoTbls
import CgForeignCall
import CgPrimOp
import CgHpc
import CgUtils
import ClosureInfo
import Cmm
import MachOp
import VarSet
import Literal
import PrimOp
import Id
import TyCon
import Type
import Maybes
import ListSetOps
import BasicTypes
import Util
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
  = do	{ amodes <- getArgAmodes args
	; cgReturnDataCon con amodes }
\end{code}

Literals are similar to constructors; they return by putting
themselves in an appropriate register and returning to the address on
top of the stack.

\begin{code}
cgExpr (StgLit lit)
  = do  { cmm_lit <- cgLit lit
	; performPrimReturn rep (CmmLit cmm_lit) }
  where
    rep = (typeCgRep) (literalType lit)
\end{code}


%********************************************************
%*							*
%* 	PrimOps and foreign calls.
%*							*
%********************************************************

NOTE about "safe" foreign calls: a safe foreign call is never compiled
inline in a case expression.  When we see

	case (ccall ...) of { ... }

We generate a proper return address for the alternatives and push the
stack frame before doing the call, so that in the event that the call
re-enters the RTS the stack is in a sane state.

\begin{code}
cgExpr (StgOpApp (StgFCallOp fcall _) stg_args res_ty) = do
    {-
	First, copy the args into temporaries.  We're going to push
	a return address right before doing the call, so the args
	must be out of the way.
    -}
    reps_n_amodes <- getArgAmodes stg_args
    let 
	-- Get the *non-void* args, and jiggle them with shimForeignCall
	arg_exprs = [ shimForeignCallArg stg_arg expr 
		    | (stg_arg, (rep,expr)) <- stg_args `zip` reps_n_amodes, 
		      nonVoidArg rep]

    arg_tmps <- mapM assignTemp arg_exprs
    let	arg_hints = zip arg_tmps (map (typeHint.stgArgType) stg_args)
    {-
	Now, allocate some result regs.
    -}
    (res_reps,res_regs,res_hints)  <- newUnboxedTupleRegs res_ty
    ccallReturnUnboxedTuple (zip res_reps (map CmmReg res_regs)) $
	emitForeignCall (zip res_regs res_hints) fcall 
	   arg_hints emptyVarSet{-no live vars-}
      
-- tagToEnum# is special: we need to pull the constructor out of the table,
-- and perform an appropriate return.

cgExpr (StgOpApp (StgPrimOp TagToEnumOp) [arg] res_ty) 
  = ASSERT(isEnumerationTyCon tycon)
    do	{ (_,amode) <- getArgAmode arg
	; amode' <- assignTemp amode	-- We're going to use it twice,
					-- so save in a temp if non-trivial
	; this_pkg <- getThisPackage
	; stmtC (CmmAssign nodeReg (tagToClosure this_pkg tycon amode'))
	; performReturn emitReturnInstr }
   where
	  -- If you're reading this code in the attempt to figure
	  -- out why the compiler panic'ed here, it is probably because
	  -- you used tagToEnum# in a non-monomorphic setting, e.g., 
	  --         intToTg :: Enum a => Int -> a ; intToTg (I# x#) = tagToEnum# x#
	  -- That won't work.
	tycon = tyConAppTyCon res_ty


cgExpr x@(StgOpApp op@(StgPrimOp primop) args res_ty)
  | primOpOutOfLine primop
	= tailCallPrimOp primop args

  | ReturnsPrim VoidRep <- result_info
	= do cgPrimOp [] primop args emptyVarSet
	     performReturn emitReturnInstr

  | ReturnsPrim rep <- result_info
	= do cgPrimOp [dataReturnConvPrim (primRepToCgRep rep)] 
			primop args emptyVarSet
	     performReturn emitReturnInstr

  | ReturnsAlg tycon <- result_info, isUnboxedTupleTyCon tycon
	= do (reps, regs, _hints) <- newUnboxedTupleRegs res_ty
	     cgPrimOp regs primop args emptyVarSet{-no live vars-}
	     returnUnboxedTuple (zip reps (map CmmReg regs))

  | ReturnsAlg tycon <- result_info, isEnumerationTyCon tycon
	-- c.f. cgExpr (...TagToEnumOp...)
	= do tag_reg <- newTemp wordRep
	     this_pkg <- getThisPackage
	     cgPrimOp [tag_reg] primop args emptyVarSet
	     stmtC (CmmAssign nodeReg (tagToClosure this_pkg tycon (CmmReg tag_reg)))
	     performReturn emitReturnInstr
  where
	result_info = getPrimOpResultInfo primop
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
  = do	{  	-- Figure out what volatile variables to save
	; nukeDeadBindings live_in_whole_let
	; (save_assts, rhs_eob_info, maybe_cc_slot) 
		<- saveVolatileVarsAndRegs live_in_rhss

	-- Save those variables right now!
	; emitStmts save_assts

	-- Produce code for the rhss
	-- and add suitable bindings to the environment
	; cgLetNoEscapeBindings live_in_rhss rhs_eob_info 
			 	maybe_cc_slot bindings

	-- Do the body
	; setEndOfBlockInfo rhs_eob_info (cgExpr body) }
\end{code}


%********************************************************
%*							*
%*		SCC Expressions				*
%*							*
%********************************************************

SCC expressions are treated specially. They set the current cost
centre.

\begin{code}
cgExpr (StgSCC cc expr) = do emitSetCCC cc; cgExpr expr
\end{code}

%********************************************************
%*                                                     *
%*             Hpc Tick Boxes                          *
%*                                                     *
%********************************************************

\begin{code}
cgExpr (StgTick m n expr) = do cgTickBox m n; cgExpr expr
\end{code}

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
  = do	{ amodes <- getArgAmodes args
	; idinfo <- buildDynCon name maybe_cc con amodes
	; returnFC (name, idinfo) }

cgRhs name (StgRhsClosure cc bi fvs upd_flag srt args body)
  = do this_pkg <- getThisPackage
       mkRhsClosure this_pkg name cc bi srt fvs upd_flag args body
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
mkRhsClosure	this_pkg bndr cc bi srt
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
    lf_info 		  = mkSelectorLFInfo bndr offset_into_int
				 (isUpdatable upd_flag)
    (_, params_w_offsets) = layOutDynConstr this_pkg con (addIdReps params)
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
mkRhsClosure 	this_pkg bndr cc bi srt
		fvs
		upd_flag
		[]			-- No args; a thunk
		body@(StgApp fun_id args)

  | args `lengthIs` (arity-1)
 	&& all isFollowableArg (map idCgRep fvs) 
 	&& isUpdatable upd_flag
 	&& arity <= mAX_SPEC_AP_SIZE 

 		   -- Ha! an Ap thunk
	= cgStdRhsClosure bndr cc bi fvs [] body lf_info payload

   where
	lf_info = mkApLFInfo bndr upd_flag arity
	-- the payload has to be in the correct order, hence we can't
 	-- just use the fvs.
	payload = StgVarArg fun_id : args
	arity 	= length fvs
\end{code}

The default case
~~~~~~~~~~~~~~~~
\begin{code}
mkRhsClosure this_pkg bndr cc bi srt fvs upd_flag args body
  = cgRhsClosure bndr cc bi srt fvs upd_flag args body
\end{code}


%********************************************************
%*							*
%*		Let-no-escape bindings
%*							*
%********************************************************
\begin{code}
cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot 
	(StgNonRec binder rhs)
  = do	{ (binder,info) <- cgLetNoEscapeRhs live_in_rhss rhs_eob_info 
					    maybe_cc_slot 	
					    NonRecursive binder rhs 
	; addBindC binder info }

cgLetNoEscapeBindings live_in_rhss rhs_eob_info maybe_cc_slot (StgRec pairs)
  = do	{ new_bindings <- fixC (\ new_bindings -> do
		{ addBindsC new_bindings
		; listFCs [ cgLetNoEscapeRhs full_live_in_rhss 
				rhs_eob_info maybe_cc_slot Recursive b e 
			  | (b,e) <- pairs ] })

	; addBindsC new_bindings }
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
newUnboxedTupleRegs :: Type -> FCode ([CgRep], [CmmReg], [MachHint])
newUnboxedTupleRegs res_ty =
   let
	ty_args = tyConAppArgs (repType res_ty)
	(reps,hints) = unzip [ (rep, typeHint ty) | ty <- ty_args, 
					   	    let rep = typeCgRep ty,
					 	    nonVoidArg rep ]
   in do
   regs <- mapM (newTemp . argMachRep) reps
   return (reps,regs,hints)
\end{code}
