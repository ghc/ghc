%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[TcPat]{Typechecking patterns}

\begin{code}
#include "HsVersions.h"

module TcPat (
	tcPat
#ifdef DPH
	, tcPats
#endif
    ) where

import TcMonad		-- typechecking monad machinery
import TcMonadFns	( newOpenTyVarTy, newPolyTyVarTy,
			  newPolyTyVarTys, copyTyVars, newMethod,
			  newOverloadedLit
			)
import AbsSyn		-- the stuff being typechecked

import AbsPrel		( charPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, charTy, stringTy, mkListTy,
			  mkTupleTy, addrTy, addrPrimTy, --OLD: eqStringId
			  PrimOp
			  IF_ATTACK_PRAGMAS(COMMA tagOf_PrimOp)
			  IF_ATTACK_PRAGMAS(COMMA pprPrimOp)
#ifdef DPH
			  ,mkProcessorTy, toDomainId
#endif {- Data Parallel Haskell -}
			)
import AbsUniType	( instantiateTauTy, applyTyCon, InstTyEnv(..)
			  IF_ATTACK_PRAGMAS(COMMA instantiateTy)
			)
import CmdLineOpts	( GlobalSwitch(..) )
import Id		( mkInstId, getIdUniType, getDataConSig,
			  getInstantiatedDataConSig, Id, DataCon(..)
			)
import Inst
import E		( lookupE_Binder, lookupE_Value,
			  lookupE_ClassOpByKey, E,
			  LVE(..), TCE(..), UniqFM, CE(..)
			-- TCE and CE for pragmas only
			)
import Errors		( dataConArityErr, Error(..), UnifyErrContext(..)
			)
import LIE		( nullLIE, plusLIE, mkLIE, LIE )
import Unify
import Unique		-- some ClassKey stuff
import Util

#ifdef DPH
import TcParQuals
#endif {- Data Parallel Haskell -}
\end{code}

The E passed in already contains bindings for all the variables in
the pattern, usually to fresh type variables (but maybe not, if there
were type signatures present).

\begin{code}
tcPat :: E -> RenamedPat -> TcM (TypecheckedPat, LIE, UniType)
\end{code}

%************************************************************************
%*									*
\subsection{Variables, wildcards, lazy pats, as-pats}
%*									*
%************************************************************************

\begin{code}
tcPat e (VarPatIn name)
  = let
	id = lookupE_Binder e name
    in
    returnTc (VarPat id, nullLIE, getIdUniType id)

tcPat e (LazyPatIn pat)
  = tcPat e pat		`thenTc` \ (pat', lie, ty) ->
    returnTc (LazyPat pat', lie, ty)

tcPat e pat_in@(AsPatIn name pat)
  = let
	id = lookupE_Binder e name
    in
    tcPat e pat				`thenTc` \ (pat', lie, ty) ->
    unifyTauTy (getIdUniType id) ty (PatCtxt pat_in) `thenTc_`
    returnTc (AsPat id pat', lie, ty)

tcPat e (WildPatIn)
  = newOpenTyVarTy    `thenNF_Tc` \ tyvar_ty ->
    returnTc (WildPat tyvar_ty, nullLIE, tyvar_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Explicit lists and tuples}
%*									*
%************************************************************************

\begin{code}
tcPat e pat_in@(ListPatIn pats)
  = tcPats e pats	`thenTc`    \ (pats', lie, tys) ->
    newPolyTyVarTy	`thenNF_Tc` \ tyvar_ty ->

    unifyTauTyList (tyvar_ty:tys) (PatCtxt pat_in) `thenTc_`

    returnTc (ListPat tyvar_ty pats', lie, mkListTy tyvar_ty)

tcPat e pat_in@(TuplePatIn pats)
  = let
	arity = length pats
    in
    tcPats e pats   `thenTc` \ (pats', lie, tys) ->

	-- We have to unify with fresh polymorphic type variables, to
	-- make sure we record that the tuples can only contain boxed
	-- types.
    newPolyTyVarTys arity   `thenNF_Tc` \ tyvar_tys ->

    unifyTauTyLists tyvar_tys tys (PatCtxt pat_in) `thenTc_`

	-- possibly do the "make all tuple-pats irrefutable" test:
    getSwitchCheckerTc	`thenNF_Tc` \ sw_chkr ->
    let
	unmangled_result = TuplePat pats'

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.
	possibly_mangled_result
	  = if sw_chkr IrrefutableTuples
	    then LazyPat unmangled_result
	    else unmangled_result

	-- ToDo: IrrefutableEverything
    in
    returnTc (possibly_mangled_result, lie, mkTupleTy arity tys)
\end{code}

%************************************************************************
%*									*
\subsection{Other constructors}
%*									*
%************************************************************************

Constructor patterns are a little fun:
\begin{itemize}
\item
typecheck the arguments
\item
look up the constructor
\item
specialise its type (ignore the translation this produces)
\item
check that the context produced by this specialisation is empty
\item
get the arguments out of the function type produced from specialising
\item
unify them with the types of the patterns
\item
back substitute with the type of the result of the constructor
\end{itemize}

ToDo: exploit new representation of constructors to make this more
efficient?

\begin{code}
tcPat e pat_in@(ConPatIn name pats)
  = let
	con_id = lookupE_Value e name
    in
    tcPats e pats `thenTc` \ (pats', lie, tys) ->

    matchConArgTys con_id tys (\ ty -> PatCtxt pat_in) `thenTc` \ data_ty ->

    returnTc (ConPat con_id data_ty pats', lie, data_ty)

tcPat e pat_in@(ConOpPatIn pat1 op pat2) -- & in binary-op form...
  = let
	con_id = lookupE_Value e op
    in
    tcPats e [pat1, pat2]   `thenTc`	\ ([pat1',pat2'], lie, tys) ->
	 -- ToDo: there exists a less ugly way, no doubt...

    matchConArgTys con_id tys (\ ty -> PatCtxt pat_in) `thenTc` \ data_ty ->

    returnTc (ConOpPat pat1' con_id pat2' data_ty, lie, data_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Non-overloaded literals}
%*									*
%************************************************************************

\begin{code}
tcPat e (LitPatIn lit@(CharLit str))
  = returnTc (LitPat lit charTy, nullLIE, charTy)

tcPat e (LitPatIn lit@(StringLit str))
  = getSrcLocTc				`thenNF_Tc` \ loc ->
    let
	origin = LiteralOrigin lit loc
	eq_id  = lookupE_ClassOpByKey e eqClassKey  SLIT("==")
    in
    newMethod origin eq_id [stringTy]	`thenNF_Tc` \ eq ->
    let
	comp_op = App (Var (mkInstId eq)) (Lit lit)
    in
    returnTc (NPat lit stringTy comp_op, mkLIE [eq], stringTy)

{- OLD:
tcPat e (LitPatIn lit@(StringLit str))
  = returnTc (NPat lit stringTy comp_op, nullLIE, stringTy)
  where
    comp_op   = App (Var eqStringId) (Lit lit)
-}

tcPat e (LitPatIn lit@(IntPrimLit _))
  = returnTc (LitPat lit intPrimTy, nullLIE, intPrimTy)
tcPat e (LitPatIn lit@(CharPrimLit _))
  = returnTc (LitPat lit charPrimTy, nullLIE, charPrimTy)
tcPat e (LitPatIn lit@(StringPrimLit _))
  = returnTc (LitPat lit addrPrimTy, nullLIE, addrPrimTy)
tcPat e (LitPatIn lit@(FloatPrimLit _))
  = returnTc (LitPat lit floatPrimTy, nullLIE, floatPrimTy)
tcPat e (LitPatIn lit@(DoublePrimLit _))
  = returnTc (LitPat lit doublePrimTy, nullLIE, doublePrimTy)
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded patterns: int literals and \tr{n+k} patterns}
%*									*
%************************************************************************

\begin{code}
tcPat e (LitPatIn lit@(IntLit i))
  = getSrcLocTc				`thenNF_Tc` \ loc ->
    let
	origin = LiteralOrigin lit loc
    in
    newPolyTyVarTy			`thenNF_Tc` \ tyvar_ty ->
    let
	from_int     = lookupE_ClassOpByKey e numClassKey SLIT("fromInt")
	from_integer = lookupE_ClassOpByKey e numClassKey SLIT("fromInteger")
	eq_id	     = lookupE_ClassOpByKey e eqClassKey  SLIT("==")
    in
    newOverloadedLit origin
		     (OverloadedIntegral i from_int from_integer)
		     tyvar_ty		`thenNF_Tc` \ over_lit ->

    newMethod origin eq_id [tyvar_ty]	`thenNF_Tc` \ eq ->

    returnTc (NPat lit tyvar_ty (App (Var (mkInstId eq))
				     (Var (mkInstId over_lit))),
	      mkLIE [over_lit, eq],
	      tyvar_ty)

tcPat e (LitPatIn lit@(FracLit f))
  = getSrcLocTc				`thenNF_Tc` \ loc ->
    let
	origin = LiteralOrigin lit loc
    in
    newPolyTyVarTy			`thenNF_Tc` \ tyvar_ty ->
    let
	eq_id	      = lookupE_ClassOpByKey e eqClassKey   	  SLIT("==")
	from_rational = lookupE_ClassOpByKey e fractionalClassKey SLIT("fromRational")
    in
    newOverloadedLit origin
		     (OverloadedFractional f from_rational)
		     tyvar_ty		`thenNF_Tc` \ over_lit ->

    newMethod origin eq_id [tyvar_ty]	`thenNF_Tc` \ eq ->

    returnTc (NPat lit tyvar_ty (App (Var (mkInstId eq))
			             (Var (mkInstId over_lit))),
	      mkLIE [over_lit, eq],
	      tyvar_ty)

tcPat e (LitPatIn lit@(LitLitLitIn s))
  = error "tcPat: can't handle ``literal-literal'' patterns"
{-
  = getSrcLocTc				`thenNF_Tc` \ loc ->
    let
	origin = LiteralOrigin lit loc
    in
    newPolyTyVarTy			`thenNF_Tc` \ tyvar_ty ->
    let
	eq_id = lookupE_ClassOpByKey e eqClassKey "=="
    in
    newOverloadedLit origin
		     (OverloadedLitLit s)
		     tyvar_ty		`thenNF_Tc` \ over_lit ->

    newMethod origin eq_id [tyvar_ty]	`thenNF_Tc` \ eq ->

    returnTc (NPat lit tyvar_ty (App (Var (mkInstId eq))
			             (Var (mkInstId over_lit))),
	      mkLIE [over_lit, eq],
	      tyvar_ty)
-}

tcPat e (NPlusKPatIn name lit@(IntLit k))
  = getSrcLocTc				`thenNF_Tc` \ loc ->
    let
	origin 	 = LiteralOrigin lit loc

	local	 = lookupE_Binder e name
	local_ty = getIdUniType local

	ge_id        = lookupE_ClassOpByKey e ordClassKey SLIT(">=")
	minus_id     = lookupE_ClassOpByKey e numClassKey SLIT("-")
	from_int     = lookupE_ClassOpByKey e numClassKey SLIT("fromInt")
	from_integer = lookupE_ClassOpByKey e numClassKey SLIT("fromInteger")
    in
    newOverloadedLit origin
		     (OverloadedIntegral k from_int from_integer)
		     local_ty		   `thenNF_Tc` \ over_lit ->

    newMethod origin ge_id  	[local_ty] `thenNF_Tc` \ ge ->
    newMethod origin minus_id	[local_ty] `thenNF_Tc` \ minus ->

    returnTc (NPlusKPat local lit local_ty
			(Var (mkInstId over_lit))
			(Var (mkInstId ge))
			(Var (mkInstId minus)),
	      mkLIE [over_lit, ge, minus],
	      local_ty)

tcPat e (NPlusKPatIn pat other) = panic "TcPat:NPlusKPat: not an IntLit"

#ifdef DPH
tcPat e (ProcessorPatIn pats pat)
  = tcPidPats e pats		`thenTc` \ (pats',convs, lie, tys)->
    tcPat e pat 		`thenTc` \ (pat', ty, lie') ->
    returnTc (ProcessorPat pats' convs pat',
	      plusLIE lie lie',
	      mkProcessorTy tys ty)
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection{Lists of patterns}
%*									*
%************************************************************************

\begin{code}
tcPats :: E -> [RenamedPat] -> TcM ([TypecheckedPat], LIE, [UniType])

tcPats e [] = returnTc ([], nullLIE, [])

tcPats e (pat:pats)
  = tcPat e pat			`thenTc` \ (pat',  lie,  ty)  ->
    tcPats e pats		`thenTc` \ (pats', lie', tys) ->

    returnTc (pat':pats', plusLIE lie lie', ty:tys)
\end{code}

@matchConArgTys@ grabs the signature of the data constructor, and
unifies the actual args against the expected ones.

\begin{code}
matchConArgTys :: Id -> [UniType] -> (UniType -> UnifyErrContext) -> TcM UniType

matchConArgTys con_id arg_tys err_ctxt
  = let
	no_of_args = length arg_tys
	(sig_tyvars, sig_theta, sig_tys, _) = getDataConSig con_id
	     -- Ignore the sig_theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
	con_arity  = length sig_tys
    in
    getSrcLocTc				`thenNF_Tc` \ loc ->
    checkTc (con_arity /= no_of_args) 
	    (dataConArityErr con_id con_arity no_of_args loc) `thenTc_`

    copyTyVars sig_tyvars	      	`thenNF_Tc` \ (inst_env, _, new_tyvar_tys) ->
    let 
	(_,inst_arg_tys,inst_result_ty) = getInstantiatedDataConSig con_id new_tyvar_tys
    in
    unifyTauTyLists arg_tys inst_arg_tys (err_ctxt inst_result_ty)  `thenTc_`
    returnTc inst_result_ty
\end{code}
