%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[TcPat]{Typechecking patterns}

\begin{code}
#include "HsVersions.h"

module TcPat ( tcPat ) where

import Ubiq{-uitous-}

import HsSyn		( InPat(..), OutPat(..), HsExpr(..), HsLit(..),
			  Match, HsBinds, Qual, PolyType,
			  ArithSeqInfo, Stmt, Fake )
import RnHsSyn		( RenamedPat(..) )
import TcHsSyn		( TcPat(..), TcIdOcc(..) )

import TcMonad
import Inst		( Inst, OverloadedLit(..), InstOrigin(..),
			  emptyLIE, plusLIE, plusLIEs, LIE(..),
			  newMethod, newOverloadedLit
			)
import TcEnv		( tcLookupGlobalValue, tcLookupGlobalValueByKey, 
			  tcLookupLocalValueOK )
import TcType 		( TcType(..), TcMaybe, tcInstType, newTyVarTy, newTyVarTys, tcInstId )
import Unify 		( unifyTauTy, unifyTauTyList, unifyTauTyLists )

import Bag		( Bag )
import CmdLineOpts	( opt_IrrefutableTuples )
import Id		( GenId, idType )
import Kind		( Kind, mkBoxedTypeKind, mkTypeKind )
import Maybes		( maybeToBool )
import PprType		( GenType, GenTyVar )
import PrelInfo		( charPrimTy, intPrimTy, floatPrimTy,
			  doublePrimTy, charTy, stringTy, mkListTy,
			  mkTupleTy, addrTy, addrPrimTy )
import Pretty
import RnHsSyn		( RnName{-instance Outputable-} )
import Type		( splitFunTy, splitRhoTy, splitSigmaTy, mkTyVarTys,
			  getFunTy_maybe, maybeAppDataTyCon,
			  Type(..), GenType
			)
import TyVar		( GenTyVar )
import Unique		( Unique, eqClassOpKey )
import Util		( assertPanic, panic{-ToDo:rm-} )
\end{code}

\begin{code}
tcPat :: RenamedPat -> TcM s (TcPat s, LIE s, TcType s)
\end{code}

%************************************************************************
%*									*
\subsection{Variables, wildcards, lazy pats, as-pats}
%*									*
%************************************************************************

\begin{code}
tcPat (VarPatIn name)
  = tcLookupLocalValueOK "tcPat1" name	`thenNF_Tc` \ id ->
    returnTc (VarPat (TcId id), emptyLIE, idType id)

tcPat (LazyPatIn pat)
  = tcPat pat		`thenTc` \ (pat', lie, ty) ->
    returnTc (LazyPat pat', lie, ty)

tcPat pat_in@(AsPatIn name pat)
  = tcLookupLocalValueOK "tcPat2"  name	`thenNF_Tc` \ id ->
    tcPat pat				`thenTc` \ (pat', lie, ty) ->
    tcAddErrCtxt (patCtxt pat_in) 	$
    unifyTauTy (idType id) ty		`thenTc_`
    returnTc (AsPat (TcId id) pat', lie, ty)

tcPat WildPatIn
  = newTyVarTy mkTypeKind	`thenNF_Tc` \ tyvar_ty ->
    returnTc (WildPat tyvar_ty, emptyLIE, tyvar_ty)

tcPat (NegPatIn pat)
  = tcPat (negate_lit pat)
  where
    negate_lit (LitPatIn (HsInt  i)) = LitPatIn (HsInt  (-i))
    negate_lit (LitPatIn (HsFrac f)) = LitPatIn (HsFrac (-f))
    negate_lit _                     = panic "TcPat:negate_pat"

tcPat (ParPatIn parend_pat)
  = tcPat parend_pat
\end{code}

%************************************************************************
%*									*
\subsection{Explicit lists and tuples}
%*									*
%************************************************************************

\begin{code}
tcPat pat_in@(ListPatIn pats)
  = tcPats pats				`thenTc`    \ (pats', lie, tys) ->
    newTyVarTy mkBoxedTypeKind		`thenNF_Tc` \ tyvar_ty ->
    tcAddErrCtxt (patCtxt pat_in)	$
    unifyTauTyList (tyvar_ty:tys)	`thenTc_`

    returnTc (ListPat tyvar_ty pats', lie, mkListTy tyvar_ty)

tcPat pat_in@(TuplePatIn pats)
  = let
	arity = length pats
    in
    tcPats pats   			`thenTc` \ (pats', lie, tys) ->

	-- Make sure we record that the tuples can only contain boxed types
    newTyVarTys arity mkBoxedTypeKind  	`thenNF_Tc` \ tyvar_tys ->

    tcAddErrCtxt (patCtxt pat_in)	$
    unifyTauTyLists tyvar_tys tys	`thenTc_`

	-- possibly do the "make all tuple-pats irrefutable" test:
    let
	unmangled_result = TuplePat pats'

	-- Under flag control turn a pattern (x,y,z) into ~(x,y,z)
	-- so that we can experiment with lazy tuple-matching.
	-- This is a pretty odd place to make the switch, but
	-- it was easy to do.

	possibly_mangled_result
	  = if opt_IrrefutableTuples
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
tcPat pat_in@(ConPatIn name pats)
  = tcPats pats				`thenTc` \ (pats', lie, tys) ->

    tcAddErrCtxt (patCtxt pat_in)	$
    matchConArgTys name tys 		`thenTc` \ (con_id, data_ty) ->

    returnTc (ConPat con_id data_ty pats', 
	      lie, 
	      data_ty)

tcPat pat_in@(ConOpPatIn pat1 op pat2) 	-- in binary-op form...
  = tcPat pat1				`thenTc` \ (pat1', lie1, ty1) ->
    tcPat pat2				`thenTc` \ (pat2', lie2, ty2) ->

    tcAddErrCtxt (patCtxt pat_in)	$
    matchConArgTys op [ty1,ty2]	`thenTc` \ (con_id, data_ty) ->

    returnTc (ConOpPat pat1' con_id pat2' data_ty, 
	      lie1 `plusLIE` lie2, 
	      data_ty)
\end{code}

%************************************************************************
%*									*
\subsection{Records}
%*									*
%************************************************************************

\begin{code}
tcPat pat_in@(RecPatIn name rpats)
  = tcLookupGlobalValue name		`thenNF_Tc` \ con_id ->
    tcInstId con_id			`thenNF_Tc` \ (_, _, con_tau) ->
    let
	     -- Ignore the con_theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
	(_, record_ty) = splitFunTy con_tau
    in
	-- Con is syntactically constrained to be a data constructor
    ASSERT( maybeToBool (maybeAppDataTyCon record_ty) )

    mapAndUnzipTc (do_bind record_ty) rpats	`thenTc` \ (rpats', lies) ->

    returnTc (RecPat con_id record_ty rpats', 
	      plusLIEs lies, 
	      record_ty)

  where
    do_bind expected_record_ty (field_label, rhs_pat, pun_flag)
      = tcLookupGlobalValue field_label		`thenNF_Tc` \ sel_id ->
	tcInstId sel_id				`thenNF_Tc` \ (_, _, tau) ->

		-- Record selectors all have type
		-- 	forall a1..an.  T a1 .. an -> tau
	ASSERT( maybeToBool (getFunTy_maybe tau) )
	let
		-- Selector must have type RecordType -> FieldType
	  Just (record_ty, field_ty) = getFunTy_maybe tau
	in
	tcAddErrCtxt (recordLabel field_label) (
	  unifyTauTy expected_record_ty record_ty
	)						`thenTc_`
	tcPat rhs_pat					`thenTc` \ (rhs_pat', lie, rhs_ty) ->
	tcAddErrCtxt (recordRhs field_label rhs_pat) (
	  unifyTauTy field_ty rhs_ty
	)			 			`thenTc_`
	returnTc ((sel_id, rhs_pat', pun_flag), lie)
\end{code}

%************************************************************************
%*									*
\subsection{Non-overloaded literals}
%*									*
%************************************************************************

\begin{code}
tcPat (LitPatIn lit@(HsChar str))
  = returnTc (LitPat lit charTy, emptyLIE, charTy)

tcPat (LitPatIn lit@(HsString str))
  = tcLookupGlobalValueByKey eqClassOpKey	`thenNF_Tc` \ sel_id ->
    newMethod (LiteralOrigin lit) 
	      (RealId sel_id) [stringTy]	`thenNF_Tc` \ (lie, eq_id) ->
    let
	comp_op = HsApp (HsVar eq_id) (HsLitOut lit stringTy)
    in
    returnTc (NPat lit stringTy comp_op, lie, stringTy)

tcPat (LitPatIn lit@(HsIntPrim _))
  = returnTc (LitPat lit intPrimTy, emptyLIE, intPrimTy)
tcPat (LitPatIn lit@(HsCharPrim _))
  = returnTc (LitPat lit charPrimTy, emptyLIE, charPrimTy)
tcPat (LitPatIn lit@(HsStringPrim _))
  = returnTc (LitPat lit addrPrimTy, emptyLIE, addrPrimTy)
tcPat (LitPatIn lit@(HsFloatPrim _))
  = returnTc (LitPat lit floatPrimTy, emptyLIE, floatPrimTy)
tcPat (LitPatIn lit@(HsDoublePrim _))
  = returnTc (LitPat lit doublePrimTy, emptyLIE, doublePrimTy)
\end{code}

%************************************************************************
%*									*
\subsection{Overloaded patterns: int literals and \tr{n+k} patterns}
%*									*
%************************************************************************

\begin{code}
tcPat (LitPatIn lit@(HsInt i))
  = newTyVarTy mkBoxedTypeKind				`thenNF_Tc` \ tyvar_ty ->
    newOverloadedLit origin  
		     (OverloadedIntegral i) tyvar_ty	`thenNF_Tc` \ (lie1, over_lit_id) ->

    tcLookupGlobalValueByKey eqClassOpKey		`thenNF_Tc` \ eq_sel_id ->
    newMethod origin (RealId eq_sel_id) [tyvar_ty]	`thenNF_Tc` \ (lie2, eq_id) ->

    returnTc (NPat lit tyvar_ty (HsApp (HsVar eq_id)
				       (HsVar over_lit_id)),
	      lie1 `plusLIE` lie2,
	      tyvar_ty)
  where
    origin = LiteralOrigin lit

tcPat (LitPatIn lit@(HsFrac f))
  = newTyVarTy mkBoxedTypeKind				`thenNF_Tc` \ tyvar_ty ->
    newOverloadedLit origin
		     (OverloadedFractional f) tyvar_ty	`thenNF_Tc` \ (lie1, over_lit_id) ->

    tcLookupGlobalValueByKey eqClassOpKey		`thenNF_Tc` \ eq_sel_id ->
    newMethod origin (RealId eq_sel_id) [tyvar_ty]	`thenNF_Tc` \ (lie2, eq_id) ->

    returnTc (NPat lit tyvar_ty (HsApp (HsVar eq_id)
				       (HsVar over_lit_id)),
	      lie1 `plusLIE` lie2,
	      tyvar_ty)
  where
    origin = LiteralOrigin lit

tcPat (LitPatIn lit@(HsLitLit s))
  = error "tcPat: can't handle ``literal-literal'' patterns"
\end{code}

%************************************************************************
%*									*
\subsection{Lists of patterns}
%*									*
%************************************************************************

\begin{code}
tcPats :: [RenamedPat] -> TcM s ([TcPat s], LIE s, [TcType s])

tcPats [] = returnTc ([], emptyLIE, [])

tcPats (pat:pats)
  = tcPat pat		`thenTc` \ (pat',  lie,  ty)  ->
    tcPats pats		`thenTc` \ (pats', lie', tys) ->

    returnTc (pat':pats', plusLIE lie lie', ty:tys)
\end{code}

@matchConArgTys@ grabs the signature of the data constructor, and
unifies the actual args against the expected ones.

\begin{code}
matchConArgTys :: RnName -> [TcType s] -> TcM s (Id, TcType s)

matchConArgTys con arg_tys
  = tcLookupGlobalValue con		`thenNF_Tc` \ con_id ->
    tcInstId con_id			`thenNF_Tc` \ (_, _, con_tau) ->
	     -- Ignore the con_theta; overloaded constructors only
	     -- behave differently when called, not when used for
	     -- matching.
    let
	(con_args, con_result) = splitFunTy con_tau
	con_arity  = length con_args
	no_of_args = length arg_tys
    in
    checkTc (con_arity == no_of_args)
	    (arityErr "Constructor" con_id con_arity no_of_args)	`thenTc_`

    unifyTauTyLists arg_tys con_args	 				`thenTc_`
    returnTc (con_id, con_result)
\end{code}


% =================================================

Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
patCtxt pat sty = ppHang (ppStr "In the pattern:") 4 (ppr sty pat)

recordLabel field_label sty
  = ppHang (ppBesides [ppStr "When matching record field", ppr sty field_label])
	 4 (ppBesides [ppStr "with its immediately enclosing constructor"])

recordRhs field_label pat sty
  = ppHang (ppStr "In the record field pattern")
	 4 (ppSep [ppr sty field_label, ppStr "=", ppr sty pat])
\end{code}
