%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[StdIdInfo]{Standard unfoldings}

This module contains definitions for the IdInfo for things that
have a standard form, namely:

	* data constructors
	* record selectors
	* method and superclass selectors
	* primitive operations

\begin{code}
module StdIdInfo (
	addStandardIdInfo
    ) where

#include "HsVersions.h"

import Type
import TyVar		( alphaTyVar )
import CoreSyn
import Literal
import CoreUnfold	( mkUnfolding, PragmaInfo(..) )
import TysWiredIn	( tupleCon )
import Id		( mkTemplateLocals, idType,
			  dataConStrictMarks, dataConFieldLabels, dataConArgTys,
			  recordSelectorFieldLabel, dataConSig,
			  StrictnessMark(..),
			  isAlgCon, isDictSelId_maybe,
			  isRecordSelector, isPrimitiveId_maybe, 
			  addIdUnfolding, addIdArity,
			  Id
			)
import IdInfo		( ArityInfo, exactArity )
import Class		( classBigSig, classTyCon )
import TyCon		( isNewTyCon, tyConDataCons, isDataTyCon )
import FieldLabel	( FieldLabel )
import PrelVals		( pAT_ERROR_ID )
import Maybes
import Maybe            ( isJust )
import Outputable
import Util		( assoc )
\end{code}		


%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

We're going to build a constructor that looks like:

	data (Data a, C b) =>  T a b = T1 !a !Int b

	T1 = /\ a b -> 
	     \d1::Data a, d2::C b ->
	     \p q r -> case p of { p ->
		       case q of { q ->
		       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is 
  very careful to preserve evaluation order, which we don't need
  to be here.

\begin{code}
addStandardIdInfo :: Id -> Id

addStandardIdInfo con_id

  | isAlgCon con_id
  = con_id `addIdUnfolding` unfolding
	   `addIdArity` exactArity (length locals)
  where
        unfolding = mkUnfolding IWantToBeINLINEd {- Always inline constructors -} con_rhs

	(tyvars, theta, con_tyvars, con_theta, arg_tys, tycon) = dataConSig con_id

	dict_tys     = [mkDictTy clas tys | (clas,tys) <- theta]
	con_dict_tys = [mkDictTy clas tys | (clas,tys) <- con_theta]
	n_dicts	     = length dict_tys
	result_ty    = mkTyConApp tycon (mkTyVarTys tyvars)

	locals        = mkTemplateLocals (dict_tys ++ con_dict_tys ++ arg_tys)
	data_args     = drop n_dicts locals
	(data_arg1:_) = data_args		-- Used for newtype only
	strict_marks  = dataConStrictMarks con_id
	strict_args   = [arg | (arg,MarkedStrict) <- data_args `zip` strict_marks]
		-- NB: we can't call mkTemplateLocals twice, because it
		-- always starts from the same unique.

	con_app | isNewTyCon tycon 
		= ASSERT( length arg_tys == 1)
		  Coerce (CoerceIn con_id) result_ty (Var data_arg1)
 		| otherwise
		= Con con_id (map TyArg (mkTyVarTys tyvars) ++ map VarArg data_args)

	con_rhs = mkTyLam tyvars $
		  mkValLam locals $
		  foldr mk_case con_app strict_args

	mk_case arg body | isUnpointedType (idType arg)
			 = body			-- "!" on unboxed arg does nothing
			 | otherwise
			 = Case (Var arg) (AlgAlts [] (BindDefault arg body))
				-- This case shadows "arg" but that's fine
\end{code}


%************************************************************************
%*									*
\subsection{Record selectors}
%*									*
%************************************************************************

We're going to build a record selector that looks like this:

	data T a b c = T1 { ..., op :: a, ...}
		     | T2 { ..., op :: a, ...}
		     | T3

	sel = /\ a b c -> \ d -> case d of
				    T1 ... x ... -> x
				    T2 ... x ... -> x
				    other	 -> error "..."

\begin{code}
addStandardIdInfo sel_id
  | isRecordSelector sel_id
  = ASSERT( null theta && isDataTyCon tycon )
    sel_id `addIdUnfolding` unfolding
	   `addIdArity` exactArity 1 
	-- ToDo: consider adding further IdInfo
  where
	unfolding = mkUnfolding NoPragmaInfo {- Don't inline every selector -} sel_rhs

	(tyvars, theta, tau)  = splitSigmaTy (idType sel_id)
	field_lbl	      = recordSelectorFieldLabel sel_id
	(data_ty,rhs_ty)      = expectJust "StdIdInfoRec" (splitFunTy_maybe tau)
					-- tau is of form (T a b c -> field-type)
	(tycon, _, data_cons) = splitAlgTyConApp data_ty
	tyvar_tys	      = mkTyVarTys tyvars
	
	[data_id] = mkTemplateLocals [data_ty]
        alts      = map mk_maybe_alt data_cons
	sel_rhs   = mkTyLam tyvars $
		    mkValLam [data_id] $
		    Case (Var data_id) 
		         -- if any of the constructors don't have the label, ...
			 (if any (not . isJust) alts then
		           AlgAlts (catMaybes alts) 
			           (BindDefault data_id error_expr)
			  else
			   AlgAlts (catMaybes alts) NoDefault)

	mk_maybe_alt data_con 
	  = case maybe_the_arg_id of
		Nothing		-> Nothing
		Just the_arg_id -> Just (data_con, arg_ids, Var the_arg_id)
	  where
	    arg_ids 	     = mkTemplateLocals (dataConArgTys data_con tyvar_tys)
				    -- The first one will shadow data_id, but who cares
	    field_lbls	     = dataConFieldLabels data_con
	    maybe_the_arg_id = assocMaybe (field_lbls `zip` arg_ids) field_lbl

	error_expr = mkApp (Var pAT_ERROR_ID) [rhs_ty] [LitArg msg_lit]
 	full_msg   = showSDoc (sep [text "No match in record selector", ppr sel_id]) 
	msg_lit    = NoRepStr (_PK_ full_msg)
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selectors}
%*									*
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

\begin{code}
addStandardIdInfo sel_id
  | maybeToBool maybe_dict_sel_id
  = sel_id `addIdUnfolding` unfolding
  where
    maybe_dict_sel_id = isDictSelId_maybe sel_id
    Just clas 	    = maybe_dict_sel_id

    unfolding = mkUnfolding IWantToBeINLINEd {- Always inline selectors -} rhs
	-- The always-inline thing means we don't need any other IdInfo

    (tyvars, _, sc_sel_ids, op_sel_ids, defms) = classBigSig clas

    tycon      = classTyCon clas
    [data_con] = tyConDataCons tycon
    tyvar_tys  = mkTyVarTys tyvars
    arg_tys    = dataConArgTys data_con tyvar_tys
    the_arg_id = assoc "StdIdInfo:mk_sel" ((sc_sel_ids ++ op_sel_ids) `zip` arg_ids) sel_id

    (dict_id:arg_ids) = mkTemplateLocals (mkDictTy clas tyvar_tys : arg_tys)

    rhs | isNewTyCon tycon = mkLam tyvars [dict_id] $
			     Coerce (CoerceOut data_con) (head arg_tys) (Var dict_id)
	| otherwise	   = mkLam tyvars [dict_id] $
			     Case (Var dict_id) $
			     AlgAlts [(data_con, arg_ids, Var the_arg_id)] NoDefault
\end{code}


%************************************************************************
%*									*
\subsection{Primitive operations
%*									*
%************************************************************************


\begin{code}
addStandardIdInfo prim_id
  | maybeToBool maybe_prim_id
  = prim_id `addIdUnfolding` unfolding
  where
    maybe_prim_id = isPrimitiveId_maybe prim_id
    Just prim_op  = maybe_prim_id

    unfolding = mkUnfolding IWantToBeINLINEd {- Always inline PrimOps -} rhs

    (tyvars, tau) = splitForAllTys (idType prim_id)
    (arg_tys, _)  = splitFunTys tau

    args = mkTemplateLocals arg_tys
    rhs =  mkLam tyvars args $
	   Prim prim_op
		([TyArg (mkTyVarTy tv) | tv <- tyvars] ++ 
		 [VarArg v | v <- args])
\end{code}


%************************************************************************
%*									*
\subsection{Catch-all}
%*									*
%************************************************************************

\begin{code}
addStandardIdInfo id
  = pprTrace "addStandardIdInfo missing:" (ppr id) id
\end{code}

