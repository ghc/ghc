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
#include "HsVersions.h"

module StdIdInfo (
	addStandardIdInfo
    ) where

IMP_Ubiq()

import Type
import CmdLineOpts      ( opt_PprUserLength )
import CoreSyn
import Literal
import CoreUnfold	( mkUnfolding, PragmaInfo(..) )
import TysWiredIn	( tupleCon )
import Id		( GenId, mkTemplateLocals, idType,
			  dataConStrictMarks, dataConFieldLabels, dataConArgTys,
			  recordSelectorFieldLabel, dataConSig,
			  StrictnessMark(..),
			  isAlgCon, isMethodSelId_maybe, isSuperDictSelId_maybe,
			  isRecordSelector, isPrimitiveId_maybe, 
			  addIdUnfolding, addIdArity,
			  SYN_IE(Id)
			)
import IdInfo		( ArityInfo, exactArity )
import Class		( GenClass, GenClassOp, classSig, classOpLocalType )
import TyCon		( isNewTyCon, isDataTyCon, isAlgTyCon )
import FieldLabel	( FieldLabel )
import PrelVals		( pAT_ERROR_ID )
import Maybes
import Outputable	( PprStyle(..), Outputable(..) )
import Pretty
import Util		( assertPanic, pprTrace, 
			  assoc
			)
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

	dict_tys     = [mkDictTy clas ty | (clas,ty) <- theta]
	con_dict_tys = [mkDictTy clas ty | (clas,ty) <- con_theta]
	n_dicts	     = length dict_tys
	result_ty    = applyTyCon tycon (mkTyVarTys tyvars)

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

	mk_case arg body | isUnboxedType (idType arg)
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
	(data_ty,rhs_ty)      = expectJust "StdIdInfoRec" (getFunTy_maybe tau)
					-- tau is of form (T a b c -> field-type)
	(tycon, _, data_cons) = getAppDataTyCon data_ty
	tyvar_tys	      = mkTyVarTys tyvars
	
	[data_id] = mkTemplateLocals [data_ty]
	sel_rhs = mkTyLam tyvars $
		  mkValLam [data_id] $
		  Case (Var data_id) (AlgAlts (catMaybes (map mk_maybe_alt data_cons))
					      (BindDefault data_id error_expr))
	mk_maybe_alt data_con 
	  = case maybe_the_arg_id of
		Nothing		-> Nothing
		Just the_arg_id -> Just (data_con, arg_ids, Var the_arg_id)
	  where
	    arg_ids 	     = mkTemplateLocals (dataConArgTys data_con tyvar_tys)
				    -- The first one will shadow data_id, but who cares
	    field_lbls	     = dataConFieldLabels data_con
	    maybe_the_arg_id = assocMaybe (field_lbls `zip` arg_ids) field_lbl

	error_expr = mkApp (Var pAT_ERROR_ID) [] [rhs_ty] [LitArg msg_lit]
 	full_msg   = show (sep [text "No match in record selector", ppr (PprForUser opt_PprUserLength) sel_id]) 
	msg_lit    = NoRepStr (_PK_ full_msg)
\end{code}


%************************************************************************
%*									*
\subsection{Super selectors}
%*									*
%************************************************************************

\begin{code}
addStandardIdInfo sel_id
  | maybeToBool maybe_sc_sel_id
  = sel_id `addIdUnfolding` unfolding
	-- The always-inline thing means we don't need any other IdInfo
  where
    maybe_sc_sel_id    = isSuperDictSelId_maybe sel_id
    Just (cls, the_sc) = maybe_sc_sel_id

    unfolding = mkUnfolding IWantToBeINLINEd {- Always inline selectors -} rhs
    rhs	      = mk_dict_selector [tyvar] dict_id arg_ids the_arg_id

    (tyvar, scs, ops)  = classSig cls
    tyvar_ty	       = mkTyVarTy tyvar
    [dict_id]	       = mkTemplateLocals [mkDictTy cls tyvar_ty]
    arg_ids	       = mkTemplateLocals ([mkDictTy sc tyvar_ty | sc <- scs] ++
					   map classOpLocalType ops)
    the_arg_id	       = assoc "StdIdInfoSC" (scs `zip` arg_ids) the_sc

addStandardIdInfo sel_id
  | maybeToBool maybe_meth_sel_id
  = sel_id `addIdUnfolding` unfolding
	-- The always-inline thing means we don't need any other IdInfo
  where
    maybe_meth_sel_id  = isMethodSelId_maybe sel_id
    Just (cls, the_op) = maybe_meth_sel_id

    unfolding = mkUnfolding IWantToBeINLINEd {- Always inline selectors -} rhs
    rhs       = mk_dict_selector [tyvar] dict_id arg_ids the_arg_id

    (tyvar, scs, ops) = classSig cls
    n_scs	      = length scs
    tyvar_ty	      = mkTyVarTy tyvar
    [dict_id]	      = mkTemplateLocals [mkDictTy cls tyvar_ty]
    arg_ids	      = mkTemplateLocals ([mkDictTy sc tyvar_ty | sc <- scs] ++
					  map classOpLocalType ops)
					  
    the_arg_id	      = assoc "StdIdInfoMeth" (ops `zip` (drop n_scs arg_ids)) the_op
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

    (tyvars, tau) = splitForAllTy (idType prim_id)
    (arg_tys, _)  = splitFunTy tau

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
  = pprTrace "addStandardIdInfo missing:" (ppr PprDebug id) id
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selector help function
%*									*
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

\begin{code}
mk_dict_selector tyvars dict_id [arg_id] the_arg_id
  = mkLam tyvars [dict_id] (Var dict_id)

mk_dict_selector tyvars dict_id arg_ids the_arg_id
  = mkLam tyvars [dict_id] $
    Case (Var dict_id) (AlgAlts [(tup_con, arg_ids, Var the_arg_id)] NoDefault)
  where
    tup_con = tupleCon (length arg_ids)
\end{code}
