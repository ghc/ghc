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
module MkId (
	mkImportedId,
	mkUserId,
	mkUserLocal, mkSysLocal, 

	mkDataCon, mkTupleCon,

	mkDictFunId,
	mkMethodSelId, mkSuperDictSelId, mkDefaultMethodId,

	mkRecordSelId,

	mkPrimitiveId, 
	mkWorkerId

    ) where

#include "HsVersions.h"

import {-# SOURCE #-} CoreUnfold ( mkUnfolding )

import Type
import CoreSyn
import Literal
import TysWiredIn	( tupleCon )
import Name		( mkLocalName, mkSysLocalName, mkCompoundName, 
			  occNameString, Name, OccName, NamedThing(..)
			)
import Id		( idType, fIRST_TAG,
			  mkTemplateLocals, mkId, mkVanillaId,
			  dataConStrictMarks, dataConFieldLabels, dataConArgTys,
			  recordSelectorFieldLabel, dataConSig,
			  StrictnessMark(..),
			  Id, IdDetails(..), GenId
			)
import IdInfo		( noIdInfo,
			  exactArity, setUnfoldingInfo, 
			  setArityInfo, setInlinePragInfo,
			  InlinePragInfo(..), IdInfo
			)
import Class		( Class, classBigSig, classTyCon )
import FieldLabel	( FieldLabel, FieldLabelTag, mkFieldLabel, fieldLabelName, 
			  firstFieldLabelTag, allFieldLabelTags
			)
import TyVar		( TyVar )
import TyCon		( TyCon, isNewTyCon, tyConDataCons, isDataTyCon )
import PrelVals		( rEC_SEL_ERROR_ID )
import Maybes
import SrcLoc		( SrcLoc )
import BasicTypes	( Arity )
import Unique		( Unique )
import Maybe            ( isJust )
import Outputable
import Util		( assoc )
\end{code}		


%************************************************************************
%*									*
\subsection{Easy ones}
%*									*
%************************************************************************

\begin{code}
mkImportedId :: Name -> ty -> IdInfo -> GenId ty
mkImportedId name ty info = mkId name ty (VanillaId True) info

-- SysLocal: for an Id being created by the compiler out of thin air...
-- UserLocal: an Id with a name the user might recognize...
mkSysLocal  :: FAST_STRING -> Unique -> (GenType flexi) -> SrcLoc -> GenId (GenType flexi)
mkUserLocal :: OccName     -> Unique -> (GenType flexi) -> SrcLoc -> GenId (GenType flexi)

mkSysLocal str uniq ty loc
  = mkVanillaId (mkSysLocalName uniq str loc) ty noIdInfo

mkUserLocal occ uniq ty loc
  = mkVanillaId (mkLocalName uniq occ loc) ty noIdInfo

mkUserId :: Name -> GenType flexi -> GenId (GenType flexi)
mkUserId name ty
  = mkVanillaId name ty noIdInfo

mkDefaultMethodId dm_name rec_c ty
  = mkVanillaId dm_name ty noIdInfo

mkDictFunId dfun_name full_ty clas itys
  = mkVanillaId dfun_name full_ty noIdInfo

mkWorkerId uniq unwrkr ty info
  = mkVanillaId name ty info
  where
    name    	    = mkCompoundName name_fn uniq (getName unwrkr)
    name_fn wkr_str = SLIT("$w") _APPEND_ wkr_str
\end{code}


%************************************************************************
%*									*
\subsection{Data constructors}
%*									*
%************************************************************************

\begin{code}
mkDataCon :: Name
	  -> [StrictnessMark] -> [FieldLabel]
	  -> [TyVar] -> ThetaType
	  -> [TyVar] -> ThetaType
	  -> [TauType] -> TyCon
	  -> Id
  -- can get the tag and all the pieces of the type from the Type

mkDataCon name stricts fields tvs ctxt con_tvs con_ctxt args_tys tycon
  = ASSERT(length stricts == length args_tys)
    data_con
  where
    -- NB: data_con self-recursion; should be OK as tags are not
    -- looked at until late in the game.
    data_con = mkId name data_con_ty details (dataConInfo data_con)
    details  = AlgConId data_con_tag stricts fields tvs ctxt con_tvs con_ctxt args_tys tycon

    data_con_tag    = assoc "mkDataCon" (data_con_family `zip` [fIRST_TAG..]) data_con
    data_con_family = tyConDataCons tycon
    data_con_ty     = mkSigmaTy (tvs++con_tvs) (ctxt++con_ctxt)
				(mkFunTys args_tys (mkTyConApp tycon (mkTyVarTys tvs)))


mkTupleCon :: Arity -> Name -> Type -> Id
mkTupleCon arity name ty 
  = con_id
  where
    con_id = mkId name ty (TupleConId arity) (dataConInfo con_id)
\end{code}

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
dataConInfo :: Id -> IdInfo

dataConInfo con_id
  = setInlinePragInfo IWantToBeINLINEd $
	    	-- Always inline constructors if possible
    setArityInfo (exactArity (length locals)) $
    setUnfoldingInfo unfolding $
    noIdInfo
  where
        unfolding = mkUnfolding con_rhs

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
		  Note (Coerce result_ty (head arg_tys)) (Var data_arg1)
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

We're going to build a record selector unfolding that looks like this:

	data T a b c = T1 { ..., op :: a, ...}
		     | T2 { ..., op :: a, ...}
		     | T3

	sel = /\ a b c -> \ d -> case d of
				    T1 ... x ... -> x
				    T2 ... x ... -> x
				    other	 -> error "..."

\begin{code}
mkRecordSelId field_label selector_ty
  = ASSERT( null theta && isDataTyCon tycon )
    sel_id
  where
    sel_id = mkId (fieldLabelName field_label) selector_ty
		  (RecordSelId field_label) info

    info = exactArity 1	`setArityInfo` (
	   unfolding	`setUnfoldingInfo`
	   noIdInfo)
	-- ToDo: consider adding further IdInfo

    unfolding = mkUnfolding sel_rhs

    (tyvars, theta, tau)  = splitSigmaTy selector_ty
    (data_ty,rhs_ty)      = expectJust "StdIdInfoRec" (splitFunTy_maybe tau)
					-- tau is of form (T a b c -> field-type)
    (tycon, _, data_cons) = splitAlgTyConApp data_ty
    tyvar_tys	          = mkTyVarTys tyvars
	
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
	    maybe_the_arg_id = assocMaybe (field_lbls `zip` arg_ids) field_label

    error_expr = mkApp (Var rEC_SEL_ERROR_ID) [rhs_ty] [LitArg msg_lit]
    full_msg   = showSDoc (sep [text "No match in record selector", ppr sel_id]) 
    msg_lit    = NoRepStr (_PK_ full_msg)
\end{code}


%************************************************************************
%*									*
\subsection{Dictionary selectors}
%*									*
%************************************************************************

\begin{code}
mkSuperDictSelId :: Unique -> Class -> FieldLabelTag -> Type -> Id
	-- The FieldLabelTag says which superclass is selected
	-- So, for 
	--	class (C a, C b) => Foo a b where ...
	-- we get superclass selectors
	--	Foo_sc1, Foo_sc2

mkSuperDictSelId uniq clas index ty
  = mkDictSelId name clas ty
  where
    name    = mkCompoundName name_fn uniq (getName clas)
    name_fn clas_str = clas_str _APPEND_ SLIT("_sc") _APPEND_ (_PK_ (show index))

	-- For method selectors the clean thing to do is
	-- to give the method selector the same name as the class op itself.
mkMethodSelId name clas ty
  = mkDictSelId name clas ty
\end{code}

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.

\begin{code}
mkDictSelId name clas ty
  = sel_id
  where
    sel_id    = mkId name ty (RecordSelId field_lbl) info
    field_lbl = mkFieldLabel name ty tag
    tag       = assoc "MkId.mkDictSelId" ((sc_sel_ids ++ op_sel_ids) `zip` allFieldLabelTags) sel_id

    info      = setInlinePragInfo IWantToBeINLINEd $
		setUnfoldingInfo  unfolding noIdInfo
	-- The always-inline thing means we don't need any other IdInfo

    unfolding = mkUnfolding rhs

    (tyvars, _, sc_sel_ids, op_sel_ids, defms) = classBigSig clas

    tycon      = classTyCon clas
    [data_con] = tyConDataCons tycon
    tyvar_tys  = mkTyVarTys tyvars
    arg_tys    = dataConArgTys data_con tyvar_tys
    the_arg_id = arg_ids !! (tag - firstFieldLabelTag)

    dict_ty    = mkDictTy clas tyvar_tys
    (dict_id:arg_ids) = mkTemplateLocals (dict_ty : arg_tys)

    rhs | isNewTyCon tycon = mkLam tyvars [dict_id] $
			     Note (Coerce (head arg_tys) dict_ty) (Var dict_id)
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
mkPrimitiveId name ty prim_op 
  = mkId name ty (PrimitiveId prim_op) info
  where

    info = setUnfoldingInfo unfolding $
	   setInlinePragInfo IMustBeINLINEd $
		-- The pragma @IMustBeINLINEd@ says that this Id absolutely 
		-- must be inlined.  It's only used for primitives, 
		-- because we don't want to make a closure for each of them.
	   noIdInfo

    unfolding = mkUnfolding rhs

    (tyvars, tau) = splitForAllTys ty
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

