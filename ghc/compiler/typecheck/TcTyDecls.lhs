%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
module TcTyDecls (
	tcTyDecl,
	tcConDecl,
	mkDataBinds
    ) where

#include "HsVersions.h"

import HsSyn		( MonoBinds(..), 
			  TyDecl(..), ConDecl(..), ConDetails(..), BangType(..),
			  andMonoBindList
			)
import RnHsSyn		( RenamedTyDecl, RenamedConDecl )
import TcHsSyn		( TcMonoBinds )
import BasicTypes	( RecFlag(..), NewOrData(..), StrictnessMark(..) )

import Inst		( InstOrigin(..) )
import TcMonoType	( tcHsTypeKind, tcHsType, tcContext )
import TcEnv		( TcIdOcc(..),
			  tcLookupTyCon, tcLookupClass,
			  tcLookupTyVarBndrs
			)
import TcMonad
import TcUnify		( unifyKind )

import Class		( Class )
import DataCon		( DataCon, dataConSig, mkDataCon, isNullaryDataCon,
			  dataConFieldLabels, dataConId
			)
import MkId		( mkDataConId, mkRecordSelId )
import Id		( getIdUnfolding )
import CoreUnfold	( getUnfoldingTemplate )
import FieldLabel
import Var		( Id, TyVar )
import Name		( isLocallyDefined, OccName(..), NamedThing(..)	)
import Outputable
import TyCon		( TyCon, mkSynTyCon, mkAlgTyCon, isAlgTyCon, 
			  isSynTyCon, tyConDataCons
			)
import Type		( typeKind, getTyVar, tyVarsOfTypes,
			  mkTyConApp, mkTyVarTys, mkForAllTys, mkFunTy,
			  mkTyVarTy,
			  mkArrowKind, mkArrowKinds, boxedTypeKind,
			  isUnboxedType, Type, ThetaType
			)
import Var		( tyVarKind )
import VarSet		( intersectVarSet, isEmptyVarSet )
import Util		( equivClasses, panic, assertPanic )
\end{code}

\begin{code}
tcTyDecl :: RecFlag -> RenamedTyDecl -> TcM s TyCon
\end{code}

Type synonym decls
~~~~~~~~~~~~~~~~~~

\begin{code}
tcTyDecl is_rec (TySynonym tycon_name tyvar_names rhs src_loc)
  = tcAddSrcLoc src_loc $
    tcAddErrCtxt (tySynCtxt tycon_name) $

	-- Look up the pieces
    tcLookupTyCon tycon_name			`thenTc` \ (tycon_kind,  _, rec_tycon) ->
    tcLookupTyVarBndrs tyvar_names		`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->

	-- Look at the rhs
    tcHsTypeKind rhs				`thenTc` \ (rhs_kind, rhs_ty) ->

	-- Unify tycon kind with (k1->...->kn->rhs)
    unifyKind tycon_kind (mkArrowKinds tyvar_kinds rhs_kind)	`thenTc_`
    let
	-- Construct the tycon
        kind  = mkArrowKinds (map tyVarKind rec_tyvars) (typeKind rhs_ty)
	tycon = mkSynTyCon (getName tycon_name)
			   kind
			   (length tyvar_names)
			   rec_tyvars
			   rhs_ty
    in
    returnTc tycon
\end{code}

Algebraic data and newtype decls
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tcTyDecl is_rec (TyData data_or_new context tycon_name tyvar_names con_decls derivings pragmas src_loc)
  = tcAddSrcLoc src_loc $
    let ctxt = case data_or_new of
		 NewType  -> tyNewCtxt tycon_name
		 DataType -> tyDataCtxt tycon_name
    in
    tcAddErrCtxt ctxt $

	-- Lookup the pieces
    tcLookupTyCon tycon_name			`thenTc` \ (tycon_kind, _, rec_tycon) ->
    tcLookupTyVarBndrs tyvar_names		`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->
    tc_derivs derivings				`thenTc` \ derived_classes ->

	-- Typecheck the context
    tcContext context				`thenTc` \ ctxt ->

	-- Unify tycon kind with (k1->...->kn->Type)
    unifyKind tycon_kind (mkArrowKinds tyvar_kinds boxedTypeKind)	`thenTc_`

	-- Walk the condecls
    mapTc (tcConDecl rec_tycon rec_tyvars ctxt) con_decls
						`thenTc` \ data_cons ->
    let
	-- Construct the tycon
	real_data_or_new = case data_or_new of
				NewType -> NewType
				DataType -> if all isNullaryDataCon data_cons then
						EnumType
					    else
						DataType

	kind = foldr (mkArrowKind . tyVarKind) boxedTypeKind rec_tyvars
	tycon = mkAlgTyCon (getName tycon_name)
			   kind
			   rec_tyvars
			   ctxt
			   data_cons
			   derived_classes
			   Nothing		-- Not a dictionary
			   real_data_or_new
			   is_rec
    in
    returnTc tycon

tc_derivs Nothing   = returnTc []
tc_derivs (Just ds) = mapTc tc_deriv ds

tc_deriv name
  = tcLookupClass name `thenTc` \ (_, clas) ->
    returnTc clas
\end{code}

Generating constructor/selector bindings for data declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
mkDataBinds :: [TyCon] -> TcM s ([Id], TcMonoBinds s)
mkDataBinds [] = returnTc ([], EmptyMonoBinds)
mkDataBinds (tycon : tycons) 
  | isSynTyCon tycon = mkDataBinds tycons
  | otherwise	     = mkDataBinds_one tycon	`thenTc` \ (ids1, b1) ->
		       mkDataBinds tycons	`thenTc` \ (ids2, b2) ->
		       returnTc (ids1++ids2, b1 `AndMonoBinds` b2)

mkDataBinds_one tycon
  = ASSERT( isAlgTyCon tycon )
    mapTc (mkRecordSelector tycon) groups	`thenTc` \ sel_ids ->
    let
	data_ids = map dataConId data_cons ++ sel_ids

	-- For the locally-defined things
	-- we need to turn the unfoldings inside the Ids into bindings,
	binds = [ CoreMonoBind (RealId data_id) (getUnfoldingTemplate (getIdUnfolding data_id))
		| data_id <- data_ids, isLocallyDefined data_id
		]
    in	
    returnTc (data_ids, andMonoBindList binds)
  where
    data_cons = tyConDataCons tycon
    fields = [ (con, field) | con   <- data_cons,
			      field <- dataConFieldLabels con
	     ]

	-- groups is list of fields that share a common name
    groups = equivClasses cmp_name fields
    cmp_name (_, field1) (_, field2) 
	= fieldLabelName field1 `compare` fieldLabelName field2
\end{code}

\begin{code}
mkRecordSelector tycon fields@((first_con, first_field_label) : other_fields)
		-- These fields all have the same name, but are from
		-- different constructors in the data type
	-- Check that all the fields in the group have the same type
	-- This check assumes that all the constructors of a given
	-- data type use the same type variables
  = checkTc (all (== field_ty) other_tys)
	    (fieldTypeMisMatch field_name)	`thenTc_`
    returnTc selector_id
  where
    field_ty   = fieldLabelType first_field_label
    field_name = fieldLabelName first_field_label
    other_tys  = [fieldLabelType fl | (_, fl) <- other_fields]
    (tyvars, _, _, _, _, _) = dataConSig first_con
    data_ty  = mkTyConApp tycon (mkTyVarTys tyvars)
    -- tyvars of first_con may be free in field_ty
    -- Now build the selector

    selector_ty :: Type
    selector_ty  = mkForAllTys tyvars $	
		   mkFunTy data_ty $
		   field_ty
      
    selector_id :: Id
    selector_id = mkRecordSelId first_field_label selector_ty
\end{code}

Constructors
~~~~~~~~~~~~
\begin{code}
tcConDecl :: TyCon -> [TyVar] -> ThetaType -> RenamedConDecl -> TcM s DataCon

tcConDecl tycon tyvars ctxt (ConDecl name ex_tvs ex_ctxt details src_loc)
  = tcAddSrcLoc src_loc	$
    tcLookupTyVarBndrs ex_tvs		`thenNF_Tc` \ (kinds, ex_tyvars) ->
    tcContext ex_ctxt			`thenTc`    \ ex_theta ->
    tc_con_help tycon tyvars ctxt name ex_tyvars ex_theta details
    
tc_con_help tycon tyvars ctxt name ex_tyvars ex_theta (VanillaCon btys)
  = tc_datacon_help tycon tyvars ctxt name ex_tyvars ex_theta btys

tc_con_help tycon tyvars ctxt name ex_tyvars ex_theta (InfixCon bty1 bty2)
  = tc_datacon_help tycon tyvars ctxt name ex_tyvars ex_theta [bty1,bty2]

tc_con_help tycon tyvars ctxt name ex_tyvars ex_theta (NewCon ty)
  = tcHsType ty `thenTc` \ arg_ty ->
    -- can't allow an unboxed type here, because we're effectively
    -- going to remove the constructor while coercing it to a boxed type.
    checkTc (not (isUnboxedType arg_ty)) (newTypeUnboxedField ty) `thenTc_`
    let
      data_con = mkDataCon (getName name)
			   [NotMarkedStrict]
			   [{- No labelled fields -}]
		      	   tyvars
		      	   ctxt
			   ex_tyvars ex_theta
		      	   [arg_ty]
		      	   tycon data_con_id
      data_con_id = mkDataConId data_con
    in
    returnTc data_con

tc_con_help tycon tyvars ctxt name ex_tyvars ex_theta (RecCon fields)
  = checkTc (null ex_tyvars) (exRecConErr name)	    `thenTc_`
    mapTc tcField fields	`thenTc` \ field_label_infos_s ->
    let
      field_label_infos = concat field_label_infos_s
      arg_stricts       = [strict | (_, _, strict) <- field_label_infos]
      arg_tys	        = [ty     | (_, ty, _)     <- field_label_infos]

      field_labels      = [ mkFieldLabel (getName name) ty tag 
			  | ((name, ty, _), tag) <- field_label_infos `zip` allFieldLabelTags ]

      data_con = mkDataCon (getName name)
			   arg_stricts
			   field_labels
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
			   ex_tyvars ex_theta
		      	   arg_tys
		      	   tycon data_con_id
      data_con_id = mkDataConId data_con
    in
    returnTc data_con

tcField (field_label_names, bty)
  = tcHsType (get_pty bty)	`thenTc` \ field_ty ->
    returnTc [(name, field_ty, get_strictness bty) | name <- field_label_names]

tc_datacon_help tycon tyvars ctxt name ex_tyvars ex_theta btys
  = let
	arg_stricts = map get_strictness btys
	tys	    = map get_pty btys
    in
    mapTc tcHsType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getName name)
			   arg_stricts
			   [{- No field labels -}]
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
			   ex_tyvars ex_theta
		      	   arg_tys
		      	   tycon data_con_id
      data_con_id = mkDataConId data_con
    in
    returnTc data_con

-- The context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
thinContext arg_tys ctxt
  = filter in_arg_tys ctxt
  where
      arg_tyvars = tyVarsOfTypes arg_tys
      in_arg_tys (clas,tys) = not $ isEmptyVarSet $ 
			      tyVarsOfTypes tys `intersectVarSet` arg_tyvars
  
get_strictness (Banged   _) = MarkedStrict
get_strictness (Unbanged _) = NotMarkedStrict

get_pty (Banged ty)   = ty
get_pty (Unbanged ty) = ty
\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
tySynCtxt tycon_name
  = hsep [ptext SLIT("In the type declaration for"), quotes (ppr tycon_name)]

tyDataCtxt tycon_name
  = hsep [ptext SLIT("In the data declaration for"), quotes (ppr tycon_name)]

tyNewCtxt tycon_name
  = hsep [ptext SLIT("In the newtype declaration for"), quotes (ppr tycon_name)]

fieldTypeMisMatch field_name
  = sep [ptext SLIT("Declared types differ for field"), quotes (ppr field_name)]

newTypeUnboxedField ty
  = sep [ptext SLIT("Newtype constructor field has an unboxed type:"), 
	 quotes (ppr ty)]

exRecConErr name
  = ptext SLIT("Can't combine named fields with locally-quantified type variables")
    $$
    (ptext SLIT("In the declaration of data constructor") <+> ppr name)
\end{code}
