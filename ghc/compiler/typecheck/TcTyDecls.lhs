%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
module TcTyDecls (
	tcTyDecl, kcTyDecl, 
	tcConDecl,
	mkImplicitDataBinds, mkNewTyConRep
    ) where

#include "HsVersions.h"

import HsSyn		( MonoBinds(..), 
			  TyClDecl(..), ConDecl(..), ConDetails(..), BangType(..),
			  andMonoBindList
			)
import RnHsSyn		( RenamedTyClDecl, RenamedConDecl )
import TcHsSyn		( TcMonoBinds, idsToMonoBinds )
import BasicTypes	( RecFlag(..), NewOrData(..) )

import TcMonoType	( tcExtendTopTyVarScope, tcExtendTyVarScope, 
			  tcHsTypeKind, kcHsType, tcHsTopType, tcHsTopBoxedType,
			  tcContext, tcHsTopTypeKind
			)
import TcType		( zonkTcTyVarToTyVar, zonkTcClassConstraints )
import TcEnv		( tcLookupTy, tcLookupValueByKey, TcTyThing(..) )
import TcMonad
import TcUnify		( unifyKind )

import Class		( Class )
import DataCon		( DataCon, mkDataCon, isNullaryDataCon,
			  dataConFieldLabels, dataConId, dataConWrapId,
			  markedStrict, notMarkedStrict, markedUnboxed, dataConRepType
			)
import MkId		( mkDataConId, mkDataConWrapId, mkRecordSelId )
import FieldLabel
import Var		( Id, TyVar )
import Name		( Name, isLocallyDefined, OccName, NamedThing(..), nameUnique )
import Outputable
import TyCon		( TyCon, AlgTyConFlavour(..), ArgVrcs, mkSynTyCon, mkAlgTyCon, 
			  tyConDataConsIfAvailable, tyConTyVars,
			  isSynTyCon, isNewTyCon
			)
import Type		( getTyVar, tyVarsOfTypes, splitFunTy, applyTys,
			  mkTyConApp, mkTyVarTys, mkForAllTys, mkFunTy,
			  mkTyVarTy, splitAlgTyConApp_maybe,
			  mkArrowKind, mkArrowKinds, boxedTypeKind,
			  isUnboxedType, Type, ThetaType, classesOfPreds
			)
import TysWiredIn	( unitTy )
import Var		( tyVarKind )
import VarSet		( intersectVarSet, isEmptyVarSet )
import Unique		( unpackCStringIdKey )
import Util		( equivClasses )
import FiniteMap        ( FiniteMap, lookupWithDefaultFM )
import CmdLineOpts	( opt_GlasgowExts )
\end{code}

%************************************************************************
%*									*
\subsection{Kind checking}
%*									*
%************************************************************************

\begin{code}
kcTyDecl :: RenamedTyClDecl -> TcM s ()

kcTyDecl (TySynonym name tyvar_names rhs src_loc)
  = tcLookupTy name				`thenNF_Tc` \ (kind, _) ->
    tcExtendTopTyVarScope kind tyvar_names	$ \ _ result_kind ->
    tcHsTypeKind rhs				`thenTc` \ (rhs_kind, _) ->
    unifyKind result_kind rhs_kind

kcTyDecl (TyData _ context tycon_name tyvar_names con_decls _ _ _ src_loc)
  = tcLookupTy tycon_name			`thenNF_Tc` \ (kind, _) ->
    tcExtendTopTyVarScope kind tyvar_names	$ \ result_kind _ ->
    tcContext context				`thenTc_` 
    mapTc kcConDecl con_decls			`thenTc_`
    returnTc ()

kcConDecl (ConDecl _ _ ex_tvs ex_ctxt details loc)
  = tcAddSrcLoc loc			(
    tcExtendTyVarScope ex_tvs		( \ tyvars -> 
    tcContext ex_ctxt			`thenTc_`
    kc_con details			`thenTc_`
    returnTc ()
    ))
  where
    kc_con (VanillaCon btys)    = mapTc kc_bty btys		`thenTc_` returnTc ()
    kc_con (InfixCon bty1 bty2) = mapTc kc_bty [bty1,bty2]	`thenTc_` returnTc ()
    kc_con (NewCon ty _)        = kcHsType ty
    kc_con (RecCon flds)        = mapTc kc_field flds		`thenTc_` returnTc ()

    kc_bty (Banged ty)   = kcHsType ty
    kc_bty (Unbanged ty) = kcHsType ty
    kc_bty (Unpacked ty) = kcHsType ty

    kc_field (_, bty)    = kc_bty bty
\end{code}


%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcTyDecl :: RecFlag -> FiniteMap Name ArgVrcs -> RenamedTyClDecl -> TcM s (Name, TcTyThing)

tcTyDecl is_rec rec_vrcs (TySynonym tycon_name tyvar_names rhs src_loc)
  = tcLookupTy tycon_name				`thenNF_Tc` \ (tycon_kind, ASynTyCon _ arity) ->
    tcExtendTopTyVarScope tycon_kind tyvar_names	$ \ tyvars _ ->
    tcHsTopTypeKind rhs					`thenTc` \ (_, rhs_ty) ->
	-- If the RHS mentions tyvars that aren't in scope, we'll 
	-- quantify over them.  With gla-exts that's right, but for H98
	-- we should complain. We can't do that here without falling into
	-- a black hole, so we do it in rnDecl (TySynonym case)
    let
	-- Construct the tycon
        argvrcs = lookupWithDefaultFM rec_vrcs (pprPanic "tcTyDecl: argvrcs:" $ ppr tycon_name)
                                      tycon_name
	tycon = mkSynTyCon tycon_name tycon_kind arity tyvars rhs_ty argvrcs
    in
    returnTc (tycon_name, ASynTyCon tycon arity)


tcTyDecl is_rec rec_vrcs (TyData data_or_new context tycon_name tyvar_names con_decls nconstrs derivings pragmas src_loc)
  = 	-- Lookup the pieces
    tcLookupTy tycon_name				`thenNF_Tc` \ (tycon_kind, ADataTyCon rec_tycon) ->
    tcExtendTopTyVarScope tycon_kind tyvar_names	$ \ tyvars _ ->

	-- Typecheck the pieces
    tcContext context					`thenTc` \ ctxt ->
    let ctxt' = classesOfPreds ctxt in
    mapTc (tcConDecl rec_tycon tyvars ctxt') con_decls	`thenTc` \ data_cons ->
    tc_derivs derivings					`thenTc` \ derived_classes ->

    let
	-- Construct the tycon
	flavour = case data_or_new of
			NewType -> NewTyCon (mkNewTyConRep tycon)
			DataType | all isNullaryDataCon data_cons -> EnumTyCon
				 | otherwise			  -> DataTyCon

        argvrcs = lookupWithDefaultFM rec_vrcs (pprPanic "tcTyDecl: argvrcs:" $ ppr tycon_name)
                                      tycon_name

	tycon = mkAlgTyCon tycon_name tycon_kind tyvars ctxt' argvrcs
			   data_cons nconstrs
			   derived_classes
			   flavour is_rec
    in
    returnTc (tycon_name, ADataTyCon tycon)
  where
	tc_derivs Nothing   = returnTc []
	tc_derivs (Just ds) = mapTc tc_deriv ds

	tc_deriv name = tcLookupTy name `thenTc` \ (_, AClass clas _) ->
			returnTc clas
\end{code}

\begin{code}
mkNewTyConRep :: TyCon -> Type
-- Find the representation type for this newtype TyCon
-- The trick is to to deal correctly with recursive newtypes
-- such as	newtype T = MkT T

mkNewTyConRep tc
  = mkForAllTys tvs (loop [] (mkTyConApp tc (mkTyVarTys tvs)))
  where
    tvs = tyConTyVars tc
    loop tcs ty = case splitAlgTyConApp_maybe ty of {
			Nothing -> ty ;
			Just (tc, tys, data_cons) | not (isNewTyCon tc) -> ty
						  | tc `elem` tcs	-> unitTy
						  | otherwise		->

		  case splitFunTy (applyTys (dataConRepType (head data_cons)) tys) of
			(rep_ty, _) -> loop (tc:tcs) rep_ty
		  }
\end{code}


%************************************************************************
%*									*
\subsection{Type check constructors}
%*									*
%************************************************************************

\begin{code}
tcConDecl :: TyCon -> [TyVar] -> [(Class,[Type])] -> RenamedConDecl -> TcM s DataCon

tcConDecl tycon tyvars ctxt (ConDecl name wkr_name ex_tvs ex_ctxt details src_loc)
  = tcAddSrcLoc src_loc			$
    tcExtendTyVarScope ex_tvs		$ \ ex_tyvars -> 
    tcContext ex_ctxt			`thenTc` \ ex_theta ->
    let 
	ex_ctxt' = classesOfPreds ex_theta
    in
    tc_con_decl_help tycon tyvars ctxt name wkr_name ex_tyvars ex_ctxt' details

tc_con_decl_help tycon tyvars ctxt name wkr_name ex_tyvars ex_theta details
  = case details of
	VanillaCon btys    -> tc_datacon btys
	InfixCon bty1 bty2 -> tc_datacon [bty1,bty2]
	NewCon ty mb_f	   -> tc_newcon ty mb_f
	RecCon fields	   -> tc_rec_con fields
  where
    tc_datacon btys
      = let
	    arg_stricts = map get_strictness btys
	    tys	        = map get_pty btys
        in
	mapTc tcHsTopType tys `thenTc` \ arg_tys ->
	mk_data_con arg_stricts arg_tys []

    tc_newcon ty mb_f
      = tcHsTopBoxedType ty	`thenTc` \ arg_ty ->
	    -- can't allow an unboxed type here, because we're effectively
	    -- going to remove the constructor while coercing it to a boxed type.
	let
	  field_label =
	    case mb_f of
	      Nothing -> []
	      Just f  -> [mkFieldLabel (getName f) tycon arg_ty (head allFieldLabelTags)]
        in	      
	mk_data_con [notMarkedStrict] [arg_ty] field_label

    tc_rec_con fields
      = checkTc (null ex_tyvars) (exRecConErr name)	    `thenTc_`
	mapTc tc_field fields	`thenTc` \ field_label_infos_s ->
	let
	    field_label_infos = concat field_label_infos_s
	    arg_stricts       = [strict | (_, _, strict) <- field_label_infos]
	    arg_tys	      = [ty     | (_, ty, _)     <- field_label_infos]

	    field_labels      = [ mkFieldLabel (getName name) tycon ty tag 
			      | ((name, ty, _), tag) <- field_label_infos `zip` allFieldLabelTags ]
	in
	mk_data_con arg_stricts arg_tys field_labels

    tc_field (field_label_names, bty)
      = tcHsTopType (get_pty bty)	`thenTc` \ field_ty ->
	returnTc [(name, field_ty, get_strictness bty) | name <- field_label_names]

    mk_data_con arg_stricts arg_tys fields
      = 	-- Now we've checked all the field types we must
		-- zonk the existential tyvars to finish the kind
		-- inference on their kinds, and commit them to being
		-- immutable type variables.  (The top-level tyvars are
		-- already fixed, by the preceding kind-inference pass.)
	mapNF_Tc zonkTcTyVarToTyVar ex_tyvars	`thenNF_Tc` \ ex_tyvars' ->
	zonkTcClassConstraints	ex_theta	`thenNF_Tc` \ ex_theta' ->
	let
	   data_con = mkDataCon name arg_stricts fields
		      	   tyvars (thinContext arg_tys ctxt)
			   ex_tyvars' ex_theta'
		      	   arg_tys
		      	   tycon data_con_id data_con_wrap_id
	   data_con_id      = mkDataConId wkr_name data_con
	   data_con_wrap_id = mkDataConWrapId data_con
	in
	returnNF_Tc data_con

-- The context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
thinContext arg_tys ctxt
  = filter in_arg_tys ctxt
  where
      arg_tyvars = tyVarsOfTypes arg_tys
      in_arg_tys (clas,tys) = not $ isEmptyVarSet $ 
			      tyVarsOfTypes tys `intersectVarSet` arg_tyvars
  
get_strictness (Banged   _) = markedStrict
get_strictness (Unbanged _) = notMarkedStrict
get_strictness (Unpacked _) = markedUnboxed

get_pty (Banged ty)   = ty
get_pty (Unbanged ty) = ty
get_pty (Unpacked ty) = ty
\end{code}



%************************************************************************
%*									*
\subsection{Generating constructor/selector bindings for data declarations}
%*									*
%************************************************************************

\begin{code}
mkImplicitDataBinds :: [TyCon] -> TcM s ([Id], TcMonoBinds)
mkImplicitDataBinds [] = returnTc ([], EmptyMonoBinds)
mkImplicitDataBinds (tycon : tycons) 
  | isSynTyCon tycon = mkImplicitDataBinds tycons
  | otherwise	     = mkImplicitDataBinds_one tycon	`thenTc` \ (ids1, b1) ->
		       mkImplicitDataBinds tycons	`thenTc` \ (ids2, b2) ->
		       returnTc (ids1++ids2, b1 `AndMonoBinds` b2)

mkImplicitDataBinds_one tycon
  = mapTc (mkRecordSelector tycon) groups	`thenTc` \ sel_ids ->
    let
	unf_ids = sel_ids ++ data_con_wrapper_ids
	all_ids = map dataConId data_cons ++ unf_ids 

	-- For the locally-defined things
	-- we need to turn the unfoldings inside the selector Ids into bindings,
	-- and build bindigns for the constructor wrappers
	binds | isLocallyDefined tycon = idsToMonoBinds unf_ids
	      | otherwise	       = EmptyMonoBinds
    in	
    returnTc (all_ids, binds)
  where
    data_cons = tyConDataConsIfAvailable tycon
	-- Abstract types mean we don't bring the 
	-- data cons into scope, which should be fine

    data_con_wrapper_ids = map dataConWrapId data_cons

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
    tcLookupValueByKey unpackCStringIdKey	`thenTc` \ unpack_id ->
    returnTc (mkRecordSelId tycon first_field_label unpack_id)
  where
    field_ty   = fieldLabelType first_field_label
    field_name = fieldLabelName first_field_label
    other_tys  = [fieldLabelType fl | (_, fl) <- other_fields]
\end{code}


Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
fieldTypeMisMatch field_name
  = sep [ptext SLIT("Declared types differ for field"), quotes (ppr field_name)]

exRecConErr name
  = ptext SLIT("Can't combine named fields with locally-quantified type variables")
    $$
    (ptext SLIT("In the declaration of data constructor") <+> ppr name)
\end{code}
