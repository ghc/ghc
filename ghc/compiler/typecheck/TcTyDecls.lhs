%
% (c) The AQUA Project, Glasgow University, 1996
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
			  andMonoBinds
			)
import HsTypes		( getTyVarName )
import RnHsSyn		( RenamedTyDecl(..), RenamedConDecl(..)	)
import TcHsSyn		( mkHsTyLam, mkHsDictLam, tcIdType,
			  TcHsBinds, TcMonoBinds
			)
import BasicTypes	( RecFlag(..), NewOrData(..) )

import Inst		( newDicts, InstOrigin(..), Inst )
import TcMonoType	( tcHsTypeKind, tcHsType, tcContext )
import TcSimplify	( tcSimplifyCheckThetas )
import TcType		( tcInstTyVars )
import TcEnv		( TcIdOcc(..), tcInstId,
			  tcLookupTyCon, tcLookupTyVar, tcLookupClass,
			  newLocalId, newLocalIds, tcLookupClassByKey
			)
import TcMonad
import TcKind		( TcKind, unifyKind, mkArrowKind, mkBoxedTypeKind )

import Class		( classInstEnv, Class )
import MkId		( mkDataCon, mkRecordSelId )
import Id		( dataConSig, idType,
			  dataConFieldLabels, dataConStrictMarks,
			  StrictnessMark(..), getIdUnfolding,
			  Id
			)
import CoreUnfold	( getUnfoldingTemplate )
import FieldLabel
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import Name		( nameSrcLoc, isLocallyDefined, getSrcLoc,
			  OccName(..), 
			  NamedThing(..)
			)
import Outputable
import TyCon		( TyCon, mkSynTyCon, mkDataTyCon, isAlgTyCon, 
			  isSynTyCon, tyConDataCons
			)
import Type		( typeKind, getTyVar, tyVarsOfTypes, splitSigmaTy,
			  mkTyConApp, mkTyVarTys, mkForAllTys, mkFunTy,
			  splitFunTys, mkTyVarTy, getTyVar_maybe,
			  isUnboxedType, Type, ThetaType
			)
import TyVar		( tyVarKind, elementOfTyVarSet, intersectTyVarSets, isEmptyTyVarSet,
			  TyVar )
import Unique		( evalClassKey )
import UniqSet		( emptyUniqSet, mkUniqSet, uniqSetToList, unionManyUniqSets, UniqSet )
import Util		( equivClasses, zipEqual, nOfThem, panic, assertPanic )
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
    mapAndUnzipNF_Tc (tcLookupTyVar.getTyVarName) tyvar_names
						`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->

	-- Look at the rhs
    tcHsTypeKind rhs				`thenTc` \ (rhs_kind, rhs_ty) ->

	-- Unify tycon kind with (k1->...->kn->rhs)
    unifyKind tycon_kind
	(foldr mkArrowKind rhs_kind tyvar_kinds)
						`thenTc_`
    let
	-- Getting the TyCon's kind is a bit of a nuisance.  We can't use the tycon_kind,
	-- because that's a TcKind and may not yet be fully unified with other kinds.
	-- We could have augmented the tycon environment with a knot-tied kind,
	-- but the simplest thing to do seems to be to get the Kind by (lazily)
	-- looking at the tyvars and rhs_ty.
	result_kind, final_tycon_kind :: Kind 	-- NB not TcKind!
	result_kind      = typeKind rhs_ty
	final_tycon_kind = foldr (mkArrowKind . tyVarKind) result_kind rec_tyvars

	-- Construct the tycon
	tycon = mkSynTyCon (getName tycon_name)
			   final_tycon_kind
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
    mapAndUnzipNF_Tc (tcLookupTyVar.getTyVarName)
				 tyvar_names	`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->
    tc_derivs derivings				`thenTc` \ derived_classes ->

	-- Typecheck the context
    tcContext context				`thenTc` \ ctxt ->

	-- Unify tycon kind with (k1->...->kn->Type)
    unifyKind tycon_kind
	(foldr mkArrowKind mkBoxedTypeKind tyvar_kinds)
						`thenTc_`

	-- Walk the condecls
    mapTc (tcConDecl rec_tycon rec_tyvars ctxt) con_decls
						`thenTc` \ con_ids ->
    let
	-- Construct the tycon
	final_tycon_kind :: Kind 		-- NB not TcKind!
	final_tycon_kind = foldr (mkArrowKind . tyVarKind) mkBoxedTypeKind rec_tyvars

	tycon = mkDataTyCon (getName tycon_name)
			    final_tycon_kind
			    rec_tyvars
			    ctxt
			    con_ids
			    derived_classes
			    Nothing		-- Not a dictionary
			    data_or_new
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
    mapTc checkConstructorContext data_cons	`thenTc_` 
    mapTc (mkRecordSelector tycon) groups	`thenTc` \ sel_ids ->
    let
	data_ids = data_cons ++ sel_ids

	-- For the locally-defined things
	-- we need to turn the unfoldings inside the Ids into bindings,
	binds = [ CoreMonoBind (RealId data_id) (getUnfoldingTemplate (getIdUnfolding data_id))
		| data_id <- data_ids, isLocallyDefined data_id
		]
    in	
    returnTc (data_ids, andMonoBinds binds)
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

-- Check that all the types of all the strict arguments are in Eval

\begin{code}
checkConstructorContext con_id
  | not (isLocallyDefined con_id)
  = returnTc ()

  | otherwise	-- It is locally defined
  = tcLookupClassByKey evalClassKey	`thenNF_Tc` \ eval_clas ->
    let
	strict_marks					   = dataConStrictMarks con_id
	(tyvars, theta, ext_tyvars, ext_theta, arg_tys, _) = dataConSig con_id

	eval_theta = [ (eval_clas, [arg_ty]) 
		     | (arg_ty, MarkedStrict) <- zipEqual "strict_args" 
						   arg_tys strict_marks
		     ]
    in
    tcAddErrCtxt (evalCtxt con_id eval_theta) $
    tcSimplifyCheckThetas theta eval_theta
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
tcConDecl :: TyCon -> [TyVar] -> ThetaType -> RenamedConDecl -> TcM s Id

tcConDecl tycon tyvars ctxt (ConDecl name ex_ctxt (VanillaCon btys) src_loc)
  = tcDataCon tycon tyvars ctxt name btys src_loc

tcConDecl tycon tyvars ctxt (ConDecl op ex_ctxt (InfixCon bty1 bty2) src_loc)
  = tcDataCon tycon tyvars ctxt op [bty1,bty2] src_loc

tcConDecl tycon tyvars ctxt (ConDecl name ex_ctxt (NewCon ty) src_loc)
  = tcAddSrcLoc src_loc	$
    tcHsType ty `thenTc` \ arg_ty ->
    -- can't allow an unboxed type here, because we're effectively
    -- going to remove the constructor while coercing it to a boxed type.
    checkTc (not (isUnboxedType arg_ty)) (newTypeUnboxedField ty) `thenTc_`
    let
      data_con = mkDataCon (getName name)
			   [NotMarkedStrict]
			   [{- No labelled fields -}]
		      	   tyvars
		      	   ctxt
			   [] []	-- Temporary; existential chaps
		      	   [arg_ty]
		      	   tycon
    in
    returnTc data_con

tcConDecl tycon tyvars ctxt (ConDecl name ex_ctxt (RecCon fields) src_loc)
  = tcAddSrcLoc src_loc	$
    mapTc tcField fields	`thenTc` \ field_label_infos_s ->
    let
      field_label_infos = concat field_label_infos_s
      stricts           = [strict | (_, _, strict) <- field_label_infos]
      arg_tys	        = [ty     | (_, ty, _)     <- field_label_infos]

      field_labels      = [ mkFieldLabel (getName name) ty tag 
			  | ((name, ty, _), tag) <- field_label_infos `zip` allFieldLabelTags ]

      data_con = mkDataCon (getName name)
			   stricts
			   field_labels
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
			   [] []	-- Temporary; existential chaps
		      	   arg_tys
		      	   tycon
    in
    returnTc data_con

tcField (field_label_names, bty)
  = tcHsType (get_pty bty)	`thenTc` \ field_ty ->
    returnTc [(name, field_ty, get_strictness bty) | name <- field_label_names]

tcDataCon tycon tyvars ctxt name btys src_loc
  = tcAddSrcLoc src_loc	$
    let
	stricts = map get_strictness btys
	tys	= map get_pty btys
    in
    mapTc tcHsType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getName name)
			   stricts
			   [{- No field labels -}]
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
			   [] []	-- Temporary existential chaps
		      	   arg_tys
		      	   tycon
    in
    returnTc data_con

-- The context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
thinContext arg_tys ctxt
  = filter in_arg_tys ctxt
  where
      arg_tyvars = tyVarsOfTypes arg_tys
      in_arg_tys (clas,tys) = not $ isEmptyTyVarSet $ 
			      tyVarsOfTypes tys `intersectTyVarSets` arg_tyvars
  
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

evalCtxt con eval_theta
  = hsep [ptext SLIT("When checking the Eval context for constructor:"), 
	   ppr con,
	   text "::", ppr eval_theta]
\end{code}
