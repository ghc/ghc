%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
module TcTyDecls ( tcTyDecl1, kcConDetails ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..), ConDecl(..), ConDetails(..), 
			  getBangType, getBangStrictness, conDetailsTys
			)
import RnHsSyn		( RenamedTyClDecl, RenamedConDecl, RenamedContext )
import BasicTypes	( NewOrData(..), RecFlag, isRec )

import TcMonoType	( tcHsRecType, tcHsTyVars, tcRecTheta,
			  kcHsContext, kcHsSigType, kcHsLiftedSigType
			)
import TcEnv		( tcExtendTyVarEnv, 
			  tcLookupTyCon, tcLookupRecId, 
			  TyThingDetails(..), RecTcEnv
			)
import TcType		( tcSplitTyConApp_maybe, tcEqType, 
			  tyVarsOfTypes, tyVarsOfPred, 
			  mkTyConApp, mkTyVarTys, mkForAllTys, 
			  Type, ThetaType
			)
import TcMonad

import DataCon		( DataCon, mkDataCon, dataConFieldLabels, dataConRepType,
			  isNullaryDataCon, dataConOrigArgTys )
import MkId		( mkDataConId, mkDataConWrapId, mkRecordSelId )
import FieldLabel
import Var		( TyVar )
import Name		( Name, NamedThing(..) )
import Outputable
import TyCon		( TyCon, AlgTyConFlavour(..), tyConTyVars )
import VarSet		( intersectVarSet, isEmptyVarSet )
import PrelNames	( unpackCStringName, unpackCStringUtf8Name )
import ListSetOps	( equivClasses )
\end{code}

%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcTyDecl1 :: RecFlag -> RecTcEnv -> RenamedTyClDecl -> TcM (Name, TyThingDetails)
tcTyDecl1 is_rec unf_env (TySynonym {tcdName = tycon_name, tcdSynRhs = rhs})
  = tcLookupTyCon tycon_name			`thenNF_Tc` \ tycon ->
    tcExtendTyVarEnv (tyConTyVars tycon)	$
    tcHsRecType is_rec rhs			`thenTc` \ rhs_ty ->
	-- Note tcHsRecType not tcHsRecSigType; we allow type synonyms
	-- that aren't types; e.g.  type List = []
	--
	-- If the RHS mentions tyvars that aren't in scope, we'll 
	-- quantify over them:
	--	e.g. 	type T = a->a
	-- will become	type T = forall a. a->a
	--
	-- With gla-exts that's right, but for H98 we should complain. 
	-- We can now do that here without falling into
	-- a black hole, we still do it in rnDecl (TySynonym case)

    returnTc (tycon_name, SynTyDetails rhs_ty)

tcTyDecl1 is_rec unf_env (TyData {tcdND = new_or_data, tcdCtxt = context,
			  	  tcdName = tycon_name, tcdCons = con_decls})
  = tcLookupTyCon tycon_name			`thenNF_Tc` \ tycon ->
    let
	tyvars = tyConTyVars tycon
    in
    tcExtendTyVarEnv tyvars				$

	-- Typecheck the pieces
    tcRecTheta is_rec context						`thenTc` \ ctxt ->
    mapTc (tcConDecl is_rec new_or_data tycon tyvars ctxt) con_decls	`thenTc` \ data_cons ->
    tcRecordSelectors is_rec unf_env tycon data_cons			`thenTc` \ sel_ids -> 
    returnTc (tycon_name, DataTyDetails ctxt data_cons sel_ids)

tcTyDecl1 is_rec unf_env (ForeignType {tcdName = tycon_name})
  = returnTc (tycon_name, ForeignTyDetails)
\end{code}


%************************************************************************
%*									*
\subsection{Kind and type check constructors}
%*									*
%************************************************************************

\begin{code}
kcConDetails :: NewOrData -> RenamedContext -> ConDetails Name -> TcM ()
kcConDetails new_or_data ex_ctxt details
  = kcHsContext ex_ctxt		`thenTc_`
    mapTc_ kc_sig_type (conDetailsTys details)
  where
    kc_sig_type = case new_or_data of
		    DataType -> kcHsSigType
		    NewType  -> kcHsLiftedSigType
	    -- Can't allow an unlifted type here, because we're effectively
	    -- going to remove the constructor while coercing it to a lifted type.


tcConDecl :: RecFlag -> NewOrData -> TyCon -> [TyVar] -> ThetaType -> RenamedConDecl -> TcM DataCon

tcConDecl is_rec new_or_data tycon tyvars ctxt (ConDecl name wkr_name ex_tvs ex_ctxt details src_loc)
  = tcAddSrcLoc src_loc							$
    tcHsTyVars ex_tvs (kcConDetails new_or_data ex_ctxt details)	$ \ ex_tyvars ->
    tcRecTheta is_rec ex_ctxt						`thenTc` \ ex_theta ->
    case details of
	VanillaCon btys    -> tc_datacon ex_tyvars ex_theta btys
	InfixCon bty1 bty2 -> tc_datacon ex_tyvars ex_theta [bty1,bty2]
	RecCon fields	   -> tc_rec_con ex_tyvars ex_theta fields
  where
    tc_datacon ex_tyvars ex_theta btys
      = let
	    arg_stricts = map getBangStrictness btys
	    tys	        = map getBangType btys
        in
	mapTc (tcHsRecType is_rec) tys		`thenTc` \ arg_tys ->
	mk_data_con ex_tyvars ex_theta arg_stricts arg_tys []

    tc_rec_con ex_tyvars ex_theta fields
      = checkTc (null ex_tyvars) (exRecConErr name)	`thenTc_`
	mapTc tc_field (fields `zip` allFieldLabelTags)	`thenTc` \ field_labels_s ->
	let
	    field_labels = concat field_labels_s
	    arg_stricts = [str | (ns, bty) <- fields, 
				 let str = getBangStrictness bty, 
				 n <- ns		-- One for each.  E.g   x,y,z :: !Int
			  ]
	in
	mk_data_con ex_tyvars ex_theta arg_stricts 
		    (map fieldLabelType field_labels) field_labels

    tc_field ((field_label_names, bty), tag)
      = tcHsRecType is_rec (getBangType bty)		`thenTc` \ field_ty ->
	returnTc [mkFieldLabel (getName name) tycon field_ty tag | name <- field_label_names]

    mk_data_con ex_tyvars ex_theta arg_stricts arg_tys fields
      = let
	   data_con = mkDataCon name arg_stricts fields
		      	   tyvars (thinContext arg_tys ctxt)
			   ex_tyvars ex_theta
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
      in_arg_tys pred = not $ isEmptyVarSet $ 
			tyVarsOfPred pred `intersectVarSet` arg_tyvars
\end{code}


%************************************************************************
%*									*
\subsection{Record selectors}
%*									*
%************************************************************************

\begin{code}
tcRecordSelectors is_rec unf_env tycon data_cons
	-- Omit the check that the fields have consistent types if 
	-- the group is recursive; TcTyClsDecls.tcGroup will repeat 
	-- with NonRecursive once we have tied the knot
  | isRec is_rec = returnTc sel_ids
  | otherwise	 = mapTc check groups 	`thenTc_` 
		   returnTc sel_ids
  where
    fields = [ field | con   <- data_cons
		     , field <- dataConFieldLabels con ]

	-- groups is list of fields that share a common name
    groups		   = equivClasses cmp_name fields
    cmp_name field1 field2 = fieldLabelName field1 `compare` fieldLabelName field2

    sel_ids = [ mkRecordSelId tycon field unpack_id unpackUtf8_id
	      | (field : _) <- groups ]

    check fields@(first_field_label : other_fields)
	-- These fields all have the same name, but are from
	-- different constructors in the data type
	= 	-- Check that all the fields in the group have the same type
		-- NB: this check assumes that all the constructors of a given
		-- data type use the same type variables
	  checkTc (all (tcEqType field_ty) other_tys) (fieldTypeMisMatch field_name)
	where
	    field_ty   = fieldLabelType first_field_label
	    field_name = fieldLabelName first_field_label
	    other_tys  = map fieldLabelType other_fields

    unpack_id     = tcLookupRecId unf_env unpackCStringName
    unpackUtf8_id = tcLookupRecId unf_env unpackCStringUtf8Name
\end{code}



%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************


\begin{code}
fieldTypeMisMatch field_name
  = sep [ptext SLIT("Different constructors give different types for field"), quotes (ppr field_name)]

exRecConErr name
  = ptext SLIT("Can't combine named fields with locally-quantified type variables")
    $$
    (ptext SLIT("In the declaration of data constructor") <+> ppr name)
\end{code}
