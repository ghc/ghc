%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
module TcTyDecls ( tcTyDecl, checkValidTyCon, kcConDetails ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..), ConDecl(..), ConDetails(..), 
			  getBangType, getBangStrictness, conDetailsTys
			)
import RnHsSyn		( RenamedTyClDecl, RenamedConDecl, RenamedContext )
import BasicTypes	( NewOrData(..) )

import TcMonoType	( tcHsTyVars, tcHsTheta, tcHsType, 
			  kcHsContext, kcHsSigType, kcHsLiftedSigType
			)
import TcEnv		( tcExtendTyVarEnv, 
			  tcLookupTyCon, tcLookupRecId, 
			  TyThingDetails(..), RecTcEnv
			)
import TcType		( tcEqType, tyVarsOfTypes, tyVarsOfPred, ThetaType )
import TcMType		( checkValidType, UserTypeCtxt(..), checkValidTheta, SourceTyCtxt(..) )
import TcMonad

import DataCon		( DataCon, mkDataCon, dataConFieldLabels, dataConWrapId, dataConName )
import MkId		( mkDataConId, mkDataConWrapId, mkRecordSelId )
import FieldLabel
import Var		( TyVar, idType )
import Name		( Name, NamedThing(..) )
import Outputable
import TyCon		( TyCon, DataConDetails(..), visibleDataCons,
			  tyConName, tyConTheta, getSynTyConDefn, 
			  tyConTyVars, tyConDataCons, isSynTyCon )
import VarSet		( intersectVarSet, isEmptyVarSet )
import PrelNames	( unpackCStringName, unpackCStringUtf8Name )
import ListSetOps	( equivClasses )
import List		( nubBy )
\end{code}

%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcTyDecl :: RecTcEnv -> RenamedTyClDecl -> TcM (Name, TyThingDetails)
tcTyDecl unf_env (TySynonym {tcdName = tycon_name, tcdSynRhs = rhs})
  = tcLookupTyCon tycon_name			`thenNF_Tc` \ tycon ->
    tcExtendTyVarEnv (tyConTyVars tycon)	$
    tcHsType rhs				`thenTc` \ rhs_ty ->
    returnTc (tycon_name, SynTyDetails rhs_ty)

tcTyDecl unf_env (TyData {tcdND = new_or_data, tcdCtxt = context,
			  tcdName = tycon_name, tcdCons = con_decls})
  = tcLookupTyCon tycon_name			`thenNF_Tc` \ tycon ->
    let
	tyvars = tyConTyVars tycon
    in
    tcExtendTyVarEnv tyvars				$
    tcHsTheta context					`thenTc` \ ctxt ->
    tcConDecls new_or_data tycon tyvars ctxt con_decls	`thenTc` \ data_cons ->
    let
	sel_ids = mkRecordSelectors unf_env tycon data_cons
    in
    returnTc (tycon_name, DataTyDetails ctxt data_cons sel_ids)

tcTyDecl unf_env (ForeignType {tcdName = tycon_name})
  = returnTc (tycon_name, ForeignTyDetails)


mkRecordSelectors unf_env tycon data_cons
  = 	-- We'll check later that fields with the same name 
	-- from different constructors have the same type.
     [ mkRecordSelId tycon field unpack_id unpackUtf8_id
     | field <- nubBy eq_name fields ]
  where
    fields = [ field | con <- visibleDataCons data_cons, 
		       field <- dataConFieldLabels con ]
    eq_name field1 field2 = fieldLabelName field1 == fieldLabelName field2

    unpack_id     = tcLookupRecId unf_env unpackCStringName
    unpackUtf8_id = tcLookupRecId unf_env unpackCStringUtf8Name
\end{code}


%************************************************************************
%*									*
\subsection{Validity check}
%*									*
%************************************************************************

checkValidTyCon is called once the mutually-recursive knot has been
tied, so we can look at things freely.

\begin{code}
checkValidTyCon :: TyCon -> TcM ()
checkValidTyCon tc
  | isSynTyCon tc = checkValidType (TySynCtxt name) syn_rhs
  | otherwise
  = 	-- Check the context on the data decl
    checkValidTheta (DataTyCtxt name) (tyConTheta tc)	`thenTc_` 
	
	-- Check arg types of data constructors
    mapTc_ check_data_con data_cons			`thenTc_`

	-- Check that fields with the same name share a type
    mapTc_ check_fields groups

  where
    name         = tyConName tc
    (_, syn_rhs) = getSynTyConDefn tc
    data_cons    = tyConDataCons tc

    fields = [field | con <- data_cons, field <- dataConFieldLabels con]
    groups = equivClasses cmp_name fields
    cmp_name field1 field2 = fieldLabelName field1 `compare` fieldLabelName field2

    check_data_con con = checkValidType (ConArgCtxt (dataConName con)) 
					(idType (dataConWrapId con))
				-- This checks the argument types and
				-- the existential context (if any)			 

    check_fields fields@(first_field_label : other_fields)
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


tcConDecls :: NewOrData -> TyCon -> [TyVar] -> ThetaType 
	   -> DataConDetails RenamedConDecl -> TcM (DataConDetails DataCon)

tcConDecls new_or_data tycon tyvars ctxt con_decls
  = case con_decls of
	Unknown     -> returnTc Unknown
	HasCons n   -> returnTc (HasCons n)
	DataCons cs -> mapTc tc_con_decl cs	`thenTc` \ data_cons ->
		       returnTc (DataCons data_cons)
  where
    tc_con_decl (ConDecl name wkr_name ex_tvs ex_ctxt details src_loc)
      = tcAddSrcLoc src_loc						$
	tcHsTyVars ex_tvs (kcConDetails new_or_data ex_ctxt details)	$ \ ex_tyvars ->
	tcHsTheta ex_ctxt						`thenTc` \ ex_theta ->
	case details of
	    VanillaCon btys    -> tc_datacon ex_tyvars ex_theta btys
	    InfixCon bty1 bty2 -> tc_datacon ex_tyvars ex_theta [bty1,bty2]
	    RecCon fields      -> tc_rec_con ex_tyvars ex_theta fields
      where
	
	tc_datacon ex_tyvars ex_theta btys
	  = mapTc tcHsType (map getBangType btys)	`thenTc` \ arg_tys ->
	    mk_data_con ex_tyvars ex_theta (map getBangStrictness btys) arg_tys []
    
	tc_rec_con ex_tyvars ex_theta fields
	  = checkTc (null ex_tyvars) (exRecConErr name)	`thenTc_`
	    mapTc tc_field (fields `zip` allFieldLabelTags)	`thenTc` \ field_labels_s ->
	    let
		field_labels = concat field_labels_s
		arg_stricts = [str | (ns, bty) <- fields, 
				     let str = getBangStrictness bty, 
				     n <- ns	-- One for each.  E.g   x,y,z :: !Int
			      ]
	    in
	    mk_data_con ex_tyvars ex_theta arg_stricts 
			(map fieldLabelType field_labels) field_labels
    
	tc_field ((field_label_names, bty), tag)
	  = tcHsType (getBangType bty)			`thenTc` \ field_ty ->
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
