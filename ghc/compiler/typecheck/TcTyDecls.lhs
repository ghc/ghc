%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
module TcTyDecls ( tcTyDecl, kcConDetails, tcMkDataCon ) where

#include "HsVersions.h"

import HsSyn		( TyClDecl(..), ConDecl(..), HsConDetails(..), BangType,
			  getBangType, getBangStrictness, conDetailsTys
			)
import RnHsSyn		( RenamedTyClDecl, RenamedConDecl, RenamedContext )
import BasicTypes	( NewOrData(..), StrictnessMark(..) )

import TcMonoType	( tcHsTyVars, tcHsTheta, tcHsType, 
			  kcHsContext, kcHsSigType, kcHsLiftedSigType
			)
import TcEnv		( tcExtendTyVarEnv, tcLookupTyCon, TyThingDetails(..) )
import TcType		( Type, tyVarsOfTypes, tyVarsOfPred, ThetaType )
import RnEnv		( lookupSysName )
import TcRnMonad

import DataCon		( DataCon, mkDataCon, dataConFieldLabels )
import FieldLabel	( FieldLabel, fieldLabelName, fieldLabelType, allFieldLabelTags, mkFieldLabel )
import MkId		( mkDataConWorkId, mkDataConWrapId, mkRecordSelId )
import Var		( TyVar )
import Name		( Name )
import OccName		( mkDataConWrapperOcc, mkDataConWorkerOcc, mkGenOcc1, mkGenOcc2 )
import Outputable
import TyCon		( TyCon, DataConDetails(..), visibleDataCons,
			  tyConTyVars, tyConName )
import VarSet		( intersectVarSet, isEmptyVarSet )
import Generics		( mkTyConGenInfo )
import CmdLineOpts	( DynFlag(..) )
import List		( nubBy )
\end{code}

%************************************************************************
%*									*
\subsection{Type checking}
%*									*
%************************************************************************

\begin{code}
tcTyDecl :: RenamedTyClDecl -> TcM (Name, TyThingDetails)
tcTyDecl (TySynonym {tcdName = tycon_name, tcdSynRhs = rhs})
  = tcLookupTyCon tycon_name			`thenM` \ tycon ->
    tcExtendTyVarEnv (tyConTyVars tycon)	$
    tcHsType rhs				`thenM` \ rhs_ty ->
    returnM (tycon_name, SynTyDetails rhs_ty)

tcTyDecl (TyData {tcdND = new_or_data, tcdCtxt = context,
		  tcdName = tycon_name, tcdCons = con_decls,
		  tcdGeneric = generic})
  = tcLookupTyCon tycon_name			`thenM` \ tycon ->
    let
	tyvars = tyConTyVars tycon
    in
    tcExtendTyVarEnv tyvars				$
    tcHsTheta context					`thenM` \ ctxt ->
    tcConDecls new_or_data tycon tyvars ctxt con_decls	`thenM` \ data_cons ->
    let
	sel_ids = mkRecordSelectors tycon data_cons
    in
    tcGenericInfo tycon generic				`thenM` \ gen_info ->
    returnM (tycon_name, DataTyDetails ctxt data_cons sel_ids gen_info)

tcTyDecl (ForeignType {tcdName = tycon_name})
  = returnM (tycon_name, ForeignTyDetails)


tcGenericInfo tycon generics	-- Source code decl: consult the flag
  = do_we_want	generics	`thenM` \ want_generics ->
    if want_generics then
	mapM (lookupSysName (tyConName tycon))
	     [mkGenOcc1, mkGenOcc2]		`thenM` \ gen_sys_names ->
	returnM (mkTyConGenInfo tycon gen_sys_names)
    else
	returnM Nothing
  where
    do_we_want (Just g) = returnM g		-- Interface file decl
						-- so look at decl
    do_we_want Nothing  = doptM Opt_Generics	-- Source code decl
						-- so look at flag

mkRecordSelectors tycon data_cons
  = 	-- We'll check later that fields with the same name 
	-- from different constructors have the same type.
     [ mkRecordSelId tycon field 
     | field <- nubBy eq_name fields ]
  where
    fields = [ field | con <- visibleDataCons data_cons, 
		       field <- dataConFieldLabels con ]
    eq_name field1 field2 = fieldLabelName field1 == fieldLabelName field2
\end{code}


%************************************************************************
%*									*
\subsection{Kind and type check constructors}
%*									*
%************************************************************************

\begin{code}
kcConDetails :: NewOrData -> RenamedContext 
	     -> HsConDetails Name (BangType Name) -> TcM ()
kcConDetails new_or_data ex_ctxt details
  = kcHsContext ex_ctxt		`thenM_`
    mappM_ kc_sig_type (conDetailsTys details)
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
	Unknown     -> returnM Unknown
	HasCons n   -> returnM (HasCons n)
	DataCons cs -> mappM tc_con_decl cs	`thenM` \ data_cons ->
		       returnM (DataCons data_cons)
  where
    tc_con_decl (ConDecl name ex_tvs ex_ctxt details src_loc)
      = addSrcLoc src_loc						$
	tcHsTyVars ex_tvs (kcConDetails new_or_data ex_ctxt details)	$ \ ex_tyvars ->
	tcHsTheta ex_ctxt						`thenM` \ ex_theta ->
	case details of
	    PrefixCon btys     -> tc_datacon ex_tyvars ex_theta btys
	    InfixCon bty1 bty2 -> tc_datacon ex_tyvars ex_theta [bty1,bty2]
	    RecCon fields      -> tc_rec_con ex_tyvars ex_theta fields
      where
	
	tc_datacon ex_tyvars ex_theta btys
	  = mappM tcHsType (map getBangType btys)	`thenM` \ arg_tys ->
	    tcMkDataCon name 
			(map getBangStrictness btys)
			[{- No field labels -}] 
			tyvars ctxt ex_tyvars ex_theta 
			arg_tys tycon
    
	tc_rec_con ex_tyvars ex_theta fields
	  = checkTc (null ex_tyvars) (exRecConErr name)		`thenM_`
	    mappM tc_field (fields `zip` allFieldLabelTags)	`thenM` \ field_labels ->
	    let
		arg_stricts = [getBangStrictness bty | (n, bty) <- fields] 
		arg_tys     = map fieldLabelType field_labels
	    in
	    tcMkDataCon name arg_stricts field_labels
			tyvars ctxt ex_tyvars ex_theta 
			arg_tys tycon
    
	tc_field ((field_label_name, bty), tag)
	  = tcHsType (getBangType bty)		`thenM` \ field_ty ->
	    returnM (mkFieldLabel field_label_name tycon field_ty tag)
    
tcMkDataCon :: Name
	    -> [StrictnessMark] -> [FieldLabel]
	    -> [TyVar] -> ThetaType
	    -> [TyVar] -> ThetaType
	    -> [Type] -> TyCon
	    -> TcM DataCon
-- A wrapper for DataCon.mkDataCon that
--   a) makes the worker Id
--   b) makes the wrapper Id if necessary, including
--	allocating its unique (hence monadic)
tcMkDataCon src_name arg_stricts fields 
	    tyvars ctxt ex_tyvars ex_theta 
	    arg_tys tycon
  = lookupSysName src_name mkDataConWrapperOcc	`thenM` \ wrap_name ->
    lookupSysName src_name mkDataConWorkerOcc 	`thenM` \ work_name -> 
	-- This last one takes the name of the data constructor in the source
	-- code, which (for Haskell source anyway) will be in the SrcDataName name
	-- space, and makes it into a "real data constructor name"

    doptM Opt_UnboxStrictFields  `thenM` \ unbox_strict_fields ->

    let
	real_stricts 
	  | unbox_strict_fields  = map unboxUserStrict arg_stricts
	  | otherwise            = arg_stricts

	unboxUserStrict MarkedUserStrict = MarkedUserUnboxed
	unboxUserStrict other            = other

	data_con = mkDataCon src_name real_stricts fields
			     tyvars (thinContext arg_tys ctxt) 
			     ex_tyvars ex_theta
			     arg_tys tycon 
			     data_con_work_id data_con_wrap_id
	data_con_work_id = mkDataConWorkId work_name data_con
	data_con_wrap_id = mkDataConWrapId wrap_name data_con
    in
    returnM data_con	

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
exRecConErr name
  = ptext SLIT("Can't combine named fields with locally-quantified type variables")
    $$
    (ptext SLIT("In the declaration of data constructor") <+> ppr name)
\end{code}
