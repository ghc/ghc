%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
#include "HsVersions.h"

module TcTyDecls (
	tcTyDecl,
	tcConDecl,
	tcRecordSelectors
    ) where

import Ubiq{-uitous-}

import HsSyn		( TyDecl(..), ConDecl(..), BangType(..), HsExpr(..), 
			  Match(..), GRHSsAndBinds(..), GRHS(..), OutPat(..), 
			  HsBinds(..), HsLit, Stmt, Qual, ArithSeqInfo, PolyType, 
			  Bind(..), MonoBinds(..), Sig, 
			  MonoType )
import RnHsSyn		( RenamedTyDecl(..), RenamedConDecl(..) )
import TcHsSyn		( TcHsBinds(..), TcIdOcc(..), mkHsTyLam )

import TcMonoType	( tcMonoTypeKind, tcMonoType, tcContext )
import TcType		( tcInstTyVars, tcInstType )
import TcEnv		( tcLookupTyCon, tcLookupTyVar, tcLookupClass,
			  newLocalId
			)
import TcMonad
import TcKind		( TcKind, unifyKind, mkTcArrowKind, mkTcTypeKind )

import Id		( mkDataCon, dataConSig, mkRecordSelectorId,
			  dataConFieldLabels, StrictnessMark(..)
			)
import FieldLabel
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import SpecEnv		( SpecEnv(..), nullSpecEnv )
import Name		( getNameFullName, Name(..) )
import Pretty
import TyCon		( TyCon, NewOrData(..), mkSynTyCon, mkDataTyCon, tyConDataCons )
import Type		( getTypeKind, getTyVar, tyVarsOfTypes, eqTy, applyTyCon,
			  mkForAllTys, mkFunTy )
import TyVar		( getTyVarKind, elementOfTyVarSet )
import UniqSet		( emptyUniqSet, mkUniqSet, uniqSetToList, unionManyUniqSets, UniqSet(..) )
import Util		( panic, equivClasses )
\end{code}

\begin{code}
tcTyDecl :: RenamedTyDecl -> TcM s TyCon
\end{code}

Type synonym decls
~~~~~~~~~~~~~~~~~~

\begin{code}
tcTyDecl (TySynonym tycon_name tyvar_names rhs src_loc)
  = tcAddSrcLoc src_loc $
    tcAddErrCtxt (tySynCtxt tycon_name) $

	-- Look up the pieces
    tcLookupTyCon tycon_name			`thenNF_Tc` \ (tycon_kind,  _, rec_tycon) ->
    mapAndUnzipNF_Tc tcLookupTyVar tyvar_names	`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->

	-- Look at the rhs
    tcMonoTypeKind rhs				`thenTc` \ (rhs_kind, rhs_ty) ->

	-- Unify tycon kind with (k1->...->kn->rhs)
    unifyKind tycon_kind
	(foldr mkTcArrowKind rhs_kind tyvar_kinds)
						`thenTc_`
    let
	-- Getting the TyCon's kind is a bit of a nuisance.  We can't use the tycon_kind,
	-- because that's a TcKind and may not yet be fully unified with other kinds.
	-- We could have augmented the tycon environment with a knot-tied kind,
	-- but the simplest thing to do seems to be to get the Kind by (lazily)
	-- looking at the tyvars and rhs_ty.
	result_kind, final_tycon_kind :: Kind 	-- NB not TcKind!
	result_kind      = getTypeKind rhs_ty
	final_tycon_kind = foldr (mkArrowKind . getTyVarKind) result_kind rec_tyvars

	-- Construct the tycon
	tycon = mkSynTyCon (getItsUnique tycon_name)
			   (getNameFullName tycon_name)
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
tcTyDecl (TyData context tycon_name tyvar_names con_decls derivings pragmas src_loc)
  = tcTyDataOrNew DataType context tycon_name tyvar_names con_decls derivings pragmas src_loc

tcTyDecl (TyNew context tycon_name tyvar_names con_decl derivings pragmas src_loc)
  = tcTyDataOrNew NewType  context tycon_name tyvar_names con_decl  derivings pragmas src_loc


tcTyDataOrNew data_or_new context tycon_name tyvar_names con_decls derivings pragmas src_loc
  = tcAddSrcLoc src_loc $
    tcAddErrCtxt (tyDataCtxt tycon_name) $

	-- Lookup the pieces
    tcLookupTyCon tycon_name			`thenNF_Tc` \ (tycon_kind, _, rec_tycon) ->
    mapAndUnzipNF_Tc tcLookupTyVar tyvar_names	`thenNF_Tc` \ (tyvar_kinds, rec_tyvars) ->
    tc_derivs derivings				`thenNF_Tc` \ derived_classes ->

	-- Typecheck the context
    tcContext context				`thenTc` \ ctxt ->

	-- Unify tycon kind with (k1->...->kn->Type)
    unifyKind tycon_kind
	(foldr mkTcArrowKind mkTcTypeKind tyvar_kinds)
						`thenTc_`

	-- Walk the condecls
    mapTc (tcConDecl rec_tycon rec_tyvars ctxt) con_decls
						`thenTc` \ con_ids ->
    let
	-- Construct the tycon
	final_tycon_kind :: Kind 		-- NB not TcKind!
	final_tycon_kind = foldr (mkArrowKind . getTyVarKind) mkBoxedTypeKind rec_tyvars

	tycon = mkDataTyCon (getItsUnique tycon_name)
			    final_tycon_kind
			    (getNameFullName tycon_name)
			    rec_tyvars
			    ctxt
			    con_ids
			    derived_classes
			    data_or_new
    in
    returnTc tycon

tc_derivs Nothing   = returnNF_Tc []
tc_derivs (Just ds) = mapNF_Tc tc_deriv ds

tc_deriv name
  = tcLookupClass name `thenNF_Tc` \ (_, clas) ->
    returnNF_Tc clas
\end{code}

Generating selector bindings for record delarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
tcRecordSelectors :: TyCon -> TcM s ([Id], TcHsBinds s)
tcRecordSelectors tycon
  = mapAndUnzipTc (tcRecordSelector tycon) groups	`thenTc` \ (ids, binds) ->
    returnTc (ids, SingleBind (NonRecBind (foldr AndMonoBinds EmptyMonoBinds binds)))
  where
    data_cons = tyConDataCons tycon
    fields = [ (con, field) | con   <- data_cons,
			      field <- dataConFieldLabels con
	     ]

	-- groups is list of fields that share a common name
    groups = equivClasses cmp_name fields
    cmp_name (_, field1) (_, field2) 
	= fieldLabelName field1 `cmp` fieldLabelName field2
\end{code}

We're going to build a record selector that looks like this:

	data T a b c = T1 { op :: a, ...}
		     | T2 { op :: a, ...}
		     | T3

	sel :: forall a b c. T a b c -> a
	sel = /\ a b c -> \ T1 { sel = x } -> x
			    T2 { sel = 2 } -> x

Note that the selector Id itself is used as the field
label; it has to be an Id, you see!

\begin{code}
tcRecordSelector tycon fields@((first_con, first_field_label) : other_fields)
  = panic "tcRecordSelector: don't typecheck"
{-
  = let
	field_ty   = fieldLabelType first_field_label
	field_name = fieldLabelName first_field_label
	other_tys  = [fieldLabelType fl | (_, fl) <- fields]
	(tyvars, _, _, _) = dataConSig first_con
	-- tyvars of first_con may be free in first_ty
    in
   
	-- Check that all the fields in the group have the same type
	-- This check assumes that all the constructors of a given
	-- data type use the same type variables
    checkTc (all (eqTy field_ty) other_tys)
	    (fieldTypeMisMatch field_name)	`thenTc_`
    
	-- Create an Id for the field itself
    tcInstTyVars tyvars			`thenNF_Tc` \ (tyvars', tyvar_tys, tenv) ->
    tcInstType tenv field_ty		`thenNF_Tc` \ field_ty' ->
    let
      data_ty'     = applyTyCon tycon tyvar_tys
    in
    newLocalId SLIT("x") field_ty'	`thenNF_Tc` \ field_id ->
    newLocalId SLIT("r") data_ty'	`thenNF_Tc` \ record_id ->

	-- Now build the selector
    let
      tycon_src_loc = getSrcLoc tycon

      selector_ty  = mkForAllTys tyvars' $
		     mkFunTy data_ty' $
		     field_ty'
      
      selector_id = mkRecordSelectorId first_field_label selector_ty

	-- HsSyn is dreadfully verbose for defining the selector!
      selector_rhs = mkHsTyLam tyvars' $
		     HsLam $
		     PatMatch (VarPat record_id) $
		     GRHSMatch $
		     GRHSsAndBindsOut [OtherwiseGRHS selector_body tycon_src_loc] 
				      EmptyBinds field_ty'

      selector_body = HsCase (HsVar record_id) (map mk_match fields) tycon_src_loc

      mk_match (con_id, field_label) 
    	= PatMatch (RecPat con_id data_ty' [(selector_id, VarPat field_id, False)]) $
	  GRHSMatch $
    	  GRHSsAndBindsOut [OtherwiseGRHS (HsVar field_id) 
					  (getSrcLoc (fieldLabelName field_label))] 
			   EmptyBinds
			   field_ty'
    in
    returnTc (selector_id, VarMonoBind selector_id selector_rhs)
-}
\end{code}

Constructors
~~~~~~~~~~~~
\begin{code}
tcConDecl :: TyCon -> [TyVar] -> [(Class,Type)] -> RenamedConDecl -> TcM s Id

tcConDecl tycon tyvars ctxt (ConDecl name btys src_loc)
  = tcDataCon tycon tyvars ctxt name btys src_loc

tcConDecl tycon tyvars ctxt (ConOpDecl bty1 op bty2 src_loc)
  = tcDataCon tycon tyvars ctxt op [bty1,bty2] src_loc

tcConDecl tycon tyvars ctxt (NewConDecl name ty src_loc)
  = tcAddSrcLoc src_loc	$
    tcMonoType ty `thenTc` \ arg_ty ->
    let
      data_con = mkDataCon (getItsUnique name)
			   (getNameFullName name)
			   [NotMarkedStrict]
			   [{- No labelled fields -}]
		      	   tyvars
		      	   ctxt
		      	   [arg_ty]
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

tcConDecl tycon tyvars ctxt (RecConDecl name fields src_loc)
  = tcAddSrcLoc src_loc	$
    mapTc tcField fields	`thenTc` \ field_label_infos_s ->
    let
      field_label_infos = concat field_label_infos_s
      stricts           = [strict | (_, _, strict) <- field_label_infos]
      arg_tys	        = [ty     | (_, ty, _)     <- field_label_infos]

      field_labels      = [ mkFieldLabel name ty tag 
			  | ((name, ty, _), tag) <- field_label_infos `zip` allFieldLabelTags
			  ]

      data_con = mkDataCon (getItsUnique name)
			   (getNameFullName name)
			   stricts
			   field_labels
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
		      	   arg_tys
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

tcField (field_label_names, bty)
  = tcMonoType (get_ty bty)	`thenTc` \ field_ty ->
    returnTc [(name, field_ty, get_strictness bty) | name <- field_label_names]

tcDataCon tycon tyvars ctxt name btys src_loc
  = tcAddSrcLoc src_loc	$
    let
	stricts = map get_strictness btys
	tys	= map get_ty btys
    in
    mapTc tcMonoType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getItsUnique name)
			   (getNameFullName name)
			   stricts
			   [{- No field labels -}]
		      	   tyvars
		      	   (thinContext arg_tys ctxt)
		      	   arg_tys
		      	   tycon
			-- nullSpecEnv
    in
    returnTc data_con

-- The context for a data constructor should be limited to
-- the type variables mentioned in the arg_tys
thinContext arg_tys ctxt
  = filter in_arg_tys ctxt
  where
      arg_tyvars = tyVarsOfTypes arg_tys
      in_arg_tys (clas,ty) = getTyVar "tcDataCon" ty `elementOfTyVarSet` arg_tyvars
  
get_strictness (Banged ty)   = MarkedStrict
get_strictness (Unbanged ty) = NotMarkedStrict

get_ty (Banged ty)   = ty
get_ty (Unbanged ty) = ty
\end{code}



Errors and contexts
~~~~~~~~~~~~~~~~~~~
\begin{code}
tySynCtxt tycon_name sty
  = ppCat [ppStr "In the type declaration for", ppr sty tycon_name]

tyDataCtxt tycon_name sty
  = ppCat [ppStr "In the data declaration for", ppr sty tycon_name]

tyNewCtxt tycon_name sty
  = ppCat [ppStr "In the newtype declaration for", ppr sty tycon_name]

fieldTypeMisMatch field_name sty
  = ppSep [ppStr "Declared types differ for field", ppr sty field_name]
\end{code}
