%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcTyDecls]{Typecheck type declarations}

\begin{code}
#include "HsVersions.h"

module TcTyDecls (
	tcTyDecl,
	tcConDecl,
	mkDataBinds
    ) where

import Ubiq{-uitous-}

import HsSyn		( TyDecl(..), ConDecl(..), BangType(..), HsExpr(..), 
			  Match(..), GRHSsAndBinds(..), GRHS(..), OutPat(..), 
			  HsBinds(..), HsLit, Stmt, Qual, ArithSeqInfo,
			  PolyType, Fake, InPat,
			  Bind(..), MonoBinds(..), Sig, 
			  MonoType )
import RnHsSyn		( RenamedTyDecl(..), RenamedConDecl(..),
			  RnName{-instance Outputable-}
			)
import TcHsSyn		( mkHsTyLam, mkHsDictLam, tcIdType,
			  TcHsBinds(..), TcIdOcc(..)
			)
import Inst		( newDicts, InstOrigin(..), Inst )
import TcMonoType	( tcMonoTypeKind, tcMonoType, tcPolyType, tcContext )
import TcSimplify	( tcSimplifyThetas )
import TcType		( tcInstTyVars, tcInstType, tcInstId )
import TcEnv		( tcLookupTyCon, tcLookupTyVar, tcLookupClass,
			  newLocalId, newLocalIds, tcLookupClassByKey
			)
import TcMonad		hiding ( rnMtoTcM )
import TcKind		( TcKind, unifyKind, mkTcArrowKind, mkTcTypeKind )

import PprType		( GenClass, GenType{-instance Outputable-} )
import Class		( GenClass{-instance Eq-}, classInstEnv )
import Id		( mkDataCon, dataConSig, mkRecordSelId, idType,
			  dataConFieldLabels, dataConStrictMarks,
			  StrictnessMark(..),
			  GenId{-instance NamedThing-}
			)
import FieldLabel
import Kind		( Kind, mkArrowKind, mkBoxedTypeKind )
import SpecEnv		( SpecEnv(..), nullSpecEnv )
import Name		( nameSrcLoc, isLocallyDefinedName, getSrcLoc,
			  Name{-instance Ord3-}
			)
import Outputable	( Outputable(..), interpp'SP )
import Pretty
import TyCon		( TyCon, NewOrData(..), mkSynTyCon, mkDataTyCon, isDataTyCon, 
			  isNewTyCon, isSynTyCon, tyConDataCons
			)
import Type		( GenType, -- instances
			  typeKind, getTyVar, tyVarsOfTypes, eqTy, splitSigmaTy,
			  applyTyCon, mkTyVarTys, mkForAllTys, mkFunTy,
			  splitFunTy, mkTyVarTy, getTyVar_maybe
			)
import PprType		( GenTyVar{-instance Outputable-}{-ToDo:possibly rm-} )
import TyVar		( tyVarKind, elementOfTyVarSet, GenTyVar{-instance Eq-} )
import Unique		( Unique {- instance Eq -}, evalClassKey )
import UniqSet		( emptyUniqSet, mkUniqSet, uniqSetToList, unionManyUniqSets, UniqSet(..) )
import Util		( equivClasses, zipEqual, nOfThem, panic, assertPanic )
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
	final_tycon_kind = foldr (mkArrowKind . tyVarKind) mkBoxedTypeKind rec_tyvars

	tycon = mkDataTyCon (getName tycon_name)
			    final_tycon_kind
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

Generating constructor/selector bindings for data declarations
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
mkDataBinds :: [TyCon] -> TcM s ([Id], TcHsBinds s)
mkDataBinds [] = returnTc ([], EmptyBinds)
mkDataBinds (tycon : tycons) 
  | isSynTyCon tycon = mkDataBinds tycons
  | otherwise	     = mkDataBinds_one tycon	`thenTc` \ (ids1, b1) ->
		       mkDataBinds tycons	`thenTc` \ (ids2, b2) ->
		       returnTc (ids1++ids2, b1 `ThenBinds` b2)

mkDataBinds_one tycon
  = ASSERT( isDataTyCon tycon || isNewTyCon tycon )
    mapAndUnzipTc mkConstructor data_cons		`thenTc` \ (con_ids, con_binds) ->	
    mapAndUnzipTc (mkRecordSelector tycon) groups	`thenTc` \ (sel_ids, sel_binds) ->
    returnTc (con_ids ++ sel_ids, 
	      SingleBind $ NonRecBind $
	      foldr AndMonoBinds 
		    (foldr AndMonoBinds EmptyMonoBinds sel_binds)
		    con_binds
    )
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

We're going to build a constructor that looks like:

	data (Data a, C b) =>  T a b = T1 !a !Int b

	T1 = /\ a b -> 
	     \d1::Data a, d2::C b ->
	     \p q r -> case p of { p ->
		       case q of { q ->
		       HsCon T1 [a,b] [p,q,r]}}

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
mkConstructor con_id
  | not (isLocallyDefinedName (getName con_id))
  = returnTc (con_id, EmptyMonoBinds)

  | otherwise	-- It is locally defined
  = tcInstId con_id			`thenNF_Tc` \ (tc_tyvars, tc_theta, tc_tau) ->
    newDicts DataDeclOrigin tc_theta	`thenNF_Tc` \ (_, dicts) ->
    let
	(tc_arg_tys, tc_result_ty) = splitFunTy tc_tau
	n_args = length tc_arg_tys
    in
    newLocalIds (nOfThem n_args SLIT("con")) tc_arg_tys	`thenNF_Tc` \ args ->

	-- Check that all the types of all the strict arguments are in Eval
    tcLookupClassByKey evalClassKey	`thenNF_Tc` \ eval_clas ->
    let
	(_,theta,tau) = splitSigmaTy (idType con_id)
	(arg_tys, _)  = splitFunTy tau
	strict_marks  = dataConStrictMarks con_id
	eval_theta    = [ (eval_clas,arg_ty) 
		        | (arg_ty, MarkedStrict) <- zipEqual "strict_args" 
							arg_tys strict_marks
			]
    in
    tcSimplifyThetas classInstEnv theta eval_theta	`thenTc` \ eval_theta' ->
    checkTc (null eval_theta')
	    (missingEvalErr con_id eval_theta')		`thenTc_`


	-- Build the data constructor
    let
	con_rhs = mkHsTyLam tc_tyvars $
		  mkHsDictLam dicts $
		  mk_pat_match args $
		  mk_case (zipEqual "strict_args" args strict_marks) $
		  HsCon con_id (mkTyVarTys tc_tyvars) (map HsVar args)

	mk_pat_match []         body = body
	mk_pat_match (arg:args) body = HsLam $
				       PatMatch (VarPat arg) $
				       SimpleMatch (mk_pat_match args body)

	mk_case [] body = body
	mk_case ((arg,MarkedStrict):args) body = HsCase (HsVar arg) 
							 [PatMatch (VarPat arg) $
						          SimpleMatch (mk_case args body)]
							 src_loc
	mk_case (_:args) body = mk_case args body

	src_loc = nameSrcLoc (getName con_id)
    in

    returnTc (con_id, VarMonoBind (RealId con_id) con_rhs)		 
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
mkRecordSelector tycon fields@((first_con, first_field_label) : other_fields)
  = let
	field_ty   = fieldLabelType first_field_label
	field_name = fieldLabelName first_field_label
	other_tys  = [fieldLabelType fl | (_, fl) <- other_fields]
	(tyvars, _, _, _) = dataConSig first_con
        data_ty  = applyTyCon tycon (mkTyVarTys tyvars)
	-- tyvars of first_con may be free in field_ty
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
      data_ty' = applyTyCon tycon tyvar_tys
    in
    newLocalId SLIT("x") field_ty'	`thenNF_Tc` \ field_id ->
    newLocalId SLIT("r") data_ty'	`thenNF_Tc` \ record_id ->

	-- Now build the selector
    let
      selector_ty :: Type
      selector_ty  = mkForAllTys tyvars $	
		     mkFunTy data_ty $
		     field_ty
      
      selector_id :: Id
      selector_id = mkRecordSelId first_field_label selector_ty

	-- HsSyn is dreadfully verbose for defining the selector!
      selector_rhs = mkHsTyLam tyvars' $
		     HsLam $
		     PatMatch (VarPat record_id) $
		     SimpleMatch $
		     selector_body

      selector_body = HsCase (HsVar record_id) (map mk_match fields) (getSrcLoc tycon)

      mk_match (con_id, field_label) 
    	= PatMatch (RecPat con_id data_ty' [(selector_id, VarPat field_id, False)]) $
	  SimpleMatch $
    	  HsVar field_id
    in
    returnTc (selector_id, if isLocallyDefinedName (getName tycon)
			   then VarMonoBind (RealId selector_id) selector_rhs
			   else EmptyMonoBinds)
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
      data_con = mkDataCon (getName name)
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

      field_labels      = [ mkFieldLabel (getName name) ty tag 
			  | ((name, ty, _), tag) <- field_label_infos `zip` allFieldLabelTags ]

      data_con = mkDataCon (getName name)
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
  = tcPolyType (get_pty bty)	`thenTc` \ field_ty ->
    returnTc [(name, field_ty, get_strictness bty) | name <- field_label_names]

tcDataCon tycon tyvars ctxt name btys src_loc
  = tcAddSrcLoc src_loc	$
    let
	stricts = map get_strictness btys
	tys	= map get_pty btys
    in
    mapTc tcPolyType tys `thenTc` \ arg_tys ->
    let
      data_con = mkDataCon (getName name)
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
  
get_strictness (Banged   _) = MarkedStrict
get_strictness (Unbanged _) = NotMarkedStrict

get_pty (Banged ty)   = ty
get_pty (Unbanged ty) = ty
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

missingEvalErr con eval_theta sty
  = ppCat [ppStr "Missing Eval context for constructor", 
	   ppQuote (ppr sty con),
	   ppStr ":", ppr sty eval_theta]
\end{code}
