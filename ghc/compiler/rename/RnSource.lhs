%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( rnIfaceDecl, rnSourceDecls, rnHsType, rnHsSigType ) where

#include "HsVersions.h"

import RnExpr
import HsSyn
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsPragmas
import HsTypes		( getTyVarName, pprClassAssertion, cmpHsTypes )
import RdrName		( RdrName, isRdrDataCon, rdrNameOcc )
import RdrHsSyn		( RdrNameContext, RdrNameHsType, RdrNameConDecl,
			  extractHsTyVars
			)
import RnHsSyn
import HsCore

import RnBinds		( rnTopBinds, rnMethodBinds, renameSigs, unknownSigErr )
import RnEnv		( bindTyVarsRn, lookupBndrRn, lookupOccRn, 
			  lookupImplicitOccRn, addImplicitOccRn,
			  bindLocalsRn, 
			  bindTyVarsFVRn, bindTyVarsFV2Rn, extendTyVarEnvFVRn,
			  checkDupOrQualNames, checkDupNames,
			  newLocallyDefinedGlobalName, newImportedGlobalName, 
			  newImportedGlobalFromRdrName,
			  newDFunName,
			  FreeVars, emptyFVs, plusFV, plusFVs, unitFV, addOneFV
			)
import RnMonad

import Name		( Name, OccName,
			  ExportFlag(..), Provenance(..), 
			  nameOccName, NamedThing(..),
			  mkDefaultMethodOcc, mkDFunOcc
			)
import NameSet
import BasicTypes	( TopLevelFlag(..) )
import TysWiredIn	( tupleTyCon, unboxedTupleTyCon, listTyCon )
import Type		( funTyCon )
import FiniteMap	( elemFM )
import PrelInfo		( derivingOccurrences, numClass_RDR, 
			  deRefStablePtr_NAME, makeStablePtr_NAME,
			  bindIO_NAME
			)
import Bag		( bagToList )
import List		( partition )
import Outputable
import SrcLoc		( SrcLoc )
import CmdLineOpts	( opt_WarnUnusedMatches )	-- Warn of unused for-all'd tyvars
import UniqFM		( lookupUFM )
import Maybes		( maybeToBool, catMaybes )
import Util
\end{code}

rnDecl `renames' declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:
\begin{enumerate}
\item
Checks that tyvars are used properly. This includes checking
for undefined tyvars, and tyvars in contexts that are ambiguous.
\item
Checks that all variable occurences are defined.
\item 
Checks the (..) etc constraints in the export list.
\end{enumerate}


%*********************************************************
%*							*
\subsection{Value declarations}
%*							*
%*********************************************************

\begin{code}
rnSourceDecls :: [RdrNameHsDecl] -> RnMS s ([RenamedHsDecl], FreeVars)
	-- The decls get reversed, but that's ok

rnSourceDecls decls
  = go emptyFVs [] decls
  where
	-- Fixity decls have been dealt with already; ignore them
    go fvs ds' []          = returnRn (ds', fvs)
    go fvs ds' (FixD _:ds) = go fvs ds' ds
    go fvs ds' (d:ds)      = rnDecl d	`thenRn` \(d', fvs') ->
			     go (fvs `plusFV` fvs') (d':ds') ds

rnIfaceDecl :: RdrNameHsDecl -> RnMS s RenamedHsDecl
rnIfaceDecl d
  = rnDecl d	`thenRn` \ (d', fvs) ->
    returnRn d'
\end{code}


%*********************************************************
%*							*
\subsection{Value declarations}
%*							*
%*********************************************************

\begin{code}
-- rnDecl does all the work
rnDecl :: RdrNameHsDecl -> RnMS s (RenamedHsDecl, FreeVars)

rnDecl (ValD binds) = rnTopBinds binds	`thenRn` \ (new_binds, fvs) ->
		      returnRn (ValD new_binds, fvs)


rnDecl (SigD (IfaceSig name ty id_infos loc))
  = pushSrcLocRn loc $
    lookupBndrRn name		`thenRn` \ name' ->
    rnIfaceType doc_str ty	`thenRn` \ ty' ->

	-- Get the pragma info (if any).
    setModeRn (InterfaceMode Optional) 		$
	-- In all the rest of the signature we read in optional mode,
	-- so that (a) we don't die
    mapRn rnIdInfo id_infos	`thenRn` \ id_infos' -> 
    returnRn (SigD (IfaceSig name' ty' id_infos' loc), emptyFVs)
		-- Don't need free-var info for iface binds
  where
    doc_str = text "the interface signature for" <+> quotes (ppr name)
\end{code}

%*********************************************************
%*							*
\subsection{Type declarations}
%*							*
%*********************************************************

@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its Name representation. In some cases (type signatures of
values), it is even necessary to go over the type first in order to
get the set of tyvars used by it, make an assoc list, and then go over
it again to rename the tyvars! However, we can also do some scoping
checks at the same time.

\begin{code}
rnDecl (TyClD (TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn tycon			    		`thenRn` \ tycon' ->
    bindTyVarsFVRn data_doc tyvars			$ \ tyvars' ->
    rnContext data_doc context 				`thenRn` \ (context', cxt_fvs) ->
    checkDupOrQualNames data_doc con_names		`thenRn_`
    mapAndUnzipRn rnConDecl condecls			`thenRn` \ (condecls', con_fvs_s) ->
    rnDerivs derivings					`thenRn` \ (derivings', deriv_fvs) ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyClD (TyData new_or_data context' tycon' tyvars' condecls' derivings' noDataPragmas src_loc),
	      cxt_fvs `plusFV` plusFVs con_fvs_s `plusFV` deriv_fvs)
  where
    data_doc = text "the data type declaration for" <+> quotes (ppr tycon)
    con_names = map conDeclName condecls

rnDecl (TyClD (TySynonym name tyvars ty src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn name				`thenRn` \ name' ->
    bindTyVarsFVRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType syn_doc ty				`thenRn` \ (ty', ty_fvs) ->
    returnRn (TyClD (TySynonym name' tyvars' ty' src_loc), ty_fvs)
  where
    syn_doc = text "the declaration for type synonym" <+> quotes (ppr name)

rnDecl (TyClD (ClassDecl context cname tyvars sigs mbinds pragmas tname dname src_loc))
  = pushSrcLocRn src_loc $

    lookupBndrRn cname					`thenRn` \ cname' ->

	-- Deal with the implicit tycon and datacon name
	-- They aren't in scope (because they aren't visible to the user)
	-- and what we want to do is simply look them up in the cache;
	-- we jolly well ought to get a 'hit' there!
	-- So the 'Imported' part of this call is not relevant. 
	-- Unclean; but since these two are the only place this happens
	-- I can't work up the energy to do it more beautifully
    newImportedGlobalFromRdrName tname			`thenRn` \ tname' ->
    newImportedGlobalFromRdrName dname			`thenRn` \ dname' ->

	-- Tyvars scope over bindings and context
    bindTyVarsFV2Rn cls_doc tyvars			( \ clas_tyvar_names tyvars' ->

	-- Check the superclasses
    rnContext cls_doc context				`thenRn` \ (context', cxt_fvs) ->

	-- Check the signatures
    let
	    -- First process the class op sigs, then the fixity sigs.
	  (op_sigs, non_op_sigs) = partition isClassOpSig sigs
	  (fix_sigs, non_sigs)   = partition isFixitySig  non_op_sigs
    in
    checkDupOrQualNames sig_doc sig_rdr_names_w_locs 	  `thenRn_` 
    mapAndUnzipRn (rn_op cname' clas_tyvar_names) op_sigs `thenRn` \ (sigs', sig_fvs_s) ->
    mapRn_  (unknownSigErr) non_sigs			  `thenRn_`
    let
     binders = mkNameSet [ nm | (ClassOpSig nm _ _ _) <- sigs' ]
    in
    renameSigs False binders lookupOccRn fix_sigs	  `thenRn` \ (fixs', fix_fvs) ->

	-- Check the methods
    checkDupOrQualNames meth_doc meth_rdr_names_w_locs	`thenRn_`
    rnMethodBinds mbinds				`thenRn` \ (mbinds', meth_fvs) ->

	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    ASSERT(isNoClassPragmas pragmas)
    returnRn (TyClD (ClassDecl context' cname' tyvars' (fixs' ++ sigs') mbinds' NoClassPragmas tname' dname' src_loc),
	      plusFVs sig_fvs_s `plusFV`
	      fix_fvs	        `plusFV`
	      cxt_fvs		`plusFV`
	      meth_fvs
	     )
    )
  where
    cls_doc  = text "the declaration for class" 	<+> ppr cname
    sig_doc  = text "the signatures for class"  	<+> ppr cname
    meth_doc = text "the default-methods for class"	<+> ppr cname

    sig_rdr_names_w_locs  = [(op,locn) | ClassOpSig op _ _ locn <- sigs]
    meth_rdr_names_w_locs = bagToList (collectMonoBinders mbinds)
    meth_rdr_names	  = map fst meth_rdr_names_w_locs

    rn_op clas clas_tyvars sig@(ClassOpSig op maybe_dm ty locn)
      = pushSrcLocRn locn $
 	lookupBndrRn op				`thenRn` \ op_name ->

		-- Check the signature
	rnHsSigType (quotes (ppr op)) ty	`thenRn` \ (new_ty, op_ty_fvs)  ->
	let
	    check_in_op_ty clas_tyvar = checkRn (clas_tyvar `elemNameSet` op_ty_fvs)
					        (classTyVarNotInOpTyErr clas_tyvar sig)
	in
        mapRn_ check_in_op_ty clas_tyvars		 `thenRn_`

		-- Make the default-method name
	let
	    dm_occ = mkDefaultMethodOcc (rdrNameOcc op)
	in
	getModuleRn			`thenRn` \ mod_name ->
	getModeRn			`thenRn` \ mode ->
	(case (mode, maybe_dm) of 
	    (SourceMode, _) | op `elem` meth_rdr_names
		-> 	-- There's an explicit method decl
		   newLocallyDefinedGlobalName mod_name dm_occ 
					       (\_ -> Exported) locn	`thenRn` \ dm_name ->
		   returnRn (Just dm_name)

	    (InterfaceMode _, Just _) 
		-> 	-- Imported class that has a default method decl
		    newImportedGlobalName mod_name dm_occ	`thenRn` \ dm_name ->
		    addOccurrenceName dm_name			`thenRn_`
		    returnRn (Just dm_name)

	    other -> returnRn Nothing
	)					`thenRn` \ maybe_dm_name ->


	returnRn (ClassOpSig op_name maybe_dm_name new_ty locn, op_ty_fvs)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (InstD (InstDecl inst_ty mbinds uprags maybe_dfun src_loc))
  = pushSrcLocRn src_loc $
    rnHsSigType (text "an instance decl") inst_ty	`thenRn` \ (inst_ty', inst_fvs) ->
    let
	inst_tyvars = case inst_ty' of
			HsForAllTy (Just inst_tyvars) _ _ -> inst_tyvars
			other			          -> []
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
    in
    extendTyVarEnvFVRn inst_tyvars		$

	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenRn_`
    rnMethodBinds mbinds			`thenRn` \ (mbinds', meth_fvs) ->
    let 
	binders = mkNameSet (map fst (bagToList (collectMonoBinders mbinds')))

	-- Delete sigs (&report) sigs that aren't allowed inside an
	-- instance decl:
	--
	--  + type signatures
	--  + fixity decls
	--
	(ok_sigs, not_ok_idecl_sigs) = partition okInInstDecl uprags
	
	okInInstDecl (FixSig _)  = False
	okInInstDecl (Sig _ _ _) = False
	okInInstDecl _		 = True
	
    in
      -- You can't have fixity decls & type signatures
      -- within an instance declaration.
    mapRn_ unknownSigErr not_ok_idecl_sigs       `thenRn_`
    renameSigs False binders lookupOccRn ok_sigs `thenRn` \ (new_uprags, prag_fvs) ->
    mkDFunName inst_ty' maybe_dfun src_loc	 `thenRn` \ dfun_name ->
    addOccurrenceName dfun_name			 `thenRn_`
			-- The dfun is not optional, because we use its version number
			-- to identify the version of the instance declaration

	-- The typechecker checks that all the bindings are for the right class.
    returnRn (InstD (InstDecl inst_ty' mbinds' new_uprags (Just dfun_name) src_loc),
	      inst_fvs `plusFV` meth_fvs `plusFV` prag_fvs)
  where
    meth_doc = text "the bindings in an instance declaration"
    meth_names   = bagToList (collectMonoBinders mbinds)
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (DefD (DefaultDecl tys src_loc))
  = pushSrcLocRn src_loc $
    rnHsTypes doc_str tys		`thenRn` \ (tys', fvs) ->
    lookupImplicitOccRn numClass_RDR	`thenRn_` 
    returnRn (DefD (DefaultDecl tys' src_loc), fvs)
  where
    doc_str = text "a `default' declaration"
\end{code}

%*********************************************************
%*							*
\subsection{Foreign declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (ForD (ForeignDecl name imp_exp ty ext_nm cconv src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn name		        `thenRn` \ name' ->
    (case imp_exp of
	FoImport _ | not isDyn -> addImplicitOccRn name'
	FoLabel    -> addImplicitOccRn name'
	FoExport   | isDyn ->
	   addImplicitOccRn makeStablePtr_NAME  `thenRn_`
	   addImplicitOccRn deRefStablePtr_NAME `thenRn_`
	   addImplicitOccRn bindIO_NAME         `thenRn_`
	   returnRn name'
	_ -> returnRn name')		        `thenRn_`
    rnHsSigType fo_decl_msg ty		        `thenRn` \ (ty', fvs) ->
    returnRn (ForD (ForeignDecl name' imp_exp ty' ext_nm cconv src_loc), fvs)
 where
  fo_decl_msg = ptext SLIT("a foreign declaration")
  isDyn	      = isDynamic ext_nm

\end{code}

%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
rnDerivs :: Maybe [RdrName] -> RnMS s (Maybe [Name], FreeVars)

rnDerivs Nothing -- derivs not specified
  = returnRn (Nothing, emptyFVs)

rnDerivs (Just ds)
  = mapRn rn_deriv ds `thenRn` \ derivs ->
    returnRn (Just derivs, foldl addOneFV emptyFVs derivs)
  where
    rn_deriv clas
      = lookupOccRn clas	    `thenRn` \ clas_name ->

		-- Now add extra "occurrences" for things that
		-- the deriving mechanism will later need in order to
		-- generate code for this class.
	case lookupUFM derivingOccurrences clas_name of
		Nothing -> addErrRn (derivingNonStdClassErr clas_name)	`thenRn_`
			   returnRn clas_name

		Just occs -> mapRn_ lookupImplicitOccRn occs	`thenRn_`
			     returnRn clas_name

\end{code}

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ l) = (n,l)

rnConDecl :: RdrNameConDecl -> RnMS s (RenamedConDecl, FreeVars)
rnConDecl (ConDecl name tvs cxt details locn)
  = pushSrcLocRn locn $
    checkConName name			`thenRn_` 
    lookupBndrRn name			`thenRn` \ new_name ->
    bindTyVarsFVRn doc tvs 		$ \ new_tyvars ->
    rnContext doc cxt			`thenRn` \ (new_context, cxt_fvs) ->
    rnConDetails doc locn details	`thenRn` \ (new_details, det_fvs) -> 
    returnRn (ConDecl new_name new_tyvars new_context new_details locn,
	      cxt_fvs `plusFV` det_fvs)
  where
    doc = text "the definition of data constructor" <+> quotes (ppr name)

rnConDetails doc locn (VanillaCon tys)
  = mapAndUnzipRn (rnBangTy doc) tys	`thenRn` \ (new_tys, fvs_s)  ->
    returnRn (VanillaCon new_tys, plusFVs fvs_s)

rnConDetails doc locn (InfixCon ty1 ty2)
  = rnBangTy doc ty1  		`thenRn` \ (new_ty1, fvs1) ->
    rnBangTy doc ty2  		`thenRn` \ (new_ty2, fvs2) ->
    returnRn (InfixCon new_ty1 new_ty2, fvs1 `plusFV` fvs2)

rnConDetails doc locn (NewCon ty mb_field)
  = rnHsType doc ty			`thenRn` \ (new_ty, fvs) ->
    rn_field mb_field			`thenRn` \ new_mb_field  ->
    returnRn (NewCon new_ty new_mb_field, fvs)
  where
    rn_field Nothing  = returnRn Nothing
    rn_field (Just f) =
       lookupBndrRn f	    `thenRn` \ new_f ->
       returnRn (Just new_f)

rnConDetails doc locn (RecCon fields)
  = checkDupOrQualNames doc field_names	`thenRn_`
    mapAndUnzipRn (rnField doc) fields	`thenRn` \ (new_fields, fvs_s) ->
    returnRn (RecCon new_fields, plusFVs fvs_s)
  where
    field_names = [(fld, locn) | (flds, _) <- fields, fld <- flds]

rnField doc (names, ty)
  = mapRn lookupBndrRn names	`thenRn` \ new_names ->
    rnBangTy doc ty		`thenRn` \ (new_ty, fvs) ->
    returnRn ((new_names, new_ty), fvs) 

rnBangTy doc (Banged ty)
  = rnHsType doc ty		`thenRn` \ (new_ty, fvs) ->
    returnRn (Banged new_ty, fvs)

rnBangTy doc (Unbanged ty)
  = rnHsType doc ty 		`thenRn` \ (new_ty, fvs) ->
    returnRn (Unbanged new_ty, fvs)

rnBangTy doc (Unpacked ty)
  = rnHsType doc ty 		`thenRn` \ (new_ty, fvs) ->
    returnRn (Unpacked new_ty, fvs)

-- This data decl will parse OK
--	data T = a Int
-- treating "a" as the constructor.
-- It is really hard to make the parser spot this malformation.
-- So the renamer has to check that the constructor is legal
--
-- We can get an operator as the constructor, even in the prefix form:
--	data T = :% Int Int
-- from interface files, which always print in prefix form

checkConName name
  = checkRn (isRdrDataCon name)
	    (badDataCon name)
\end{code}


%*********************************************************
%*							*
\subsection{Naming a dfun}
%*							*
%*********************************************************

Make a name for the dict fun for an instance decl

\begin{code}
mkDFunName :: RenamedHsType 	-- Instance type
	    -> Maybe RdrName	-- Dfun thing from decl; Nothing <=> source
	    -> SrcLoc
	    -> RnMS s Name

mkDFunName inst_ty maybe_df src_loc
  = newDFunName cl_occ tycon_occ maybe_df src_loc
  where
    (cl_occ, tycon_occ) = get_key inst_ty

    get_key (HsForAllTy _ _ ty)     = get_key ty
    get_key (MonoFunTy _ ty)        = get_key ty
    get_key (MonoDictTy cls (ty:_)) = (nameOccName cls, get_tycon_key ty)

    get_tycon_key (MonoTyVar tv)   = nameOccName (getName tv)
    get_tycon_key (MonoTyApp ty _) = get_tycon_key ty
    get_tycon_key (MonoTupleTy tys True)  = getOccName (tupleTyCon        (length tys))
    get_tycon_key (MonoTupleTy tys False) = getOccName (unboxedTupleTyCon (length tys))
    get_tycon_key (MonoListTy _)   = getOccName listTyCon
    get_tycon_key (MonoFunTy _ _)  = getOccName funTyCon
\end{code}


%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnHsSigType :: SDoc -> RdrNameHsType -> RnMS s (RenamedHsType, FreeVars)
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnHsType (text "the type signature for" <+> doc_str) ty
    
rnIfaceType :: SDoc -> RdrNameHsType -> RnMS s RenamedHsType
rnIfaceType doc ty 
 = rnHsType doc ty	`thenRn` \ (ty,_) ->
   returnRn ty


rnForAll doc forall_tyvars ctxt ty
  = bindTyVarsFVRn doc forall_tyvars			$ \ new_tyvars ->
    rnContext doc ctxt					`thenRn` \ (new_ctxt, cxt_fvs) ->
    rnHsType doc ty					`thenRn` \ (new_ty, ty_fvs) ->
    returnRn (mkHsForAllTy new_tyvars new_ctxt new_ty,
	      cxt_fvs `plusFV` ty_fvs)

-- Check that each constraint mentions at least one of the forall'd type variables
-- Since the forall'd type variables are a subset of the free tyvars
-- of the tau-type part, this guarantees that every constraint mentions
-- at least one of the free tyvars in ty
checkConstraints explicit_forall doc forall_tyvars ctxt ty
   = mapRn check ctxt			`thenRn` \ maybe_ctxt' ->
     returnRn (catMaybes maybe_ctxt')
	    -- Remove problem ones, to avoid duplicate error message.
   where
     check ct@(_,tys)
	| forall_mentioned = returnRn (Just ct)
	| otherwise	   = addErrRn (ctxtErr explicit_forall doc forall_tyvars ct ty)	`thenRn_`
			     returnRn Nothing
        where
	  forall_mentioned = foldr ((||) . any (`elem` forall_tyvars) . extractHsTyVars)
			     False
			     tys


rnHsType :: SDoc -> RdrNameHsType -> RnMS s (RenamedHsType, FreeVars)

rnHsType doc (HsForAllTy Nothing ctxt ty)
	-- From source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalNameEnv		`thenRn` \ name_env ->
    let
	mentioned_tyvars = extractHsTyVars ty
	forall_tyvars    = filter (not . (`elemFM` name_env)) mentioned_tyvars
    in
    checkConstraints False doc forall_tyvars ctxt ty	`thenRn` \ ctxt' ->
    rnForAll doc (map UserTyVar forall_tyvars) ctxt' ty

rnHsType doc (HsForAllTy (Just forall_tyvars) ctxt ty)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are a subset of the
	-- free tyvars in the tau-type part
	-- That's only a warning... unless the tyvar is constrained by a 
	-- context in which case it's an error
  = let
	mentioned_tyvars      = extractHsTyVars ty
	constrained_tyvars    = [tv | (_,tys) <- ctxt,
				      ty <- tys,
				      tv <- extractHsTyVars ty]
	dubious_guys	      = filter (`notElem` mentioned_tyvars) forall_tyvar_names
	(bad_guys, warn_guys) = partition (`elem` constrained_tyvars) dubious_guys
	forall_tyvar_names    = map getTyVarName forall_tyvars
    in
    mapRn_ (forAllErr doc ty) bad_guys 				`thenRn_`
    mapRn_ (forAllWarn doc ty) warn_guys			`thenRn_`
    checkConstraints True doc forall_tyvar_names ctxt ty	`thenRn` \ ctxt' ->
    rnForAll doc forall_tyvars ctxt' ty

rnHsType doc (MonoTyVar tyvar)
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (MonoTyVar tyvar', unitFV tyvar')

rnHsType doc (MonoFunTy ty1 ty2)
  = rnHsType doc ty1	`thenRn` \ (ty1', fvs1) ->
    rnHsType doc ty2	`thenRn` \ (ty2', fvs2) ->
    returnRn (MonoFunTy ty1' ty2', fvs1 `plusFV` fvs2)

rnHsType doc (MonoListTy ty)
  = addImplicitOccRn listTyCon_name		`thenRn_`
    rnHsType doc ty				`thenRn` \ (ty', fvs) ->
    returnRn (MonoListTy ty', fvs `addOneFV` listTyCon_name)

rnHsType doc (MonoTupleTy tys boxed)
  = addImplicitOccRn tup_con_name	`thenRn_`
    rnHsTypes doc tys			`thenRn` \ (tys', fvs) ->
    returnRn (MonoTupleTy tys' boxed, fvs `addOneFV` tup_con_name)
  where
    tup_con_name = tupleTyCon_name boxed (length tys)

rnHsType doc (MonoTyApp ty1 ty2)
  = rnHsType doc ty1		`thenRn` \ (ty1', fvs1) ->
    rnHsType doc ty2		`thenRn` \ (ty2', fvs2) ->
    returnRn (MonoTyApp ty1' ty2', fvs1 `plusFV` fvs2)

rnHsType doc (MonoDictTy clas tys)
  = lookupOccRn clas		`thenRn` \ clas' ->
    rnHsTypes doc tys		`thenRn` \ (tys', fvs) ->
    returnRn (MonoDictTy clas' tys', fvs `addOneFV` clas')

rnHsTypes doc tys
  = mapAndUnzipRn (rnHsType doc) tys	`thenRn` \ (tys, fvs_s) ->
    returnRn (tys, plusFVs fvs_s)
\end{code}


\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnMS s (RenamedContext, FreeVars)

rnContext doc ctxt
  = mapAndUnzipRn rn_ctxt ctxt		`thenRn` \ (theta, fvs_s) ->
    let
	(_, dup_asserts) = removeDups cmp_assert theta
    in
	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    mapRn_ (addWarnRn . dupClassAssertWarn theta) dup_asserts	`thenRn_`

    returnRn (theta, plusFVs fvs_s)
  where
    rn_ctxt (clas, tys)
      =	lookupOccRn clas		`thenRn` \ clas_name ->
	rnHsTypes doc tys		`thenRn` \ (tys', fvs) ->
	returnRn ((clas_name, tys'), fvs `addOneFV` clas_name)

    cmp_assert (c1,tys1) (c2,tys2)
      = (c1 `compare` c2) `thenCmp` (cmpHsTypes compare tys1 tys2)
\end{code}


%*********************************************************
%*							*
\subsection{IdInfo}
%*							*
%*********************************************************

\begin{code}
rnIdInfo (HsStrictness str) = returnRn (HsStrictness str)

rnIdInfo (HsWorker worker cons)
	-- The sole purpose of the "cons" field is so that we can mark the 
	-- constructors needed to build the wrapper as "needed", so that their
	-- data type decl will be slurped in. After that their usefulness is 
	-- o'er, so we just put in the empty list.
  = lookupOccRn worker			`thenRn` \ worker' ->
    mapRn lookupOccRn cons		`thenRn_` 
    returnRn (HsWorker worker' [])

rnIdInfo (HsUnfold inline (Just expr))	= rnCoreExpr expr	`thenRn` \ expr' ->
				  	  returnRn (HsUnfold inline (Just expr'))
rnIdInfo (HsUnfold inline Nothing)	= returnRn (HsUnfold inline Nothing)
rnIdInfo (HsArity arity)	= returnRn (HsArity arity)
rnIdInfo (HsUpdate update)	= returnRn (HsUpdate update)
rnIdInfo (HsNoCafRefs)		= returnRn (HsNoCafRefs)
rnIdInfo (HsCprInfo cpr_info)	= returnRn (HsCprInfo cpr_info)
rnIdInfo (HsSpecialise tyvars tys expr)
  = bindTyVarsRn doc tyvars	$ \ tyvars' ->
    rnCoreExpr expr		`thenRn` \ expr' ->
    mapRn (rnIfaceType doc) tys	`thenRn` \ tys' ->
    returnRn (HsSpecialise tyvars' tys' expr')
  where
    doc = text "Specialise in interface pragma"
\end{code}

UfCore expressions.

\begin{code}
rnCoreExpr (UfType ty)
  = rnIfaceType (text "unfolding type") ty	`thenRn` \ ty' ->
    returnRn (UfType ty')

rnCoreExpr (UfVar v)
  = lookupOccRn v 	`thenRn` \ v' ->
    returnRn (UfVar v')

rnCoreExpr (UfCon con args) 
  = rnUfCon con			`thenRn` \ con' ->
    mapRn rnCoreExpr args	`thenRn` \ args' ->
    returnRn (UfCon con' args')

rnCoreExpr (UfTuple con args) 
  = lookupOccRn con		`thenRn` \ con' ->
    mapRn rnCoreExpr args	`thenRn` \ args' ->
    returnRn (UfTuple con' args')

rnCoreExpr (UfApp fun arg)
  = rnCoreExpr fun		`thenRn` \ fun' ->
    rnCoreExpr arg		`thenRn` \ arg' ->
    returnRn (UfApp fun' arg')

rnCoreExpr (UfCase scrut bndr alts) 
  = rnCoreExpr scrut			`thenRn` \ scrut' ->
    bindLocalsRn "a UfCase" [bndr]	$ \ [bndr'] ->
    mapRn rnCoreAlt alts		`thenRn` \ alts' ->
    returnRn (UfCase scrut' bndr' alts')

rnCoreExpr (UfNote note expr) 
  = rnNote note			`thenRn` \ note' ->
    rnCoreExpr expr		`thenRn` \ expr' ->
    returnRn  (UfNote note' expr') 

rnCoreExpr (UfLam bndr body)
  = rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLam bndr' body')

rnCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = rnCoreExpr rhs		`thenRn` \ rhs' ->
    rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLet (UfNonRec bndr' rhs') body')

rnCoreExpr (UfLet (UfRec pairs) body)
  = rnCoreBndrs bndrs		$ \ bndrs' ->
    mapRn rnCoreExpr rhss	`thenRn` \ rhss' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfLet (UfRec (bndrs' `zip` rhss')) body')
  where
    (bndrs, rhss) = unzip pairs
\end{code}

\begin{code}
rnCoreBndr (UfValBinder name ty) thing_inside
  = rnIfaceType (text str) ty	`thenRn` \ ty' ->
    bindLocalsRn str [name]	$ \ [name'] ->
    thing_inside (UfValBinder name' ty')
  where
    str = "unfolding id"
    
rnCoreBndr (UfTyBinder name kind) thing_inside
  = bindLocalsRn "an unfolding tyvar" [name] $ \ [name'] ->
    thing_inside (UfTyBinder name' kind)
    
rnCoreBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapRn (rnIfaceType (text str)) tys	`thenRn` \ tys' ->
    bindLocalsRn str names		$ \ names' ->
    thing_inside (zipWith UfValBinder names' tys')
  where
    str   = "unfolding id"
    names = map (\ (UfValBinder name _ ) -> name) bndrs
    tys   = map (\ (UfValBinder _    ty) -> ty)   bndrs
\end{code}    

\begin{code}
rnCoreAlt (con, bndrs, rhs)
  = rnUfCon con					`thenRn` \ con' ->
    bindLocalsRn "an unfolding alt" bndrs	$ \ bndrs' ->
    rnCoreExpr rhs				`thenRn` \ rhs' ->
    returnRn (con', bndrs', rhs')


rnNote (UfCoerce ty)
  = rnIfaceType (text "unfolding coerce") ty	`thenRn` \ ty' ->
    returnRn (UfCoerce ty')

rnNote (UfSCC cc)   = returnRn (UfSCC cc)
rnNote UfInlineCall = returnRn UfInlineCall


rnUfCon UfDefault
  = returnRn UfDefault

rnUfCon (UfDataCon con)
  = lookupOccRn con		`thenRn` \ con' ->
    returnRn (UfDataCon con')

rnUfCon (UfLitCon lit)
  = returnRn (UfLitCon lit)

rnUfCon (UfLitLitCon lit ty)
  = rnIfaceType (text "litlit") ty		`thenRn` \ ty' ->
    returnRn (UfLitLitCon lit ty')

rnUfCon (UfPrimOp op)
  = lookupOccRn op		`thenRn` \ op' ->
    returnRn (UfPrimOp op')

rnUfCon (UfCCallOp str is_dyn casm gc)
  = returnRn (UfCCallOp str is_dyn casm gc)
\end{code}

%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
derivingNonStdClassErr clas
  = hsep [ptext SLIT("non-standard class"), ppr clas, ptext SLIT("in deriving clause")]

classTyVarNotInOpTyErr clas_tyvar sig
  = hang (hsep [ptext SLIT("Class type variable"),
		       quotes (ppr clas_tyvar),
		       ptext SLIT("does not appear in method signature")])
	 4 (ppr sig)

dupClassAssertWarn ctxt (assertion : dups)
  = sep [hsep [ptext SLIT("Duplicate class assertion"), 
	       quotes (pprClassAssertion assertion),
	       ptext SLIT("in the context:")],
	 nest 4 (pprContext ctxt <+> ptext SLIT("..."))]

badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

forAllWarn doc ty tyvar
  | not opt_WarnUnusedMatches = returnRn ()
  | otherwise
  = addWarnRn (
      sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
	   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
      $$
      (ptext SLIT("In") <+> doc))

forAllErr doc ty tyvar
  = addErrRn (
      sep [ptext SLIT("The constrained type variable") <+> quotes (ppr tyvar),
	   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
      $$
      (ptext SLIT("In") <+> doc))

ctxtErr explicit_forall doc tyvars constraint ty
  = sep [ptext SLIT("The constraint") <+> quotes (pprClassAssertion constraint) <+>
		   ptext SLIT("does not mention any of"),
	 if explicit_forall then
 	   nest 4 (ptext SLIT("the universally quantified type variables") <+> braces (interpp'SP tyvars))
	 else
	   nest 4 (ptext SLIT("the type variables in the type") <+> quotes (ppr ty))
    ]
    $$
    (ptext SLIT("In") <+> doc)
\end{code}
