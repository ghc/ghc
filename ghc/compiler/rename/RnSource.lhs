%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnSource]{Main pass of renamer}

\begin{code}
#include "HsVersions.h"

module RnSource ( rnSource, rnTyDecl, rnClassDecl, rnInstDecl, rnPolyType ) where

import Ubiq
import RnLoop		-- *check* the RnPass/RnExpr/RnBinds loop-breaking

import HsSyn
import HsPragmas
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnBinds		( rnTopBinds, rnMethodBinds )
import RnUtils		( lubExportFlag )

import Bag		( emptyBag, unitBag, consBag, unionManyBags, unionBags, listToBag, bagToList )
import Class		( derivableClassKeys )
import FiniteMap	( emptyFM, lookupFM, addListToFM_C )
import ListSetOps	( unionLists, minusList )
import Maybes		( maybeToBool, catMaybes )
import Name		( Name, isLocallyDefined, isLexVarId, getLocalName, ExportFlag(..), 
			  nameImportFlag, RdrName, pprNonSym )
import Outputable -- ToDo:rm
import PprStyle -- ToDo:rm 
import Pretty
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import UniqFM		( emptyUFM, addListToUFM, addListToUFM_C, listToUFM, lookupUFM, eltsUFM )
import UniqSet		( UniqSet(..) )
import Util		( isIn, isn'tIn, sortLt, removeDups, cmpPString, panic, assertPanic, pprTrace{-ToDo:rm-} )

\end{code}

rnSource `renames' the source module and export list.
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


\begin{code}
rnSource :: [Module]
	 -> Bag (Module,RnName)		-- unqualified imports from module
	 -> Bag RenamedFixityDecl	-- fixity info for imported names
	 -> RdrNameHsModule
	 -> RnM s (RenamedHsModule,
		   Name -> ExportFlag,		-- export info
		   Bag (RnName, RdrName))	-- occurrence info

rnSource imp_mods unqual_imps imp_fixes
	(HsModule mod version exports _ fixes
	   ty_decls specdata_sigs class_decls
	   inst_decls specinst_sigs defaults
	   binds _ src_loc)

  = pushSrcLocRn src_loc $

    rnExports (mod:imp_mods) unqual_imps exports	`thenRn` \ exported_fn ->
    rnFixes fixes					`thenRn` \ src_fixes ->
    let
	pair_name inf@(InfixL n _) = (n, inf)
	pair_name inf@(InfixR n _) = (n, inf)
	pair_name inf@(InfixN n _) = (n, inf)

	imp_fixes_fm = listToUFM (map pair_name (bagToList imp_fixes))
	all_fixes_fm = addListToUFM imp_fixes_fm (map pair_name src_fixes)
    in
    setExtraRn all_fixes_fm $

    mapRn rnTyDecl	ty_decls	`thenRn` \ new_ty_decls ->
    mapRn rnSpecDataSig specdata_sigs	`thenRn` \ new_specdata_sigs ->
    mapRn rnClassDecl	class_decls	`thenRn` \ new_class_decls ->
    mapRn rnInstDecl	inst_decls	`thenRn` \ new_inst_decls ->
    mapRn rnSpecInstSig specinst_sigs   `thenRn` \ new_specinst_sigs ->
    rnDefaultDecl	defaults	`thenRn` \ new_defaults ->
    rnTopBinds binds			`thenRn` \ new_binds ->

    getOccurrenceUpRn			`thenRn` \ occ_info ->

    returnRn (
	      HsModule mod version
		trashed_exports trashed_imports	src_fixes
		new_ty_decls new_specdata_sigs new_class_decls
		new_inst_decls new_specinst_sigs new_defaults
		new_binds [] src_loc,
	      exported_fn,
	      occ_info
	     )
  where
    trashed_exports = {-trace "rnSource:trashed_exports"-} Nothing
    trashed_imports = {-trace "rnSource:trashed_imports"-} []
\end{code}


%*********************************************************
%*							*
\subsection{Export list}
%*							*
%*********************************************************

\begin{code}
rnExports :: [Module]
	  -> Bag (Module,RnName)
	  -> Maybe [RdrNameIE]
	  -> RnM s (Name -> ExportFlag)

rnExports mods unqual_imps Nothing
  = returnRn (\n -> if isLocallyDefined n then ExportAll else NotExported)

rnExports mods unqual_imps (Just exps)
  = mapAndUnzipRn (rnIE mods) exps `thenRn` \ (mod_maybes, exp_bags) ->
    let 
        exp_names = bagToList (unionManyBags exp_bags)
        exp_mods  = catMaybes mod_maybes

	-- Warn for duplicate names and modules
	(uniq_exp_names, dup_names) = removeDups cmp_fst exp_names
	(uniq_exp_mods,  dup_mods)  = removeDups cmpPString exp_mods
	cmp_fst (x,_) (y,_) = x `cmp` y

	-- Build finite map of exported names to export flag
	exp_map0 = addListToUFM_C lub_expflag emptyUFM (map pair_fst uniq_exp_names)
	exp_map1 = foldl add_mod_names exp_map0 uniq_exp_mods

	mod_fm = addListToFM_C unionBags emptyFM
		 [(mod, unitBag (getName rn, nameImportFlag (getName rn)))
		  | (mod,rn) <- bagToList unqual_imps]

        add_mod_names exp_map mod
	  = case lookupFM mod_fm mod of
	      Nothing        -> exp_map
	      Just mod_names -> addListToUFM_C lub_expflag exp_map (map pair_fst (bagToList mod_names))

	pair_fst p@(f,_) = (f,p)
	lub_expflag (n, flag1) (_, flag2) = (n, lubExportFlag flag1 flag2)

	-- Check for exporting of duplicate local names
	exp_locals = [(getLocalName n, n) | (n,_) <- eltsUFM exp_map1]
	(_, dup_locals) = removeDups cmp_local exp_locals
	cmp_local (x,_) (y,_) = x `cmpPString` y


	-- Build export flag function
	exp_fn n = case lookupUFM exp_map1 n of
		     Nothing       -> NotExported
		     Just (_,flag) -> flag
    in
    getSrcLocRn 						`thenRn` \ src_loc ->
    mapRn (addWarnRn . dupNameExportWarn   src_loc) dup_names 	`thenRn_`
    mapRn (addWarnRn . dupModuleExportWarn src_loc) dup_mods 	`thenRn_`
    mapRn (addErrRn  . dupLocalsExportErr  src_loc) dup_locals 	`thenRn_`
    returnRn exp_fn


rnIE mods (IEVar name)
  = lookupValue name	`thenRn` \ rn ->
    checkIEVar rn	`thenRn` \ exps ->
    returnRn (Nothing, exps)
  where
    checkIEVar (RnName n)         = returnRn (unitBag (n,ExportAll))
    checkIEVar (RnUnbound _)      = returnRn emptyBag
    checkIEVar rn@(RnClassOp _ _) = getSrcLocRn `thenRn` \ src_loc ->
			            failButContinueRn emptyBag (classOpExportErr rn src_loc)
    checkIEVar rn                 = panic "checkIEVar"

rnIE mods (IEThingAbs name)
  = lookupTyConOrClass name	`thenRn` \ rn ->
    checkIEAbs rn		`thenRn` \ exps ->
    returnRn (Nothing, exps)
  where
    checkIEAbs (RnSyn n)     = returnRn (unitBag (n,ExportAbs))
    checkIEAbs (RnData n _)  = returnRn (unitBag (n,ExportAbs))
    checkIEAbs (RnClass n _) = returnRn (unitBag (n,ExportAbs))
    checkIEAbs (RnUnbound _) = returnRn emptyBag
    checkIEAbs rn            = panic "checkIEAbs"

rnIE mods (IEThingAll name)
  = lookupTyConOrClass name	`thenRn` \ rn ->
    checkIEAll rn		`thenRn` \ exps ->
    checkImportAll rn           `thenRn_`
    returnRn (Nothing, exps)
  where
    checkIEAll (RnData n cons) = returnRn (consBag (exp_all n) (listToBag (map exp_all cons)))
    checkIEAll (RnClass n ops) = returnRn (consBag (exp_all n) (listToBag (map exp_all ops)))
    checkIEAll (RnUnbound _)   = returnRn emptyBag
    checkIEAll rn@(RnSyn _)    = getSrcLocRn `thenRn` \ src_loc ->
			         warnAndContinueRn emptyBag (synAllExportErr rn src_loc)
    checkIEAll rn              = panic "checkIEAll"

    exp_all n = (n, ExportAll)

rnIE mods (IEThingWith name names)
  = lookupTyConOrClass name	`thenRn` \ rn ->
    mapRn lookupValue names	`thenRn` \ rns ->
    checkIEWith rn rns		`thenRn` \ exps ->
    checkImportAll rn    	`thenRn_`
    returnRn (Nothing, exps)
  where
    checkIEWith rn@(RnData n cons) rns
		 | same_names cons rns = returnRn (consBag (exp_all n) (listToBag (map exp_all cons)))
		 | otherwise 	       = rnWithErr "constructrs" rn cons rns 
    checkIEWith rn@(RnClass n ops) rns
		 | same_names ops rns  = returnRn (consBag (exp_all n) (listToBag (map exp_all ops)))
		 | otherwise 	       = rnWithErr "class ops" rn ops rns
    checkIEWith (RnUnbound _)      rns = returnRn emptyBag
    checkIEWith rn@(RnSyn _)       rns = getSrcLocRn `thenRn` \ src_loc ->
			                 failButContinueRn emptyBag (synAllExportErr rn src_loc)
    checkIEWith rn                 rns = panic "checkIEWith"

    exp_all n = (n, ExportAll)

    same_names has rns
      = all (not.isRnUnbound) rns &&
	sortLt (<) (map uniqueOf has) == sortLt (<) (map uniqueOf rns)

    rnWithErr str rn has rns
      = getSrcLocRn `thenRn` \ src_loc ->
	failButContinueRn emptyBag (withExportErr str rn has rns src_loc)

rnIE mods (IEModuleContents mod)
  | isIn "rnIE:IEModule" mod mods
  = returnRn (Just mod, emptyBag)
  | otherwise
  = getSrcLocRn `thenRn` \ src_loc ->
    failButContinueRn (Nothing,emptyBag) (badModExportErr mod src_loc)


checkImportAll rn 
  = case nameImportFlag (getName rn) of
      ExportAll -> returnRn ()
      exp	-> getSrcLocRn `thenRn` \ src_loc ->
		   addErrRn (importAllErr rn src_loc)
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
rnTyDecl :: RdrNameTyDecl -> RnM_Fixes s RenamedTyDecl

rnTyDecl (TyData context tycon tyvars condecls derivings pragmas src_loc)
  = pushSrcLocRn src_loc $
    lookupTyCon tycon		       `thenRn` \ tycon' ->
    mkTyVarNamesEnv src_loc tyvars     `thenRn` \ (tv_env, tyvars') ->
    rnContext tv_env context	       `thenRn` \ context' ->
    rnConDecls tv_env condecls	       `thenRn` \ condecls' ->
    rn_derivs tycon' src_loc derivings `thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyData context' tycon' tyvars' condecls' derivings' noDataPragmas src_loc)

rnTyDecl (TyNew context tycon tyvars condecl derivings pragmas src_loc)
  = pushSrcLocRn src_loc $
    lookupTyCon tycon		      `thenRn` \ tycon' ->
    mkTyVarNamesEnv src_loc tyvars    `thenRn` \ (tv_env, tyvars') ->
    rnContext tv_env context	      `thenRn` \ context' ->
    rnConDecls tv_env condecl	      `thenRn` \ condecl' ->
    rn_derivs tycon' src_loc derivings `thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyNew context' tycon' tyvars' condecl' derivings' noDataPragmas src_loc)

rnTyDecl (TySynonym name tyvars ty src_loc)
  = pushSrcLocRn src_loc $
    lookupTyCon name		    `thenRn` \ name' ->
    mkTyVarNamesEnv src_loc tyvars  `thenRn` \ (tv_env, tyvars') ->
    rnMonoType tv_env ty	    `thenRn` \ ty' ->
    returnRn (TySynonym name' tyvars' ty' src_loc)

rn_derivs tycon2 locn Nothing -- derivs not specified
  = returnRn Nothing

rn_derivs tycon2 locn (Just ds)
  = mapRn (rn_deriv tycon2 locn) ds `thenRn` \ derivs ->
    returnRn (Just derivs)
  where
    rn_deriv tycon2 locn clas
      = lookupClass clas	    `thenRn` \ clas_name ->
	addErrIfRn (uniqueOf clas_name `not_elem` derivableClassKeys)
		   (derivingNonStdClassErr clas locn)
				    `thenRn_`
	returnRn clas_name
      where
	not_elem = isn'tIn "rn_deriv"
\end{code}

@rnConDecls@ uses the `global name function' to create a new
constructor in which local names have been replaced by their original
names, reporting any unknown names.

\begin{code}
rnConDecls :: TyVarNamesEnv
	   -> [RdrNameConDecl]
	   -> RnM_Fixes s [RenamedConDecl]

rnConDecls tv_env con_decls
  = mapRn rn_decl con_decls
  where
    rn_decl (ConDecl name tys src_loc)
      = pushSrcLocRn src_loc $
	lookupValue name	`thenRn` \ new_name ->
	mapRn rn_bang_ty tys	`thenRn` \ new_tys  ->
	returnRn (ConDecl new_name new_tys src_loc)

    rn_decl (ConOpDecl ty1 op ty2 src_loc)
      = pushSrcLocRn src_loc $
	lookupValue op		`thenRn` \ new_op  ->
	rn_bang_ty ty1  	`thenRn` \ new_ty1 ->
	rn_bang_ty ty2  	`thenRn` \ new_ty2 ->
	returnRn (ConOpDecl new_ty1 new_op new_ty2 src_loc)

    rn_decl (NewConDecl name ty src_loc)
      = pushSrcLocRn src_loc $
	lookupValue name	`thenRn` \ new_name ->
	rn_mono_ty ty		`thenRn` \ new_ty  ->
	returnRn (NewConDecl new_name new_ty src_loc)

    rn_decl (RecConDecl con fields src_loc)
      = panic "rnConDecls:RecConDecl"

    ----------
    rn_mono_ty = rnMonoType tv_env

    rn_bang_ty (Banged ty)
      = rn_mono_ty ty `thenRn` \ new_ty ->
	returnRn (Banged new_ty)
    rn_bang_ty (Unbanged ty)
      = rn_mono_ty ty `thenRn` \ new_ty ->
	returnRn (Unbanged new_ty)
\end{code}

%*********************************************************
%*							 *
\subsection{SPECIALIZE data pragmas}
%*							 *
%*********************************************************

\begin{code}
rnSpecDataSig :: RdrNameSpecDataSig
	      -> RnM_Fixes s RenamedSpecDataSig

rnSpecDataSig (SpecDataSig tycon ty src_loc)
  = pushSrcLocRn src_loc $
    let
	tyvars = extractMonoTyNames is_tyvar_name ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn` \ (tv_env,_) ->
    lookupTyCon tycon			`thenRn` \ tycon' ->
    rnMonoType tv_env ty		`thenRn` \ ty' ->
    returnRn (SpecDataSig tycon' ty' src_loc)

is_tyvar_name n = isLexVarId (getLocalName n)
\end{code}

%*********************************************************
%*							*
\subsection{Class declarations}
%*							*
%*********************************************************

@rnClassDecl@ uses the `global name function' to create a new
class declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnClassDecl :: RdrNameClassDecl -> RnM_Fixes s RenamedClassDecl

rnClassDecl (ClassDecl context cname tyvar sigs mbinds pragmas src_loc)
  = pushSrcLocRn src_loc $
    mkTyVarNamesEnv src_loc [tyvar]	`thenRn` \ (tv_env, [tyvar']) ->
    rnContext tv_env context	    	`thenRn` \ context' ->
    lookupClass cname		    	`thenRn` \ cname' ->
    mapRn (rn_op cname' tv_env) sigs    `thenRn` \ sigs' ->
    rnMethodBinds cname' mbinds    	`thenRn` \ mbinds' ->
    ASSERT(isNoClassPragmas pragmas)
    returnRn (ClassDecl context' cname' tyvar' sigs' mbinds' NoClassPragmas src_loc)
  where
    rn_op clas tv_env (ClassOpSig op ty pragmas locn)
      = pushSrcLocRn locn $
	lookupClassOp clas op		`thenRn` \ op_name ->
	rnPolyType tv_env ty		`thenRn` \ new_ty  ->

{-
*** Please check here that tyvar' appears in new_ty ***
*** (used to be in tcClassSig, but it's better here)
***	    not_elem = isn'tIn "tcClassSigs"
***	    -- Check that the class type variable is mentioned
***	checkTc (clas_tyvar `not_elem` extractTyVarTemplatesFromTy local_ty)
***		(methodTypeLacksTyVarErr clas_tyvar (_UNPK_ op_name) src_loc) `thenTc_`
-}

	ASSERT(isNoClassOpPragmas pragmas)
	returnRn (ClassOpSig op_name new_ty noClassOpPragmas locn)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************


@rnInstDecl@ uses the `global name function' to create a new of
instance declaration in which local names have been replaced by their
original names, reporting any unknown names.

\begin{code}
rnInstDecl :: RdrNameInstDecl -> RnM_Fixes s RenamedInstDecl

rnInstDecl (InstDecl cname ty mbinds from_here modname uprags pragmas src_loc)
  = pushSrcLocRn src_loc $
    lookupClass cname 		     	`thenRn` \ cname' ->

    rnPolyType [] ty			`thenRn` \ ty' ->
	-- [] tv_env ensures that tyvars will be foralled

    rnMethodBinds cname' mbinds		`thenRn` \ mbinds' ->
    mapRn (rn_uprag cname') uprags	`thenRn` \ new_uprags ->

    ASSERT(isNoInstancePragmas pragmas)
    returnRn (InstDecl cname' ty' mbinds'
		       from_here modname new_uprags noInstancePragmas src_loc)
  where
    rn_uprag class_name (SpecSig op ty using locn)
      = pushSrcLocRn src_loc $
	lookupClassOp class_name op	`thenRn` \ op_name ->
	rnPolyType nullTyVarNamesEnv ty	`thenRn` \ new_ty ->
	rn_using using			`thenRn` \ new_using ->
	returnRn (SpecSig op_name new_ty new_using locn)

    rn_uprag class_name (InlineSig op locn)
      = pushSrcLocRn locn $
	lookupClassOp class_name op	`thenRn` \ op_name ->
	returnRn (InlineSig op_name locn)

    rn_uprag class_name (DeforestSig op locn)
      = pushSrcLocRn locn $
	lookupClassOp class_name op	`thenRn` \ op_name ->
	returnRn (DeforestSig op_name locn)

    rn_uprag class_name (MagicUnfoldingSig op str locn)
      = pushSrcLocRn locn $
	lookupClassOp class_name op	`thenRn` \ op_name ->
	returnRn (MagicUnfoldingSig op_name str locn)

    rn_using Nothing 
      = returnRn Nothing
    rn_using (Just v)
      = lookupValue v	`thenRn` \ new_v ->
	returnRn (Just new_v)
\end{code}

%*********************************************************
%*							*
\subsection{@SPECIALIZE instance@ user-pragmas}
%*							*
%*********************************************************

\begin{code}
rnSpecInstSig :: RdrNameSpecInstSig
	      -> RnM_Fixes s RenamedSpecInstSig

rnSpecInstSig (SpecInstSig clas ty src_loc)
  = pushSrcLocRn src_loc $
    let
	tyvars = extractMonoTyNames is_tyvar_name ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn` \ (tv_env,_) ->
    lookupClass clas			`thenRn` \ new_clas ->
    rnMonoType tv_env ty		`thenRn` \ new_ty ->
    returnRn (SpecInstSig new_clas new_ty src_loc)
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

@rnDefaultDecl@ uses the `global name function' to create a new set
of default declarations in which local names have been replaced by
their original names, reporting any unknown names.

\begin{code}
rnDefaultDecl :: [RdrNameDefaultDecl] -> RnM_Fixes s [RenamedDefaultDecl]

rnDefaultDecl [] = returnRn []
rnDefaultDecl [DefaultDecl tys src_loc]
  = pushSrcLocRn src_loc $
    mapRn (rnMonoType nullTyVarNamesEnv) tys `thenRn` \ tys' ->
    returnRn [DefaultDecl tys' src_loc]
rnDefaultDecl defs@(d:ds)
  = addErrRn (dupDefaultDeclErr defs) `thenRn_`
    rnDefaultDecl [d]
\end{code}

%*************************************************************************
%*									*
\subsection{Fixity declarations}
%*									*
%*************************************************************************

\begin{code}
rnFixes :: [RdrNameFixityDecl]  -> RnM s [RenamedFixityDecl]

rnFixes fixities
  = mapRn rn_fixity fixities	`thenRn` \ fixes_maybe ->
    returnRn (catMaybes fixes_maybe)
  where
    rn_fixity fix@(InfixL name i)
      = rn_fixity_pieces InfixL name i fix
    rn_fixity fix@(InfixR name i)
      = rn_fixity_pieces InfixR name i fix
    rn_fixity fix@(InfixN name i)
      = rn_fixity_pieces InfixN name i fix

    rn_fixity_pieces mk_fixity name i fix
      = lookupValueMaybe name	`thenRn` \ maybe_res ->
	case maybe_res of
	  Just res | isLocallyDefined res
	    -> returnRn (Just (mk_fixity res i))
	  _ -> failButContinueRn Nothing (undefinedFixityDeclErr fix)
		
\end{code}

%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnPolyType :: TyVarNamesEnv
	   -> RdrNamePolyType
	   -> RnM_Fixes s RenamedPolyType

rnPolyType tv_env (HsForAllTy tvs ctxt ty)
  = rn_poly_help tv_env tvs ctxt ty

rnPolyType tv_env (HsPreForAllTy ctxt ty)
  = rn_poly_help tv_env forall_tyvars ctxt ty
  where
    mentioned_tyvars = extractCtxtTyNames ctxt `unionLists` extractMonoTyNames is_tyvar_name ty
    forall_tyvars    = --pprTrace "mentioned:" (ppCat (map (ppr PprShowAll) mentioned_tyvars)) $
		       --pprTrace "from_ty:" (ppCat (map (ppr PprShowAll) (extractMonoTyNames is_tyvar_name ty))) $
		       mentioned_tyvars `minusList` domTyVarNamesEnv tv_env

------------
rn_poly_help :: TyVarNamesEnv
	     -> [RdrName]
	     -> RdrNameContext
	     -> RdrNameMonoType
	     -> RnM_Fixes s RenamedPolyType

rn_poly_help tv_env tyvars ctxt ty
  = --pprTrace "rnPolyType:" (ppCat [ppCat (map (ppr PprShowAll . snd) tv_env),
    --				   ppStr ";tvs=", ppCat (map (ppr PprShowAll) tyvars),
    --				   ppStr ";ctxt=", ppCat (map (ppr PprShowAll) ctxt),
    --				   ppStr ";ty=", ppr PprShowAll ty]
    --			   ) $
    getSrcLocRn 				`thenRn` \ src_loc ->
    mkTyVarNamesEnv src_loc tyvars	 	`thenRn` \ (tv_env1, new_tyvars) ->
    let
	tv_env2 = catTyVarNamesEnvs tv_env1 tv_env
    in
    rnContext tv_env2 ctxt	`thenRn` \ new_ctxt ->
    rnMonoType tv_env2 ty	`thenRn` \ new_ty ->
    returnRn (HsForAllTy new_tyvars new_ctxt new_ty)
\end{code}

\begin{code}
rnMonoType :: TyVarNamesEnv
	   -> RdrNameMonoType
	   -> RnM_Fixes s RenamedMonoType

rnMonoType tv_env (MonoTyVar tyvar)
  = lookupTyVarName tv_env tyvar 	`thenRn` \ tyvar' ->
    returnRn (MonoTyVar tyvar')

rnMonoType tv_env (MonoListTy ty)
  = rnMonoType tv_env ty	`thenRn` \ ty' ->
    returnRn (MonoListTy ty')

rnMonoType tv_env (MonoFunTy ty1 ty2)
  = andRn MonoFunTy (rnMonoType tv_env ty1)
		    (rnMonoType tv_env ty2)

rnMonoType  tv_env (MonoTupleTy tys)
  = mapRn (rnMonoType tv_env) tys `thenRn` \ tys' ->
    returnRn (MonoTupleTy tys')

rnMonoType tv_env (MonoTyApp name tys)
  = let
	lookup_fn = if isLexVarId (getLocalName name) 
		    then lookupTyVarName tv_env
  	            else lookupTyCon
    in
    lookup_fn name			`thenRn` \ name' ->
    mapRn (rnMonoType tv_env) tys	`thenRn` \ tys' ->
    returnRn (MonoTyApp name' tys')
\end{code}

\begin{code}
rnContext :: TyVarNamesEnv -> RdrNameContext -> RnM_Fixes s RenamedContext

rnContext tv_env ctxt
  = mapRn rn_ctxt ctxt
  where
    rn_ctxt (clas, tyvar)
     = lookupClass clas	    	    `thenRn` \ clas_name ->
       lookupTyVarName tv_env tyvar `thenRn` \ tyvar_name ->
       returnRn (clas_name, tyvar_name)
\end{code}


\begin{code}
dupNameExportWarn locn names@((n,_):_) sty
  = ppHang (ppCat [pprNonSym sty n, ppStr "exported", ppInt (length names), ppStr "times:"])
	 4 (ppr sty locn)

dupModuleExportWarn locn mods@(mod:_) sty
  = ppHang (ppCat [ppStr "module", ppPStr mod, ppStr "appears", ppInt (length mods), ppStr "times in export list:"])
	 4 (ppr sty locn)

dupLocalsExportErr locn locals@((str,_):_) sty
  = ppHang (ppBesides [ppStr "Exported names have same local name `", ppPStr str, ppStr "': ", ppr sty locn])
	 4 (ppInterleave ppSP (map (pprNonSym sty . snd) locals))

classOpExportErr op locn sty 
  = ppHang (ppStr "Class operation can only be exported with class:")
         4 (ppCat [ppr sty op, ppr sty locn])

synAllExportErr syn locn sty
  = ppHang (ppStr "Type synonym should be exported abstractly:")
         4 (ppCat [ppr sty syn, ppr sty locn])

withExportErr str rn has rns locn sty
  = ppHang (ppBesides [ppStr "Inconsistent list of ", ppStr str, ppStr ": ", ppr sty locn])
         4 (ppAbove (ppCat [ppStr "expected:", ppInterleave ppComma (map (ppr sty) has)])
		    (ppCat [ppStr "found:   ", ppInterleave ppComma (map (ppr sty) rns)]))

importAllErr rn locn sty
  = ppHang (ppCat [pprNonSym sty rn, ppStr "exported concretely but only imported abstractly"])
         4 (ppr sty locn)

badModExportErr mod locn sty
  = ppHang (ppStr "Unknown module in export list:")
         4 (ppCat [ppStr "module", ppPStr mod, ppr sty locn])

derivingNonStdClassErr clas locn sty
  = ppHang (ppStr "Non-standard class in deriving:")
         4 (ppCat [ppr sty clas, ppr sty locn])

dupDefaultDeclErr defs sty
  = ppHang (ppStr "Duplicate default declarations:")
         4 (ppAboves (map pp_def_loc defs))
  where
    pp_def_loc (DefaultDecl _ src_loc) = ppr sty src_loc

undefinedFixityDeclErr decl sty
  = ppHang (ppStr "Fixity declaration for unknown operator:")
	 4 (ppr sty decl)
\end{code}
