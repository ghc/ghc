%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( rnDecl, rnSourceDecls, rnHsType, rnHsSigType ) where

#include "HsVersions.h"

import RnExpr
import HsSyn
import HsPragmas
import HsTypes		( getTyVarName )
import RdrName		( RdrName, isRdrDataCon, rdrNameOcc, isRdrTyVar, mkRdrNameWkr )
import RdrHsSyn		( RdrNameContext, RdrNameHsType, RdrNameConDecl,
			  extractRuleBndrsTyVars, extractHsTyRdrTyVars, extractHsTysRdrTyVars
			)
import RnHsSyn
import HsCore

import RnBinds		( rnTopBinds, rnMethodBinds, renameSigs, unknownSigErr )
import RnEnv		( bindTyVarsRn, lookupBndrRn, lookupOccRn, getIPName,
			  lookupImplicitOccRn, lookupImplicitOccsRn,
			  bindLocalsRn, bindLocalRn, bindLocalsFVRn, bindUVarRn,
			  bindTyVarsFVRn, bindTyVarsFV2Rn, extendTyVarEnvFVRn,
			  bindCoreLocalFVRn, bindCoreLocalsFVRn,
			  checkDupOrQualNames, checkDupNames,
			  mkImportedGlobalName, mkImportedGlobalFromRdrName,
			  newDFunName, getDFunKey, newImplicitBinder,
			  FreeVars, emptyFVs, plusFV, plusFVs, unitFV, 
			  addOneFV, mapFvRn
			)
import RnMonad

import FunDeps		( oclose )
import Class		( FunDep )

import Name		( Name, OccName,
			  ExportFlag(..), Provenance(..), 
			  nameOccName, NamedThing(..)
			)
import NameSet
import OccName		( mkDefaultMethodOcc )
import BasicTypes	( TopLevelFlag(..) )
import FiniteMap	( elemFM )
import PrelInfo		( derivableClassKeys, cCallishClassKeys,
			  deRefStablePtr_RDR, makeStablePtr_RDR, 
			  bindIO_RDR, returnIO_RDR
			)
import Bag		( bagToList )
import List		( partition, nub )
import Outputable
import SrcLoc		( SrcLoc )
import CmdLineOpts	( opt_GlasgowExts, opt_WarnUnusedMatches )	-- Warn of unused for-all'd tyvars
import Unique		( Uniquable(..) )
import UniqFM		( lookupUFM )
import ErrUtils		( Message )
import CStrings		( isCLabelString )
import Maybes		( maybeToBool, catMaybes )
import Util
\end{code}

@rnDecl@ `renames' declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:
\begin{enumerate}
\item
Checks that tyvars are used properly. This includes checking
for undefined tyvars, and tyvars in contexts that are ambiguous.
(Some of this checking has now been moved to module @TcMonoType@,
since we don't have functional dependency information at this point.)
\item
Checks that all variable occurences are defined.
\item 
Checks the @(..)@ etc constraints in the export list.
\end{enumerate}


%*********************************************************
%*							*
\subsection{Value declarations}
%*							*
%*********************************************************

\begin{code}
rnSourceDecls :: [RdrNameHsDecl] -> RnMS ([RenamedHsDecl], FreeVars)
	-- The decls get reversed, but that's ok

rnSourceDecls decls
  = go emptyFVs [] decls
  where
	-- Fixity and deprecations have been dealt with already; ignore them
    go fvs ds' []             = returnRn (ds', fvs)
    go fvs ds' (FixD _:ds)    = go fvs ds' ds
    go fvs ds' (DeprecD _:ds) = go fvs ds' ds
    go fvs ds' (d:ds)         = rnDecl d	`thenRn` \(d', fvs') ->
			        go (fvs `plusFV` fvs') (d':ds') ds
\end{code}


%*********************************************************
%*							*
\subsection{Value declarations}
%*							*
%*********************************************************

\begin{code}
-- rnDecl does all the work
rnDecl :: RdrNameHsDecl -> RnMS (RenamedHsDecl, FreeVars)

rnDecl (ValD binds) = rnTopBinds binds	`thenRn` \ (new_binds, fvs) ->
		      returnRn (ValD new_binds, fvs)


rnDecl (SigD (IfaceSig name ty id_infos loc))
  = pushSrcLocRn loc $
    mkImportedGlobalFromRdrName name	`thenRn` \ name' ->
    rnHsType doc_str ty			`thenRn` \ (ty',fvs1) ->
    mapFvRn rnIdInfo id_infos		`thenRn` \ (id_infos', fvs2) -> 
    returnRn (SigD (IfaceSig name' ty' id_infos' loc), fvs1 `plusFV` fvs2)
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
tyvar to its @Name@ representation.
In some cases (type signatures of values),
it is even necessary to go over the type first
in order to get the set of tyvars used by it, make an assoc list,
and then go over it again to rename the tyvars!
However, we can also do some scoping checks at the same time.

\begin{code}
rnDecl (TyClD (TyData new_or_data context tycon tyvars condecls nconstrs derivings pragmas src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn tycon			    	`thenRn` \ tycon' ->
    bindTyVarsFVRn data_doc tyvars		$ \ tyvars' ->
    rnContext data_doc context 			`thenRn` \ (context', cxt_fvs) ->
    checkDupOrQualNames data_doc con_names	`thenRn_`
    mapFvRn rnConDecl condecls			`thenRn` \ (condecls', con_fvs) ->
    rnDerivs derivings				`thenRn` \ (derivings', deriv_fvs) ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyClD (TyData new_or_data context' tycon' tyvars' condecls' nconstrs
                     derivings' noDataPragmas src_loc),
	      cxt_fvs `plusFV` con_fvs `plusFV` deriv_fvs)
  where
    data_doc = text "the data type declaration for" <+> quotes (ppr tycon)
    con_names = map conDeclName condecls

rnDecl (TyClD (TySynonym name tyvars ty src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn name				`thenRn` \ name' ->
    bindTyVarsFVRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType syn_doc (unquantify ty)		`thenRn` \ (ty', ty_fvs) ->
    returnRn (TyClD (TySynonym name' tyvars' ty' src_loc), ty_fvs)
  where
    syn_doc = text "the declaration for type synonym" <+> quotes (ppr name)

	-- For H98 we do *not* universally quantify on the RHS of a synonym
	-- Silently discard context... but the tyvars in the rest won't be in scope
    unquantify (HsForAllTy Nothing ctxt ty) | not opt_GlasgowExts = ty
    unquantify ty				   	          = ty

rnDecl (TyClD (ClassDecl context cname tyvars fds sigs mbinds pragmas
               tname dname dwname snames src_loc))
  = pushSrcLocRn src_loc $

    lookupBndrRn cname					`thenRn` \ cname' ->

	-- Deal with the implicit tycon and datacon name
	-- They aren't in scope (because they aren't visible to the user)
	-- and what we want to do is simply look them up in the cache;
	-- we jolly well ought to get a 'hit' there!
	-- So the 'Imported' part of this call is not relevant. 
	-- Unclean; but since these two are the only place this happens
	-- I can't work up the energy to do it more beautifully
    mkImportedGlobalFromRdrName tname			`thenRn` \ tname' ->
    mkImportedGlobalFromRdrName dname			`thenRn` \ dname' ->
    mkImportedGlobalFromRdrName dwname			`thenRn` \ dwname' ->
    mapRn mkImportedGlobalFromRdrName snames		`thenRn` \ snames' ->

	-- Tyvars scope over bindings and context
    bindTyVarsFV2Rn cls_doc tyvars		( \ clas_tyvar_names tyvars' ->

	-- Check the superclasses
    rnContext cls_doc context			`thenRn` \ (context', cxt_fvs) ->

	-- Check the functional dependencies
    rnFds cls_doc fds			`thenRn` \ (fds', fds_fvs) ->

	-- Check the signatures
    let
	    -- First process the class op sigs, then the fixity sigs.
	  (op_sigs, non_op_sigs) = partition isClassOpSig sigs
    in
    checkDupOrQualNames sig_doc sig_rdr_names_w_locs 	  `thenRn_` 
    mapFvRn (rn_op cname' clas_tyvar_names fds') op_sigs  `thenRn` \ (sigs', sig_fvs) ->
    let
     binders = mkNameSet [ nm | (ClassOpSig nm _ _ _ _) <- sigs' ]
    in
    renameSigs (okClsDclSig binders) non_op_sigs	  `thenRn` \ (non_ops', fix_fvs) ->

	-- Check the methods
    checkDupOrQualNames meth_doc meth_rdr_names_w_locs	`thenRn_`
    rnMethodBinds mbinds
    `thenRn` \ (mbinds', meth_fvs) ->

	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    ASSERT(isNoClassPragmas pragmas)
    returnRn (TyClD (ClassDecl context' cname' tyvars' fds' (non_ops' ++ sigs') mbinds'
			       NoClassPragmas tname' dname' dwname' snames' src_loc),
	      sig_fvs	`plusFV`
	      fix_fvs	`plusFV`
	      cxt_fvs	`plusFV`
	      fds_fvs	`plusFV`
	      meth_fvs
	     )
    )
  where
    cls_doc  = text "the declaration for class" 	<+> ppr cname
    sig_doc  = text "the signatures for class"  	<+> ppr cname
    meth_doc = text "the default-methods for class"	<+> ppr cname

    sig_rdr_names_w_locs  = [(op,locn) | ClassOpSig op _ _ _ locn <- sigs]
    meth_rdr_names_w_locs = bagToList (collectMonoBinders mbinds)
    meth_rdr_names	  = map fst meth_rdr_names_w_locs

    rn_op clas clas_tyvars clas_fds sig@(ClassOpSig op dm_rdr_name explicit_dm ty locn)
      = pushSrcLocRn locn $
 	lookupBndrRn op				`thenRn` \ op_name ->

		-- Check the signature
	rnHsSigType (quotes (ppr op)) ty	`thenRn` \ (new_ty, op_ty_fvs)  ->
	let
	    check_in_op_ty clas_tyvar =
		 checkRn (clas_tyvar `elemNameSet` oclose clas_fds op_ty_fvs)
			 (classTyVarNotInOpTyErr clas_tyvar sig)
	in
        mapRn_ check_in_op_ty clas_tyvars		 `thenRn_`

		-- Make the default-method name
	getModeRn					`thenRn` \ mode ->
	(case mode of 
	    SourceMode -> -- Source class decl
		   newImplicitBinder (mkDefaultMethodOcc (rdrNameOcc op)) locn	   `thenRn` \ dm_name ->
		   returnRn (dm_name, op `elem` meth_rdr_names, emptyFVs)

	    InterfaceMode
		-> 	-- Imported class that has a default method decl
			-- See comments with tname, snames, above
		    lookupImplicitOccRn dm_rdr_name 	`thenRn` \ dm_name ->
		    returnRn (dm_name, explicit_dm, if explicit_dm then unitFV dm_name else emptyFVs)
			-- An imported class decl for a class decl that had an explicit default
			-- method, mentions, rather than defines,
			-- the default method, so we must arrange to pull it in
	)						`thenRn` \ (dm_name, final_explicit_dm, dm_fvs) ->

	returnRn (ClassOpSig op_name dm_name final_explicit_dm new_ty locn, op_ty_fvs `plusFV` dm_fvs)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (InstD (InstDecl inst_ty mbinds uprags dfun_rdr_name src_loc))
  = pushSrcLocRn src_loc $
    rnHsSigType (text "an instance decl") inst_ty `thenRn` \ (inst_ty', inst_fvs) ->
    let
	inst_tyvars = case inst_ty' of
			HsForAllTy (Just inst_tyvars) _ _ -> inst_tyvars
			other			          -> []
	-- (Slightly strangely) the forall-d tyvars scope over
	-- the method bindings too
    in

	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenRn_`
    extendTyVarEnvFVRn inst_tyvars (		
	rnMethodBinds mbinds
    )						`thenRn` \ (mbinds', meth_fvs) ->
    let 
	binders = mkNameSet (map fst (bagToList (collectMonoBinders mbinds')))
    in
	-- Rename the prags and signatures.
	-- Note that the type variables are not in scope here,
	-- so that	instance Eq a => Eq (T a) where
	--			{-# SPECIALISE instance Eq a => Eq (T [a]) #-}
	-- works OK. 
    renameSigs (okInstDclSig binders) uprags	`thenRn` \ (new_uprags, prag_fvs) ->

    getModeRn		`thenRn` \ mode ->
    (case mode of
	InterfaceMode -> lookupImplicitOccRn dfun_rdr_name	`thenRn` \ dfun_name ->
			 returnRn (dfun_name, unitFV dfun_name)
	SourceMode    -> newDFunName (getDFunKey inst_ty') src_loc
                         `thenRn` \ dfun_name ->
			 returnRn (dfun_name, emptyFVs)
    )
    `thenRn` \ (dfun_name, dfun_fv) ->

    -- The typechecker checks that all the bindings are for the right class.
    returnRn (InstD (InstDecl inst_ty' mbinds' new_uprags dfun_name src_loc),
	      inst_fvs `plusFV` meth_fvs `plusFV` prag_fvs `plusFV` dfun_fv)
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
    lookupOccRn name		        `thenRn` \ name' ->
    let 
	extra_fvs FoExport 
	  | isDyn = 
		lookupImplicitOccsRn [makeStablePtr_RDR, deRefStablePtr_RDR,
				      bindIO_RDR, returnIO_RDR]
	  | otherwise = 
		lookupImplicitOccsRn [bindIO_RDR, returnIO_RDR] `thenRn` \ fvs ->
		returnRn (addOneFV fvs name')
	extra_fvs other = returnRn emptyFVs
    in
    checkRn (ok_ext_nm ext_nm) (badExtName ext_nm)	`thenRn_`

    extra_fvs imp_exp					`thenRn` \ fvs1 -> 

    rnHsSigType fo_decl_msg ty		       		`thenRn` \ (ty', fvs2) ->
    returnRn (ForD (ForeignDecl name' imp_exp ty' ext_nm cconv src_loc), 
	      fvs1 `plusFV` fvs2)
 where
  fo_decl_msg = ptext SLIT("a foreign declaration")
  isDyn	      = isDynamicExtName ext_nm

  ok_ext_nm Dynamic 		   = True
  ok_ext_nm (ExtName nm (Just mb)) = isCLabelString nm && isCLabelString mb
  ok_ext_nm (ExtName nm Nothing)   = isCLabelString nm
\end{code}

%*********************************************************
%*							*
\subsection{Rules}
%*							*
%*********************************************************

\begin{code}
rnDecl (RuleD (IfaceRule rule_name vars fn args rhs src_loc))
  = pushSrcLocRn src_loc	$
    lookupOccRn fn		`thenRn` \ fn' ->
    rnCoreBndrs vars		$ \ vars' ->
    mapFvRn rnCoreExpr args	`thenRn` \ (args', fvs1) ->
    rnCoreExpr rhs		`thenRn` \ (rhs',  fvs2) ->
    returnRn (RuleD (IfaceRule rule_name vars' fn' args' rhs' src_loc), 
	      (fvs1 `plusFV` fvs2) `addOneFV` fn')

rnDecl (RuleD (IfaceRuleOut fn rule))
	-- This one is used for BuiltInRules
	-- The rule itself is already done, but the thing
	-- to attach it to is not.
  = lookupOccRn fn		`thenRn` \ fn' ->
    returnRn (RuleD (IfaceRuleOut fn' rule), unitFV fn')

rnDecl (RuleD (HsRule rule_name tvs vars lhs rhs src_loc))
  = ASSERT( null tvs )
    pushSrcLocRn src_loc			$

    bindTyVarsFV2Rn doc (map UserTyVar sig_tvs)	$ \ sig_tvs' _ ->
    bindLocalsFVRn doc (map get_var vars)	$ \ ids ->
    mapFvRn rn_var (vars `zip` ids)		`thenRn` \ (vars', fv_vars) ->

    rnExpr lhs					`thenRn` \ (lhs', fv_lhs) ->
    rnExpr rhs					`thenRn` \ (rhs', fv_rhs) ->
    checkRn (validRuleLhs ids lhs')
	    (badRuleLhsErr rule_name lhs')	`thenRn_`
    let
	bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs)]
    in
    mapRn (addErrRn . badRuleVar rule_name) bad_vars	`thenRn_`
    returnRn (RuleD (HsRule rule_name sig_tvs' vars' lhs' rhs' src_loc),
	      fv_vars `plusFV` fv_lhs `plusFV` fv_rhs)
  where
    doc = text "the transformation rule" <+> ptext rule_name
    sig_tvs = extractRuleBndrsTyVars vars
  
    get_var (RuleBndr v)      = v
    get_var (RuleBndrSig v _) = v

    rn_var (RuleBndr v, id)	 = returnRn (RuleBndr id, emptyFVs)
    rn_var (RuleBndrSig v t, id) = rnHsType doc t	`thenRn` \ (t', fvs) ->
				   returnRn (RuleBndrSig id t', fvs)
\end{code}


%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
rnDerivs :: Maybe [RdrName] -> RnMS (Maybe [Name], FreeVars)

rnDerivs Nothing -- derivs not specified
  = returnRn (Nothing, emptyFVs)

rnDerivs (Just clss)
  = mapRn do_one clss	`thenRn` \ clss' ->
    returnRn (Just clss', mkNameSet clss')
  where
    do_one cls = lookupOccRn cls	`thenRn` \ clas_name ->
		 checkRn (getUnique clas_name `elem` derivableClassKeys)
			 (derivingNonStdClassErr clas_name)	`thenRn_`
		 returnRn clas_name
\end{code}

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ _ l) = (n,l)

rnConDecl :: RdrNameConDecl -> RnMS (RenamedConDecl, FreeVars)
rnConDecl (ConDecl name wkr tvs cxt details locn)
  = pushSrcLocRn locn $
    checkConName name			`thenRn_` 
    lookupBndrRn name			`thenRn` \ new_name ->

    mkImportedGlobalFromRdrName wkr	`thenRn` \ new_wkr ->
	-- See comments with ClassDecl

    bindTyVarsFVRn doc tvs 		$ \ new_tyvars ->
    rnContext doc cxt			`thenRn` \ (new_context, cxt_fvs) ->
    rnConDetails doc locn details	`thenRn` \ (new_details, det_fvs) -> 
    returnRn (ConDecl new_name new_wkr new_tyvars new_context new_details locn,
	      cxt_fvs `plusFV` det_fvs)
  where
    doc = text "the definition of data constructor" <+> quotes (ppr name)

rnConDetails doc locn (VanillaCon tys)
  = mapFvRn (rnBangTy doc) tys	`thenRn` \ (new_tys, fvs)  ->
    returnRn (VanillaCon new_tys, fvs)

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
    mapFvRn (rnField doc) fields	`thenRn` \ (new_fields, fvs) ->
    returnRn (RecCon new_fields, fvs)
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
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnHsSigType :: SDoc -> RdrNameHsType -> RnMS (RenamedHsType, FreeVars)
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty
  = rnHsType (text "the type signature for" <+> doc_str) ty
    
---------------------------------------
rnHsType :: SDoc -> RdrNameHsType -> RnMS (RenamedHsType, FreeVars)

rnHsType doc (HsForAllTy Nothing ctxt ty)
	-- Implicit quantifiction in source code (no kinds on tyvars)
	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
  = getLocalNameEnv		`thenRn` \ name_env ->
    let
	mentioned_in_tau = extractHsTyRdrTyVars ty
	forall_tyvars    = filter (not . (`elemFM` name_env)) mentioned_in_tau
    in
    checkConstraints doc forall_tyvars mentioned_in_tau ctxt ty	`thenRn` \ ctxt' ->
    rnForAll doc (map UserTyVar forall_tyvars) ctxt' ty

rnHsType doc (HsForAllTy (Just forall_tyvars) ctxt tau)
	-- Explicit quantification.
	-- Check that the forall'd tyvars are a subset of the
	-- free tyvars in the tau-type part
	-- That's only a warning... unless the tyvar is constrained by a 
	-- context in which case it's an error
  = let
	mentioned_in_tau  = extractHsTyRdrTyVars tau
	mentioned_in_ctxt = nub [tv | p <- ctxt,
				      ty <- tys_of_pred p,
				      tv <- extractHsTyRdrTyVars ty]
	tys_of_pred (HsPClass clas tys) = tys
	tys_of_pred (HsPIParam n ty) = [ty]

	dubious_guys	      = filter (`notElem` mentioned_in_tau) forall_tyvar_names
		-- dubious = explicitly quantified but not mentioned in tau type

	(bad_guys, warn_guys) = partition (`elem` mentioned_in_ctxt) dubious_guys
		-- bad  = explicitly quantified and constrained, but not mentioned in tau
		-- warn = explicitly quantified but not mentioned in ctxt or tau
 
	forall_tyvar_names    = map getTyVarName forall_tyvars
    in
    -- mapRn_ (forAllErr doc tau) bad_guys 					`thenRn_`
    mapRn_ (forAllWarn doc tau) warn_guys					`thenRn_`
    checkConstraints doc forall_tyvar_names mentioned_in_tau ctxt tau	`thenRn` \ ctxt' ->
    rnForAll doc forall_tyvars ctxt' tau

rnHsType doc (HsTyVar tyvar)
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (HsTyVar tyvar', unitFV tyvar')

rnHsType doc (HsFunTy ty1 ty2)
  = rnHsType doc ty1	`thenRn` \ (ty1', fvs1) ->
	-- Might find a for-all as the arg of a function type
    rnHsType doc ty2	`thenRn` \ (ty2', fvs2) ->
	-- Or as the result.  This happens when reading Prelude.hi
	-- when we find return :: forall m. Monad m -> forall a. a -> m a
    returnRn (HsFunTy ty1' ty2', fvs1 `plusFV` fvs2)

rnHsType doc (HsListTy ty)
  = rnHsType doc ty				`thenRn` \ (ty', fvs) ->
    returnRn (HsListTy ty', fvs `addOneFV` listTyCon_name)

-- Unboxed tuples are allowed to have poly-typed arguments.  These
-- sometimes crop up as a result of CPR worker-wrappering dictionaries.
rnHsType doc (HsTupleTy (HsTupCon _ boxity) tys)
	-- Don't do lookupOccRn, because this is built-in syntax
	-- so it doesn't need to be in scope
  = mapFvRn (rnHsType doc) tys	  	`thenRn` \ (tys', fvs) ->
    returnRn (HsTupleTy (HsTupCon n' boxity) tys', fvs `addOneFV` n')
  where
    n' = tupleTyCon_name boxity (length tys)
  

rnHsType doc (HsAppTy ty1 ty2)
  = rnHsType doc ty1		`thenRn` \ (ty1', fvs1) ->
    rnHsType doc ty2		`thenRn` \ (ty2', fvs2) ->
    returnRn (HsAppTy ty1' ty2', fvs1 `plusFV` fvs2)

rnHsType doc (HsPredTy pred)
  = rnPred doc pred	`thenRn` \ (pred', fvs) ->
    returnRn (HsPredTy pred', fvs)

rnHsType doc (HsUsgForAllTy uv_rdr ty)
  = bindUVarRn doc uv_rdr $ \ uv_name ->
    rnHsType doc ty       `thenRn` \ (ty', fvs) ->
    returnRn (HsUsgForAllTy uv_name ty',
              fvs )

rnHsType doc (HsUsgTy usg ty)
  = newUsg usg                      `thenRn` \ (usg', usg_fvs) ->
    rnHsType doc ty                 `thenRn` \ (ty', ty_fvs) ->
	-- A for-all can occur inside a usage annotation
    returnRn (HsUsgTy usg' ty',
              usg_fvs `plusFV` ty_fvs)
  where
    newUsg usg = case usg of
                   HsUsOnce       -> returnRn (HsUsOnce, emptyFVs)
                   HsUsMany       -> returnRn (HsUsMany, emptyFVs)
                   HsUsVar uv_rdr -> lookupOccRn uv_rdr `thenRn` \ uv_name ->
                                       returnRn (HsUsVar uv_name, emptyFVs)

rnHsTypes doc tys = mapFvRn (rnHsType doc) tys
\end{code}

\begin{code}
-- We use lookupOcc here because this is interface file only stuff
-- and we need the workers...
rnHsTupCon (HsTupCon n boxity)
  = lookupOccRn n	`thenRn` \ n' ->
    returnRn (HsTupCon n' boxity, unitFV n')

rnHsTupConWkr (HsTupCon n boxity)
	-- Tuple construtors are for the *worker* of the tuple
	-- Going direct saves needless messing about 
  = lookupOccRn (mkRdrNameWkr n)	`thenRn` \ n' ->
    returnRn (HsTupCon n' boxity, unitFV n')
\end{code}

\begin{code}
-- Check that each constraint mentions at least one of the forall'd type variables
-- Since the forall'd type variables are a subset of the free tyvars
-- of the tau-type part, this guarantees that every constraint mentions
-- at least one of the free tyvars in ty
checkConstraints doc forall_tyvars tau_vars ctxt ty
   = mapRn (checkPred doc forall_tyvars ty) ctxt `thenRn` \ maybe_ctxt' ->
     returnRn (catMaybes maybe_ctxt')
	    -- Remove problem ones, to avoid duplicate error message.
	
checkPred doc forall_tyvars ty p@(HsPClass clas tys)
  | not_univ  = failWithRn Nothing (univErr  doc p ty)
  | otherwise = returnRn (Just p)
  where
      ct_vars  = extractHsTysRdrTyVars tys
      not_univ =  -- At least one of the tyvars in each constraint must
		  -- be universally quantified. This restriction isn't in Hugs
		  not (any (`elem` forall_tyvars) ct_vars)
checkPred doc forall_tyvars ty p@(HsPIParam _ _)
  = returnRn (Just p)

rnForAll doc forall_tyvars ctxt ty
  = bindTyVarsFVRn doc forall_tyvars	$ \ new_tyvars ->
    rnContext doc ctxt			`thenRn` \ (new_ctxt, cxt_fvs) ->
    rnHsType doc ty			`thenRn` \ (new_ty, ty_fvs) ->
    returnRn (mkHsForAllTy (Just new_tyvars) new_ctxt new_ty,
	      cxt_fvs `plusFV` ty_fvs)
\end{code}

\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnMS (RenamedContext, FreeVars)
rnContext doc ctxt
  = mapAndUnzipRn rn_pred ctxt		`thenRn` \ (theta, fvs_s) ->
    let
	(_, dups) = removeDupsEq theta
		-- We only have equality, not ordering
    in
	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    mapRn (addWarnRn . dupClassAssertWarn theta) dups		`thenRn_`
    returnRn (theta, plusFVs fvs_s)
  where
   	--Someone discovered that @CCallable@ and @CReturnable@
	-- could be used in contexts such as:
	--	foo :: CCallable a => a -> PrimIO Int
	-- Doing this utterly wrecks the whole point of introducing these
	-- classes so we specifically check that this isn't being done.
    rn_pred pred = rnPred doc pred				`thenRn` \ (pred', fvs)->
		   checkRn (not (bad_pred pred'))
			   (naughtyCCallContextErr pred')	`thenRn_`
		   returnRn (pred', fvs)

    bad_pred (HsPClass clas _) = getUnique clas `elem` cCallishClassKeys
    bad_pred other	       = False


rnPred doc (HsPClass clas tys)
  = lookupOccRn clas		`thenRn` \ clas_name ->
    rnHsTypes doc tys		`thenRn` \ (tys', fvs) ->
    returnRn (HsPClass clas_name tys', fvs `addOneFV` clas_name)

rnPred doc (HsPIParam n ty)
  = getIPName n			`thenRn` \ name ->
    rnHsType doc ty		`thenRn` \ (ty', fvs) ->
    returnRn (HsPIParam name ty', fvs)
\end{code}

\begin{code}
rnFds :: SDoc -> [FunDep RdrName] -> RnMS ([FunDep Name], FreeVars)

rnFds doc fds
  = mapAndUnzipRn rn_fds fds		`thenRn` \ (theta, fvs_s) ->
    returnRn (theta, plusFVs fvs_s)
  where
    rn_fds (tys1, tys2)
      =	rnHsTyVars doc tys1		`thenRn` \ (tys1', fvs1) ->
	rnHsTyVars doc tys2		`thenRn` \ (tys2', fvs2) ->
	returnRn ((tys1', tys2'), fvs1 `plusFV` fvs2)

rnHsTyVars doc tvs = mapFvRn (rnHsTyvar doc) tvs
rnHsTyvar doc tyvar
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (tyvar', unitFV tyvar')
\end{code}

%*********************************************************
%*							 *
\subsection{IdInfo}
%*							 *
%*********************************************************

\begin{code}
rnIdInfo (HsStrictness str) = returnRn (HsStrictness str, emptyFVs)

rnIdInfo (HsWorker worker)
  = lookupOccRn worker			`thenRn` \ worker' ->
    returnRn (HsWorker worker', unitFV worker')

rnIdInfo (HsUnfold inline expr)	= rnCoreExpr expr `thenRn` \ (expr', fvs) ->
				  returnRn (HsUnfold inline expr', fvs)
rnIdInfo (HsArity arity)	= returnRn (HsArity arity, emptyFVs)
rnIdInfo (HsUpdate update)	= returnRn (HsUpdate update, emptyFVs)
rnIdInfo HsNoCafRefs		= returnRn (HsNoCafRefs, emptyFVs)
rnIdInfo HsCprInfo		= returnRn (HsCprInfo, emptyFVs)

\end{code}

@UfCore@ expressions.

\begin{code}
rnCoreExpr (UfType ty)
  = rnHsType (text "unfolding type") ty	`thenRn` \ (ty', fvs) ->
    returnRn (UfType ty', fvs)

rnCoreExpr (UfVar v)
  = lookupOccRn v 	`thenRn` \ v' ->
    returnRn (UfVar v', unitFV v')

rnCoreExpr (UfLit l)
  = returnRn (UfLit l, emptyFVs)

rnCoreExpr (UfLitLit l ty)
  = rnHsType (text "litlit") ty	`thenRn` \ (ty', fvs) ->
    returnRn (UfLitLit l ty', fvs)

rnCoreExpr (UfCCall cc ty)
  = rnHsType (text "ccall") ty	`thenRn` \ (ty', fvs) ->
    returnRn (UfCCall cc ty', fvs)

rnCoreExpr (UfTuple con args) 
  = rnHsTupConWkr con			`thenRn` \ (con', fvs1) ->
    mapFvRn rnCoreExpr args		`thenRn` \ (args', fvs2) ->
    returnRn (UfTuple con' args', fvs1 `plusFV` fvs2)

rnCoreExpr (UfApp fun arg)
  = rnCoreExpr fun		`thenRn` \ (fun', fv1) ->
    rnCoreExpr arg		`thenRn` \ (arg', fv2) ->
    returnRn (UfApp fun' arg', fv1 `plusFV` fv2)

rnCoreExpr (UfCase scrut bndr alts)
  = rnCoreExpr scrut			`thenRn` \ (scrut', fvs1) ->
    bindCoreLocalFVRn bndr		( \ bndr' ->
	mapFvRn rnCoreAlt alts		`thenRn` \ (alts', fvs2) ->
	returnRn (UfCase scrut' bndr' alts', fvs2)
    )						`thenRn` \ (case', fvs3) ->
    returnRn (case', fvs1 `plusFV` fvs3)

rnCoreExpr (UfNote note expr) 
  = rnNote note			`thenRn` \ (note', fvs1) ->
    rnCoreExpr expr		`thenRn` \ (expr', fvs2) ->
    returnRn  (UfNote note' expr', fvs1 `plusFV` fvs2) 

rnCoreExpr (UfLam bndr body)
  = rnCoreBndr bndr 		$ \ bndr' ->
    rnCoreExpr body		`thenRn` \ (body', fvs) ->
    returnRn (UfLam bndr' body', fvs)

rnCoreExpr (UfLet (UfNonRec bndr rhs) body)
  = rnCoreExpr rhs		`thenRn` \ (rhs', fvs1) ->
    rnCoreBndr bndr 		( \ bndr' ->
	rnCoreExpr body		`thenRn` \ (body', fvs2) ->
	returnRn (UfLet (UfNonRec bndr' rhs') body', fvs2)
    )				`thenRn` \ (result, fvs3) ->
    returnRn (result, fvs1 `plusFV` fvs3)

rnCoreExpr (UfLet (UfRec pairs) body)
  = rnCoreBndrs bndrs		$ \ bndrs' ->
    mapFvRn rnCoreExpr rhss	`thenRn` \ (rhss', fvs1) ->
    rnCoreExpr body		`thenRn` \ (body', fvs2) ->
    returnRn (UfLet (UfRec (bndrs' `zip` rhss')) body', fvs1 `plusFV` fvs2)
  where
    (bndrs, rhss) = unzip pairs
\end{code}

\begin{code}
rnCoreBndr (UfValBinder name ty) thing_inside
  = rnHsType doc ty		`thenRn` \ (ty', fvs1) ->
    bindCoreLocalFVRn name	( \ name' ->
	    thing_inside (UfValBinder name' ty')
    )				`thenRn` \ (result, fvs2) ->
    returnRn (result, fvs1 `plusFV` fvs2)
  where
    doc = text "unfolding id"
    
rnCoreBndr (UfTyBinder name kind) thing_inside
  = bindCoreLocalFVRn name		$ \ name' ->
    thing_inside (UfTyBinder name' kind)
    
rnCoreBndrs []     thing_inside = thing_inside []
rnCoreBndrs (b:bs) thing_inside = rnCoreBndr b		$ \ name' ->
				  rnCoreBndrs bs 	$ \ names' ->
				  thing_inside (name':names')
\end{code}    

\begin{code}
rnCoreAlt (con, bndrs, rhs)
  = rnUfCon con bndrs			`thenRn` \ (con', fvs1) ->
    bindCoreLocalsFVRn bndrs		( \ bndrs' ->
	rnCoreExpr rhs			`thenRn` \ (rhs', fvs2) ->
	returnRn ((con', bndrs', rhs'), fvs2)
    )					`thenRn` \ (result, fvs3) ->
    returnRn (result, fvs1 `plusFV` fvs3)

rnNote (UfCoerce ty)
  = rnHsType (text "unfolding coerce") ty	`thenRn` \ (ty', fvs) ->
    returnRn (UfCoerce ty', fvs)

rnNote (UfSCC cc)   = returnRn (UfSCC cc, emptyFVs)
rnNote UfInlineCall = returnRn (UfInlineCall, emptyFVs)
rnNote UfInlineMe   = returnRn (UfInlineMe, emptyFVs)


rnUfCon UfDefault _
  = returnRn (UfDefault, emptyFVs)

rnUfCon (UfTupleAlt tup_con) bndrs
  = rnHsTupCon tup_con 		`thenRn` \ (HsTupCon con' _, fvs) -> 
    returnRn (UfDataAlt con', fvs)
	-- Makes the type checker a little easier

rnUfCon (UfDataAlt con) _
  = lookupOccRn con		`thenRn` \ con' ->
    returnRn (UfDataAlt con', unitFV con')

rnUfCon (UfLitAlt lit) _
  = returnRn (UfLitAlt lit, emptyFVs)

rnUfCon (UfLitLitAlt lit ty) _
  = rnHsType (text "litlit") ty		`thenRn` \ (ty', fvs) ->
    returnRn (UfLitLitAlt lit ty', fvs)
\end{code}

%*********************************************************
%*							 *
\subsection{Rule shapes}
%*							 *
%*********************************************************

Check the shape of a transformation rule LHS.  Currently
we only allow LHSs of the form @(f e1 .. en)@, where @f@ is
not one of the @forall@'d variables.

\begin{code}
validRuleLhs foralls lhs
  = check lhs
  where
    check (HsApp e1 e2) 		  = check e1
    check (HsVar v) | v `notElem` foralls = True
    check other				  = False
\end{code}


%*********************************************************
%*							 *
\subsection{Errors}
%*							 *
%*********************************************************

\begin{code}
derivingNonStdClassErr clas
  = hsep [ptext SLIT("non-standard class"), ppr clas, ptext SLIT("in deriving clause")]

classTyVarNotInOpTyErr clas_tyvar sig
  = hang (hsep [ptext SLIT("Class type variable"),
		       quotes (ppr clas_tyvar),
		       ptext SLIT("does not appear in method signature")])
	 4 (ppr sig)

badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

forAllWarn doc ty tyvar
  | not opt_WarnUnusedMatches = returnRn ()
  | otherwise
  = getModeRn		`thenRn` \ mode ->
    case mode of {
#ifndef DEBUG
	InterfaceMode -> returnRn () ;	-- Don't warn of unused tyvars in interface files
					-- unless DEBUG is on, in which case it is slightly
					-- informative.  They can arise from mkRhsTyLam,
#endif					-- leading to (say) 	f :: forall a b. [b] -> [b]
	other ->

    addWarnRn (
      sep [ptext SLIT("The universally quantified type variable") <+> quotes (ppr tyvar),
	   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
      $$
      (ptext SLIT("In") <+> doc))
    }

forAllErr doc ty tyvar
  = addErrRn (
      sep [ptext SLIT("The constrained type variable") <+> quotes (ppr tyvar),
	   nest 4 (ptext SLIT("does not appear in the type") <+> quotes (ppr ty))]
      $$
      (ptext SLIT("In") <+> doc))

univErr doc constraint ty
  = sep [ptext SLIT("All of the type variable(s) in the constraint")
          <+> quotes (ppr constraint) 
	  <+> ptext SLIT("are already in scope"),
	 nest 4 (ptext SLIT("At least one must be universally quantified here"))
    ]
    $$
    (ptext SLIT("In") <+> doc)

ambigErr doc constraint ty
  = sep [ptext SLIT("Ambiguous constraint") <+> quotes (ppr constraint),
	 nest 4 (ptext SLIT("in the type:") <+> ppr ty),
	 nest 4 (ptext SLIT("Each forall-d type variable mentioned by the constraint must appear after the =>."))]
    $$
    (ptext SLIT("In") <+> doc)

badRuleLhsErr name lhs
  = sep [ptext SLIT("Rule") <+> ptext name <> colon,
	 nest 4 (ptext SLIT("Illegal left-hand side:") <+> ppr lhs)]
    $$
    ptext SLIT("LHS must be of form (f e1 .. en) where f is not forall'd")

badRuleVar name var
  = sep [ptext SLIT("Rule") <+> ptext name <> colon,
	 ptext SLIT("Forall'd variable") <+> quotes (ppr var) <+> 
		ptext SLIT("does not appear on left hand side")]

badExtName :: ExtName -> Message
badExtName ext_nm
  = sep [quotes (ppr ext_nm) <+> ptext SLIT("is not a valid C identifier")]

dupClassAssertWarn ctxt (assertion : dups)
  = sep [hsep [ptext SLIT("Duplicate class assertion"), 
	       quotes (ppr assertion),
	       ptext SLIT("in the context:")],
	 nest 4 (ppr ctxt <+> ptext SLIT("..."))]

naughtyCCallContextErr (HsPClass clas _)
  = sep [ptext SLIT("Can't use class") <+> quotes (ppr clas), 
	 ptext SLIT("in a context")]
\end{code}
