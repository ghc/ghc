%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[RnSource]{Main pass of renamer}

\begin{code}
module RnSource ( rnDecl, rnHsSigType ) where

#include "HsVersions.h"

import RnExpr
import HsSyn
import HsDecls		( HsIdInfo(..), HsStrictnessInfo(..) )
import HsPragmas
import HsTypes		( getTyVarName, pprClassAssertion, cmpHsTypes )
import RdrHsSyn
import RnHsSyn
import HsCore

import RnBinds		( rnTopBinds, rnMethodBinds, renameSigs )
import RnEnv		( bindTyVarsRn, lookupBndrRn, lookupOccRn, 
			  lookupImplicitOccRn, addImplicitOccRn,
			  bindLocalsRn,
			  newDfunName, checkDupOrQualNames, checkDupNames,
			  newLocallyDefinedGlobalName, newImportedGlobalName, 
			  ifaceFlavour, listTyCon_name, tupleTyCon_name )
import RnMonad

import Name		( Name, OccName(..), occNameString, prefixOccName,
			  ExportFlag(..), Provenance(..),
			  nameOccName, NamedThing(..), isLexCon,
			  mkDefaultMethodName
			)
import NameSet
import BasicTypes	( TopLevelFlag(..) )
import FiniteMap	( elemFM )
import PrelInfo		( derivingOccurrences, numClass_RDR, 
			  deRefStablePtr_NAME, makeStablePtr_NAME,
			  bindIO_NAME
			)
import Bag		( bagToList )
import Outputable
import SrcLoc		( SrcLoc )
import UniqFM		( lookupUFM )
import Maybes		( maybeToBool )
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
rnDecl :: RdrNameHsDecl -> RnMS s RenamedHsDecl

rnDecl (ValD binds) = rnTopBinds binds	`thenRn` \ new_binds ->
		      returnRn (ValD new_binds)


rnDecl (SigD (IfaceSig name ty id_infos loc))
  = pushSrcLocRn loc $
    lookupBndrRn name		`thenRn` \ name' ->
    rnHsType doc_str ty		`thenRn` \ ty' ->

	-- Get the pragma info (if any).
    getModeRn			`thenRn` \ (InterfaceMode _ print_unqual) ->
    setModeRn (InterfaceMode Optional print_unqual) $
	-- In all the rest of the signature we read in optional mode,
	-- so that (a) we don't die
    mapRn rnIdInfo id_infos	`thenRn` \ id_infos' -> 
    returnRn (SigD (IfaceSig name' ty' id_infos' loc))
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
rnDecl (TyD (TyData new_or_data context tycon tyvars condecls derivings pragmas src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn tycon			    		`thenRn` \ tycon' ->
    bindTyVarsRn data_doc tyvars			$ \ tyvars' ->
    rnContext data_doc context 				`thenRn` \ context' ->
    checkDupOrQualNames data_doc con_names		`thenRn_`
    mapRn rnConDecl condecls				`thenRn` \ condecls' ->
    rnDerivs derivings					`thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyD (TyData new_or_data context' tycon' tyvars' condecls' derivings' noDataPragmas src_loc))
  where
    data_doc = text "the data type declaration for" <+> ppr tycon
    con_names = map conDeclName condecls

rnDecl (TyD (TySynonym name tyvars ty src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn name				`thenRn` \ name' ->
    bindTyVarsRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType syn_doc ty				`thenRn` \ ty' ->
    returnRn (TyD (TySynonym name' tyvars' ty' src_loc))
  where
    syn_doc = text "the declaration for type synonym" <+> quotes (ppr name)
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
rnDecl (ClD (ClassDecl context cname tyvars sigs mbinds pragmas tname dname src_loc))
  = pushSrcLocRn src_loc $

    lookupBndrRn cname					`thenRn` \ cname' ->
    lookupBndrRn tname					`thenRn` \ tname' ->
    lookupBndrRn dname					`thenRn` \ dname' ->

    bindTyVarsRn cls_doc tyvars					( \ tyvars' ->
	rnContext cls_doc context				`thenRn` \ context' ->

	     -- Check the signatures
	let
	  clas_tyvar_names = map getTyVarName tyvars'
	in
	checkDupOrQualNames sig_doc sig_rdr_names_w_locs 	`thenRn_` 
	mapRn (rn_op cname' clas_tyvar_names) sigs		`thenRn` \ sigs' ->
	returnRn (tyvars', context', sigs')
    )							`thenRn` \ (tyvars', context', sigs') ->

	-- Check the methods
    checkDupOrQualNames meth_doc meth_rdr_names_w_locs	`thenRn_`
    rnMethodBinds mbinds				`thenRn` \ mbinds' ->

	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    ASSERT(isNoClassPragmas pragmas)
    returnRn (ClD (ClassDecl context' cname' tyvars' sigs' mbinds' NoClassPragmas tname' dname' src_loc))
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
	rnHsSigType (quotes (ppr op)) ty	`thenRn` \ new_ty  ->

		-- Make the default-method name
	let
	    dm_occ = mkDefaultMethodName (rdrNameOcc op)
	in
	getModuleRn			`thenRn` \ mod_name ->
	getModeRn			`thenRn` \ mode ->
	(case (mode, maybe_dm) of 
	    (SourceMode, _) | op `elem` meth_rdr_names
		-> 	-- There's an explicit method decl
		   newLocallyDefinedGlobalName mod_name dm_occ 
					       (\_ -> Exported) locn	`thenRn` \ dm_name ->
		   returnRn (Just dm_name)

	    (InterfaceMode _ _, Just _) 
		-> 	-- Imported class that has a default method decl
		    newImportedGlobalName mod_name dm_occ (ifaceFlavour clas)	`thenRn` \ dm_name ->
		    addOccurrenceName dm_name					`thenRn_`
		    returnRn (Just dm_name)

	    other -> returnRn Nothing
	)					`thenRn` \ maybe_dm_name ->

		-- Check that each class tyvar appears in op_ty
	let
	    (ctxt, op_ty) = case new_ty of
				HsForAllTy tvs ctxt op_ty -> (ctxt, op_ty)
				other			  -> ([], new_ty)
	    ctxt_fvs  = extractHsCtxtTyNames ctxt	-- Includes tycons/classes but we
	    op_ty_fvs = extractHsTyNames op_ty		-- don't care about that

	    check_in_op_ty clas_tyvar = checkRn (clas_tyvar `elemNameSet` op_ty_fvs)
					        (classTyVarNotInOpTyErr clas_tyvar sig)
	in
        mapRn check_in_op_ty clas_tyvars		 `thenRn_`

	returnRn (ClassOpSig op_name maybe_dm_name new_ty locn)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (InstD (InstDecl inst_ty mbinds uprags maybe_dfun src_loc))
  = pushSrcLocRn src_loc $
    rnHsSigType (text "an instance decl") inst_ty	`thenRn` \ inst_ty' ->


	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenRn_`
    rnMethodBinds mbinds			`thenRn` \ mbinds' ->
    let 
	binders = mkNameSet (map fst (bagToList (collectMonoBinders mbinds')))
    in
    renameSigs NotTopLevel True binders uprags	`thenRn` \ new_uprags ->
   
    let
     -- We use the class name and the name of the first
     -- type constructor the class is applied to.
     (cl_nm, tycon_nm) = mkDictPrefix inst_ty'
     
     mkDictPrefix (MonoDictTy cl tys) = 
        case tys of
	  []     -> (c_nm, nilOccName )
	  (ty:_) -> (c_nm, getInstHeadTy ty)
	where
	 c_nm = nameOccName (getName cl)

     mkDictPrefix (HsForAllTy _ _ ty)  = mkDictPrefix ty  -- can this 
     mkDictPrefix _		       = (nilOccName, nilOccName)

     getInstHeadTy t 
      = case t of
          MonoTyVar tv    -> nameOccName (getName tv)
          MonoTyApp t _   -> getInstHeadTy t
	  _		  -> nilOccName
	    -- I cannot see how the rest of HsType constructors
	    -- can occur, but this isn't really a failure condition,
	    -- so we return silently.

     nilOccName = (VarOcc _NIL_) -- ToDo: add OccName constructor fun for this.
    in
    newDfunName cl_nm tycon_nm maybe_dfun src_loc  `thenRn` \ dfun_name ->
    addOccurrenceName dfun_name			   `thenRn_`
			-- The dfun is not optional, because we use its version number
			-- to identify the version of the instance declaration

	-- The typechecker checks that all the bindings are for the right class.
    returnRn (InstD (InstDecl inst_ty' mbinds' new_uprags (Just dfun_name) src_loc))
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
    mapRn (rnHsType doc_str) tys	`thenRn` \ tys' ->
    lookupImplicitOccRn numClass_RDR	`thenRn_` 
    returnRn (DefD (DefaultDecl tys' src_loc))
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
	_ -> returnRn name')		`thenRn_`
    rnHsSigType fo_decl_msg ty		`thenRn` \ ty' ->
    returnRn (ForD (ForeignDecl name' imp_exp ty' ext_nm cconv src_loc))
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
rnDerivs :: Maybe [RdrName] -> RnMS s (Maybe [Name])

rnDerivs Nothing -- derivs not specified
  = returnRn Nothing

rnDerivs (Just ds)
  = mapRn rn_deriv ds `thenRn` \ derivs ->
    returnRn (Just derivs)
  where
    rn_deriv clas
      = lookupOccRn clas	    `thenRn` \ clas_name ->

		-- Now add extra "occurrences" for things that
		-- the deriving mechanism will later need in order to
		-- generate code for this class.
	case lookupUFM derivingOccurrences clas_name of
		Nothing -> addErrRn (derivingNonStdClassErr clas_name)	`thenRn_`
			   returnRn clas_name

		Just occs -> mapRn lookupImplicitOccRn occs	`thenRn_`
			     returnRn clas_name
\end{code}

\begin{code}
conDeclName :: RdrNameConDecl -> (RdrName, SrcLoc)
conDeclName (ConDecl n _ _ _ l) = (n,l)

rnConDecl :: RdrNameConDecl -> RnMS s RenamedConDecl
rnConDecl (ConDecl name tvs cxt details locn)
  = pushSrcLocRn locn $
    checkConName name			`thenRn_` 
    lookupBndrRn name			`thenRn` \ new_name ->
    bindTyVarsRn doc tvs 		$ \ new_tyvars ->
    rnContext doc cxt			`thenRn` \ new_context ->
    rnConDetails doc locn details	`thenRn` \ new_details -> 
    returnRn (ConDecl new_name new_tyvars new_context new_details locn)
  where
    doc = text "the definition of data constructor" <+> quotes (ppr name)

rnConDetails doc locn (VanillaCon tys)
  = mapRn (rnBangTy doc) tys		`thenRn` \ new_tys  ->
    returnRn (VanillaCon new_tys)

rnConDetails doc locn (InfixCon ty1 ty2)
  = rnBangTy doc ty1  		`thenRn` \ new_ty1 ->
    rnBangTy doc ty2  		`thenRn` \ new_ty2 ->
    returnRn (InfixCon new_ty1 new_ty2)

rnConDetails doc locn (NewCon ty)
  = rnHsType doc ty			`thenRn` \ new_ty  ->
    returnRn (NewCon new_ty)

rnConDetails doc locn (RecCon fields)
  = checkDupOrQualNames doc field_names	`thenRn_`
    mapRn (rnField doc) fields		`thenRn` \ new_fields ->
    returnRn (RecCon new_fields)
  where
    field_names = [(fld, locn) | (flds, _) <- fields, fld <- flds]

rnField doc (names, ty)
  = mapRn lookupBndrRn names	`thenRn` \ new_names ->
    rnBangTy doc ty		`thenRn` \ new_ty ->
    returnRn (new_names, new_ty) 

rnBangTy doc (Banged ty)
  = rnHsType doc ty `thenRn` \ new_ty ->
    returnRn (Banged new_ty)

rnBangTy doc (Unbanged ty)
  = rnHsType doc ty `thenRn` \ new_ty ->
    returnRn (Unbanged new_ty)

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
  = checkRn (isLexCon (occNameString (rdrNameOcc name)))
	    (badDataCon name)
\end{code}


%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnHsSigType :: SDoc -> RdrNameHsType -> RnMS s RenamedHsType 
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.
rnHsSigType doc_str ty = rnHsType (text "the type signature for" <+> doc_str) ty




rnHsType :: SDoc -> RdrNameHsType -> RnMS s RenamedHsType

rnHsType doc (HsForAllTy [] ctxt ty)
	-- From source code (no kinds on tyvars)

	-- Given the signature  C => T  we universally quantify 
	-- over FV(T) \ {in-scope-tyvars} 
	-- 
	-- We insist that the universally quantified type vars is a superset of FV(C)
	-- It follows that FV(T) is a superset of FV(C), so that the context constrains
	-- no type variables that don't appear free in the tau-type part.

  = getLocalNameEnv		`thenRn` \ name_env ->
    let
	mentioned_tyvars = extractHsTyVars ty
	forall_tyvars    = filter (not . (`elemFM` name_env)) mentioned_tyvars

	ctxt_w_ftvs :: [((RdrName,[RdrNameHsType]), [RdrName])]
	ctxt_w_ftvs  = [ (constraint, foldr ((++) . extractHsTyVars) [] tys)
		       | constraint@(_,tys) <- ctxt]

	-- A 'non-poly constraint' is one that does not mention *any*
	-- of the forall'd type variables
	non_poly_constraints = filter non_poly ctxt_w_ftvs
	non_poly (c,ftvs)    = not (any (`elem` forall_tyvars) ftvs)

	-- A 'non-mentioned' constraint is one that mentions a
	-- type variable that does not appear in 'ty'
	non_mentioned_constraints = filter non_mentioned ctxt_w_ftvs
	non_mentioned (c,ftvs)    = any (not . (`elem` mentioned_tyvars)) ftvs

	-- Zap the context if there's a problem, to avoid duplicate error message.
	ctxt' | null non_poly_constraints && null non_mentioned_constraints = ctxt
	      | otherwise = []
    in
    mapRn (ctxtErr1 doc forall_tyvars ty) non_poly_constraints		`thenRn_`
    mapRn (ctxtErr2 doc ty)               non_mentioned_constraints	`thenRn_`

    (bindTyVarsRn doc (map UserTyVar forall_tyvars)	$ \ new_tyvars ->
    rnContext doc ctxt'					`thenRn` \ new_ctxt ->
    rnHsType doc ty					`thenRn` \ new_ty ->
    returnRn (mkHsForAllTy new_tyvars new_ctxt new_ty))

rnHsType doc (HsForAllTy tvs ctxt ty)
	-- tvs are non-empty, hence must be from an interface file
	-- 	(tyvars may be kinded)
  = bindTyVarsRn doc tvs		$ \ new_tyvars ->
    rnContext doc ctxt			`thenRn` \ new_ctxt ->
    rnHsType doc ty			`thenRn` \ new_ty ->
    returnRn (mkHsForAllTy new_tyvars new_ctxt new_ty)


rnHsType doc (MonoTyVar tyvar)
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (MonoTyVar tyvar')

rnHsType doc (MonoFunTy ty1 ty2)
  = andRn MonoFunTy (rnHsType doc ty1) (rnHsType doc ty2)

rnHsType doc (MonoListTy ty)
  = addImplicitOccRn listTyCon_name		`thenRn_`
    rnHsType doc ty				`thenRn` \ ty' ->
    returnRn (MonoListTy ty')

rnHsType doc (MonoTupleTy tys boxed)
  = addImplicitOccRn (tupleTyCon_name boxed (length tys)) `thenRn_`
    mapRn (rnHsType doc) tys				  `thenRn` \ tys' ->
    returnRn (MonoTupleTy tys' boxed)

rnHsType doc (MonoTyApp ty1 ty2)
  = rnHsType doc ty1		`thenRn` \ ty1' ->
    rnHsType doc ty2		`thenRn` \ ty2' ->
    returnRn (MonoTyApp ty1' ty2')

rnHsType doc (MonoDictTy clas tys)
  = lookupOccRn clas		`thenRn` \ clas' ->
    mapRn (rnHsType doc) tys	`thenRn` \ tys' ->
    returnRn (MonoDictTy clas' tys')
\end{code}


\begin{code}
rnContext :: SDoc -> RdrNameContext -> RnMS s RenamedContext

rnContext doc ctxt
  = mapRn rn_ctxt ctxt		`thenRn` \ theta  ->
    let
	(_, dup_asserts) = removeDups cmp_assert theta
    in
	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    mapRn (addWarnRn . dupClassAssertWarn theta) dup_asserts	`thenRn_`

    returnRn theta
  where
    rn_ctxt (clas, tys)
      =	lookupBndrRn clas		`thenRn` \ clas_name ->
	addOccurrenceName clas_name	`thenRn_`
	mapRn (rnHsType doc) tys	`thenRn` \ tys' ->
	returnRn (clas_name, tys')

    cmp_assert (c1,tys1) (c2,tys2)
      = (c1 `compare` c2) `thenCmp` (cmpHsTypes compare tys1 tys2)
\end{code}


%*********************************************************
%*							*
\subsection{IdInfo}
%*							*
%*********************************************************

\begin{code}
rnIdInfo (HsStrictness strict)
  = rnStrict strict	`thenRn` \ strict' ->
    returnRn (HsStrictness strict')

rnIdInfo (HsUnfold inline (Just expr))	= rnCoreExpr expr	`thenRn` \ expr' ->
				  	  returnRn (HsUnfold inline (Just expr'))
rnIdInfo (HsUnfold inline Nothing)	= returnRn (HsUnfold inline Nothing)
rnIdInfo (HsArity arity)	= returnRn (HsArity arity)
rnIdInfo (HsUpdate update)	= returnRn (HsUpdate update)
rnIdInfo (HsNoCafRefs)		= returnRn (HsNoCafRefs)
rnIdInfo (HsSpecialise tyvars tys expr)
  = bindTyVarsRn doc tyvars	$ \ tyvars' ->
    rnCoreExpr expr		`thenRn` \ expr' ->
    mapRn (rnHsType doc) tys	`thenRn` \ tys' ->
    returnRn (HsSpecialise tyvars' tys' expr')
  where
    doc = text "Specialise in interface pragma"
    

rnStrict (HsStrictnessInfo demands (Just (worker,cons)))
	-- The sole purpose of the "cons" field is so that we can mark the constructors
	-- needed to build the wrapper as "needed", so that their data type decl will be
	-- slurped in. After that their usefulness is o'er, so we just put in the empty list.
  = lookupOccRn worker			`thenRn` \ worker' ->
    mapRn lookupOccRn cons		`thenRn_` 
    returnRn (HsStrictnessInfo demands (Just (worker',[])))

-- Boring, but necessary for the type checker.
rnStrict (HsStrictnessInfo demands Nothing) = returnRn (HsStrictnessInfo demands Nothing)
rnStrict HsBottom			  = returnRn HsBottom
\end{code}

UfCore expressions.

\begin{code}
rnCoreExpr (UfType ty)
  = rnHsType (text "unfolding type") ty	`thenRn` \ ty' ->
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
    bindLocalsRn "UfCase" [bndr]	$ \ [bndr'] ->
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
  = rnHsType (text str) ty	`thenRn` \ ty' ->
    bindLocalsRn str [name]	$ \ [name'] ->
    thing_inside (UfValBinder name' ty')
  where
    str = "unfolding id"
    
rnCoreBndr (UfTyBinder name kind) thing_inside
  = bindLocalsRn "unfolding tyvar" [name] $ \ [name'] ->
    thing_inside (UfTyBinder name' kind)
    
rnCoreBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapRn (rnHsType (text str)) tys	`thenRn` \ tys' ->
    bindLocalsRn str names		$ \ names' ->
    thing_inside (zipWith UfValBinder names' tys')
  where
    str   = "unfolding id"
    names = map (\ (UfValBinder name _ ) -> name) bndrs
    tys   = map (\ (UfValBinder _    ty) -> ty)   bndrs
\end{code}    

\begin{code}
rnCoreAlt (con, bndrs, rhs)
  = rnUfCon con				`thenRn` \ con' ->
    bindLocalsRn "unfolding alt" bndrs	$ \ bndrs' ->
    rnCoreExpr rhs			`thenRn` \ rhs' ->
    returnRn (con', bndrs', rhs')


rnNote (UfCoerce ty)
  = rnHsType (text "unfolding coerce") ty	`thenRn` \ ty' ->
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
  = rnHsType (text "litlit") ty		`thenRn` \ ty' ->
    returnRn (UfLitLitCon lit ty')

rnUfCon (UfPrimOp op)
  = lookupOccRn op		`thenRn` \ op' ->
    returnRn (UfPrimOp op')

rnUfCon (UfCCallOp str casm gc)
  = returnRn (UfCCallOp str casm gc)
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
	 nest 4 (pprContext ctxt)]

badDataCon name
   = hsep [ptext SLIT("Illegal data constructor name"), quotes (ppr name)]

ctxtErr1 doc tyvars ty (constraint, _)
  = addErrRn (
      sep [ptext SLIT("The constraint") <+> quotes (pprClassAssertion constraint) <+>
		   ptext SLIT("does not mention any of"),
	   nest 4 (ptext SLIT("the universally quantified type variables") <+> braces (interpp'SP tyvars)),
	   nest 4 (ptext SLIT("of the type") <+> quotes (ppr ty))
      ]
      $$
      (ptext SLIT("In") <+> doc)
    )

ctxtErr2 doc ty (constraint,_)
  = addErrRn (
	sep [ptext SLIT("The constraint") <+> quotes (pprClassAssertion constraint),
	nest 4 (ptext SLIT("mentions type variables that do not appear in the type")),
	nest 4 (quotes (ppr ty))]
        $$
	(ptext SLIT("In") <+> doc)
    )
\end{code}
