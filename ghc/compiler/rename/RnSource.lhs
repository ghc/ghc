%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnSource]{Main pass of renamer}

\begin{code}
#include "HsVersions.h"

module RnSource ( rnDecl, rnHsType, rnHsSigType ) where

IMP_Ubiq()
IMPORT_DELOOPER(RnLoop)		-- *check* the RnPass/RnExpr/RnBinds loop-breaking
IMPORT_1_3(List(partition))

import HsSyn
import HsDecls		( HsIdInfo(..) )
import HsPragmas
import HsTypes		( getTyVarName )
import RdrHsSyn
import RnHsSyn
import HsCore
import CmdLineOpts	( opt_IgnoreIfacePragmas )

import RnBinds		( rnTopBinds, rnMethodBinds )
import RnEnv		( bindTyVarsRn, lookupBndrRn, lookupOccRn, lookupImplicitOccRn, bindLocalsRn,
			  newSysName, newDfunName, checkDupOrQualNames, checkDupNames,
			  listType_RDR, tupleType_RDR )
import RnMonad

import Name		( Name, isLocallyDefined, 
			  OccName(..), occNameString, prefixOccName,
			  ExportFlag(..),
			  Provenance,
			  SYN_IE(NameSet), unionNameSets, emptyNameSet, mkNameSet, unitNameSet,
			  elemNameSet
			)
import ErrUtils		( addErrLoc, addShortErrLocLine, addShortWarnLocLine )
import FiniteMap	( emptyFM, lookupFM, addListToFM_C )
import Id		( GenId{-instance NamedThing-} )
import IdInfo		( IdInfo, StrictnessInfo(..), FBTypeInfo, DemandInfo, ArgUsageInfo )
import SpecEnv		( SpecEnv )
import Lex		( isLexCon )
import CoreUnfold	( Unfolding(..), SimpleUnfolding )
import MagicUFs		( MagicUnfoldingFun )
import PrelInfo		( derivingOccurrences, evalClass_RDR, numClass_RDR, allClass_NAME )
import ListSetOps	( unionLists, minusList )
import Maybes		( maybeToBool, catMaybes )
import Bag		( emptyBag, unitBag, consBag, unionManyBags, unionBags, listToBag, bagToList )
import Outputable	( PprStyle(..), Outputable(..){-instances-} )
import Pretty
import SrcLoc		( SrcLoc )
import Unique		( Unique )
import UniqSet		( SYN_IE(UniqSet) )
import UniqFM		( UniqFM, lookupUFM )
import Util	{-	( isIn, isn'tIn, thenCmp, removeDups, cmpPString,
			  panic, assertPanic{- , pprTrace ToDo:rm-} ) -}
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
    rnHsType ty			`thenRn` \ ty' ->

	-- Get the pragma info, unless we should ignore it
    (if opt_IgnoreIfacePragmas then
	returnRn []
     else
	setModeRn (InterfaceMode Optional) $
		-- In all the rest of the signature we read in optional mode,
		-- so that (a) we don't die
	mapRn rnIdInfo id_infos
    )				`thenRn` \ id_infos' -> 

    returnRn (SigD (IfaceSig name' ty' id_infos' loc))
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
    rnContext context   				`thenRn` \ context' ->
    checkDupOrQualNames data_doc con_names		`thenRn_`
    mapRn rnConDecl condecls				`thenRn` \ condecls' ->
    rnDerivs derivings					`thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyD (TyData new_or_data context' tycon' tyvars' condecls' derivings' noDataPragmas src_loc))
  where
    data_doc sty = text "the data type declaration for" <+> ppr sty tycon
    con_names = map conDeclName condecls

rnDecl (TyD (TySynonym name tyvars ty src_loc))
  = pushSrcLocRn src_loc $
    lookupBndrRn name				`thenRn` \ name' ->
    bindTyVarsRn syn_doc tyvars 		$ \ tyvars' ->
    rnHsType ty	    				`thenRn` \ ty' ->
    returnRn (TyD (TySynonym name' tyvars' ty' src_loc))
  where
    syn_doc sty = text "the declaration for type synonym" <+> ppr sty name
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
rnDecl (ClD (ClassDecl context cname tyvar sigs mbinds pragmas src_loc))
  = pushSrcLocRn src_loc $
    bindTyVarsRn cls_doc [tyvar]			$ \ [tyvar'] ->
    rnContext context	    				`thenRn` \ context' ->
    lookupBndrRn cname					`thenRn` \ cname' ->

	-- Check the signatures
    checkDupOrQualNames sig_doc sig_names		`thenRn_` 
    mapRn (rn_op cname' (getTyVarName tyvar')) sigs	`thenRn` \ sigs' ->


	-- Check the methods
    checkDupOrQualNames meth_doc meth_names		`thenRn_`
    rnMethodBinds mbinds				`thenRn` \ mbinds' ->

	-- Typechecker is responsible for checking that we only
	-- give default-method bindings for things in this class.
	-- The renamer *could* check this for class decls, but can't
	-- for instance decls.

    ASSERT(isNoClassPragmas pragmas)
    returnRn (ClD (ClassDecl context' cname' tyvar' sigs' mbinds' NoClassPragmas src_loc))
  where
    cls_doc sty  = text "the declaration for class" 	<+> ppr sty cname
    sig_doc sty  = text "the signatures for class"  	<+> ppr sty cname
    meth_doc sty = text "the default-methods for class" <+> ppr sty cname

    sig_names   = [(op,locn) | ClassOpSig op _ _ locn <- sigs]
    meth_names   = bagToList (collectMonoBinders mbinds)

    rn_op clas clas_tyvar sig@(ClassOpSig op _ ty locn)
      = pushSrcLocRn locn $
	lookupBndrRn op				`thenRn` \ op_name ->
	rnHsSigType (\sty -> ppr sty op) ty	`thenRn` \ new_ty  ->

		-- Call up interface info for default method, if such info exists
	let
	    dm_occ = prefixOccName SLIT("$m") (rdrNameOcc op)
	in
        newSysName dm_occ Exported locn		`thenRn` \ dm_name ->
	setModeRn (InterfaceMode Optional) (
            addOccurrenceName dm_name
	)						`thenRn_`

		-- Checks.....
	let
	    (ctxt, op_ty) = case new_ty of
				HsForAllTy tvs ctxt op_ty -> (ctxt, op_ty)
				other			  -> ([], new_ty)
	    ctxt_fvs  = extractCtxtTyNames ctxt
	    op_ty_fvs = extractHsTyNames op_ty		-- Includes tycons/classes but we
							-- don't care about that
	in
		-- Check that class tyvar appears in op_ty
        checkRn (clas_tyvar `elemNameSet` op_ty_fvs)
	        (classTyVarNotInOpTyErr clas_tyvar sig)
							 `thenRn_`

		-- Check that class tyvar *doesn't* appear in the sig's context
        checkRn (not (clas_tyvar `elemNameSet` ctxt_fvs))
		(classTyVarInOpCtxtErr clas_tyvar sig)
							 `thenRn_`

	returnRn (ClassOpSig op_name dm_name new_ty locn)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (InstD (InstDecl inst_ty mbinds uprags maybe_dfun src_loc))
  = pushSrcLocRn src_loc $
    rnHsSigType (\sty -> text "an instance decl") inst_ty	`thenRn` \ inst_ty' ->


	-- Rename the bindings
	-- NB meth_names can be qualified!
    checkDupNames meth_doc meth_names 		`thenRn_`
    rnMethodBinds mbinds			`thenRn` \ mbinds' ->
    mapRn rn_uprag uprags			`thenRn` \ new_uprags ->

    newDfunName maybe_dfun src_loc		`thenRn` \ dfun_name ->
    addOccurrenceName dfun_name			`thenRn_`
			-- The dfun is not optional, because we use its version number
			-- to identify the version of the instance declaration

	-- The typechecker checks that all the bindings are for the right class.
    returnRn (InstD (InstDecl inst_ty' mbinds' new_uprags (Just dfun_name) src_loc))
  where
    meth_doc sty = text "the bindings in an instance declaration"
    meth_names   = bagToList (collectMonoBinders mbinds)

    rn_uprag (SpecSig op ty using locn)
      = pushSrcLocRn src_loc $
	lookupBndrRn op				`thenRn` \ op_name ->
	rnHsSigType (\sty -> ppr sty op) ty	`thenRn` \ new_ty ->
	rn_using using				`thenRn` \ new_using ->
	returnRn (SpecSig op_name new_ty new_using locn)

    rn_uprag (InlineSig op locn)
      = pushSrcLocRn locn $
	lookupBndrRn op			`thenRn` \ op_name ->
	returnRn (InlineSig op_name locn)

    rn_uprag (DeforestSig op locn)
      = pushSrcLocRn locn $
	lookupBndrRn op			`thenRn` \ op_name ->
	returnRn (DeforestSig op_name locn)

    rn_uprag (MagicUnfoldingSig op str locn)
      = pushSrcLocRn locn $
	lookupBndrRn op			`thenRn` \ op_name ->
	returnRn (MagicUnfoldingSig op_name str locn)

    rn_using Nothing  = returnRn Nothing
    rn_using (Just v) = lookupOccRn v	`thenRn` \ new_v ->
			returnRn (Just new_v)
\end{code}

%*********************************************************
%*							*
\subsection{Default declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (DefD (DefaultDecl tys src_loc))
  = pushSrcLocRn src_loc $
    mapRn rnHsType tys 			`thenRn` \ tys' ->
    lookupImplicitOccRn numClass_RDR	`thenRn_` 
    returnRn (DefD (DefaultDecl tys' src_loc))
\end{code}

%*********************************************************
%*							*
\subsection{Support code for type/data declarations}
%*							*
%*********************************************************

\begin{code}
rnDerivs :: Maybe [RdrName] -> RnMS s (Maybe [Name])

rnDerivs Nothing -- derivs not specified
  = lookupImplicitOccRn evalClass_RDR		`thenRn_`
    returnRn Nothing

rnDerivs (Just ds)
  = lookupImplicitOccRn evalClass_RDR		`thenRn_`
    mapRn rn_deriv ds `thenRn` \ derivs ->
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
conDeclName (ConDecl n _ _ l)     = (n,l)

rnConDecl :: RdrNameConDecl -> RnMS s RenamedConDecl
rnConDecl (ConDecl name cxt details locn)
  = pushSrcLocRn locn $
    checkConName name			`thenRn_` 
    lookupBndrRn name			`thenRn` \ new_name ->
    rnConDetails name locn details	`thenRn` \ new_details -> 
    rnContext cxt			`thenRn` \ new_context ->
    returnRn (ConDecl new_name new_context new_details locn)

rnConDetails con locn (VanillaCon tys)
  = mapRn rnBangTy tys		`thenRn` \ new_tys  ->
    returnRn (VanillaCon new_tys)

rnConDetails con locn (InfixCon ty1 ty2)
  = rnBangTy ty1  		`thenRn` \ new_ty1 ->
    rnBangTy ty2  		`thenRn` \ new_ty2 ->
    returnRn (InfixCon new_ty1 new_ty2)

rnConDetails con locn (NewCon ty)
  = rnHsType ty			`thenRn` \ new_ty  ->
    returnRn (NewCon new_ty)

rnConDetails con locn (RecCon fields)
  = checkDupOrQualNames fld_doc field_names	`thenRn_`
    mapRn rnField fields			`thenRn` \ new_fields ->
    returnRn (RecCon new_fields)
  where
    fld_doc sty = text "the fields of constructor" <> ppr sty con
    field_names = [(fld, locn) | (flds, _) <- fields, fld <- flds]

rnField (names, ty)
  = mapRn lookupBndrRn names	`thenRn` \ new_names ->
    rnBangTy ty			`thenRn` \ new_ty ->
    returnRn (new_names, new_ty) 

rnBangTy (Banged ty)
  = rnHsType ty `thenRn` \ new_ty ->
    returnRn (Banged new_ty)

rnBangTy (Unbanged ty)
  = rnHsType ty `thenRn` \ new_ty ->
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
rnHsSigType :: (PprStyle -> Doc) -> RdrNameHsType -> RnMS s RenamedHsType 
	-- rnHsSigType is used for source-language type signatures,
	-- which use *implicit* universal quantification.

rnHsSigType doc_str full_ty@(HsPreForAllTy ctxt ty)	-- From source code (no kinds on tyvars)
  = getNameEnv		`thenRn` \ name_env ->
    let
	mentioned_tyvars = extractHsTyVars full_ty
	forall_tyvars    = filter not_in_scope mentioned_tyvars
	not_in_scope tv  = case lookupFM name_env tv of
				    Nothing -> True
				    Just _  -> False

	non_foralld_constrained = [tv | (clas, ty) <- ctxt,
					tv <- extractHsTyVars ty,
					not (tv `elem` forall_tyvars)
				  ]
    in
    checkRn (null non_foralld_constrained)
	    (ctxtErr sig_doc non_foralld_constrained)	`thenRn_`

    (bindTyVarsRn sig_doc (map UserTyVar forall_tyvars)	$ \ new_tyvars ->
     rnContext ctxt					`thenRn` \ new_ctxt ->
     rnHsType ty					`thenRn` \ new_ty ->
     returnRn (HsForAllTy new_tyvars new_ctxt new_ty)
    )
  where
    sig_doc sty = text "the type signature for" <+> doc_str sty
			     

rnHsSigType doc_str other_ty = rnHsType other_ty

rnHsType :: RdrNameHsType -> RnMS s RenamedHsType
rnHsType (HsForAllTy tvs ctxt ty)		-- From an interface file (tyvars may be kinded)
  = rn_poly_help tvs ctxt ty

rnHsType full_ty@(HsPreForAllTy ctxt ty)	-- A (context => ty) embedded in a type.
						-- Universally quantify over tyvars in context
  = getNameEnv		`thenRn` \ name_env ->
    let
	forall_tyvars = foldr unionLists [] (map (extractHsTyVars . snd) ctxt)
    in
    rn_poly_help (map UserTyVar forall_tyvars) ctxt ty

rnHsType (MonoTyVar tyvar)
  = lookupOccRn tyvar 		`thenRn` \ tyvar' ->
    returnRn (MonoTyVar tyvar')

rnHsType (MonoFunTy ty1 ty2)
  = andRn MonoFunTy (rnHsType ty1) (rnHsType ty2)

rnHsType (MonoListTy _ ty)
  = lookupImplicitOccRn listType_RDR		`thenRn` \ tycon_name ->
    rnHsType ty					`thenRn` \ ty' ->
    returnRn (MonoListTy tycon_name ty')

rnHsType (MonoTupleTy _ tys)
  = lookupImplicitOccRn (tupleType_RDR (length tys))	`thenRn` \ tycon_name ->
    mapRn rnHsType tys					`thenRn` \ tys' ->
    returnRn (MonoTupleTy tycon_name tys')

rnHsType (MonoTyApp ty1 ty2)
  = rnHsType ty1		`thenRn` \ ty1' ->
    rnHsType ty2		`thenRn` \ ty2' ->
    returnRn (MonoTyApp ty1' ty2')

rnHsType (MonoDictTy clas ty)
  = lookupOccRn clas		`thenRn` \ clas' ->
    rnHsType ty			`thenRn` \ ty' ->
    returnRn (MonoDictTy clas' ty')

rn_poly_help :: [HsTyVar RdrName]		-- Universally quantified tyvars
	     -> RdrNameContext
	     -> RdrNameHsType
	     -> RnMS s RenamedHsType
rn_poly_help tyvars ctxt ty
  = bindTyVarsRn sig_doc tyvars				$ \ new_tyvars ->
    rnContext ctxt					`thenRn` \ new_ctxt ->
    rnHsType ty						`thenRn` \ new_ty ->
    returnRn (HsForAllTy new_tyvars new_ctxt new_ty)
  where
    sig_doc sty = text "a nested for-all type"
\end{code}


\begin{code}
rnContext :: RdrNameContext -> RnMS s RenamedContext

rnContext  ctxt
  = mapRn rn_ctxt ctxt	`thenRn` \ result ->
    let
	(_, dup_asserts) = removeDups cmp_assert result
	(alls, theta)    = partition (\(c,_) -> c == allClass_NAME) result
	non_tyvar_alls   = [(c,t) | (c,t) <- alls, not (is_tyvar t)]
    in

	-- Check for duplicate assertions
	-- If this isn't an error, then it ought to be:
    mapRn (addWarnRn . dupClassAssertWarn theta) dup_asserts `thenRn_`

	-- Check for All constraining a non-type-variable
    mapRn (addWarnRn . allOfNonTyVar) non_tyvar_alls	`thenRn_`
    
	-- Done.  Return a theta omitting all the "All" constraints.
	-- They have done done their work by ensuring that we universally
	-- quantify over their tyvar.
    returnRn theta
  where
    rn_ctxt (clas, ty)
      =		-- Mini hack here.  If the class is our pseudo-class "All",
		-- then we don't want to record it as an occurrence, otherwise
		-- we try to slurp it in later and it doesn't really exist at all.
		-- Easiest thing is simply not to put it in the occurrence set.
	lookupBndrRn clas	`thenRn` \ clas_name ->
	(if clas_name /= allClass_NAME then
		addOccurrenceName clas_name
	 else
		returnRn clas_name
	)			`thenRn_`
	rnHsType ty		`thenRn` \ ty' ->
	returnRn (clas_name, ty')

    cmp_assert (c1,ty1) (c2,ty2)
      = (c1 `cmp` c2) `thenCmp` (cmpHsType cmp ty1 ty2)

    is_tyvar (MonoTyVar _) = True
    is_tyvar other         = False
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

rnIdInfo (HsUnfold inline expr)	= rnCoreExpr expr	`thenRn` \ expr' ->
				  returnRn (HsUnfold inline expr')
rnIdInfo (HsArity arity)	= returnRn (HsArity arity)
rnIdInfo (HsUpdate update)	= returnRn (HsUpdate update)
rnIdInfo (HsFBType fb)		= returnRn (HsFBType fb)
rnIdInfo (HsArgUsage au)	= returnRn (HsArgUsage au)
rnIdInfo (HsDeforest df)	= returnRn (HsDeforest df)

rnStrict (StrictnessInfo demands (Just (worker,cons)))
	-- The sole purpose of the "cons" field is so that we can mark the constructors
	-- needed to build the wrapper as "needed", so that their data type decl will be
	-- slurped in. After that their usefulness is o'er, so we just put in the empty list.
  = lookupOccRn worker			`thenRn` \ worker' ->
    mapRn lookupOccRn cons		`thenRn_` 
    returnRn (StrictnessInfo demands (Just (worker',[])))

-- Boring, but necessary for the type checker.
rnStrict (StrictnessInfo demands Nothing) = returnRn (StrictnessInfo demands Nothing)
rnStrict BottomGuaranteed		  = returnRn BottomGuaranteed
rnStrict NoStrictnessInfo		  = returnRn NoStrictnessInfo
\end{code}

UfCore expressions.

\begin{code}
rnCoreExpr (UfVar v)
  = lookupOccRn v 	`thenRn` \ v' ->
    returnRn (UfVar v')

rnCoreExpr (UfLit lit) = returnRn (UfLit lit)

rnCoreExpr (UfCon con args) 
  = lookupOccRn con		`thenRn` \ con' ->
    mapRn rnCoreArg args	`thenRn` \ args' ->
    returnRn (UfCon con' args')

rnCoreExpr (UfPrim prim args) 
  = rnCorePrim prim		`thenRn` \ prim' ->
    mapRn rnCoreArg args	`thenRn` \ args' ->
    returnRn (UfPrim prim' args')

rnCoreExpr (UfApp fun arg)
  = rnCoreExpr fun		`thenRn` \ fun' ->
    rnCoreArg arg		`thenRn` \ arg' ->
    returnRn (UfApp fun' arg')

rnCoreExpr (UfCase scrut alts) 
  = rnCoreExpr scrut		`thenRn` \ scrut' ->
    rnCoreAlts alts		`thenRn` \ alts' ->
    returnRn (UfCase scrut' alts')

rnCoreExpr (UfSCC cc expr) 
  = rnCoreExpr expr		`thenRn` \ expr' ->
    returnRn  (UfSCC cc expr') 

rnCoreExpr(UfCoerce coercion ty body)
  = rnCoercion coercion		`thenRn` \ coercion' ->
    rnHsType ty			`thenRn` \ ty' ->
    rnCoreExpr body		`thenRn` \ body' ->
    returnRn (UfCoerce coercion' ty' body')

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
  = rnHsType ty			`thenRn` \ ty' ->
    bindLocalsRn "unfolding value" [name] $ \ [name'] ->
    thing_inside (UfValBinder name' ty')
    
rnCoreBndr (UfTyBinder name kind) thing_inside
  = bindLocalsRn "unfolding tyvar" [name] $ \ [name'] ->
    thing_inside (UfTyBinder name' kind)
    
rnCoreBndr (UfUsageBinder name) thing_inside
  = bindLocalsRn "unfolding usage" [name] $ \ [name'] ->
    thing_inside (UfUsageBinder name')

rnCoreBndrs bndrs thing_inside		-- Expect them all to be ValBinders
  = mapRn rnHsType tys			`thenRn` \ tys' ->
    bindLocalsRn "unfolding value" names $ \ names' ->
    thing_inside (zipWith UfValBinder names' tys')
  where
    names = map (\ (UfValBinder name _) -> name) bndrs
    tys   = map (\ (UfValBinder _   ty) -> ty)   bndrs

rnCoreBndrNamess names thing_inside
  = bindLocalsRn "unfolding value" names $ \ names' ->
    thing_inside names'
\end{code}    

\begin{code}
rnCoreArg (UfVarArg v)	 = lookupOccRn v	`thenRn` \ v' -> returnRn (UfVarArg v')
rnCoreArg (UfUsageArg u) = lookupOccRn u	`thenRn` \ u' -> returnRn (UfUsageArg u')
rnCoreArg (UfTyArg ty)	 = rnHsType ty			`thenRn` \ ty' -> returnRn (UfTyArg ty')
rnCoreArg (UfLitArg lit) = returnRn (UfLitArg lit)

rnCoreAlts (UfAlgAlts alts deflt)
  = mapRn rn_alt alts		`thenRn` \ alts' ->
    rnCoreDefault deflt		`thenRn` \ deflt' ->
    returnRn (UfAlgAlts alts' deflt')
  where
    rn_alt (con, bndrs, rhs) =	lookupOccRn con			`thenRn` \ con' ->
				bindLocalsRn "unfolding alt" bndrs	$ \ bndrs' ->
				rnCoreExpr rhs				`thenRn` \ rhs' ->
				returnRn (con', bndrs', rhs')

rnCoreAlts (UfPrimAlts alts deflt)
  = mapRn rn_alt alts		`thenRn` \ alts' ->
    rnCoreDefault deflt		`thenRn` \ deflt' ->
    returnRn (UfPrimAlts alts' deflt')
  where
    rn_alt (lit, rhs) =	rnCoreExpr rhs		`thenRn` \ rhs' ->
			returnRn (lit, rhs')

rnCoreDefault UfNoDefault = returnRn UfNoDefault
rnCoreDefault (UfBindDefault bndr rhs) = bindLocalsRn "unfolding default" [bndr]	$ \ [bndr'] ->
					 rnCoreExpr rhs					`thenRn` \ rhs' ->
				  	 returnRn (UfBindDefault bndr' rhs')

rnCoercion (UfIn  n) = lookupOccRn n `thenRn` \ n' -> returnRn (UfIn  n')
rnCoercion (UfOut n) = lookupOccRn n `thenRn` \ n' -> returnRn (UfOut n')

rnCorePrim (UfOtherOp op) 
  = lookupOccRn op	`thenRn` \ op' ->
    returnRn (UfOtherOp op')

rnCorePrim (UfCCallOp str casm gc arg_tys res_ty)
  = mapRn rnHsType arg_tys	`thenRn` \ arg_tys' ->
    rnHsType res_ty		`thenRn` \ res_ty' ->
    returnRn (UfCCallOp str casm gc arg_tys' res_ty')
\end{code}

%*********************************************************
%*							*
\subsection{Errors}
%*							*
%*********************************************************

\begin{code}
derivingNonStdClassErr clas sty
  = hsep [ptext SLIT("non-standard class in deriving:"), ppr sty clas]

classTyVarNotInOpTyErr clas_tyvar sig sty
  = hang (hcat [ptext SLIT("Class type variable `"), 
		       ppr sty clas_tyvar, 
		       ptext SLIT("' does not appear in method signature:")])
	 4 (ppr sty sig)

classTyVarInOpCtxtErr clas_tyvar sig sty
  = hang (hcat [ ptext SLIT("Class type variable `"), ppr sty clas_tyvar, 
			ptext SLIT("' present in method's local overloading context:")])
	 4 (ppr sty sig)

dupClassAssertWarn ctxt dups sty
  = hang (hcat [ptext SLIT("Duplicate class assertion `"), 
		       ppr sty dups, 
		       ptext SLIT("' in context:")])
	 4 (ppr sty ctxt)

badDataCon name sty
   = hsep [ptext SLIT("Illegal data constructor name:"), ppr sty name]

allOfNonTyVar ty sty
  = hsep [ptext SLIT("`All' applied to a non-type variable:"), ppr sty ty]

ctxtErr doc tyvars sty
  = hsep [ptext SLIT("Context constrains type variable(s)"), 
	  hsep (punctuate comma (map (ppr sty) tyvars))]
    $$ nest 4 (ptext SLIT("in") <+> doc sty)
\end{code}
