%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnSource]{Main pass of renamer}

\begin{code}
#include "HsVersions.h"

module RnSource ( rnDecl, rnHsType ) where

IMP_Ubiq()
IMPORT_DELOOPER(RnLoop)		-- *check* the RnPass/RnExpr/RnBinds loop-breaking

import HsSyn
import HsDecls		( HsIdInfo(..) )
import HsPragmas
import HsTypes		( getTyVarName )
import RdrHsSyn
import RnHsSyn
import HsCore

import RnBinds		( rnTopBinds, rnMethodBinds )
import RnEnv		( bindTyVarsRn, lookupRn, lookupOccRn, lookupImplicitOccRn, bindLocalsRn,
			  lookupOptionalOccRn, newDfunName, 
			  listType_RDR, tupleType_RDR )
import RnMonad

import Name		( Name, isLocallyDefined, isTvOcc, pprNonSym,
			  Provenance,
			  SYN_IE(NameSet), unionNameSets, emptyNameSet, mkNameSet, unitNameSet,
			  elemNameSet
			)
import ErrUtils		( addErrLoc, addShortErrLocLine, addShortWarnLocLine )
import FiniteMap	( emptyFM, lookupFM, addListToFM_C )
import Id		( GenId{-instance NamedThing-} )
import IdInfo		( IdInfo, StrictnessInfo(..), FBTypeInfo, DemandInfo, ArgUsageInfo )
import SpecEnv		( SpecEnv )
import CoreUnfold	( Unfolding(..), SimpleUnfolding )
import MagicUFs		( MagicUnfoldingFun )
import PrelInfo		( derivingOccurrences, evalClass_RDR, numClass_RDR )
import ListSetOps	( unionLists, minusList )
import Maybes		( maybeToBool, catMaybes )
import Bag		( emptyBag, unitBag, consBag, unionManyBags, unionBags, listToBag, bagToList )
import Outputable	( Outputable(..){-instances-} )
--import PprStyle 	-- ToDo:rm 
import Pretty
import SrcLoc		( SrcLoc )
-- import TyCon		( TyCon{-instance NamedThing-} )
import Unique		( Unique )
import UniqSet		( SYN_IE(UniqSet) )
import UniqFM		( UniqFM, lookupUFM )
import Util		( isIn, isn'tIn, thenCmp, removeDups, cmpPString,
			  panic, assertPanic{- , pprTrace ToDo:rm-} )
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
    lookupRn name		`thenRn` \ name' ->
    rnHsType ty			`thenRn` \ ty' ->
    mapRn rnIdInfo id_infos	`thenRn` \ id_infos' -> 
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
rnDecl (TyD (TyData context tycon tyvars condecls derivings pragmas src_loc))
  = pushSrcLocRn src_loc $
    lookupRn tycon		    		`thenRn` \ tycon' ->
    bindTyVarsRn "data declaration" tyvars	$ \ tyvars' ->
    rnContext context   			`thenRn` \ context' ->
    mapRn rnConDecl condecls			`thenRn` \ condecls' ->
    rnDerivs derivings				`thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyD (TyData context' tycon' tyvars' condecls' derivings' noDataPragmas src_loc))

rnDecl (TyD (TyNew context tycon tyvars condecl derivings pragmas src_loc))
  = pushSrcLocRn src_loc $
    lookupRn tycon				`thenRn` \ tycon' ->
    bindTyVarsRn "newtype declaration" tyvars 	$ \ tyvars' ->
    rnContext context				`thenRn` \ context' ->
    rnConDecl condecl				`thenRn` \ condecl' ->
    rnDerivs derivings				`thenRn` \ derivings' ->
    ASSERT(isNoDataPragmas pragmas)
    returnRn (TyD (TyNew context' tycon' tyvars' condecl' derivings' noDataPragmas src_loc))

rnDecl (TyD (TySynonym name tyvars ty src_loc))
  = pushSrcLocRn src_loc $
    lookupRn name				`thenRn` \ name' ->
    bindTyVarsRn "type declaration" tyvars 	$ \ tyvars' ->
    rnHsType ty	    				`thenRn` \ ty' ->
    returnRn (TyD (TySynonym name' tyvars' ty' src_loc))
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
    bindTyVarsRn "class declaration" [tyvar]		$ \ [tyvar'] ->
    rnContext context	    				`thenRn` \ context' ->
    lookupRn cname					`thenRn` \ cname' ->
    mapRn (rn_op cname' (getTyVarName tyvar')) sigs	`thenRn` \ sigs' ->
    rnMethodBinds mbinds				`thenRn` \ mbinds' ->
    ASSERT(isNoClassPragmas pragmas)
    returnRn (ClD (ClassDecl context' cname' tyvar' sigs' mbinds' NoClassPragmas src_loc))
  where
    rn_op clas clas_tyvar sig@(ClassOpSig op ty pragmas locn)
      = pushSrcLocRn locn $
	lookupRn op			`thenRn` \ op_name ->
	rnHsType ty			`thenRn` \ new_ty  ->
	let
	    (ctxt, op_ty) = case new_ty of
				HsForAllTy tvs ctxt op_ty -> (ctxt, op_ty)
				other			  -> ([], new_ty)
	    ctxt_fvs  = extractCtxtTyNames ctxt
	    op_ty_fvs = extractHsTyNames op_ty		-- Includes tycons/classes but we
							-- don't care about that
	in
	-- check that class tyvar appears in op_ty
        checkRn (clas_tyvar `elemNameSet` op_ty_fvs)
	        (classTyVarNotInOpTyErr clas_tyvar sig)
							 `thenRn_`

	-- check that class tyvar *doesn't* appear in the sig's context
        checkRn (not (clas_tyvar `elemNameSet` ctxt_fvs))
		(classTyVarInOpCtxtErr clas_tyvar sig)
							 `thenRn_`

	ASSERT(isNoClassOpPragmas pragmas)
	returnRn (ClassOpSig op_name new_ty noClassOpPragmas locn)
\end{code}


%*********************************************************
%*							*
\subsection{Instance declarations}
%*							*
%*********************************************************

\begin{code}
rnDecl (InstD (InstDecl inst_ty mbinds uprags maybe_dfun_name src_loc))
  = pushSrcLocRn src_loc $
    rnHsType inst_ty			`thenRn` \ inst_ty' ->
    rnMethodBinds mbinds		`thenRn` \ mbinds' ->
    mapRn rn_uprag uprags		`thenRn` \ new_uprags ->
    rn_dfun maybe_dfun_name		`thenRn` \ dfun_name' ->

    returnRn (InstD (InstDecl inst_ty' mbinds' new_uprags dfun_name' src_loc))
  where
    rn_dfun Nothing  = newDfunName src_loc	`thenRn` \ n' ->
		       returnRn (Just n')
    rn_dfun (Just n) = lookupOccRn n		`thenRn` \ n' ->
				-- The dfun is not optional, because we use its version number
				-- to identify the version of the instance declaration
		       returnRn (Just n')

    rn_uprag (SpecSig op ty using locn)
      = pushSrcLocRn src_loc $
	lookupRn op			`thenRn` \ op_name ->
	rnHsType ty			`thenRn` \ new_ty ->
	rn_using using			`thenRn` \ new_using ->
	returnRn (SpecSig op_name new_ty new_using locn)

    rn_uprag (InlineSig op locn)
      = pushSrcLocRn locn $
	lookupRn op			`thenRn` \ op_name ->
	returnRn (InlineSig op_name locn)

    rn_uprag (DeforestSig op locn)
      = pushSrcLocRn locn $
	lookupRn op			`thenRn` \ op_name ->
	returnRn (DeforestSig op_name locn)

    rn_uprag (MagicUnfoldingSig op str locn)
      = pushSrcLocRn locn $
	lookupRn op			`thenRn` \ op_name ->
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
rnConDecl :: RdrNameConDecl -> RnMS s RenamedConDecl

rnConDecl (ConDecl name tys src_loc)
  = pushSrcLocRn src_loc $
    lookupRn name		`thenRn` \ new_name ->
    mapRn rnBangTy tys		`thenRn` \ new_tys  ->
    returnRn (ConDecl new_name new_tys src_loc)

rnConDecl (ConOpDecl ty1 op ty2 src_loc)
  = pushSrcLocRn src_loc $
    lookupRn op			`thenRn` \ new_op  ->
    rnBangTy ty1  		`thenRn` \ new_ty1 ->
    rnBangTy ty2  		`thenRn` \ new_ty2 ->
    returnRn (ConOpDecl new_ty1 new_op new_ty2 src_loc)

rnConDecl (NewConDecl name ty src_loc)
  = pushSrcLocRn src_loc $
    lookupRn name		`thenRn` \ new_name ->
    rnHsType ty			`thenRn` \ new_ty  ->
    returnRn (NewConDecl new_name new_ty src_loc)

rnConDecl (RecConDecl name fields src_loc)
  = pushSrcLocRn src_loc $
    lookupRn name		`thenRn` \ new_name ->
    mapRn rnField fields	`thenRn` \ new_fields ->
    returnRn (RecConDecl new_name new_fields src_loc)

rnField (names, ty)
  = mapRn lookupRn names	`thenRn` \ new_names ->
    rnBangTy ty			`thenRn` \ new_ty ->
    returnRn (new_names, new_ty) 

rnBangTy (Banged ty)
  = rnHsType ty `thenRn` \ new_ty ->
    returnRn (Banged new_ty)

rnBangTy (Unbanged ty)
  = rnHsType ty `thenRn` \ new_ty ->
    returnRn (Unbanged new_ty)
\end{code}


%*********************************************************
%*							*
\subsection{Support code to rename types}
%*							*
%*********************************************************

\begin{code}
rnHsType :: RdrNameHsType -> RnMS s RenamedHsType

rnHsType (HsForAllTy tvs ctxt ty)
  = rn_poly_help tvs ctxt ty

rnHsType full_ty@(HsPreForAllTy ctxt ty)
  = getNameEnv		`thenRn` \ name_env ->
    let
	mentioned_tyvars = extractHsTyVars full_ty
	forall_tyvars    = filter not_in_scope mentioned_tyvars
	not_in_scope tv  = case lookupFM name_env tv of
				    Nothing -> True
				    Just _  -> False
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

rnHsType (MonoTyApp name tys)
  = lookupOccRn name		`thenRn` \ name' ->
    mapRn rnHsType tys		`thenRn` \ tys' ->
    returnRn (MonoTyApp name' tys')

rnHsType (MonoDictTy clas ty)
  = lookupOccRn clas		`thenRn` \ clas' ->
    rnHsType ty			`thenRn` \ ty' ->
    returnRn (MonoDictTy clas' ty')


rn_poly_help :: [HsTyVar RdrName]		-- Universally quantified tyvars
	     -> RdrNameContext
	     -> RdrNameHsType
	     -> RnMS s RenamedHsType

rn_poly_help tyvars ctxt ty
  = bindTyVarsRn "type signature" tyvars		$ \ new_tyvars ->
    rnContext ctxt					`thenRn` \ new_ctxt ->
    rnHsType ty						`thenRn` \ new_ty ->
    returnRn (HsForAllTy new_tyvars new_ctxt new_ty)
\end{code}


\begin{code}
rnContext :: RdrNameContext -> RnMS s RenamedContext

rnContext  ctxt
  = mapRn rn_ctxt ctxt	`thenRn` \ result ->
    let
	(_, dup_asserts) = removeDups cmp_assert result
    in
    -- If this isn't an error, then it ought to be:
    mapRn (addWarnRn . dupClassAssertWarn result) dup_asserts `thenRn_`
    returnRn result
  where
    rn_ctxt (clas, ty)
      = lookupOccRn clas	`thenRn` \ clas_name ->
	rnHsType ty		`thenRn` \ ty' ->
	returnRn (clas_name, ty')

    cmp_assert (c1,ty1) (c2,ty2)
      = (c1 `cmp` c2) `thenCmp` (cmpHsType cmp ty1 ty2)
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

rnIdInfo (HsUnfold expr)	= rnCoreExpr expr	`thenRn` \ expr' ->
				  returnRn (HsUnfold expr')
rnIdInfo (HsArity arity)	= returnRn (HsArity arity)
rnIdInfo (HsUpdate update)	= returnRn (HsUpdate update)
rnIdInfo (HsFBType fb)		= returnRn (HsFBType fb)
rnIdInfo (HsArgUsage au)	= returnRn (HsArgUsage au)
rnIdInfo (HsDeforest df)	= returnRn (HsDeforest df)

rnStrict (StrictnessInfo demands (Just worker))
  = lookupOptionalOccRn worker		`thenRn` \ worker' ->
    returnRn (StrictnessInfo demands (Just worker'))

-- Boring, but necessary for the type checker.
rnStrict (StrictnessInfo demands Nothing) = returnRn (StrictnessInfo demands Nothing)
rnStrict BottomGuaranteed		  = returnRn BottomGuaranteed
rnStrict NoStrictnessInfo		  = returnRn NoStrictnessInfo
\end{code}

UfCore expressions.

\begin{code}
rnCoreExpr (UfVar v)
  = lookupOptionalOccRn v 	`thenRn` \ v' ->
    returnRn (UfVar v')

rnCoreExpr (UfLit lit) = returnRn (UfLit lit)

rnCoreExpr (UfCon con args) 
  = lookupOptionalOccRn con		`thenRn` \ con' ->
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
\end{code}    

\begin{code}
rnCoreArg (UfVarArg v)	 = lookupOptionalOccRn v	`thenRn` \ v' -> returnRn (UfVarArg v')
rnCoreArg (UfUsageArg u) = lookupOptionalOccRn u	`thenRn` \ u' -> returnRn (UfUsageArg u')
rnCoreArg (UfTyArg ty)	 = rnHsType ty			`thenRn` \ ty' -> returnRn (UfTyArg ty')
rnCoreArg (UfLitArg lit) = returnRn (UfLitArg lit)

rnCoreAlts (UfAlgAlts alts deflt)
  = mapRn rn_alt alts		`thenRn` \ alts' ->
    rnCoreDefault deflt		`thenRn` \ deflt' ->
    returnRn (UfAlgAlts alts' deflt')
  where
    rn_alt (con, bndrs, rhs) =	lookupOptionalOccRn con	`thenRn` \ con' ->
				rnCoreBndrs bndrs	$ \ bndrs' ->
				rnCoreExpr rhs		`thenRn` \ rhs' ->
				returnRn (con', bndrs', rhs')

rnCoreAlts (UfPrimAlts alts deflt)
  = mapRn rn_alt alts		`thenRn` \ alts' ->
    rnCoreDefault deflt		`thenRn` \ deflt' ->
    returnRn (UfPrimAlts alts' deflt')
  where
    rn_alt (lit, rhs) =	rnCoreExpr rhs		`thenRn` \ rhs' ->
			returnRn (lit, rhs')

rnCoreDefault UfNoDefault = returnRn UfNoDefault
rnCoreDefault (UfBindDefault bndr rhs) = rnCoreBndr bndr	$ \ bndr' ->
					 rnCoreExpr rhs		`thenRn` \ rhs' ->
				  	 returnRn (UfBindDefault bndr' rhs')

rnCoercion (UfIn  n) = lookupOptionalOccRn n `thenRn` \ n' -> returnRn (UfIn  n')
rnCoercion (UfOut n) = lookupOptionalOccRn n `thenRn` \ n' -> returnRn (UfOut n')

rnCorePrim (UfOtherOp op) 
  = lookupOptionalOccRn op	`thenRn` \ op' ->
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
  = ppCat [ppStr "non-standard class in deriving:", ppr sty clas]

classTyVarNotInOpTyErr clas_tyvar sig sty
  = ppHang (ppBesides [ppStr "Class type variable `", ppr sty clas_tyvar, ppStr "' does not appear in method signature:"])
	 4 (ppr sty sig)

classTyVarInOpCtxtErr clas_tyvar sig sty
  = ppHang (ppBesides [ ppStr "Class type variable `", ppr sty clas_tyvar, 
			ppStr "' present in method's local overloading context:"])
	 4 (ppr sty sig)

dupClassAssertWarn ctxt dups sty
  = ppHang (ppBesides [ppStr "Duplicate class assertion `", ppr sty dups, ppStr "' in context:"])
	 4 (ppr sty ctxt)
\end{code}





===================	OLD STUFF    ======================

%*********************************************************
%*							 *
\subsection{SPECIALIZE data pragmas}
%*							 *
%*********************************************************

\begin{pseudocode}
rnSpecDataSig :: RdrNameSpecDataSig
	      -> RnMS s RenamedSpecDataSig

rnSpecDataSig (SpecDataSig tycon ty src_loc)
  = pushSrcLocRn src_loc $
    let
	tyvars = filter extractHsTyNames ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn` \ (tv_env,_) ->
    lookupOccRn tycon			`thenRn` \ tycon' ->
    rnHsType tv_env ty		`thenRn` \ ty' ->
    returnRn (SpecDataSig tycon' ty' src_loc)

\end{pseudocode}

%*********************************************************
%*							*
\subsection{@SPECIALIZE instance@ user-pragmas}
%*							*
%*********************************************************

\begin{pseudocode}
rnSpecInstSig :: RdrNameSpecInstSig
	      -> RnMS s RenamedSpecInstSig

rnSpecInstSig (SpecInstSig clas ty src_loc)
  = pushSrcLocRn src_loc $
    let
	tyvars = extractHsTyNames is_tyvar_name ty
    in
    mkTyVarNamesEnv src_loc tyvars     	`thenRn` \ (tv_env,_) ->
    lookupOccRn clas			`thenRn` \ new_clas ->
    rnHsType tv_env ty		`thenRn` \ new_ty ->
    returnRn (SpecInstSig new_clas new_ty src_loc)
\end{pseudocode}
