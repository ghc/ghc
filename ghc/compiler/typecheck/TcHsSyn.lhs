%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcHsSyn]{Specialisations of the @HsSyn@ syntax for the typechecker}

This module is an extension of @HsSyn@ syntax, for use in the type
checker.

\begin{code}
module TcHsSyn (
	TcMonoBinds, TcHsBinds, TcPat,
	TcExpr, TcGRHSs, TcGRHS, TcMatch,
	TcStmt, TcArithSeqInfo, TcRecordBinds,
	TcHsModule, TcDictBinds,
	TcForeignDecl,
	TcCmd, TcCmdTop,
	
	TypecheckedHsBinds, TypecheckedRuleDecl,
	TypecheckedMonoBinds, TypecheckedPat,
	TypecheckedHsExpr, TypecheckedArithSeqInfo,
	TypecheckedStmt, TypecheckedForeignDecl,
	TypecheckedMatch, TypecheckedHsModule,
	TypecheckedGRHSs, TypecheckedGRHS,
	TypecheckedRecordBinds, TypecheckedDictBinds,
	TypecheckedMatchContext, TypecheckedCoreBind,
	TypecheckedHsCmd, TypecheckedHsCmdTop,

	mkHsTyApp, mkHsDictApp, mkHsConApp,
	mkHsTyLam, mkHsDictLam, mkHsLet,
	hsLitType, hsPatType, 

	-- Coercions
	Coercion, ExprCoFn, PatCoFn, 
	(<$>), (<.>), mkCoercion, 
	idCoercion, isIdCoercion,

	-- re-exported from TcMonad
	TcId, TcIdSet,

	zonkTopBinds, zonkTopDecls, zonkTopExpr,
	zonkId, zonkTopBndrs
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it

-- others:
import Id	( idType, setIdType, Id )
import DataCon	( dataConWrapId )	

import TcRnMonad
import Type	  ( Type )
import TcType	  ( TcType, TcTyVar, eqKind, isTypeKind, mkTyVarTy,
		    tcGetTyVar, isAnyTypeKind, mkTyConApp )
import qualified  Type
import TcMType	  ( zonkTcTyVarToTyVar, zonkType, zonkTcType, zonkTcTyVars,
		    putTcTyVar )
import TysPrim	  ( charPrimTy, intPrimTy, floatPrimTy,
		    doublePrimTy, addrPrimTy
		  )
import TysWiredIn ( charTy, stringTy, intTy, integerTy,
		    mkListTy, mkPArrTy, mkTupleTy, unitTy,
		    voidTy, listTyCon, tupleTyCon )
import TyCon	  ( mkPrimTyCon, tyConKind )
import PrimRep	  ( PrimRep(VoidRep) )
import CoreSyn    ( CoreExpr )
import Name	  ( Name, getOccName, mkInternalName, mkDerivedTyConOcc )
import Var	  ( isId, isLocalVar, tyVarKind )
import VarSet
import VarEnv
import BasicTypes ( RecFlag(..), Boxity(..), IPName(..), ipNameName, mapIPName )
import Maybes	  ( orElse )
import Maybe  	  ( isNothing )
import Unique	  ( Uniquable(..) )
import SrcLoc	  ( noSrcLoc )
import Bag
import Outputable
\end{code}


Type definitions
~~~~~~~~~~~~~~~~

The @Tc...@ datatypes are the ones that apply {\em during} type checking.
All the types in @Tc...@ things have mutable type-variables in them for
unification.

At the end of type checking we zonk everything to @Typechecked...@ datatypes,
which have immutable type variables in them.

\begin{code}
type TcHsBinds     	= HsBinds      	TcId
type TcMonoBinds	= MonoBinds    	TcId 
type TcDictBinds	= TcMonoBinds 
type TcPat	     	= OutPat       	TcId
type TcExpr	     	= HsExpr       	TcId 
type TcGRHSs		= GRHSs        	TcId
type TcGRHS		= GRHS         	TcId
type TcMatch		= Match        	TcId
type TcStmt		= Stmt         	TcId
type TcArithSeqInfo	= ArithSeqInfo 	TcId
type TcRecordBinds	= HsRecordBinds TcId
type TcHsModule		= HsModule	TcId
type TcForeignDecl      = ForeignDecl  TcId
type TcRuleDecl 	= RuleDecl     TcId
type TcCmd	     	= HsCmd		TcId 
type TcCmdTop	     	= HsCmdTop     	TcId 

type TypecheckedPat		= OutPat	Id
type TypecheckedMonoBinds 	= MonoBinds	Id
type TypecheckedDictBinds 	= TypecheckedMonoBinds
type TypecheckedHsBinds		= HsBinds	Id
type TypecheckedHsExpr		= HsExpr	Id
type TypecheckedArithSeqInfo	= ArithSeqInfo	Id
type TypecheckedStmt		= Stmt		Id
type TypecheckedMatch		= Match		Id
type TypecheckedGRHSs		= GRHSs		Id
type TypecheckedGRHS		= GRHS		Id
type TypecheckedRecordBinds	= HsRecordBinds Id
type TypecheckedHsModule	= HsModule	Id
type TypecheckedForeignDecl     = ForeignDecl   Id
type TypecheckedRuleDecl	= RuleDecl      Id
type TypecheckedCoreBind        = (Id, CoreExpr)
type TypecheckedHsCmd		= HsCmd		Id
type TypecheckedHsCmdTop	= HsCmdTop	Id

type TypecheckedMatchContext	= HsMatchContext Name	-- Keeps consistency with 
							-- HsDo arg StmtContext
\end{code}

\begin{code}
mkHsTyApp expr []  = expr
mkHsTyApp expr tys = TyApp expr tys

mkHsDictApp expr []	 = expr
mkHsDictApp expr dict_vars = DictApp expr dict_vars

mkHsTyLam []     expr = expr
mkHsTyLam tyvars expr = TyLam tyvars expr

mkHsDictLam []    expr = expr
mkHsDictLam dicts expr = DictLam dicts expr

mkHsLet EmptyMonoBinds expr = expr
mkHsLet mbinds	       expr = HsLet (MonoBind mbinds [] Recursive) expr

mkHsConApp data_con tys args = foldl HsApp (HsVar (dataConWrapId data_con) `mkHsTyApp` tys) args
\end{code}


%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Note: If @hsPatType@ doesn't bear a strong resemblance to @exprType@,
then something is wrong.
\begin{code}
hsPatType :: TypecheckedPat -> Type

hsPatType (ParPat pat)		  = hsPatType pat
hsPatType (WildPat ty)		  = ty
hsPatType (VarPat var)		  = idType var
hsPatType (LazyPat pat)		  = hsPatType pat
hsPatType (LitPat lit)		  = hsLitType lit
hsPatType (AsPat var pat)	  = idType var
hsPatType (ListPat _ ty)	  = mkListTy ty
hsPatType (PArrPat _ ty)	  = mkPArrTy ty
hsPatType (TuplePat pats box)	  = mkTupleTy box (length pats) (map hsPatType pats)
hsPatType (ConPatOut _ _ ty _ _)  = ty
hsPatType (SigPatOut _ ty _)	  = ty
hsPatType (NPatOut lit ty _)	  = ty
hsPatType (NPlusKPatOut id _ _ _) = idType id
hsPatType (DictPat ds ms)         = case (ds ++ ms) of
				       []  -> unitTy
				       [d] -> idType d
				       ds  -> mkTupleTy Boxed (length ds) (map idType ds)


hsLitType :: HsLit -> TcType
hsLitType (HsChar c)       = charTy
hsLitType (HsCharPrim c)   = charPrimTy
hsLitType (HsString str)   = stringTy
hsLitType (HsStringPrim s) = addrPrimTy
hsLitType (HsInt i)	   = intTy
hsLitType (HsIntPrim i)    = intPrimTy
hsLitType (HsInteger i)    = integerTy
hsLitType (HsRat _ ty)	   = ty
hsLitType (HsFloatPrim f)  = floatPrimTy
hsLitType (HsDoublePrim d) = doublePrimTy
\end{code}

%************************************************************************
%*									*
\subsection{Coercion functions}
%*									*
%************************************************************************

\begin{code}
type Coercion a = Maybe (a -> a)
	-- Nothing => identity fn

type ExprCoFn = Coercion TypecheckedHsExpr
type PatCoFn  = Coercion TcPat

(<.>) :: Coercion a -> Coercion a -> Coercion a	-- Composition
Nothing <.> Nothing = Nothing
Nothing <.> Just f  = Just f
Just f  <.> Nothing = Just f
Just f1 <.> Just f2 = Just (f1 . f2)

(<$>) :: Coercion a -> a -> a
Just f  <$> e = f e
Nothing <$> e = e

mkCoercion :: (a -> a) -> Coercion a
mkCoercion f = Just f

idCoercion :: Coercion a
idCoercion = Nothing

isIdCoercion :: Coercion a -> Bool
isIdCoercion = isNothing
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = zonkTcType (idType id) `thenM` \ ty' ->
    returnM (setIdType id ty')
\end{code}

The rest of the zonking is done *after* typechecking.
The main zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcId to an Id by zonking its type

The type variables are converted by binding mutable tyvars to immutable ones
and then zonking as normal.

The Ids are converted by binding them in the normal Tc envt; that
way we maintain sharing; eg an Id is zonked at its binding site and they
all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and 
the environment manipulation is tiresome.

\begin{code}
data ZonkEnv = ZonkEnv	(TcType -> TcM Type) 	-- How to zonk a type
			(IdEnv Id)		-- What variables are in scope
	-- Maps an Id to its zonked version; both have the same Name
	-- Is only consulted lazily; hence knot-tying

emptyZonkEnv = ZonkEnv zonkTypeZapping emptyVarEnv

extendZonkEnv :: ZonkEnv -> [Id] -> ZonkEnv
extendZonkEnv (ZonkEnv zonk_ty env) ids 
  = ZonkEnv zonk_ty (extendVarEnvList env [(id,id) | id <- ids])

setZonkType :: ZonkEnv -> (TcType -> TcM Type) -> ZonkEnv
setZonkType (ZonkEnv _ env) zonk_ty = ZonkEnv zonk_ty env

mkZonkEnv :: [Id] -> ZonkEnv
mkZonkEnv ids = extendZonkEnv emptyZonkEnv ids

zonkIdOcc :: ZonkEnv -> TcId -> Id
-- Ids defined in this module should be in the envt; 
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase 
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error 
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separte chunk.
zonkIdOcc (ZonkEnv zonk_ty env) id 
  | isLocalVar id = lookupVarEnv env id `orElse` id
  | otherwise	  = id

zonkIdOccs env ids = map (zonkIdOcc env) ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give 
zonkIdBndr :: ZonkEnv -> TcId -> TcM Id
zonkIdBndr env id
  = zonkTcTypeToType env (idType id)	`thenM` \ ty' ->
    returnM (setIdType id ty')

zonkIdBndrs :: ZonkEnv -> [TcId] -> TcM [Id]
zonkIdBndrs env ids = mappM (zonkIdBndr env) ids

zonkTopBndrs :: [TcId] -> TcM [Id]
zonkTopBndrs ids = zonkIdBndrs emptyZonkEnv ids
\end{code}


\begin{code}
zonkTopExpr :: TcExpr -> TcM TypecheckedHsExpr
zonkTopExpr e = zonkExpr emptyZonkEnv e

zonkTopDecls :: TcMonoBinds -> [TcRuleDecl] -> [TcForeignDecl]
	     -> TcM ([Id], 
			TypecheckedMonoBinds, 
			[TypecheckedForeignDecl],
			[TypecheckedRuleDecl])
zonkTopDecls binds rules fords	-- Top level is implicitly recursive
  = fixM (\ ~(new_ids, _, _, _) ->
	let
	   zonk_env = mkZonkEnv new_ids
	in
	zonkMonoBinds zonk_env binds		`thenM` \ (binds', new_ids) ->
	zonkRules zonk_env rules		`thenM` \ rules' ->
	zonkForeignExports zonk_env fords	`thenM` \ fords' ->
	
	returnM (bagToList new_ids, binds', fords', rules')
    )

zonkTopBinds :: TcMonoBinds -> TcM ([Id], TypecheckedMonoBinds)
zonkTopBinds binds
  = fixM (\ ~(new_ids, _) ->
	let
	   zonk_env = mkZonkEnv new_ids
	in
	zonkMonoBinds zonk_env binds		`thenM` \ (binds', new_ids) ->
	returnM (bagToList new_ids, binds')
    )

---------------------------------------------
zonkBinds :: ZonkEnv -> TcHsBinds -> TcM (ZonkEnv, TypecheckedHsBinds)
zonkBinds env EmptyBinds = returnM (env, EmptyBinds)

zonkBinds env (ThenBinds b1 b2)
  = zonkBinds env b1 	`thenM` \ (env1, b1') -> 
    zonkBinds env1 b2 	`thenM` \ (env2, b2') -> 
    returnM (env2, b1' `ThenBinds` b2')

zonkBinds env (MonoBind bind sigs is_rec)
  = ASSERT( null sigs )
    fixM (\ ~(_, _, new_ids) ->
	let 
	   env1 = extendZonkEnv env (bagToList new_ids)
	in
	zonkMonoBinds env1 bind	`thenM` \ (new_bind, new_ids) ->
	returnM (env1, new_bind, new_ids)
    )				`thenM` \ (env1, new_bind, _) ->
   returnM (env1, mkMonoBind is_rec new_bind)

zonkBinds env (IPBinds binds)
  = mappM zonk_ip_bind binds	`thenM` \ new_binds ->
    let
	env1 = extendZonkEnv env (map (ipNameName . fst) new_binds)
    in
    returnM (env1, IPBinds new_binds)
  where
    zonk_ip_bind (n, e)
	= mapIPNameTc (zonkIdBndr env) n	`thenM` \ n' ->
	  zonkExpr env e			`thenM` \ e' ->
	  returnM (n', e')


---------------------------------------------
zonkMonoBinds :: ZonkEnv -> TcMonoBinds
	      -> TcM (TypecheckedMonoBinds, Bag Id)

zonkMonoBinds env EmptyMonoBinds = returnM (EmptyMonoBinds, emptyBag)

zonkMonoBinds env (AndMonoBinds mbinds1 mbinds2)
  = zonkMonoBinds env mbinds1		`thenM` \ (b1', ids1) ->
    zonkMonoBinds env mbinds2		`thenM` \ (b2', ids2) ->
    returnM (b1' `AndMonoBinds` b2', 
	     ids1 `unionBags` ids2)

zonkMonoBinds env (PatMonoBind pat grhss locn)
  = zonkPat env pat	`thenM` \ (new_pat, ids) ->
    zonkGRHSs env grhss	`thenM` \ new_grhss ->
    returnM (PatMonoBind new_pat new_grhss locn, ids)

zonkMonoBinds env (VarMonoBind var expr)
  = zonkIdBndr env var 	`thenM` \ new_var ->
    zonkExpr env expr	`thenM` \ new_expr ->
    returnM (VarMonoBind new_var new_expr, unitBag new_var)

zonkMonoBinds env (FunMonoBind var inf ms locn)
  = zonkIdBndr env var			`thenM` \ new_var ->
    mappM (zonkMatch env) ms		`thenM` \ new_ms ->
    returnM (FunMonoBind new_var inf new_ms locn, unitBag new_var)


zonkMonoBinds env (AbsBinds tyvars dicts exports inlines val_bind)
  = mappM zonkTcTyVarToTyVar tyvars	`thenM` \ new_tyvars ->
	-- No need to extend tyvar env: the effects are
	-- propagated through binding the tyvars themselves

    zonkIdBndrs env dicts		`thenM` \ new_dicts ->
    fixM (\ ~(_, _, val_bind_ids) ->
	let
	  env1 = extendZonkEnv (extendZonkEnv env new_dicts)
			       (bagToList val_bind_ids)
	in
	zonkMonoBinds env1 val_bind 		`thenM` \ (new_val_bind, val_bind_ids) ->
        mappM (zonkExport env1) exports	`thenM` \ new_exports ->
	returnM (new_val_bind, new_exports, val_bind_ids)
    )						`thenM ` \ (new_val_bind, new_exports, _) ->
    let
	new_globals = listToBag [global | (_, global, local) <- new_exports]
    in
    returnM (AbsBinds new_tyvars new_dicts new_exports inlines new_val_bind,
		 new_globals)
  where
    zonkExport env (tyvars, global, local)
	= zonkTcTyVars tyvars		`thenM` \ tys ->
	  let
		new_tyvars = map (tcGetTyVar "zonkExport") tys
		-- This isn't the binding occurrence of these tyvars
		-- but they should *be* tyvars.  Hence tcGetTyVar.
	  in
	  zonkIdBndr env global		`thenM` \ new_global ->
	  returnM (new_tyvars, new_global, zonkIdOcc env local)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
%*									*
%************************************************************************

\begin{code}
zonkMatch :: ZonkEnv -> TcMatch -> TcM TypecheckedMatch

zonkMatch env (Match pats _ grhss)
  = zonkPats env pats						`thenM` \ (new_pats, new_ids) ->
    zonkGRHSs (extendZonkEnv env (bagToList new_ids)) grhss 	`thenM` \ new_grhss ->
    returnM (Match new_pats Nothing new_grhss)

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv -> TcGRHSs -> TcM TypecheckedGRHSs

zonkGRHSs env (GRHSs grhss binds ty)
  = zonkBinds env binds   	`thenM` \ (new_env, new_binds) ->
    let
	zonk_grhs (GRHS guarded locn)
	  = zonkStmts new_env guarded  `thenM` \ new_guarded ->
	    returnM (GRHS new_guarded locn)
    in
    mappM zonk_grhs grhss 	`thenM` \ new_grhss ->
    zonkTcTypeToType env ty 	`thenM` \ new_ty ->
    returnM (GRHSs new_grhss new_binds new_ty)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkExprs :: ZonkEnv -> [TcExpr] -> TcM [TypecheckedHsExpr]
zonkExpr  :: ZonkEnv -> TcExpr -> TcM TypecheckedHsExpr

zonkExprs env exprs = mappM (zonkExpr env) exprs


zonkExpr env (HsVar id)
  = returnM (HsVar (zonkIdOcc env id))

zonkExpr env (HsIPVar id)
  = returnM (HsIPVar (mapIPName (zonkIdOcc env) id))

zonkExpr env (HsLit (HsRat f ty))
  = zonkTcTypeToType env ty	   `thenM` \ new_ty  ->
    returnM (HsLit (HsRat f new_ty))

zonkExpr env (HsLit lit)
  = returnM (HsLit lit)

-- HsOverLit doesn't appear in typechecker output

zonkExpr env (HsLam match)
  = zonkMatch env match	`thenM` \ new_match ->
    returnM (HsLam new_match)

zonkExpr env (HsApp e1 e2)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    returnM (HsApp new_e1 new_e2)

zonkExpr env (HsBracketOut body bs) 
  = mappM zonk_b bs	`thenM` \ bs' ->
    returnM (HsBracketOut body bs')
  where
    zonk_b (n,e) = zonkExpr env e	`thenM` \ e' ->
		   returnM (n,e')

zonkExpr env (HsReify r) = returnM (HsReify r)	-- Nothing to zonk; only top
						-- level things can be reified (for now)
zonkExpr env (HsSplice n e loc) = WARN( True, ppr e )	-- Should not happen
			          returnM (HsSplice n e loc)

zonkExpr env (OpApp e1 op fixity e2)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env op	`thenM` \ new_op ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    returnM (OpApp new_e1 new_op fixity new_e2)

zonkExpr env (NegApp _ _) = panic "zonkExpr env: NegApp"

zonkExpr env (HsPar e)    
  = zonkExpr env e	`thenM` \new_e ->
    returnM (HsPar new_e)

zonkExpr env (SectionL expr op)
  = zonkExpr env expr	`thenM` \ new_expr ->
    zonkExpr env op		`thenM` \ new_op ->
    returnM (SectionL new_expr new_op)

zonkExpr env (SectionR op expr)
  = zonkExpr env op		`thenM` \ new_op ->
    zonkExpr env expr		`thenM` \ new_expr ->
    returnM (SectionR new_op new_expr)

zonkExpr env (HsCase expr ms src_loc)
  = zonkExpr env expr    	`thenM` \ new_expr ->
    mappM (zonkMatch env) ms	`thenM` \ new_ms ->
    returnM (HsCase new_expr new_ms src_loc)

zonkExpr env (HsIf e1 e2 e3 src_loc)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    zonkExpr env e3	`thenM` \ new_e3 ->
    returnM (HsIf new_e1 new_e2 new_e3 src_loc)

zonkExpr env (HsLet binds expr)
  = zonkBinds env binds		`thenM` \ (new_env, new_binds) ->
    zonkExpr new_env expr	`thenM` \ new_expr ->
    returnM (HsLet new_binds new_expr)

zonkExpr env (HsDo do_or_lc stmts ids ty src_loc)
  = zonkStmts env stmts 	`thenM` \ new_stmts ->
    zonkTcTypeToType env ty	`thenM` \ new_ty   ->
    zonkReboundNames env ids	`thenM` \ new_ids ->
    returnM (HsDo do_or_lc new_stmts new_ids
		  new_ty src_loc)

zonkExpr env (ExplicitList ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkExprs env exprs		`thenM` \ new_exprs ->
    returnM (ExplicitList new_ty new_exprs)

zonkExpr env (ExplicitPArr ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkExprs env exprs		`thenM` \ new_exprs ->
    returnM (ExplicitPArr new_ty new_exprs)

zonkExpr env (ExplicitTuple exprs boxed)
  = zonkExprs env exprs  	`thenM` \ new_exprs ->
    returnM (ExplicitTuple new_exprs boxed)

zonkExpr env (RecordConOut data_con con_expr rbinds)
  = zonkExpr env con_expr	`thenM` \ new_con_expr ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordConOut data_con new_con_expr new_rbinds)

zonkExpr env (RecordUpd _ _) = panic "zonkExpr env:RecordUpd"

zonkExpr env (RecordUpdOut expr in_ty out_ty rbinds)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType env in_ty	`thenM` \ new_in_ty ->
    zonkTcTypeToType env out_ty	`thenM` \ new_out_ty ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordUpdOut new_expr new_in_ty new_out_ty new_rbinds)

zonkExpr env (ExprWithTySig _ _) = panic "zonkExpr env:ExprWithTySig"
zonkExpr env (ArithSeqIn _)      = panic "zonkExpr env:ArithSeqIn"
zonkExpr env (PArrSeqIn _)       = panic "zonkExpr env:PArrSeqIn"

zonkExpr env (ArithSeqOut expr info)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (ArithSeqOut new_expr new_info)

zonkExpr env (PArrSeqOut expr info)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (PArrSeqOut new_expr new_info)

zonkExpr env (HsSCC lbl expr)
  = zonkExpr env expr	`thenM` \ new_expr ->
    returnM (HsSCC lbl new_expr)

-- hdaume: core annotations
zonkExpr env (HsCoreAnn lbl expr)
  = zonkExpr env expr   `thenM` \ new_expr ->
    returnM (HsCoreAnn lbl new_expr)

zonkExpr env (TyLam tyvars expr)
  = mappM zonkTcTyVarToTyVar tyvars	`thenM` \ new_tyvars ->
	-- No need to extend tyvar env; see AbsBinds

    zonkExpr env expr			`thenM` \ new_expr ->
    returnM (TyLam new_tyvars new_expr)

zonkExpr env (TyApp expr tys)
  = zonkExpr env expr    	 	`thenM` \ new_expr ->
    mappM (zonkTcTypeToType env) tys	`thenM` \ new_tys ->
    returnM (TyApp new_expr new_tys)

zonkExpr env (DictLam dicts expr)
  = zonkIdBndrs env dicts	`thenM` \ new_dicts ->
    let
	env1 = extendZonkEnv env new_dicts
    in
    zonkExpr env1 expr  	`thenM` \ new_expr ->
    returnM (DictLam new_dicts new_expr)

zonkExpr env (DictApp expr dicts)
  = zonkExpr env expr    	    	`thenM` \ new_expr ->
    returnM (DictApp new_expr (zonkIdOccs env dicts))

-- arrow notation extensions
zonkExpr env (HsProc pat body src_loc)
  = zonkPat env pat	    	    	`thenM` \ (new_pat, new_ids) ->
    let
	env1 = extendZonkEnv env (bagToList new_ids)
    in
    zonkCmdTop env1 body    	    	`thenM` \ new_body ->
    returnM (HsProc new_pat new_body src_loc)

zonkExpr env (HsArrApp e1 e2 ty ho rl src_loc)
  = zonkExpr env e1	    	    	`thenM` \ new_e1 ->
    zonkExpr env e2	    	    	`thenM` \ new_e2 ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    returnM (HsArrApp new_e1 new_e2 new_ty ho rl src_loc)

zonkExpr env (HsArrForm op fixity args src_loc)
  = zonkExpr env op	    	    	`thenM` \ new_op ->
    mappM (zonkCmdTop env) args		`thenM` \ new_args ->
    returnM (HsArrForm new_op fixity new_args src_loc)

zonkCmdTop :: ZonkEnv -> TcCmdTop -> TcM TypecheckedHsCmdTop
zonkCmdTop env (HsCmdTop cmd stack_tys ty ids)
  = zonkExpr env cmd	    	    	`thenM` \ new_cmd ->
    mappM (zonkTcTypeToType env) stack_tys
			 		`thenM` \ new_stack_tys ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    zonkReboundNames env ids		`thenM` \ new_ids ->
    returnM (HsCmdTop new_cmd new_stack_tys new_ty new_ids)

-------------------------------------------------------------------------
zonkReboundNames :: ZonkEnv -> ReboundNames Id -> TcM (ReboundNames Id)
zonkReboundNames env prs 
  = mapM zonk prs
  where
    zonk (n, e) = zonkExpr env e `thenM` \ new_e ->
		  returnM (n, new_e)


-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> TcArithSeqInfo -> TcM TypecheckedArithSeqInfo

zonkArithSeq env (From e)
  = zonkExpr env e		`thenM` \ new_e ->
    returnM (From new_e)

zonkArithSeq env (FromThen e1 e2)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    returnM (FromThen new_e1 new_e2)

zonkArithSeq env (FromTo e1 e2)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    returnM (FromTo new_e1 new_e2)

zonkArithSeq env (FromThenTo e1 e2 e3)
  = zonkExpr env e1	`thenM` \ new_e1 ->
    zonkExpr env e2	`thenM` \ new_e2 ->
    zonkExpr env e3	`thenM` \ new_e3 ->
    returnM (FromThenTo new_e1 new_e2 new_e3)


-------------------------------------------------------------------------
zonkStmts  :: ZonkEnv -> [TcStmt] -> TcM [TypecheckedStmt]

zonkStmts env stmts = zonk_stmts env stmts	`thenM` \ (_, stmts) ->
		      returnM stmts

zonk_stmts :: ZonkEnv -> [TcStmt] -> TcM (ZonkEnv, [TypecheckedStmt])

zonk_stmts env [] = returnM (env, [])

zonk_stmts env (ParStmt stmts_w_bndrs : stmts)
  = mappM zonk_branch stmts_w_bndrs	`thenM` \ new_stmts_w_bndrs ->
    let 
	new_binders = concat (map snd new_stmts_w_bndrs)
	env1 = extendZonkEnv env new_binders
    in
    zonk_stmts env1 stmts		`thenM` \ (env2, new_stmts) ->
    returnM (env2, ParStmt new_stmts_w_bndrs : new_stmts)
  where
    zonk_branch (stmts, bndrs) = zonk_stmts env stmts	`thenM` \ (env1, new_stmts) ->
				 returnM (new_stmts, zonkIdOccs env1 bndrs)

zonk_stmts env (RecStmt segStmts lvs rvs rets : stmts)
  = zonkIdBndrs env rvs		`thenM` \ new_rvs ->
    let
	env1 = extendZonkEnv env new_rvs
    in
    zonk_stmts env1 segStmts	`thenM` \ (env2, new_segStmts) ->
	-- Zonk the ret-expressions in an envt that 
	-- has the polymorphic bindings in the envt
    zonkExprs env2 rets		`thenM` \ new_rets ->
    let
	new_lvs = zonkIdOccs env2 lvs
	env3 = extendZonkEnv env new_lvs	-- Only the lvs are needed
    in
    zonk_stmts env3 stmts	`thenM` \ (env4, new_stmts) ->
    returnM (env4, RecStmt new_segStmts new_lvs new_rvs new_rets : new_stmts)

zonk_stmts env (ResultStmt expr locn : stmts)
  = ASSERT( null stmts )
    zonkExpr env expr	`thenM` \ new_expr ->
    returnM (env, [ResultStmt new_expr locn])

zonk_stmts env (ExprStmt expr ty locn : stmts)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonk_stmts env stmts	`thenM` \ (env1, new_stmts) ->
    returnM (env1, ExprStmt new_expr new_ty locn : new_stmts)

zonk_stmts env (LetStmt binds : stmts)
  = zonkBinds env binds		`thenM` \ (env1, new_binds) ->
    zonk_stmts env1 stmts	`thenM` \ (env2, new_stmts) ->
    returnM (env2, LetStmt new_binds : new_stmts)

zonk_stmts env (BindStmt pat expr locn : stmts)
  = zonkExpr env expr			`thenM` \ new_expr ->
    zonkPat env pat			`thenM` \ (new_pat, new_ids) ->
    let
	env1 = extendZonkEnv env (bagToList new_ids)
    in
    zonk_stmts env1 stmts		`thenM` \ (env2, new_stmts) ->
    returnM (env2, BindStmt new_pat new_expr locn : new_stmts)



-------------------------------------------------------------------------
zonkRbinds :: ZonkEnv -> TcRecordBinds -> TcM TypecheckedRecordBinds

zonkRbinds env rbinds
  = mappM zonk_rbind rbinds
  where
    zonk_rbind (field, expr)
      = zonkExpr env expr	`thenM` \ new_expr ->
	returnM (zonkIdOcc env field, new_expr)

-------------------------------------------------------------------------
mapIPNameTc :: (a -> TcM b) -> IPName a -> TcM (IPName b)
mapIPNameTc f (Dupable n) = f n  `thenM` \ r -> returnM (Dupable r)
mapIPNameTc f (Linear  n) = f n  `thenM` \ r -> returnM (Linear r)
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: ZonkEnv -> TcPat -> TcM (TypecheckedPat, Bag Id)

zonkPat env (ParPat p)
  = zonkPat env p	`thenM` \ (new_p, ids) ->
    returnM (ParPat new_p, ids)

zonkPat env (WildPat ty)
  = zonkTcTypeToType env ty   `thenM` \ new_ty ->
    returnM (WildPat new_ty, emptyBag)

zonkPat env (VarPat v)
  = zonkIdBndr env v	    `thenM` \ new_v ->
    returnM (VarPat new_v, unitBag new_v)

zonkPat env (LazyPat pat)
  = zonkPat env pat	    `thenM` \ (new_pat, ids) ->
    returnM (LazyPat new_pat, ids)

zonkPat env (AsPat n pat)
  = zonkIdBndr env n	    `thenM` \ new_n ->
    zonkPat env pat	    `thenM` \ (new_pat, ids) ->
    returnM (AsPat new_n new_pat, new_n `consBag` ids)

zonkPat env (ListPat pats ty)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkPats env pats		`thenM` \ (new_pats, ids) ->
    returnM (ListPat new_pats new_ty, ids)

zonkPat env (PArrPat pats ty)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkPats env pats		`thenM` \ (new_pats, ids) ->
    returnM (PArrPat new_pats new_ty, ids)

zonkPat env (TuplePat pats boxed)
  = zonkPats env pats   		`thenM` \ (new_pats, ids) ->
    returnM (TuplePat new_pats boxed, ids)

zonkPat env (ConPatOut n stuff ty tvs dicts)
  = zonkTcTypeToType env ty		`thenM` \ new_ty ->
    mappM zonkTcTyVarToTyVar tvs	`thenM` \ new_tvs ->
    zonkIdBndrs env dicts		`thenM` \ new_dicts ->
    let
	env1 = extendZonkEnv env new_dicts
    in
    zonkConStuff env stuff		`thenM` \ (new_stuff, ids) ->
    returnM (ConPatOut n new_stuff new_ty new_tvs new_dicts, 
		 listToBag new_dicts `unionBags` ids)

zonkPat env (LitPat lit) = returnM (LitPat lit, emptyBag)

zonkPat env (SigPatOut pat ty expr)
  = zonkPat env pat		`thenM` \ (new_pat, ids) ->
    zonkTcTypeToType env ty	`thenM` \ new_ty  ->
    zonkExpr env expr		`thenM` \ new_expr ->
    returnM (SigPatOut new_pat new_ty new_expr, ids)

zonkPat env (NPatOut lit ty expr)
  = zonkTcTypeToType env ty	`thenM` \ new_ty   ->
    zonkExpr env expr		`thenM` \ new_expr ->
    returnM (NPatOut lit new_ty new_expr, emptyBag)

zonkPat env (NPlusKPatOut n k e1 e2)
  = zonkIdBndr env n	        `thenM` \ new_n ->
    zonkExpr env e1		        `thenM` \ new_e1 ->
    zonkExpr env e2		        `thenM` \ new_e2 ->
    returnM (NPlusKPatOut new_n k new_e1 new_e2, unitBag new_n)

zonkPat env (DictPat ds ms)
  = zonkIdBndrs env ds      `thenM` \ new_ds ->
    zonkIdBndrs env ms     `thenM` \ new_ms ->
    returnM (DictPat new_ds new_ms,
		 listToBag new_ds `unionBags` listToBag new_ms)

---------------------------
zonkConStuff env (PrefixCon pats)
  = zonkPats env pats		`thenM` \ (new_pats, ids) ->
    returnM (PrefixCon new_pats, ids)

zonkConStuff env (InfixCon p1 p2)
  = zonkPat env p1		`thenM` \ (new_p1, ids1) ->
    zonkPat env p2		`thenM` \ (new_p2, ids2) ->
    returnM (InfixCon new_p1 new_p2, ids1 `unionBags` ids2)

zonkConStuff env (RecCon rpats)
  = mapAndUnzipM zonk_rpat rpats	`thenM` \ (new_rpats, ids_s) ->
    returnM (RecCon new_rpats, unionManyBags ids_s)
  where
    zonk_rpat (f, pat)
      = zonkPat env pat		`thenM` \ (new_pat, ids) ->
	returnM ((f, new_pat), ids)

---------------------------
zonkPats env []
  = returnM ([], emptyBag)

zonkPats env (pat:pats) 
  = zonkPat env pat	`thenM` \ (pat',  ids1) ->
    zonkPats env pats	`thenM` \ (pats', ids2) ->
    returnM (pat':pats', ids1 `unionBags` ids2)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************


\begin{code}
zonkForeignExports :: ZonkEnv -> [TcForeignDecl] -> TcM [TypecheckedForeignDecl]
zonkForeignExports env ls = mappM (zonkForeignExport env) ls

zonkForeignExport :: ZonkEnv -> TcForeignDecl -> TcM (TypecheckedForeignDecl)
zonkForeignExport env (ForeignExport i hs_ty spec isDeprec src_loc) =
   returnM (ForeignExport (zonkIdOcc env i) undefined spec isDeprec src_loc)
zonkForeignExport env for_imp 
  = returnM for_imp	-- Foreign imports don't need zonking
\end{code}

\begin{code}
zonkRules :: ZonkEnv -> [TcRuleDecl] -> TcM [TypecheckedRuleDecl]
zonkRules env rs = mappM (zonkRule env) rs

zonkRule env (HsRule name act vars lhs rhs loc)
  = mappM zonk_bndr vars		`thenM` \ new_bndrs ->
    newMutVar emptyVarSet		`thenM` \ unbound_tv_set ->
    let
	env_rhs = extendZonkEnv env (filter isId new_bndrs)
	-- Type variables don't need an envt
	-- They are bound through the mutable mechanism

	env_lhs = setZonkType env_rhs (zonkTypeCollecting unbound_tv_set)
	-- We need to gather the type variables mentioned on the LHS so we can 
	-- quantify over them.  Example:
	--   data T a = C
	-- 
	--   foo :: T a -> Int
	--   foo C = 1
	--
	--   {-# RULES "myrule"  foo C = 1 #-}
	-- 
	-- After type checking the LHS becomes (foo a (C a))
	-- and we do not want to zap the unbound tyvar 'a' to (), because
	-- that limits the applicability of the rule.  Instead, we
	-- want to quantify over it!  
	--
	-- It's easiest to find the free tyvars here. Attempts to do so earlier
	-- are tiresome, because (a) the data type is big and (b) finding the 
	-- free type vars of an expression is necessarily monadic operation.
	--	(consider /\a -> f @ b, where b is side-effected to a)
    in
    zonkExpr env_lhs lhs		`thenM` \ new_lhs ->
    zonkExpr env_rhs rhs		`thenM` \ new_rhs ->

    readMutVar unbound_tv_set		`thenM` \ unbound_tvs ->
    let
	final_bndrs = map RuleBndr (varSetElems unbound_tvs ++ new_bndrs)
	-- I hate this map RuleBndr stuff
    in
    returnM (HsRule name act final_bndrs new_lhs new_rhs loc)
  where
   zonk_bndr (RuleBndr v) 
	| isId v    = zonkIdBndr env v
	| otherwise = zonkTcTyVarToTyVar v

zonkRule env (IfaceRuleOut fun rule)
  = returnM (IfaceRuleOut (zonkIdOcc env fun) rule)
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************

\begin{code}
zonkTcTypeToType :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToType (ZonkEnv zonk_ty _) ty = zonk_ty ty

zonkTypeCollecting :: TcRef TyVarSet -> TcType -> TcM Type
-- This variant collects unbound type variables in a mutable variable
zonkTypeCollecting unbound_tv_set
  = zonkType zonk_unbound_tyvar
  where
    zonk_unbound_tyvar tv 
	= zonkTcTyVarToTyVar tv					`thenM` \ tv' ->
	  readMutVar unbound_tv_set				`thenM` \ tv_set ->
	  writeMutVar unbound_tv_set (extendVarSet tv_set tv')	`thenM_`
	  return (mkTyVarTy tv')

zonkTypeZapping :: TcType -> TcM Type
-- This variant is used for everything except the LHS of rules
-- It zaps unbound type variables to (), or some other arbitrary type
zonkTypeZapping ty 
  = zonkType zonk_unbound_tyvar ty
  where
	-- Zonk a mutable but unbound type variable to an arbitrary type
	-- We know it's unbound even though we don't carry an environment,
	-- because at the binding site for a type variable we bind the
	-- mutable tyvar to a fresh immutable one.  So the mutable store
	-- plays the role of an environment.  If we come across a mutable
	-- type variable that isn't so bound, it must be completely free.
    zonk_unbound_tyvar tv = putTcTyVar tv (mkArbitraryType tv)


-- When the type checker finds a type variable with no binding,
-- which means it can be instantiated with an arbitrary type, it
-- usually instantiates it to Void.  Eg.
-- 
-- 	length []
-- ===>
-- 	length Void (Nil Void)
-- 
-- But in really obscure programs, the type variable might have
-- a kind other than *, so we need to invent a suitably-kinded type.
-- 
-- This commit uses
-- 	Void for kind *
-- 	List for kind *->*
-- 	Tuple for kind *->...*->*
-- 
-- which deals with most cases.  (Previously, it only dealt with
-- kind *.)   
-- 
-- In the other cases, it just makes up a TyCon with a suitable
-- kind.  If this gets into an interface file, anyone reading that
-- file won't understand it.  This is fixable (by making the client
-- of the interface file make up a TyCon too) but it is tiresome and
-- never happens, so I am leaving it 

mkArbitraryType :: TcTyVar -> Type
-- Make up an arbitrary type whose kind is the same as the tyvar.
-- We'll use this to instantiate the (unbound) tyvar.
mkArbitraryType tv 
  | isAnyTypeKind kind = voidTy		-- The vastly common case
  | otherwise	       = mkTyConApp tycon []
  where
    kind       = tyVarKind tv
    (args,res) = Type.splitFunTys kind	-- Kinds are simple; use Type.splitFunTys

    tycon | kind `eqKind` tyConKind listTyCon 	-- *->*
	  = listTyCon				-- No tuples this size

	  | all isTypeKind args && isTypeKind res
	  = tupleTyCon Boxed (length args)	-- *-> ... ->*->*

	  | otherwise
	  = pprTrace "Urk! Inventing strangely-kinded void TyCon:" (ppr tc_name $$ ppr kind) $
	    mkPrimTyCon tc_name kind 0 [] VoidRep
		-- Same name as the tyvar, apart from making it start with a colon (sigh)
		-- I dread to think what will happen if this gets out into an 
		-- interface file.  Catastrophe likely.  Major sigh.

    tc_name = mkInternalName (getUnique tv) (mkDerivedTyConOcc (getOccName tv)) noSrcLoc
\end{code}
