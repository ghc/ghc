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
	
	TypecheckedHsBinds, TypecheckedRuleDecl,
	TypecheckedMonoBinds, TypecheckedPat,
	TypecheckedHsExpr, TypecheckedArithSeqInfo,
	TypecheckedStmt, TypecheckedForeignDecl,
	TypecheckedMatch, TypecheckedHsModule,
	TypecheckedGRHSs, TypecheckedGRHS,
	TypecheckedRecordBinds, TypecheckedDictBinds,
	TypecheckedMatchContext, TypecheckedCoreBind,

	mkHsTyApp, mkHsDictApp, mkHsConApp,
	mkHsTyLam, mkHsDictLam, mkHsLet,
	hsLitType, hsPatType, 

	-- re-exported from TcMonad
	TcId, TcIdSet,

	zonkTopBinds, zonkTopDecls, zonkTopExpr,
	zonkId, zonkIdBndr
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it

-- others:
import Id	( idType, setIdType, Id )
import DataCon	( dataConWrapId )	

import TcRnMonad
import Type	  ( Type )
import TcType	  ( TcType, tcGetTyVar )
import TcMType	  ( zonkTcTypeToType, zonkTcTyVarToTyVar, zonkTcType, zonkTcTyVars )
import TysPrim	  ( charPrimTy, intPrimTy, floatPrimTy,
		    doublePrimTy, addrPrimTy
		  )
import TysWiredIn ( charTy, stringTy, intTy, integerTy,
		    mkListTy, mkPArrTy, mkTupleTy, unitTy )
import CoreSyn    ( CoreExpr )
import Var	  ( isId, isLocalVar )
import VarEnv
import BasicTypes ( RecFlag(..), Boxity(..), IPName(..), ipNameName, mapIPName )
import Maybes	  ( orElse )
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

type TypecheckedPat		= OutPat	Id
type TypecheckedMonoBinds 	= MonoBinds	Id
type TypecheckedDictBinds 	= TypecheckedMonoBinds
type TypecheckedHsBinds		= HsBinds	Id
type TypecheckedHsExpr		= HsExpr	Id
type TypecheckedArithSeqInfo	= ArithSeqInfo	Id
type TypecheckedStmt		= Stmt		Id
type TypecheckedMatch		= Match		Id
type TypecheckedMatchContext	= HsMatchContext Id
type TypecheckedGRHSs		= GRHSs		Id
type TypecheckedGRHS		= GRHS		Id
type TypecheckedRecordBinds	= HsRecordBinds Id
type TypecheckedHsModule	= HsModule	Id
type TypecheckedForeignDecl     = ForeignDecl   Id
type TypecheckedRuleDecl	= RuleDecl      Id
type TypecheckedCoreBind        = (Id, CoreExpr)
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
hsLitType (HsLitLit _ ty)  = ty
\end{code}

\begin{code}
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> TcM TcId
zonkId id
  = zonkTcType (idType id) `thenM` \ ty' ->
    returnM (setIdType id ty')
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

This zonking pass runs over the bindings

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
type ZonkEnv = IdEnv Id
	-- Maps an Id to its zonked version; both have the same Name
	-- Is only consulted lazily; hence knot-tying

emptyZonkEnv = emptyVarEnv

extendZonkEnv :: ZonkEnv -> [Id] -> ZonkEnv
extendZonkEnv env ids = extendVarEnvList env [(id,id) | id <- ids]

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
zonkIdOcc env id 
  | isLocalVar id = lookupVarEnv env id `orElse` id
  | otherwise	  = id

zonkIdOccs env ids = map (zonkIdOcc env) ids

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give 
zonkIdBndr :: TcId -> TcM Id
zonkIdBndr id
  = zonkTcTypeToType (idType id)	`thenM` \ ty' ->
    returnM (setIdType id ty')
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
    fixM (\ ~(env1, _) ->
	zonkMonoBinds env1 bind		`thenM` \ (new_bind, new_ids) ->
	let 
	   env2 = extendZonkEnv env (bagToList new_ids)
	in
	returnM (env2, mkMonoBind new_bind [] is_rec)
    )

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
  = zonkIdBndr var    	`thenM` \ new_var ->
    zonkExpr env expr	`thenM` \ new_expr ->
    returnM (VarMonoBind new_var new_expr, unitBag new_var)

zonkMonoBinds env (CoreMonoBind var core_expr)
  = zonkIdBndr var    	`thenM` \ new_var ->
    returnM (CoreMonoBind new_var core_expr, unitBag new_var)

zonkMonoBinds env (FunMonoBind var inf ms locn)
  = zonkIdBndr var			`thenM` \ new_var ->
    mappM (zonkMatch env) ms		`thenM` \ new_ms ->
    returnM (FunMonoBind new_var inf new_ms locn, unitBag new_var)


zonkMonoBinds env (AbsBinds tyvars dicts exports inlines val_bind)
  = mappM zonkTcTyVarToTyVar tyvars	`thenM` \ new_tyvars ->
	-- No need to extend tyvar env: the effects are
	-- propagated through binding the tyvars themselves

    mappM zonkIdBndr  dicts		`thenM` \ new_dicts ->
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
	  zonkIdBndr global		`thenM` \ new_global ->
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
    zonkTcTypeToType ty 	`thenM` \ new_ty ->
    returnM (GRHSs new_grhss new_binds new_ty)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkExpr :: ZonkEnv -> TcExpr -> TcM TypecheckedHsExpr

zonkExpr env (HsVar id)
  = returnM (HsVar (zonkIdOcc env id))

zonkExpr env (HsIPVar id)
  = returnM (HsIPVar (mapIPName (zonkIdOcc env) id))

zonkExpr env (HsLit (HsRat f ty))
  = zonkTcTypeToType ty	    `thenM` \ new_ty  ->
    returnM (HsLit (HsRat f new_ty))

zonkExpr env (HsLit (HsLitLit lit ty))
  = zonkTcTypeToType ty	    `thenM` \ new_ty  ->
    returnM (HsLit (HsLitLit lit new_ty))

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

zonkExpr env (HsSplice n e) = WARN( True, ppr e )	-- Should not happen
			      returnM (HsSplice n e)

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

zonkExpr env (HsWith expr binds is_with)
  = mappM zonk_ip_bind binds	`thenM` \ new_binds ->
    let
	env1 = extendZonkEnv env (map (ipNameName . fst) new_binds)
    in
    zonkExpr env1 expr		`thenM` \ new_expr ->
    returnM (HsWith new_expr new_binds is_with)
    where
	zonk_ip_bind (n, e)
	    = mapIPNameTc zonkIdBndr n	`thenM` \ n' ->
	      zonkExpr env e		`thenM` \ e' ->
	      returnM (n', e')

zonkExpr env (HsDo do_or_lc stmts ids ty src_loc)
  = zonkStmts env stmts 	`thenM` \ new_stmts ->
    zonkTcTypeToType ty		`thenM` \ new_ty   ->
    returnM (HsDo do_or_lc new_stmts 
		      (zonkIdOccs env ids) 
		      new_ty src_loc)

zonkExpr env (ExplicitList ty exprs)
  = zonkTcTypeToType ty			`thenM` \ new_ty ->
    mappM (zonkExpr env) exprs	`thenM` \ new_exprs ->
    returnM (ExplicitList new_ty new_exprs)

zonkExpr env (ExplicitPArr ty exprs)
  = zonkTcTypeToType ty			`thenM` \ new_ty ->
    mappM (zonkExpr env) exprs	`thenM` \ new_exprs ->
    returnM (ExplicitPArr new_ty new_exprs)

zonkExpr env (ExplicitTuple exprs boxed)
  = mappM (zonkExpr env) exprs  	`thenM` \ new_exprs ->
    returnM (ExplicitTuple new_exprs boxed)

zonkExpr env (RecordConOut data_con con_expr rbinds)
  = zonkExpr env con_expr	`thenM` \ new_con_expr ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordConOut data_con new_con_expr new_rbinds)

zonkExpr env (RecordUpd _ _) = panic "zonkExpr env:RecordUpd"

zonkExpr env (RecordUpdOut expr in_ty out_ty rbinds)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType in_ty	`thenM` \ new_in_ty ->
    zonkTcTypeToType out_ty	`thenM` \ new_out_ty ->
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

zonkExpr env (HsCCall fun args may_gc is_casm result_ty)
  = mappM (zonkExpr env) args	`thenM` \ new_args ->
    zonkTcTypeToType result_ty		`thenM` \ new_result_ty ->
    returnM (HsCCall fun new_args may_gc is_casm new_result_ty)

zonkExpr env (HsSCC lbl expr)
  = zonkExpr env expr	`thenM` \ new_expr ->
    returnM (HsSCC lbl new_expr)

zonkExpr env (TyLam tyvars expr)
  = mappM zonkTcTyVarToTyVar tyvars	`thenM` \ new_tyvars ->
	-- No need to extend tyvar env; see AbsBinds

    zonkExpr env expr			`thenM` \ new_expr ->
    returnM (TyLam new_tyvars new_expr)

zonkExpr env (TyApp expr tys)
  = zonkExpr env expr    	    		`thenM` \ new_expr ->
    mappM zonkTcTypeToType tys	`thenM` \ new_tys ->
    returnM (TyApp new_expr new_tys)

zonkExpr env (DictLam dicts expr)
  = mappM zonkIdBndr dicts		`thenM` \ new_dicts ->
    let
	env1 = extendZonkEnv env new_dicts
    in
    zonkExpr env1 expr    	    		`thenM` \ new_expr ->
    returnM (DictLam new_dicts new_expr)

zonkExpr env (DictApp expr dicts)
  = zonkExpr env expr    	    	`thenM` \ new_expr ->
    returnM (DictApp new_expr (zonkIdOccs env dicts))



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
zonkStmts :: ZonkEnv -> [TcStmt] -> TcM [TypecheckedStmt]

zonkStmts env [] = returnM []

zonkStmts env (ParStmtOut bndrstmtss : stmts)
  = mappM (mappM zonkId) bndrss	`thenM` \ new_bndrss ->
    mappM (zonkStmts env) stmtss	`thenM` \ new_stmtss ->
    let 
	new_binders = concat new_bndrss
	env1 = extendZonkEnv env new_binders
    in
    zonkStmts env1 stmts		`thenM` \ new_stmts ->
    returnM (ParStmtOut (zip new_bndrss new_stmtss) : new_stmts)
  where
    (bndrss, stmtss) = unzip bndrstmtss

zonkStmts env (ResultStmt expr locn : stmts)
  = zonkExpr env expr	`thenM` \ new_expr ->
    zonkStmts env stmts	`thenM` \ new_stmts ->
    returnM (ResultStmt new_expr locn : new_stmts)

zonkStmts env (ExprStmt expr ty locn : stmts)
  = zonkExpr env expr	`thenM` \ new_expr ->
    zonkTcTypeToType ty	`thenM` \ new_ty ->
    zonkStmts env stmts	`thenM` \ new_stmts ->
    returnM (ExprStmt new_expr new_ty locn : new_stmts)

zonkStmts env (LetStmt binds : stmts)
  = zonkBinds env binds		`thenM` \ (new_env, new_binds) ->
    zonkStmts new_env stmts	`thenM` \ new_stmts ->
    returnM (LetStmt new_binds : new_stmts)

zonkStmts env (BindStmt pat expr locn : stmts)
  = zonkExpr env expr			`thenM` \ new_expr ->
    zonkPat env pat			`thenM` \ (new_pat, new_ids) ->
    let
	env1 = extendZonkEnv env (bagToList new_ids)
    in
    zonkStmts env1 stmts		`thenM` \ new_stmts ->
    returnM (BindStmt new_pat new_expr locn : new_stmts)



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
  = zonkTcTypeToType ty	    `thenM` \ new_ty ->
    returnM (WildPat new_ty, emptyBag)

zonkPat env (VarPat v)
  = zonkIdBndr v	    `thenM` \ new_v ->
    returnM (VarPat new_v, unitBag new_v)

zonkPat env (LazyPat pat)
  = zonkPat env pat	    `thenM` \ (new_pat, ids) ->
    returnM (LazyPat new_pat, ids)

zonkPat env (AsPat n pat)
  = zonkIdBndr n	    `thenM` \ new_n ->
    zonkPat env pat	    `thenM` \ (new_pat, ids) ->
    returnM (AsPat new_n new_pat, new_n `consBag` ids)

zonkPat env (ListPat pats ty)
  = zonkTcTypeToType ty	`thenM` \ new_ty ->
    zonkPats env pats		`thenM` \ (new_pats, ids) ->
    returnM (ListPat new_pats new_ty, ids)

zonkPat env (PArrPat pats ty)
  = zonkTcTypeToType ty	`thenM` \ new_ty ->
    zonkPats env pats		`thenM` \ (new_pats, ids) ->
    returnM (PArrPat new_pats new_ty, ids)

zonkPat env (TuplePat pats boxed)
  = zonkPats env pats   		`thenM` \ (new_pats, ids) ->
    returnM (TuplePat new_pats boxed, ids)

zonkPat env (ConPatOut n stuff ty tvs dicts)
  = zonkTcTypeToType ty			`thenM` \ new_ty ->
    mappM zonkTcTyVarToTyVar tvs	`thenM` \ new_tvs ->
    mappM zonkIdBndr dicts		`thenM` \ new_dicts ->
    let
	env1 = extendZonkEnv env new_dicts
    in
    zonkConStuff env stuff		`thenM` \ (new_stuff, ids) ->
    returnM (ConPatOut n new_stuff new_ty new_tvs new_dicts, 
		 listToBag new_dicts `unionBags` ids)

zonkPat env (LitPat lit) = returnM (LitPat lit, emptyBag)

zonkPat env (SigPatOut pat ty expr)
  = zonkPat env pat			`thenM` \ (new_pat, ids) ->
    zonkTcTypeToType ty		`thenM` \ new_ty  ->
    zonkExpr env expr		`thenM` \ new_expr ->
    returnM (SigPatOut new_pat new_ty new_expr, ids)

zonkPat env (NPatOut lit ty expr)
  = zonkTcTypeToType ty		`thenM` \ new_ty   ->
    zonkExpr env expr		`thenM` \ new_expr ->
    returnM (NPatOut lit new_ty new_expr, emptyBag)

zonkPat env (NPlusKPatOut n k e1 e2)
  = zonkIdBndr n	        `thenM` \ new_n ->
    zonkExpr env e1		        `thenM` \ new_e1 ->
    zonkExpr env e2		        `thenM` \ new_e2 ->
    returnM (NPlusKPatOut new_n k new_e1 new_e2, unitBag new_n)

zonkPat env (DictPat ds ms)
  = mappM zonkIdBndr ds      `thenM` \ new_ds ->
    mappM zonkIdBndr ms      `thenM` \ new_ms ->
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
\end{code}

\begin{code}
zonkRules :: ZonkEnv -> [TcRuleDecl] -> TcM [TypecheckedRuleDecl]
zonkRules env rs = mappM (zonkRule env) rs

zonkRule env (HsRule name act vars lhs rhs loc)
  = mappM zonk_bndr vars				`thenM` \ new_bndrs ->
    let
	env1 = extendZonkEnv env (filter isId new_bndrs)
	-- Type variables don't need an envt
	-- They are bound through the mutable mechanism
    in
    zonkExpr env1 lhs					`thenM` \ new_lhs ->
    zonkExpr env1 rhs					`thenM` \ new_rhs ->
    returnM (HsRule name act (map RuleBndr new_bndrs) new_lhs new_rhs loc)
	-- I hate this map RuleBndr stuff
  where
   zonk_bndr (RuleBndr v) 
	| isId v    = zonkIdBndr v
	| otherwise = zonkTcTyVarToTyVar v

zonkRule env (IfaceRuleOut fun rule)
  = returnM (IfaceRuleOut (zonkIdOcc env fun) rule)
\end{code}

