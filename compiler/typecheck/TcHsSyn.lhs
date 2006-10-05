	%
% (c) The AQUA Project, Glasgow University, 1996-1998
%
\section[TcHsSyn]{Specialisations of the @HsSyn@ syntax for the typechecker}

This module is an extension of @HsSyn@ syntax, for use in the type
checker.

\begin{code}
module TcHsSyn (
	mkHsConApp, mkHsDictLet, mkHsApp,
	hsLitType, hsLPatType, hsPatType, 
	mkHsAppTy, mkSimpleHsAlt,
	nlHsIntLit, mkVanillaTuplePat,
	

	-- re-exported from TcMonad
	TcId, TcIdSet, TcDictBinds,

	zonkTopDecls, zonkTopExpr, zonkTopLExpr,
	zonkId, zonkTopBndrs
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it

-- others:
import Id	( idType, setIdType, Id )

import TcRnMonad
import Type	  ( Type, isLiftedTypeKind, liftedTypeKind, isSubKind, eqKind  )
import TcType	  ( TcType, TcTyVar, mkTyVarTy, mkTyConApp, isImmutableTyVar )
import qualified  Type
import TcMType	  ( zonkQuantifiedTyVar, zonkType, zonkTcType, writeMetaTyVar )
import TysPrim	  ( charPrimTy, intPrimTy, floatPrimTy,
		    doublePrimTy, addrPrimTy
		  )
import TysWiredIn ( charTy, stringTy, intTy, 
		    mkListTy, mkPArrTy, mkTupleTy, unitTy,
		    voidTy, listTyCon, tupleTyCon )
import TyCon	  ( mkPrimTyCon, tyConKind, PrimRep(..) )
import {- Kind parts of -} Type	  ( splitKindFunTys )
import Name	  ( Name, getOccName, mkInternalName, mkDerivedTyConOcc )
import Var	  ( Var, isId, isLocalVar, tyVarKind )
import VarSet
import VarEnv
import BasicTypes ( Boxity(..), IPName(..), ipNameName, mapIPName )
import Maybes	  ( orElse )
import Unique	  ( Uniquable(..) )
import SrcLoc	  ( noSrcLoc, noLoc, Located(..), unLoc )
import Util	  ( mapSnd )
import Bag
import Outputable
\end{code}


%************************************************************************
%*									*
\subsection[mkFailurePair]{Code for pattern-matching and other failures}
%*									*
%************************************************************************

Note: If @hsLPatType@ doesn't bear a strong resemblance to @exprType@,
then something is wrong.
\begin{code}
mkVanillaTuplePat :: [OutPat Id] -> Boxity -> Pat Id
-- A vanilla tuple pattern simply gets its type from its sub-patterns
mkVanillaTuplePat pats box 
  = TuplePat pats box (mkTupleTy box (length pats) (map hsLPatType pats))

hsLPatType :: OutPat Id -> Type
hsLPatType (L _ pat) = hsPatType pat

hsPatType (ParPat pat)		    = hsLPatType pat
hsPatType (WildPat ty)		    = ty
hsPatType (VarPat var)		    = idType var
hsPatType (VarPatOut var _)	    = idType var
hsPatType (BangPat pat)		    = hsLPatType pat
hsPatType (LazyPat pat)		    = hsLPatType pat
hsPatType (LitPat lit)		    = hsLitType lit
hsPatType (AsPat var pat)	    = idType (unLoc var)
hsPatType (ListPat _ ty)	    = mkListTy ty
hsPatType (PArrPat _ ty)	    = mkPArrTy ty
hsPatType (TuplePat pats box ty)    = ty
hsPatType (ConPatOut{ pat_ty = ty })= ty
hsPatType (SigPatOut pat ty)	    = ty
hsPatType (NPat lit _ _ ty)	    = ty
hsPatType (NPlusKPat id _ _ _)      = idType (unLoc id)
hsPatType (CoPat _ _ ty)	    = ty
hsPatType (DictPat ds ms)           = case (ds ++ ms) of
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
hsLitType (HsInteger i ty) = ty
hsLitType (HsRat _ ty)	   = ty
hsLitType (HsFloatPrim f)  = floatPrimTy
hsLitType (HsDoublePrim d) = doublePrimTy
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

extendZonkEnv1 :: ZonkEnv -> Id -> ZonkEnv
extendZonkEnv1 (ZonkEnv zonk_ty env) id 
  = ZonkEnv zonk_ty (extendVarEnv env id id)

setZonkType :: ZonkEnv -> (TcType -> TcM Type) -> ZonkEnv
setZonkType (ZonkEnv _ env) zonk_ty = ZonkEnv zonk_ty env

zonkEnvIds :: ZonkEnv -> [Id]
zonkEnvIds (ZonkEnv _ env) = varEnvElts env

zonkIdOcc :: ZonkEnv -> TcId -> Id
-- Ids defined in this module should be in the envt; 
-- ignore others.  (Actually, data constructors are also
-- not LocalVars, even when locally defined, but that is fine.)
-- (Also foreign-imported things aren't currently in the ZonkEnv;
--  that's ok because they don't need zonking.)
--
-- Actually, Template Haskell works in 'chunks' of declarations, and
-- an earlier chunk won't be in the 'env' that the zonking phase 
-- carries around.  Instead it'll be in the tcg_gbl_env, already fully
-- zonked.  There's no point in looking it up there (except for error 
-- checking), and it's not conveniently to hand; hence the simple
-- 'orElse' case in the LocalVar branch.
--
-- Even without template splices, in module Main, the checking of
-- 'main' is done as a separate chunk.
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
zonkTopExpr :: HsExpr TcId -> TcM (HsExpr Id)
zonkTopExpr e = zonkExpr emptyZonkEnv e

zonkTopLExpr :: LHsExpr TcId -> TcM (LHsExpr Id)
zonkTopLExpr e = zonkLExpr emptyZonkEnv e

zonkTopDecls :: LHsBinds TcId -> [LRuleDecl TcId] -> [LForeignDecl TcId]
	     -> TcM ([Id], 
		     Bag (LHsBind  Id),
		     [LForeignDecl Id],
		     [LRuleDecl    Id])
zonkTopDecls binds rules fords
  = do	{ (env, binds') <- zonkRecMonoBinds emptyZonkEnv binds
			-- Top level is implicitly recursive
	; rules' <- zonkRules env rules
	; fords' <- zonkForeignExports env fords
	; return (zonkEnvIds env, binds', fords', rules') }

---------------------------------------------
zonkLocalBinds :: ZonkEnv -> HsLocalBinds TcId -> TcM (ZonkEnv, HsLocalBinds Id)
zonkLocalBinds env EmptyLocalBinds
  = return (env, EmptyLocalBinds)

zonkLocalBinds env (HsValBinds binds)
  = do	{ (env1, new_binds) <- zonkValBinds env binds
	; return (env1, HsValBinds new_binds) }

zonkLocalBinds env (HsIPBinds (IPBinds binds dict_binds))
  = mappM (wrapLocM zonk_ip_bind) binds	`thenM` \ new_binds ->
    let
	env1 = extendZonkEnv env [ipNameName n | L _ (IPBind n _) <- new_binds]
    in
    zonkRecMonoBinds env1 dict_binds 	`thenM` \ (env2, new_dict_binds) -> 
    returnM (env2, HsIPBinds (IPBinds new_binds new_dict_binds))
  where
    zonk_ip_bind (IPBind n e)
	= mapIPNameTc (zonkIdBndr env) n	`thenM` \ n' ->
	  zonkLExpr env e			`thenM` \ e' ->
	  returnM (IPBind n' e')


---------------------------------------------
zonkValBinds :: ZonkEnv -> HsValBinds TcId -> TcM (ZonkEnv, HsValBinds Id)
zonkValBinds env bs@(ValBindsIn _ _) 
  = panic "zonkValBinds"	-- Not in typechecker output
zonkValBinds env (ValBindsOut binds sigs) 
  = do 	{ (env1, new_binds) <- go env binds
	; return (env1, ValBindsOut new_binds sigs) }
  where
    go env []         = return (env, [])
    go env ((r,b):bs) = do { (env1, b')  <- zonkRecMonoBinds env b
			   ; (env2, bs') <- go env1 bs
			   ; return (env2, (r,b'):bs') }

---------------------------------------------
zonkRecMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (ZonkEnv, LHsBinds Id)
zonkRecMonoBinds env binds 
 = fixM (\ ~(_, new_binds) -> do 
	{ let env1 = extendZonkEnv env (collectHsBindBinders new_binds)
        ; binds' <- zonkMonoBinds env1 binds
        ; return (env1, binds') })

---------------------------------------------
zonkMonoBinds :: ZonkEnv -> LHsBinds TcId -> TcM (LHsBinds Id)
zonkMonoBinds env binds = mapBagM (wrapLocM (zonk_bind env)) binds

zonk_bind :: ZonkEnv -> HsBind TcId -> TcM (HsBind Id)
zonk_bind env bind@(PatBind { pat_lhs = pat, pat_rhs = grhss, pat_rhs_ty = ty})
  = do	{ (_env, new_pat) <- zonkPat env pat		-- Env already extended
	; new_grhss <- zonkGRHSs env grhss
	; new_ty    <- zonkTcTypeToType env ty
	; return (bind { pat_lhs = new_pat, pat_rhs = new_grhss, pat_rhs_ty = new_ty }) }

zonk_bind env (VarBind { var_id = var, var_rhs = expr })
  = zonkIdBndr env var 			`thenM` \ new_var ->
    zonkLExpr env expr			`thenM` \ new_expr ->
    returnM (VarBind { var_id = new_var, var_rhs = new_expr })

zonk_bind env bind@(FunBind { fun_id = var, fun_matches = ms, fun_co_fn = co_fn })
  = wrapLocM (zonkIdBndr env) var	`thenM` \ new_var ->
    zonkCoFn env co_fn			`thenM` \ (env1, new_co_fn) ->
    zonkMatchGroup env1 ms		`thenM` \ new_ms ->
    returnM (bind { fun_id = new_var, fun_matches = new_ms, fun_co_fn = new_co_fn })

zonk_bind env (AbsBinds { abs_tvs = tyvars, abs_dicts = dicts, 
			  abs_exports = exports, abs_binds = val_binds })
  = ASSERT( all isImmutableTyVar tyvars )
    zonkIdBndrs env dicts		`thenM` \ new_dicts ->
    fixM (\ ~(new_val_binds, _) ->
	let
	  env1 = extendZonkEnv env new_dicts
	  env2 = extendZonkEnv env1 (collectHsBindBinders new_val_binds)
	in
	zonkMonoBinds env2 val_binds 		`thenM` \ new_val_binds ->
        mappM (zonkExport env2) exports		`thenM` \ new_exports ->
	returnM (new_val_binds, new_exports)
    )						`thenM` \ (new_val_bind, new_exports) ->
    returnM (AbsBinds { abs_tvs = tyvars, abs_dicts = new_dicts, 
			abs_exports = new_exports, abs_binds = new_val_bind })
  where
    zonkExport env (tyvars, global, local, prags)
	-- The tyvars are already zonked
	= zonkIdBndr env global			`thenM` \ new_global ->
	  mapM zonk_prag prags			`thenM` \ new_prags -> 
	  returnM (tyvars, new_global, zonkIdOcc env local, new_prags)
    zonk_prag prag@(InlinePrag {})  = return prag
    zonk_prag (SpecPrag expr ty ds inl) = do { expr' <- zonkExpr env expr 
					     ; ty'   <- zonkTcTypeToType env ty
					     ; let ds' = zonkIdOccs env ds
					     ; return (SpecPrag expr' ty' ds' inl) }
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
%*									*
%************************************************************************

\begin{code}
zonkMatchGroup :: ZonkEnv -> MatchGroup TcId-> TcM (MatchGroup Id)
zonkMatchGroup env (MatchGroup ms ty) 
  = do	{ ms' <- mapM (zonkMatch env) ms
	; ty' <- zonkTcTypeToType env ty
	; return (MatchGroup ms' ty') }

zonkMatch :: ZonkEnv -> LMatch TcId-> TcM (LMatch Id)
zonkMatch env (L loc (Match pats _ grhss))
  = do	{ (env1, new_pats) <- zonkPats env pats
	; new_grhss <- zonkGRHSs env1 grhss
	; return (L loc (Match new_pats Nothing new_grhss)) }

-------------------------------------------------------------------------
zonkGRHSs :: ZonkEnv -> GRHSs TcId -> TcM (GRHSs Id)

zonkGRHSs env (GRHSs grhss binds)
  = zonkLocalBinds env binds   	`thenM` \ (new_env, new_binds) ->
    let
	zonk_grhs (GRHS guarded rhs)
	  = zonkStmts new_env guarded	`thenM` \ (env2, new_guarded) ->
	    zonkLExpr env2 rhs		`thenM` \ new_rhs ->
	    returnM (GRHS new_guarded new_rhs)
    in
    mappM (wrapLocM zonk_grhs) grhss 	`thenM` \ new_grhss ->
    returnM (GRHSs new_grhss new_binds)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkLExprs :: ZonkEnv -> [LHsExpr TcId] -> TcM [LHsExpr Id]
zonkLExpr  :: ZonkEnv -> LHsExpr TcId   -> TcM (LHsExpr Id)
zonkExpr   :: ZonkEnv -> HsExpr TcId    -> TcM (HsExpr Id)

zonkLExprs env exprs = mappM (zonkLExpr env) exprs
zonkLExpr  env expr  = wrapLocM (zonkExpr env) expr

zonkExpr env (HsVar id)
  = returnM (HsVar (zonkIdOcc env id))

zonkExpr env (HsIPVar id)
  = returnM (HsIPVar (mapIPName (zonkIdOcc env) id))

zonkExpr env (HsLit (HsRat f ty))
  = zonkTcTypeToType env ty	   `thenM` \ new_ty  ->
    returnM (HsLit (HsRat f new_ty))

zonkExpr env (HsLit lit)
  = returnM (HsLit lit)

zonkExpr env (HsOverLit lit)
  = do	{ lit' <- zonkOverLit env lit
	; return (HsOverLit lit') }

zonkExpr env (HsLam matches)
  = zonkMatchGroup env matches	`thenM` \ new_matches ->
    returnM (HsLam new_matches)

zonkExpr env (HsApp e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (HsApp new_e1 new_e2)

zonkExpr env (HsBracketOut body bs) 
  = mappM zonk_b bs	`thenM` \ bs' ->
    returnM (HsBracketOut body bs')
  where
    zonk_b (n,e) = zonkLExpr env e	`thenM` \ e' ->
		   returnM (n,e')

zonkExpr env (HsSpliceE s) = WARN( True, ppr s )	-- Should not happen
			     returnM (HsSpliceE s)

zonkExpr env (OpApp e1 op fixity e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env op	`thenM` \ new_op ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (OpApp new_e1 new_op fixity new_e2)

zonkExpr env (NegApp expr op)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    zonkExpr env op	`thenM` \ new_op ->
    returnM (NegApp new_expr new_op)

zonkExpr env (HsPar e)    
  = zonkLExpr env e	`thenM` \new_e ->
    returnM (HsPar new_e)

zonkExpr env (SectionL expr op)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    zonkLExpr env op		`thenM` \ new_op ->
    returnM (SectionL new_expr new_op)

zonkExpr env (SectionR op expr)
  = zonkLExpr env op		`thenM` \ new_op ->
    zonkLExpr env expr		`thenM` \ new_expr ->
    returnM (SectionR new_op new_expr)

zonkExpr env (HsCase expr ms)
  = zonkLExpr env expr    	`thenM` \ new_expr ->
    zonkMatchGroup env ms	`thenM` \ new_ms ->
    returnM (HsCase new_expr new_ms)

zonkExpr env (HsIf e1 e2 e3)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    zonkLExpr env e3	`thenM` \ new_e3 ->
    returnM (HsIf new_e1 new_e2 new_e3)

zonkExpr env (HsLet binds expr)
  = zonkLocalBinds env binds	`thenM` \ (new_env, new_binds) ->
    zonkLExpr new_env expr	`thenM` \ new_expr ->
    returnM (HsLet new_binds new_expr)

zonkExpr env (HsDo do_or_lc stmts body ty)
  = zonkStmts env stmts 	`thenM` \ (new_env, new_stmts) ->
    zonkLExpr new_env body	`thenM` \ new_body ->
    zonkTcTypeToType env ty	`thenM` \ new_ty   ->
    returnM (HsDo (zonkDo env do_or_lc) 
		  new_stmts new_body new_ty)

zonkExpr env (ExplicitList ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkLExprs env exprs	`thenM` \ new_exprs ->
    returnM (ExplicitList new_ty new_exprs)

zonkExpr env (ExplicitPArr ty exprs)
  = zonkTcTypeToType env ty	`thenM` \ new_ty ->
    zonkLExprs env exprs	`thenM` \ new_exprs ->
    returnM (ExplicitPArr new_ty new_exprs)

zonkExpr env (ExplicitTuple exprs boxed)
  = zonkLExprs env exprs  	`thenM` \ new_exprs ->
    returnM (ExplicitTuple new_exprs boxed)

zonkExpr env (RecordCon data_con con_expr rbinds)
  = zonkExpr env con_expr	`thenM` \ new_con_expr ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordCon data_con new_con_expr new_rbinds)

zonkExpr env (RecordUpd expr rbinds in_ty out_ty)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkTcTypeToType env in_ty	`thenM` \ new_in_ty ->
    zonkTcTypeToType env out_ty	`thenM` \ new_out_ty ->
    zonkRbinds env rbinds	`thenM` \ new_rbinds ->
    returnM (RecordUpd new_expr new_rbinds new_in_ty new_out_ty)

zonkExpr env (ExprWithTySigOut e ty) 
  = do { e' <- zonkLExpr env e
       ; return (ExprWithTySigOut e' ty) }

zonkExpr env (ExprWithTySig _ _) = panic "zonkExpr env:ExprWithTySig"

zonkExpr env (ArithSeq expr info)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (ArithSeq new_expr new_info)

zonkExpr env (PArrSeq expr info)
  = zonkExpr env expr		`thenM` \ new_expr ->
    zonkArithSeq env info	`thenM` \ new_info ->
    returnM (PArrSeq new_expr new_info)

zonkExpr env (HsSCC lbl expr)
  = zonkLExpr env expr	`thenM` \ new_expr ->
    returnM (HsSCC lbl new_expr)

-- hdaume: core annotations
zonkExpr env (HsCoreAnn lbl expr)
  = zonkLExpr env expr   `thenM` \ new_expr ->
    returnM (HsCoreAnn lbl new_expr)

-- arrow notation extensions
zonkExpr env (HsProc pat body)
  = do	{ (env1, new_pat) <- zonkPat env pat
	; new_body <- zonkCmdTop env1 body
	; return (HsProc new_pat new_body) }

zonkExpr env (HsArrApp e1 e2 ty ho rl)
  = zonkLExpr env e1	    	    	`thenM` \ new_e1 ->
    zonkLExpr env e2	    	    	`thenM` \ new_e2 ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    returnM (HsArrApp new_e1 new_e2 new_ty ho rl)

zonkExpr env (HsArrForm op fixity args)
  = zonkLExpr env op	    	    	`thenM` \ new_op ->
    mappM (zonkCmdTop env) args		`thenM` \ new_args ->
    returnM (HsArrForm new_op fixity new_args)

zonkExpr env (HsWrap co_fn expr)
  = zonkCoFn env co_fn	`thenM` \ (env1, new_co_fn) ->
    zonkExpr env1 expr	`thenM` \ new_expr ->
    return (HsWrap new_co_fn new_expr)

zonkExpr env other = pprPanic "zonkExpr" (ppr other)

zonkCmdTop :: ZonkEnv -> LHsCmdTop TcId -> TcM (LHsCmdTop Id)
zonkCmdTop env cmd = wrapLocM (zonk_cmd_top env) cmd

zonk_cmd_top env (HsCmdTop cmd stack_tys ty ids)
  = zonkLExpr env cmd	    		`thenM` \ new_cmd ->
    zonkTcTypeToTypes env stack_tys	`thenM` \ new_stack_tys ->
    zonkTcTypeToType env ty 		`thenM` \ new_ty ->
    mapSndM (zonkExpr env) ids		`thenM` \ new_ids ->
    returnM (HsCmdTop new_cmd new_stack_tys new_ty new_ids)

-------------------------------------------------------------------------
zonkCoFn :: ZonkEnv -> HsWrapper -> TcM (ZonkEnv, HsWrapper)
zonkCoFn env WpHole = return (env, WpHole)
zonkCoFn env (WpCompose c1 c2) = do { (env1, c1') <- zonkCoFn env c1
				    ; (env2, c2') <- zonkCoFn env1 c2
				    ; return (env2, WpCompose c1' c2') }
zonkCoFn env (WpCo co)      = do { co' <- zonkTcTypeToType env co
				 ; return (env, WpCo co') }
zonkCoFn env (WpLam id)     = do { id' <- zonkIdBndr env id
				 ; let env1 = extendZonkEnv1 env id'
				 ; return (env1, WpLam id') }
zonkCoFn env (WpTyLam tv)   = ASSERT( isImmutableTyVar tv )
			      do { return (env, WpTyLam tv) }
zonkCoFn env (WpApp id)     = do { return (env, WpApp (zonkIdOcc env id)) }
zonkCoFn env (WpTyApp ty)   = do { ty' <- zonkTcTypeToType env ty
				 ; return (env, WpTyApp ty') }
zonkCoFn env (WpLet bs)     = do { (env1, bs') <- zonkRecMonoBinds env bs
				 ; return (env1, WpLet bs') }


-------------------------------------------------------------------------
zonkDo :: ZonkEnv -> HsStmtContext Name -> HsStmtContext Name
-- Only used for 'do', so the only Ids are in a MDoExpr table
zonkDo env (MDoExpr tbl) = MDoExpr (mapSnd (zonkIdOcc env) tbl)
zonkDo env do_or_lc      = do_or_lc

-------------------------------------------------------------------------
zonkOverLit :: ZonkEnv -> HsOverLit TcId -> TcM (HsOverLit Id)
zonkOverLit env (HsIntegral i e)
  = do	{ e' <- zonkExpr env e; return (HsIntegral i e') }
zonkOverLit env (HsFractional r e)
  = do	{ e' <- zonkExpr env e; return (HsFractional r e') }

-------------------------------------------------------------------------
zonkArithSeq :: ZonkEnv -> ArithSeqInfo TcId -> TcM (ArithSeqInfo Id)

zonkArithSeq env (From e)
  = zonkLExpr env e		`thenM` \ new_e ->
    returnM (From new_e)

zonkArithSeq env (FromThen e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (FromThen new_e1 new_e2)

zonkArithSeq env (FromTo e1 e2)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    returnM (FromTo new_e1 new_e2)

zonkArithSeq env (FromThenTo e1 e2 e3)
  = zonkLExpr env e1	`thenM` \ new_e1 ->
    zonkLExpr env e2	`thenM` \ new_e2 ->
    zonkLExpr env e3	`thenM` \ new_e3 ->
    returnM (FromThenTo new_e1 new_e2 new_e3)


-------------------------------------------------------------------------
zonkStmts :: ZonkEnv -> [LStmt TcId] -> TcM (ZonkEnv, [LStmt Id])
zonkStmts env []     = return (env, [])
zonkStmts env (s:ss) = do { (env1, s')  <- wrapLocSndM (zonkStmt env) s
			  ; (env2, ss') <- zonkStmts env1 ss
			  ; return (env2, s' : ss') }

zonkStmt :: ZonkEnv -> Stmt TcId -> TcM (ZonkEnv, Stmt Id)
zonkStmt env (ParStmt stmts_w_bndrs)
  = mappM zonk_branch stmts_w_bndrs	`thenM` \ new_stmts_w_bndrs ->
    let 
	new_binders = concat (map snd new_stmts_w_bndrs)
	env1 = extendZonkEnv env new_binders
    in
    return (env1, ParStmt new_stmts_w_bndrs)
  where
    zonk_branch (stmts, bndrs) = zonkStmts env stmts	`thenM` \ (env1, new_stmts) ->
				 returnM (new_stmts, zonkIdOccs env1 bndrs)

zonkStmt env (RecStmt segStmts lvs rvs rets binds)
  = zonkIdBndrs env rvs		`thenM` \ new_rvs ->
    let
	env1 = extendZonkEnv env new_rvs
    in
    zonkStmts env1 segStmts	`thenM` \ (env2, new_segStmts) ->
	-- Zonk the ret-expressions in an envt that 
	-- has the polymorphic bindings in the envt
    mapM (zonkExpr env2) rets	`thenM` \ new_rets ->
    let
	new_lvs = zonkIdOccs env2 lvs
	env3 = extendZonkEnv env new_lvs	-- Only the lvs are needed
    in
    zonkRecMonoBinds env3 binds	`thenM` \ (env4, new_binds) ->
    returnM (env4, RecStmt new_segStmts new_lvs new_rvs new_rets new_binds)

zonkStmt env (ExprStmt expr then_op ty)
  = zonkLExpr env expr		`thenM` \ new_expr ->
    zonkExpr env then_op	`thenM` \ new_then ->
    zonkTcTypeToType env ty	`thenM` \ new_ty ->
    returnM (env, ExprStmt new_expr new_then new_ty)

zonkStmt env (LetStmt binds)
  = zonkLocalBinds env binds	`thenM` \ (env1, new_binds) ->
    returnM (env1, LetStmt new_binds)

zonkStmt env (BindStmt pat expr bind_op fail_op)
  = do	{ new_expr <- zonkLExpr env expr
	; (env1, new_pat) <- zonkPat env pat
	; new_bind <- zonkExpr env bind_op
	; new_fail <- zonkExpr env fail_op
	; return (env1, BindStmt new_pat new_expr new_bind new_fail) }


-------------------------------------------------------------------------
zonkRbinds :: ZonkEnv -> HsRecordBinds TcId -> TcM (HsRecordBinds Id)

zonkRbinds env rbinds
  = mappM zonk_rbind rbinds
  where
    zonk_rbind (field, expr)
      = zonkLExpr env expr	`thenM` \ new_expr ->
	returnM (fmap (zonkIdOcc env) field, new_expr)

-------------------------------------------------------------------------
mapIPNameTc :: (a -> TcM b) -> IPName a -> TcM (IPName b)
mapIPNameTc f (IPName n) = f n  `thenM` \ r -> returnM (IPName r)
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: ZonkEnv -> OutPat TcId -> TcM (ZonkEnv, OutPat Id)
-- Extend the environment as we go, because it's possible for one
-- pattern to bind something that is used in another (inside or
-- to the right)
zonkPat env pat = wrapLocSndM (zonk_pat env) pat

zonk_pat env (ParPat p)
  = do	{ (env', p') <- zonkPat env p
  	; return (env', ParPat p') }

zonk_pat env (WildPat ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; return (env, WildPat ty') }

zonk_pat env (VarPat v)
  = do	{ v' <- zonkIdBndr env v
	; return (extendZonkEnv1 env v', VarPat v') }

zonk_pat env (VarPatOut v binds)
  = do	{ v' <- zonkIdBndr env v
	; (env', binds') <- zonkRecMonoBinds (extendZonkEnv1 env v') binds
  	; returnM (env', VarPatOut v' binds') }

zonk_pat env (LazyPat pat)
  = do	{ (env', pat') <- zonkPat env pat
	; return (env',  LazyPat pat') }

zonk_pat env (BangPat pat)
  = do	{ (env', pat') <- zonkPat env pat
	; return (env',  BangPat pat') }

zonk_pat env (AsPat (L loc v) pat)
  = do	{ v' <- zonkIdBndr env v
	; (env', pat') <- zonkPat (extendZonkEnv1 env v') pat
 	; return (env', AsPat (L loc v') pat') }

zonk_pat env (ListPat pats ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pats') <- zonkPats env pats
	; return (env', ListPat pats' ty') }

zonk_pat env (PArrPat pats ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pats') <- zonkPats env pats
	; return (env', PArrPat pats' ty') }

zonk_pat env (TuplePat pats boxed ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pats') <- zonkPats env pats
	; return (env', TuplePat pats' boxed ty') }

zonk_pat env p@(ConPatOut { pat_ty = ty, pat_dicts = dicts, pat_binds = binds, pat_args = args })
  = ASSERT( all isImmutableTyVar (pat_tvs p) ) 
    do	{ new_ty <- zonkTcTypeToType env ty
	; new_dicts <- zonkIdBndrs env dicts
	; let env1 = extendZonkEnv env new_dicts
	; (env2, new_binds) <- zonkRecMonoBinds env1 binds
	; (env', new_args) <- zonkConStuff env2 args
	; returnM (env', p { pat_ty = new_ty, pat_dicts = new_dicts, 
			     pat_binds = new_binds, pat_args = new_args }) }

zonk_pat env (LitPat lit) = return (env, LitPat lit)

zonk_pat env (SigPatOut pat ty)
  = do	{ ty' <- zonkTcTypeToType env ty
	; (env', pat') <- zonkPat env pat
	; return (env', SigPatOut pat' ty') }

zonk_pat env (NPat lit mb_neg eq_expr ty)
  = do	{ lit' <- zonkOverLit env lit
 	; mb_neg' <- case mb_neg of
			Nothing  -> return Nothing
			Just neg -> do { neg' <- zonkExpr env neg
				       ; return (Just neg') }
 	; eq_expr' <- zonkExpr env eq_expr
	; ty' <- zonkTcTypeToType env ty
	; return (env, NPat lit' mb_neg' eq_expr' ty') }

zonk_pat env (NPlusKPat (L loc n) lit e1 e2)
  = do	{ n' <- zonkIdBndr env n
	; lit' <- zonkOverLit env lit
 	; e1' <- zonkExpr env e1
	; e2' <- zonkExpr env e2
	; return (extendZonkEnv1 env n', NPlusKPat (L loc n') lit' e1' e2') }

zonk_pat env (DictPat ds ms)
  = do	{ ds' <- zonkIdBndrs env ds
	; ms' <- zonkIdBndrs env ms
	; return (extendZonkEnv env (ds' ++ ms'), DictPat ds' ms') }

zonk_pat env (CoPat co_fn pat ty) 
  = do { (env', co_fn') <- zonkCoFn env co_fn
       ; (env'', pat') <- zonkPat env' (noLoc pat)
       ; ty' <- zonkTcTypeToType env'' ty
       ; return (env'', CoPat co_fn' (unLoc pat') ty') }

zonk_pat env pat = pprPanic "zonk_pat" (ppr pat)

---------------------------
zonkConStuff env (PrefixCon pats)
  = do	{ (env', pats') <- zonkPats env pats
	; return (env', PrefixCon pats') }

zonkConStuff env (InfixCon p1 p2)
  = do	{ (env1, p1') <- zonkPat env  p1
	; (env', p2') <- zonkPat env1 p2
	; return (env', InfixCon p1' p2') }

zonkConStuff env (RecCon rpats)
  = do	{ let (fields, pats) = unzip [ (f, p) | HsRecField f p _  <- rpats ]
	; (env', pats') <- zonkPats env pats
	; let recCon = RecCon [ mkRecField f p | (f, p) <- zip fields pats' ]
	; returnM (env', recCon) }

---------------------------
zonkPats env []		= return (env, [])
zonkPats env (pat:pats) = do { (env1, pat') <- zonkPat env pat
		     ; (env', pats') <- zonkPats env1 pats
		     ; return (env', pat':pats') }
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************


\begin{code}
zonkForeignExports :: ZonkEnv -> [LForeignDecl TcId] -> TcM [LForeignDecl Id]
zonkForeignExports env ls = mappM (wrapLocM (zonkForeignExport env)) ls

zonkForeignExport :: ZonkEnv -> ForeignDecl TcId -> TcM (ForeignDecl Id)
zonkForeignExport env (ForeignExport i hs_ty spec) =
   returnM (ForeignExport (fmap (zonkIdOcc env) i) undefined spec)
zonkForeignExport env for_imp 
  = returnM for_imp	-- Foreign imports don't need zonking
\end{code}

\begin{code}
zonkRules :: ZonkEnv -> [LRuleDecl TcId] -> TcM [LRuleDecl Id]
zonkRules env rs = mappM (wrapLocM (zonkRule env)) rs

zonkRule :: ZonkEnv -> RuleDecl TcId -> TcM (RuleDecl Id)
zonkRule env (HsRule name act (vars::[RuleBndr TcId]) lhs fv_lhs rhs fv_rhs)
  = mappM zonk_bndr vars		`thenM` \ new_bndrs ->
    newMutVar emptyVarSet		`thenM` \ unbound_tv_set ->
    let
	env_rhs = extendZonkEnv env [id | b <- new_bndrs, let id = unLoc b, isId id]
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
    zonkLExpr env_lhs lhs		`thenM` \ new_lhs ->
    zonkLExpr env_rhs rhs		`thenM` \ new_rhs ->

    readMutVar unbound_tv_set		`thenM` \ unbound_tvs ->
    let
	final_bndrs :: [Located Var]
	final_bndrs = map noLoc (varSetElems unbound_tvs) ++ new_bndrs
    in
    returnM (HsRule name act (map RuleBndr final_bndrs) new_lhs fv_lhs new_rhs fv_rhs)
		-- I hate this map RuleBndr stuff
  where
   zonk_bndr (RuleBndr v) 
	| isId (unLoc v) = wrapLocM (zonkIdBndr env)   v
	| otherwise      = ASSERT( isImmutableTyVar (unLoc v) )
			   return v
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************

\begin{code}
zonkTcTypeToType :: ZonkEnv -> TcType -> TcM Type
zonkTcTypeToType (ZonkEnv zonk_ty _) ty = zonk_ty ty

zonkTcTypeToTypes :: ZonkEnv -> [TcType] -> TcM [Type]
zonkTcTypeToTypes env tys = mapM (zonkTcTypeToType env) tys

zonkTypeCollecting :: TcRef TyVarSet -> TcType -> TcM Type
-- This variant collects unbound type variables in a mutable variable
zonkTypeCollecting unbound_tv_set
  = zonkType zonk_unbound_tyvar
  where
    zonk_unbound_tyvar tv 
	= zonkQuantifiedTyVar tv				`thenM` \ tv' ->
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
    zonk_unbound_tyvar tv = do { writeMetaTyVar tv ty; return ty }
			  where 
			    ty = mkArbitraryType tv


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
  | liftedTypeKind `isSubKind` kind = voidTy		-- The vastly common case
  | otherwise			    = mkTyConApp tycon []
  where
    kind       = tyVarKind tv
    (args,res) = splitKindFunTys kind

    tycon | eqKind kind (tyConKind listTyCon) 	--  *->*
	  = listTyCon				-- No tuples this size

	  | all isLiftedTypeKind args && isLiftedTypeKind res
	  = tupleTyCon Boxed (length args)	--  *-> ... ->*->*

	  | otherwise
	  = pprTrace "Urk! Inventing strangely-kinded void TyCon:" (ppr tc_name $$ ppr kind) $
	    mkPrimTyCon tc_name kind 0 VoidRep
		-- Same name as the tyvar, apart from making it start with a colon (sigh)
		-- I dread to think what will happen if this gets out into an 
		-- interface file.  Catastrophe likely.  Major sigh.

    tc_name = mkInternalName (getUnique tv) (mkDerivedTyConOcc (getOccName tv)) noSrcLoc
\end{code}
