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
	TcHsModule, TcCoreExpr, TcDictBinds,
	TcForeignExportDecl,
	
	TypecheckedHsBinds, TypecheckedRuleDecl,
	TypecheckedMonoBinds, TypecheckedPat,
	TypecheckedHsExpr, TypecheckedArithSeqInfo,
	TypecheckedStmt, TypecheckedForeignDecl,
	TypecheckedMatch, TypecheckedHsModule,
	TypecheckedGRHSs, TypecheckedGRHS,
	TypecheckedRecordBinds, TypecheckedDictBinds,

	mkHsTyApp, mkHsDictApp, mkHsConApp,
	mkHsTyLam, mkHsDictLam, mkHsLet,
	idsToMonoBinds,

	-- re-exported from TcEnv
	TcId, tcInstId,

	zonkTopBinds, zonkId, zonkIdOcc,
	zonkForeignExports, zonkRules
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it

-- others:
import Id	( idName, idType, idUnfolding, setIdType, omitIfaceSigForId, isIP, Id )
import DataCon	( DataCon, dataConWrapId, splitProductType_maybe )	
import TcEnv	( tcLookupValueMaybe, tcExtendGlobalValEnv, tcGetValueEnv,
		  ValueEnv, TcId, tcInstId
		)

import TcMonad
import TcType	( TcType, TcTyVar,
		  zonkTcTypeToType, zonkTcTyVarToTyVar, zonkTcTyVarBndr, zonkTcType
		)
import Type	( mkTyVarTy, isUnLiftedType, Type )
import Name	( isLocallyDefined )
import Var	( TyVar )
import VarEnv	( TyVarEnv, emptyVarEnv, extendVarEnvList )
import VarSet	( isEmptyVarSet )
import CoreSyn  ( Expr )
import CoreUnfold( unfoldingTemplate )
import BasicTypes ( RecFlag(..) )
import Bag
import UniqFM
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
type TcHsBinds     	= HsBinds TcId TcPat
type TcMonoBinds	= MonoBinds TcId TcPat
type TcDictBinds	= TcMonoBinds
type TcPat	     	= OutPat TcId
type TcExpr	     	= HsExpr TcId TcPat
type TcGRHSs		= GRHSs TcId TcPat
type TcGRHS		= GRHS TcId TcPat
type TcMatch		= Match TcId TcPat
type TcStmt		= Stmt TcId TcPat
type TcArithSeqInfo	= ArithSeqInfo TcId TcPat
type TcRecordBinds	= HsRecordBinds TcId TcPat
type TcHsModule	= HsModule TcId TcPat

type TcCoreExpr	= Expr TcId
type TcForeignExportDecl = ForeignDecl TcId
type TcRuleDecl 	 = RuleDecl    TcId TcPat

type TypecheckedPat		= OutPat	Id
type TypecheckedMonoBinds 	= MonoBinds	Id TypecheckedPat
type TypecheckedDictBinds 	= TypecheckedMonoBinds
type TypecheckedHsBinds		= HsBinds	Id TypecheckedPat
type TypecheckedHsExpr		= HsExpr	Id TypecheckedPat
type TypecheckedArithSeqInfo	= ArithSeqInfo	Id TypecheckedPat
type TypecheckedStmt		= Stmt		Id TypecheckedPat
type TypecheckedMatch		= Match		Id TypecheckedPat
type TypecheckedGRHSs		= GRHSs		Id TypecheckedPat
type TypecheckedGRHS		= GRHS		Id TypecheckedPat
type TypecheckedRecordBinds	= HsRecordBinds Id TypecheckedPat
type TypecheckedHsModule	= HsModule	Id TypecheckedPat
type TypecheckedForeignDecl     = ForeignDecl Id
type TypecheckedRuleDecl	= RuleDecl      Id TypecheckedPat
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

idsToMonoBinds :: [Id] -> TcMonoBinds 
idsToMonoBinds ids
  = andMonoBindList [ CoreMonoBind id (unfoldingTemplate (idUnfolding id))
		    | id <- ids
		    ]
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
-- zonkId is used *during* typechecking just to zonk the Id's type
zonkId :: TcId -> NF_TcM s TcId
zonkId id
  = zonkTcType (idType id) `thenNF_Tc` \ ty' ->
    returnNF_Tc (setIdType id ty')

-- zonkIdBndr is used *after* typechecking to get the Id's type
-- to its final form.  The TyVarEnv give 
zonkIdBndr :: TcId -> NF_TcM s Id
zonkIdBndr id
  = zonkTcTypeToType (idType id)	`thenNF_Tc` \ ty' ->
    returnNF_Tc (setIdType id ty')

zonkIdOcc :: TcId -> NF_TcM s Id
zonkIdOcc id 
  | not (isLocallyDefined id) || omitIfaceSigForId id || isIP id
	-- The omitIfaceSigForId thing may look wierd but it's quite
	-- sensible really.  We're avoiding looking up superclass selectors
	-- and constructors; zonking them is a no-op anyway, and the
	-- superclass selectors aren't in the environment anyway.
  = returnNF_Tc id
  | otherwise 
  = tcLookupValueMaybe (idName id)	`thenNF_Tc` \ maybe_id' ->
    let
	new_id = case maybe_id' of
		    Just id' -> id'
		    Nothing  -> pprTrace "zonkIdOcc: " (ppr id) id
    in
    returnNF_Tc new_id
\end{code}


\begin{code}
zonkTopBinds :: TcMonoBinds -> NF_TcM s (TypecheckedMonoBinds, ValueEnv)
zonkTopBinds binds	-- Top level is implicitly recursive
  = fixNF_Tc (\ ~(_, new_ids) ->
	tcExtendGlobalValEnv (bagToList new_ids)	$
	zonkMonoBinds binds			`thenNF_Tc` \ (binds', new_ids) ->
	tcGetValueEnv				`thenNF_Tc` \ env ->
	returnNF_Tc ((binds', env), new_ids)
    )					`thenNF_Tc` \ (stuff, _) ->
    returnNF_Tc stuff

zonkBinds :: TcHsBinds -> NF_TcM s (TypecheckedHsBinds, TcEnv)

zonkBinds binds 
  = go binds (\ binds' -> tcGetEnv `thenNF_Tc` \ env -> 
			  returnNF_Tc (binds', env))
  where
    -- go :: TcHsBinds
    --    -> (TypecheckedHsBinds
    --	      -> NF_TcM s (TypecheckedHsBinds, TcEnv)
    --       ) 
    --	  -> NF_TcM s (TypecheckedHsBinds, TcEnv)

    go (ThenBinds b1 b2) thing_inside = go b1 	$ \ b1' -> 
					go b2 	$ \ b2' ->
					thing_inside (b1' `ThenBinds` b2')

    go EmptyBinds thing_inside = thing_inside EmptyBinds

    go (MonoBind bind sigs is_rec) thing_inside
	  = ASSERT( null sigs )
	    fixNF_Tc (\ ~(_, new_ids) ->
		tcExtendGlobalValEnv (bagToList new_ids)	$
		zonkMonoBinds bind				`thenNF_Tc` \ (new_bind, new_ids) ->
		thing_inside (mkMonoBind new_bind [] is_rec)	`thenNF_Tc` \ stuff ->
		returnNF_Tc (stuff, new_ids)
	    )							`thenNF_Tc` \ (stuff, _) ->
	   returnNF_Tc stuff
\end{code}

\begin{code}
-------------------------------------------------------------------------
zonkMonoBinds :: TcMonoBinds
	      -> NF_TcM s (TypecheckedMonoBinds, Bag Id)

zonkMonoBinds EmptyMonoBinds = returnNF_Tc (EmptyMonoBinds, emptyBag)

zonkMonoBinds (AndMonoBinds mbinds1 mbinds2)
  = zonkMonoBinds mbinds1		`thenNF_Tc` \ (b1', ids1) ->
    zonkMonoBinds mbinds2		`thenNF_Tc` \ (b2', ids2) ->
    returnNF_Tc (b1' `AndMonoBinds` b2', 
		 ids1 `unionBags` ids2)

zonkMonoBinds (PatMonoBind pat grhss locn)
  = zonkPat pat		`thenNF_Tc` \ (new_pat, ids) ->
    zonkGRHSs grhss	`thenNF_Tc` \ new_grhss ->
    returnNF_Tc (PatMonoBind new_pat new_grhss locn, ids)

zonkMonoBinds (VarMonoBind var expr)
  = zonkIdBndr var    	`thenNF_Tc` \ new_var ->
    zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (VarMonoBind new_var new_expr, unitBag new_var)

zonkMonoBinds (CoreMonoBind var core_expr)
  = zonkIdBndr var    	`thenNF_Tc` \ new_var ->
    returnNF_Tc (CoreMonoBind new_var core_expr, unitBag new_var)

zonkMonoBinds (FunMonoBind var inf ms locn)
  = zonkIdBndr var			`thenNF_Tc` \ new_var ->
    mapNF_Tc zonkMatch ms		`thenNF_Tc` \ new_ms ->
    returnNF_Tc (FunMonoBind new_var inf new_ms locn, unitBag new_var)


zonkMonoBinds (AbsBinds tyvars dicts exports inlines val_bind)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
	-- No need to extend tyvar env: the effects are
	-- propagated through binding the tyvars themselves

    mapNF_Tc zonkIdBndr  dicts		`thenNF_Tc` \ new_dicts ->
    tcExtendGlobalValEnv new_dicts			$

    fixNF_Tc (\ ~(_, _, val_bind_ids) ->
	tcExtendGlobalValEnv (bagToList val_bind_ids)	$
	zonkMonoBinds val_bind 				`thenNF_Tc` \ (new_val_bind, val_bind_ids) ->
        mapNF_Tc zonkExport exports			`thenNF_Tc` \ new_exports ->
	returnNF_Tc (new_val_bind, new_exports,  val_bind_ids)
    )						`thenNF_Tc ` \ (new_val_bind, new_exports, _) ->
    let
	    new_globals = listToBag [global | (_, global, local) <- new_exports]
    in
    returnNF_Tc (AbsBinds new_tyvars new_dicts new_exports inlines new_val_bind,
		 new_globals)
  where
    zonkExport (tyvars, global, local)
	= mapNF_Tc zonkTcTyVarBndr tyvars	`thenNF_Tc` \ new_tyvars ->
	  zonkIdBndr global			`thenNF_Tc` \ new_global ->
	  zonkIdOcc local			`thenNF_Tc` \ new_local -> 
	  returnNF_Tc (new_tyvars, new_global, new_local)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSs}
%*									*
%************************************************************************

\begin{code}
zonkMatch :: TcMatch -> NF_TcM s TypecheckedMatch

zonkMatch (Match _ pats _ grhss)
  = zonkPats pats				`thenNF_Tc` \ (new_pats, new_ids) ->
    tcExtendGlobalValEnv (bagToList new_ids)	$
    zonkGRHSs grhss 				`thenNF_Tc` \ new_grhss ->
    returnNF_Tc (Match [] new_pats Nothing new_grhss)

-------------------------------------------------------------------------
zonkGRHSs :: TcGRHSs
	  -> NF_TcM s TypecheckedGRHSs

zonkGRHSs (GRHSs grhss binds (Just ty))
  = zonkBinds binds   		`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env $
    let
	zonk_grhs (GRHS guarded locn)
	  = zonkStmts guarded  `thenNF_Tc` \ new_guarded ->
	    returnNF_Tc (GRHS new_guarded locn)
    in
    mapNF_Tc zonk_grhs grhss 	`thenNF_Tc` \ new_grhss ->
    zonkTcTypeToType ty 	`thenNF_Tc` \ new_ty ->
    returnNF_Tc (GRHSs new_grhss new_binds (Just new_ty))
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkExpr :: TcExpr -> NF_TcM s TypecheckedHsExpr

zonkExpr (HsVar id)
  = zonkIdOcc id	`thenNF_Tc` \ id' ->
    returnNF_Tc (HsVar id')

zonkExpr (HsIPVar id)
  = zonkIdOcc id	`thenNF_Tc` \ id' ->
    returnNF_Tc (HsIPVar id')

zonkExpr (HsLit _) = panic "zonkExpr:HsLit"

zonkExpr (HsLitOut lit ty)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (HsLitOut lit new_ty)

zonkExpr (HsLam match)
  = zonkMatch match	`thenNF_Tc` \ new_match ->
    returnNF_Tc (HsLam new_match)

zonkExpr (HsApp e1 e2)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (HsApp new_e1 new_e2)

zonkExpr (OpApp e1 op fixity e2)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr op	`thenNF_Tc` \ new_op ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (OpApp new_e1 new_op fixity new_e2)

zonkExpr (NegApp _ _) = panic "zonkExpr: NegApp"
zonkExpr (HsPar _)    = panic "zonkExpr: HsPar"

zonkExpr (SectionL expr op)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkExpr op		`thenNF_Tc` \ new_op ->
    returnNF_Tc (SectionL new_expr new_op)

zonkExpr (SectionR op expr)
  = zonkExpr op		`thenNF_Tc` \ new_op ->
    zonkExpr expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SectionR new_op new_expr)

zonkExpr (HsCase expr ms src_loc)
  = zonkExpr expr    	    `thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkMatch ms   `thenNF_Tc` \ new_ms ->
    returnNF_Tc (HsCase new_expr new_ms src_loc)

zonkExpr (HsIf e1 e2 e3 src_loc)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (HsIf new_e1 new_e2 new_e3 src_loc)

zonkExpr (HsLet binds expr)
  = zonkBinds binds		`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env		$
    zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsLet new_binds new_expr)

zonkExpr (HsWith expr binds)
  = zonkExpr expr		`thenNF_Tc` \ new_expr ->
    zonkIPBinds binds		`thenNF_Tc` \ new_binds ->
    returnNF_Tc (HsWith new_expr new_binds)
    where
	zonkIPBinds = mapNF_Tc zonkIPBind
	zonkIPBind (n, e) =
	    zonkExpr e		`thenNF_Tc` \ e' ->
	    returnNF_Tc (n, e')

zonkExpr (HsDo _ _ _) = panic "zonkExpr:HsDo"

zonkExpr (HsDoOut do_or_lc stmts return_id then_id zero_id ty src_loc)
  = zonkStmts stmts 		`thenNF_Tc` \ new_stmts ->
    zonkTcTypeToType ty	`thenNF_Tc` \ new_ty   ->
    zonkIdOcc return_id		`thenNF_Tc` \ new_return_id ->
    zonkIdOcc then_id		`thenNF_Tc` \ new_then_id ->
    zonkIdOcc zero_id		`thenNF_Tc` \ new_zero_id ->
    returnNF_Tc (HsDoOut do_or_lc new_stmts new_return_id new_then_id new_zero_id
			 new_ty src_loc)

zonkExpr (ExplicitList _) = panic "zonkExpr:ExplicitList"

zonkExpr (ExplicitListOut ty exprs)
  = zonkTcTypeToType ty		`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkExpr exprs	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitListOut new_ty new_exprs)

zonkExpr (ExplicitTuple exprs boxed)
  = mapNF_Tc zonkExpr exprs  	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitTuple new_exprs boxed)

zonkExpr (RecordConOut data_con con_expr rbinds)
  = zonkExpr con_expr	`thenNF_Tc` \ new_con_expr ->
    zonkRbinds rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordConOut data_con new_con_expr new_rbinds)

zonkExpr (RecordUpd _ _) = panic "zonkExpr:RecordUpd"

zonkExpr (RecordUpdOut expr ty dicts rbinds)
  = zonkExpr expr		`thenNF_Tc` \ new_expr ->
    zonkTcTypeToType ty	`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkIdOcc dicts	`thenNF_Tc` \ new_dicts ->
    zonkRbinds rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordUpdOut new_expr new_ty new_dicts new_rbinds)

zonkExpr (ExprWithTySig _ _) = panic "zonkExpr:ExprWithTySig"
zonkExpr (ArithSeqIn _)      = panic "zonkExpr:ArithSeqIn"

zonkExpr (ArithSeqOut expr info)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkArithSeq info	`thenNF_Tc` \ new_info ->
    returnNF_Tc (ArithSeqOut new_expr new_info)

zonkExpr (HsCCall fun args may_gc is_casm result_ty)
  = mapNF_Tc zonkExpr args 	`thenNF_Tc` \ new_args ->
    zonkTcTypeToType result_ty	`thenNF_Tc` \ new_result_ty ->
    returnNF_Tc (HsCCall fun new_args may_gc is_casm new_result_ty)

zonkExpr (HsSCC lbl expr)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsSCC lbl new_expr)

zonkExpr (TyLam tyvars expr)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
	-- No need to extend tyvar env; see AbsBinds

    zonkExpr expr			`thenNF_Tc` \ new_expr ->
    returnNF_Tc (TyLam new_tyvars new_expr)

zonkExpr (TyApp expr tys)
  = zonkExpr expr    	    		`thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkTcTypeToType tys	`thenNF_Tc` \ new_tys ->
    returnNF_Tc (TyApp new_expr new_tys)

zonkExpr (DictLam dicts expr)
  = mapNF_Tc zonkIdBndr dicts		`thenNF_Tc` \ new_dicts ->
    tcExtendGlobalValEnv new_dicts	$
    zonkExpr expr    	    		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictLam new_dicts new_expr)

zonkExpr (DictApp expr dicts)
  = zonkExpr expr    	    	`thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkIdOcc dicts	`thenNF_Tc` \ new_dicts ->
    returnNF_Tc (DictApp new_expr new_dicts)



-------------------------------------------------------------------------
zonkArithSeq :: TcArithSeqInfo -> NF_TcM s TypecheckedArithSeqInfo

zonkArithSeq (From e)
  = zonkExpr e		`thenNF_Tc` \ new_e ->
    returnNF_Tc (From new_e)

zonkArithSeq (FromThen e1 e2)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromThen new_e1 new_e2)

zonkArithSeq (FromTo e1 e2)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromTo new_e1 new_e2)

zonkArithSeq (FromThenTo e1 e2 e3)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (FromThenTo new_e1 new_e2 new_e3)

-------------------------------------------------------------------------
zonkStmts :: [TcStmt]
	  -> NF_TcM s [TypecheckedStmt]

zonkStmts [] = returnNF_Tc []

zonkStmts [ReturnStmt expr]
  = zonkExpr expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc [ReturnStmt new_expr]

zonkStmts (ExprStmt expr locn : stmts)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkStmts stmts	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (ExprStmt new_expr locn : new_stmts)

zonkStmts (GuardStmt expr locn : stmts)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkStmts stmts	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (GuardStmt new_expr locn : new_stmts)

zonkStmts (LetStmt binds : stmts)
  = zonkBinds binds		`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env		$
    zonkStmts stmts		`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (LetStmt new_binds : new_stmts)

zonkStmts (BindStmt pat expr locn : stmts)
  = zonkExpr expr				`thenNF_Tc` \ new_expr ->
    zonkPat pat					`thenNF_Tc` \ (new_pat, new_ids) ->
    tcExtendGlobalValEnv (bagToList new_ids)	$ 
    zonkStmts stmts				`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (BindStmt new_pat new_expr locn : new_stmts)



-------------------------------------------------------------------------
zonkRbinds :: TcRecordBinds -> NF_TcM s TypecheckedRecordBinds

zonkRbinds rbinds
  = mapNF_Tc zonk_rbind rbinds
  where
    zonk_rbind (field, expr, pun)
      = zonkExpr expr		`thenNF_Tc` \ new_expr ->
	zonkIdOcc field		`thenNF_Tc` \ new_field ->
	returnNF_Tc (new_field, new_expr, pun)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: TcPat -> NF_TcM s (TypecheckedPat, Bag Id)

zonkPat (WildPat ty)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (WildPat new_ty, emptyBag)

zonkPat (VarPat v)
  = zonkIdBndr v	    `thenNF_Tc` \ new_v ->
    returnNF_Tc (VarPat new_v, unitBag new_v)

zonkPat (LazyPat pat)
  = zonkPat pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (LazyPat new_pat, ids)

zonkPat (AsPat n pat)
  = zonkIdBndr n	    `thenNF_Tc` \ new_n ->
    zonkPat pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (AsPat new_n new_pat, new_n `consBag` ids)

zonkPat (ListPat ty pats)
  = zonkTcTypeToType ty	`thenNF_Tc` \ new_ty ->
    zonkPats pats		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ListPat new_ty new_pats, ids)

zonkPat (TuplePat pats boxed)
  = zonkPats pats   		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (TuplePat new_pats boxed, ids)

zonkPat (ConPat n ty tvs dicts pats)
  = zonkTcTypeToType ty		`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkTcTyVarToTyVar tvs	`thenNF_Tc` \ new_tvs ->
    mapNF_Tc zonkIdBndr dicts		`thenNF_Tc` \ new_dicts ->
    tcExtendGlobalValEnv new_dicts	$
    zonkPats pats			`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ConPat n new_ty new_tvs new_dicts new_pats, 
		 listToBag new_dicts `unionBags` ids)

zonkPat (RecPat n ty tvs dicts rpats)
  = zonkTcTypeToType ty			`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkTcTyVarToTyVar tvs	`thenNF_Tc` \ new_tvs ->
    mapNF_Tc zonkIdBndr dicts		`thenNF_Tc` \ new_dicts ->
    tcExtendGlobalValEnv new_dicts	$
    mapAndUnzipNF_Tc zonk_rpat rpats	`thenNF_Tc` \ (new_rpats, ids_s) ->
    returnNF_Tc (RecPat n new_ty new_tvs new_dicts new_rpats, 
		 listToBag new_dicts `unionBags` unionManyBags ids_s)
  where
    zonk_rpat (f, pat, pun)
      = zonkPat pat		`thenNF_Tc` \ (new_pat, ids) ->
	returnNF_Tc ((f, new_pat, pun), ids)

zonkPat (LitPat lit ty)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (LitPat lit new_ty, emptyBag)

zonkPat (NPat lit ty expr)
  = zonkTcTypeToType ty		`thenNF_Tc` \ new_ty   ->
    zonkExpr expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (NPat lit new_ty new_expr, emptyBag)

zonkPat (NPlusKPat n k ty e1 e2)
  = zonkIdBndr n		`thenNF_Tc` \ new_n ->
    zonkTcTypeToType ty	`thenNF_Tc` \ new_ty ->
    zonkExpr e1		`thenNF_Tc` \ new_e1 ->
    zonkExpr e2		`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (NPlusKPat new_n k new_ty new_e1 new_e2, unitBag new_n)

zonkPat (DictPat ds ms)
  = mapNF_Tc zonkIdBndr ds    `thenNF_Tc` \ new_ds ->
    mapNF_Tc zonkIdBndr ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (DictPat new_ds new_ms,
		 listToBag new_ds `unionBags` listToBag new_ms)


zonkPats []
  = returnNF_Tc ([], emptyBag)

zonkPats (pat:pats) 
  = zonkPat pat		`thenNF_Tc` \ (pat',  ids1) ->
    zonkPats pats	`thenNF_Tc` \ (pats', ids2) ->
    returnNF_Tc (pat':pats', ids1 `unionBags` ids2)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************


\begin{code}
zonkForeignExports :: [TcForeignExportDecl] -> NF_TcM s [TypecheckedForeignDecl]
zonkForeignExports ls = mapNF_Tc zonkForeignExport ls

zonkForeignExport :: TcForeignExportDecl -> NF_TcM s (TypecheckedForeignDecl)
zonkForeignExport (ForeignDecl i imp_exp hs_ty ext_nm cconv src_loc) =
   zonkIdOcc i	`thenNF_Tc` \ i' ->
   returnNF_Tc (ForeignDecl i' imp_exp undefined ext_nm cconv src_loc)
\end{code}

\begin{code}
zonkRules :: [TcRuleDecl] -> NF_TcM s [TypecheckedRuleDecl]
zonkRules rs = mapNF_Tc zonkRule rs

zonkRule (RuleDecl name tyvars vars lhs rhs loc)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars			`thenNF_Tc` \ new_tyvars ->
    mapNF_Tc zonkIdBndr [v | RuleBndr v <- vars]	`thenNF_Tc` \ new_bndrs ->
    tcExtendGlobalValEnv new_bndrs			$
    zonkExpr lhs					`thenNF_Tc` \ new_lhs ->
    zonkExpr rhs					`thenNF_Tc` \ new_rhs ->
    returnNF_Tc (RuleDecl name new_tyvars (map RuleBndr new_bndrs) new_lhs new_rhs loc)
	-- I hate this map RuleBndr stuff

zonkRule (IfaceRuleDecl fun rule loc)
  = returnNF_Tc (IfaceRuleDecl fun rule loc)
\end{code}
