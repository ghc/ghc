%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcHsSyn]{Specialisations of the @HsSyn@ syntax for the typechecker}

This module is an extension of @HsSyn@ syntax, for use in the type
checker.

\begin{code}
module TcHsSyn (
	TcMonoBinds, TcHsBinds, TcPat,
	TcExpr, TcGRHSsAndBinds, TcGRHS, TcMatch,
	TcStmt, TcArithSeqInfo, TcRecordBinds,
	TcHsModule, TcCoreExpr, TcDictBinds,
	TcForeignExportDecl,
	
	TypecheckedHsBinds, 
	TypecheckedMonoBinds, TypecheckedPat,
	TypecheckedHsExpr, TypecheckedArithSeqInfo,
	TypecheckedStmt, TypecheckedForeignDecl,
	TypecheckedMatch, TypecheckedHsModule,
	TypecheckedGRHSsAndBinds, TypecheckedGRHS,
	TypecheckedRecordBinds, TypecheckedDictBinds,

	mkHsTyApp, mkHsDictApp,
	mkHsTyLam, mkHsDictLam,

	-- re-exported from TcEnv
	TcIdOcc(..), TcIdBndr, tcIdType, tcIdTyVars, tcInstId,

	maybeBoxedPrimType,

	zonkTopBinds, zonkBinds, zonkMonoBinds, zonkTcId,
	zonkForeignExports
  ) where

#include "HsVersions.h"

-- friends:
import HsSyn	-- oodles of it
import Id	( idType, dataConArgTys, mkIdWithNewType, Id
		)

-- others:
import Name	( NamedThing(..) )
import BasicTypes ( IfaceFlavour, Unused )
import TcEnv	( tcLookupGlobalValueMaybe, tcExtendGlobalValEnv, tcGetGlobalValEnv,
		  TcIdOcc(..), TcIdBndr, GlobalValueEnv,
		  tcIdType, tcIdTyVars, tcInstId
		)

import TcMonad
import TcType	( TcType, TcMaybe, TcTyVar, TcBox,
		  zonkTcTypeToType, zonkTcTyVarToTyVar, zonkTcType
		)
import TyCon	( isDataTyCon )
import Type	( mkTyVarTy, splitAlgTyConApp_maybe, isUnpointedType, Type )
import TyVar	( TyVar, TyVarEnv, emptyTyVarEnv, growTyVarEnvList )
import TysPrim	( voidTy )
import CoreSyn  ( GenCoreExpr )
import Unique	( Unique )		-- instances
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
type TcHsBinds s     	= HsBinds (TcBox s) (TcIdOcc s) (TcPat s)
type TcMonoBinds s	= MonoBinds (TcBox s) (TcIdOcc s) (TcPat s)
type TcDictBinds s	= TcMonoBinds s
type TcPat s	     	= OutPat (TcBox s) (TcIdOcc s)
type TcExpr s	     	= HsExpr (TcBox s) (TcIdOcc s) (TcPat s)
type TcGRHSsAndBinds s	= GRHSsAndBinds (TcBox s) (TcIdOcc s) (TcPat s)
type TcGRHS s		= GRHS (TcBox s) (TcIdOcc s) (TcPat s)
type TcMatch s		= Match (TcBox s) (TcIdOcc s) (TcPat s)
type TcStmt s		= Stmt (TcBox s) (TcIdOcc s) (TcPat s)
type TcArithSeqInfo s	= ArithSeqInfo (TcBox s) (TcIdOcc s) (TcPat s)
type TcRecordBinds s	= HsRecordBinds (TcBox s) (TcIdOcc s) (TcPat s)
type TcHsModule s	= HsModule (TcBox s) (TcIdOcc s) (TcPat s)

type TcCoreExpr s	   = GenCoreExpr (TcIdOcc s) (TcIdOcc s) (TcBox s)
type TcForeignExportDecl s = ForeignDecl (TcIdOcc s)

type TypecheckedPat		= OutPat	Unused Id
type TypecheckedMonoBinds 	= MonoBinds	Unused Id TypecheckedPat
type TypecheckedDictBinds 	= TypecheckedMonoBinds
type TypecheckedHsBinds		= HsBinds	Unused Id TypecheckedPat
type TypecheckedHsExpr		= HsExpr	Unused Id TypecheckedPat
type TypecheckedArithSeqInfo	= ArithSeqInfo	Unused Id TypecheckedPat
type TypecheckedStmt		= Stmt		Unused Id TypecheckedPat
type TypecheckedMatch		= Match		Unused Id TypecheckedPat
type TypecheckedGRHSsAndBinds	= GRHSsAndBinds Unused Id TypecheckedPat
type TypecheckedGRHS		= GRHS		Unused Id TypecheckedPat
type TypecheckedRecordBinds	= HsRecordBinds Unused Id TypecheckedPat
type TypecheckedHsModule	= HsModule	Unused Id TypecheckedPat
type TypecheckedForeignDecl     = ForeignDecl Id
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
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

Some gruesome hackery for desugaring ccalls. It's here because if we put it
in Type.lhs we get irritating loops, and it's only used by TcInstDcls.lhs and
DsCCall.lhs.

\begin{code}
maybeBoxedPrimType :: Type -> Maybe (Id, Type)
maybeBoxedPrimType ty
  = case splitAlgTyConApp_maybe ty of					-- Data type,
      Just (tycon, tys_applied, [data_con]) | isDataTyCon tycon 	-- with exactly one constructor
        -> case (dataConArgTys data_con tys_applied) of
	     [data_con_arg_ty]			    	-- Applied to exactly one type,
	        | isUnpointedType data_con_arg_ty 	-- which is primitive
	        -> Just (data_con, data_con_arg_ty)
	     other_cases -> Nothing
      other_cases -> Nothing
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

@zonkTcId@ just works on TcIdOccs.  It's used when zonking Method insts.

\begin{code}
zonkTcId :: TcIdOcc s -> NF_TcM s (TcIdOcc s)
zonkTcId tc_id@(RealId id) = returnNF_Tc tc_id
zonkTcId (TcId id)
  = zonkTcType (idType id)    `thenNF_Tc` \ ty' ->
    returnNF_Tc (TcId (mkIdWithNewType id ty'))
\end{code}

This zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void
 c) convert each TcIdBndr to an Id by zonking its type

We pass an environment around so that

 a) we know which TyVars are unbound
 b) we maintain sharing; eg an Id is zonked at its binding site and they
    all occurrences of that Id point to the common zonked copy

Actually, since this is all in the Tc monad, it's convenient to keep the
mapping from TcIds to Ids in the GVE of the Tc monad.   (Those TcIds
were previously in the LVE of the Tc monad.)

It's all pretty boring stuff, because HsSyn is such a large type, and 
the environment manipulation is tiresome.

\begin{code}
extend_te te tyvars = growTyVarEnvList te [(tyvar, mkTyVarTy tyvar) | tyvar <- tyvars]

zonkIdBndr :: TyVarEnv Type -> TcIdOcc s -> NF_TcM s Id
zonkIdBndr te (RealId id) = returnNF_Tc id
zonkIdBndr te (TcId id)
  = zonkTcTypeToType te (idType id)	`thenNF_Tc` \ ty' ->
    returnNF_Tc (mkIdWithNewType id ty')


zonkIdOcc :: TcIdOcc s -> NF_TcM s Id
zonkIdOcc (RealId id) = returnNF_Tc id
zonkIdOcc (TcId id)   
  = tcLookupGlobalValueMaybe (getName id)	`thenNF_Tc` \ maybe_id' ->
    let
	new_id = case maybe_id' of
		    Just id' -> id'
		    Nothing  -> pprTrace "zonkIdOcc: " (ppr id) $
				    mkIdWithNewType id voidTy
    in
    returnNF_Tc new_id
\end{code}


\begin{code}
zonkTopBinds :: TcMonoBinds s -> NF_TcM s (TypecheckedMonoBinds, GlobalValueEnv)
zonkTopBinds binds	-- Top level is implicitly recursive
  = fixNF_Tc (\ ~(_, new_ids) ->
	tcExtendGlobalValEnv (bagToList new_ids)	$
	zonkMonoBinds emptyTyVarEnv binds		`thenNF_Tc` \ (binds', new_ids) ->
	tcGetGlobalValEnv				`thenNF_Tc` \ env ->
	returnNF_Tc ((binds', env), new_ids)
    )					`thenNF_Tc` \ (stuff, _) ->
    returnNF_Tc stuff


zonkBinds :: TyVarEnv Type
	  -> TcHsBinds s 
	  -> NF_TcM s (TypecheckedHsBinds, TcEnv s)

zonkBinds te binds 
  = go binds (\ binds' -> tcGetEnv `thenNF_Tc` \ env -> returnNF_Tc (binds', env))
  where
    -- go :: TcHsBinds s -> (TypecheckedHsBinds -> NF_TcM s (TypecheckedHsBinds, TcEnv s)) 
    --		         -> NF_TcM s (TypecheckedHsBinds, TcEnv s)
    go (ThenBinds b1 b2) thing_inside = go b1 	$ \ b1' -> 
					go b2	$ \ b2' ->
					thing_inside (b1' `ThenBinds` b2')

    go EmptyBinds thing_inside = thing_inside EmptyBinds

    go (MonoBind bind sigs is_rec) thing_inside
	  = ASSERT( null sigs )
	    fixNF_Tc (\ ~(_, new_ids) ->
		tcExtendGlobalValEnv (bagToList new_ids)	$
		zonkMonoBinds te bind				`thenNF_Tc` \ (new_bind, new_ids) ->
		thing_inside (MonoBind new_bind [] is_rec)	`thenNF_Tc` \ stuff ->
		returnNF_Tc (stuff, new_ids)
	    )						`thenNF_Tc` \ (stuff, _) ->
	   returnNF_Tc stuff
\end{code}

\begin{code}
-------------------------------------------------------------------------
zonkMonoBinds :: TyVarEnv Type
	      -> TcMonoBinds s 
	      -> NF_TcM s (TypecheckedMonoBinds, Bag Id)

zonkMonoBinds te EmptyMonoBinds = returnNF_Tc (EmptyMonoBinds, emptyBag)

zonkMonoBinds te (AndMonoBinds mbinds1 mbinds2)
  = zonkMonoBinds te mbinds1		`thenNF_Tc` \ (b1', ids1) ->
    zonkMonoBinds te mbinds2		`thenNF_Tc` \ (b2', ids2) ->
    returnNF_Tc (b1' `AndMonoBinds` b2', ids1 `unionBags` ids2)

zonkMonoBinds te (PatMonoBind pat grhss_w_binds locn)
  = zonkPat te pat	   			`thenNF_Tc` \ (new_pat, ids) ->
    zonkGRHSsAndBinds te grhss_w_binds		`thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (PatMonoBind new_pat new_grhss_w_binds locn, ids)

zonkMonoBinds te (VarMonoBind var expr)
  = zonkIdBndr te var    	`thenNF_Tc` \ new_var ->
    zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (VarMonoBind new_var new_expr, unitBag new_var)

zonkMonoBinds te (CoreMonoBind var core_expr)
  = zonkIdBndr te var    	`thenNF_Tc` \ new_var ->
    returnNF_Tc (CoreMonoBind new_var core_expr, unitBag new_var)

zonkMonoBinds te (FunMonoBind var inf ms locn)
  = zonkIdBndr te var			`thenNF_Tc` \ new_var ->
    mapNF_Tc (zonkMatch te) ms		`thenNF_Tc` \ new_ms ->
    returnNF_Tc (FunMonoBind new_var inf new_ms locn, unitBag new_var)


zonkMonoBinds te (AbsBinds tyvars dicts exports val_bind)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    let
	new_te = extend_te te new_tyvars
    in
    mapNF_Tc (zonkIdBndr new_te) dicts		`thenNF_Tc` \ new_dicts ->

    tcExtendGlobalValEnv new_dicts			$
    fixNF_Tc (\ ~(_, _, val_bind_ids) ->
	tcExtendGlobalValEnv (bagToList val_bind_ids)		$
	zonkMonoBinds new_te val_bind 		`thenNF_Tc` \ (new_val_bind, val_bind_ids) ->
        mapNF_Tc (zonkExport new_te) exports	`thenNF_Tc` \ new_exports ->
	returnNF_Tc (new_val_bind, new_exports, val_bind_ids)
    )						`thenNF_Tc ` \ (new_val_bind, new_exports, _) ->
    let
	    new_globals = listToBag [global | (_, global, local) <- new_exports]
    in
    returnNF_Tc (AbsBinds new_tyvars new_dicts new_exports new_val_bind,
		 new_globals)
  where
    zonkExport te (tyvars, global, local)
	= mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
	  zonkIdBndr te global			`thenNF_Tc` \ new_global ->
	  zonkIdOcc local			`thenNF_Tc` \ new_local -> 
	  returnNF_Tc (new_tyvars, new_global, new_local)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSsAndBinds}
%*									*
%************************************************************************

\begin{code}
zonkMatch :: TyVarEnv Type
	  -> TcMatch s -> NF_TcM s TypecheckedMatch

zonkMatch te (PatMatch pat match)
  = zonkPat te pat	    	`thenNF_Tc` \ (new_pat, ids) ->
    tcExtendGlobalValEnv (bagToList ids)	$
    zonkMatch te match  	`thenNF_Tc` \ new_match ->
    returnNF_Tc (PatMatch new_pat new_match)

zonkMatch te (GRHSMatch grhss_w_binds)
  = zonkGRHSsAndBinds te grhss_w_binds `thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (GRHSMatch new_grhss_w_binds)

zonkMatch te (SimpleMatch expr)
  = zonkExpr te expr   `thenNF_Tc` \ new_expr ->
    returnNF_Tc (SimpleMatch new_expr)

-------------------------------------------------------------------------
zonkGRHSsAndBinds :: TyVarEnv Type
	          -> TcGRHSsAndBinds s
		  -> NF_TcM s TypecheckedGRHSsAndBinds

zonkGRHSsAndBinds te (GRHSsAndBindsOut grhss binds ty)
  = zonkBinds te binds   		`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env $
    let
	zonk_grhs (GRHS guard expr locn)
	  = zonkStmts te guard  `thenNF_Tc` \ (new_guard, new_env) ->
	    tcSetEnv new_env $
	    zonkExpr te expr	`thenNF_Tc` \ new_expr  ->
	    returnNF_Tc (GRHS new_guard new_expr locn)
    in
    mapNF_Tc zonk_grhs grhss 	`thenNF_Tc` \ new_grhss ->
    zonkTcTypeToType te ty 	`thenNF_Tc` \ new_ty ->
    returnNF_Tc (GRHSsAndBindsOut new_grhss new_binds new_ty)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

\begin{code}
zonkExpr :: TyVarEnv Type
	 -> TcExpr s -> NF_TcM s TypecheckedHsExpr

zonkExpr te (HsVar id)
  = zonkIdOcc id	`thenNF_Tc` \ id' ->
    returnNF_Tc (HsVar id')

zonkExpr te (HsLit _) = panic "zonkExpr te:HsLit"

zonkExpr te (HsLitOut lit ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (HsLitOut lit new_ty)

zonkExpr te (HsLam match)
  = zonkMatch te match	`thenNF_Tc` \ new_match ->
    returnNF_Tc (HsLam new_match)

zonkExpr te (HsApp e1 e2)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (HsApp new_e1 new_e2)

zonkExpr te (OpApp e1 op fixity e2)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te op	`thenNF_Tc` \ new_op ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (OpApp new_e1 new_op fixity new_e2)

zonkExpr te (NegApp _ _) = panic "zonkExpr te:NegApp"
zonkExpr te (HsPar _)    = panic "zonkExpr te:HsPar"

zonkExpr te (SectionL expr op)
  = zonkExpr te expr	`thenNF_Tc` \ new_expr ->
    zonkExpr te op		`thenNF_Tc` \ new_op ->
    returnNF_Tc (SectionL new_expr new_op)

zonkExpr te (SectionR op expr)
  = zonkExpr te op		`thenNF_Tc` \ new_op ->
    zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SectionR new_op new_expr)

zonkExpr te (HsCase expr ms src_loc)
  = zonkExpr te expr    	    `thenNF_Tc` \ new_expr ->
    mapNF_Tc (zonkMatch te) ms   `thenNF_Tc` \ new_ms ->
    returnNF_Tc (HsCase new_expr new_ms src_loc)

zonkExpr te (HsIf e1 e2 e3 src_loc)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr te e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (HsIf new_e1 new_e2 new_e3 src_loc)

zonkExpr te (HsLet binds expr)
  = zonkBinds te binds		`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env		$
    zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsLet new_binds new_expr)

zonkExpr te (HsDo _ _ _) = panic "zonkExpr te:HsDo"

zonkExpr te (HsDoOut do_or_lc stmts return_id then_id zero_id ty src_loc)
  = zonkStmts te stmts 		`thenNF_Tc` \ (new_stmts, _) ->
    zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty   ->
    zonkIdOcc return_id		`thenNF_Tc` \ new_return_id ->
    zonkIdOcc then_id		`thenNF_Tc` \ new_then_id ->
    zonkIdOcc zero_id		`thenNF_Tc` \ new_zero_id ->
    returnNF_Tc (HsDoOut do_or_lc new_stmts new_return_id new_then_id new_zero_id
			 new_ty src_loc)

zonkExpr te (ExplicitList _) = panic "zonkExpr te:ExplicitList"

zonkExpr te (ExplicitListOut ty exprs)
  = zonkTcTypeToType te ty		`thenNF_Tc` \ new_ty ->
    mapNF_Tc (zonkExpr te) exprs	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitListOut new_ty new_exprs)

zonkExpr te (ExplicitTuple exprs)
  = mapNF_Tc (zonkExpr te) exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitTuple new_exprs)

zonkExpr te (HsCon con_id tys exprs)
  = mapNF_Tc (zonkTcTypeToType te) tys	`thenNF_Tc` \ new_tys ->
    mapNF_Tc (zonkExpr te) exprs	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (HsCon con_id new_tys new_exprs)

zonkExpr te (RecordCon con_id con_expr rbinds)
  = zonkIdOcc con_id		`thenNF_Tc` \ new_con_id ->
    zonkExpr te con_expr	`thenNF_Tc` \ new_con_expr ->
    zonkRbinds te rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordCon new_con_id new_con_expr new_rbinds)

zonkExpr te (RecordUpd _ _) = panic "zonkExpr te:RecordUpd"

zonkExpr te (RecordUpdOut expr ty dicts rbinds)
  = zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkIdOcc dicts	`thenNF_Tc` \ new_dicts ->
    zonkRbinds te rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordUpdOut new_expr new_ty new_dicts new_rbinds)

zonkExpr te (ExprWithTySig _ _) = panic "zonkExpr te:ExprWithTySig"
zonkExpr te (ArithSeqIn _) = panic "zonkExpr te:ArithSeqIn"

zonkExpr te (ArithSeqOut expr info)
  = zonkExpr te expr	`thenNF_Tc` \ new_expr ->
    zonkArithSeq te info	`thenNF_Tc` \ new_info ->
    returnNF_Tc (ArithSeqOut new_expr new_info)

zonkExpr te (CCall fun args may_gc is_casm result_ty)
  = mapNF_Tc (zonkExpr te) args 	`thenNF_Tc` \ new_args ->
    zonkTcTypeToType te result_ty	`thenNF_Tc` \ new_result_ty ->
    returnNF_Tc (CCall fun new_args may_gc is_casm new_result_ty)

zonkExpr te (HsSCC label expr)
  = zonkExpr te expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsSCC label new_expr)

zonkExpr te (TyLam tyvars expr)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    let
	new_te = extend_te te new_tyvars
    in
    zonkExpr new_te expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (TyLam new_tyvars new_expr)

zonkExpr te (TyApp expr tys)
  = zonkExpr te expr    	    	`thenNF_Tc` \ new_expr ->
    mapNF_Tc (zonkTcTypeToType te) tys	`thenNF_Tc` \ new_tys ->
    returnNF_Tc (TyApp new_expr new_tys)

zonkExpr te (DictLam dicts expr)
  = mapNF_Tc (zonkIdBndr te) dicts	`thenNF_Tc` \ new_dicts ->
    tcExtendGlobalValEnv new_dicts	$
    zonkExpr te expr    	    	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictLam new_dicts new_expr)

zonkExpr te (DictApp expr dicts)
  = zonkExpr te expr    	    	`thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkIdOcc dicts	`thenNF_Tc` \ new_dicts ->
    returnNF_Tc (DictApp new_expr new_dicts)



-------------------------------------------------------------------------
zonkArithSeq :: TyVarEnv Type
	     -> TcArithSeqInfo s -> NF_TcM s TypecheckedArithSeqInfo

zonkArithSeq te (From e)
  = zonkExpr te e		`thenNF_Tc` \ new_e ->
    returnNF_Tc (From new_e)

zonkArithSeq te (FromThen e1 e2)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromThen new_e1 new_e2)

zonkArithSeq te (FromTo e1 e2)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromTo new_e1 new_e2)

zonkArithSeq te (FromThenTo e1 e2 e3)
  = zonkExpr te e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr te e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (FromThenTo new_e1 new_e2 new_e3)

-------------------------------------------------------------------------
zonkStmts :: TyVarEnv Type
	  -> [TcStmt s] -> NF_TcM s ([TypecheckedStmt], TcEnv s)

zonkStmts te [] = tcGetEnv	`thenNF_Tc` \ env ->
		  returnNF_Tc ([], env)

zonkStmts te [ReturnStmt expr]
  = zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    tcGetEnv			`thenNF_Tc` \ env ->
    returnNF_Tc ([ReturnStmt new_expr], env)

zonkStmts te (ExprStmt expr locn : stmts)
  = zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    zonkStmts te	stmts	`thenNF_Tc` \ (new_stmts, new_env) ->
    returnNF_Tc (ExprStmt new_expr locn : new_stmts, new_env)

zonkStmts te (GuardStmt expr locn : stmts)
  = zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    zonkStmts te	stmts	`thenNF_Tc` \ (new_stmts, new_env) ->
    returnNF_Tc (GuardStmt new_expr locn : new_stmts, new_env)

zonkStmts te (LetStmt binds : stmts)
  = zonkBinds te     binds	`thenNF_Tc` \ (new_binds, new_env) ->
    tcSetEnv new_env		$
    zonkStmts te stmts		`thenNF_Tc` \ (new_stmts, new_env2) ->
    returnNF_Tc (LetStmt new_binds : new_stmts, new_env2)

zonkStmts te (BindStmt pat expr locn : stmts)
  = zonkPat te pat		`thenNF_Tc` \ (new_pat, ids) ->
    zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    tcExtendGlobalValEnv (bagToList ids)	$ 
    zonkStmts te stmts		`thenNF_Tc` \ (new_stmts, new_env) ->
    returnNF_Tc (BindStmt new_pat new_expr locn : new_stmts, new_env)



-------------------------------------------------------------------------
zonkRbinds :: TyVarEnv Type
	   -> TcRecordBinds s -> NF_TcM s TypecheckedRecordBinds

zonkRbinds te rbinds
  = mapNF_Tc zonk_rbind rbinds
  where
    zonk_rbind (field, expr, pun)
      = zonkExpr te expr	`thenNF_Tc` \ new_expr ->
	zonkIdOcc field		`thenNF_Tc` \ new_field ->
	returnNF_Tc (new_field, new_expr, pun)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: TyVarEnv Type
	-> TcPat s -> NF_TcM s (TypecheckedPat, Bag Id)

zonkPat te (WildPat ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (WildPat new_ty, emptyBag)

zonkPat te (VarPat v)
  = zonkIdBndr te v	    `thenNF_Tc` \ new_v ->
    returnNF_Tc (VarPat new_v, unitBag new_v)

zonkPat te (LazyPat pat)
  = zonkPat te pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (LazyPat new_pat, ids)

zonkPat te (AsPat n pat)
  = zonkIdBndr te n	    `thenNF_Tc` \ new_n ->
    zonkPat te pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (AsPat new_n new_pat, new_n `consBag` ids)

zonkPat te (ConPat n ty pats)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    zonkPats te pats		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ConPat n new_ty new_pats, ids)

zonkPat te (ConOpPat pat1 op pat2 ty)
  = zonkPat te pat1	    `thenNF_Tc` \ (new_pat1, ids1) ->
    zonkPat te pat2	    `thenNF_Tc` \ (new_pat2, ids2) ->
    zonkTcTypeToType te ty  `thenNF_Tc` \ new_ty ->
    returnNF_Tc (ConOpPat new_pat1 op new_pat2 new_ty, ids1 `unionBags` ids2)

zonkPat te (ListPat ty pats)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    zonkPats te pats		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ListPat new_ty new_pats, ids)

zonkPat te (TuplePat pats)
  = zonkPats te pats   		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (TuplePat new_pats, ids)

zonkPat te (RecPat n ty rpats)
  = zonkTcTypeToType te ty		`thenNF_Tc` \ new_ty ->
    mapAndUnzipNF_Tc zonk_rpat rpats	`thenNF_Tc` \ (new_rpats, ids_s) ->
    returnNF_Tc (RecPat n new_ty new_rpats, unionManyBags ids_s)
  where
    zonk_rpat (f, pat, pun)
      = zonkPat te pat	     `thenNF_Tc` \ (new_pat, ids) ->
	returnNF_Tc ((f, new_pat, pun), ids)

zonkPat te (LitPat lit ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (LitPat lit new_ty, emptyBag)

zonkPat te (NPat lit ty expr)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty   ->
    zonkExpr te expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (NPat lit new_ty new_expr, emptyBag)

zonkPat te (NPlusKPat n k ty e1 e2)
  = zonkIdBndr te n		`thenNF_Tc` \ new_n ->
    zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    zonkExpr te e1		`thenNF_Tc` \ new_e1 ->
    zonkExpr te e2		`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (NPlusKPat new_n k new_ty new_e1 new_e2, unitBag new_n)

zonkPat te (DictPat ds ms)
  = mapNF_Tc (zonkIdBndr te) ds    `thenNF_Tc` \ new_ds ->
    mapNF_Tc (zonkIdBndr te) ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (DictPat new_ds new_ms, 
		 listToBag new_ds `unionBags` listToBag new_ms)


zonkPats te [] 
  = returnNF_Tc ([], emptyBag)
zonkPats te (pat:pats) 
  = zonkPat te pat	`thenNF_Tc` \ (pat', ids1) ->
    zonkPats te pats	`thenNF_Tc` \ (pats', ids2) ->
    returnNF_Tc (pat':pats', ids1 `unionBags` ids2)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Foreign]{Foreign exports}
%*									*
%************************************************************************


\begin{code}
zonkForeignExports :: [TcForeignExportDecl s] -> NF_TcM s [TypecheckedForeignDecl]
zonkForeignExports ls = mapNF_Tc zonkForeignExport ls

zonkForeignExport :: TcForeignExportDecl s -> NF_TcM s (TypecheckedForeignDecl)
zonkForeignExport (ForeignDecl i imp_exp hs_ty ext_nm cconv src_loc) =
   zonkIdOcc i	`thenNF_Tc` \ i' ->
   returnNF_Tc (ForeignDecl i' imp_exp undefined ext_nm cconv src_loc)

\end{code}
