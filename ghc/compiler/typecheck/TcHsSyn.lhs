%
% (c) The AQUA Project, Glasgow University, 1996
%
\section[TcHsSyn]{Specialisations of the @HsSyn@ syntax for the typechecker}

This module is an extension of @HsSyn@ syntax, for use in the type
checker.

\begin{code}
module TcHsSyn (
	TcIdBndr(..), TcIdOcc(..),
	
	TcMonoBinds(..), TcHsBinds(..), TcBind(..), TcPat(..),
	TcExpr(..), TcGRHSsAndBinds(..), TcGRHS(..), TcMatch(..),
	TcQual(..), TcStmt(..), TcArithSeqInfo(..), TcRecordBinds(..),
	TcHsModule(..),
	
	TypecheckedHsBinds(..), TypecheckedBind(..),
	TypecheckedMonoBinds(..), TypecheckedPat(..),
	TypecheckedHsExpr(..), TypecheckedArithSeqInfo(..),
	TypecheckedQual(..), TypecheckedStmt(..),
	TypecheckedMatch(..), TypecheckedHsModule(..),
	TypecheckedGRHSsAndBinds(..), TypecheckedGRHS(..),
	TypecheckedRecordBinds(..),

	mkHsTyApp, mkHsDictApp,
	mkHsTyLam, mkHsDictLam,
	tcIdType,

	zonkBinds,
	zonkDictBinds
  ) where

import Ubiq{-uitous-}

-- friends:
import HsSyn	-- oodles of it
import Id	( GenId(..), IdDetails, PragmaInfo,	-- Can meddle modestly with Ids
		  DictVar(..), idType,
		  IdEnv(..), growIdEnvList, lookupIdEnv
		)

-- others:
import Name	( Name{--O only-} )
import TcMonad	hiding ( rnMtoTcM )
import TcType	( TcType(..), TcMaybe, TcTyVar(..),
		  zonkTcTypeToType, zonkTcTyVarToTyVar,
		  tcInstType
		)
import Usage	( UVar(..) )
import Util	( zipEqual, panic, pprPanic, pprTrace )

import PprType  ( GenType, GenTyVar ) 	-- instances
import Type	( mkTyVarTy )
import TyVar	( GenTyVar {- instances -},
		  TyVarEnv(..), growTyVarEnvList )		-- instances
import TysWiredIn	( voidTy )
import Unique	( Unique )		-- instances
import UniqFM
import PprStyle
import Pretty
\end{code}


Type definitions
~~~~~~~~~~~~~~~~

The @Tc...@ datatypes are the ones that apply {\em during} type checking.
All the types in @Tc...@ things have mutable type-variables in them for
unification.

At the end of type checking we zonk everything to @Typechecked...@ datatypes,
which have immutable type variables in them.

\begin{code}
type TcIdBndr s = GenId  (TcType s)	-- Binders are all TcTypes
data TcIdOcc  s = TcId   (TcIdBndr s)	-- Bindees may be either
		| RealId Id

type TcHsBinds s     	= HsBinds (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcBind s	     	= Bind (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcMonoBinds s	= MonoBinds (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcPat s	     	= OutPat (TcTyVar s) UVar (TcIdOcc s)
type TcExpr s	     	= HsExpr (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcGRHSsAndBinds s	= GRHSsAndBinds (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcGRHS s		= GRHS (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcMatch s		= Match (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcQual s		= Qual (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcStmt s		= Stmt (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcArithSeqInfo s	= ArithSeqInfo (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcRecordBinds s	= HsRecordBinds (TcTyVar s) UVar (TcIdOcc s) (TcPat s)
type TcHsModule s	= HsModule (TcTyVar s) UVar (TcIdOcc s) (TcPat s)

type TypecheckedPat		= OutPat	TyVar UVar Id
type TypecheckedMonoBinds 	= MonoBinds	TyVar UVar Id TypecheckedPat
type TypecheckedHsBinds		= HsBinds	TyVar UVar Id TypecheckedPat
type TypecheckedBind		= Bind		TyVar UVar Id TypecheckedPat
type TypecheckedHsExpr		= HsExpr	TyVar UVar Id TypecheckedPat
type TypecheckedArithSeqInfo	= ArithSeqInfo	TyVar UVar Id TypecheckedPat
type TypecheckedQual		= Qual		TyVar UVar Id TypecheckedPat
type TypecheckedStmt		= Stmt		TyVar UVar Id TypecheckedPat
type TypecheckedMatch		= Match		TyVar UVar Id TypecheckedPat
type TypecheckedGRHSsAndBinds	= GRHSsAndBinds TyVar UVar Id TypecheckedPat
type TypecheckedGRHS		= GRHS		TyVar UVar Id TypecheckedPat
type TypecheckedRecordBinds	= HsRecordBinds TyVar UVar Id TypecheckedPat
type TypecheckedHsModule	= HsModule	TyVar UVar Id TypecheckedPat
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

tcIdType :: TcIdOcc s -> TcType s
tcIdType (TcId   id) = idType id
tcIdType (RealId id) = pprPanic "tcIdType:" (ppr PprDebug id)
\end{code}



\begin{code}
instance Eq (TcIdOcc s) where
  (TcId id1)   == (TcId id2)   = id1 == id2
  (RealId id1) == (RealId id2) = id1 == id2
  _	       == _	       = False

instance Outputable (TcIdOcc s) where
  ppr sty (TcId id)   = ppr sty id
  ppr sty (RealId id) = ppr sty id

instance NamedThing (TcIdOcc s) where
  getName (TcId id)   = getName id
  getName (RealId id) = getName id
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

This zonking pass runs over the bindings

 a) to convert TcTyVars to TyVars etc, dereferencing any bindings etc
 b) convert unbound TcTyVar to Void

We pass an environment around so that
 a) we know which TyVars are unbound
 b) we maintain sharing; eg an Id is zonked at its binding site and they
    all occurrences of that Id point to the common zonked copy

It's all pretty boring stuff, because HsSyn is such a large type, and 
the environment manipulation is tiresome.


\begin{code}
zonkIdBndr :: TyVarEnv Type -> TcIdOcc s -> NF_TcM s Id
zonkIdBndr te (TcId (Id u n ty details prags info))
  = zonkTcTypeToType te ty	`thenNF_Tc` \ ty' ->
    returnNF_Tc (Id u n ty' details prags info)

zonkIdBndr te (RealId id) = returnNF_Tc id

zonkIdOcc :: IdEnv Id -> TcIdOcc s -> Id
zonkIdOcc ve (RealId id) = id
zonkIdOcc ve (TcId id)   = case (lookupIdEnv ve id) of
				Just id' -> id'
				Nothing  -> pprTrace "zonkIdOcc: " (ppr PprDebug id) $
					    Id u n voidTy details prags info
				         where
					    Id u n _ details prags info = id

extend_ve ve ids    = growIdEnvList ve [(id,id) | id <- ids]
extend_te te tyvars = growTyVarEnvList te [(tyvar, mkTyVarTy tyvar) | tyvar <- tyvars]
\end{code}

\begin{code}
	-- Implicitly mutually recursive, which is overkill,
	-- but it means that later ones see earlier ones
zonkDictBinds te ve dbs 
  = fixNF_Tc (\ ~(_,new_ve) ->
	zonkDictBindsLocal te new_ve dbs	`thenNF_Tc` \ (new_binds, dict_ids) ->
        returnNF_Tc (new_binds, extend_ve ve dict_ids)
    )

	-- The ..Local version assumes the caller has set up
	-- a ve that contains all the things bound here
zonkDictBindsLocal te ve [] = returnNF_Tc ([], [])

zonkDictBindsLocal te ve ((dict,rhs) : binds)
  = zonkIdBndr te dict			`thenNF_Tc` \ new_dict ->
    zonkExpr te ve rhs			`thenNF_Tc` \ new_rhs ->
    zonkDictBindsLocal te ve binds	`thenNF_Tc` \ (new_binds, dict_ids) ->
    returnNF_Tc ((new_dict,new_rhs) : new_binds, 
		 new_dict:dict_ids)
\end{code}

\begin{code}
zonkBinds :: TyVarEnv Type -> IdEnv Id 
	  -> TcHsBinds s -> NF_TcM s (TypecheckedHsBinds, IdEnv Id)

zonkBinds te ve EmptyBinds = returnNF_Tc (EmptyBinds, ve)

zonkBinds te ve (ThenBinds binds1 binds2)
  = zonkBinds te ve binds1   `thenNF_Tc` \ (new_binds1, ve1) ->
    zonkBinds te ve1 binds2  `thenNF_Tc` \ (new_binds2, ve2) ->
    returnNF_Tc (ThenBinds new_binds1 new_binds2, ve2)

zonkBinds te ve (SingleBind bind)
  = fixNF_Tc (\ ~(_,new_ve) ->
	zonkBind te new_ve bind  `thenNF_Tc` \ (new_bind, new_ids) ->
	returnNF_Tc (SingleBind new_bind, extend_ve ve new_ids)
    )

zonkBinds te ve (AbsBinds tyvars dicts locprs dict_binds val_bind)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    let
	new_te = extend_te te new_tyvars
    in
    mapNF_Tc (zonkIdBndr new_te) dicts		`thenNF_Tc` \ new_dicts ->
    mapNF_Tc (zonkIdBndr new_te) globals	`thenNF_Tc` \ new_globals ->
    let
	ve1 = extend_ve ve  new_globals
        ve2 = extend_ve ve1 new_dicts
    in
    fixNF_Tc (\ ~(_, ve3) ->
	zonkDictBindsLocal new_te ve3 dict_binds  `thenNF_Tc` \ (new_dict_binds, ds) ->
	zonkBind new_te ve3 val_bind		  `thenNF_Tc` \ (new_val_bind, ls) ->
	let
	    new_locprs = zipEqual "zonkBinds" (map (zonkIdOcc ve3) locals) new_globals
        in
        returnNF_Tc (AbsBinds new_tyvars new_dicts new_locprs new_dict_binds new_val_bind,
		     extend_ve ve2 (ds++ls))
    )						`thenNF_Tc` \ (binds, _) ->
    returnNF_Tc (binds, ve1)	-- Yes, the "ve1" is right (SLPJ)
  where
    (locals, globals) = unzip locprs
\end{code}

\begin{code}
-------------------------------------------------------------------------
zonkBind :: TyVarEnv Type -> IdEnv Id 
	 -> TcBind s -> NF_TcM s (TypecheckedBind, [Id])

zonkBind te ve EmptyBind = returnNF_Tc (EmptyBind, [])

zonkBind te ve (NonRecBind mbinds)
  = zonkMonoBinds te ve mbinds	`thenNF_Tc` \ (new_mbinds, new_ids) ->
    returnNF_Tc (NonRecBind new_mbinds, new_ids)

zonkBind te ve (RecBind mbinds)
  = zonkMonoBinds te ve mbinds	`thenNF_Tc` \ (new_mbinds, new_ids) ->
    returnNF_Tc (RecBind new_mbinds, new_ids)

-------------------------------------------------------------------------
zonkMonoBinds :: TyVarEnv Type -> IdEnv Id 
	      -> TcMonoBinds s -> NF_TcM s (TypecheckedMonoBinds, [Id])

zonkMonoBinds te ve EmptyMonoBinds = returnNF_Tc (EmptyMonoBinds, [])

zonkMonoBinds te ve (AndMonoBinds mbinds1 mbinds2)
  = zonkMonoBinds te ve mbinds1  `thenNF_Tc` \ (new_mbinds1, ids1) ->
    zonkMonoBinds te ve mbinds2  `thenNF_Tc` \ (new_mbinds2, ids2) ->
    returnNF_Tc (AndMonoBinds new_mbinds1 new_mbinds2, ids1 ++ ids2)

zonkMonoBinds te ve (PatMonoBind pat grhss_w_binds locn)
  = zonkPat te ve pat	   			`thenNF_Tc` \ (new_pat, ids) ->
    zonkGRHSsAndBinds te ve grhss_w_binds	`thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (PatMonoBind new_pat new_grhss_w_binds locn, ids)

zonkMonoBinds te ve (VarMonoBind var expr)
  = zonkIdBndr te var    	`thenNF_Tc` \ new_var ->
    zonkExpr te ve expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (VarMonoBind new_var new_expr, [new_var])

zonkMonoBinds te ve (FunMonoBind var inf ms locn)
  = zonkIdBndr te var			`thenNF_Tc` \ new_var ->
    mapNF_Tc (zonkMatch te ve) ms	`thenNF_Tc` \ new_ms ->
    returnNF_Tc (FunMonoBind new_var inf new_ms locn, [new_var])
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSsAndBinds}
%*									*
%************************************************************************

\begin{code}
zonkMatch :: TyVarEnv Type -> IdEnv Id 
	  -> TcMatch s -> NF_TcM s TypecheckedMatch

zonkMatch te ve (PatMatch pat match)
  = zonkPat te ve pat	    	`thenNF_Tc` \ (new_pat, ids) ->
    let
	new_ve = extend_ve ve ids
    in
    zonkMatch te new_ve match  	`thenNF_Tc` \ new_match ->
    returnNF_Tc (PatMatch new_pat new_match)

zonkMatch te ve (GRHSMatch grhss_w_binds)
  = zonkGRHSsAndBinds te ve grhss_w_binds `thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (GRHSMatch new_grhss_w_binds)

zonkMatch te ve (SimpleMatch expr)
  = zonkExpr te ve expr   `thenNF_Tc` \ new_expr ->
    returnNF_Tc (SimpleMatch new_expr)

-------------------------------------------------------------------------
zonkGRHSsAndBinds :: TyVarEnv Type -> IdEnv Id 
	          -> TcGRHSsAndBinds s
		  -> NF_TcM s TypecheckedGRHSsAndBinds

zonkGRHSsAndBinds te ve (GRHSsAndBindsOut grhss binds ty)
  = zonkBinds te ve binds   		`thenNF_Tc` \ (new_binds, new_ve) ->
    let
	zonk_grhs (GRHS guard expr locn)
	  = zonkExpr te new_ve guard  `thenNF_Tc` \ new_guard ->
	    zonkExpr te new_ve expr   `thenNF_Tc` \ new_expr  ->
	    returnNF_Tc (GRHS new_guard new_expr locn)

        zonk_grhs (OtherwiseGRHS expr locn)
          = zonkExpr te new_ve expr   `thenNF_Tc` \ new_expr  ->
	    returnNF_Tc (OtherwiseGRHS new_expr locn)
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
zonkExpr :: TyVarEnv Type -> IdEnv Id 
	 -> TcExpr s -> NF_TcM s TypecheckedHsExpr

zonkExpr te ve (HsVar name)
  = returnNF_Tc (HsVar (zonkIdOcc ve name))

zonkExpr te ve (HsLit _) = panic "zonkExpr te ve:HsLit"

zonkExpr te ve (HsLitOut lit ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (HsLitOut lit new_ty)

zonkExpr te ve (HsLam match)
  = zonkMatch te ve match	`thenNF_Tc` \ new_match ->
    returnNF_Tc (HsLam new_match)

zonkExpr te ve (HsApp e1 e2)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (HsApp new_e1 new_e2)

zonkExpr te ve (OpApp e1 op e2)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve op	`thenNF_Tc` \ new_op ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (OpApp new_e1 new_op new_e2)

zonkExpr te ve (NegApp _ _) = panic "zonkExpr te ve:NegApp"
zonkExpr te ve (HsPar _)    = panic "zonkExpr te ve:HsPar"

zonkExpr te ve (SectionL expr op)
  = zonkExpr te ve expr	`thenNF_Tc` \ new_expr ->
    zonkExpr te ve op		`thenNF_Tc` \ new_op ->
    returnNF_Tc (SectionL new_expr new_op)

zonkExpr te ve (SectionR op expr)
  = zonkExpr te ve op		`thenNF_Tc` \ new_op ->
    zonkExpr te ve expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SectionR new_op new_expr)

zonkExpr te ve (HsCase expr ms src_loc)
  = zonkExpr te ve expr    	    `thenNF_Tc` \ new_expr ->
    mapNF_Tc (zonkMatch te ve) ms   `thenNF_Tc` \ new_ms ->
    returnNF_Tc (HsCase new_expr new_ms src_loc)

zonkExpr te ve (HsIf e1 e2 e3 src_loc)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr te ve e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (HsIf new_e1 new_e2 new_e3 src_loc)

zonkExpr te ve (HsLet binds expr)
  = zonkBinds te ve binds	`thenNF_Tc` \ (new_binds, new_ve) ->
    zonkExpr te new_ve expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsLet new_binds new_expr)

zonkExpr te ve (HsDo _ _) = panic "zonkExpr te ve:HsDo"

zonkExpr te ve (HsDoOut stmts m_id mz_id src_loc)
  = zonkStmts te ve stmts 	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (HsDoOut new_stmts m_new mz_new src_loc)
  where
    m_new  = zonkIdOcc ve m_id
    mz_new = zonkIdOcc ve mz_id

zonkExpr te ve (ListComp expr quals)
  = zonkQuals te ve quals	`thenNF_Tc` \ (new_quals, new_ve) ->
    zonkExpr te new_ve expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (ListComp new_expr new_quals)

zonkExpr te ve (ExplicitList _) = panic "zonkExpr te ve:ExplicitList"

zonkExpr te ve (ExplicitListOut ty exprs)
  = zonkTcTypeToType te ty		`thenNF_Tc` \ new_ty ->
    mapNF_Tc (zonkExpr te ve) exprs	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitListOut new_ty new_exprs)

zonkExpr te ve (ExplicitTuple exprs)
  = mapNF_Tc (zonkExpr te ve) exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitTuple new_exprs)

zonkExpr te ve (RecordCon con rbinds)
  = zonkExpr te ve con		`thenNF_Tc` \ new_con ->
    zonkRbinds te ve rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordCon new_con new_rbinds)

zonkExpr te ve (RecordUpd _ _) = panic "zonkExpr te ve:RecordUpd"

zonkExpr te ve (RecordUpdOut expr dicts rbinds)
  = zonkExpr te ve expr		`thenNF_Tc` \ new_expr ->
    zonkRbinds te ve rbinds	`thenNF_Tc` \ new_rbinds ->
    returnNF_Tc (RecordUpdOut new_expr new_dicts new_rbinds)
  where
    new_dicts = map (zonkIdOcc ve) dicts

zonkExpr te ve (ExprWithTySig _ _) = panic "zonkExpr te ve:ExprWithTySig"
zonkExpr te ve (ArithSeqIn _) = panic "zonkExpr te ve:ArithSeqIn"

zonkExpr te ve (ArithSeqOut expr info)
  = zonkExpr te ve expr	`thenNF_Tc` \ new_expr ->
    zonkArithSeq te ve info	`thenNF_Tc` \ new_info ->
    returnNF_Tc (ArithSeqOut new_expr new_info)

zonkExpr te ve (CCall fun args may_gc is_casm result_ty)
  = mapNF_Tc (zonkExpr te ve) args 	`thenNF_Tc` \ new_args ->
    zonkTcTypeToType te result_ty	`thenNF_Tc` \ new_result_ty ->
    returnNF_Tc (CCall fun new_args may_gc is_casm new_result_ty)

zonkExpr te ve (HsSCC label expr)
  = zonkExpr te ve expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsSCC label new_expr)

zonkExpr te ve (TyLam tyvars expr)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    let
	new_te = extend_te te new_tyvars
    in
    zonkExpr new_te ve expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (TyLam new_tyvars new_expr)

zonkExpr te ve (TyApp expr tys)
  = zonkExpr te ve expr    	    	`thenNF_Tc` \ new_expr ->
    mapNF_Tc (zonkTcTypeToType te) tys	`thenNF_Tc` \ new_tys ->
    returnNF_Tc (TyApp new_expr new_tys)

zonkExpr te ve (DictLam dicts expr)
  = mapNF_Tc (zonkIdBndr te) dicts	`thenNF_Tc` \ new_dicts ->
    let
	new_ve = extend_ve ve new_dicts
    in
    zonkExpr te new_ve expr    	    	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictLam new_dicts new_expr)

zonkExpr te ve (DictApp expr dicts)
  = zonkExpr te ve expr    	    	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictApp new_expr new_dicts)
  where
    new_dicts = map (zonkIdOcc ve) dicts

zonkExpr te ve (ClassDictLam dicts methods expr)
  = zonkExpr te ve expr    	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (ClassDictLam new_dicts new_methods new_expr)
  where
    new_dicts   = map (zonkIdOcc ve) dicts
    new_methods = map (zonkIdOcc ve) methods
    

zonkExpr te ve (Dictionary dicts methods)
  = returnNF_Tc (Dictionary new_dicts new_methods)
  where
    new_dicts   = map (zonkIdOcc ve) dicts
    new_methods = map (zonkIdOcc ve) methods

zonkExpr te ve (SingleDict name)
  = returnNF_Tc (SingleDict (zonkIdOcc ve name))

zonkExpr te ve (HsCon con tys vargs)
  = mapNF_Tc (zonkTcTypeToType te) tys	`thenNF_Tc` \ new_tys   ->
    mapNF_Tc (zonkExpr te ve) vargs	`thenNF_Tc` \ new_vargs ->
    returnNF_Tc (HsCon con new_tys new_vargs)

-------------------------------------------------------------------------
zonkArithSeq :: TyVarEnv Type -> IdEnv Id 
	     -> TcArithSeqInfo s -> NF_TcM s TypecheckedArithSeqInfo

zonkArithSeq te ve (From e)
  = zonkExpr te ve e		`thenNF_Tc` \ new_e ->
    returnNF_Tc (From new_e)

zonkArithSeq te ve (FromThen e1 e2)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromThen new_e1 new_e2)

zonkArithSeq te ve (FromTo e1 e2)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromTo new_e1 new_e2)

zonkArithSeq te ve (FromThenTo e1 e2 e3)
  = zonkExpr te ve e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr te ve e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr te ve e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (FromThenTo new_e1 new_e2 new_e3)

-------------------------------------------------------------------------
zonkQuals :: TyVarEnv Type -> IdEnv Id 
	  -> [TcQual s] -> NF_TcM s ([TypecheckedQual], IdEnv Id)

zonkQuals te ve [] 
  = returnNF_Tc ([], ve)

zonkQuals te ve (GeneratorQual pat expr : quals)
  = zonkPat te ve pat	`thenNF_Tc` \ (new_pat, ids) ->
    zonkExpr te ve expr	`thenNF_Tc` \ new_expr ->
    let
	new_ve = extend_ve ve ids
    in
    zonkQuals te new_ve quals	`thenNF_Tc` \ (new_quals, final_ve) ->
    returnNF_Tc (GeneratorQual new_pat new_expr : new_quals, final_ve)

zonkQuals te ve (FilterQual expr : quals)
  = zonkExpr te ve expr    	`thenNF_Tc` \ new_expr ->
    zonkQuals te ve quals	`thenNF_Tc` \ (new_quals, final_ve) ->
    returnNF_Tc (FilterQual new_expr : new_quals, final_ve)

zonkQuals te ve (LetQual binds : quals)
  = zonkBinds te ve binds	`thenNF_Tc` \ (new_binds, new_ve) ->
    zonkQuals te new_ve quals	`thenNF_Tc` \ (new_quals, final_ve) ->
    returnNF_Tc (LetQual new_binds : new_quals, final_ve)

-------------------------------------------------------------------------
zonkStmts :: TyVarEnv Type -> IdEnv Id 
	  -> [TcStmt s] -> NF_TcM s [TypecheckedStmt]

zonkStmts te ve []
  = returnNF_Tc []

zonkStmts te ve (BindStmt pat expr src_loc : stmts)
  = zonkPat te ve pat    `thenNF_Tc` \ (new_pat, ids) ->
    zonkExpr te ve expr   `thenNF_Tc` \ new_expr ->
    let
	new_ve = extend_ve ve ids
    in
    zonkStmts te new_ve stmts	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (BindStmt new_pat new_expr src_loc : new_stmts)

zonkStmts te ve (ExprStmt expr src_loc : stmts)
  = zonkExpr te ve expr		`thenNF_Tc` \ new_expr ->
    zonkStmts te ve stmts	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (ExprStmt new_expr src_loc : new_stmts)

zonkStmts te ve (LetStmt binds : stmts)
  = zonkBinds te ve binds	`thenNF_Tc` \ (new_binds, new_ve) ->
    zonkStmts te new_ve stmts	`thenNF_Tc` \ new_stmts ->
    returnNF_Tc (LetStmt new_binds : new_stmts)

-------------------------------------------------------------------------
zonkRbinds :: TyVarEnv Type -> IdEnv Id 
	   -> TcRecordBinds s -> NF_TcM s TypecheckedRecordBinds

zonkRbinds te ve rbinds
  = mapNF_Tc zonk_rbind rbinds
  where
    zonk_rbind (field, expr, pun)
      = zonkExpr te ve expr	`thenNF_Tc` \ new_expr ->
	returnNF_Tc (zonkIdOcc ve field, new_expr, pun)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: TyVarEnv Type -> IdEnv Id 
	-> TcPat s -> NF_TcM s (TypecheckedPat, [Id])

zonkPat te ve (WildPat ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (WildPat new_ty, [])

zonkPat te ve (VarPat v)
  = zonkIdBndr te v	    `thenNF_Tc` \ new_v ->
    returnNF_Tc (VarPat new_v, [new_v])

zonkPat te ve (LazyPat pat)
  = zonkPat te ve pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (LazyPat new_pat, ids)

zonkPat te ve (AsPat n pat)
  = zonkIdBndr te n	    `thenNF_Tc` \ new_n ->
    zonkPat te ve pat	    `thenNF_Tc` \ (new_pat, ids) ->
    returnNF_Tc (AsPat new_n new_pat, new_n:ids)

zonkPat te ve (ConPat n ty pats)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    zonkPats te ve pats		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ConPat n new_ty new_pats, ids)

zonkPat te ve (ConOpPat pat1 op pat2 ty)
  = zonkPat te ve pat1	    `thenNF_Tc` \ (new_pat1, ids1) ->
    zonkPat te ve pat2	    `thenNF_Tc` \ (new_pat2, ids2) ->
    zonkTcTypeToType te ty  `thenNF_Tc` \ new_ty ->
    returnNF_Tc (ConOpPat new_pat1 op new_pat2 new_ty, ids1 ++ ids2)

zonkPat te ve (ListPat ty pats)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty ->
    zonkPats te ve pats		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (ListPat new_ty new_pats, ids)

zonkPat te ve (TuplePat pats)
  = zonkPats te ve pats   		`thenNF_Tc` \ (new_pats, ids) ->
    returnNF_Tc (TuplePat new_pats, ids)

zonkPat te ve (RecPat n ty rpats)
  = zonkTcTypeToType te ty		`thenNF_Tc` \ new_ty ->
    mapAndUnzipNF_Tc zonk_rpat rpats	`thenNF_Tc` \ (new_rpats, ids_s) ->
    returnNF_Tc (RecPat n new_ty new_rpats, concat ids_s)
  where
    zonk_rpat (f, pat, pun)
      = zonkPat te ve pat	     `thenNF_Tc` \ (new_pat, ids) ->
	returnNF_Tc ((f, new_pat, pun), ids)

zonkPat te ve (LitPat lit ty)
  = zonkTcTypeToType te ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (LitPat lit new_ty, [])

zonkPat te ve (NPat lit ty expr)
  = zonkTcTypeToType te ty	`thenNF_Tc` \ new_ty   ->
    zonkExpr te ve expr		`thenNF_Tc` \ new_expr ->
    returnNF_Tc (NPat lit new_ty new_expr, [])

zonkPat te ve (DictPat ds ms)
  = mapNF_Tc (zonkIdBndr te) ds    `thenNF_Tc` \ new_ds ->
    mapNF_Tc (zonkIdBndr te) ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (DictPat new_ds new_ms, new_ds ++ new_ms)


zonkPats te ve [] 
  = returnNF_Tc ([], [])
zonkPats te ve (pat:pats) 
  = zonkPat te ve pat	`thenNF_Tc` \ (pat', ids1) ->
    zonkPats te ve pats	`thenNF_Tc` \ (pats', ids2) ->
    returnNF_Tc (pat':pats', ids1 ++ ids2)

\end{code}


