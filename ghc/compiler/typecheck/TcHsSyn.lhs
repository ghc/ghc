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

	mkHsTyApp, mkHsDictApp,
	mkHsTyLam, mkHsDictLam,
	tcIdType,

	zonkBinds,
	zonkInst,
	zonkId,	    -- TcIdBndr s -> NF_TcM s Id
	unZonkId    -- Id         -> NF_TcM s (TcIdBndr s)
  ) where

import Ubiq{-uitous-}

-- friends:
import HsSyn	-- oodles of it
import Id	( GenId(..), IdDetails, PragmaInfo,	-- Can meddle modestly with Ids
		  DictVar(..), idType
		)

-- others:
import TcMonad
import TcType	( TcType(..), TcMaybe, TcTyVar(..),
		  zonkTcTypeToType, zonkTcTyVarToTyVar,
		  tcInstType
		)
import Usage	( UVar(..) )
import Util	( panic )

import PprType  ( GenType, GenTyVar ) 	-- instances
import TyVar	( GenTyVar )		-- instances
import Unique	( Unique )		-- instances
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
tcIdType (TcId id) = idType id
tcIdType other     = panic "tcIdType"
\end{code}



\begin{code}
instance Eq (TcIdOcc s) where
  (TcId id1)   == (TcId id2)   = id1 == id2
  (RealId id1) == (RealId id2) = id1 == id2

instance Outputable (TcIdOcc s) where
  ppr sty (TcId id)   = ppr sty id
  ppr sty (RealId id) = ppr sty id

instance NamedThing (TcIdOcc s) where
  getOccurrenceName (TcId id)   = getOccurrenceName id
  getOccurrenceName (RealId id) = getOccurrenceName id
\end{code}


%************************************************************************
%*									*
\subsection[BackSubst-HsBinds]{Running a substitution over @HsBinds@}
%*									*
%************************************************************************

\begin{code}
zonkId   :: TcIdOcc s -> NF_TcM s Id
unZonkId :: Id	      -> NF_TcM s (TcIdBndr s)

zonkId (RealId id) = returnNF_Tc id

zonkId (TcId (Id u ty details prags info))
  = zonkTcTypeToType ty	`thenNF_Tc` \ ty' ->
    returnNF_Tc (Id u ty' details prags info)

unZonkId (Id u ty details prags info)
  = tcInstType [] ty 	`thenNF_Tc` \ ty' ->
    returnNF_Tc (Id u ty' details prags info)
\end{code}

\begin{code}
zonkInst :: (TcIdOcc s, TcExpr s) -> NF_TcM s (Id, TypecheckedHsExpr)
zonkInst (id, expr)
  = zonkId id		`thenNF_Tc` \ id' ->
    zonkExpr expr	`thenNF_Tc` \ expr' ->
    returnNF_Tc (id', expr') 
\end{code}

\begin{code}
zonkBinds :: TcHsBinds s -> NF_TcM s TypecheckedHsBinds

zonkBinds EmptyBinds = returnNF_Tc EmptyBinds

zonkBinds (ThenBinds binds1 binds2)
  = zonkBinds binds1  `thenNF_Tc` \ new_binds1 ->
    zonkBinds binds2  `thenNF_Tc` \ new_binds2 ->
    returnNF_Tc (ThenBinds new_binds1 new_binds2)

zonkBinds (SingleBind bind)
  = zonkBind bind  `thenNF_Tc` \ new_bind ->
    returnNF_Tc (SingleBind new_bind)

zonkBinds (AbsBinds tyvars dicts locprs dict_binds val_bind)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    mapNF_Tc zonkId dicts		`thenNF_Tc` \ new_dicts ->
    mapNF_Tc subst_pair locprs		`thenNF_Tc` \ new_locprs ->
    mapNF_Tc subst_bind dict_binds	`thenNF_Tc` \ new_dict_binds ->
    zonkBind val_bind			`thenNF_Tc` \ new_val_bind ->
    returnNF_Tc (AbsBinds new_tyvars new_dicts new_locprs new_dict_binds new_val_bind)
  where
    subst_pair (l, g)
      = zonkId l	`thenNF_Tc` \ new_l ->
	zonkId g	`thenNF_Tc` \ new_g ->
	returnNF_Tc (new_l, new_g)

    subst_bind (v, e)
      = zonkId v	`thenNF_Tc` \ new_v ->
	zonkExpr e	`thenNF_Tc` \ new_e ->
	returnNF_Tc (new_v, new_e)
\end{code}

\begin{code}
-------------------------------------------------------------------------
zonkBind :: TcBind s -> NF_TcM s TypecheckedBind

zonkBind EmptyBind = returnNF_Tc EmptyBind

zonkBind (NonRecBind mbinds)
  = zonkMonoBinds mbinds	`thenNF_Tc` \ new_mbinds ->
    returnNF_Tc (NonRecBind new_mbinds)

zonkBind (RecBind mbinds)
  = zonkMonoBinds mbinds	`thenNF_Tc` \ new_mbinds ->
    returnNF_Tc (RecBind new_mbinds)

-------------------------------------------------------------------------
zonkMonoBinds :: TcMonoBinds s -> NF_TcM s TypecheckedMonoBinds

zonkMonoBinds EmptyMonoBinds = returnNF_Tc EmptyMonoBinds

zonkMonoBinds (AndMonoBinds mbinds1 mbinds2)
  = zonkMonoBinds mbinds1  `thenNF_Tc` \ new_mbinds1 ->
    zonkMonoBinds mbinds2  `thenNF_Tc` \ new_mbinds2 ->
    returnNF_Tc (AndMonoBinds new_mbinds1 new_mbinds2)

zonkMonoBinds (PatMonoBind pat grhss_w_binds locn)
  = zonkPat pat	    	   		`thenNF_Tc` \ new_pat ->
    zonkGRHSsAndBinds grhss_w_binds	`thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (PatMonoBind new_pat new_grhss_w_binds locn)

zonkMonoBinds (VarMonoBind var expr)
  = zonkId var    	`thenNF_Tc` \ new_var ->
    zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (VarMonoBind new_var new_expr)

zonkMonoBinds (FunMonoBind name ms locn)
  = zonkId name			`thenNF_Tc` \ new_name ->
    mapNF_Tc zonkMatch ms	`thenNF_Tc` \ new_ms ->
    returnNF_Tc (FunMonoBind new_name new_ms locn)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSsAndBinds}
%*									*
%************************************************************************

\begin{code}
zonkMatch :: TcMatch s -> NF_TcM s TypecheckedMatch

zonkMatch (PatMatch pat match)
  = zonkPat pat	    	`thenNF_Tc` \ new_pat ->
    zonkMatch match  	`thenNF_Tc` \ new_match ->
    returnNF_Tc (PatMatch new_pat new_match)

zonkMatch (GRHSMatch grhss_w_binds)
  = zonkGRHSsAndBinds grhss_w_binds `thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (GRHSMatch new_grhss_w_binds)

-------------------------------------------------------------------------
zonkGRHSsAndBinds :: TcGRHSsAndBinds s
		   -> NF_TcM s TypecheckedGRHSsAndBinds

zonkGRHSsAndBinds (GRHSsAndBindsOut grhss binds ty)
  = mapNF_Tc zonk_grhs grhss 	`thenNF_Tc` \ new_grhss ->
    zonkBinds binds   		`thenNF_Tc` \ new_binds ->
    zonkTcTypeToType ty 	`thenNF_Tc` \ new_ty ->
    returnNF_Tc (GRHSsAndBindsOut new_grhss new_binds new_ty)
  where
    zonk_grhs (GRHS guard expr locn)
      = zonkExpr guard  `thenNF_Tc` \ new_guard ->
	zonkExpr expr   `thenNF_Tc` \ new_expr  ->
	returnNF_Tc (GRHS new_guard new_expr locn)

    zonk_grhs (OtherwiseGRHS expr locn)
      = zonkExpr expr   `thenNF_Tc` \ new_expr  ->
	returnNF_Tc (OtherwiseGRHS new_expr locn)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-HsExpr]{Running a zonkitution over a TypeCheckedExpr}
%*									*
%************************************************************************

ToDo: panic on things that can't be in @TypecheckedHsExpr@.

\begin{code}
zonkExpr :: TcExpr s -> NF_TcM s TypecheckedHsExpr

zonkExpr (HsVar name)
  = zonkId name	`thenNF_Tc` \ new_name ->
    returnNF_Tc (HsVar new_name)

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

zonkExpr (OpApp e1 op e2)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr op	`thenNF_Tc` \ new_op ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (OpApp new_e1 new_op new_e2)

zonkExpr (SectionL expr op)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkExpr op		`thenNF_Tc` \ new_op ->
    returnNF_Tc (SectionL new_expr new_op)

zonkExpr (SectionR op expr)
  = zonkExpr op		`thenNF_Tc` \ new_op ->
    zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SectionR new_op new_expr)

zonkExpr (CCall fun args may_gc is_casm result_ty)
  = mapNF_Tc zonkExpr args 	`thenNF_Tc` \ new_args ->
    zonkTcTypeToType result_ty	`thenNF_Tc` \ new_result_ty ->
    returnNF_Tc (CCall fun new_args may_gc is_casm new_result_ty)

zonkExpr (HsSCC label expr)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsSCC label new_expr)

zonkExpr (HsCase expr ms src_loc)
  = zonkExpr expr    	    `thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkMatch ms   `thenNF_Tc` \ new_ms ->
    returnNF_Tc (HsCase new_expr new_ms src_loc)

zonkExpr (HsLet binds expr)
  = zonkBinds binds	`thenNF_Tc` \ new_binds ->
    zonkExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (HsLet new_binds new_expr)

zonkExpr (HsDoOut stmts m_id mz_id src_loc)
  = zonkStmts stmts 	`thenNF_Tc` \ new_stmts ->
    zonkId m_id		`thenNF_Tc` \ m_new ->
    zonkId mz_id	`thenNF_Tc` \ mz_new ->
    returnNF_Tc (HsDoOut new_stmts m_new mz_new src_loc)

zonkExpr (ListComp expr quals)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkQuals quals	`thenNF_Tc` \ new_quals ->
    returnNF_Tc (ListComp new_expr new_quals)

--ExplicitList: not in typechecked exprs

zonkExpr (ExplicitListOut ty exprs)
  = zonkTcTypeToType  ty	`thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkExpr exprs	`thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitListOut new_ty new_exprs)

zonkExpr (ExplicitTuple exprs)
  = mapNF_Tc zonkExpr exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitTuple new_exprs)

zonkExpr (RecordCon con rbinds)
  = panic "zonkExpr:RecordCon"
zonkExpr (RecordUpd exp rbinds)
  = panic "zonkExpr:RecordUpd"

zonkExpr (HsIf e1 e2 e3 src_loc)
  = zonkExpr e1	`thenNF_Tc` \ new_e1 ->
    zonkExpr e2	`thenNF_Tc` \ new_e2 ->
    zonkExpr e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (HsIf new_e1 new_e2 new_e3 src_loc)

zonkExpr (ArithSeqOut expr info)
  = zonkExpr expr	`thenNF_Tc` \ new_expr ->
    zonkArithSeq info	`thenNF_Tc` \ new_info ->
    returnNF_Tc (ArithSeqOut new_expr new_info)

zonkExpr (TyLam tyvars expr)
  = mapNF_Tc zonkTcTyVarToTyVar tyvars	`thenNF_Tc` \ new_tyvars ->
    zonkExpr expr			`thenNF_Tc` \ new_expr ->
    returnNF_Tc (TyLam new_tyvars new_expr)

zonkExpr (TyApp expr tys)
  = zonkExpr expr    	    	  `thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkTcTypeToType tys `thenNF_Tc` \ new_tys ->
    returnNF_Tc (TyApp new_expr new_tys)

zonkExpr (DictLam dicts expr)
  = mapNF_Tc zonkId dicts	`thenNF_Tc` \ new_dicts ->
    zonkExpr expr    	    	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictLam new_dicts new_expr)

zonkExpr (DictApp expr dicts)
  = zonkExpr expr    	    	`thenNF_Tc` \ new_expr ->
    mapNF_Tc zonkId dicts	`thenNF_Tc` \ new_dicts ->
    returnNF_Tc (DictApp new_expr new_dicts)

zonkExpr (ClassDictLam dicts methods expr)
  = mapNF_Tc zonkId dicts   `thenNF_Tc` \ new_dicts ->
    mapNF_Tc zonkId methods `thenNF_Tc` \ new_methods ->
    zonkExpr expr    	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (ClassDictLam new_dicts new_methods new_expr)

zonkExpr (Dictionary dicts methods)
  = mapNF_Tc zonkId dicts   `thenNF_Tc` \ new_dicts ->
    mapNF_Tc zonkId methods `thenNF_Tc` \ new_methods ->
    returnNF_Tc (Dictionary new_dicts new_methods)

zonkExpr (SingleDict name)
  = zonkId name  	`thenNF_Tc` \ new_name ->
    returnNF_Tc (SingleDict new_name)

-------------------------------------------------------------------------
zonkArithSeq :: TcArithSeqInfo s -> NF_TcM s TypecheckedArithSeqInfo

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
zonkQuals :: [TcQual s] -> NF_TcM s [TypecheckedQual]

zonkQuals quals
  = mapNF_Tc zonk_qual quals
  where
    zonk_qual (GeneratorQual pat expr)
      = zonkPat  pat    `thenNF_Tc` \ new_pat ->
	zonkExpr expr   `thenNF_Tc` \ new_expr ->
	returnNF_Tc (GeneratorQual new_pat new_expr)

    zonk_qual (FilterQual expr)
      = zonkExpr expr    `thenNF_Tc` \ new_expr ->
	returnNF_Tc (FilterQual new_expr)

    zonk_qual (LetQual binds)
      = zonkBinds binds	 `thenNF_Tc` \ new_binds ->
	returnNF_Tc (LetQual new_binds)

-------------------------------------------------------------------------
zonkStmts :: [TcStmt s] -> NF_TcM s [TypecheckedStmt]

zonkStmts stmts
  = mapNF_Tc zonk_stmt stmts
  where
    zonk_stmt (BindStmt pat expr src_loc)
      = zonkPat  pat    `thenNF_Tc` \ new_pat ->
	zonkExpr expr   `thenNF_Tc` \ new_expr ->
	returnNF_Tc (BindStmt new_pat new_expr src_loc)

    zonk_stmt (ExprStmt expr src_loc)
      = zonkExpr expr    `thenNF_Tc` \ new_expr ->
	returnNF_Tc (ExprStmt new_expr src_loc)

    zonk_stmt (LetStmt binds)
      = zonkBinds binds	 `thenNF_Tc` \ new_binds ->
	returnNF_Tc (LetStmt new_binds)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
zonkPat :: TcPat s -> NF_TcM s TypecheckedPat

zonkPat (WildPat ty)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (WildPat new_ty)

zonkPat (VarPat v)
  = zonkId v	    `thenNF_Tc` \ new_v ->
    returnNF_Tc (VarPat new_v)

zonkPat (LazyPat pat)
  = zonkPat pat	    `thenNF_Tc` \ new_pat ->
    returnNF_Tc (LazyPat new_pat)

zonkPat (AsPat n pat)
  = zonkId n	    `thenNF_Tc` \ new_n ->
    zonkPat pat	    `thenNF_Tc` \ new_pat ->
    returnNF_Tc (AsPat new_n new_pat)

zonkPat (ConPat n ty pats)
  = zonkTcTypeToType ty	     `thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkPat pats    `thenNF_Tc` \ new_pats ->
    returnNF_Tc (ConPat n new_ty new_pats)

zonkPat (ConOpPat pat1 op pat2 ty)
  = zonkPat pat1	    `thenNF_Tc` \ new_pat1 ->
    zonkPat pat2	    `thenNF_Tc` \ new_pat2 ->
    zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (ConOpPat new_pat1 op new_pat2 new_ty)

zonkPat (ListPat ty pats)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty ->
    mapNF_Tc zonkPat pats   `thenNF_Tc` \ new_pats ->
    returnNF_Tc (ListPat new_ty new_pats)

zonkPat (TuplePat pats)
  = mapNF_Tc zonkPat pats    `thenNF_Tc` \ new_pats ->
    returnNF_Tc (TuplePat new_pats)

zonkPat (LitPat lit ty)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty  ->
    returnNF_Tc (LitPat lit new_ty)

zonkPat (NPat lit ty expr)
  = zonkTcTypeToType ty	    `thenNF_Tc` \ new_ty   ->
    zonkExpr expr	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (NPat lit new_ty new_expr)

zonkPat (DictPat ds ms)
  = mapNF_Tc zonkId ds    `thenNF_Tc` \ new_ds ->
    mapNF_Tc zonkId ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (DictPat new_ds new_ms)
\end{code}


