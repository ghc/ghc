%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1995
%
\section[BackSubst]{Back substitution functions}

This module applies a typechecker substitution over the whole abstract
syntax.

\begin{code}
#include "HsVersions.h"

module BackSubst (
	 applyTcSubstToBinds,

	 -- and to make the interface self-sufficient...
	 Subst, Binds, MonoBinds, Id, TypecheckedPat
   ) where

IMPORT_Trace		-- ToDo: rm (debugging)
import Outputable
import Pretty

import AbsSyn
import AbsUniType	( getTyVar )
import TcMonad
import Util
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Binds]{Running a substitution over @Binds@}
%*									*
%************************************************************************

\begin{code}
applyTcSubstToBinds :: TypecheckedBinds -> NF_TcM TypecheckedBinds

applyTcSubstToBinds EmptyBinds = returnNF_Tc EmptyBinds

applyTcSubstToBinds (ThenBinds binds1 binds2)
  = applyTcSubstToBinds binds1  `thenNF_Tc` \ new_binds1 ->
    applyTcSubstToBinds binds2  `thenNF_Tc` \ new_binds2 ->
    returnNF_Tc (ThenBinds new_binds1 new_binds2)

applyTcSubstToBinds (SingleBind bind)
  = substBind bind  `thenNF_Tc` \ new_bind ->
    returnNF_Tc (SingleBind new_bind)

applyTcSubstToBinds (AbsBinds tyvars dicts locprs dict_binds val_bind)
  = subst_tyvars tyvars 	    `thenNF_Tc` \ new_tyvars ->
    mapNF_Tc applyTcSubstToId dicts   `thenNF_Tc` \ new_dicts ->
    mapNF_Tc subst_pair locprs	    `thenNF_Tc` \ new_locprs ->
    mapNF_Tc subst_bind dict_binds    `thenNF_Tc` \ new_dict_binds ->
    substBind val_bind		    `thenNF_Tc` \ new_val_bind ->
    returnNF_Tc (AbsBinds new_tyvars new_dicts new_locprs new_dict_binds new_val_bind)
  where
    subst_pair (l, g)
      = applyTcSubstToId l	`thenNF_Tc` \ new_l ->
	applyTcSubstToId g	`thenNF_Tc` \ new_g ->
	returnNF_Tc (new_l, new_g)

    subst_bind (v, e)
      = applyTcSubstToInst v	`thenNF_Tc` \ new_v ->
	substExpr e		`thenNF_Tc` \ new_e ->
	returnNF_Tc (new_v, new_e)
\end{code}

\begin{code}
-------------------------------------------------------------------------
substBind :: TypecheckedBind -> NF_TcM TypecheckedBind

substBind (NonRecBind mbinds)
  = applyTcSubstToMonoBinds mbinds	`thenNF_Tc` \ new_mbinds ->
    returnNF_Tc (NonRecBind new_mbinds)

substBind (RecBind mbinds)
  = applyTcSubstToMonoBinds mbinds	`thenNF_Tc` \ new_mbinds ->
    returnNF_Tc (RecBind new_mbinds)

substBind other = returnNF_Tc other

-------------------------------------------------------------------------
applyTcSubstToMonoBinds :: TypecheckedMonoBinds -> NF_TcM TypecheckedMonoBinds

applyTcSubstToMonoBinds EmptyMonoBinds = returnNF_Tc EmptyMonoBinds

applyTcSubstToMonoBinds (AndMonoBinds mbinds1 mbinds2)
  = applyTcSubstToMonoBinds mbinds1  `thenNF_Tc` \ new_mbinds1 ->
    applyTcSubstToMonoBinds mbinds2  `thenNF_Tc` \ new_mbinds2 ->
    returnNF_Tc (AndMonoBinds new_mbinds1 new_mbinds2)

applyTcSubstToMonoBinds (PatMonoBind pat grhss_w_binds locn)
  = substPat pat	    	    	    `thenNF_Tc` \ new_pat ->
    substGRHSsAndBinds grhss_w_binds  `thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (PatMonoBind new_pat new_grhss_w_binds locn)

applyTcSubstToMonoBinds (VarMonoBind var expr)
  = applyTcSubstToId var    `thenNF_Tc` \ new_var ->
    substExpr expr	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (VarMonoBind new_var new_expr)

applyTcSubstToMonoBinds (FunMonoBind name ms locn)
  = applyTcSubstToId name   `thenNF_Tc` \ new_name ->
    mapNF_Tc substMatch ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (FunMonoBind new_name new_ms locn)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Match-GRHSs]{Match and GRHSsAndBinds}
%*									*
%************************************************************************

\begin{code}
substMatch :: TypecheckedMatch -> NF_TcM TypecheckedMatch

substMatch (PatMatch pat match)
  = substPat pat	    `thenNF_Tc` \ new_pat ->
    substMatch match  	    `thenNF_Tc` \ new_match ->
    returnNF_Tc (PatMatch new_pat new_match)

substMatch (GRHSMatch grhss_w_binds)
  = substGRHSsAndBinds grhss_w_binds `thenNF_Tc` \ new_grhss_w_binds ->
    returnNF_Tc (GRHSMatch new_grhss_w_binds)

-------------------------------------------------------------------------
substGRHSsAndBinds :: TypecheckedGRHSsAndBinds
		   -> NF_TcM TypecheckedGRHSsAndBinds

substGRHSsAndBinds (GRHSsAndBindsOut grhss binds ty)
  = mapNF_Tc subst_grhs grhss 	`thenNF_Tc` \ new_grhss ->
    applyTcSubstToBinds binds   `thenNF_Tc` \ new_binds ->
    applyTcSubstToTy ty 	`thenNF_Tc` \ new_ty ->
    returnNF_Tc (GRHSsAndBindsOut new_grhss new_binds new_ty)
  where
    subst_grhs (GRHS guard expr locn)
      = substExpr guard  `thenNF_Tc` \ new_guard ->
	substExpr expr   `thenNF_Tc` \ new_expr  ->
	returnNF_Tc (GRHS new_guard new_expr locn)

    subst_grhs (OtherwiseGRHS expr locn)
      = substExpr expr   `thenNF_Tc` \ new_expr  ->
	returnNF_Tc (OtherwiseGRHS new_expr locn)
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-Expr]{Running a substitution over a TypeCheckedExpr}
%*									*
%************************************************************************

ToDo: panic on things that can't be in @TypecheckedExpr@.

\begin{code}
substExpr :: TypecheckedExpr -> NF_TcM TypecheckedExpr

substExpr (Var name)
  = applyTcSubstToId name	`thenNF_Tc` \ new_name ->
    returnNF_Tc (Var new_name)

substExpr (Lit (LitLitLit s ty))
  = applyTcSubstToTy ty		`thenNF_Tc` \ new_ty ->
    returnNF_Tc (Lit (LitLitLit s new_ty))

substExpr other_lit@(Lit lit) = returnNF_Tc other_lit

substExpr (Lam match)
  = substMatch match	`thenNF_Tc` \ new_match ->
    returnNF_Tc (Lam new_match)

substExpr (App e1 e2)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (App new_e1 new_e2)

substExpr (OpApp e1 op e2)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr op	`thenNF_Tc` \ new_op ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (OpApp new_e1 new_op new_e2)

substExpr (SectionL expr op)
  = substExpr expr	`thenNF_Tc` \ new_expr ->
    substExpr op	`thenNF_Tc` \ new_op ->
    returnNF_Tc (SectionL new_expr new_op)

substExpr (SectionR op expr)
  = substExpr op	`thenNF_Tc` \ new_op ->
    substExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SectionR new_op new_expr)

substExpr (CCall fun args may_gc is_casm result_ty)
  = mapNF_Tc substExpr args 	`thenNF_Tc` \ new_args ->
    applyTcSubstToTy result_ty	`thenNF_Tc` \ new_result_ty ->
    returnNF_Tc (CCall fun new_args may_gc is_casm new_result_ty)

substExpr (SCC label expr)
  = substExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (SCC label new_expr)

substExpr (Case expr ms)
  = substExpr expr    	    `thenNF_Tc` \ new_expr ->
    mapNF_Tc substMatch ms    `thenNF_Tc` \ new_ms ->
    returnNF_Tc (Case new_expr new_ms)

substExpr (ListComp expr quals)
  = substExpr expr	`thenNF_Tc` \ new_expr ->
    substQuals quals	`thenNF_Tc` \ new_quals ->
    returnNF_Tc (ListComp new_expr new_quals)

substExpr (Let binds expr)
  = applyTcSubstToBinds binds `thenNF_Tc` \ new_binds ->
    substExpr expr	      `thenNF_Tc` \ new_expr ->
    returnNF_Tc (Let new_binds new_expr)

--ExplicitList: not in typechecked exprs

substExpr (ExplicitListOut ty exprs)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    mapNF_Tc substExpr exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitListOut new_ty new_exprs)

substExpr (ExplicitTuple exprs)
  = mapNF_Tc substExpr exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitTuple new_exprs)

substExpr (If e1 e2 e3)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    substExpr e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (If new_e1 new_e2 new_e3)

substExpr (ArithSeqOut expr info)
  = substExpr expr	`thenNF_Tc` \ new_expr ->
    substArithSeq info	`thenNF_Tc` \ new_info ->
    returnNF_Tc (ArithSeqOut new_expr new_info)

substExpr (TyLam tyvars expr)
  = subst_tyvars tyvars	`thenNF_Tc` \ new_tyvars ->
    substExpr expr	`thenNF_Tc` \ new_expr ->
    returnNF_Tc (TyLam new_tyvars new_expr)

substExpr (TyApp expr tys)
  = substExpr expr    	    	  `thenNF_Tc` \ new_expr ->
    mapNF_Tc (applyTcSubstToTy) tys `thenNF_Tc` \ new_tys ->
    returnNF_Tc (TyApp new_expr new_tys)

substExpr (DictLam dicts expr)
  = mapNF_Tc applyTcSubstToId dicts `thenNF_Tc` \ new_dicts ->
    substExpr expr    	    	  `thenNF_Tc` \ new_expr ->
    returnNF_Tc (DictLam new_dicts new_expr)

substExpr (DictApp expr dicts)
  = substExpr expr    	    	  `thenNF_Tc` \ new_expr ->
    mapNF_Tc applyTcSubstToId dicts `thenNF_Tc` \ new_dicts ->
    returnNF_Tc (DictApp new_expr new_dicts)

substExpr (ClassDictLam dicts methods expr)
  = mapNF_Tc applyTcSubstToId dicts   `thenNF_Tc` \ new_dicts ->
    mapNF_Tc applyTcSubstToId methods `thenNF_Tc` \ new_methods ->
    substExpr expr    	    	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (ClassDictLam new_dicts new_methods new_expr)

substExpr (Dictionary dicts methods)
  = mapNF_Tc applyTcSubstToId dicts   `thenNF_Tc` \ new_dicts ->
    mapNF_Tc applyTcSubstToId methods `thenNF_Tc` \ new_methods ->
    returnNF_Tc (Dictionary new_dicts new_methods)

substExpr (SingleDict name)
  = applyTcSubstToId name  	`thenNF_Tc` \ new_name ->
    returnNF_Tc (SingleDict new_name)

#ifdef DPH

substExpr (ParallelZF expr quals)
  = substExpr expr	`thenNF_Tc` \ new_expr ->
    substParQuals quals	`thenNF_Tc` \ new_quals ->
    returnNF_Tc (ParallelZF new_expr new_quals)

--substExpr (ExplicitPodIn exprs) :: not in typechecked

substExpr (ExplicitPodOut ty exprs)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    mapNF_Tc substExpr exprs  `thenNF_Tc` \ new_exprs ->
    returnNF_Tc (ExplicitPodOut new_ty new_exprs)

substExpr (ExplicitProcessor exprs expr)
  = mapNF_Tc substExpr exprs  `thenNF_Tc` \ new_exprs ->
    substExpr expr	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (ExplicitProcessor new_exprs new_expr)

#endif {- Data Parallel Haskell -}

-------------------------------------------------------------------------
substArithSeq :: TypecheckedArithSeqInfo -> NF_TcM TypecheckedArithSeqInfo

substArithSeq (From e)
  = substExpr e		`thenNF_Tc` \ new_e ->
    returnNF_Tc (From new_e)

substArithSeq (FromThen e1 e2)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromThen new_e1 new_e2)

substArithSeq (FromTo e1 e2)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    returnNF_Tc (FromTo new_e1 new_e2)

substArithSeq (FromThenTo e1 e2 e3)
  = substExpr e1	`thenNF_Tc` \ new_e1 ->
    substExpr e2	`thenNF_Tc` \ new_e2 ->
    substExpr e3	`thenNF_Tc` \ new_e3 ->
    returnNF_Tc (FromThenTo new_e1 new_e2 new_e3)

-------------------------------------------------------------------------
substQuals :: [TypecheckedQual] -> NF_TcM [TypecheckedQual]

substQuals quals
  = mapNF_Tc subst_qual quals
  where
    subst_qual (GeneratorQual pat expr)
      = substPat  pat    `thenNF_Tc` \ new_pat ->
	substExpr expr   `thenNF_Tc` \ new_expr ->
	returnNF_Tc (GeneratorQual new_pat new_expr)

    subst_qual (FilterQual expr)
      = substExpr expr    `thenNF_Tc` \ new_expr ->
	returnNF_Tc (FilterQual new_expr)

-------------------------------------------------------------------------
#ifdef DPH
substParQuals :: TypecheckedParQuals -> NF_TcM TypecheckedParQuals

substParQuals (AndParQuals quals1 quals2)
 = substParQuals quals1	    	`thenNF_Tc` \ new_quals1 ->
   substParQuals quals2	    	`thenNF_Tc` \ new_quals2 ->
   returnNF_Tc (AndParQuals new_quals1 new_quals2)

--substParQuals (DrawnGenIn pats pat expr) :: not in typechecked

substParQuals (DrawnGenOut pats convs pat expr)
 = mapNF_Tc substPat pats	    `thenNF_Tc` \ new_pats  ->
   mapNF_Tc substExpr convs   `thenNF_Tc` \ new_convs ->
   substPat pat		    `thenNF_Tc` \ new_pat   -> 
   substExpr expr	    `thenNF_Tc` \ new_expr  ->
   returnNF_Tc (DrawnGenOut new_pats new_convs new_pat new_expr)

substParQuals (IndexGen pats pat expr)
 = mapNF_Tc substExpr pats    `thenNF_Tc` \ new_pats ->
   substPat pat		    `thenNF_Tc` \ new_pat  -> 
   substExpr expr	    `thenNF_Tc` \ new_expr ->
   returnNF_Tc (IndexGen new_pats new_pat new_expr)

substParQuals (ParFilter expr) 
 = substExpr expr	    `thenNF_Tc` \ new_expr ->
   returnNF_Tc (ParFilter new_expr)
#endif {- Data Parallel Haskell -}
\end{code}
 
%************************************************************************
%*									*
\subsection[BackSubst-Pats]{Patterns}
%*									*
%************************************************************************

\begin{code}
substPat :: TypecheckedPat -> NF_TcM TypecheckedPat

substPat (WildPat ty)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (WildPat new_ty)

substPat (VarPat v)
  = applyTcSubstToId v	    `thenNF_Tc` \ new_v ->
    returnNF_Tc (VarPat new_v)

substPat (LazyPat pat)
  = substPat pat	    `thenNF_Tc` \ new_pat ->
    returnNF_Tc (LazyPat new_pat)

substPat (AsPat n pat)
  = applyTcSubstToId n	    `thenNF_Tc` \ new_n ->
    substPat pat	    `thenNF_Tc` \ new_pat ->
    returnNF_Tc (AsPat new_n new_pat)

substPat (ConPat n ty pats)
  = applyTcSubstToId n	    `thenNF_Tc` \ new_n ->
	-- ToDo: "n"'s global, so omit?
    applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    mapNF_Tc substPat pats    `thenNF_Tc` \ new_pats ->
    returnNF_Tc (ConPat new_n new_ty new_pats)

substPat (ConOpPat pat1 op pat2 ty)
  = substPat pat1	    `thenNF_Tc` \ new_pat1 ->
    applyTcSubstToId op	    `thenNF_Tc` \ new_op ->
    substPat pat2	    `thenNF_Tc` \ new_pat2 ->
    applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (ConOpPat new_pat1 new_op new_pat2 new_ty)

substPat (ListPat ty pats)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    mapNF_Tc substPat pats    `thenNF_Tc` \ new_pats ->
    returnNF_Tc (ListPat new_ty new_pats)

substPat (TuplePat pats)
  = mapNF_Tc substPat pats    `thenNF_Tc` \ new_pats ->
    returnNF_Tc (TuplePat new_pats)

substPat (LitPat lit ty)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    returnNF_Tc (LitPat lit new_ty)

substPat (NPat lit ty expr)
  = applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    substExpr expr	    `thenNF_Tc` \ new_expr ->
    returnNF_Tc (NPat lit new_ty new_expr)

substPat (NPlusKPat n k ty e1 e2 e3)
  = applyTcSubstToId n	    `thenNF_Tc` \ new_n ->
    applyTcSubstToTy ty	    `thenNF_Tc` \ new_ty ->
    substExpr e1	    `thenNF_Tc` \ new_e1 ->
    substExpr e2	    `thenNF_Tc` \ new_e2 ->
    substExpr e3	    `thenNF_Tc` \ new_e3 ->
    returnNF_Tc (NPlusKPat new_n k new_ty new_e1 new_e2 new_e3)

#ifdef DPH
substPat (ProcessorPat pats convs pat)
  = mapNF_Tc substPat pats    `thenNF_Tc` \ new_pats ->
    mapNF_Tc substExpr convs  `thenNF_Tc` \ new_convs ->
    substPat pat	    `thenNF_Tc` \ new_pat ->
    returnNF_Tc (ProcessorPat new_pats new_convs new_pat)
#endif {- Data Parallel Haskell -}
\end{code}

%************************************************************************
%*									*
\subsection[BackSubst-TyVar]{Running a substitution over type variables}
%*									*
%************************************************************************

The type variables in an @AbsBinds@ or @TyLam@ may have a binding in the
substitution as a result of a @matchTy@ call.  So we should subsitute for
them too. The result should certainly be a type variable.

\begin{code}
subst_tyvars tyvars
  = mapNF_Tc applyTcSubstToTyVar tyvars `thenNF_Tc` \ new_tyvar_tys ->
    returnNF_Tc (map (getTyVar "subst_tyvars") new_tyvar_tys)
\end{code}
