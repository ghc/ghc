%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnExpr]{Renaming of expressions}

Basically dependency analysis.

Handles @Match@, @GRHSsAndBinds@, @HsExpr@, and @Qual@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
#include "HsVersions.h"

module RnExpr (
	rnMatch, rnGRHSsAndBinds, rnPat,
	checkPrecMatch
   ) where

import Ubiq
import RnLoop		-- break the RnPass/RnExpr/RnBinds loops

import HsSyn
import RdrHsSyn
import RnHsSyn
import RnMonad

import ErrUtils		( addErrLoc, addShortErrLocLine )
import Name		( isLocallyDefinedName, pprSym, Name, RdrName )
import Pretty
import UniqFM		( lookupUFM, ufmToList{-ToDo:rm-} )
import UniqSet		( emptyUniqSet, unitUniqSet,
			  unionUniqSets, unionManyUniqSets,
			  UniqSet(..)
			)
import Util		( Ord3(..), removeDups, panic )
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat :: RdrNamePat -> RnM_Fixes s RenamedPat

rnPat WildPatIn = returnRn WildPatIn

rnPat (VarPatIn name)
  = lookupValue name	`thenRn` \ vname ->
    returnRn (VarPatIn vname)

rnPat (LitPatIn n) = returnRn (LitPatIn n)

rnPat (LazyPatIn pat)
  = rnPat pat		`thenRn` \ pat' ->
    returnRn (LazyPatIn pat')

rnPat (AsPatIn name pat)
  = rnPat pat		`thenRn` \ pat' ->
    lookupValue name	`thenRn` \ vname ->
    returnRn (AsPatIn vname pat')

rnPat (ConPatIn con pats)
  = lookupConstr con	`thenRn` \ con' ->
    mapRn rnPat pats  	`thenRn` \ patslist ->
    returnRn (ConPatIn con' patslist)

rnPat (ConOpPatIn pat1 con pat2)
  = lookupConstr con	`thenRn` \ con' ->
    rnPat pat1		`thenRn` \ pat1' ->
    rnPat pat2		`thenRn` \ pat2' ->
    precParsePat (ConOpPatIn pat1' con' pat2')

rnPat neg@(NegPatIn pat)
  = getSrcLocRn		`thenRn` \ src_loc ->
    addErrIfRn (not (valid_neg_pat pat)) (negPatErr neg src_loc)
			`thenRn_`
    rnPat pat		`thenRn` \ pat' ->
    returnRn (NegPatIn pat')
  where
    valid_neg_pat (LitPatIn (HsInt  _)) = True
    valid_neg_pat (LitPatIn (HsFrac _)) = True
    valid_neg_pat _                     = False

rnPat (ParPatIn pat)
  = rnPat pat		`thenRn` \ pat' ->
    returnRn (ParPatIn pat')

rnPat (ListPatIn pats)
  = mapRn rnPat pats 	`thenRn` \ patslist ->
    returnRn (ListPatIn patslist)

rnPat (TuplePatIn pats)
  = mapRn rnPat pats 	`thenRn` \ patslist ->
    returnRn (TuplePatIn patslist)

rnPat (RecPatIn con rpats)
  = lookupConstr con 	`thenRn` \ con' ->
    rnRpats rpats	`thenRn` \ rpats' ->
    returnRn (RecPatIn con' rpats')
\end{code}

************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch :: RdrNameMatch -> RnM_Fixes s (RenamedMatch, FreeVars)

rnMatch match
  = getSrcLocRn			`thenRn` \ src_loc ->
    newLocalNames "variable in pattern"
	 (binders `zip` repeat src_loc)	`thenRn` \ new_binders ->
    extendSS2 new_binders (rnMatch_aux match)
  where
    binders = collect_binders match

    collect_binders :: RdrNameMatch -> [RdrName]

    collect_binders (GRHSMatch _) = []
    collect_binders (PatMatch pat match)
      = collectPatBinders pat ++ collect_binders match

rnMatch_aux (PatMatch pat match)
  = rnPat pat		`thenRn` \ pat' ->
    rnMatch_aux match	`thenRn` \ (match', fvMatch) ->
    returnRn (PatMatch pat' match', fvMatch)

rnMatch_aux (GRHSMatch grhss_and_binds)
  = rnGRHSsAndBinds grhss_and_binds `thenRn` \ (grhss_and_binds', fvs) ->
    returnRn (GRHSMatch grhss_and_binds', fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSsAndBinds)}
%*									*
%************************************************************************

\begin{code}
rnGRHSsAndBinds :: RdrNameGRHSsAndBinds -> RnM_Fixes s (RenamedGRHSsAndBinds, FreeVars)

rnGRHSsAndBinds (GRHSsAndBindsIn grhss binds)
  = rnBinds binds			`thenRn` \ (binds', fvBinds, scope) ->
    extendSS2 scope (rnGRHSs grhss)	`thenRn` \ (grhss', fvGRHS) ->
    returnRn (GRHSsAndBindsIn grhss' binds', fvBinds `unionUniqSets` fvGRHS)
  where
    rnGRHSs [] = returnRn ([], emptyUniqSet)

    rnGRHSs (grhs:grhss)
      = rnGRHS  grhs   `thenRn` \ (grhs',  fvs) ->
	rnGRHSs grhss  `thenRn` \ (grhss', fvss) ->
	returnRn (grhs' : grhss', fvs `unionUniqSets` fvss)

    rnGRHS (GRHS guard expr locn)
      = pushSrcLocRn locn $		    
	rnExpr guard	`thenRn` \ (guard', fvsg) ->
	rnExpr expr	`thenRn` \ (expr',  fvse) ->
	returnRn (GRHS guard' expr' locn, fvsg `unionUniqSets` fvse)

    rnGRHS (OtherwiseGRHS expr locn)
      = pushSrcLocRn locn $
	rnExpr expr	`thenRn` \ (expr', fvs) ->
	returnRn (OtherwiseGRHS expr' locn, fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs :: [RdrNameHsExpr] -> RnM_Fixes s ([RenamedHsExpr], FreeVars)

rnExprs [] = returnRn ([], emptyUniqSet)

rnExprs (expr:exprs)
  = rnExpr expr 	`thenRn` \ (expr', fvExpr) ->
    rnExprs exprs	`thenRn` \ (exprs', fvExprs) ->
    returnRn (expr':exprs', fvExpr `unionUniqSets` fvExprs)
\end{code}

Variables. We look up the variable and return the resulting name.  The
interesting question is what the free-variable set should be.  We
don't want to return imported or prelude things as free vars.  So we
look at the RnName returned from the lookup, and make it part of the
free-var set iff if it's a LocallyDefined RnName.

ToDo: what about RnClassOps ???
\end{itemize}

\begin{code}
fv_set vname@(RnName n) | isLocallyDefinedName n
		        = unitUniqSet vname
fv_set _		= emptyUniqSet


rnExpr :: RdrNameHsExpr -> RnM_Fixes s (RenamedHsExpr, FreeVars)

rnExpr (HsVar v)
  = lookupValue v	`thenRn` \ vname ->
    returnRn (HsVar vname, fv_set vname)

rnExpr (HsLit lit)
  = returnRn (HsLit lit, emptyUniqSet)

rnExpr (HsLam match)
  = rnMatch match	`thenRn` \ (match', fvMatch) ->
    returnRn (HsLam match', fvMatch)

rnExpr (HsApp fun arg)
  = rnExpr fun		`thenRn` \ (fun',fvFun) ->
    rnExpr arg		`thenRn` \ (arg',fvArg) ->
    returnRn (HsApp fun' arg', fvFun `unionUniqSets` fvArg)

rnExpr (OpApp e1 op e2)
  = rnExpr e1		`thenRn` \ (e1', fvs_e1) ->
    rnExpr op		`thenRn` \ (op', fvs_op) ->
    rnExpr e2		`thenRn` \ (e2', fvs_e2) ->
    precParseExpr (OpApp e1' op' e2') `thenRn` \ exp ->
    returnRn (exp, (fvs_op `unionUniqSets` fvs_e1) `unionUniqSets` fvs_e2)

rnExpr (NegApp e n)
  = rnExpr e 		`thenRn` \ (e', fvs_e) ->
    rnExpr n		`thenRn` \ (n', fvs_n) ->
    returnRn (NegApp e' n', fvs_e `unionUniqSets` fvs_n)

rnExpr (HsPar e)
  = rnExpr e 		`thenRn` \ (e', fvs_e) ->
    returnRn (HsPar e', fvs_e)

rnExpr (SectionL expr op)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    rnExpr op	 	`thenRn` \ (op', fvs_op) ->
    returnRn (SectionL expr' op', fvs_op `unionUniqSets` fvs_expr)

rnExpr (SectionR op expr)
  = rnExpr op	 	`thenRn` \ (op',   fvs_op) ->
    rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (SectionR op' expr', fvs_op `unionUniqSets` fvs_expr)

rnExpr (CCall fun args may_gc is_casm fake_result_ty)
  = rnExprs args	`thenRn` \ (args', fvs_args) ->
    returnRn (CCall fun args' may_gc is_casm fake_result_ty, fvs_args)

rnExpr (HsSCC label expr)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (HsSCC label expr', fvs_expr)

rnExpr (HsCase expr ms src_loc)
  = pushSrcLocRn src_loc $
    rnExpr expr		 	`thenRn` \ (new_expr, e_fvs) ->
    mapAndUnzipRn rnMatch ms	`thenRn` \ (new_ms, ms_fvs) ->
    returnRn (HsCase new_expr new_ms src_loc, unionManyUniqSets (e_fvs : ms_fvs))

rnExpr (HsLet binds expr)
  = rnBinds binds		`thenRn` \ (binds', fvBinds, new_binders) ->
    extendSS2 new_binders (rnExpr expr) `thenRn` \ (expr',fvExpr) ->
    returnRn (HsLet binds' expr', fvBinds `unionUniqSets` fvExpr)

rnExpr (HsDo stmts src_loc)
  = pushSrcLocRn src_loc $
    rnStmts stmts		`thenRn` \ (stmts', fvStmts) ->
    returnRn (HsDo stmts' src_loc, fvStmts)

rnExpr (ListComp expr quals)
  = rnQuals quals 		`thenRn` \ ((quals', qual_binders), fvQuals) ->
    extendSS2 qual_binders (rnExpr expr) `thenRn` \ (expr', fvExpr) ->
    returnRn (ListComp expr' quals', fvExpr `unionUniqSets` fvQuals)

rnExpr (ExplicitList exps)
  = rnExprs exps	 	`thenRn` \ (exps', fvs) ->
    returnRn  (ExplicitList exps', fvs)

rnExpr (ExplicitTuple exps)
  = rnExprs exps	 	`thenRn` \ (exps', fvExps) ->
    returnRn (ExplicitTuple exps', fvExps)

rnExpr (RecordCon (HsVar con) rbinds)
  = lookupConstr con 			`thenRn` \ conname ->
    rnRbinds "construction" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordCon (HsVar conname) rbinds', fvRbinds)

rnExpr (RecordUpd expr rbinds)
  = rnExpr expr			`thenRn` \ (expr', fvExpr) ->
    rnRbinds "update" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordUpd expr' rbinds', fvExpr `unionUniqSets` fvRbinds)

rnExpr (ExprWithTySig expr pty)
  = rnExpr expr			 	`thenRn` \ (expr', fvExpr) ->
    rnPolyType nullTyVarNamesEnv pty `thenRn` \ pty' ->
    returnRn (ExprWithTySig expr' pty', fvExpr)

rnExpr (HsIf p b1 b2 src_loc)
  = pushSrcLocRn src_loc $
    rnExpr p		`thenRn` \ (p', fvP) ->
    rnExpr b1		`thenRn` \ (b1', fvB1) ->
    rnExpr b2		`thenRn` \ (b2', fvB2) ->
    returnRn (HsIf p' b1' b2' src_loc, unionManyUniqSets [fvP, fvB1, fvB2])

rnExpr (ArithSeqIn seq)
  = rn_seq seq 		`thenRn` \ (new_seq, fvs) ->
    returnRn (ArithSeqIn new_seq, fvs)
  where
    rn_seq (From expr)
     = rnExpr expr 	`thenRn` \ (expr', fvExpr) ->
       returnRn (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr expr1 	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromThen expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromTo expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       rnExpr expr3	`thenRn` \ (expr3', fvExpr3) ->
       returnRn (FromThenTo expr1' expr2' expr3',
		  unionManyUniqSets [fvExpr1, fvExpr2, fvExpr3])
\end{code}

%************************************************************************
%*									*
\subsubsection{@Rbinds@s and @Rpats@s: in record expressions}
%*									*
%************************************************************************

\begin{code}
rnRbinds str rbinds 
  = mapRn field_dup_err dup_fields	`thenRn_`
    mapAndUnzipRn rn_rbind rbinds	`thenRn` \ (rbinds', fvRbind_s) ->
    returnRn (rbinds', unionManyUniqSets fvRbind_s)
  where
    (_, dup_fields) = removeDups cmp [ f | (f,_,_) <- rbinds ]

    field_dup_err dups = getSrcLocRn `thenRn` \ src_loc ->
		         addErrRn (dupFieldErr str src_loc dups)

    rn_rbind (field, expr, pun)
      = lookupField field	`thenRn` \ fieldname ->
	rnExpr expr		`thenRn` \ (expr', fvExpr) ->
	returnRn ((fieldname, expr', pun), fvExpr)

rnRpats rpats
  = mapRn field_dup_err dup_fields 	`thenRn_`
    mapRn rn_rpat rpats
  where
    (_, dup_fields) = removeDups cmp [ f | (f,_,_) <- rpats ]

    field_dup_err dups = getSrcLocRn `thenRn` \ src_loc ->
		         addErrRn (dupFieldErr "pattern" src_loc dups)

    rn_rpat (field, pat, pun)
      = lookupField field	`thenRn` \ fieldname ->
	rnPat pat		`thenRn` \ pat' ->
	returnRn (fieldname, pat', pun)
\end{code}

%************************************************************************
%*									*
\subsubsection{@Qual@s: in list comprehensions}
%*									*
%************************************************************************

Note that although some bound vars may appear in the free var set for
the first qual, these will eventually be removed by the caller. For
example, if we have @[p | r <- s, q <- r, p <- q]@, when doing
@[q <- r, p <- q]@, the free var set for @q <- r@ will
be @{r}@, and the free var set for the entire Quals will be @{r}@. This
@r@ will be removed only when we finally return from examining all the
Quals.

\begin{code}
rnQuals :: [RdrNameQual]
	 -> RnM_Fixes s (([RenamedQual],	-- renamed qualifiers
		         [RnName]),		-- qualifiers' binders
		         FreeVars)		-- free variables

rnQuals [qual] 				-- must be at least one qual
  = rnQual qual `thenRn` \ ((new_qual, bs), fvs) ->
    returnRn (([new_qual], bs), fvs)

rnQuals (qual: quals)
  = rnQual qual				`thenRn` \ ((qual',  bs1), fvQuals1) ->
    extendSS2 bs1 (rnQuals quals)	`thenRn` \ ((quals', bs2), fvQuals2) ->
    returnRn
       ((qual' : quals', bs1 ++ bs2),	-- The ones on the right (bs2) shadow the
					-- ones on the left (bs1)
	fvQuals1 `unionUniqSets` fvQuals2)

rnQual (GeneratorQual pat expr)
  = rnExpr expr		 `thenRn` \ (expr', fvExpr) ->
    let
	binders = collectPatBinders pat
    in
    getSrcLocRn		 `thenRn` \ src_loc ->
    newLocalNames "variable in list-comprehension-generator pattern"
	 (binders `zip` repeat src_loc)	  `thenRn` \ new_binders ->
    extendSS new_binders (rnPat pat) `thenRn` \ pat' ->

    returnRn ((GeneratorQual pat' expr', new_binders), fvExpr)

rnQual (FilterQual expr)
  = rnExpr expr	 `thenRn` \ (expr', fvs) ->
    returnRn ((FilterQual expr', []), fvs)

rnQual (LetQual binds)
  = rnBinds binds	`thenRn` \ (binds', binds_fvs, new_binders) ->
    returnRn ((LetQual binds', new_binders), binds_fvs)
\end{code}


%************************************************************************
%*									*
\subsubsection{@Stmt@s: in @do@ expressions}
%*									*
%************************************************************************

\begin{code}
rnStmts :: [RdrNameStmt] -> RnM_Fixes s ([RenamedStmt], FreeVars)

rnStmts [stmt@(ExprStmt _ _)]		-- last stmt must be ExprStmt
  = rnStmt stmt				`thenRn` \ ((stmt',[]), fvStmt) ->
    returnRn ([stmt'], fvStmt)

rnStmts (stmt:stmts)
  = rnStmt stmt				`thenRn` \ ((stmt',bs), fvStmt) ->
    extendSS2 bs (rnStmts stmts)	`thenRn` \ (stmts',     fvStmts) ->
    returnRn (stmt':stmts', fvStmt `unionUniqSets` fvStmts)


rnStmt (BindStmt pat expr src_loc)
  = pushSrcLocRn src_loc $
    rnExpr expr		 		`thenRn` \ (expr', fvExpr) ->
    let
	binders = collectPatBinders pat
    in
    newLocalNames "variable in do binding"
	 (binders `zip` repeat src_loc)	`thenRn` \ new_binders ->
    extendSS new_binders (rnPat pat) 	`thenRn` \ pat' ->

    returnRn ((BindStmt pat' expr' src_loc, new_binders), fvExpr)

rnStmt (ExprStmt expr src_loc)
  = 
    rnExpr expr	 			`thenRn` \ (expr', fvs) ->
    returnRn ((ExprStmt expr' src_loc, []), fvs)

rnStmt (LetStmt binds)
  = rnBinds binds	`thenRn` \ (binds', binds_fvs, new_binders) ->
    returnRn ((LetStmt binds', new_binders), binds_fvs)

\end{code}

%************************************************************************
%*									*
\subsubsection{Precedence Parsing}
%*									*
%************************************************************************

\begin{code}
precParseExpr :: RenamedHsExpr -> RnM_Fixes s RenamedHsExpr
precParsePat  :: RenamedPat -> RnM_Fixes s RenamedPat

precParseExpr exp@(OpApp (NegApp e1 n) (HsVar op) e2)
  = lookupFixity op		`thenRn` \ (op_fix, op_prec) ->
    if 6 < op_prec then		
	-- negate precedence 6 wired in
	-- (-x)*y  ==> -(x*y)
	precParseExpr (OpApp e1 (HsVar op) e2) `thenRn` \ op_app ->
	returnRn (NegApp op_app n)
    else
	returnRn exp

precParseExpr exp@(OpApp (OpApp e11 (HsVar op1) e12) (HsVar op) e2)
  = lookupFixity op		 `thenRn` \ (op_fix, op_prec) ->
    lookupFixity op1		 `thenRn` \ (op1_fix, op1_prec) ->
    -- pprTrace "precParse:" (ppCat [ppr PprDebug op, ppInt op_prec, ppr PprDebug op1, ppInt op1_prec]) $
    case (op1_prec `cmp` op_prec) of
      LT_  -> rearrange
      EQ_  -> case (op1_fix, op_fix) of
		(INFIXR, INFIXR) -> rearrange
		(INFIXL, INFIXL) -> returnRn exp
		_ -> getSrcLocRn `thenRn` \ src_loc ->
		     failButContinueRn exp
		     (precParseErr (op1,op1_fix,op1_prec) (op,op_fix,op_prec) src_loc)
      GT__ -> returnRn exp
  where
    rearrange = precParseExpr (OpApp e12 (HsVar op) e2) `thenRn` \ e2' ->
	        returnRn (OpApp e11 (HsVar op1) e2')

precParseExpr exp = returnRn exp


precParsePat pat@(ConOpPatIn (NegPatIn e1) op e2)
  = lookupFixity op		`thenRn` \ (op_fix, op_prec) ->
    if 6 < op_prec then	
	-- negate precedence 6 wired in
	getSrcLocRn `thenRn` \ src_loc ->
	failButContinueRn pat (precParseNegPatErr (op,op_fix,op_prec) src_loc)
    else
	returnRn pat

precParsePat pat@(ConOpPatIn (ConOpPatIn p11 op1 p12) op p2)
  = lookupFixity op		 `thenRn` \ (op_fix, op_prec) ->
    lookupFixity op1		 `thenRn` \ (op1_fix, op1_prec) ->
    case (op1_prec `cmp` op_prec) of
      LT_  -> rearrange
      EQ_  -> case (op1_fix, op_fix) of
		(INFIXR, INFIXR) -> rearrange
		(INFIXL, INFIXL) -> returnRn pat
		_ -> getSrcLocRn `thenRn` \ src_loc ->
		     failButContinueRn pat
		       (precParseErr (op1,op1_fix,op1_prec) (op,op_fix,op_prec) src_loc)
      GT__ -> returnRn pat
  where
    rearrange = precParsePat (ConOpPatIn p12 op p2) `thenRn` \ p2' ->
	        returnRn (ConOpPatIn p11 op1 p2')

precParsePat pat = returnRn pat


data INFIX = INFIXL | INFIXR | INFIXN deriving Eq

lookupFixity :: RnName -> RnM_Fixes s (INFIX, Int)
lookupFixity op
  = getExtraRn `thenRn` \ fixity_fm ->
    -- pprTrace "lookupFixity:" (ppAboves [ppCat [pprUnique u, ppr PprDebug i_f] | (u,i_f) <- ufmToList fixity_fm]) $
    case lookupUFM fixity_fm op of
      Nothing           -> returnRn (INFIXL, 9)
      Just (InfixL _ n) -> returnRn (INFIXL, n)
      Just (InfixR _ n) -> returnRn (INFIXR, n)
      Just (InfixN _ n) -> returnRn (INFIXN, n)
\end{code}

\begin{code}
checkPrecMatch :: Bool -> RnName -> RenamedMatch -> RnM_Fixes s ()

checkPrecMatch False fn match
  = returnRn ()
checkPrecMatch True op (PatMatch p1 (PatMatch p2 (GRHSMatch _)))
  = checkPrec op p1 False	`thenRn_`
    checkPrec op p2 True
checkPrecMatch True op _
  = panic "checkPrecMatch"

checkPrec op (ConOpPatIn _ op1 _) right
  = lookupFixity op	`thenRn` \ (op_fix, op_prec) ->
    lookupFixity op1	`thenRn` \ (op1_fix, op1_prec) ->
    getSrcLocRn 	`thenRn` \ src_loc ->
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_fix == INFIXR && op_fix == INFIXR && right ||
		   op1_fix == INFIXL && op_fix == INFIXL && not right))

	info  = (op,op_fix,op_prec)
	info1 = (op1,op1_fix,op1_prec)
	(infol, infor) = if right then (info, info1) else (info1, info)
    in
    addErrIfRn (not inf_ok) (precParseErr infol infor src_loc)

checkPrec op (NegPatIn _) right
  = lookupFixity op	`thenRn` \ (op_fix, op_prec) ->
    getSrcLocRn 	`thenRn` \ src_loc ->
    addErrIfRn (6 < op_prec) (precParseNegPatErr (op,op_fix,op_prec) src_loc)

checkPrec op pat right
  = returnRn ()
\end{code}

\begin{code}
dupFieldErr str src_loc (dup:rest)
  = addShortErrLocLine src_loc (\ sty ->
    ppBesides [ppStr "duplicate field name `", ppr sty dup, ppStr "' in record ", ppStr str])

negPatErr pat src_loc
  = addShortErrLocLine src_loc (\ sty ->
    ppSep [ppStr "prefix `-' not applied to literal in pattern", ppr sty pat])

precParseNegPatErr op src_loc
  = addErrLoc src_loc "precedence parsing error" (\ sty ->
    ppBesides [ppStr "prefix `-' has lower precedence than ", pp_op sty op, ppStr " in pattern"])

precParseErr op1 op2 src_loc
  = addErrLoc src_loc "precedence parsing error" (\ sty -> 
    ppBesides [ppStr "cannot mix ", pp_op sty op1, ppStr " and ", pp_op sty op2,
	       ppStr " in the same infix expression"])

pp_op sty (op, fix, prec) = ppBesides [pprSym sty op, ppLparen, pp_fix fix, ppSP, ppInt prec, ppRparen]
pp_fix INFIXL = ppStr "infixl"
pp_fix INFIXR = ppStr "infixr"
pp_fix INFIXN = ppStr "infix"
\end{code}
