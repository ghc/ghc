%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnExpr4]{Renaming of expressions (pass 4)}

Basically dependency analysis.

Handles @Match@, @GRHSsAndBinds@, @HsExpr@, and @Qual@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
#include "HsVersions.h"

module RnExpr4 (
	rnMatch, rnGRHSsAndBinds, rnPat

	-- and to make the interface self-sufficient...
   ) where

import Ubiq{-uitous-}
import RnLoop		-- break the RnPass4/RnExpr4/RnBinds4 loops

import HsSyn
import RdrHsSyn
import RnHsSyn
import RnMonad4

-- others:
import Name		( Name(..) )
import NameTypes	( FullName{-instances-} )
import Outputable	( isConop )
import UniqSet		( emptyUniqSet, unitUniqSet,
			  unionUniqSets, unionManyUniqSets,
			  UniqSet(..)
			)
import Util		( panic )
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat ::  ProtoNamePat -> Rn4M RenamedPat

rnPat WildPatIn = returnRn4 WildPatIn

rnPat (VarPatIn name)
  = lookupValue name	`thenRn4` \ vname ->
    returnRn4 (VarPatIn vname)

rnPat (LitPatIn n) = returnRn4 (LitPatIn n)

rnPat (LazyPatIn pat)
  = rnPat pat	`thenRn4` \ pat' ->
    returnRn4 (LazyPatIn pat')

rnPat (AsPatIn name pat)
  = rnPat pat	`thenRn4` \ pat' ->
    lookupValue name	`thenRn4` \ vname ->
    returnRn4 (AsPatIn vname pat')

rnPat (ConPatIn name pats)
  = lookupValue name	    `thenRn4` \ name' ->
    mapRn4 rnPat pats  `thenRn4` \ patslist ->
    returnRn4 (ConPatIn name' patslist)

rnPat (ConOpPatIn pat1 name pat2)
  = lookupValue name	`thenRn4` \ name' ->
    rnPat pat1	`thenRn4` \ pat1' ->
    rnPat pat2	`thenRn4` \ pat2' ->
    returnRn4 (ConOpPatIn pat1' name' pat2')

rnPat (ListPatIn pats)
  = mapRn4 rnPat pats `thenRn4` \ patslist ->
    returnRn4 (ListPatIn patslist)

rnPat (TuplePatIn pats)
  = mapRn4 rnPat pats `thenRn4` \ patslist ->
    returnRn4 (TuplePatIn patslist)

rnPat (RecPatIn con rpats)
  = panic "rnPat:RecPatIn"

\end{code}

************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch :: ProtoNameMatch -> Rn4M (RenamedMatch, FreeVars)

rnMatch match
  = getSrcLocRn4			`thenRn4` \ src_loc ->
    namesFromProtoNames "variable in pattern"
	 (binders `zip` repeat src_loc)	`thenRn4` \ new_binders ->
    extendSS2 new_binders (rnMatch_aux match)
  where
    binders = collect_binders match

    collect_binders :: ProtoNameMatch -> [ProtoName]

    collect_binders (GRHSMatch _) = []
    collect_binders (PatMatch pat match)
      = collectPatBinders pat ++ collect_binders match

rnMatch_aux (PatMatch pat match)
  = rnPat pat		`thenRn4` \ pat' ->
    rnMatch_aux match	`thenRn4` \ (match', fvMatch) ->
    returnRn4 (PatMatch pat' match', fvMatch)

rnMatch_aux (GRHSMatch grhss_and_binds)
  = rnGRHSsAndBinds grhss_and_binds `thenRn4` \ (grhss_and_binds', fvs) ->
    returnRn4 (GRHSMatch grhss_and_binds', fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSsAndBinds)}
%*									*
%************************************************************************

\begin{code}
rnGRHSsAndBinds :: ProtoNameGRHSsAndBinds -> Rn4M (RenamedGRHSsAndBinds, FreeVars)

rnGRHSsAndBinds (GRHSsAndBindsIn grhss binds)
  = rnBinds binds			`thenRn4` \ (binds', fvBinds, scope) ->
    extendSS2 scope (rnGRHSs grhss)	`thenRn4` \ (grhss', fvGRHS) ->
    returnRn4 (GRHSsAndBindsIn grhss' binds', fvBinds `unionUniqSets` fvGRHS)
  where
    rnGRHSs [] = returnRn4 ([], emptyUniqSet)

    rnGRHSs (grhs:grhss)
      = rnGRHS  grhs   `thenRn4` \ (grhs',  fvs) ->
	rnGRHSs grhss  `thenRn4` \ (grhss', fvss) ->
	returnRn4 (grhs' : grhss', fvs `unionUniqSets` fvss)

    rnGRHS (GRHS guard expr locn)
      = pushSrcLocRn4 locn			      	  (
	rnExpr guard   `thenRn4` \ (guard', fvsg) ->
	rnExpr expr	`thenRn4` \ (expr',  fvse) ->
	returnRn4 (GRHS guard' expr' locn, fvsg `unionUniqSets` fvse)
	)

    rnGRHS (OtherwiseGRHS expr locn)
      = pushSrcLocRn4 locn			      	  (
	rnExpr expr	`thenRn4` \ (expr', fvs) ->
	returnRn4 (OtherwiseGRHS expr' locn, fvs)
	)
\end{code}

%************************************************************************
%*									*
\subsubsection{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs :: [ProtoNameHsExpr] -> Rn4M ([RenamedHsExpr], FreeVars)

rnExprs [] = returnRn4 ([], emptyUniqSet)

rnExprs (expr:exprs)
  = rnExpr expr 	`thenRn4` \ (expr', fvExpr) ->
    rnExprs exprs	`thenRn4` \ (exprs', fvExprs) ->
    returnRn4 (expr':exprs', fvExpr `unionUniqSets` fvExprs)
\end{code}

Variables. We look up the variable and return the resulting name.  The
interesting question is what the free-variable set should be.  We
don't want to return imported or prelude things as free vars.  So we
look at the Name returned from the lookup, and make it part of the
free-var set iff:
\begin{itemize}
\item
if it's a @Short@,
\item
or it's an @ValName@ and it's defined in this module
(this includes locally-defined constructrs, but that's too bad)
\end{itemize}

\begin{code}
rnExpr :: ProtoNameHsExpr -> Rn4M (RenamedHsExpr, FreeVars)

rnExpr (HsVar v)
  = lookupValue v	`thenRn4` \ vname ->
    returnRn4 (HsVar vname, fv_set vname)
  where
    fv_set n@(Short uniq sname)	    = unitUniqSet n
    fv_set n@(ValName uniq fname)
	  | isLocallyDefined fname
	  && not (isConop (getOccurrenceName fname))
				    = unitUniqSet n
    fv_set other  		    = emptyUniqSet

rnExpr (HsLit lit)  = returnRn4 (HsLit lit, emptyUniqSet)

rnExpr (HsLam match)
  = rnMatch match	`thenRn4` \ (match', fvMatch) ->
    returnRn4 (HsLam match', fvMatch)

rnExpr (HsApp fun arg)
  = rnExpr fun		`thenRn4` \ (fun',fvFun) ->
    rnExpr arg		`thenRn4` \ (arg',fvArg) ->
    returnRn4 (HsApp fun' arg', fvFun `unionUniqSets` fvArg)

rnExpr (OpApp e1 op e2)
  = rnExpr e1 		`thenRn4` \ (e1', fvs_e1) ->
    rnExpr op 		`thenRn4` \ (op', fvs_op) ->
    rnExpr e2 		`thenRn4` \ (e2', fvs_e2) ->
    returnRn4 (OpApp e1' op' e2', (fvs_op `unionUniqSets` fvs_e1) `unionUniqSets` fvs_e2)

rnExpr (SectionL expr op)
  = rnExpr expr	 	`thenRn4` \ (expr', fvs_expr) ->
    rnExpr op	 	`thenRn4` \ (op', fvs_op) ->
    returnRn4 (SectionL expr' op', fvs_op `unionUniqSets` fvs_expr)

rnExpr (SectionR op expr)
  = rnExpr op	 	`thenRn4` \ (op',   fvs_op) ->
    rnExpr expr	 	`thenRn4` \ (expr', fvs_expr) ->
    returnRn4 (SectionR op' expr', fvs_op `unionUniqSets` fvs_expr)

rnExpr (CCall fun args may_gc is_casm fake_result_ty)
  = rnExprs args	 `thenRn4` \ (args', fvs_args) ->
    returnRn4 (CCall fun args' may_gc is_casm fake_result_ty, fvs_args)

rnExpr (HsSCC label expr)
  = rnExpr expr	 	`thenRn4` \ (expr', fvs_expr) ->
    returnRn4 (HsSCC label expr', fvs_expr)

rnExpr (HsCase expr ms src_loc)
  = pushSrcLocRn4 src_loc $
    rnExpr expr		 	`thenRn4` \ (new_expr, e_fvs) ->
    mapAndUnzipRn4 rnMatch ms   `thenRn4` \ (new_ms, ms_fvs) ->
    returnRn4 (HsCase new_expr new_ms src_loc, unionManyUniqSets (e_fvs : ms_fvs))

rnExpr (HsLet binds expr)
  = rnBinds binds		`thenRn4` \ (binds', fvBinds, new_binders) ->
    extendSS2 new_binders (rnExpr expr) `thenRn4` \ (expr',fvExpr) ->
    returnRn4 (HsLet binds' expr', fvBinds `unionUniqSets` fvExpr)

rnExpr (HsDo stmts src_loc)
  = pushSrcLocRn4 src_loc $
    rnStmts stmts		`thenRn4` \ (stmts', fvStmts) ->
    returnRn4 (HsDo stmts' src_loc, fvStmts)

rnExpr (ListComp expr quals)
  = rnQuals quals 		`thenRn4` \ ((quals', qual_binders), fvQuals) ->
    extendSS2 qual_binders (rnExpr expr) `thenRn4` \ (expr', fvExpr) ->
    returnRn4 (ListComp expr' quals', fvExpr `unionUniqSets` fvQuals)

rnExpr (ExplicitList exps)
  = rnExprs exps	 `thenRn4` \ (exps', fvs) ->
    returnRn4  (ExplicitList exps', fvs)

rnExpr (ExplicitTuple exps)
  = rnExprs exps	 `thenRn4` \ (exps', fvExps) ->
    returnRn4 (ExplicitTuple exps', fvExps)

rnExpr (RecordCon con rbinds)
  = panic "rnExpr:RecordCon"
rnExpr (RecordUpd exp rbinds)
  = panic "rnExpr:RecordUpd"

rnExpr (ExprWithTySig expr pty)
  = rnExpr expr			    `thenRn4` \ (expr', fvExpr) ->
    rnPolyType False nullTyVarNamesEnv pty `thenRn4` \ pty' ->
    returnRn4 (ExprWithTySig expr' pty', fvExpr)

rnExpr (HsIf p b1 b2 src_loc)
  = pushSrcLocRn4 src_loc $
    rnExpr p	`thenRn4` \ (p', fvP) ->
    rnExpr b1	`thenRn4` \ (b1', fvB1) ->
    rnExpr b2	`thenRn4` \ (b2', fvB2) ->
    returnRn4 (HsIf p' b1' b2' src_loc, unionManyUniqSets [fvP, fvB1, fvB2])

rnExpr (ArithSeqIn seq)
  = rn_seq seq `thenRn4` \ (new_seq, fvs) ->
    returnRn4 (ArithSeqIn new_seq, fvs)
  where
    rn_seq (From expr)
     = rnExpr expr 	 `thenRn4` \ (expr', fvExpr) ->
       returnRn4 (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr expr1 	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       returnRn4 (FromThen expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       returnRn4 (FromTo expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       rnExpr expr3	 `thenRn4` \ (expr3', fvExpr3) ->
       returnRn4 (FromThenTo expr1' expr2' expr3',
		  unionManyUniqSets [fvExpr1, fvExpr2, fvExpr3])

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
rnQuals :: [ProtoNameQual]
	 -> Rn4M (([RenamedQual],	-- renamed qualifiers
		  [Name]),		-- qualifiers' binders
		  FreeVars)		-- free variables

rnQuals [qual] 				-- must be at least one qual
  = rnQual qual `thenRn4` \ ((new_qual, bs), fvs) ->
    returnRn4 (([new_qual], bs), fvs)

rnQuals (qual: quals)
  = rnQual qual				`thenRn4` \ ((qual',  bs1), fvQuals1) ->
    extendSS2 bs1 (rnQuals quals)	`thenRn4` \ ((quals', bs2), fvQuals2) ->
    returnRn4
       ((qual' : quals', bs2 ++ bs1),	-- The ones on the right (bs2) shadow the
					-- ones on the left (bs1)
	fvQuals1 `unionUniqSets` fvQuals2)

rnQual (GeneratorQual pat expr)
  = rnExpr expr		 `thenRn4` \ (expr', fvExpr) ->
    let
	binders = collectPatBinders pat
    in
    getSrcLocRn4		 `thenRn4` \ src_loc ->
    namesFromProtoNames "variable in list-comprehension-generator pattern"
	 (binders `zip` repeat src_loc)	  `thenRn4` \ new_binders ->
    extendSS new_binders (rnPat pat) `thenRn4` \ pat' ->

    returnRn4 ((GeneratorQual pat' expr', new_binders), fvExpr)

rnQual (FilterQual expr)
  = rnExpr expr	 `thenRn4` \ (expr', fvs) ->
    returnRn4 ((FilterQual expr', []), fvs)

rnQual (LetQual binds)
  = rnBinds binds	`thenRn4` \ (binds', binds_fvs, new_binders) ->
    returnRn4 ((LetQual binds', new_binders), binds_fvs)
\end{code}


%************************************************************************
%*									*
\subsubsection{@Stmt@s: in @do@ expressions}
%*									*
%************************************************************************

\begin{code}
rnStmts :: [ProtoNameStmt]
	-> Rn4M ([RenamedStmt],		-- renamed qualifiers
	         FreeVars)		-- free variables

rnStmts [stmt@(ExprStmt _ _)]		-- last stmt must be ExprStmt
  = rnStmt stmt				`thenRn4` \ ((stmt',[]), fvStmt) ->
    returnRn4 ([stmt'], fvStmt)

rnStmts (stmt:stmts)
  = rnStmt stmt				`thenRn4` \ ((stmt',bs), fvStmt) ->
    extendSS2 bs (rnStmts stmts)	`thenRn4` \ (stmts',     fvStmts) ->
    returnRn4 (stmt':stmts', fvStmt `unionUniqSets` fvStmts)


rnStmt (BindStmt pat expr src_loc)
  = pushSrcLocRn4 src_loc $
    rnExpr expr		 		`thenRn4` \ (expr', fvExpr) ->
    let
	binders = collectPatBinders pat
    in
    namesFromProtoNames "variable in do binding"
	 (binders `zip` repeat src_loc)	`thenRn4` \ new_binders ->
    extendSS new_binders (rnPat pat) 	`thenRn4` \ pat' ->

    returnRn4 ((BindStmt pat' expr' src_loc, new_binders), fvExpr)

rnStmt (ExprStmt expr src_loc)
  = 
    rnExpr expr	 			`thenRn4` \ (expr', fvs) ->
    returnRn4 ((ExprStmt expr' src_loc, []), fvs)

rnStmt (LetStmt binds)
  = rnBinds binds	`thenRn4` \ (binds', binds_fvs, new_binders) ->
    returnRn4 ((LetStmt binds', new_binders), binds_fvs)

\end{code}
