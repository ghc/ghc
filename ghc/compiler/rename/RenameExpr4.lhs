%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1994
%
\section[RenameExpr]{Renaming of expressions}

Basically dependency analysis.

Handles @Match@, @GRHSsAndBinds@, @Expr@, and @Qual@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
#include "HsVersions.h"

module RenameExpr4 (
	rnMatch4, rnGRHSsAndBinds4, rnPat4,
	
	-- and to make the interface self-sufficient...
	Bag, GRHSsAndBinds, InPat, Name, Maybe,
	ProtoName, GlobalNameFun(..), UniqSet(..), UniqFM, SrcLoc,
	Unique, SplitUniqSupply,
	Pretty(..), PprStyle, PrettyRep
   ) where

import AbsSyn
import NameTypes	( FullName )
import Outputable
import ProtoName	( ProtoName(..) )
import Rename4		( rnPolyType4 )
import RenameAuxFuns	( GlobalNameFuns(..) ) -- ToDo: rm this line
import RenameBinds4	( rnBinds4, FreeVars(..) )
import RenameMonad4
import UniqSet
import Util
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat4 ::  ProtoNamePat -> Rn4M RenamedPat

rnPat4  WildPatIn = returnRn4 WildPatIn

rnPat4 (VarPatIn name)
  = lookupValue name	`thenRn4` \ vname ->
    returnRn4 (VarPatIn vname)

rnPat4  (LitPatIn n) = returnRn4 (LitPatIn n)

rnPat4  (LazyPatIn pat)
  = rnPat4  pat	`thenRn4` \ pat' ->
    returnRn4 (LazyPatIn pat')

rnPat4  (AsPatIn name pat)
  = rnPat4  pat	`thenRn4` \ pat' ->
    lookupValue name	`thenRn4` \ vname ->
    returnRn4 (AsPatIn vname pat')

rnPat4 (ConPatIn name pats)
  = lookupValue name	    `thenRn4` \ name' ->
    mapRn4 rnPat4 pats  `thenRn4` \ patslist ->
    returnRn4 (ConPatIn name' patslist)

rnPat4  (ConOpPatIn pat1 name pat2)
  = lookupValue name	`thenRn4` \ name' ->
    rnPat4  pat1	`thenRn4` \ pat1' ->
    rnPat4  pat2	`thenRn4` \ pat2' ->
    returnRn4 (ConOpPatIn pat1' name' pat2')

rnPat4  (ListPatIn pats)
  = mapRn4 rnPat4 pats `thenRn4` \ patslist ->
    returnRn4 (ListPatIn patslist)

rnPat4  (TuplePatIn pats)
  = mapRn4 rnPat4 pats `thenRn4` \ patslist ->
    returnRn4 (TuplePatIn patslist)

rnPat4  (NPlusKPatIn name lit)
  = lookupValue name	`thenRn4` \ vname ->
    returnRn4 (NPlusKPatIn vname lit)

#ifdef DPH
rnPat4  (ProcessorPatIn pats pat)
  = mapRn4 rnPat4 pats  `thenRn4` \ pats' ->
    rnPat4 pat	    `thenRn4` \ pat'  ->
    returnRn4 (ProcessorPatIn pats' pat')
#endif {- Data Parallel Haskell -}
\end{code}

************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch4 :: ProtoNameMatch -> Rn4M (RenamedMatch, FreeVars)

rnMatch4 match
  = getSrcLocRn4			`thenRn4` \ src_loc ->
    namesFromProtoNames "variable in pattern"
	 (binders `zip` repeat src_loc)	`thenRn4` \ new_binders ->
    extendSS2 new_binders (rnMatch4_aux match)
  where
    binders = collect_binders match

    collect_binders :: ProtoNameMatch -> [ProtoName]

    collect_binders (GRHSMatch _) = []
    collect_binders (PatMatch pat match)
      = collectPatBinders pat ++ collect_binders match

rnMatch4_aux (PatMatch pat match)
  = rnPat4 pat		`thenRn4` \ pat' ->
    rnMatch4_aux match	`thenRn4` \ (match', fvMatch) ->
    returnRn4 (PatMatch pat' match', fvMatch)

rnMatch4_aux (GRHSMatch grhss_and_binds)
  = rnGRHSsAndBinds4 grhss_and_binds `thenRn4` \ (grhss_and_binds', fvs) ->
    returnRn4 (GRHSMatch grhss_and_binds', fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection[dep-GRHSs]{Guarded right-hand sides (GRHSsAndBinds)}
%*									*
%************************************************************************

\begin{code}
rnGRHSsAndBinds4 :: ProtoNameGRHSsAndBinds -> Rn4M (RenamedGRHSsAndBinds, FreeVars)

rnGRHSsAndBinds4 (GRHSsAndBindsIn grhss binds)
  = rnBinds4 binds			`thenRn4` \ (binds', fvBinds, scope) ->
    extendSS2 scope (rnGRHSs4 grhss)	`thenRn4` \ (grhss', fvGRHS) ->
    returnRn4 (GRHSsAndBindsIn grhss' binds', fvBinds `unionUniqSets` fvGRHS)
  where
    rnGRHSs4 [] = returnRn4 ([], emptyUniqSet)

    rnGRHSs4 (grhs:grhss)
      = rnGRHS4  grhs   `thenRn4` \ (grhs',  fvs) ->
	rnGRHSs4 grhss  `thenRn4` \ (grhss', fvss) ->
	returnRn4 (grhs' : grhss', fvs `unionUniqSets` fvss)

    rnGRHS4 (GRHS guard expr locn)
      = pushSrcLocRn4 locn			      	  (
        rnExpr4 guard   `thenRn4` \ (guard', fvsg) ->
	rnExpr4 expr	`thenRn4` \ (expr',  fvse) ->
	returnRn4 (GRHS guard' expr' locn, fvsg `unionUniqSets` fvse)
	)

    rnGRHS4 (OtherwiseGRHS expr locn)
      = pushSrcLocRn4 locn			      	  (
        rnExpr4 expr	`thenRn4` \ (expr', fvs) ->
	returnRn4 (OtherwiseGRHS expr' locn, fvs)
	)
\end{code}

%************************************************************************
%*									*
\subsubsection[dep-Expr]{Expressions}
%*									*
%************************************************************************

\begin{code}
rnExprs4 :: [ProtoNameExpr] -> Rn4M ([RenamedExpr], FreeVars)

rnExprs4 [] = returnRn4 ([], emptyUniqSet)

rnExprs4 (expr:exprs)
  = rnExpr4 expr 	`thenRn4` \ (expr', fvExpr) ->
    rnExprs4 exprs	`thenRn4` \ (exprs', fvExprs) ->
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
or it's an @OtherTopId@ and it's defined in this module
(this includes locally-defined constructrs, but that's too bad)
\end{itemize}

\begin{code}
rnExpr4 :: ProtoNameExpr -> Rn4M (RenamedExpr, FreeVars)

rnExpr4 (Var v)
  = lookupValue v		`thenRn4` \ vname ->
    returnRn4 (Var vname, fv_set vname)
  where
    fv_set n@(Short uniq sname)	    = singletonUniqSet n
    fv_set n@(OtherTopId uniq fname)
	  | isLocallyDefined fname
	  && not (isConop (getOccurrenceName fname))
				    = singletonUniqSet n
    fv_set other  		    = emptyUniqSet

rnExpr4 (Lit lit)  = returnRn4 (Lit lit, emptyUniqSet)

rnExpr4 (Lam match)
  = rnMatch4 match	`thenRn4` \ (match', fvMatch) ->
    returnRn4 (Lam match', fvMatch)

rnExpr4 (App fun arg)
  = rnExpr4 fun	`thenRn4` \ (fun',fvFun) ->
    rnExpr4 arg	`thenRn4` \ (arg',fvArg) ->
    returnRn4 (App fun' arg', fvFun `unionUniqSets` fvArg)

rnExpr4 (OpApp e1 op e2)
  = rnExpr4 e1 	`thenRn4` \ (e1', fvs_e1) ->
    rnExpr4 op 	`thenRn4` \ (op', fvs_op) ->
    rnExpr4 e2 	`thenRn4` \ (e2', fvs_e2) ->
    returnRn4 (OpApp e1' op' e2', (fvs_op `unionUniqSets` fvs_e1) `unionUniqSets` fvs_e2)

rnExpr4 (SectionL expr op)
  = rnExpr4 expr	 `thenRn4` \ (expr', fvs_expr) ->
    rnExpr4 op	 `thenRn4` \ (op', fvs_op) ->
    returnRn4 (SectionL expr' op', fvs_op `unionUniqSets` fvs_expr)

rnExpr4 (SectionR op expr)
  = rnExpr4 op	 `thenRn4` \ (op',   fvs_op) ->
    rnExpr4 expr	 `thenRn4` \ (expr', fvs_expr) ->
    returnRn4 (SectionR op' expr', fvs_op `unionUniqSets` fvs_expr)

rnExpr4 (CCall fun args may_gc is_casm fake_result_ty)
  = rnExprs4 args	 `thenRn4` \ (args', fvs_args) ->
    returnRn4 (CCall fun args' may_gc is_casm fake_result_ty, fvs_args)

rnExpr4 (SCC label expr)
  = rnExpr4 expr	 `thenRn4` \ (expr', fvs_expr) ->
    returnRn4 (SCC label expr', fvs_expr)

rnExpr4 (Case expr ms)
  = rnExpr4 expr		 `thenRn4` \ (new_expr, e_fvs) ->
    mapAndUnzipRn4 rnMatch4 ms   `thenRn4` \ (new_ms, ms_fvs) ->
    returnRn4 (Case new_expr new_ms, unionManyUniqSets (e_fvs : ms_fvs))

rnExpr4 (ListComp expr quals)
  = rnQuals4 quals 	`thenRn4` \ ((quals', qual_binders), fvQuals) ->
    extendSS2 qual_binders (rnExpr4 expr) `thenRn4` \ (expr', fvExpr) ->
    returnRn4 (ListComp expr' quals', fvExpr `unionUniqSets` fvQuals)

rnExpr4 (Let binds expr)
  = rnBinds4 binds	`thenRn4` \ (binds', fvBinds, new_binders) ->
    extendSS2 new_binders (rnExpr4 expr) `thenRn4` \ (expr',fvExpr) ->
    returnRn4 (Let binds' expr', fvBinds `unionUniqSets` fvExpr)

rnExpr4 (ExplicitList exps)
  = rnExprs4 exps	 `thenRn4` \ (exps', fvs) ->
    returnRn4  (ExplicitList exps', fvs)

rnExpr4 (ExplicitTuple exps)
  = rnExprs4 exps	 `thenRn4` \ (exps', fvExps) ->
    returnRn4 (ExplicitTuple exps', fvExps)

rnExpr4 (ExprWithTySig expr pty)
  = rnExpr4 expr				 `thenRn4` \ (expr', fvExpr) ->
    rnPolyType4 False True nullTyVarNamesEnv pty `thenRn4` \ pty' ->
    returnRn4 (ExprWithTySig expr' pty', fvExpr)

rnExpr4 (If p b1 b2)
  = rnExpr4 p	`thenRn4` \ (p', fvP) ->
    rnExpr4 b1	`thenRn4` \ (b1', fvB1) ->
    rnExpr4 b2	`thenRn4` \ (b2', fvB2) ->
    returnRn4 (If p' b1' b2', unionManyUniqSets [fvP, fvB1, fvB2])

rnExpr4 (ArithSeqIn seq)
  = rn_seq seq `thenRn4` \ (new_seq, fvs) ->
    returnRn4 (ArithSeqIn new_seq, fvs)
  where
    rn_seq (From expr)
     = rnExpr4 expr 	 `thenRn4` \ (expr', fvExpr) ->
       returnRn4 (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr4 expr1 	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr4 expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       returnRn4 (FromThen expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr4 expr1	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr4 expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       returnRn4 (FromTo expr1' expr2', fvExpr1 `unionUniqSets` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr4 expr1	 `thenRn4` \ (expr1', fvExpr1) ->
       rnExpr4 expr2	 `thenRn4` \ (expr2', fvExpr2) ->
       rnExpr4 expr3	 `thenRn4` \ (expr3', fvExpr3) ->
       returnRn4 (FromThenTo expr1' expr2' expr3',
		  unionManyUniqSets [fvExpr1, fvExpr2, fvExpr3])

#ifdef DPH
rnExpr4 (ParallelZF expr quals)
  = rnParQuals4 quals	  `thenRn4` \ ((quals',binds),fvQuals)->
    extendSS2  binds 
		(rnExpr4 expr) `thenRn4` \ (expr', fvExpr ) ->
    returnRn4 (ParallelZF expr' quals' , fvExpr `unionUniqSets` fvQuals)

rnExpr4 (ExplicitProcessor exprs expr)
  = rnExprs4 exprs	`thenRn4` \ (exprs',fvExprs) ->
    rnExpr4  expr	`thenRn4` \ (expr' ,fvExpr)  ->
    returnRn4 (ExplicitProcessor exprs' expr',fvExprs `unionUniqSets` fvExpr)

rnExpr4 (ExplicitPodIn exprs)
  = rnExprs4 exprs	`thenRn4` \ (exprs',fvExprs) ->
    returnRn4 (ExplicitPodIn exprs',fvExprs)

-- ExplicitPodOut : not in ProtoNameExprs (pops out of typechecker :-)

#endif {- Data Parallel Haskell -}

-- ArithSeqOut: not in ProtoNameExprs
\end{code}

%************************************************************************
%*									*
\subsubsection[dep-Quals]{@Qual@s: in list comprehensions}
%*									*
%************************************************************************

Note that although some bound vars may appear in the free var set for
the first qual, these will eventually be removed by the caller. For
example, if we have @[p | r <- s, q <- r, p <- q]@, when doing
@(AndQuals (q <- r) (p <- q))@, the free var set for @(q <- r)@ will
be @[r]@, and the free var set for the entire Quals will be @[r]@. This
@r@ will be removed only when we finally return from examining all the
Quals.

\begin{code}
rnQuals4 :: [ProtoNameQual]  -> Rn4M (([RenamedQual], [Name]), FreeVars)

rnQuals4 [qual]
  = rnQual4 qual `thenRn4` \ ((new_qual, bs), fvs) ->
    returnRn4 (([new_qual], bs), fvs)

rnQuals4 (qual: quals)
  = rnQual4 qual			`thenRn4` \ ((qual',  bs1), fvQuals1) ->
    extendSS2 bs1 (rnQuals4 quals)	`thenRn4` \ ((quals', bs2), fvQuals2) ->
    returnRn4
       ((qual' : quals', bs2 ++ bs1),	-- The ones on the right (bs2) shadow the
					-- ones on the left (bs1)
	fvQuals1 `unionUniqSets` fvQuals2)

rnQual4 (GeneratorQual pat expr)
  = rnExpr4 expr		 `thenRn4` \ (expr', fvExpr) ->
    let
	binders = collectPatBinders pat
    in
    getSrcLocRn4		 `thenRn4` \ src_loc ->
    namesFromProtoNames "variable in list-comprehension-generator pattern"
	 (binders `zip` repeat src_loc)	  `thenRn4` \ new_binders ->
    extendSS new_binders (rnPat4 pat) `thenRn4` \ pat' ->

    returnRn4 ((GeneratorQual pat' expr', new_binders), fvExpr)

rnQual4 (FilterQual expr)
  = rnExpr4 expr	 `thenRn4` \ (expr', fvs) ->
    returnRn4 ((FilterQual expr', []), fvs)
\end{code}

%************************************************************************
%*									*
%* Parallel Quals (in Parallel Zf expressions)				*
%*									*
%************************************************************************
\subsubsection[dep-ParQuals]{ParQuals}

\begin{code}
#ifdef DPH
rnPats4 :: [ProtoNamePat] -> Rn4M [RenamedPat]
rnPats4 [] = returnRn4 []
rnPats4 (pat:pats)
  = (rnPat4  pat) 	 	`thenRn4` (\ pat'  ->
    (rnPats4 pats)   	`thenRn4` (\ pats' ->
    returnRn4 (pat':pats') ))

rnParQuals4 :: ProtoNameParQuals  -> Rn4M ((RenamedParQuals, [Name]), FreeVars)

rnParQuals4 (AndParQuals q1 q2)
 = rnParQuals4 q1		`thenRn4` (\ ((quals1', bs1), fvQuals1) ->
   extendSS2 bs1 (rnParQuals4 q2)	
				`thenRn4` (\ ((quals2', bs2), fvQuals2) ->
   returnRn4 ((AndParQuals quals1' quals2', bs2 ++ bs1),
       		    fvQuals1 `unionUniqSets` fvQuals2) ))

	
rnParQuals4 (DrawnGenIn pats pat expr)
 = rnExpr4 expr		 `thenRn4` 	(\ (expr', fvExpr) ->
   let_1_0 (concat (map collectPatBinders pats))	(\ binders1 ->
   getSrcLocRn4		`thenRn4`		(\ src_loc ->
   namesFromProtoNames "variable in pattern" 
	(binders1 `zip` repeat src_loc)
				`thenRn4`		(\ binders1' ->
   extendSS binders1' (rnPats4 pats)		
				`thenRn4` 		(\ pats' ->
   let_1_0 (collectPatBinders pat)			(\ binders2 ->
   namesFromProtoNames "variable in pattern" 
	(binders2 `zip` repeat src_loc)
				`thenRn4`		(\ binders2' ->
   extendSS binders2' (rnPat4 pat)		
				`thenRn4` 		(\ pat' ->
   returnRn4 ((DrawnGenIn pats' pat' expr' , binders1' ++ binders2'),
		   fvExpr) ))))))))
   
rnParQuals4 (IndexGen exprs pat expr)
 = rnExpr4  expr		 `thenRn4` 	(\ (expr',  fvExpr) ->
   rnExprs4 exprs		 `thenRn4` 	(\ (exprs', fvExprs) ->
   let_1_0 (collectPatBinders pat)			(\ binders ->
   getSrcLocRn4		`thenRn4`		(\ src_loc ->
   namesFromProtoNames "variable in pattern" 
	(binders `zip` repeat src_loc)
				`thenRn4`	(\ binders' ->
   extendSS binders' (rnPat4 pat)		
				`thenRn4` 	(\ pat' ->
   returnRn4 ((IndexGen exprs' pat' expr' , binders'),
		   fvExpr `unionUniqSets` fvExprs) ))))))

rnParQuals4 (ParFilter expr)
 = rnExpr4 expr	 `thenRn4` (\  (expr', fvExpr) ->
   returnRn4         ((ParFilter expr', []), fvExpr) )
#endif {- Data Parallel Haskell -}
\end{code}
