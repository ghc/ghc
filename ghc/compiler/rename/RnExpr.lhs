%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1996
%
\section[RnExpr]{Renaming of expressions}

Basically dependency analysis.

Handles @Match@, @GRHSsAndBinds@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.

\begin{code}
#include "HsVersions.h"

module RnExpr (
	rnMatch, rnGRHSsAndBinds, rnPat,
	checkPrecMatch
   ) where

IMP_Ubiq()
IMPORT_DELOOPER(RnLoop)		-- break the RnPass/RnExpr/RnBinds loops

import HsSyn
import RdrHsSyn
import RnHsSyn
import RnMonad
import RnEnv
import PrelInfo		( numClass_RDR, fractionalClass_RDR, eqClass_RDR, ccallableClass_RDR,
			  creturnableClass_RDR, monadZeroClass_RDR, enumClass_RDR, ordClass_RDR,
			  negate_RDR
			)
import TysPrim		( charPrimTyCon, addrPrimTyCon, intPrimTyCon, 
			  floatPrimTyCon, doublePrimTyCon
			)
import TyCon		( TyCon )
import Id		( GenId )
import ErrUtils		( addErrLoc, addShortErrLocLine )
import Name
import Pretty
import Unique		( Unique, otherwiseIdKey )
import UniqFM		( lookupUFM{-, ufmToList ToDo:rm-} )
import UniqSet		( emptyUniqSet, unitUniqSet,
			  unionUniqSets, unionManyUniqSets,
			  SYN_IE(UniqSet)
			)
import PprStyle		( PprStyle(..) )
import Util		( Ord3(..), removeDups, panic, pprPanic, assertPanic )
\end{code}


*********************************************************
*							*
\subsection{Patterns}
*							*
*********************************************************

\begin{code}
rnPat :: RdrNamePat -> RnMS s RenamedPat

rnPat WildPatIn = returnRn WildPatIn

rnPat (VarPatIn name)
  = lookupBndrRn  name			`thenRn` \ vname ->
    returnRn (VarPatIn vname)

rnPat (LitPatIn lit) 
  = litOccurrence lit			`thenRn_`
    lookupImplicitOccRn eqClass_RDR	`thenRn_`	-- Needed to find equality on pattern
    returnRn (LitPatIn lit)

rnPat (LazyPatIn pat)
  = rnPat pat		`thenRn` \ pat' ->
    returnRn (LazyPatIn pat')

rnPat (AsPatIn name pat)
  = rnPat pat		`thenRn` \ pat' ->
    lookupBndrRn name	`thenRn` \ vname ->
    returnRn (AsPatIn vname pat')

rnPat (ConPatIn con pats)
  = lookupOccRn con	`thenRn` \ con' ->
    mapRn rnPat pats  	`thenRn` \ patslist ->
    returnRn (ConPatIn con' patslist)

rnPat (ConOpPatIn pat1 con _ pat2)
  = rnPat pat1		`thenRn` \ pat1' ->
    lookupOccRn con	`thenRn` \ con' ->
    lookupFixity con	`thenRn` \ fixity ->
    rnPat pat2		`thenRn` \ pat2' ->
    mkConOpPatRn pat1' con' fixity pat2'

-- Negated patters can only be literals, and they are dealt with
-- by negating the literal at compile time, not by using the negation
-- operation in Num.  So we don't need to make an implicit reference
-- to negate_RDR.
rnPat neg@(NegPatIn pat)
  = checkRn (valid_neg_pat pat) (negPatErr neg)
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

rnPat (NPlusKPatIn name lit)
  = litOccurrence lit			`thenRn_`
    lookupImplicitOccRn ordClass_RDR	`thenRn_`
    lookupBndrRn name			`thenRn` \ name' ->
    returnRn (NPlusKPatIn name' lit)

rnPat (ListPatIn pats)
  = addImplicitOccRn listType_name	`thenRn_` 
    mapRn rnPat pats 			`thenRn` \ patslist ->
    returnRn (ListPatIn patslist)

rnPat (TuplePatIn pats)
  = addImplicitOccRn (tupleType_name (length pats))	`thenRn_` 
    mapRn rnPat pats 					`thenRn` \ patslist ->
    returnRn (TuplePatIn patslist)

rnPat (RecPatIn con rpats)
  = lookupOccRn con 	`thenRn` \ con' ->
    rnRpats rpats	`thenRn` \ rpats' ->
    returnRn (RecPatIn con' rpats')
\end{code}

************************************************************************
*									*
\subsection{Match}
*									*
************************************************************************

\begin{code}
rnMatch :: RdrNameMatch -> RnMS s (RenamedMatch, FreeVars)

rnMatch (PatMatch pat match)
  = bindLocalsRn "pattern" binders	$ \ new_binders ->
    rnPat pat				`thenRn` \ pat' ->
    rnMatch match			`thenRn` \ (match', fvMatch) ->
    returnRn (PatMatch pat' match', fvMatch `minusNameSet` mkNameSet new_binders)
 where
    binders = collectPatBinders pat

rnMatch (GRHSMatch grhss_and_binds)
  = rnGRHSsAndBinds grhss_and_binds `thenRn` \ (grhss_and_binds', fvs) ->
    returnRn (GRHSMatch grhss_and_binds', fvs)
\end{code}

%************************************************************************
%*									*
\subsubsection{Guarded right-hand sides (GRHSsAndBinds)}
%*									*
%************************************************************************

\begin{code}
rnGRHSsAndBinds :: RdrNameGRHSsAndBinds -> RnMS s (RenamedGRHSsAndBinds, FreeVars)

rnGRHSsAndBinds (GRHSsAndBindsIn grhss binds)
  = rnBinds binds		$ \ binds' ->
    rnGRHSs grhss		`thenRn` \ (grhss', fvGRHS) ->
    returnRn (GRHSsAndBindsIn grhss' binds', fvGRHS)
  where
    rnGRHSs [] = returnRn ([], emptyNameSet)

    rnGRHSs (grhs:grhss)
      = rnGRHS  grhs   `thenRn` \ (grhs',  fvs) ->
	rnGRHSs grhss  `thenRn` \ (grhss', fvss) ->
	returnRn (grhs' : grhss', fvs `unionNameSets` fvss)

    rnGRHS (GRHS guard expr locn)
      = pushSrcLocRn locn $		    
	rnExpr guard	`thenRn` \ (guard', fvsg) ->
	rnExpr expr	`thenRn` \ (expr',  fvse) ->

	-- Turn an "otherwise" guard into an OtherwiseGRHS.
	-- This is the first moment that we can be sure we havn't got a shadowed binding
	-- of "otherwise".
	let grhs' = case guard' of
			HsVar v | uniqueOf v == otherwiseIdKey -> OtherwiseGRHS expr' locn
			other				       -> GRHS guard' expr' locn			   
	in
	returnRn (grhs', fvsg `unionNameSets` fvse)

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
rnExprs :: [RdrNameHsExpr] -> RnMS s ([RenamedHsExpr], FreeVars)
rnExprs ls =
 rnExprs' ls [] `thenRn` \  (exprs, fvExprs) ->
 returnRn (exprs, unionManyNameSets fvExprs)

rnExprs' [] acc = returnRn ([], acc)
rnExprs' (expr:exprs) acc
  = rnExpr expr 	        `thenRn` \ (expr', fvExpr) ->
    rnExprs' exprs (fvExpr:acc)	`thenRn` \ (exprs', fvExprs) ->
    returnRn (expr':exprs', fvExprs)
\end{code}

Variables. We look up the variable and return the resulting name.  The
interesting question is what the free-variable set should be.  We
don't want to return imported or prelude things as free vars.  So we
look at the Name returned from the lookup, and make it part of the
free-var set iff if it's a LocallyDefined Name.
\end{itemize}

\begin{code}
rnExpr :: RdrNameHsExpr -> RnMS s (RenamedHsExpr, FreeVars)

rnExpr (HsVar v)
  = lookupOccRn v	`thenRn` \ vname ->
    returnRn (HsVar vname, if isLocallyDefined vname
			   then unitNameSet vname
			   else emptyUniqSet)

rnExpr (HsLit lit) 
  = litOccurrence lit		`thenRn_`
    returnRn (HsLit lit, emptyNameSet)

rnExpr (HsLam match)
  = rnMatch match	`thenRn` \ (match', fvMatch) ->
    returnRn (HsLam match', fvMatch)

rnExpr (HsApp fun arg)
  = rnExpr fun		`thenRn` \ (fun',fvFun) ->
    rnExpr arg		`thenRn` \ (arg',fvArg) ->
    returnRn (HsApp fun' arg', fvFun `unionNameSets` fvArg)

rnExpr (OpApp e1 op@(HsVar op_name) _ e2) 
  = rnExpr e1				`thenRn` \ (e1', fv_e1) ->
    rnExpr e2				`thenRn` \ (e2', fv_e2) ->
    rnExpr op				`thenRn` \ (op', fv_op) ->

	-- Deal wth fixity
    lookupFixity op_name		`thenRn` \ fixity ->
    getModeRn				`thenRn` \ mode -> 
    (case mode of
	SourceMode    -> mkOpAppRn e1' op' fixity e2'
	InterfaceMode -> returnRn (OpApp e1' op' fixity e2')
    )					`thenRn` \ final_e -> 

    returnRn (final_e,
	      fv_e1 `unionNameSets` fv_op `unionNameSets` fv_e2)

rnExpr (NegApp e n)
  = rnExpr e				`thenRn` \ (e', fv_e) ->
    lookupImplicitOccRn negate_RDR	`thenRn` \ neg ->
    getModeRn				`thenRn` \ mode -> 
    mkNegAppRn mode e' (HsVar neg)	`thenRn` \ final_e ->
    returnRn (final_e, fv_e)

rnExpr (HsPar e)
  = rnExpr e 		`thenRn` \ (e', fvs_e) ->
    returnRn (HsPar e', fvs_e)

rnExpr (SectionL expr op)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    rnExpr op	 	`thenRn` \ (op', fvs_op) ->
    returnRn (SectionL expr' op', fvs_op `unionNameSets` fvs_expr)

rnExpr (SectionR op expr)
  = rnExpr op	 	`thenRn` \ (op',   fvs_op) ->
    rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (SectionR op' expr', fvs_op `unionNameSets` fvs_expr)

rnExpr (CCall fun args may_gc is_casm fake_result_ty)
  = lookupImplicitOccRn ccallableClass_RDR	`thenRn_`
    lookupImplicitOccRn creturnableClass_RDR	`thenRn_`
    rnExprs args				`thenRn` \ (args', fvs_args) ->
    returnRn (CCall fun args' may_gc is_casm fake_result_ty, fvs_args)

rnExpr (HsSCC label expr)
  = rnExpr expr	 	`thenRn` \ (expr', fvs_expr) ->
    returnRn (HsSCC label expr', fvs_expr)

rnExpr (HsCase expr ms src_loc)
  = pushSrcLocRn src_loc $
    rnExpr expr		 	`thenRn` \ (new_expr, e_fvs) ->
    mapAndUnzipRn rnMatch ms	`thenRn` \ (new_ms, ms_fvs) ->
    returnRn (HsCase new_expr new_ms src_loc, unionManyNameSets (e_fvs : ms_fvs))

rnExpr (HsLet binds expr)
  = rnBinds binds		$ \ binds' ->
    rnExpr expr			 `thenRn` \ (expr',fvExpr) ->
    returnRn (HsLet binds' expr', fvExpr)

rnExpr (HsDo do_or_lc stmts src_loc)
  = pushSrcLocRn src_loc $
    lookupImplicitOccRn monadZeroClass_RDR	`thenRn_`	-- Forces Monad to come too
    rnStmts stmts				`thenRn` \ (stmts', fvStmts) ->
    returnRn (HsDo do_or_lc stmts' src_loc, fvStmts)

rnExpr (ExplicitList exps)
  = addImplicitOccRn listType_name	`thenRn_` 
    rnExprs exps		 	`thenRn` \ (exps', fvs) ->
    returnRn  (ExplicitList exps', fvs)

rnExpr (ExplicitTuple exps)
  = addImplicitOccRn (tupleType_name (length exps))	`thenRn_` 
    rnExprs exps	 				`thenRn` \ (exps', fvExps) ->
    returnRn (ExplicitTuple exps', fvExps)

rnExpr (RecordCon (HsVar con) rbinds)
  = lookupOccRn con 			`thenRn` \ conname ->
    rnRbinds "construction" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordCon (HsVar conname) rbinds', fvRbinds)

rnExpr (RecordUpd expr rbinds)
  = rnExpr expr			`thenRn` \ (expr', fvExpr) ->
    rnRbinds "update" rbinds	`thenRn` \ (rbinds', fvRbinds) ->
    returnRn (RecordUpd expr' rbinds', fvExpr `unionNameSets` fvRbinds)

rnExpr (ExprWithTySig expr pty)
  = rnExpr expr			 	`thenRn` \ (expr', fvExpr) ->
    rnHsType pty			`thenRn` \ pty' ->
    returnRn (ExprWithTySig expr' pty', fvExpr)

rnExpr (HsIf p b1 b2 src_loc)
  = pushSrcLocRn src_loc $
    rnExpr p		`thenRn` \ (p', fvP) ->
    rnExpr b1		`thenRn` \ (b1', fvB1) ->
    rnExpr b2		`thenRn` \ (b2', fvB2) ->
    returnRn (HsIf p' b1' b2' src_loc, unionManyNameSets [fvP, fvB1, fvB2])

rnExpr (ArithSeqIn seq)
  = lookupImplicitOccRn enumClass_RDR	`thenRn_`
    rn_seq seq	 			`thenRn` \ (new_seq, fvs) ->
    returnRn (ArithSeqIn new_seq, fvs)
  where
    rn_seq (From expr)
     = rnExpr expr 	`thenRn` \ (expr', fvExpr) ->
       returnRn (From expr', fvExpr)

    rn_seq (FromThen expr1 expr2)
     = rnExpr expr1 	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromThen expr1' expr2', fvExpr1 `unionNameSets` fvExpr2)

    rn_seq (FromTo expr1 expr2)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       returnRn (FromTo expr1' expr2', fvExpr1 `unionNameSets` fvExpr2)

    rn_seq (FromThenTo expr1 expr2 expr3)
     = rnExpr expr1	`thenRn` \ (expr1', fvExpr1) ->
       rnExpr expr2	`thenRn` \ (expr2', fvExpr2) ->
       rnExpr expr3	`thenRn` \ (expr3', fvExpr3) ->
       returnRn (FromThenTo expr1' expr2' expr3',
		  unionManyNameSets [fvExpr1, fvExpr2, fvExpr3])
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
    returnRn (rbinds', unionManyNameSets fvRbind_s)
  where
    (_, dup_fields) = removeDups cmp [ f | (f,_,_) <- rbinds ]

    field_dup_err dups = addErrRn (dupFieldErr str dups)

    rn_rbind (field, expr, pun)
      = lookupGlobalOccRn field	`thenRn` \ fieldname ->
	rnExpr expr		`thenRn` \ (expr', fvExpr) ->
	returnRn ((fieldname, expr', pun), fvExpr)

rnRpats rpats
  = mapRn field_dup_err dup_fields 	`thenRn_`
    mapRn rn_rpat rpats
  where
    (_, dup_fields) = removeDups cmp [ f | (f,_,_) <- rpats ]

    field_dup_err dups = addErrRn (dupFieldErr "pattern" dups)

    rn_rpat (field, pat, pun)
      = lookupGlobalOccRn field	`thenRn` \ fieldname ->
	rnPat pat		`thenRn` \ pat' ->
	returnRn (fieldname, pat', pun)
\end{code}

%************************************************************************
%*									*
\subsubsection{@Stmt@s: in @do@ expressions}
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
rnStmts :: [RdrNameStmt] -> RnMS s ([RenamedStmt], FreeVars)

rnStmts [] = returnRn ([], emptyNameSet)

rnStmts (stmt:stmts)
  = rnStmt stmt				$ \ stmt' ->
    rnStmts stmts			`thenRn` \ (stmts', fv_stmts) ->
    returnRn (stmt':stmts', fv_stmts)


-- rnStmt :: RdrNameStmt -> (RenamedStmt -> RnMS s (a, FreeVars)) -> RnMS s (a, FreeVars)
-- Because of mutual recursion the actual type is a bit less general than this [Haskell 1.2]

rnStmt (BindStmt pat expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rnExpr expr			 			`thenRn` \ (expr', fv_expr) ->
    bindLocalsRn "pattern in do binding" binders	$ \ new_binders ->
    rnPat pat					 	`thenRn` \ pat' ->

    thing_inside (BindStmt pat' expr' src_loc)		`thenRn` \ (result, fvs) -> 
    returnRn (result, fv_expr `unionNameSets` (fvs `minusNameSet` mkNameSet new_binders))
  where
    binders = collectPatBinders pat

rnStmt (ExprStmt expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rnExpr expr	 				`thenRn` \ (expr', fv_expr) ->
    thing_inside (ExprStmt expr' src_loc)	`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `unionNameSets` fvs)

rnStmt (GuardStmt expr src_loc) thing_inside
  = pushSrcLocRn src_loc $
    rnExpr expr	 				`thenRn` \ (expr', fv_expr) ->
    thing_inside (GuardStmt expr' src_loc)	`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `unionNameSets` fvs)

rnStmt (ReturnStmt expr) thing_inside
  = rnExpr expr	 				`thenRn` \ (expr', fv_expr) ->
    thing_inside (ReturnStmt expr')		`thenRn` \ (result, fvs) ->
    returnRn (result, fv_expr `unionNameSets` fvs)

rnStmt (LetStmt binds) thing_inside
  = rnBinds binds		$ \ binds' ->
    thing_inside (LetStmt binds')
\end{code}

%************************************************************************
%*									*
\subsubsection{Precedence Parsing}
%*									*
%************************************************************************

@mkOpAppRn@ deals with operator fixities.  The argument expressions
are assumed to be already correctly arranged.  It needs the fixities
recorded in the OpApp nodes, because fixity info applies to the things
the programmer actually wrote, so you can't find it out from the Name.

Furthermore, the second argument is guaranteed not to be another
operator application.  Why? Because the parser parses all
operator appications left-associatively.

\begin{code}
mkOpAppRn :: RenamedHsExpr -> RenamedHsExpr -> Fixity -> RenamedHsExpr
	  -> RnMS s RenamedHsExpr

mkOpAppRn e1@(OpApp e11 op1 fix1 e12) 
	  op2 fix2 e2
  | nofix_error
  = addErrRn (precParseErr (get op1,fix1) (get op2,fix2))	`thenRn_`
    returnRn (OpApp e1 op2 fix2 e2)

  | rearrange_me
  = mkOpAppRn e12 op2 fix2 e2		`thenRn` \ new_e ->
    returnRn (OpApp e11 op1 fix1 new_e)
  where
    (nofix_error, rearrange_me) = compareFixity fix1 fix2
    get (HsVar n) = n

mkOpAppRn e1@(NegApp neg_arg neg_id) 
	  op2 
	  fix2@(Fixity prec2 dir2)
	  e2
  | prec2 > 6 	-- Precedence of unary - is wired in as 6!
  = mkOpAppRn neg_arg op2 fix2 e2	`thenRn` \ new_e ->
    returnRn (NegApp new_e neg_id)

mkOpAppRn e1 op fix e2 			-- Default case, no rearrangment
  = ASSERT( right_op_ok fix e2 )
    returnRn (OpApp e1 op fix e2)

-- Parser left-associates everything, but 
-- derived instances may have correctly-associated things to
-- in the right operarand.  So we just check that the right operand is OK
right_op_ok fix1 (OpApp _ _ fix2 _)
  = not error_please && associate_right
  where
    (error_please, associate_right) = compareFixity fix1 fix2
right_op_ok fix1 other
  = True

-- Parser initially makes negation bind more tightly than any other operator
mkNegAppRn mode neg_arg neg_id
  = ASSERT( not_op_app mode neg_arg )
    returnRn (NegApp neg_arg neg_id)

not_op_app SourceMode (OpApp _ _ _ _) = False
not_op_app mode other	 	      = True
\end{code}

\begin{code}
mkConOpPatRn :: RenamedPat -> Name -> Fixity -> RenamedPat
	     -> RnMS s RenamedPat

mkConOpPatRn p1@(ConOpPatIn p11 op1 fix1 p12) 
	     op2 fix2 p2
  | nofix_error
  = addErrRn (precParseErr (op1,fix1) (op2,fix2))	`thenRn_`
    returnRn (ConOpPatIn p1 op2 fix2 p2)

  | rearrange_me
  = mkConOpPatRn p12 op2 fix2 p2		`thenRn` \ new_p ->
    returnRn (ConOpPatIn p11 op1 fix1 new_p)

  where
    (nofix_error, rearrange_me) = compareFixity fix1 fix2

mkConOpPatRn p1@(NegPatIn neg_arg) 
	  op2 
	  fix2@(Fixity prec2 dir2)
	  p2
  | prec2 > 6 	-- Precedence of unary - is wired in as 6!
  = addErrRn (precParseNegPatErr (op2,fix2))	`thenRn_`
    returnRn (ConOpPatIn p1 op2 fix2 p2)

mkConOpPatRn p1 op fix p2 			-- Default case, no rearrangment
  = ASSERT( not_op_pat p2 )
    returnRn (ConOpPatIn p1 op fix p2)

not_op_pat (ConOpPatIn _ _ _ _) = False
not_op_pat other   	        = True
\end{code}

\begin{code}
checkPrecMatch :: Bool -> RdrName -> RdrNameMatch -> RnMS s ()

checkPrecMatch False fn match
  = returnRn ()
checkPrecMatch True op (PatMatch p1 (PatMatch p2 (GRHSMatch _)))
  = checkPrec op p1 False	`thenRn_`
    checkPrec op p2 True
checkPrecMatch True op _
  = panic "checkPrecMatch"

checkPrec op (ConOpPatIn _ op1 _ _) right
  = lookupFixity op	`thenRn` \  op_fix@(Fixity op_prec  op_dir) ->
    lookupFixity op1	`thenRn` \ op1_fix@(Fixity op1_prec op1_dir) ->
    let
	inf_ok = op1_prec > op_prec || 
	         (op1_prec == op_prec &&
		  (op1_dir == InfixR && op_dir == InfixR && right ||
		   op1_dir == InfixL && op_dir == InfixL && not right))

	info  = (op,op_fix)
	info1 = (op1,op1_fix)
	(infol, infor) = if right then (info, info1) else (info1, info)
    in
    checkRn inf_ok (precParseErr infol infor)

checkPrec op (NegPatIn _) right
  = lookupFixity op	`thenRn` \ op_fix@(Fixity op_prec op_dir) ->
    checkRn (op_prec <= 6) (precParseNegPatErr (op,op_fix))

checkPrec op pat right
  = returnRn ()
\end{code}

Consider
	a `op1` b `op2` c

(compareFixity op1 op2) tells which way to arrange appication, or
whether there's an error.

\begin{code}
compareFixity :: Fixity -> Fixity
	      -> (Bool,		-- Error please
		  Bool)		-- Associate to the right: a op1 (b op2 c)
compareFixity (Fixity prec1 dir1) (Fixity prec2 dir2)
  = case prec1 `cmp` prec2 of
	GT_ -> left
	LT_ -> right
	EQ_ -> case (dir1, dir2) of
			(InfixR, InfixR) -> right
			(InfixL, InfixL) -> left
			_		 -> error_please
  where
    right	 = (False, True)
    left         = (False, False)
    error_please = (True,  False)
\end{code}

%************************************************************************
%*									*
\subsubsection{Literals}
%*									*
%************************************************************************

When literals occur we have to make sure that the types and classes they involve
are made available.

\begin{code}
litOccurrence (HsChar _)
  = addImplicitOccRn charType_name

litOccurrence (HsCharPrim _)
  = addImplicitOccRn (getName charPrimTyCon)

litOccurrence (HsString _)
  = addImplicitOccRn listType_name	`thenRn_`
    addImplicitOccRn charType_name

litOccurrence (HsStringPrim _)
  = addImplicitOccRn (getName addrPrimTyCon)

litOccurrence (HsInt _)
  = lookupImplicitOccRn numClass_RDR			-- Int and Integer are forced in by Num

litOccurrence (HsFrac _)
  = lookupImplicitOccRn fractionalClass_RDR		-- ... similarly Rational

litOccurrence (HsIntPrim _)
  = addImplicitOccRn (getName intPrimTyCon)

litOccurrence (HsFloatPrim _)
  = addImplicitOccRn (getName floatPrimTyCon)

litOccurrence (HsDoublePrim _)
  = addImplicitOccRn (getName doublePrimTyCon)

litOccurrence (HsLitLit _)
  = lookupImplicitOccRn ccallableClass_RDR
\end{code}


%************************************************************************
%*									*
\subsubsection{Errors}
%*									*
%************************************************************************

\begin{code}
dupFieldErr str (dup:rest) sty
  = ppBesides [ppPStr SLIT("duplicate field name `"), 
               ppr sty dup, 
	       ppPStr SLIT("' in record "), ppStr str]

negPatErr pat  sty
  = ppSep [ppPStr SLIT("prefix `-' not applied to literal in pattern"), ppr sty pat]

precParseNegPatErr op sty 
  = ppHang (ppPStr SLIT("precedence parsing error"))
      4 (ppBesides [ppPStr SLIT("prefix `-' has lower precedence than "), 
		    pp_op sty op, 
		    ppPStr SLIT(" in pattern")])

precParseErr op1 op2  sty
  = ppHang (ppPStr SLIT("precedence parsing error"))
      4 (ppBesides [ppPStr SLIT("cannot mix "), pp_op sty op1, ppPStr SLIT(" and "), pp_op sty op2,
	 	    ppPStr SLIT(" in the same infix expression")])

pp_op sty (op, fix) = ppBesides [pprSym sty op, ppLparen, ppr sty fix, ppRparen]
\end{code}
