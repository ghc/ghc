\%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchLambda, tcStmts, tcGRHSs ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcExpr )

import HsSyn		( HsBinds(..), Match(..), GRHSs(..), GRHS(..),
			  MonoBinds(..), StmtCtxt(..), Stmt(..),
			  pprMatch, getMatchLoc
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt )

import TcMonad
import TcMonoType	( checkSigTyVars, tcHsTyVar, tcHsType, noSigs, sigPatCtxt )
import Inst		( Inst, LIE, plusLIE, emptyLIE, plusLIEs )
import TcEnv		( tcExtendLocalValEnv, tcExtendGlobalTyVars, tcExtendTyVarEnv )
import TcPat		( tcPat, polyPatSig )
import TcType		( TcType, newTyVarTy, newTyVarTy_OpenKind )
import TcBinds		( tcBindsAndThen )
import TcSimplify	( tcSimplifyAndCheck, bindInstsOfLocalFuns )
import TcUnify		( unifyFunTy, unifyTauTy )
import Name		( Name )
import TysWiredIn	( boolTy )

import BasicTypes	( RecFlag(..) )
import Type		( Kind, tyVarsOfType, isTauTy, mkFunTy, boxedTypeKind )
import VarSet
import Var		( Id )
import Util
import Bag
import Outputable
import List		( nub )
\end{code}

%************************************************************************
%*									*
\subsection{tcMatchesFun, tcMatchesCase}
%*									*
%************************************************************************

@tcMatchesFun@ typechecks a @[Match]@ list which occurs in a
@FunMonoBind@.  The second argument is the name of the function, which
is used in error messages.  It checks that all the equations have the
same number of arguments before using @tcMatches@ to do the work.

\begin{code}
tcMatchesFun :: [(Name,Id)]	-- Bindings for the variables bound in this group
	     -> Name
	     -> TcType 		-- Expected type
	     -> [RenamedMatch]
	     -> TcM s ([TcMatch], LIE)

tcMatchesFun xve fun_name expected_ty matches@(first_match:_)
  =	 -- Check that they all have the same no of arguments
	 -- Set the location to that of the first equation, so that
	 -- any inter-equation error messages get some vaguely
	 -- sensible location.	Note: we have to do this odd
	 -- ann-grabbing, because we don't always have annotations in
	 -- hand when we call tcMatchesFun...
    tcAddSrcLoc (getMatchLoc first_match)	 (
	    checkTc (sameNoOfArgs matches)
		    (varyingArgsErr fun_name matches)
    )						 `thenTc_`

	-- ToDo: Don't use "expected" stuff if there ain't a type signature
	-- because inconsistency between branches
	-- may show up as something wrong with the (non-existent) type signature

	-- No need to zonk expected_ty, because unifyFunTy does that on the fly
    tcMatches xve matches expected_ty (FunRhs fun_name)
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: [RenamedMatch]		-- The case alternatives
	      -> TcType 		-- Type of whole case expressions
	      -> TcM s (TcType,		-- Inferred type of the scrutinee
			[TcMatch], 	-- Translated alternatives
			LIE)

tcMatchesCase matches expr_ty
  = newTyVarTy_OpenKind 					`thenNF_Tc` \ scrut_ty ->
    tcMatches [] matches (mkFunTy scrut_ty expr_ty) CaseAlt	`thenTc` \ (matches', lie) ->
    returnTc (scrut_ty, matches', lie)

tcMatchLambda :: RenamedMatch -> TcType -> TcM s (TcMatch, LIE)
tcMatchLambda match res_ty = tcMatch [] match res_ty LambdaBody
\end{code}


\begin{code}
tcMatches :: [(Name,Id)]
	  -> [RenamedMatch]
	  -> TcType
	  -> StmtCtxt
	  -> TcM s ([TcMatch], LIE)

tcMatches xve matches expected_ty fun_or_case
  = mapAndUnzipTc tc_match matches	`thenTc` \ (matches, lies) ->
    returnTc (matches, plusLIEs lies)
  where
    tc_match match = tcMatch xve match expected_ty fun_or_case
\end{code}


%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatch :: [(Name,Id)]
	-> RenamedMatch
	-> TcType 		-- Expected result-type of the Match.
				-- Early unification with this guy gives better error messages
	-> StmtCtxt
	-> TcM s (TcMatch, LIE)

tcMatch xve1 match@(Match sig_tvs pats maybe_rhs_sig grhss) expected_ty ctxt
  = tcAddSrcLoc (getMatchLoc match)		$
    tcAddErrCtxt (matchCtxt ctxt match)		$

    if null sig_tvs then	-- The common case
	tc_match expected_ty	`thenTc` \ (_, match_and_lie) ->
	returnTc match_and_lie

    else
	-- If there are sig tve we must be careful *not* to use
	-- expected_ty right away, else we'll unify with tyvars free
	-- in the envt.  So invent a fresh tyvar and use that instead
	newTyVarTy_OpenKind		`thenNF_Tc` \ tyvar_ty ->

	-- Extend the tyvar env and check the match itself
	mapNF_Tc tcHsTyVar sig_tvs 	`thenNF_Tc` \ sig_tyvars ->
	tcExtendTyVarEnv sig_tyvars (
		tc_match tyvar_ty
	)				`thenTc` \ (pat_ids, match_and_lie) ->

	-- Check that the scoped type variables from the patterns
	-- have not been constrained
        tcAddErrCtxtM (sigPatCtxt sig_tyvars pat_ids)		(
		checkSigTyVars sig_tyvars
	)							`thenTc_`

	-- *Now* we're free to unify with expected_ty
	unifyTauTy expected_ty tyvar_ty	`thenTc_`

	returnTc match_and_lie

  where
    tc_match expexted_ty 	-- Any sig tyvars are in scope by now
      = -- STEP 1: Typecheck the patterns
	tcMatchPats pats expected_ty	`thenTc` \ (rhs_ty, pats', lie_req1, ex_tvs, pat_bndrs, lie_avail) ->
        let
	  xve2       = bagToList pat_bndrs
	  pat_ids    = map snd xve2
	  ex_tv_list = bagToList ex_tvs
        in

	-- STEP 2: Check that the remaining "expected type" is not a rank-2 type
	-- If it is it'll mess up the unifier when checking the RHS
	checkTc (isTauTy rhs_ty) lurkingRank2SigErr 		`thenTc_`

	-- STEP 3: Unify with the rhs type signature if any
	(case maybe_rhs_sig of
	    Nothing  -> returnTc ()
	    Just sig -> tcHsType sig	`thenTc` \ sig_ty ->

			-- Check that the signature isn't a polymorphic one, which
			-- we don't permit (at present, anyway)
		        checkTc (isTauTy sig_ty) (polyPatSig sig_ty)	`thenTc_`
		        unifyTauTy rhs_ty sig_ty
	)						`thenTc_`

	-- STEP 4: Typecheck the guarded RHSs and the associated where clause
	tcExtendLocalValEnv xve1 (tcExtendLocalValEnv xve2 (
	    tcGRHSs grhss rhs_ty ctxt
	))					`thenTc` \ (grhss', lie_req2) ->

	-- STEP 5: Check for existentially bound type variables
	tcExtendGlobalTyVars (tyVarsOfType rhs_ty)	(
	    tcAddErrCtxtM (sigPatCtxt ex_tv_list pat_ids)	$
	    checkSigTyVars ex_tv_list				`thenTc` \ zonked_ex_tvs ->
	    tcSimplifyAndCheck 
		(text ("the existential context of a data constructor"))
		(mkVarSet zonked_ex_tvs)
		lie_avail (lie_req1 `plusLIE` lie_req2)
	)							`thenTc` \ (lie_req', ex_binds) ->

    	-- STEP 6 In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
	bindInstsOfLocalFuns lie_req' pat_ids	 	`thenTc` \ (lie_req'', inst_binds) ->

	-- Phew!  All done.
	let
            grhss'' = glue_on Recursive ex_binds $
		      glue_on Recursive inst_binds grhss'
	in
	returnTc (pat_ids, (Match [] pats' Nothing grhss'', lie_req''))

	-- glue_on just avoids stupid dross
glue_on _ EmptyMonoBinds grhss = grhss		-- The common case
glue_on is_rec mbinds (GRHSs grhss binds ty)
  = GRHSs grhss (MonoBind mbinds [] is_rec `ThenBinds` binds) ty

tcGRHSs :: RenamedGRHSs
	-> TcType -> StmtCtxt
	-> TcM s (TcGRHSs, LIE)

tcGRHSs (GRHSs grhss binds _) expected_ty ctxt
  = tcBindsAndThen glue_on binds (tc_grhss grhss)
  where
    tc_grhss grhss
	= mapAndUnzipTc tc_grhs grhss		`thenTc` \ (grhss', lies) ->
	  returnTc (GRHSs grhss' EmptyBinds (Just expected_ty), plusLIEs lies)

    tc_grhs (GRHS guarded locn)
	= tcAddSrcLoc locn				$
	  tcStmts ctxt (\ty -> ty) guarded expected_ty	`thenTc` \ (guarded', lie) ->
	  returnTc (GRHS guarded' locn, lie)
\end{code}


%************************************************************************
%*									*
\subsection{tcMatchPats}
%*									*
%************************************************************************

\begin{code}
tcMatchPats [] expected_ty
  = returnTc (expected_ty, [], emptyLIE, emptyBag, emptyBag, emptyLIE)

tcMatchPats (pat:pats) expected_ty
  = unifyFunTy expected_ty	`thenTc` \ (arg_ty, rest_ty) ->
    tcPat noSigs pat arg_ty	`thenTc` \ (pat', lie_req, pat_tvs, pat_ids, lie_avail) ->
    tcMatchPats pats rest_ty	`thenTc` \ (rhs_ty, pats', lie_reqs, pats_tvs, pats_ids, lie_avails) ->
    returnTc (	rhs_ty, 
		pat':pats',
		lie_req `plusLIE` lie_reqs,
		pat_tvs `unionBags` pats_tvs,
		pat_ids `unionBags` pats_ids,
		lie_avail `plusLIE` lie_avails
    )
\end{code}


%************************************************************************
%*									*
\subsection{tcStmts}
%*									*
%************************************************************************


\begin{code}
tcStmts :: StmtCtxt
        -> (TcType -> TcType)	-- m, the relationship type of pat and rhs in pat <- rhs
        -> [RenamedStmt]
	-> TcType			-- elt_ty, where type of the comprehension is (m elt_ty)
        -> TcM s ([TcStmt], LIE)

tcStmts do_or_lc m (stmt@(ReturnStmt exp) : stmts) elt_ty
  = ASSERT( null stmts )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) 	$
    tcExpr exp elt_ty				`thenTc`    \ (exp', exp_lie) ->
    returnTc ([ReturnStmt exp'], exp_lie)

	-- ExprStmt at the end
tcStmts do_or_lc m [stmt@(ExprStmt exp src_loc)] elt_ty
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) 	$
    tcExpr exp (m elt_ty)			`thenTc`    \ (exp', exp_lie) ->
    returnTc ([ExprStmt exp' src_loc], exp_lie)

	-- ExprStmt not at the end
tcStmts do_or_lc m (stmt@(ExprStmt exp src_loc) : stmts) elt_ty
  = ASSERT( isDoStmt do_or_lc )
    tcAddSrcLoc src_loc 		(
	tcSetErrCtxt (stmtCtxt do_or_lc stmt)	$
	    -- exp has type (m tau) for some tau (doesn't matter what)
  	newTyVarTy_OpenKind			`thenNF_Tc` \ any_ty ->
  	tcExpr exp (m any_ty)
    )					`thenTc` \ (exp', exp_lie) ->
    tcStmts do_or_lc m stmts elt_ty	`thenTc` \ (stmts', stmts_lie) ->
    returnTc (ExprStmt exp' src_loc : stmts',
  	      exp_lie `plusLIE` stmts_lie)

tcStmts do_or_lc m (stmt@(GuardStmt exp src_loc) : stmts) elt_ty
  = ASSERT( not (isDoStmt do_or_lc) )
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	tcAddSrcLoc src_loc 		$
  	tcExpr exp boolTy
    )					`thenTc` \ (exp', exp_lie) ->
    tcStmts do_or_lc m stmts elt_ty	`thenTc` \ (stmts', stmts_lie) ->
    returnTc (GuardStmt exp' src_loc : stmts',
  	      exp_lie `plusLIE` stmts_lie)

tcStmts do_or_lc m (stmt@(BindStmt pat exp src_loc) : stmts) elt_ty
  = tcAddSrcLoc src_loc		(
	tcSetErrCtxt (stmtCtxt do_or_lc stmt)	$
    	newTyVarTy boxedTypeKind		`thenNF_Tc` \ pat_ty ->
  	tcPat noSigs pat pat_ty			`thenTc` \ (pat', pat_lie, pat_tvs, pat_ids, avail) ->  
      	tcExpr exp (m pat_ty)			`thenTc` \ (exp', exp_lie) ->
  	returnTc (pat', exp',
		  pat_lie `plusLIE` exp_lie,
		  pat_tvs, pat_ids, avail)
    )					`thenTc` \ (pat', exp', lie_req, pat_tvs, pat_bndrs, lie_avail) ->
    let
	new_val_env = bagToList pat_bndrs
	pat_ids     = map snd new_val_env
	pat_tv_list = bagToList pat_tvs
    in

	-- Do the rest; we don't need to add the pat_tvs to the envt
	-- because they all appear in the pat_ids's types
    tcExtendLocalValEnv new_val_env (
       tcStmts do_or_lc m stmts elt_ty
    )						`thenTc` \ (stmts', stmts_lie) ->


	-- Reinstate context for existential checks
    tcSetErrCtxt (stmtCtxt do_or_lc stmt)		$
    tcExtendGlobalTyVars (tyVarsOfType (m elt_ty))	$
    tcAddErrCtxtM (sigPatCtxt pat_tv_list pat_ids)	$

    checkSigTyVars pat_tv_list				`thenTc` \ zonked_pat_tvs ->

    tcSimplifyAndCheck 
	(text ("the existential context of a data constructor"))
	(mkVarSet zonked_pat_tvs)
	lie_avail stmts_lie			`thenTc` \ (final_lie, dict_binds) ->

    returnTc (BindStmt pat' exp' src_loc : 
	        LetStmt (MonoBind dict_binds [] Recursive) :
	          stmts',
  	      lie_req `plusLIE` final_lie)

tcStmts do_or_lc m (LetStmt binds : stmts) elt_ty
     = tcBindsAndThen		-- No error context, but a binding group is
  	combine			-- rather a large thing for an error context anyway
  	binds
  	(tcStmts do_or_lc m stmts elt_ty)
     where
      	combine is_rec binds' stmts' = LetStmt (MonoBind binds' [] is_rec) : stmts'


isDoStmt DoStmt = True
isDoStmt other  = False
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

@sameNoOfArgs@ takes a @[RenamedMatch]@ and decides whether the same
number of args are used in each equation.

\begin{code}
sameNoOfArgs :: [RenamedMatch] -> Bool
sameNoOfArgs matches = length (nub (map args_in_match matches)) == 1
  where
    args_in_match :: RenamedMatch -> Int
    args_in_match (Match _ pats _ _) = length pats
\end{code}

\begin{code}
matchCtxt CaseAlt match
  = hang (ptext SLIT("In a \"case\" branch:"))
	 4 (pprMatch (True,empty) {-is_case-} match)

matchCtxt (FunRhs fun) match
  = hang (hcat [ptext SLIT("In an equation for function "), quotes (ppr_fun), char ':'])
	 4 (pprMatch (False, ppr_fun) {-not case-} match)
  where
    ppr_fun = ppr fun

matchCtxt LambdaBody match
  = hang (ptext SLIT("In the lambda expression"))
	 4 (pprMatch (True, empty) match)

varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

lurkingRank2SigErr
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")

stmtCtxt do_or_lc stmt
  = hang (ptext SLIT("In") <+> what <> colon)
         4 (ppr stmt)
  where
    what = case do_or_lc of
		ListComp -> ptext SLIT("a list-comprehension qualifier")
		DoStmt   -> ptext SLIT("a do statement:")
		PatBindRhs -> thing <+> ptext SLIT("a pattern binding")
		FunRhs f   -> thing <+> ptext SLIT("an equation for") <+> quotes (ppr f)
		CaseAlt	   -> thing <+> ptext SLIT("a case alternative")
		LambdaBody -> thing <+> ptext SLIT("a lambda abstraction")
    thing = case stmt of
		BindStmt _ _ _ -> ptext SLIT("a pattern guard for")
		GuardStmt _ _  -> ptext SLIT("a guard for")
		ExprStmt _ _   -> ptext SLIT("the right-hand side of")
\end{code}
