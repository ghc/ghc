%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchLambda, 
		   tcStmts, tcStmtsAndThen, tcGRHSs 
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcExpr )

import HsSyn		( HsBinds(..), Match(..), GRHSs(..), GRHS(..),
			  MonoBinds(..), Stmt(..), HsMatchContext(..),
			  pprMatch, getMatchLoc, pprMatchContext, isDoExpr,
			  mkMonoBind, nullMonoBinds, collectSigTysFromPats
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt, TcDictBinds )

import TcMonad
import TcMonoType	( kcHsSigType, tcTyVars, checkSigTyVars, tcHsSigType, sigPatCtxt )
import Inst		( LIE, isEmptyLIE, plusLIE, emptyLIE, plusLIEs, lieToList )
import TcEnv		( TcId, tcLookupLocalIds, tcExtendTyVarEnv, tcExtendLocalValEnv, tcExtendGlobalTyVars )
import TcPat		( tcPat, tcMonoPatBndr, polyPatSig )
import TcType		( TcType, newTyVarTy )
import TcBinds		( tcBindsAndThen )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import TcUnify		( unifyFunTy, unifyTauTy )
import Name		( Name )
import TysWiredIn	( boolTy, mkListTy )
import Id		( idType )
import BasicTypes	( RecFlag(..) )
import Type		( tyVarsOfType, isTauTy,  mkFunTy,
			  liftedTypeKind, openTypeKind, splitSigmaTy )
import VarSet
import Var		( Id )
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
	     -> TcM ([TcMatch], LIE)

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
	      -> TcM (TcType,		-- Inferred type of the scrutinee
			[TcMatch], 	-- Translated alternatives
			LIE)

tcMatchesCase matches expr_ty
  = newTyVarTy openTypeKind 					`thenNF_Tc` \ scrut_ty ->
    tcMatches [] matches (mkFunTy scrut_ty expr_ty) CaseAlt	`thenTc` \ (matches', lie) ->
    returnTc (scrut_ty, matches', lie)

tcMatchLambda :: RenamedMatch -> TcType -> TcM (TcMatch, LIE)
tcMatchLambda match res_ty = tcMatch [] match res_ty LambdaExpr
\end{code}


\begin{code}
tcMatches :: [(Name,Id)]
	  -> [RenamedMatch]
	  -> TcType
	  -> HsMatchContext 
	  -> TcM ([TcMatch], LIE)

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
	-> HsMatchContext
	-> TcM (TcMatch, LIE)

tcMatch xve1 match@(Match sig_tvs pats maybe_rhs_sig grhss) expected_ty ctxt
  = tcAddSrcLoc (getMatchLoc match)		$
    tcAddErrCtxt (matchCtxt ctxt match)		$

    if null sig_tvs then	-- The common case
	tc_match expected_ty	`thenTc` \ (_, match_and_lie) ->
	returnTc match_and_lie

    else
	-- If there are sig tvs we must be careful *not* to use
	-- expected_ty right away, else we'll unify with tyvars free
	-- in the envt.  So invent a fresh tyvar and use that instead
	newTyVarTy openTypeKind					`thenNF_Tc` \ tyvar_ty ->

	-- Extend the tyvar env and check the match itself
	tcTyVars sig_tvs (mapTc_ kcHsSigType sig_tys)		`thenTc` \ sig_tyvars ->
	tcExtendTyVarEnv sig_tyvars (tc_match tyvar_ty)		`thenTc` \ (pat_ids, match_and_lie) ->

	-- Check that the scoped type variables from the patterns
	-- have not been constrained
        tcAddErrCtxtM (sigPatCtxt sig_tyvars pat_ids)		(
		checkSigTyVars sig_tyvars emptyVarSet
	)							`thenTc_`

	-- *Now* we're free to unify with expected_ty
	unifyTauTy expected_ty tyvar_ty	`thenTc_`

	returnTc match_and_lie

  where
    sig_tys = case maybe_rhs_sig of { Just t -> [t]; Nothing -> [] }
	      ++ collectSigTysFromPats pats
	      
    tc_match expected_ty 	-- Any sig tyvars are in scope by now
      = -- STEP 1: Typecheck the patterns
	tcMatchPats pats expected_ty	`thenTc` \ (rhs_ty, pats', lie_req1, ex_tvs, pat_bndrs, lie_avail) ->
        let
	  xve2       = bagToList pat_bndrs
	  pat_ids    = map snd xve2
        in

	-- STEP 2: Check that the remaining "expected type" is not a rank-2 type
	-- If it is it'll mess up the unifier when checking the RHS
	checkTc (isTauTy rhs_ty) lurkingRank2SigErr 		`thenTc_`

	-- STEP 3: Unify with the rhs type signature if any
	(case maybe_rhs_sig of
	    Nothing  -> returnTc ()
	    Just sig -> tcHsSigType sig		`thenTc` \ sig_ty ->

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
	tcCheckExistentialPat pat_ids ex_tvs lie_avail 
			      (lie_req1 `plusLIE` lie_req2) 
			      rhs_ty 		`thenTc` \ (lie_req', ex_binds) ->

	-- Phew!  All done.
	let
            match' = Match [] pats' Nothing (glue_on Recursive ex_binds grhss')
	in
	returnTc (pat_ids, (match', lie_req'))

	-- glue_on just avoids stupid dross
glue_on _ EmptyMonoBinds grhss = grhss		-- The common case
glue_on is_rec mbinds (GRHSs grhss binds ty)
  = GRHSs grhss (mkMonoBind mbinds [] is_rec `ThenBinds` binds) ty

tcGRHSs :: RenamedGRHSs
	-> TcType -> HsMatchContext
	-> TcM (TcGRHSs, LIE)

tcGRHSs (GRHSs grhss binds _) expected_ty ctxt
  = tcBindsAndThen glue_on binds (tc_grhss grhss)
  where
    tc_grhss grhss
	= mapAndUnzipTc tc_grhs grhss	    `thenTc` \ (grhss', lies) ->
	  returnTc (GRHSs grhss' EmptyBinds (Just expected_ty), plusLIEs lies)

    tc_grhs (GRHS guarded locn)
	= tcAddSrcLoc locn					$
	  tcStmts ctxt (\ty -> ty, expected_ty) guarded		`thenTc` \ (guarded', lie) ->
	  returnTc (GRHS guarded' locn, lie)


tcCheckExistentialPat :: [TcId]		-- Ids bound by this pattern
		      -> Bag TcTyVar	-- Existentially quantified tyvars bound by pattern
		      -> LIE		--   and context
		      -> LIE		-- Required context
		      -> TcType		--   and result type; vars in here must not escape
		      -> TcM (LIE, TcDictBinds)	-- LIE to float out and dict bindings
tcCheckExistentialPat ids ex_tvs lie_avail lie_req result_ty
  | isEmptyBag ex_tvs && all not_overloaded ids
	-- Short cut for case when there are no existentials
	-- and no polymorphic overloaded variables
	--  e.g. f :: (forall a. Ord a => a -> a) -> Int -> Int
	--	 f op x = ....
	--  Here we must discharge op Methods
  = ASSERT( isEmptyLIE lie_avail )
    returnTc (lie_req, EmptyMonoBinds)

  | otherwise
  = tcExtendGlobalTyVars (tyVarsOfType result_ty)		$
    tcAddErrCtxtM (sigPatCtxt tv_list ids)			$

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    bindInstsOfLocalFuns lie_req ids	 			`thenTc` \ (lie1, inst_binds) ->

	-- Deal with overloaded functions bound by the pattern
    tcSimplifyCheck doc tv_list
		    (lieToList lie_avail) lie1		`thenTc` \ (lie2, dict_binds) ->
    checkSigTyVars tv_list emptyVarSet				`thenTc_` 

    returnTc (lie2, dict_binds `AndMonoBinds` inst_binds)
  where
    doc     = text ("the existential context of a data constructor")
    tv_list = bagToList ex_tvs
    not_overloaded id = case splitSigmaTy (idType id) of
			  (_, theta, _) -> null theta
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
  = unifyFunTy expected_ty		`thenTc` \ (arg_ty, rest_ty) ->
    tcPat tcMonoPatBndr pat arg_ty	`thenTc` \ (pat', lie_req, pat_tvs, pat_ids, lie_avail) ->
    tcMatchPats pats rest_ty		`thenTc` \ (rhs_ty, pats', lie_reqs, pats_tvs, pats_ids, lie_avails) ->
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

Typechecking statements is rendered a bit tricky by parallel list comprehensions:

	[ (g x, h x) | ... ; let g v = ...
		     | ... ; let h v = ... ]

It's possible that g,h are overloaded, so we need to feed the LIE from the
(g x, h x) up through both lots of bindings (so we get the bindInstsOfLocalFuns).
Similarly if we had an existential pattern match:

	data T = forall a. Show a => C a

	[ (show x, show y) | ... ; C x <- ...
			   | ... ; C y <- ... ]

Then we need the LIE from (show x, show y) to be simplified against
the bindings for x and y.  

It's difficult to do this in parallel, so we rely on the renamer to 
ensure that g,h and x,y don't duplicate, and simply grow the environment.
So the binders of the first parallel group will be in scope in the second
group.  But that's fine; there's no shadowing to worry about.

\begin{code}
tcStmts do_or_lc m_ty stmts
  = tcStmtsAndThen (:) do_or_lc m_ty stmts (returnTc ([], emptyLIE))

tcStmtsAndThen
	:: (TcStmt -> thing -> thing)	-- Combiner
	-> HsMatchContext
        -> (TcType -> TcType, TcType)	-- m, the relationship type of pat and rhs in pat <- rhs
					-- elt_ty, where type of the comprehension is (m elt_ty)
        -> [RenamedStmt]
	-> TcM (thing, LIE)
        -> TcM (thing, LIE)

	-- Base case
tcStmtsAndThen combine do_or_lc m_ty [] do_next
  = do_next

	-- LetStmt
tcStmtsAndThen combine do_or_lc m_ty (LetStmt binds : stmts) do_next
  = tcBindsAndThen		-- No error context, but a binding group is
  	(glue_binds combine)	-- rather a large thing for an error context anyway
  	binds
  	(tcStmtsAndThen combine do_or_lc m_ty stmts do_next)

	-- BindStmt
tcStmtsAndThen combine do_or_lc m_ty@(m,elt_ty) (stmt@(BindStmt pat exp src_loc) : stmts) do_next
  = tcAddSrcLoc src_loc		(
	tcSetErrCtxt (stmtCtxt do_or_lc stmt)	$
    	newTyVarTy liftedTypeKind	`thenNF_Tc` \ pat_ty ->
  	tcPat tcMonoPatBndr pat pat_ty	`thenTc` \ (pat', pat_lie, pat_tvs, pat_ids, avail) ->  
      	tcExpr exp (m pat_ty)		`thenTc` \ (exp', exp_lie) ->
  	returnTc (pat', exp',
		  pat_lie `plusLIE` exp_lie,
		  pat_tvs, pat_ids, avail)
    )					`thenTc` \ (pat', exp', lie_req, pat_tvs, pat_bndrs, lie_avail) ->
    let
	new_val_env = bagToList pat_bndrs
	pat_ids     = map snd new_val_env
    in

	-- Do the rest; we don't need to add the pat_tvs to the envt
	-- because they all appear in the pat_ids's types
    tcExtendLocalValEnv new_val_env (
       tcStmtsAndThen combine do_or_lc m_ty stmts do_next
    )						`thenTc` \ (thing, stmts_lie) ->

	-- Reinstate context for existential checks
    tcSetErrCtxt (stmtCtxt do_or_lc stmt) 		$
    tcCheckExistentialPat pat_ids pat_tvs lie_avail
			  stmts_lie (m elt_ty) 		`thenTc` \ (final_lie, dict_binds) ->

    returnTc (combine (BindStmt pat' exp' src_loc)
		      (glue_binds combine Recursive dict_binds thing),
	      lie_req `plusLIE` final_lie)


	-- ParStmt
tcStmtsAndThen combine do_or_lc m_ty (ParStmtOut bndr_stmts_s : stmts) do_next
  = loop bndr_stmts_s		`thenTc` \ ((pairs', thing), lie) ->
    returnTc (combine (ParStmtOut pairs') thing, lie)
  where
    loop []
      = tcStmtsAndThen combine do_or_lc m_ty stmts do_next	`thenTc` \ (thing, stmts_lie) ->
	returnTc (([], thing), stmts_lie)

    loop ((bndrs,stmts) : pairs)
      = tcStmtsAndThen 
		combine_par ListComp (mkListTy, not_required) stmts
		(tcLookupLocalIds bndrs	`thenNF_Tc` \ bndrs' ->
		 loop pairs		`thenTc` \ ((pairs', thing), lie) ->
		 returnTc (([], (bndrs', pairs', thing)), lie))	`thenTc` \ ((stmts', (bndrs', pairs', thing)), lie) ->

	returnTc ( ((bndrs',stmts') : pairs', thing), lie)

    combine_par stmt (stmts, thing) = (stmt:stmts, thing)
    not_required = panic "tcStmtsAndThen: elt_ty"

	-- The simple-statment case
tcStmtsAndThen combine do_or_lc m_ty (stmt@(ExprStmt exp locn):stmts) do_next
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	tcExprStmt do_or_lc m_ty exp (null stmts)
    )							`thenTc` \ (exp', stmt_lie) ->

    tcStmtsAndThen combine do_or_lc m_ty stmts do_next	`thenTc` \ (thing, stmts_lie) ->

    returnTc (combine (ExprStmt exp' locn) thing,
	      stmt_lie `plusLIE` stmts_lie)


------------------------------
	-- ExprStmt; see comments with HsExpr.HsStmt 
	-- 	     for meaning of ExprStmt
tcExprStmt do_or_lc (m, res_elt_ty) exp is_last_stmt
  = compute_expr_ty		`thenNF_Tc` \ expr_ty ->
    tcExpr exp expr_ty
  where
    compute_expr_ty
	| is_last_stmt = if isDoExpr do_or_lc then
				returnNF_Tc (m res_elt_ty)
			 else
				returnNF_Tc res_elt_ty

	| otherwise    = if isDoExpr do_or_lc then
				newTyVarTy openTypeKind 	`thenNF_Tc` \ any_ty ->
				returnNF_Tc (m any_ty)	
			 else
				returnNF_Tc boolTy	

------------------------------
glue_binds combine is_rec binds thing 
  | nullMonoBinds binds = thing
  | otherwise		= combine (LetStmt (mkMonoBind binds [] is_rec)) thing
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
  = hang (ptext SLIT("In a case alternative:"))
	 4 (pprMatch (True,empty) {-is_case-} match)

matchCtxt (FunRhs fun) match
  = hang (hcat [ptext SLIT("In an equation for function "), quotes (ppr_fun), char ':'])
	 4 (pprMatch (False, ppr_fun) {-not case-} match)
  where
    ppr_fun = ppr fun

matchCtxt LambdaExpr match
  = hang (ptext SLIT("In the lambda expression"))
	 4 (pprMatch (True, empty) match)

varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

lurkingRank2SigErr
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")

stmtCtxt do_or_lc stmt = hang (pprMatchContext do_or_lc <> colon) 4 (ppr stmt)
\end{code}
