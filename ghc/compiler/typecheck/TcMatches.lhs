%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchLambda, 
		   tcStmts, tcStmtsAndThen, tcGRHSs 
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcMonoExpr )

import HsSyn		( HsBinds(..), Match(..), GRHSs(..), GRHS(..),
			  MonoBinds(..), Stmt(..), HsMatchContext(..), HsDoContext(..),
			  pprMatch, getMatchLoc, pprMatchContext, isDoExpr,
			  mkMonoBind, nullMonoBinds, collectSigTysFromPats
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt, RenamedPat, RenamedMatchContext )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt, TcDictBinds, TypecheckedPat )

import TcMonad
import TcMonoType	( tcAddScopedTyVars, tcHsSigType, UserTypeCtxt(..) )
import Inst		( LIE, isEmptyLIE, plusLIE, emptyLIE, plusLIEs, lieToList )
import TcEnv		( TcId, tcLookupLocalIds, tcExtendLocalValEnv2 )
import TcPat		( tcPat, tcMonoPatBndr )
import TcMType		( newTyVarTy, zonkTcType, zapToType )
import TcType		( TcType, TcTyVar, tyVarsOfType, tidyOpenTypes, tidyOpenType,
			  mkFunTy, isOverloadedTy, liftedTypeKind, openTypeKind  )
import TcBinds		( tcBindsAndThen )
import TcUnify		( subFunTy, checkSigTyVarsWrt, tcSubExp, isIdCoercion, (<$>) )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import Name		( Name )
import TysWiredIn	( boolTy )
import Id		( idType )
import CoreFVs		( idFreeTyVars )
import BasicTypes	( RecFlag(..) )
import VarSet
import Var		( Id )
import Bag
import Util		( isSingleton, lengthExceeds )
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

	-- No need to zonk expected_ty, because subFunTy does that on the fly
    tcMatches xve (FunRhs fun_name) matches expected_ty
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
    tcMatches [] CaseAlt matches (mkFunTy scrut_ty expr_ty)	`thenTc` \ (matches', lie) ->
    returnTc (scrut_ty, matches', lie)

tcMatchLambda :: RenamedMatch -> TcType -> TcM (TcMatch, LIE)
tcMatchLambda match res_ty = tcMatch [] LambdaExpr match res_ty
\end{code}


\begin{code}
tcMatches :: [(Name,Id)]
	  -> RenamedMatchContext 
	  -> [RenamedMatch]
	  -> TcType
	  -> TcM ([TcMatch], LIE)

tcMatches xve ctxt matches expected_ty
  = 	-- If there is more than one branch, and expected_ty is a 'hole',
	-- all branches must be types, not type schemes, otherwise the
	-- in which we check them would affect the result.
    (if lengthExceeds matches 1 then
	zapToType expected_ty
     else
	returnNF_Tc expected_ty)			`thenNF_Tc` \ expected_ty' ->

    mapAndUnzipTc (tc_match expected_ty') matches	`thenTc` \ (matches, lies) ->
    returnTc (matches, plusLIEs lies)
  where
    tc_match expected_ty match = tcMatch xve ctxt match expected_ty
\end{code}


%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatch :: [(Name,Id)]
	-> RenamedMatchContext
	-> RenamedMatch
	-> TcType 	-- Expected result-type of the Match.
			-- Early unification with this guy gives better error messages
			-- We regard the Match as having type 
			--	(ty1 -> ... -> tyn -> result_ty)
			-- where there are n patterns.
	-> TcM (TcMatch, LIE)

tcMatch xve1 ctxt match@(Match pats maybe_rhs_sig grhss) expected_ty
  = tcAddSrcLoc (getMatchLoc match)		$	-- At one stage I removed this;
    tcAddErrCtxt (matchCtxt ctxt match)		$	-- I'm not sure why, so I put it back
    tcMatchPats pats expected_ty tc_grhss	`thenTc` \ (pats', grhss', lie, ex_binds) ->
    returnTc (Match pats' Nothing (glue_on Recursive ex_binds grhss'), lie)

  where
    tc_grhss rhs_ty 
	= tcExtendLocalValEnv2 xve1 			$

		-- Deal with the result signature
	  case maybe_rhs_sig of
	    Nothing ->  tcGRHSs ctxt grhss rhs_ty

	    Just sig ->	 tcAddScopedTyVars [sig]	$
				-- Bring into scope the type variables in the signature
			 tcHsSigType ResSigCtxt sig	`thenTc` \ sig_ty ->
			 tcGRHSs ctxt grhss sig_ty	`thenTc` \ (grhss', lie1) ->
			 tcSubExp rhs_ty sig_ty		`thenTc` \ (co_fn, lie2)  ->
			 returnTc (lift_grhss co_fn rhs_ty grhss', 
				   lie1 `plusLIE` lie2)

-- lift_grhss pushes the coercion down to the right hand sides,
-- because there is no convenient place to hang it otherwise.
lift_grhss co_fn rhs_ty grhss 
  | isIdCoercion co_fn = grhss
lift_grhss co_fn rhs_ty (GRHSs grhss binds ty)
  = GRHSs (map lift_grhs grhss) binds rhs_ty	-- Change the type, since we
  where
    lift_grhs (GRHS stmts loc) = GRHS (map lift_stmt stmts) loc
	      
    lift_stmt (ResultStmt e l) = ResultStmt (co_fn <$> e) l
    lift_stmt stmt	       = stmt
   
-- glue_on just avoids stupid dross
glue_on _ EmptyMonoBinds grhss = grhss		-- The common case
glue_on is_rec mbinds (GRHSs grhss binds ty)
  = GRHSs grhss (mkMonoBind mbinds [] is_rec `ThenBinds` binds) ty


tcGRHSs :: RenamedMatchContext -> RenamedGRHSs
	-> TcType
	-> TcM (TcGRHSs, LIE)

tcGRHSs ctxt (GRHSs grhss binds _) expected_ty
  = tcBindsAndThen glue_on binds (tc_grhss grhss)
  where
    tc_grhss grhss
	= mapAndUnzipTc tc_grhs grhss	    `thenTc` \ (grhss', lies) ->
	  returnTc (GRHSs grhss' EmptyBinds expected_ty, plusLIEs lies)

    tc_grhs (GRHS guarded locn)
	= tcAddSrcLoc locn					$
	  tcStmts ctxt (\ty -> ty, expected_ty) guarded		`thenTc` \ (guarded', lie) ->
	  returnTc (GRHS guarded' locn, lie)
\end{code}


%************************************************************************
%*									*
\subsection{tcMatchPats}
%*									*
%************************************************************************

\begin{code}	  
tcMatchPats
	:: [RenamedPat] -> TcType
	-> (TcType -> TcM (a, LIE))
	-> TcM ([TypecheckedPat], a, LIE, TcDictBinds)
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

tcMatchPats pats expected_ty thing_inside
  = 	-- STEP 1: Bring pattern-signature type variables into scope
    tcAddScopedTyVars (collectSigTysFromPats pats)	(

	-- STEP 2: Typecheck the patterns themselves, gathering all the stuff
	--	   then do the thing inside
        tc_match_pats pats expected_ty thing_inside

    ) `thenTc` \ (pats', lie_req, ex_tvs, ex_ids, ex_lie, result) -> 

	-- STEP 4: Check for existentially bound type variables
	-- Do this *outside* the scope of the tcAddScopedTyVars, else checkSigTyVars
	-- complains that 'a' is captured by the inscope 'a'!  (Test (d) in checkSigTyVars.)
	--
	-- I'm a bit concerned that lie_req1 from an 'inner' pattern in the list
	-- might need (via lie_req2) something made available from an 'outer' 
	-- pattern.  But it's inconvenient to deal with, and I can't find an example
    tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req expected_ty	`thenTc` \ (lie_req', ex_binds) ->
	-- NB: we *must* pass "expected_ty" not "result_ty" to tcCheckExistentialPat
	-- For example, we must reject this program:
	--	data C = forall a. C (a -> Int) 
	-- 	f (C g) x = g x
	-- Here, result_ty will be simply Int, but expected_ty is (a -> Int).

    returnTc (pats', result, lie_req', ex_binds)

tc_match_pats [] expected_ty thing_inside
  = thing_inside expected_ty 	`thenTc` \ (answer, lie) ->
    returnTc ([], lie, emptyBag, [], emptyLIE, answer)

tc_match_pats (pat:pats) expected_ty thing_inside
  = subFunTy expected_ty		$ \ arg_ty rest_ty ->
	-- This is the unique place we call subFunTy
	-- The point is that if expected_y is a "hole", we want 
	-- to make arg_ty and rest_ty as "holes" too.
    tcPat tcMonoPatBndr pat arg_ty	`thenTc` \ (pat', lie_req, ex_tvs, pat_bndrs, ex_lie) ->
    let
	xve    = bagToList pat_bndrs
	ex_ids = [id | (_, id) <- xve]
		-- ex_ids is all the pattern-bound Ids, a superset
		-- of the existential Ids used in checkExistentialPat
    in
    tcExtendLocalValEnv2 xve 			$
    tc_match_pats pats rest_ty thing_inside	`thenTc` \ (pats', lie_reqs, exs_tvs, exs_ids, exs_lie, answer) ->
    returnTc (	pat':pats',
		lie_req `plusLIE` lie_reqs,
		ex_tvs `unionBags` exs_tvs,
		ex_ids ++ exs_ids,
		ex_lie `plusLIE` exs_lie,
		answer
    )


tcCheckExistentialPat :: Bag TcTyVar	-- Existentially quantified tyvars bound by pattern
		      -> [TcId]		-- Ids bound by this pattern; used 
					--   (a) by bindsInstsOfLocalFuns
					--   (b) to generate helpful error messages
		      -> LIE		--   and context
		      -> LIE		-- Required context
		      -> TcType		--   and type of the Match; vars in here must not escape
		      -> TcM (LIE, TcDictBinds)	-- LIE to float out and dict bindings
tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req match_ty
  | isEmptyBag ex_tvs && all not_overloaded ex_ids
	-- Short cut for case when there are no existentials
	-- and no polymorphic overloaded variables
	--  e.g. f :: (forall a. Ord a => a -> a) -> Int -> Int
	--	 f op x = ....
	--  Here we must discharge op Methods
  = ASSERT( isEmptyLIE ex_lie )
    returnTc (lie_req, EmptyMonoBinds)

  | otherwise
  = tcAddErrCtxtM (sigPatCtxt tv_list ex_ids match_ty)		$

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    bindInstsOfLocalFuns lie_req ex_ids	 			`thenTc` \ (lie1, inst_binds) ->

	-- Deal with overloaded functions bound by the pattern
    tcSimplifyCheck doc tv_list (lieToList ex_lie) lie1	`thenTc` \ (lie2, dict_binds) ->
    checkSigTyVarsWrt (tyVarsOfType match_ty) tv_list	`thenTc_` 

    returnTc (lie2, dict_binds `AndMonoBinds` inst_binds)
  where
    doc     = text ("the existential context of a data constructor")
    tv_list = bagToList ex_tvs
    not_overloaded id = not (isOverloadedTy (idType id))
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
	-> RenamedMatchContext
        -> (TcType -> TcType, TcType)	-- m, the relationship type of pat and rhs in pat <- rhs
					-- elt_ty, where type of the comprehension is (m elt_ty)
        -> [RenamedStmt]
	-> TcM (thing, LIE)
        -> TcM (thing, LIE)

	-- Base case
tcStmtsAndThen combine do_or_lc m_ty [] do_next
  = do_next

tcStmtsAndThen combine do_or_lc m_ty (stmt:stmts) do_next
  = tcStmtAndThen combine do_or_lc m_ty stmt
	(tcStmtsAndThen combine do_or_lc m_ty stmts do_next)

	-- LetStmt
tcStmtAndThen combine do_or_lc m_ty (LetStmt binds) thing_inside
  = tcBindsAndThen		-- No error context, but a binding group is
  	(glue_binds combine)	-- rather a large thing for an error context anyway
  	binds
  	thing_inside

tcStmtAndThen combine do_or_lc m_ty@(m,elt_ty) stmt@(BindStmt pat exp src_loc) thing_inside
  = tcAddSrcLoc src_loc					$
    tcAddErrCtxt (stmtCtxt do_or_lc stmt)		$
    newTyVarTy liftedTypeKind				`thenNF_Tc` \ pat_ty ->
    tcMonoExpr exp (m pat_ty)				`thenTc` \ (exp', exp_lie) ->
    tcMatchPats [pat] (mkFunTy pat_ty (m elt_ty))	(\ _ ->
	tcPopErrCtxt thing_inside
    )							`thenTc` \ ([pat'], thing, lie, dict_binds) ->
    returnTc (combine (BindStmt pat' exp' src_loc)
		      (glue_binds combine Recursive dict_binds thing),
	      lie `plusLIE` exp_lie)


	-- ParStmt
tcStmtAndThen combine do_or_lc m_ty (ParStmtOut bndr_stmts_s) thing_inside
  = loop bndr_stmts_s		`thenTc` \ ((pairs', thing), lie) ->
    returnTc (combine (ParStmtOut pairs') thing, lie)
  where
    loop []
      = thing_inside				`thenTc` \ (thing, stmts_lie) ->
	returnTc (([], thing), stmts_lie)

    loop ((bndrs,stmts) : pairs)
      = tcStmtsAndThen 
		combine_par (DoCtxt ListComp) m_ty stmts
			-- Notice we pass on m_ty; the result type is used only
			-- to get escaping type variables for checkExistentialPat
		(tcLookupLocalIds bndrs	`thenNF_Tc` \ bndrs' ->
		 loop pairs		`thenTc` \ ((pairs', thing), lie) ->
		 returnTc (([], (bndrs', pairs', thing)), lie))	`thenTc` \ ((stmts', (bndrs', pairs', thing)), lie) ->

	returnTc ( ((bndrs',stmts') : pairs', thing), lie)

    combine_par stmt (stmts, thing) = (stmt:stmts, thing)

	-- ExprStmt
tcStmtAndThen combine do_or_lc m_ty@(m, res_elt_ty) stmt@(ExprStmt exp _ locn) thing_inside
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	if isDoExpr do_or_lc then
		newTyVarTy openTypeKind 	`thenNF_Tc` \ any_ty ->
		tcMonoExpr exp (m any_ty)	`thenNF_Tc` \ (exp', lie) ->
		returnTc (ExprStmt exp' any_ty locn, lie)
	else
		tcMonoExpr exp boolTy		`thenNF_Tc` \ (exp', lie) ->
		returnTc (ExprStmt exp' boolTy locn, lie)
    )						`thenTc` \ (stmt', stmt_lie) ->

    thing_inside 				`thenTc` \ (thing, stmts_lie) ->

    returnTc (combine stmt' thing, stmt_lie `plusLIE` stmts_lie)


	-- Result statements
tcStmtAndThen combine do_or_lc m_ty@(m, res_elt_ty) stmt@(ResultStmt exp locn) thing_inside
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	if isDoExpr do_or_lc then
		tcMonoExpr exp (m res_elt_ty)
	else
		tcMonoExpr exp res_elt_ty
    )						`thenTc` \ (exp', stmt_lie) ->

    thing_inside 				`thenTc` \ (thing, stmts_lie) ->

    returnTc (combine (ResultStmt exp' locn) thing,
	      stmt_lie `plusLIE` stmts_lie)


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
sameNoOfArgs matches = isSingleton (nub (map args_in_match matches))
  where
    args_in_match :: RenamedMatch -> Int
    args_in_match (Match pats _ _) = length pats
\end{code}

\begin{code}
varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

matchCtxt ctxt  match  = hang (pprMatchContext ctxt     <> colon) 4 (pprMatch ctxt match)
stmtCtxt do_or_lc stmt = hang (pprMatchContext do_or_lc <> colon) 4 (ppr stmt)

sigPatCtxt bound_tvs bound_ids match_ty tidy_env 
  = zonkTcType match_ty		`thenNF_Tc` \ match_ty' ->
    let
	(env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
	(env2, tidy_mty) = tidyOpenType  env1     match_ty'
    in
    returnNF_Tc (env1,
		 sep [ptext SLIT("When checking an existential match that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys)),
		      ptext SLIT("and whose type is") <+> ppr tidy_mty])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    ppr_id id ty     = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}
