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
			  MonoBinds(..), Stmt(..), HsMatchContext(..), HsDoContext(..),
			  pprMatch, getMatchLoc, pprMatchContext, isDoExpr,
			  mkMonoBind, nullMonoBinds, collectSigTysFromPats
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt, RenamedPat, RenamedHsType,
			  RenamedMatchContext, extractHsTyVars )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt, TcDictBinds, TypecheckedPat )

import TcMonad
import TcMonoType	( kcHsSigTypes, tcScopedTyVars, checkSigTyVars, tcHsSigType, sigPatCtxt )
import Inst		( LIE, isEmptyLIE, plusLIE, emptyLIE, plusLIEs, lieToList )
import TcEnv		( TcId, tcLookupLocalIds, tcExtendLocalValEnv, tcExtendGlobalTyVars,
			  tcInLocalScope )
import TcPat		( tcPat, tcMonoPatBndr, polyPatSig )
import TcMType		( newTyVarTy, unifyFunTy, unifyTauTy )
import TcType		( tyVarsOfType, isTauTy,  mkFunTy, isOverloadedTy,
			  liftedTypeKind, openTypeKind  )
import TcBinds		( tcBindsAndThen )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import Name		( Name )
import TysWiredIn	( boolTy )
import Id		( idType )
import BasicTypes	( RecFlag(..) )
import NameSet
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

tcMatches xve fun_or_case matches expected_ty
  = mapAndUnzipTc tc_match matches	`thenTc` \ (matches, lies) ->
    returnTc (matches, plusLIEs lies)
  where
    tc_match match = tcMatch xve fun_or_case match expected_ty
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
	-> TcType 		-- Expected result-type of the Match.
				-- Early unification with this guy gives better error messages
	-> TcM (TcMatch, LIE)

tcMatch xve1 ctxt match@(Match sig_tvs pats maybe_rhs_sig grhss) expected_ty
  = tcAddSrcLoc (getMatchLoc match)		$	-- At one stage I removed this;
    tcAddErrCtxt (matchCtxt ctxt match)		$	-- I'm not sure why, so I put it back
    
    tcMatchPats pats expected_ty tc_grhss	`thenTc` \ ((pats', grhss'), lie, ex_binds) ->
    returnTc (Match [] pats' Nothing (glue_on Recursive ex_binds grhss'), lie)

  where
    tc_grhss pats' rhs_ty 
	=	-- Check that the remaining "expected type" is not a rank-2 type
		-- If it is it'll mess up the unifier when checking the RHS
	  checkTc (isTauTy rhs_ty) lurkingRank2SigErr 		`thenTc_`

		-- Deal with the result signature
	  tc_result_sig maybe_rhs_sig	( 

		-- Typecheck the body
		tcExtendLocalValEnv xve1 	$
		tcGRHSs ctxt grhss rhs_ty	`thenTc` \ (grhss', lie) ->
		returnTc ((pats', grhss'), lie)
	  )

    tc_result_sig Nothing thing_inside
	= thing_inside
    tc_result_sig (Just sig) thing_inside
	= tcAddScopedTyVars [sig]			$
	  tcHsSigType sig				`thenTc` \ sig_ty ->

		-- Check that the signature isn't a polymorphic one, which
		-- we don't permit (at present, anyway)
	  checkTc (isTauTy sig_ty) (polyPatSig sig_ty)	`thenTc_`
	  unifyTauTy expected_ty sig_ty			`thenTc_`
	  thing_inside


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
	-> ([TypecheckedPat] -> TcType -> TcM (a, LIE))
	-> TcM (a, LIE, TcDictBinds)
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

tcMatchPats pats expected_ty thing_inside
  = 	-- STEP 1: Bring pattern-signature type variables into scope
    tcAddScopedTyVars (collectSigTysFromPats pats)			$

	-- STEP 2: Typecheck the patterns themselves, gathering all the stuff
    tc_match_pats pats expected_ty	`thenTc` \ (rhs_ty, pats', lie_req1, ex_tvs, pat_bndrs, lie_avail) ->
    
	-- STEP 3: Extend the environment, and do the thing inside
    let
	  xve     = bagToList pat_bndrs
	  pat_ids = map snd xve
    in
    tcExtendLocalValEnv xve (thing_inside pats' rhs_ty)		`thenTc` \ (result, lie_req2) ->

	-- STEP 4: Check for existentially bound type variables
	-- I'm a bit concerned that lie_req1 from an 'inner' pattern in the list
	-- might need (via lie_req2) something made available from an 'outer' 
	-- pattern.  But it's inconvenient to deal with, and I can't find an example
    tcCheckExistentialPat pat_ids ex_tvs lie_avail lie_req2 rhs_ty	`thenTc` \ (lie_req2', ex_binds) ->

    returnTc (result, lie_req1 `plusLIE` lie_req2', ex_binds)

tcAddScopedTyVars :: [RenamedHsType] -> TcM a -> TcM a
-- Find the not-already-in-scope signature type variables,
-- kind-check them, and bring them into scope
--
-- We no longer specify that these type variables must be univerally 
-- quantified (lots of email on the subject).  If you want to put that 
-- back in, you need to
--	a) Do a checkSigTyVars after thing_inside
--	b) More insidiously, don't pass in expected_ty, else
--	   we unify with it too early and checkSigTyVars barfs
--	   Instead you have to pass in a fresh ty var, and unify
--	   it with expected_ty afterwards
tcAddScopedTyVars sig_tys thing_inside
  = tcGetEnv					`thenNF_Tc` \ env ->
    let
	all_sig_tvs = foldr (unionNameSets . extractHsTyVars) emptyNameSet sig_tys
	sig_tvs = filter not_in_scope (nameSetToList all_sig_tvs)
 	not_in_scope tv = not (tcInLocalScope env tv)
    in	      
    tcScopedTyVars sig_tvs (kcHsSigTypes sig_tys) thing_inside

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
    not_overloaded id = not (isOverloadedTy (idType id))

tc_match_pats [] expected_ty
  = returnTc (expected_ty, [], emptyLIE, emptyBag, emptyBag, emptyLIE)

tc_match_pats (pat:pats) expected_ty
  = unifyFunTy expected_ty		`thenTc` \ (arg_ty, rest_ty) ->
    tcPat tcMonoPatBndr pat arg_ty	`thenTc` \ (pat', lie_req, pat_tvs, pat_ids, lie_avail) ->
    tc_match_pats pats rest_ty		`thenTc` \ (rhs_ty, pats', lie_reqs, pats_tvs, pats_ids, lie_avails) ->
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
    tcExpr exp (m pat_ty)				`thenTc` \ (exp', exp_lie) ->
    tcMatchPats [pat] (mkFunTy pat_ty (m elt_ty))	(\ [pat'] _ ->
	tcPopErrCtxt 				$
	thing_inside 				`thenTc` \ (thing, lie) ->
	returnTc ((BindStmt pat' exp' src_loc, thing), lie)
    )							`thenTc` \ ((stmt', thing), lie, dict_binds) ->
    returnTc (combine stmt' (glue_binds combine Recursive dict_binds thing),
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
		tcExpr exp (m any_ty)		`thenNF_Tc` \ (exp', lie) ->
		returnTc (ExprStmt exp' any_ty locn, lie)
	else
		tcExpr exp boolTy		`thenNF_Tc` \ (exp', lie) ->
		returnTc (ExprStmt exp' boolTy locn, lie)
    )						`thenTc` \ (stmt', stmt_lie) ->

    thing_inside 				`thenTc` \ (thing, stmts_lie) ->

    returnTc (combine stmt' thing, stmt_lie `plusLIE` stmts_lie)


	-- Result statements
tcStmtAndThen combine do_or_lc m_ty@(m, res_elt_ty) stmt@(ResultStmt exp locn) thing_inside
  = tcSetErrCtxt (stmtCtxt do_or_lc stmt) (
	if isDoExpr do_or_lc then
		tcExpr exp (m res_elt_ty)
	else
		tcExpr exp res_elt_ty
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
sameNoOfArgs matches = length (nub (map args_in_match matches)) == 1
  where
    args_in_match :: RenamedMatch -> Int
    args_in_match (Match _ pats _ _) = length pats
\end{code}

\begin{code}
matchCtxt ctxt  match  = hang (pprMatchContext ctxt     <> colon) 4 (pprMatch ctxt match)
stmtCtxt do_or_lc stmt = hang (pprMatchContext do_or_lc <> colon) 4 (ppr stmt)

varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

lurkingRank2SigErr
  = ptext SLIT("Too few explicit arguments when defining a function with a rank-2 type")
\end{code}
