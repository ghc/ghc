%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcMatchesCase, tcMatchLambda, 
		   tcDoStmts, tcStmtsAndThen, tcGRHSs 
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcMonoExpr )

import HsSyn		( HsExpr(..), HsBinds(..), Match(..), GRHSs(..), GRHS(..),
			  MonoBinds(..), Stmt(..), HsMatchContext(..), HsDoContext(..),
			  pprMatch, getMatchLoc, pprMatchContext, isDoExpr,
			  mkMonoBind, nullMonoBinds, collectSigTysFromPats, andMonoBindList
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt, 
			  RenamedPat, RenamedMatchContext )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt, TcDictBinds, 
			  TcMonoBinds, TcPat, TcStmt )

import TcRnMonad
import TcMonoType	( tcAddScopedTyVars, tcHsSigType, UserTypeCtxt(..) )
import Inst		( tcSyntaxName )
import TcEnv		( TcId, tcLookupLocalIds, tcExtendLocalValEnv2 )
import TcPat		( tcPat, tcMonoPatBndr )
import TcMType		( newTyVarTy, zonkTcType, zapToType )
import TcType		( TcType, TcTyVar, tyVarsOfType, tidyOpenTypes, tidyOpenType,
			  mkFunTy, isOverloadedTy, liftedTypeKind, openTypeKind, 
			  mkArrowKind, mkAppTy )
import TcBinds		( tcBindsAndThen )
import TcUnify		( unifyPArrTy,subFunTy, unifyListTy, unifyTauTy,
			  checkSigTyVarsWrt, tcSubExp, isIdCoercion, (<$>) )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import Name		( Name )
import PrelNames	( monadNames )
import TysWiredIn	( boolTy, mkListTy, mkPArrTy )
import Id		( idType, mkSysLocal )
import CoreFVs		( idFreeTyVars )
import BasicTypes	( RecFlag(..) )
import VarSet
import Var		( Id )
import Bag
import Util		( isSingleton, lengthExceeds, notNull, zipEqual )
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
	     -> TcM [TcMatch]

tcMatchesFun xve fun_name expected_ty matches@(first_match:_)
  =	 -- Check that they all have the same no of arguments
	 -- Set the location to that of the first equation, so that
	 -- any inter-equation error messages get some vaguely
	 -- sensible location.	Note: we have to do this odd
	 -- ann-grabbing, because we don't always have annotations in
	 -- hand when we call tcMatchesFun...
    addSrcLoc (getMatchLoc first_match)	 (
	    checkTc (sameNoOfArgs matches)
		    (varyingArgsErr fun_name matches)
    )						 `thenM_`

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
			[TcMatch]) 	-- Translated alternatives

tcMatchesCase matches expr_ty
  = newTyVarTy openTypeKind 					`thenM` \ scrut_ty ->
    tcMatches [] CaseAlt matches (mkFunTy scrut_ty expr_ty)	`thenM` \ matches' ->
    returnM (scrut_ty, matches')

tcMatchLambda :: RenamedMatch -> TcType -> TcM TcMatch
tcMatchLambda match res_ty = tcMatch [] LambdaExpr match res_ty
\end{code}


\begin{code}
tcMatches :: [(Name,Id)]
	  -> RenamedMatchContext 
	  -> [RenamedMatch]
	  -> TcType
	  -> TcM [TcMatch]

tcMatches xve ctxt matches expected_ty
  = 	-- If there is more than one branch, and expected_ty is a 'hole',
	-- all branches must be types, not type schemes, otherwise the
	-- in which we check them would affect the result.
    (if lengthExceeds matches 1 then
	zapToType expected_ty
     else
	returnM expected_ty)			`thenM` \ expected_ty' ->

    mappM (tc_match expected_ty') matches
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
	-> TcM TcMatch

tcMatch xve1 ctxt match@(Match pats maybe_rhs_sig grhss) expected_ty
  = addSrcLoc (getMatchLoc match)		$	-- At one stage I removed this;
    addErrCtxt (matchCtxt ctxt match)		$	-- I'm not sure why, so I put it back
    tcMatchPats pats expected_ty tc_grhss	`thenM` \ (pats', grhss', ex_binds) ->
    returnM (Match pats' Nothing (glue_on Recursive ex_binds grhss'))

  where
    tc_grhss rhs_ty 
	= tcExtendLocalValEnv2 xve1 			$

		-- Deal with the result signature
	  case maybe_rhs_sig of
	    Nothing ->  tcGRHSs ctxt grhss rhs_ty

	    Just sig ->	 tcAddScopedTyVars [sig]	$
				-- Bring into scope the type variables in the signature
			 tcHsSigType ResSigCtxt sig	`thenM` \ sig_ty ->
			 tcGRHSs ctxt grhss sig_ty	`thenM` \ grhss' ->
			 tcSubExp rhs_ty sig_ty		`thenM` \ co_fn  ->
			 returnM (lift_grhss co_fn rhs_ty grhss')

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
	-> TcM TcGRHSs

tcGRHSs ctxt (GRHSs grhss binds _) expected_ty
  = tcBindsAndThen glue_on binds (tc_grhss grhss)
  where
    tc_grhss grhss
	= mappM tc_grhs grhss	    `thenM` \ grhss' ->
	  returnM (GRHSs grhss' EmptyBinds expected_ty)

    tc_grhs (GRHS guarded locn)
	= addSrcLoc locn				$
	  tcStmts ctxt (\ty -> ty, expected_ty) guarded	`thenM` \ guarded' ->
	  returnM (GRHS guarded' locn)
\end{code}


%************************************************************************
%*									*
\subsection{tcMatchPats}
%*									*
%************************************************************************

\begin{code}	  
tcMatchPats
	:: [RenamedPat] -> TcType
	-> (TcType -> TcM a)
	-> TcM ([TcPat], a, TcDictBinds)
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

tcMatchPats pats expected_ty thing_inside
  = 	-- STEP 1: Bring pattern-signature type variables into scope
    tcAddScopedTyVars (collectSigTysFromPats pats)	(

	-- STEP 2: Typecheck the patterns themselves, gathering all the stuff
	--	   then do the thing inside
        getLIE (tc_match_pats pats expected_ty thing_inside)

    ) `thenM` \ ((pats', ex_tvs, ex_ids, ex_lie, result), lie_req) -> 

	-- STEP 4: Check for existentially bound type variables
	-- Do this *outside* the scope of the tcAddScopedTyVars, else checkSigTyVars
	-- complains that 'a' is captured by the inscope 'a'!  (Test (d) in checkSigTyVars.)
	--
	-- I'm a bit concerned that lie_req1 from an 'inner' pattern in the list
	-- might need (via lie_req2) something made available from an 'outer' 
	-- pattern.  But it's inconvenient to deal with, and I can't find an example
    tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req expected_ty	`thenM` \ ex_binds ->
	-- NB: we *must* pass "expected_ty" not "result_ty" to tcCheckExistentialPat
	-- For example, we must reject this program:
	--	data C = forall a. C (a -> Int) 
	-- 	f (C g) x = g x
	-- Here, result_ty will be simply Int, but expected_ty is (a -> Int).

    returnM (pats', result, ex_binds)

tc_match_pats [] expected_ty thing_inside
  = thing_inside expected_ty 	`thenM` \ answer ->
    returnM ([], emptyBag, [], [], answer)

tc_match_pats (pat:pats) expected_ty thing_inside
  = subFunTy expected_ty		$ \ arg_ty rest_ty ->
	-- This is the unique place we call subFunTy
	-- The point is that if expected_y is a "hole", we want 
	-- to make arg_ty and rest_ty as "holes" too.
    tcPat tcMonoPatBndr pat arg_ty	`thenM` \ (pat', ex_tvs, pat_bndrs, ex_lie) ->
    let
	xve    = bagToList pat_bndrs
	ex_ids = [id | (_, id) <- xve]
		-- ex_ids is all the pattern-bound Ids, a superset
		-- of the existential Ids used in checkExistentialPat
    in
    tcExtendLocalValEnv2 xve 			$
    tc_match_pats pats rest_ty thing_inside	`thenM` \ (pats', exs_tvs, exs_ids, exs_lie, answer) ->
    returnM (	pat':pats',
		ex_tvs `unionBags` exs_tvs,
		ex_ids ++ exs_ids,
		ex_lie ++ exs_lie,
		answer
    )


tcCheckExistentialPat :: Bag TcTyVar	-- Existentially quantified tyvars bound by pattern
		      -> [TcId]		-- Ids bound by this pattern; used 
					--   (a) by bindsInstsOfLocalFuns
					--   (b) to generate helpful error messages
		      -> [Inst]		--   and context
		      -> [Inst]		-- Required context
		      -> TcType		--   and type of the Match; vars in here must not escape
		      -> TcM TcDictBinds	-- LIE to float out and dict bindings
tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req match_ty
  | isEmptyBag ex_tvs && all not_overloaded ex_ids
	-- Short cut for case when there are no existentials
	-- and no polymorphic overloaded variables
	--  e.g. f :: (forall a. Ord a => a -> a) -> Int -> Int
	--	 f op x = ....
	--  Here we must discharge op Methods
  = ASSERT( null ex_lie )
    extendLIEs lie_req		`thenM_` 
    returnM EmptyMonoBinds

  | otherwise
  = addErrCtxtM (sigPatCtxt tv_list ex_ids match_ty)		$

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    getLIE (bindInstsOfLocalFuns lie_req ex_ids)	`thenM` \ (inst_binds, lie) ->

	-- Deal with overloaded functions bound by the pattern
    tcSimplifyCheck doc tv_list ex_lie lie		`thenM` \ dict_binds ->
    checkSigTyVarsWrt (tyVarsOfType match_ty) tv_list	`thenM_` 

    returnM (dict_binds `AndMonoBinds` inst_binds)
  where
    doc     = text ("existential context of a data constructor")
    tv_list = bagToList ex_tvs
    not_overloaded id = not (isOverloadedTy (idType id))
\end{code}


%************************************************************************
%*									*
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
%*									*
%************************************************************************

\begin{code}
tcDoStmts :: HsDoContext -> [RenamedStmt] -> [Name] -> TcType
	  -> TcM (TcMonoBinds, [TcStmt], [Id])
tcDoStmts PArrComp stmts method_names res_ty
  = unifyPArrTy res_ty			  `thenM` \elt_ty ->
    tcStmts (DoCtxt PArrComp) 
	    (mkPArrTy, elt_ty) stmts      `thenM` \ stmts' ->
    returnM (EmptyMonoBinds, stmts', [{- unused -}])

tcDoStmts ListComp stmts method_names res_ty
  = unifyListTy res_ty			`thenM` \ elt_ty ->
    tcStmts (DoCtxt ListComp) 
	    (mkListTy, elt_ty) stmts	`thenM` \ stmts' ->
    returnM (EmptyMonoBinds, stmts', [{- unused -}])

tcDoStmts DoExpr stmts method_names res_ty
  = newTyVarTy (mkArrowKind liftedTypeKind liftedTypeKind)	`thenM` \ m_ty ->
    newTyVarTy liftedTypeKind 					`thenM` \ elt_ty ->
    unifyTauTy res_ty (mkAppTy m_ty elt_ty)			`thenM_`

    tcStmts (DoCtxt DoExpr) (mkAppTy m_ty, elt_ty) stmts	`thenM` \ stmts' ->

	-- Build the then and zero methods in case we need them
	-- It's important that "then" and "return" appear just once in the final LIE,
	-- not only for typechecker efficiency, but also because otherwise during
	-- simplification we end up with silly stuff like
	--	then = case d of (t,r) -> t
	--	then = then
	-- where the second "then" sees that it already exists in the "available" stuff.
	--
    mapAndUnzipM (tc_syn_name m_ty) 
	         (zipEqual "tcDoStmts" monadNames method_names)  `thenM` \ (binds, ids) ->
    returnM (andMonoBindList binds, stmts', ids)
  where
    tc_syn_name :: TcType -> (Name,Name) -> TcM (TcMonoBinds, Id)
    tc_syn_name m_ty (std_nm, usr_nm)
	= tcSyntaxName DoOrigin m_ty std_nm usr_nm 	`thenM` \ (expr, expr_ty) ->
	  case expr of
	    HsVar v -> returnM (EmptyMonoBinds, v)
	    other   -> newUnique		`thenM` \ uniq ->
		       let
			  id = mkSysLocal FSLIT("syn") uniq expr_ty
		       in
		       returnM (VarMonoBind id expr, id)
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
  = ASSERT( notNull stmts )
    tcStmtsAndThen (:) do_or_lc m_ty stmts (returnM [])

tcStmtsAndThen
	:: (TcStmt -> thing -> thing)	-- Combiner
	-> RenamedMatchContext
        -> (TcType -> TcType, TcType)	-- m, the relationship type of pat and rhs in pat <- rhs
					-- elt_ty, where type of the comprehension is (m elt_ty)
        -> [RenamedStmt]
	-> TcM thing
        -> TcM thing

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
  = addSrcLoc src_loc					$
    addErrCtxt (stmtCtxt do_or_lc stmt)		$
    newTyVarTy liftedTypeKind				`thenM` \ pat_ty ->
    tcMonoExpr exp (m pat_ty)				`thenM` \ exp' ->
    tcMatchPats [pat] (mkFunTy pat_ty (m elt_ty))	(\ _ ->
	popErrCtxt thing_inside
    )							`thenM` \ ([pat'], thing, dict_binds) ->
    returnM (combine (BindStmt pat' exp' src_loc)
		     (glue_binds combine Recursive dict_binds thing))

	-- ParStmt
tcStmtAndThen combine do_or_lc m_ty (ParStmtOut bndr_stmts_s) thing_inside
  = loop bndr_stmts_s		`thenM` \ (pairs', thing) ->
    returnM (combine (ParStmtOut pairs') thing)
  where
    loop []
      = thing_inside			`thenM` \ thing ->
	returnM ([], thing)

    loop ((bndrs,stmts) : pairs)
      = tcStmtsAndThen 
		combine_par (DoCtxt ListComp) m_ty stmts
			-- Notice we pass on m_ty; the result type is used only
			-- to get escaping type variables for checkExistentialPat
		(tcLookupLocalIds bndrs	`thenM` \ bndrs' ->
		 loop pairs		`thenM` \ (pairs', thing) ->
		 returnM ([], (bndrs', pairs', thing)))	`thenM` \ (stmts', (bndrs', pairs', thing)) ->

	returnM ((bndrs',stmts') : pairs', thing)

    combine_par stmt (stmts, thing) = (stmt:stmts, thing)

	-- ExprStmt
tcStmtAndThen combine do_or_lc m_ty@(m, res_elt_ty) stmt@(ExprStmt exp _ locn) thing_inside
  = setErrCtxt (stmtCtxt do_or_lc stmt) (
	if isDoExpr do_or_lc then
		newTyVarTy openTypeKind 	`thenM` \ any_ty ->
		tcMonoExpr exp (m any_ty)	`thenM` \ exp' ->
		returnM (ExprStmt exp' any_ty locn)
	else
		tcMonoExpr exp boolTy		`thenM` \ exp' ->
		returnM (ExprStmt exp' boolTy locn)
    )						`thenM` \ stmt' ->

    thing_inside 				`thenM` \ thing ->
    returnM (combine stmt' thing)


	-- Result statements
tcStmtAndThen combine do_or_lc m_ty@(m, res_elt_ty) stmt@(ResultStmt exp locn) thing_inside
  = setErrCtxt (stmtCtxt do_or_lc stmt) (
	if isDoExpr do_or_lc then
		tcMonoExpr exp (m res_elt_ty)
	else
		tcMonoExpr exp res_elt_ty
    )						`thenM` \ exp' ->

    thing_inside 				`thenM` \ thing ->

    returnM (combine (ResultStmt exp' locn) thing)


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
  = zonkTcType match_ty		`thenM` \ match_ty' ->
    let
	(env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
	(env2, tidy_mty) = tidyOpenType  env1     match_ty'
    in
    returnM (env1,
		 sep [ptext SLIT("When checking an existential match that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys)),
		      ptext SLIT("and whose type is") <+> ppr tidy_mty])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    ppr_id id ty     = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}
