%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcGRHSsPat, tcMatchesCase, tcMatchLambda,
		   matchCtxt,
		   tcDoStmts, tcStmtsAndThen, tcStmts, tcThingWithSig,
		   tcMatchPats,
		   TcStmtCtxt(..), TcMatchCtxt(..)
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcCheckRho, tcMonoExpr )

import HsSyn		( HsExpr(..), HsBinds(..), Match(..), GRHSs(..), GRHS(..),
			  MonoBinds(..), Stmt(..), HsMatchContext(..), HsStmtContext(..),
			  ReboundNames,
			  pprMatch, getMatchLoc, isDoExpr,
			  pprMatchContext, pprStmtContext, pprStmtResultContext,
			  mkMonoBind, collectSigTysFromPats, andMonoBindList, glueBindsOnGRHSs
			)
import RnHsSyn		( RenamedMatch, RenamedGRHSs, RenamedStmt, RenamedHsExpr,
			  RenamedPat, RenamedMatchContext )
import TcHsSyn		( TcMatch, TcGRHSs, TcStmt, TcDictBinds, TcHsBinds, TcExpr,
			  TcMonoBinds, TcPat, TcStmt, ExprCoFn,
			  isIdCoercion, (<$>), (<.>) )

import TcRnMonad
import TcMonoType	( tcAddScopedTyVars, tcHsSigType, UserTypeCtxt(..) )
import Inst		( tcSyntaxName, tcInstCall )
import TcEnv		( TcId, tcLookupLocalIds, tcLookupId, tcExtendLocalValEnv, tcExtendLocalValEnv2 )
import TcPat		( tcPat, tcMonoPatBndr )
import TcMType		( newTyVarTy, newTyVarTys, zonkTcType ) 
import TcType		( TcType, TcTyVar, TcSigmaType, TcRhoType,
			  tyVarsOfTypes, tidyOpenTypes, tidyOpenType, isSigmaTy,
			  mkFunTy, isOverloadedTy, liftedTypeKind, openTypeKind, 
			  mkArrowKind, mkAppTy )
import TcBinds		( tcBindsAndThen )
import TcUnify		( Expected(..), newHole, zapExpectedType, zapExpectedBranches, readExpectedType,
			  unifyTauTy, subFunTys, unifyPArrTy, unifyListTy, unifyFunTy,
			  checkSigTyVarsWrt, tcSubExp, tcGen )
import TcSimplify	( tcSimplifyCheck, bindInstsOfLocalFuns )
import Name		( Name )
import PrelNames	( monadNames, mfixName )
import TysWiredIn	( boolTy, mkListTy, mkPArrTy )
import Id		( idType, mkSysLocal, mkLocalId )
import CoreFVs		( idFreeTyVars )
import BasicTypes	( RecFlag(..) )
import VarSet
import Var		( Id )
import Bag
import Util		( isSingleton, notNull, zipEqual )
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
tcMatchesFun :: Name
	     -> [RenamedMatch]
	     -> Expected TcRhoType 		-- Expected type
	     -> TcM [TcMatch]

tcMatchesFun fun_name matches@(first_match:_) expected_ty
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

	-- No need to zonk expected_ty, because subFunTys does that on the fly
    tcMatches match_ctxt matches expected_ty
  where
    match_ctxt = MC { mc_what = FunRhs fun_name,
		      mc_body = tcMonoExpr }
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: TcMatchCtxt		-- Case context
	      -> [RenamedMatch]		-- The case alternatives
	      -> Expected TcRhoType 	-- Type of whole case expressions
	      -> TcM (TcRhoType,	-- Inferred type of the scrutinee
		      [TcMatch]) 	-- Translated alternatives

tcMatchesCase ctxt matches (Check expr_ty)
  = 	-- This case is a bit yukky, because it prevents the
	-- scrutinee being higher-ranked, which might just possible
	-- matter if we were seq'ing on it.  But it's awkward to fix.
    newTyVarTy openTypeKind 						`thenM` \ scrut_ty ->
    tcMatches ctxt matches (Check (mkFunTy scrut_ty expr_ty))	`thenM` \ matches' ->
    returnM (scrut_ty, matches')

tcMatchesCase ctxt matches (Infer hole)
  = newHole					`thenM` \ fun_hole ->
    tcMatches ctxt matches (Infer fun_hole)	`thenM` \ matches' ->
    readMutVar fun_hole				`thenM` \ fun_ty ->
	-- The result of tcMatches is bound to be a function type
    unifyFunTy fun_ty				`thenM` \ (scrut_ty, res_ty) ->
    writeMutVar hole res_ty			`thenM_` 
    returnM (scrut_ty, matches')
    

tcMatchLambda :: RenamedMatch -> Expected TcRhoType -> TcM TcMatch
tcMatchLambda match res_ty = tcMatch match_ctxt match res_ty
  where
    match_ctxt = MC { mc_what = LambdaExpr,
		      mc_body = tcMonoExpr }
\end{code}

@tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.

\begin{code}
tcGRHSsPat :: RenamedGRHSs
	   -> Expected TcRhoType
	   -> TcM TcGRHSs
tcGRHSsPat grhss exp_ty = tcGRHSs match_ctxt grhss exp_ty
  where
    match_ctxt = MC { mc_what = PatBindRhs,
		      mc_body = tcMonoExpr }
\end{code}

\begin{code}
data TcMatchCtxt 
  = MC { mc_what :: RenamedMatchContext,		-- What kind of thing this is
    	 mc_body :: RenamedHsExpr -> Expected TcRhoType -> TcM TcExpr }	-- Type checker for a body of an alternative

tcMatches :: TcMatchCtxt
	  -> [RenamedMatch]
	  -> Expected TcRhoType
	  -> TcM [TcMatch]

tcMatches ctxt matches exp_ty
  = 	-- If there is more than one branch, and exp_ty is a 'hole',
	-- all branches must be types, not type schemes, otherwise the
	-- order in which we check them would affect the result.
    zapExpectedBranches matches exp_ty 	`thenM` \ exp_ty' ->
    mappM (tc_match exp_ty') matches
  where
    tc_match exp_ty match = tcMatch ctxt match exp_ty
\end{code}


%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatch :: TcMatchCtxt
	-> RenamedMatch
	-> Expected TcRhoType 	-- Expected result-type of the Match.
			-- Early unification with this guy gives better error messages
			-- We regard the Match as having type 
			--	(ty1 -> ... -> tyn -> result_ty)
			-- where there are n patterns.
	-> TcM TcMatch

tcMatch ctxt match@(Match pats maybe_rhs_sig grhss) expected_ty
  = addSrcLoc (getMatchLoc match)		$	-- At one stage I removed this;
    addErrCtxt (matchCtxt (mc_what ctxt) match)	$	-- I'm not sure why, so I put it back
    subFunTys pats expected_ty			$ \ pats_w_tys rhs_ty ->
	-- This is the unique place we call subFunTys
	-- The point is that if expected_y is a "hole", we want 
	-- to make arg_ty and rest_ty as "holes" too.
    tcMatchPats pats_w_tys rhs_ty (tc_grhss rhs_ty)	`thenM` \ (pats', grhss', ex_binds) ->
    returnM (Match pats' Nothing (glueBindsOnGRHSs ex_binds grhss'))

  where
    tc_grhss rhs_ty 
	= case maybe_rhs_sig of  -- Deal with the result signature
	    Nothing  ->  tcGRHSs ctxt grhss rhs_ty

	    Just sig ->	 tcAddScopedTyVars [sig]	$
				-- Bring into scope the type variables in the signature
			 tcHsSigType ResSigCtxt sig					`thenM` \ sig_ty ->
			 tcThingWithSig sig_ty (tcGRHSs ctxt grhss . Check) rhs_ty	`thenM` \ (co_fn, grhss') ->

			-- Pushes the coercion down to the right hand sides,
			-- because there is no convenient place to hang it otherwise.
			 if isIdCoercion co_fn then
				returnM grhss'
			 else
			 readExpectedType rhs_ty		`thenM` \ rhs_ty' ->
			 returnM (lift_grhss co_fn rhs_ty' grhss')

lift_grhss co_fn rhs_ty (GRHSs grhss binds ty)
  = GRHSs (map lift_grhs grhss) binds rhs_ty	-- Change the type, since the coercion does
  where
    lift_grhs (GRHS stmts loc) = GRHS (map lift_stmt stmts) loc
	      
    lift_stmt (ResultStmt e l) = ResultStmt (co_fn <$> e) l
    lift_stmt stmt	       = stmt

tcGRHSs :: TcMatchCtxt -> RenamedGRHSs
	-> Expected TcRhoType
	-> TcM TcGRHSs

  -- Special case when there is just one equation with a degenerate 
  -- guard; then we pass in the full Expected type, so that we get
  -- good inference from simple things like
  --	f = \(x::forall a.a->a) -> <stuff>
  -- This is a consequence of the fact that tcStmts takes a TcType,
  -- not a Expected TcType, a decision we could revisit if necessary
tcGRHSs ctxt (GRHSs [GRHS [ResultStmt rhs loc1] loc2] binds _) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    mc_body ctxt rhs exp_ty			`thenM` \ rhs' ->
    readExpectedType exp_ty			`thenM` \ exp_ty' ->
    returnM (GRHSs [GRHS [ResultStmt rhs' loc1] loc2] EmptyBinds exp_ty')

tcGRHSs ctxt (GRHSs grhss binds _) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    zapExpectedType exp_ty			`thenM` \ exp_ty' ->
	-- Even if there is only one guard, we zap the RHS type to
	-- a monotype.  Reason: it makes tcStmts much easier,
	-- and even a one-armed guard has a notional second arm
    let
      stmt_ctxt = SC { sc_what = PatGuard (mc_what ctxt), 
		       sc_rhs  = tcCheckRho, 
		       sc_body = sc_body,
		       sc_ty   = exp_ty' }
      sc_body body = mc_body ctxt body (Check exp_ty')

      tc_grhs (GRHS guarded locn)
	= addSrcLoc locn		$
	  tcStmts stmt_ctxt  guarded	`thenM` \ guarded' ->
	  returnM (GRHS guarded' locn)
    in
    mappM tc_grhs grhss			`thenM` \ grhss' ->
    returnM (GRHSs grhss' EmptyBinds exp_ty')
\end{code}


\begin{code}
tcThingWithSig :: TcSigmaType 		-- Type signature
	       -> (TcRhoType -> TcM r) 	-- How to type check the thing inside
	       -> Expected TcRhoType 	-- Overall expected result type
	       -> TcM (ExprCoFn, r)
-- Used for expressions with a type signature, and for result type signatures

tcThingWithSig sig_ty thing_inside res_ty
  | not (isSigmaTy sig_ty)
  = thing_inside sig_ty		`thenM` \ result ->
    tcSubExp res_ty sig_ty	`thenM` \ co_fn ->
    returnM (co_fn, result)

  | otherwise 	-- The signature has some outer foralls
  =	-- Must instantiate the outer for-alls of sig_tc_ty
	-- else we risk instantiating a ? res_ty to a forall-type
	-- which breaks the invariant that tcMonoExpr only returns phi-types
    tcGen sig_ty emptyVarSet thing_inside	`thenM` \ (gen_fn, result) ->
    tcInstCall SignatureOrigin sig_ty		`thenM` \ (inst_fn, inst_sig_ty) ->
    tcSubExp res_ty inst_sig_ty			`thenM` \ co_fn ->
    returnM (co_fn <.> inst_fn <.> gen_fn,  result)
	-- Note that we generalise, then instantiate. Ah well.
\end{code}


%************************************************************************
%*									*
\subsection{tcMatchPats}
%*									*
%************************************************************************

\begin{code}	  
tcMatchPats
	:: [(RenamedPat, Expected TcRhoType)]
	-> Expected TcRhoType
	-> TcM a
	-> TcM ([TcPat], a, TcHsBinds)
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

tcMatchPats pats_w_tys body_ty thing_inside
  = 	-- STEP 1: Bring pattern-signature type variables into scope
    tcAddScopedTyVars (collectSigTysFromPats (map fst pats_w_tys))	(

	-- STEP 2: Typecheck the patterns themselves, gathering all the stuff
	--	   then do the thing inside
        getLIE (tc_match_pats pats_w_tys thing_inside)

    ) `thenM` \ ((pats', ex_tvs, ex_ids, ex_lie, result), lie_req) -> 

	-- STEP 4: Check for existentially bound type variables
	-- Do this *outside* the scope of the tcAddScopedTyVars, else checkSigTyVars
	-- complains that 'a' is captured by the inscope 'a'!  (Test (d) in checkSigTyVars.)
	--
	-- I'm a bit concerned that lie_req1 from an 'inner' pattern in the list
	-- might need (via lie_req2) something made available from an 'outer' 
	-- pattern.  But it's inconvenient to deal with, and I can't find an example
    tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req 
			  pats_w_tys body_ty 		`thenM` \ ex_binds ->
	-- NB: we *must* pass "pats_w_tys" not just "body_ty" to tcCheckExistentialPat
	-- For example, we must reject this program:
	--	data C = forall a. C (a -> Int) 
	-- 	f (C g) x = g x
	-- Here, result_ty will be simply Int, but expected_ty is (C -> a -> Int).

    returnM (pats', result, mkMonoBind Recursive ex_binds)

tc_match_pats [] thing_inside
  = thing_inside 	`thenM` \ answer ->
    returnM ([], emptyBag, [], [], answer)

tc_match_pats ((pat,pat_ty):pats) thing_inside
  = tcPat tcMonoPatBndr pat pat_ty	`thenM` \ (pat', ex_tvs, pat_bndrs, ex_lie) ->
    let
	xve    = bagToList pat_bndrs
	ex_ids = [id | (_, id) <- xve]
		-- ex_ids is all the pattern-bound Ids, a superset
		-- of the existential Ids used in checkExistentialPat
    in
    tcExtendLocalValEnv2 xve 			$
    tc_match_pats pats thing_inside	`thenM` \ (pats', exs_tvs, exs_ids, exs_lie, answer) ->
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
		      -> [(pat,Expected TcRhoType)] 	-- Types of the patterns
		      -> Expected TcRhoType		-- Type of the body of the match
		      					-- Tyvars in either of these must not escape
		      -> TcM TcDictBinds	-- LIE to float out and dict bindings
tcCheckExistentialPat ex_tvs ex_ids ex_lie lie_req pats_w_tys body_ty
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
  =	-- Read the by-now-filled-in expected types
    mapM readExpectedType (body_ty : map snd pats_w_tys)	`thenM` \ tys ->
    addErrCtxtM (sigPatCtxt tv_list ex_ids tys)			$

    	-- In case there are any polymorpic, overloaded binders in the pattern
	-- (which can happen in the case of rank-2 type signatures, or data constructors
	-- with polymorphic arguments), we must do a bindInstsOfLocalFns here
    getLIE (bindInstsOfLocalFuns lie_req ex_ids)	`thenM` \ (inst_binds, lie) ->

	-- Deal with overloaded functions bound by the pattern
    tcSimplifyCheck doc tv_list ex_lie lie		`thenM` \ dict_binds ->

	-- Check for type variable escape
    checkSigTyVarsWrt (tyVarsOfTypes tys) tv_list		`thenM_` 

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
tcDoStmts :: HsStmtContext Name 
	  -> [RenamedStmt] -> ReboundNames Name
	  -> TcRhoType		-- To keep it simple, we don't have an "expected" type here
	  -> TcM ([TcStmt], ReboundNames TcId)
tcDoStmts PArrComp stmts method_names res_ty
  = unifyPArrTy res_ty					`thenM` \elt_ty ->
    tcComprehension PArrComp mkPArrTy elt_ty stmts	`thenM` \ stmts' ->
    returnM (stmts', [{- unused -}])

tcDoStmts ListComp stmts method_names res_ty
  = unifyListTy res_ty				`	thenM` \ elt_ty ->
    tcComprehension ListComp mkListTy elt_ty stmts	`thenM` \ stmts' ->
    returnM (stmts', [{- unused -}])

tcDoStmts do_or_mdo stmts method_names res_ty
  = newTyVarTy (mkArrowKind liftedTypeKind liftedTypeKind)	`thenM` \ m_ty ->
    newTyVarTy liftedTypeKind 					`thenM` \ elt_ty ->
    unifyTauTy res_ty (mkAppTy m_ty elt_ty)			`thenM_`
    let
	ctxt = SC { sc_what = do_or_mdo,
		    sc_rhs  = \ rhs rhs_elt_ty -> tcCheckRho rhs (mkAppTy m_ty rhs_elt_ty),
		    sc_body = \ body -> tcCheckRho body res_ty,
		    sc_ty   = res_ty }
    in	
    tcStmts ctxt stmts						`thenM` \ stmts' ->

	-- Build the then and zero methods in case we need them
	-- It's important that "then" and "return" appear just once in the final LIE,
	-- not only for typechecker efficiency, but also because otherwise during
	-- simplification we end up with silly stuff like
	--	then = case d of (t,r) -> t
	--	then = then
	-- where the second "then" sees that it already exists in the "available" stuff.
    mapM (tcSyntaxName DoOrigin m_ty) method_names		  `thenM` \ methods ->

    returnM (stmts', methods)

tcComprehension do_or_lc mk_mty elt_ty stmts
  = tcStmts ctxt stmts
  where
    ctxt = SC { sc_what = do_or_lc,
		sc_rhs  = \ rhs rhs_elt_ty -> tcCheckRho rhs (mk_mty rhs_elt_ty),
		sc_body = \ body -> tcCheckRho body elt_ty,	-- Note: no mk_mty!
		sc_ty   = mk_mty elt_ty }
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
tcStmts ctxt stmts
  = ASSERT( notNull stmts )
    tcStmtsAndThen (:) ctxt stmts (returnM [])

data TcStmtCtxt 
  = SC { sc_what :: HsStmtContext Name,		-- What kind of thing this is
    	 sc_rhs  :: RenamedHsExpr -> TcType -> TcM TcExpr,	-- Type checker for RHS computations
	 sc_body :: RenamedHsExpr -> TcM TcExpr,		-- Type checker for return computation
	 sc_ty   :: TcType }					-- Return type; used *only* to check
								-- for escape in existential patterns
tcStmtsAndThen
	:: (TcStmt -> thing -> thing)	-- Combiner
	-> TcStmtCtxt
        -> [RenamedStmt]
	-> TcM thing
        -> TcM thing

	-- Base case
tcStmtsAndThen combine ctxt [] thing_inside
  = thing_inside

tcStmtsAndThen combine ctxt (stmt:stmts) thing_inside
  = tcStmtAndThen  combine ctxt stmt  $
    tcStmtsAndThen combine ctxt stmts $
    thing_inside

	-- LetStmt
tcStmtAndThen combine ctxt (LetStmt binds) thing_inside
  = tcBindsAndThen		-- No error context, but a binding group is
  	(glue_binds combine)	-- rather a large thing for an error context anyway
  	binds
  	thing_inside

	-- BindStmt
tcStmtAndThen combine ctxt stmt@(BindStmt pat exp src_loc) thing_inside
  = addSrcLoc src_loc					$
    addErrCtxt (stmtCtxt ctxt stmt)			$
    newTyVarTy liftedTypeKind				`thenM` \ pat_ty ->
    sc_rhs ctxt exp pat_ty				`thenM` \ exp' ->
    tcMatchPats [(pat, Check pat_ty)] (Check (sc_ty ctxt)) (
	popErrCtxt thing_inside
    )							`thenM` \ ([pat'], thing, dict_binds) ->
    returnM (combine (BindStmt pat' exp' src_loc)
		     (glue_binds combine dict_binds thing))

	-- ExprStmt
tcStmtAndThen combine ctxt stmt@(ExprStmt exp _ src_loc) thing_inside
  = addSrcLoc src_loc		(
    	addErrCtxt (stmtCtxt ctxt stmt) $
	if isDoExpr (sc_what ctxt)
	then	-- do or mdo; the expression is a computation
		newTyVarTy openTypeKind 	`thenM` \ any_ty ->
		sc_rhs ctxt exp any_ty		`thenM` \ exp' ->
		returnM (ExprStmt exp' any_ty src_loc)
	else	-- List comprehensions, pattern guards; expression is a boolean
		tcCheckRho exp boolTy		`thenM` \ exp' ->
		returnM (ExprStmt exp' boolTy src_loc)
    )						`thenM` \ stmt' ->

    thing_inside 				`thenM` \ thing ->
    returnM (combine stmt' thing)


	-- ParStmt
tcStmtAndThen combine ctxt (ParStmt bndr_stmts_s) thing_inside
  = loop bndr_stmts_s		`thenM` \ (pairs', thing) ->
    returnM (combine (ParStmt pairs') thing)
  where
    loop [] = thing_inside		`thenM` \ thing ->
	      returnM ([], thing)

    loop ((stmts, bndrs) : pairs)
      = tcStmtsAndThen combine_par ctxt stmts $
			-- Notice we pass on ctxt; the result type is used only
			-- to get escaping type variables for checkExistentialPat
	tcLookupLocalIds bndrs		`thenM` \ bndrs' ->
	loop pairs			`thenM` \ (pairs', thing) ->
	returnM (([], bndrs') : pairs', thing)

    combine_par stmt ((stmts, bndrs) : pairs , thing) = ((stmt:stmts, bndrs) : pairs, thing)

	-- RecStmt
tcStmtAndThen combine ctxt (RecStmt stmts laterNames recNames _) thing_inside
  = newTyVarTys (length recNames) liftedTypeKind		`thenM` \ recTys ->
    let
	rec_ids = zipWith mkLocalId recNames recTys
    in
    tcExtendLocalValEnv rec_ids			$
    tcStmtsAndThen combine_rec ctxt stmts (
	mappM tc_ret (recNames `zip` recTys)	`thenM` \ rec_rets ->
	tcLookupLocalIds laterNames		`thenM` \ later_ids ->
	returnM ([], (later_ids, rec_rets))
    )						`thenM` \ (stmts', (later_ids, rec_rets)) ->

    tcExtendLocalValEnv later_ids		$
	-- NB:	The rec_ids for the recursive things 
	-- 	already scope over this part
    thing_inside				`thenM` \ thing ->
  
    returnM (combine (RecStmt stmts' later_ids rec_ids rec_rets) thing)
  where 
    combine_rec stmt (stmts, thing) = (stmt:stmts, thing)

    -- Unify the types of the "final" Ids with those of "knot-tied" Ids
    tc_ret (rec_name, mono_ty)
	= tcLookupId rec_name				`thenM` \ poly_id ->
		-- poly_id may have a polymorphic type
		-- but mono_ty is just a monomorphic type variable
	  tcSubExp (Check mono_ty) (idType poly_id) 	`thenM` \ co_fn ->
	  returnM (co_fn <$> HsVar poly_id) 

	-- Result statements
tcStmtAndThen combine ctxt stmt@(ResultStmt exp locn) thing_inside
  = addErrCtxt (stmtCtxt ctxt stmt) (sc_body ctxt exp)	`thenM` \ exp' ->
    thing_inside 					`thenM` \ thing ->
    returnM (combine (ResultStmt exp' locn) thing)


------------------------------
glue_binds combine EmptyBinds  thing = thing
glue_binds combine other_binds thing = combine (LetStmt other_binds) thing
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

matchCtxt ctxt  match  = hang (ptext SLIT("In") <+> pprMatchContext ctxt <> colon) 
			      4 (pprMatch ctxt match)

stmtCtxt ctxt stmt = hang (ptext SLIT("In") <+> pp_ctxt (sc_what ctxt) <> colon) 4 (ppr stmt)
	where
	  pp_ctxt  = case stmt of
			ResultStmt _ _ -> pprStmtResultContext
			other	       -> pprStmtContext
			
sigPatCtxt bound_tvs bound_ids tys tidy_env 
  = 	-- tys is (body_ty : pat_tys)  
    mapM zonkTcType tys		`thenM` \ tys' ->
    let
	(env1, tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
	(env2, tidy_body_ty : tidy_pat_tys) = tidyOpenTypes env1 tys'
    in
    returnM (env1,
		 sep [ptext SLIT("When checking an existential match that binds"),
		      nest 4 (vcat (zipWith ppr_id show_ids tidy_tys)),
		      ptext SLIT("The pattern(s) have type(s):") <+> vcat (map ppr tidy_pat_tys),
		      ptext SLIT("The body has type:") <+> ppr tidy_body_ty
		])
  where
    show_ids = filter is_interesting bound_ids
    is_interesting id = any (`elemVarSet` idFreeTyVars id) bound_tvs

    ppr_id id ty = ppr id <+> dcolon <+> ppr ty
	-- Don't zonk the types so we get the separate, un-unified versions
\end{code}
