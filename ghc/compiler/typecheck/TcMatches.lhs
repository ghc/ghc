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

import {-# SOURCE #-}	TcExpr( tcCheckRho, tcInferRho, tcMonoExpr )

import HsSyn		( HsExpr(..), LHsExpr, MatchGroup(..),
			  Match(..), LMatch, GRHSs(..), GRHS(..), 
			  Stmt(..), LStmt, HsMatchContext(..), HsStmtContext(..),
			  ReboundNames, LPat, HsBindGroup(..),
			  pprMatch, isDoExpr,
			  pprMatchContext, pprStmtContext, pprStmtResultContext,
			  collectPatsBinders, glueBindsOnGRHSs
			)
import TcHsSyn		( ExprCoFn, isIdCoercion, (<$>), (<.>) )

import TcRnMonad
import TcHsType		( tcHsPatSigType, UserTypeCtxt(..) )
import Inst		( tcSyntaxName, tcInstCall )
import TcEnv		( TcId, tcLookupLocalIds, tcLookupId, tcExtendIdEnv, 
			  tcExtendTyVarEnv )
import TcPat		( PatCtxt(..), tcPats )
import TcMType		( newTyFlexiVarTy, newTyFlexiVarTys, zonkTcType ) 
import TcType		( TcType, TcTyVar, TcSigmaType, TcRhoType, mkFunTys,
			  tyVarsOfTypes, tidyOpenTypes, isSigmaTy, mkTyConApp,
			  liftedTypeKind, openTypeKind, mkArrowKind, mkAppTy )
import TcBinds		( tcBindsAndThen )
import TcUnify		( Expected(..), zapExpectedType, readExpectedType,
			  unifyTauTy, subFunTys, unifyListTy, unifyTyConApp,
			  checkSigTyVarsWrt, zapExpectedBranches, tcSubExp, tcGen,
			  unifyAppTy )
import TcSimplify	( bindInstsOfLocalFuns )
import Name		( Name )
import TysWiredIn	( boolTy, parrTyCon, listTyCon )
import Id		( idType, mkLocalId )
import CoreFVs		( idFreeTyVars )
import VarSet
import BasicTypes	( RecFlag(..) )
import Util		( isSingleton, notNull )
import Outputable
import SrcLoc		( Located(..), noLoc )

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
	     -> MatchGroup Name
	     -> Expected TcRhoType 	-- Expected type of function
	     -> TcM (MatchGroup TcId)	-- Returns type of body

tcMatchesFun fun_name matches exp_ty
  = do	{  -- Check that they all have the same no of arguments
	   -- Location is in the monad, set the caller so that 
	   -- any inter-equation error messages get some vaguely
	   -- sensible location.	Note: we have to do this odd
	   -- ann-grabbing, because we don't always have annotations in
	   -- hand when we call tcMatchesFun...
	  checkTc (sameNoOfArgs matches) (varyingArgsErr fun_name matches)

	-- ToDo: Don't use "expected" stuff if there ain't a type signature
	-- because inconsistency between branches
	-- may show up as something wrong with the (non-existent) type signature

		-- This is one of two places places we call subFunTys
		-- The point is that if expected_y is a "hole", we want 
		-- to make pat_tys and rhs_ty as "holes" too.
	; exp_ty' <- zapExpectedBranches matches exp_ty
	; subFunTys matches exp_ty' 	$ \ pat_tys rhs_ty -> 
	  tcMatches match_ctxt pat_tys rhs_ty matches
	}
  where
    match_ctxt = MC { mc_what = FunRhs fun_name,
		      mc_body = tcMonoExpr }
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: TcMatchCtxt		-- Case context
	      -> TcRhoType		-- Type of scrutinee
	      -> MatchGroup Name	-- The case alternatives
	      -> Expected TcRhoType 	-- Type of whole case expressions
	      -> TcM (MatchGroup TcId)	-- Translated alternatives

tcMatchesCase ctxt scrut_ty matches exp_ty
  = do	{ exp_ty' <- zapExpectedBranches matches exp_ty
	; tcMatches ctxt [Check scrut_ty] exp_ty' matches }

tcMatchLambda :: MatchGroup Name -> Expected TcRhoType -> TcM (MatchGroup TcId)
tcMatchLambda match exp_ty 	-- One branch so no unifyBranches needed
  = subFunTys match exp_ty 	$ \ pat_tys rhs_ty ->
    tcMatches match_ctxt pat_tys rhs_ty match
  where
    match_ctxt = MC { mc_what = LambdaExpr,
		      mc_body = tcMonoExpr }
\end{code}

@tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.

\begin{code}
tcGRHSsPat :: GRHSs Name
	   -> Expected TcRhoType
	   -> TcM (GRHSs TcId)
tcGRHSsPat grhss exp_ty = tcGRHSs match_ctxt grhss exp_ty
  where
    match_ctxt = MC { mc_what = PatBindRhs,
		      mc_body = tcMonoExpr }
\end{code}


%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatches :: TcMatchCtxt
	  -> [Expected TcRhoType] 	-- Expected pattern types
	  -> Expected TcRhoType		-- Expected result-type of the Match.
	  -> MatchGroup Name
	  -> TcM (MatchGroup TcId)

data TcMatchCtxt 	-- c.f. TcStmtCtxt, also in this module
  = MC { mc_what :: HsMatchContext Name,	-- What kind of thing this is
    	 mc_body :: LHsExpr Name 		-- Type checker for a body of an alternative
		 -> Expected TcRhoType 
		 -> TcM (LHsExpr TcId) }	

tcMatches ctxt pat_tys rhs_ty (MatchGroup matches _)
  = do	{ matches' <- mapM (tcMatch ctxt pat_tys rhs_ty) matches
	; pat_tys' <- mapM readExpectedType pat_tys
	; rhs_ty'  <- readExpectedType rhs_ty
	; return (MatchGroup matches' (mkFunTys pat_tys' rhs_ty')) }

-------------
tcMatch :: TcMatchCtxt
	-> [Expected TcRhoType] 	-- Expected pattern types
	-> Expected TcRhoType	 	-- Expected result-type of the Match.
	-> LMatch Name
	-> TcM (LMatch TcId)

tcMatch ctxt pat_tys rhs_ty match 
  = wrapLocM (tc_match ctxt pat_tys rhs_ty) match

tc_match ctxt pat_tys rhs_ty match@(Match pats maybe_rhs_sig grhss)
  = addErrCtxt (matchCtxt (mc_what ctxt) match)	$	
    do	{ (pats', grhss') <- tcMatchPats pats pat_tys rhs_ty $
			     tc_grhss ctxt maybe_rhs_sig grhss rhs_ty
	; returnM (Match pats' Nothing grhss') }


-------------
tc_grhss ctxt Nothing grhss rhs_ty 
  = tcGRHSs ctxt grhss rhs_ty	-- No result signature

tc_grhss ctxt (Just res_sig) grhss rhs_ty 
  = do	{ (sig_tvs, sig_ty) <- tcHsPatSigType ResSigCtxt res_sig
	; traceTc (text "tc_grhss" <+> ppr sig_tvs)
	; (co_fn, grhss') <- tcExtendTyVarEnv sig_tvs $
			     tcThingWithSig sig_ty (tcGRHSs ctxt grhss . Check) rhs_ty

		-- Push the coercion down to the right hand sides,
		-- because there is no convenient place to hang it otherwise.
	; if isIdCoercion co_fn then
		return grhss'
	  else
		return (lift_grhss co_fn grhss') }

-------------
lift_grhss co_fn (GRHSs grhss binds)
  = GRHSs (map (fmap lift_grhs) grhss) binds
  where
    lift_grhs (GRHS stmts) = GRHS (map lift_stmt stmts)
	      
    lift_stmt (L loc (ResultStmt e)) = L loc (ResultStmt (fmap (co_fn <$>) e))
    lift_stmt stmt	  	     = stmt

-------------
tcGRHSs :: TcMatchCtxt -> GRHSs Name
	-> Expected TcRhoType
	-> TcM (GRHSs TcId)

  -- Special case when there is just one equation with a degenerate 
  -- guard; then we pass in the full Expected type, so that we get
  -- good inference from simple things like
  --	f = \(x::forall a.a->a) -> <stuff>
  -- This is a consequence of the fact that tcStmts takes a TcType,
  -- not a Expected TcType, a decision we could revisit if necessary
tcGRHSs ctxt (GRHSs [L loc1 (GRHS [L loc2 (ResultStmt rhs)])] binds) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    mc_body ctxt rhs exp_ty			`thenM` \ rhs' ->
    returnM (GRHSs [L loc1 (GRHS [L loc2 (ResultStmt rhs')])] [])

tcGRHSs ctxt (GRHSs grhss binds) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    zapExpectedType exp_ty openTypeKind		`thenM` \ exp_ty' ->
	-- Even if there is only one guard, we zap the RHS type to
	-- a monotype.  Reason: it makes tcStmts much easier,
	-- and even a one-armed guard has a notional second arm
    let
      stmt_ctxt = SC { sc_what = PatGuard (mc_what ctxt), 
		       sc_rhs  = tcInferRho, 
		       sc_body = sc_body,
		       sc_ty   = exp_ty' }
      sc_body body = mc_body ctxt body (Check exp_ty')

      tc_grhs (GRHS guarded)
	= tcStmts stmt_ctxt  guarded	`thenM` \ guarded' ->
	  returnM (GRHS guarded')
    in
    mappM (wrapLocM tc_grhs) grhss	`thenM` \ grhss' ->
    returnM (GRHSs grhss' [])
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
    tcInstCall InstSigOrigin sig_ty		`thenM` \ (inst_fn, _, inst_sig_ty) ->
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
tcMatchPats :: [LPat Name] 
	    -> [Expected TcSigmaType]	-- Pattern types
	    -> Expected TcRhoType	-- Result type;
					-- used only to check existential escape
	    -> TcM a
	    -> TcM ([LPat TcId], a)
-- Typecheck the patterns, extend the environment to bind the variables,
-- do the thing inside, use any existentially-bound dictionaries to 
-- discharge parts of the returning LIE, and deal with pattern type
-- signatures

tcMatchPats pats tys body_ty thing_inside
  = do	{ (pats', ex_tvs, res) <- tcPats LamPat pats tys thing_inside 
	; tcCheckExistentialPat pats' ex_tvs tys body_ty
	; returnM (pats', res) }
  where

tcCheckExistentialPat :: [LPat TcId]		-- Patterns (just for error message)
		      -> [TcTyVar]		-- Existentially quantified tyvars bound by pattern
		      -> [Expected TcSigmaType] -- Types of the patterns
		      -> Expected TcRhoType	-- Type of the body of the match
		      				-- Tyvars in either of these must not escape
		      -> TcM ()
	-- NB: we *must* pass "pats_tys" not just "body_ty" to tcCheckExistentialPat
	-- For example, we must reject this program:
	--	data C = forall a. C (a -> Int) 
	-- 	f (C g) x = g x
	-- Here, result_ty will be simply Int, but expected_ty is (C -> a -> Int).

tcCheckExistentialPat pats [] pat_tys body_ty
  = return ()	-- Short cut for case when there are no existentials

tcCheckExistentialPat pats ex_tvs pat_tys body_ty
  = do	{ tys <- mapM readExpectedType (body_ty : pat_tys)
	; addErrCtxtM (sigPatCtxt (collectPatsBinders pats) ex_tvs tys)	$
    	  checkSigTyVarsWrt (tyVarsOfTypes tys) ex_tvs }
\end{code}


%************************************************************************
%*									*
\subsection{@tcDoStmts@ typechecks a {\em list} of do statements}
%*									*
%************************************************************************

\begin{code}
tcDoStmts :: HsStmtContext Name 
	  -> [LStmt Name] -> ReboundNames Name
	  -> TcRhoType		-- To keep it simple, we don't have an "expected" type here
	  -> TcM ([LStmt TcId], ReboundNames TcId)
tcDoStmts PArrComp stmts method_names res_ty
  = do 	{ [elt_ty] <- unifyTyConApp parrTyCon res_ty
	; stmts' <- tcComprehension PArrComp parrTyCon elt_ty stmts
	; return (stmts', [{- unused -}]) }

tcDoStmts ListComp stmts method_names res_ty
  = unifyListTy res_ty				`	thenM` \ elt_ty ->
    tcComprehension ListComp listTyCon elt_ty stmts	`thenM` \ stmts' ->
    returnM (stmts', [{- unused -}])

tcDoStmts do_or_mdo stmts method_names res_ty
  = newTyFlexiVarTy (mkArrowKind liftedTypeKind liftedTypeKind)	`thenM` \ m_ty ->
    newTyFlexiVarTy liftedTypeKind 				`thenM` \ elt_ty ->
    unifyTauTy res_ty (mkAppTy m_ty elt_ty)			`thenM_`
    let
	ctxt = SC { sc_what = do_or_mdo,
		    sc_rhs  = \ rhs -> do { (rhs', rhs_ty) <- tcInferRho rhs
					  ; rhs_elt_ty <- unifyAppTy m_ty rhs_ty
					  ; return (rhs', rhs_elt_ty) },
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

tcComprehension do_or_lc m_tycon elt_ty stmts
  = tcStmts ctxt stmts
  where
    ctxt = SC { sc_what = do_or_lc,
		sc_rhs  = \ rhs -> do { (rhs', rhs_ty) <- tcInferRho rhs
				      ; [rhs_elt_ty] <- unifyTyConApp m_tycon rhs_ty
				      ; return (rhs', rhs_elt_ty) },
		sc_body = \ body -> tcCheckRho body elt_ty,	-- Note: no m_tycon here!
		sc_ty   = mkTyConApp m_tycon [elt_ty] }
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
  = SC { sc_what :: HsStmtContext Name,				-- What kind of thing this is
    	 sc_rhs  :: LHsExpr Name -> TcM (LHsExpr TcId, TcType),	-- Type inference for RHS computations
	 sc_body :: LHsExpr Name -> TcM (LHsExpr TcId),		-- Type checker for return computation
	 sc_ty   :: TcType }					-- Return type; used *only* to check
								-- for escape in existential patterns
	-- We use type *inference* for the RHS computations, becuase of GADTs. 
	-- 	do { pat <- rhs; <rest> }
	-- is rather like
	--	case rhs of { pat -> <rest> }
	-- We do inference on rhs, so that information about its type can be refined
	-- when type-checking the pattern. 

tcStmtsAndThen
	:: (LStmt TcId -> thing -> thing)	-- Combiner
	-> TcStmtCtxt
        -> [LStmt Name]
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
tcStmtAndThen combine ctxt (L _ (LetStmt binds)) thing_inside
  = tcBindsAndThen		-- No error context, but a binding group is
  	(glue_binds combine)	-- rather a large thing for an error context anyway
  	binds
  	thing_inside

	-- BindStmt
tcStmtAndThen combine ctxt (L src_loc stmt@(BindStmt pat exp)) thing_inside
  = setSrcSpan src_loc					$
    addErrCtxt (stmtCtxt ctxt stmt)			$
    do	{ (exp', pat_ty)  <- sc_rhs ctxt exp
	; ([pat'], thing) <- tcMatchPats [pat] [Check pat_ty] (Check (sc_ty ctxt)) $
			     popErrCtxt thing_inside
	; return (combine (L src_loc (BindStmt pat' exp')) thing) }

	-- ExprStmt
tcStmtAndThen combine ctxt (L src_loc stmt@(ExprStmt exp _)) thing_inside
  = setSrcSpan src_loc		(
    	addErrCtxt (stmtCtxt ctxt stmt) $
	if isDoExpr (sc_what ctxt)
	then	-- do or mdo; the expression is a computation
		sc_rhs ctxt exp			`thenM` \ (exp', exp_ty) ->
		returnM (L src_loc (ExprStmt exp' exp_ty))
	else	-- List comprehensions, pattern guards; expression is a boolean
		tcCheckRho exp boolTy		`thenM` \ exp' ->
		returnM (L src_loc (ExprStmt exp' boolTy))
    )						`thenM` \ stmt' ->

    thing_inside 				`thenM` \ thing ->
    returnM (combine stmt' thing)


	-- ParStmt
tcStmtAndThen combine ctxt (L src_loc (ParStmt bndr_stmts_s)) thing_inside
  = loop bndr_stmts_s		`thenM` \ (pairs', thing) ->
    returnM (combine (L src_loc (ParStmt pairs')) thing)
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
tcStmtAndThen combine ctxt (L src_loc (RecStmt stmts laterNames recNames _)) thing_inside
  = newTyFlexiVarTys (length recNames) liftedTypeKind		`thenM` \ recTys ->
    let
	rec_ids = zipWith mkLocalId recNames recTys
    in
    tcExtendIdEnv rec_ids			$
    tcStmtsAndThen combine_rec ctxt stmts (
	zipWithM tc_ret recNames recTys		`thenM` \ rec_rets ->
	tcLookupLocalIds laterNames		`thenM` \ later_ids ->
	returnM ([], (later_ids, rec_rets))
    )						`thenM` \ (stmts', (later_ids, rec_rets)) ->

    tcExtendIdEnv later_ids		$
	-- NB:	The rec_ids for the recursive things 
	-- 	already scope over this part. This binding may shadow
	--	some of them with polymorphic things with the same Name
	--	(see note [RecStmt] in HsExpr)
    getLIE thing_inside				`thenM` \ (thing, lie) ->
    bindInstsOfLocalFuns lie later_ids		`thenM` \ lie_binds ->
  
    returnM (combine (L src_loc (RecStmt stmts' later_ids rec_ids rec_rets))     $
	     combine (L src_loc (LetStmt [HsBindGroup lie_binds  [] Recursive])) $
	     thing)
  where 
    combine_rec stmt (stmts, thing) = (stmt:stmts, thing)

    -- Unify the types of the "final" Ids with those of "knot-tied" Ids
    tc_ret rec_name mono_ty
	= tcLookupId rec_name				`thenM` \ poly_id ->
		-- poly_id may have a polymorphic type
		-- but mono_ty is just a monomorphic type variable
	  tcSubExp (Check mono_ty) (idType poly_id) 	`thenM` \ co_fn ->
	  returnM (L src_loc (co_fn <$> HsVar poly_id))

	-- Result statements
tcStmtAndThen combine ctxt (L src_loc stmt@(ResultStmt exp)) thing_inside
  = addErrCtxt (stmtCtxt ctxt stmt) (sc_body ctxt exp)	`thenM` \ exp' ->
    thing_inside 					`thenM` \ thing ->
    returnM (combine (L src_loc (ResultStmt exp')) thing)


------------------------------
glue_binds combine binds thing = combine (noLoc (LetStmt [binds])) thing
	-- ToDo: fix the noLoc
\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

@sameNoOfArgs@ takes a @[RenamedMatch]@ and decides whether the same
number of args are used in each equation.

\begin{code}
sameNoOfArgs :: MatchGroup Name -> Bool
sameNoOfArgs (MatchGroup matches _)
   = isSingleton (nub (map args_in_match matches))
  where
    args_in_match :: LMatch Name -> Int
    args_in_match (L _ (Match pats _ _)) = length pats
\end{code}

\begin{code}
varyingArgsErr name matches
  = sep [ptext SLIT("Varying number of arguments for function"), quotes (ppr name)]

matchCtxt ctxt  match  = hang (ptext SLIT("In") <+> pprMatchContext ctxt <> colon) 
			      4 (pprMatch ctxt match)

stmtCtxt ctxt stmt = hang (ptext SLIT("In") <+> pp_ctxt (sc_what ctxt) <> colon) 4 (ppr stmt)
	where
	  pp_ctxt  = case stmt of
			ResultStmt _ -> pprStmtResultContext
			other	     -> pprStmtContext
			
sigPatCtxt bound_ids bound_tvs tys tidy_env 
  = 	-- tys is (body_ty : pat_tys)  
    mapM zonkTcType tys		`thenM` \ tys' ->
    let
	(env1,  tidy_tys) = tidyOpenTypes tidy_env (map idType show_ids)
	(_env2, tidy_body_ty : tidy_pat_tys) = tidyOpenTypes env1 tys'
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
