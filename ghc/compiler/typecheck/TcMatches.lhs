%
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
\section[TcMatches]{Typecheck some @Matches@}

\begin{code}
module TcMatches ( tcMatchesFun, tcGRHSsPat, tcMatchesCase, tcMatchLambda,
		   matchCtxt,
		   tcDoStmts, tcStmts, tcMDoStmt, tcGuardStmt, tcThingWithSig,
		   tcMatchPats,
		   TcMatchCtxt(..)
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcSyntaxOp, tcCheckRho, tcInferRho, tcMonoExpr, tcCheckSigma )

import HsSyn		( HsExpr(..), LHsExpr, MatchGroup(..),
			  Match(..), LMatch, GRHSs(..), GRHS(..), 
			  Stmt(..), LStmt, HsMatchContext(..), HsStmtContext(..),
			  LPat, pprMatch, isIrrefutableHsPat,
			  pprMatchContext, pprStmtContext, pprMatchRhsContext,
			  collectPatsBinders, glueBindsOnGRHSs, noSyntaxExpr
			)
import TcHsSyn		( ExprCoFn, isIdCoercion, (<$>), (<.>) )

import TcRnMonad
import TcHsType		( tcHsPatSigType, UserTypeCtxt(..) )
import Inst		( tcInstCall, newMethodFromName )
import TcEnv		( TcId, tcLookupLocalIds, tcLookupId, tcExtendIdEnv, 
			  tcExtendTyVarEnv )
import TcPat		( PatCtxt(..), tcPats )
import TcMType		( newTyFlexiVarTy, newTyFlexiVarTys, zonkTcType ) 
import TcType		( TcType, TcTyVar, TcSigmaType, TcRhoType, mkFunTys,
			  tyVarsOfTypes, tidyOpenTypes, isSigmaTy, 
			  liftedTypeKind, openTypeKind, mkFunTy, mkAppTy )
import TcBinds		( tcBindsAndThen )
import TcUnify		( Expected(..), zapExpectedType, readExpectedType,
			  unifyTauTy, subFunTys, unifyTyConApp,
			  checkSigTyVarsWrt, zapExpectedBranches, tcSubExp, tcGen,
			  unifyAppTy, zapToListTy, zapToTyConApp )
import TcSimplify	( bindInstsOfLocalFuns )
import Name		( Name )
import TysWiredIn	( stringTy, boolTy, parrTyCon, listTyCon, mkListTy, mkPArrTy )
import PrelNames	( bindMName, returnMName, mfixName, thenMName, failMName )
import Id		( idType, mkLocalId )
import TyCon		( TyCon )
import CoreFVs		( idFreeTyVars )
import VarSet
import Util		( isSingleton )
import Outputable
import SrcLoc		( Located(..) )

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
    lift_grhs (GRHS stmts rhs) = GRHS stmts (fmap (co_fn <$>) rhs)

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
tcGRHSs ctxt (GRHSs [L loc1 (GRHS [] rhs)] binds) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    mc_body ctxt rhs exp_ty			`thenM` \ rhs' ->
    returnM (GRHSs [L loc1 (GRHS [] rhs')] [])

tcGRHSs ctxt (GRHSs grhss binds) exp_ty
  = tcBindsAndThen glueBindsOnGRHSs binds 	$
    do	{ exp_ty' <- zapExpectedType exp_ty openTypeKind
		-- Even if there is only one guard, we zap the RHS type to
		-- a monotype.  Reason: it makes tcStmts much easier,
		-- and even a one-armed guard has a notional second arm

	; let match_ctxt = mc_what ctxt
	      stmt_ctxt  = PatGuard match_ctxt
	      tc_grhs (GRHS guards rhs)
		= do  { (guards', rhs')
			    <- tcStmts stmt_ctxt (tcGuardStmt exp_ty') guards $
			       addErrCtxt (grhsCtxt match_ctxt rhs) $
			       tcCheckRho rhs exp_ty'
		      ; return (GRHS guards' rhs') }

	; grhss' <- mappM (wrapLocM tc_grhs) grhss
	; returnM (GRHSs grhss' []) }
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
	  -> [LStmt Name]
	  -> LHsExpr Name
	  -> Expected TcRhoType
	  -> TcM (HsExpr TcId)		-- Returns a HsDo
tcDoStmts ListComp stmts body res_ty
  = do	{ elt_ty <- zapToListTy res_ty
	; (stmts', body') <- tcStmts ListComp (tcLcStmt listTyCon elt_ty) stmts $
			     addErrCtxt (doBodyCtxt ListComp body) $
			     tcCheckRho body elt_ty
	; return (HsDo ListComp stmts' body' (mkListTy elt_ty)) }

tcDoStmts PArrComp stmts body res_ty
  = do 	{ [elt_ty] <- zapToTyConApp parrTyCon res_ty
	; (stmts', body') <- tcStmts PArrComp (tcLcStmt parrTyCon elt_ty) stmts $
			     addErrCtxt (doBodyCtxt PArrComp body) $
			     tcCheckRho body elt_ty
	; return (HsDo PArrComp stmts' body' (mkPArrTy elt_ty)) }

tcDoStmts DoExpr stmts body res_ty
  = do	{ res_ty'   <- zapExpectedType res_ty liftedTypeKind
	; (m_ty, _) <- unifyAppTy res_ty'
	; (stmts', body') <- tcStmts DoExpr (tcDoStmt m_ty res_ty') stmts $
			     addErrCtxt (doBodyCtxt DoExpr body) $
			     tcCheckRho body res_ty'
	; return (HsDo DoExpr stmts' body' res_ty') }

tcDoStmts cxt@(MDoExpr _) stmts body res_ty
  = do	{ res_ty'   <- zapExpectedType res_ty liftedTypeKind
	; (m_ty, _) <- unifyAppTy res_ty'
	; let tc_rhs rhs = do	{ (rhs', rhs_ty) <- tcInferRho rhs
				; (n_ty, pat_ty) <- unifyAppTy rhs_ty
				; unifyTauTy m_ty n_ty
				; return (rhs', pat_ty) }

	; (stmts', body') <- tcStmts cxt (tcMDoStmt res_ty' tc_rhs) stmts $
			     addErrCtxt (doBodyCtxt cxt body) $
			     tcCheckRho body res_ty'

	; let names = [mfixName, bindMName, thenMName, returnMName, failMName]
	; insts <- mapM (newMethodFromName DoOrigin m_ty) names
	; return (HsDo (MDoExpr (names `zip` insts)) stmts' body' res_ty') }

tcDoStmts ctxt stmts body res_ty = pprPanic "tcDoStmts" (pprStmtContext ctxt)
\end{code}


%************************************************************************
%*									*
\subsection{tcStmts}
%*									*
%************************************************************************

\begin{code}
type TcStmtChecker
  = forall thing.  HsStmtContext Name
           	   -> Stmt Name
	      	   -> TcM thing
              	   -> TcM (Stmt TcId, thing)

tcStmts :: HsStmtContext Name
	-> TcStmtChecker	-- NB: higher-rank type
        -> [LStmt Name]
	-> TcM thing
        -> TcM ([LStmt TcId], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmts ctxt stmt_chk [] thing_inside
  = do	{ thing <- thing_inside
	; return ([], thing) }

-- LetStmts are handled uniformly, regardless of context
tcStmts ctxt stmt_chk (L loc (LetStmt binds) : stmts) thing_inside
  = tcBindsAndThen	-- No error context, but a binding group is
  	glue_binds	-- rather a large thing for an error context anyway
  	binds
  	(tcStmts ctxt stmt_chk stmts thing_inside)
  where
    glue_binds binds (stmts, thing) = (L loc (LetStmt [binds]) : stmts, thing)


-- For the vanilla case, handle the location-setting part
tcStmts ctxt stmt_chk (L loc stmt : stmts) thing_inside
  = do 	{ (stmt', (stmts', thing)) <- 
		setSrcSpan loc		 	$
    		addErrCtxt (stmtCtxt ctxt stmt)	$
		stmt_chk ctxt stmt		$
		popErrCtxt 			$
		tcStmts ctxt stmt_chk stmts	$
		thing_inside
	; return (L loc stmt' : stmts', thing) }

--------------------------------
--	Pattern guards
tcGuardStmt :: TcType -> TcStmtChecker
tcGuardStmt res_ty ctxt (ExprStmt guard _ _) thing_inside
  = do	{ guard' <- tcCheckRho guard boolTy
	; thing  <- thing_inside
	; return (ExprStmt guard' noSyntaxExpr boolTy, thing) }

tcGuardStmt res_ty ctxt (BindStmt pat rhs _ _) thing_inside
  = do	{ (rhs', rhs_ty) <- tcInferRho rhs
	; (pat', thing)  <- tcBindPat pat rhs_ty res_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcGuardStmt res_ty ctxt stmt thing_inside
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


--------------------------------
--	List comprehensions and PArrays

tcLcStmt :: TyCon	-- The list/Parray type constructor ([] or PArray)
	 -> TcType	-- The element type of the list or PArray
	 -> TcStmtChecker

-- A generator, pat <- rhs
tcLcStmt m_tc elt_ty ctxt (BindStmt pat rhs _ _) thing_inside
  = do	{ (rhs', rhs_ty) <- tcInferRho rhs
	; [pat_ty]       <- unifyTyConApp m_tc rhs_ty
	; (pat', thing)  <- tcBindPat pat pat_ty elt_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

-- A boolean guard
tcLcStmt m_tc elt_ty ctxt (ExprStmt rhs _ _) thing_inside
  = do	{ rhs'  <- tcCheckRho rhs boolTy
	; thing <- thing_inside
	; return (ExprStmt rhs' noSyntaxExpr boolTy, thing) }

-- A parallel set of comprehensions
--	[ (g x, h x) | ... ; let g v = ...
--		     | ... ; let h v = ... ]
--
-- It's possible that g,h are overloaded, so we need to feed the LIE from the
-- (g x, h x) up through both lots of bindings (so we get the bindInstsOfLocalFuns).
-- Similarly if we had an existential pattern match:
--
--	data T = forall a. Show a => C a
--
--	[ (show x, show y) | ... ; C x <- ...
--			   | ... ; C y <- ... ]
--
-- Then we need the LIE from (show x, show y) to be simplified against
-- the bindings for x and y.  
-- 
-- It's difficult to do this in parallel, so we rely on the renamer to 
-- ensure that g,h and x,y don't duplicate, and simply grow the environment.
-- So the binders of the first parallel group will be in scope in the second
-- group.  But that's fine; there's no shadowing to worry about.

tcLcStmt m_tc elt_ty ctxt (ParStmt bndr_stmts_s) thing_inside
  = do	{ (pairs', thing) <- loop bndr_stmts_s
	; return (ParStmt pairs', thing) }
  where
    -- loop :: [([LStmt Name], [Name])] -> TcM ([([LStmt TcId], [TcId])], thing)
    loop [] = do { thing <- thing_inside
		 ; return ([], thing) }

    loop ((stmts, names) : pairs)
      = do { (stmts', (ids, pairs', thing))
		<- tcStmts ctxt (tcLcStmt m_tc elt_ty) stmts $
		   do { ids <- tcLookupLocalIds names
		      ; (pairs', thing) <- loop pairs
		      ; return (ids, pairs', thing) }
	   ; return ( (stmts', ids) : pairs', thing ) }

tcLcStmt m_tc elt_ty ctxt stmt thing_inside
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)

--------------------------------
--	Do-notation
-- The main excitement here is dealing with rebindable syntax

tcDoStmt :: TcType		-- Monad type,  m
	 -> TcType		-- Result type, m b
	 -> TcStmtChecker
	-- BindStmt
tcDoStmt m_ty res_ty ctxt (BindStmt pat rhs bind_op fail_op) thing_inside
  = do	{ 	-- Deal with rebindable syntax; (>>=) :: m a -> (a -> m b) -> m b
	; (rhs', rhs_ty) <- tcInferRho rhs
		-- We should use type *inference* for the RHS computations, becuase of GADTs. 
		-- 	do { pat <- rhs; <rest> }
		-- is rather like
		--	case rhs of { pat -> <rest> }
		-- We do inference on rhs, so that information about its type can be refined
		-- when type-checking the pattern. 

	; (n_ty, pat_ty) <- unifyAppTy rhs_ty
	; unifyTauTy m_ty n_ty
	; let bind_ty = mkFunTys [rhs_ty, mkFunTy pat_ty res_ty] res_ty

	; (pat', thing) <- tcBindPat pat pat_ty res_ty thing_inside

	-- Rebindable syntax stuff
	; bind_op' <- tcSyntaxOp DoOrigin bind_op bind_ty
		-- If (but only if) the pattern can fail, 
		-- typecheck the 'fail' operator
	; fail_op' <- if isIrrefutableHsPat pat' 
		      then return noSyntaxExpr
		      else tcSyntaxOp DoOrigin fail_op (mkFunTy stringTy res_ty)
	; return (BindStmt pat' rhs' bind_op' fail_op', thing) }


tcDoStmt m_ty res_ty ctxt (ExprStmt rhs then_op _) thing_inside
  = do	{ 	-- Deal with rebindable syntax; (>>) :: m a -> m b -> m b
	  a_ty <- newTyFlexiVarTy liftedTypeKind
	; let rhs_ty  = mkAppTy m_ty a_ty
	      then_ty = mkFunTys [rhs_ty, res_ty] res_ty
	; then_op' <- tcSyntaxOp DoOrigin then_op then_ty
	; rhs' <- tcCheckSigma rhs rhs_ty
	; thing <- thing_inside
	; return (ExprStmt rhs' then_op' rhs_ty, thing) }

tcDoStmt m_ty res_ty ctxt stmt thing_inside
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)

--------------------------------
--	Mdo-notation
-- The distinctive features here are
--	(a) RecStmts, and
--	(b) no rebindable syntax

tcMDoStmt :: TcType		-- Result type, m b
	  -> (LHsExpr Name -> TcM (LHsExpr TcId, TcType))	-- RHS inference
	  -> TcStmtChecker
tcMDoStmt res_ty tc_rhs ctxt (BindStmt pat rhs bind_op fail_op) thing_inside
  = do	{ (rhs', pat_ty) <- tc_rhs rhs
	; (pat', thing)  <- tcBindPat pat pat_ty res_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcMDoStmt res_ty tc_rhs ctxt (ExprStmt rhs then_op _) thing_inside
  = do	{ (rhs', elt_ty) <- tc_rhs rhs
	; thing 	 <- thing_inside
	; return (ExprStmt rhs' noSyntaxExpr elt_ty, thing) }

tcMDoStmt res_ty tc_rhs ctxt (RecStmt stmts laterNames recNames _ _) thing_inside
  = do	{ rec_tys <- newTyFlexiVarTys (length recNames) liftedTypeKind
	; let rec_ids = zipWith mkLocalId recNames rec_tys
	; tcExtendIdEnv rec_ids			$ do
    	{ (stmts', (later_ids, rec_rets))
		<- tcStmts ctxt (tcMDoStmt res_ty tc_rhs) stmts	$ 
			-- ToDo: res_ty not really right
		   do { rec_rets <- zipWithM tc_ret recNames rec_tys
		      ; later_ids <- tcLookupLocalIds laterNames
		      ; return (later_ids, rec_rets) }

	; (thing,lie) <- tcExtendIdEnv later_ids (getLIE thing_inside)
		-- NB:	The rec_ids for the recursive things 
		-- 	already scope over this part. This binding may shadow
		--	some of them with polymorphic things with the same Name
		--	(see note [RecStmt] in HsExpr)
	; lie_binds <- bindInstsOfLocalFuns lie later_ids
  
	; return (RecStmt stmts' later_ids rec_ids rec_rets lie_binds, thing)
	}}
  where 
    -- Unify the types of the "final" Ids with those of "knot-tied" Ids
    tc_ret rec_name mono_ty
	= tcLookupId rec_name				`thenM` \ poly_id ->
		-- poly_id may have a polymorphic type
		-- but mono_ty is just a monomorphic type variable
	  tcSubExp (Check mono_ty) (idType poly_id) 	`thenM` \ co_fn ->
	  returnM (co_fn <$> HsVar poly_id)

tcMDoStmt res_ty tc_rhs ctxt stmt thing_inside
  = pprPanic "tcMDoStmt: unexpected Stmt" (ppr stmt)

-----------------
tcBindPat :: LPat Name -> TcType 
	  -> TcType	-- Result type; used only to check existential escape
	  -> TcM a
	  -> TcM (LPat TcId, a)
tcBindPat pat pat_ty res_ty thing_inside
  = do	{ ([pat'],thing) <- tcMatchPats [pat] [Check pat_ty] 
				        (Check res_ty) thing_inside
	; return (pat', thing) }
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

matchCtxt ctxt match  = hang (ptext SLIT("In") <+> pprMatchContext ctxt <> colon) 
			   4 (pprMatch ctxt match)

grhsCtxt ctxt rhs = hang (ptext SLIT("In") <+> pprMatchRhsContext ctxt <> colon) 
		       4 (ppr rhs)

doBodyCtxt :: HsStmtContext Name -> LHsExpr Name -> SDoc
doBodyCtxt ctxt body = hang (ptext SLIT("In the result of") <+> pprStmtContext ctxt <> colon) 
		          4 (ppr body)

stmtCtxt ctxt stmt = hang (ptext SLIT("In") <+> pprStmtContext ctxt <> colon)
		  	4 (ppr stmt)
			
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
