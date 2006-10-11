%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcMatches: Typecheck some @Matches@

\begin{code}
module TcMatches ( tcMatchesFun, tcGRHSsPat, tcMatchesCase, tcMatchLambda,
		   matchCtxt, TcMatchCtxt(..), 
		   tcStmts, tcDoStmts, tcBody,
		   tcDoStmt, tcMDoStmt, tcGuardStmt
       ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcSyntaxOp, tcInferRho, tcMonoExpr, tcPolyExpr )

import HsSyn
import TcRnMonad
import TcGadt
import Inst
import TcEnv
import TcPat
import TcMType
import TcType
import TcBinds
import TcUnify
import TcSimplify
import Name
import TysWiredIn
import PrelNames
import Id
import TyCon
import Outputable
import SrcLoc
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
	     -> BoxyRhoType 		-- Expected type of function
	     -> TcM (HsWrapper, MatchGroup TcId)	-- Returns type of body

tcMatchesFun fun_name matches exp_ty
  = do	{  -- Check that they all have the same no of arguments
	   -- Location is in the monad, set the caller so that 
	   -- any inter-equation error messages get some vaguely
	   -- sensible location.	Note: we have to do this odd
	   -- ann-grabbing, because we don't always have annotations in
	   -- hand when we call tcMatchesFun...
	  checkArgs fun_name matches

	-- ToDo: Don't use "expected" stuff if there ain't a type signature
	-- because inconsistency between branches
	-- may show up as something wrong with the (non-existent) type signature

		-- This is one of two places places we call subFunTys
		-- The point is that if expected_y is a "hole", we want 
		-- to make pat_tys and rhs_ty as "holes" too.
	; subFunTys doc n_pats exp_ty     $ \ pat_tys rhs_ty -> 
	  tcMatches match_ctxt pat_tys rhs_ty matches
	}
  where
    doc = ptext SLIT("The equation(s) for") <+> quotes (ppr fun_name)
	  <+> ptext SLIT("have") <+> speakNOf n_pats (ptext SLIT("argument"))
    n_pats = matchGroupArity matches
    match_ctxt = MC { mc_what = FunRhs fun_name, mc_body = tcBody }
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: TcMatchCtxt		-- Case context
	      -> TcRhoType		-- Type of scrutinee
	      -> MatchGroup Name	-- The case alternatives
	      -> BoxyRhoType 		-- Type of whole case expressions
	      -> TcM (MatchGroup TcId)	-- Translated alternatives

tcMatchesCase ctxt scrut_ty matches res_ty
  = tcMatches ctxt [scrut_ty] res_ty matches

tcMatchLambda :: MatchGroup Name -> BoxyRhoType -> TcM (HsWrapper, MatchGroup TcId)
tcMatchLambda match res_ty 
  = subFunTys doc n_pats res_ty 	$ \ pat_tys rhs_ty ->
    tcMatches match_ctxt pat_tys rhs_ty match
  where
    n_pats = matchGroupArity match
    doc = sep [ ptext SLIT("The lambda expression")
		 <+> quotes (pprSetDepth 1 $ pprMatches LambdaExpr match),
			-- The pprSetDepth makes the abstraction print briefly
		ptext SLIT("has") <+> speakNOf n_pats (ptext SLIT("argument"))]
    match_ctxt = MC { mc_what = LambdaExpr,
		      mc_body = tcBody }
\end{code}

@tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.

\begin{code}
tcGRHSsPat :: GRHSs Name -> BoxyRhoType -> TcM (GRHSs TcId)
-- Used for pattern bindings
tcGRHSsPat grhss res_ty = tcGRHSs match_ctxt grhss (emptyRefinement, res_ty)
			-- emptyRefinement: no refinement in a pattern binding
  where
    match_ctxt = MC { mc_what = PatBindRhs,
		      mc_body = tcBody }
\end{code}


%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatches :: TcMatchCtxt
	  -> [BoxySigmaType] 		-- Expected pattern types
	  -> BoxyRhoType		-- Expected result-type of the Match.
	  -> MatchGroup Name
	  -> TcM (MatchGroup TcId)

data TcMatchCtxt 	-- c.f. TcStmtCtxt, also in this module
  = MC { mc_what :: HsMatchContext Name,	-- What kind of thing this is
    	 mc_body :: LHsExpr Name 		-- Type checker for a body of an alternative
		 -> (Refinement, BoxyRhoType) 
		 -> TcM (LHsExpr TcId) }	

tcMatches ctxt pat_tys rhs_ty (MatchGroup matches _)
  = do	{ matches' <- mapM (tcMatch ctxt pat_tys rhs_ty) matches
	; return (MatchGroup matches' (mkFunTys pat_tys rhs_ty)) }

-------------
tcMatch :: TcMatchCtxt
	-> [BoxySigmaType]	-- Expected pattern types
	-> BoxyRhoType	 	-- Expected result-type of the Match.
	-> LMatch Name
	-> TcM (LMatch TcId)

tcMatch ctxt pat_tys rhs_ty match 
  = wrapLocM (tc_match ctxt pat_tys rhs_ty) match
  where
    tc_match ctxt pat_tys rhs_ty match@(Match pats maybe_rhs_sig grhss)
      = addErrCtxt (matchCtxt (mc_what ctxt) match)	$	
        do { (pats', grhss') <- tcLamPats pats pat_tys rhs_ty $
    			        tc_grhss ctxt maybe_rhs_sig grhss
	   ; return (Match pats' Nothing grhss') }

    tc_grhss ctxt Nothing grhss rhs_ty 
      = tcGRHSs ctxt grhss rhs_ty	-- No result signature

	-- Result type sigs are no longer supported
    tc_grhss ctxt (Just res_sig) grhss (co, rhs_ty)
      = do { addErr (ptext SLIT("Ignoring (deprecated) result type signature")
			<+> ppr res_sig)
    	   ; tcGRHSs ctxt grhss (co, rhs_ty) }

-------------
tcGRHSs :: TcMatchCtxt -> GRHSs Name -> (Refinement, BoxyRhoType) 
	-> TcM (GRHSs TcId)

-- Notice that we pass in the full res_ty, so that we get
-- good inference from simple things like
--	f = \(x::forall a.a->a) -> <stuff>
-- We used to force it to be a monotype when there was more than one guard
-- but we don't need to do that any more

tcGRHSs ctxt (GRHSs grhss binds) res_ty
  = do	{ (binds', grhss') <- tcLocalBinds binds $
			      mappM (wrapLocM (tcGRHS ctxt res_ty)) grhss

	; returnM (GRHSs grhss' binds') }

-------------
tcGRHS :: TcMatchCtxt -> (Refinement, BoxyRhoType) -> GRHS Name -> TcM (GRHS TcId)

tcGRHS ctxt res_ty (GRHS guards rhs)
  = do  { (guards', rhs') <- tcStmts stmt_ctxt tcGuardStmt guards res_ty $
			     mc_body ctxt rhs
	; return (GRHS guards' rhs') }
  where
    stmt_ctxt  = PatGuard (mc_what ctxt)
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
	  -> BoxyRhoType
	  -> TcM (HsExpr TcId)		-- Returns a HsDo
tcDoStmts ListComp stmts body res_ty
  = do	{ elt_ty <- boxySplitListTy res_ty
	; (stmts', body') <- tcStmts ListComp (tcLcStmt listTyCon) stmts 
				     (emptyRefinement,elt_ty) $
			     tcBody body
	; return (HsDo ListComp stmts' body' (mkListTy elt_ty)) }

tcDoStmts PArrComp stmts body res_ty
  = do	{ [elt_ty] <- boxySplitTyConApp parrTyCon res_ty
	; (stmts', body') <- tcStmts PArrComp (tcLcStmt parrTyCon) stmts 
				     (emptyRefinement, elt_ty) $
			     tcBody body
	; return (HsDo PArrComp stmts' body' (mkPArrTy elt_ty)) }

tcDoStmts DoExpr stmts body res_ty
  = do	{ (m_ty, elt_ty) <- boxySplitAppTy res_ty
	; let res_ty' = mkAppTy m_ty elt_ty	-- The boxySplit consumes res_ty
	; (stmts', body') <- tcStmts DoExpr (tcDoStmt m_ty) stmts 
				     (emptyRefinement, res_ty') $
			     tcBody body
	; return (HsDo DoExpr stmts' body' res_ty') }

tcDoStmts ctxt@(MDoExpr _) stmts body res_ty
  = do	{ (m_ty, elt_ty) <- boxySplitAppTy res_ty
 	; let res_ty' = mkAppTy m_ty elt_ty	-- The boxySplit consumes res_ty
	      tc_rhs rhs = withBox liftedTypeKind $ \ pat_ty ->
			   tcMonoExpr rhs (mkAppTy m_ty pat_ty)

	; (stmts', body') <- tcStmts ctxt (tcMDoStmt tc_rhs) stmts 
				     (emptyRefinement, res_ty') $
			     tcBody body

	; let names = [mfixName, bindMName, thenMName, returnMName, failMName]
	; insts <- mapM (newMethodFromName DoOrigin m_ty) names
	; return (HsDo (MDoExpr (names `zip` insts)) stmts' body' res_ty') }

tcDoStmts ctxt stmts body res_ty = pprPanic "tcDoStmts" (pprStmtContext ctxt)

tcBody :: LHsExpr Name -> (Refinement, BoxyRhoType) -> TcM (LHsExpr TcId)
tcBody body (reft, res_ty)
  = do	{ traceTc (text "tcBody" <+> ppr res_ty <+> ppr reft)
	; let (co, res_ty') = refineResType reft res_ty
	; body' <- tcPolyExpr body res_ty'
	; return (mkLHsWrap co body') } 
\end{code}


%************************************************************************
%*									*
\subsection{tcStmts}
%*									*
%************************************************************************

\begin{code}
type TcStmtChecker
  =  forall thing. HsStmtContext Name
        	-> Stmt Name
		-> (Refinement, BoxyRhoType)			-- Result type for comprehension
	      	-> ((Refinement,BoxyRhoType) -> TcM thing)	-- Checker for what follows the stmt
              	-> TcM (Stmt TcId, thing)

  -- The incoming BoxyRhoType may be refined by type refinements
  -- before being passed to the thing_inside

tcStmts :: HsStmtContext Name
	-> TcStmtChecker	-- NB: higher-rank type
        -> [LStmt Name]
	-> (Refinement, BoxyRhoType)
	-> ((Refinement, BoxyRhoType) -> TcM thing)
        -> TcM ([LStmt TcId], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmts ctxt stmt_chk [] res_ty thing_inside
  = do	{ thing <- thing_inside res_ty
	; return ([], thing) }

-- LetStmts are handled uniformly, regardless of context
tcStmts ctxt stmt_chk (L loc (LetStmt binds) : stmts) res_ty thing_inside
  = do	{ (binds', (stmts',thing)) <- tcLocalBinds binds $
				      tcStmts ctxt stmt_chk stmts res_ty thing_inside
	; return (L loc (LetStmt binds') : stmts', thing) }

-- For the vanilla case, handle the location-setting part
tcStmts ctxt stmt_chk (L loc stmt : stmts) res_ty thing_inside
  = do 	{ (stmt', (stmts', thing)) <- 
		setSrcSpan loc		 		$
    		addErrCtxt (stmtCtxt ctxt stmt)		$
		stmt_chk ctxt stmt res_ty		$ \ res_ty' ->
		popErrCtxt 				$
		tcStmts ctxt stmt_chk stmts res_ty'	$
		thing_inside
	; return (L loc stmt' : stmts', thing) }

--------------------------------
--	Pattern guards
tcGuardStmt :: TcStmtChecker
tcGuardStmt ctxt (ExprStmt guard _ _) res_ty thing_inside
  = do	{ guard' <- tcMonoExpr guard boolTy
	; thing  <- thing_inside res_ty
	; return (ExprStmt guard' noSyntaxExpr boolTy, thing) }

tcGuardStmt ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do	{ (rhs', rhs_ty) <- tcInferRho rhs
	; (pat', thing)  <- tcLamPat pat rhs_ty res_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcGuardStmt ctxt stmt res_ty thing_inside
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


--------------------------------
--	List comprehensions and PArrays

tcLcStmt :: TyCon	-- The list/Parray type constructor ([] or PArray)
	 -> TcStmtChecker

-- A generator, pat <- rhs
tcLcStmt m_tc ctxt (BindStmt pat rhs _ _) res_ty thing_inside 
 = do	{ (rhs', pat_ty) <- withBox liftedTypeKind $ \ ty ->
			    tcMonoExpr rhs (mkTyConApp m_tc [ty])
	; (pat', thing)  <- tcLamPat pat pat_ty res_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

-- A boolean guard
tcLcStmt m_tc ctxt (ExprStmt rhs _ _) res_ty thing_inside
  = do	{ rhs'  <- tcMonoExpr rhs boolTy
	; thing <- thing_inside res_ty
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

tcLcStmt m_tc ctxt (ParStmt bndr_stmts_s) elt_ty thing_inside
  = do	{ (pairs', thing) <- loop bndr_stmts_s
	; return (ParStmt pairs', thing) }
  where
    -- loop :: [([LStmt Name], [Name])] -> TcM ([([LStmt TcId], [TcId])], thing)
    loop [] = do { thing <- thing_inside elt_ty	-- No refinement from pattern 
		 ; return ([], thing) }		-- matching in the branches

    loop ((stmts, names) : pairs)
      = do { (stmts', (ids, pairs', thing))
		<- tcStmts ctxt (tcLcStmt m_tc) stmts elt_ty $ \ elt_ty' ->
		   do { ids <- tcLookupLocalIds names
		      ; (pairs', thing) <- loop pairs
		      ; return (ids, pairs', thing) }
	   ; return ( (stmts', ids) : pairs', thing ) }

tcLcStmt m_tc ctxt stmt elt_ty thing_inside
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)

--------------------------------
--	Do-notation
-- The main excitement here is dealing with rebindable syntax

tcDoStmt :: TcType		-- Monad type,  m
	 -> TcStmtChecker

tcDoStmt m_ty ctxt (BindStmt pat rhs bind_op fail_op) reft_res_ty@(_,res_ty) thing_inside
  = do	{ (rhs', pat_ty) <- withBox liftedTypeKind $ \ pat_ty -> 
			    tcMonoExpr rhs (mkAppTy m_ty pat_ty)
		-- We should use type *inference* for the RHS computations, becuase of GADTs. 
		-- 	do { pat <- rhs; <rest> }
		-- is rather like
		--	case rhs of { pat -> <rest> }
		-- We do inference on rhs, so that information about its type can be refined
		-- when type-checking the pattern. 

	; (pat', thing) <- tcLamPat pat pat_ty reft_res_ty thing_inside

	-- Deal with rebindable syntax; (>>=) :: m a -> (a -> m b) -> m b
	; let bind_ty = mkFunTys [mkAppTy m_ty pat_ty, 
				  mkFunTy pat_ty res_ty] res_ty
	; bind_op' <- tcSyntaxOp DoOrigin bind_op bind_ty
		-- If (but only if) the pattern can fail, 
		-- typecheck the 'fail' operator
	; fail_op' <- if isIrrefutableHsPat pat' 
		      then return noSyntaxExpr
		      else tcSyntaxOp DoOrigin fail_op (mkFunTy stringTy res_ty)
	; return (BindStmt pat' rhs' bind_op' fail_op', thing) }


tcDoStmt m_ty ctxt (ExprStmt rhs then_op _) reft_res_ty@(_,res_ty) thing_inside
  = do	{ 	-- Deal with rebindable syntax; (>>) :: m a -> m b -> m b
	  a_ty <- newFlexiTyVarTy liftedTypeKind
	; let rhs_ty  = mkAppTy m_ty a_ty
	      then_ty = mkFunTys [rhs_ty, res_ty] res_ty
	; then_op' <- tcSyntaxOp DoOrigin then_op then_ty
	; rhs' <- tcPolyExpr rhs rhs_ty
	; thing <- thing_inside reft_res_ty
	; return (ExprStmt rhs' then_op' rhs_ty, thing) }

tcDoStmt m_ty ctxt stmt res_ty thing_inside
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)

--------------------------------
--	Mdo-notation
-- The distinctive features here are
--	(a) RecStmts, and
--	(b) no rebindable syntax

tcMDoStmt :: (LHsExpr Name -> TcM (LHsExpr TcId, TcType))	-- RHS inference
	  -> TcStmtChecker
tcMDoStmt tc_rhs ctxt (BindStmt pat rhs bind_op fail_op) res_ty thing_inside
  = do	{ (rhs', pat_ty) <- tc_rhs rhs
	; (pat', thing)  <- tcLamPat pat pat_ty res_ty thing_inside
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcMDoStmt tc_rhs ctxt (ExprStmt rhs then_op _) res_ty thing_inside
  = do	{ (rhs', elt_ty) <- tc_rhs rhs
	; thing 	 <- thing_inside res_ty
	; return (ExprStmt rhs' noSyntaxExpr elt_ty, thing) }

tcMDoStmt tc_rhs ctxt (RecStmt stmts laterNames recNames _ _) res_ty thing_inside
  = do	{ rec_tys <- newFlexiTyVarTys (length recNames) liftedTypeKind
	; let rec_ids = zipWith mkLocalId recNames rec_tys
	; tcExtendIdEnv rec_ids			$ do
    	{ (stmts', (later_ids, rec_rets))
		<- tcStmts ctxt (tcMDoStmt tc_rhs) stmts res_ty	$ \ res_ty' -> 
			-- ToDo: res_ty not really right
		   do { rec_rets <- zipWithM tc_ret recNames rec_tys
		      ; later_ids <- tcLookupLocalIds laterNames
		      ; return (later_ids, rec_rets) }

	; (thing,lie) <- tcExtendIdEnv later_ids (getLIE (thing_inside res_ty))
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
	= do { poly_id <- tcLookupId rec_name
		-- poly_id may have a polymorphic type
		-- but mono_ty is just a monomorphic type variable
	     ; co_fn <- tcSubExp (idType poly_id) mono_ty
	     ; return (mkHsWrap co_fn (HsVar poly_id)) }

tcMDoStmt tc_rhs ctxt stmt res_ty thing_inside
  = pprPanic "tcMDoStmt: unexpected Stmt" (ppr stmt)

\end{code}


%************************************************************************
%*									*
\subsection{Errors and contexts}
%*									*
%************************************************************************

@sameNoOfArgs@ takes a @[RenamedMatch]@ and decides whether the same
number of args are used in each equation.

\begin{code}
checkArgs :: Name -> MatchGroup Name -> TcM ()
checkArgs fun (MatchGroup (match1:matches) _)
    | null bad_matches = return ()
    | otherwise
    = failWithTc (vcat [ptext SLIT("Equations for") <+> quotes (ppr fun) <+> 
			  ptext SLIT("have different numbers of arguments"),
			nest 2 (ppr (getLoc match1)),
			nest 2 (ppr (getLoc (head bad_matches)))])
  where
    n_args1 = args_in_match match1
    bad_matches = [m | m <- matches, args_in_match m /= n_args1]

    args_in_match :: LMatch Name -> Int
    args_in_match (L _ (Match pats _ _)) = length pats
checkArgs fun other = panic "TcPat.checkArgs"	-- Matches always non-empty
\end{code}

\begin{code}
matchCtxt ctxt match  = hang (ptext SLIT("In") <+> pprMatchContext ctxt <> colon) 
			   4 (pprMatch ctxt match)

stmtCtxt ctxt stmt = hang (ptext SLIT("In") <+> pprStmtContext ctxt <> colon)
		  	4 (ppr stmt)
\end{code}
