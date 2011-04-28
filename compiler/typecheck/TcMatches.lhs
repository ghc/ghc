%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

TcMatches: Typecheck some @Matches@

\begin{code}
module TcMatches ( tcMatchesFun, tcGRHSsPat, tcMatchesCase, tcMatchLambda,
		   TcMatchCtxt(..), 
		   tcStmts, tcDoStmts, tcBody,
		   tcDoStmt, tcMDoStmt, tcGuardStmt
       ) where

import {-# SOURCE #-}	TcExpr( tcSyntaxOp, tcInferRhoNC, tcCheckId,
                                tcMonoExpr, tcMonoExprNC, tcPolyExpr )

import HsSyn
import BasicTypes
import TcRnMonad
import TcEnv
import TcPat
import TcMType
import TcType
import TcBinds
import TcUnify
import Name
import TysWiredIn
import Id
import TyCon
import TysPrim
import Coercion		( mkSymCoI )
import Outputable
import Util
import SrcLoc
import FastString

-- Create chunkified tuple tybes for monad comprehensions
import MkCore

import Control.Monad

#include "HsVersions.h"
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

Note [Polymorphic expected type for tcMatchesFun]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tcMatchesFun may be given a *sigma* (polymorphic) type
so it must be prepared to use tcGen to skolemise it.
See Note [sig_tau may be polymorphic] in TcPat.

\begin{code}
tcMatchesFun :: Name -> Bool
	     -> MatchGroup Name
	     -> TcSigmaType			   -- Expected type of function
	     -> TcM (HsWrapper, MatchGroup TcId)   -- Returns type of body
tcMatchesFun fun_name inf matches exp_ty
  = do	{  -- Check that they all have the same no of arguments
	   -- Location is in the monad, set the caller so that 
	   -- any inter-equation error messages get some vaguely
	   -- sensible location.	Note: we have to do this odd
	   -- ann-grabbing, because we don't always have annotations in
	   -- hand when we call tcMatchesFun...
          traceTc "tcMatchesFun" (ppr fun_name $$ ppr exp_ty)
	; checkArgs fun_name matches

	; (wrap_gen, (wrap_fun, group)) 
            <- tcGen (FunSigCtxt fun_name) exp_ty $ \ _ exp_rho ->
	          -- Note [Polymorphic expected type for tcMatchesFun]
               matchFunTys herald arity exp_rho $ \ pat_tys rhs_ty -> 
	       tcMatches match_ctxt pat_tys rhs_ty matches 
        ; return (wrap_gen <.> wrap_fun, group) }
  where
    arity = matchGroupArity matches
    herald = ptext (sLit "The equation(s) for")
             <+> quotes (ppr fun_name) <+> ptext (sLit "have")
    match_ctxt = MC { mc_what = FunRhs fun_name inf, mc_body = tcBody }
\end{code}

@tcMatchesCase@ doesn't do the argument-count check because the
parser guarantees that each equation has exactly one argument.

\begin{code}
tcMatchesCase :: TcMatchCtxt		-- Case context
	      -> TcRhoType		-- Type of scrutinee
	      -> MatchGroup Name	-- The case alternatives
	      -> TcRhoType 		-- Type of whole case expressions
	      -> TcM (MatchGroup TcId)	-- Translated alternatives

tcMatchesCase ctxt scrut_ty matches res_ty
  | isEmptyMatchGroup matches   -- Allow empty case expressions
  = return (MatchGroup [] (mkFunTys [scrut_ty] res_ty)) 

  | otherwise
  = tcMatches ctxt [scrut_ty] res_ty matches

tcMatchLambda :: MatchGroup Name -> TcRhoType -> TcM (HsWrapper, MatchGroup TcId)
tcMatchLambda match res_ty 
  = matchFunTys herald n_pats res_ty  $ \ pat_tys rhs_ty ->
    tcMatches match_ctxt pat_tys rhs_ty match
  where
    n_pats = matchGroupArity match
    herald = sep [ ptext (sLit "The lambda expression")
	        	 <+> quotes (pprSetDepth (PartWay 1) $ 
                             pprMatches (LambdaExpr :: HsMatchContext Name) match),
			-- The pprSetDepth makes the abstraction print briefly
		ptext (sLit "has")]
    match_ctxt = MC { mc_what = LambdaExpr,
		      mc_body = tcBody }
\end{code}

@tcGRHSsPat@ typechecks @[GRHSs]@ that occur in a @PatMonoBind@.

\begin{code}
tcGRHSsPat :: GRHSs Name -> TcRhoType -> TcM (GRHSs TcId)
-- Used for pattern bindings
tcGRHSsPat grhss res_ty = tcGRHSs match_ctxt grhss res_ty
  where
    match_ctxt = MC { mc_what = PatBindRhs,
		      mc_body = tcBody }
\end{code}


\begin{code}
matchFunTys
  :: SDoc	-- See Note [Herald for matchExpecteFunTys] in TcUnify
  -> Arity
  -> TcRhoType
  -> ([TcSigmaType] -> TcRhoType -> TcM a)
  -> TcM (HsWrapper, a)

-- Written in CPS style for historical reasons; 
-- could probably be un-CPSd, like matchExpectedTyConApp

matchFunTys herald arity res_ty thing_inside
  = do	{ (coi, pat_tys, res_ty) <- matchExpectedFunTys herald arity res_ty
	; res <- thing_inside pat_tys res_ty
        ; return (coiToHsWrapper (mkSymCoI coi), res) }
\end{code}

%************************************************************************
%*									*
\subsection{tcMatch}
%*									*
%************************************************************************

\begin{code}
tcMatches :: TcMatchCtxt
	  -> [TcSigmaType] 	-- Expected pattern types
	  -> TcRhoType		-- Expected result-type of the Match.
	  -> MatchGroup Name
	  -> TcM (MatchGroup TcId)

data TcMatchCtxt 	-- c.f. TcStmtCtxt, also in this module
  = MC { mc_what :: HsMatchContext Name,	-- What kind of thing this is
    	 mc_body :: LHsExpr Name 		-- Type checker for a body of
                                                -- an alternative
		 -> TcRhoType
		 -> TcM (LHsExpr TcId) }	

tcMatches ctxt pat_tys rhs_ty (MatchGroup matches _)
  = ASSERT( not (null matches) )	-- Ensure that rhs_ty is filled in
    do	{ matches' <- mapM (tcMatch ctxt pat_tys rhs_ty) matches
	; return (MatchGroup matches' (mkFunTys pat_tys rhs_ty)) }

-------------
tcMatch :: TcMatchCtxt
	-> [TcSigmaType]	-- Expected pattern types
	-> TcRhoType	 	-- Expected result-type of the Match.
	-> LMatch Name
	-> TcM (LMatch TcId)

tcMatch ctxt pat_tys rhs_ty match 
  = wrapLocM (tc_match ctxt pat_tys rhs_ty) match
  where
    tc_match ctxt pat_tys rhs_ty match@(Match pats maybe_rhs_sig grhss)
      = add_match_ctxt match $
        do { (pats', grhss') <- tcPats (mc_what ctxt) pats pat_tys $
    			        tc_grhss ctxt maybe_rhs_sig grhss rhs_ty
	   ; return (Match pats' Nothing grhss') }

    tc_grhss ctxt Nothing grhss rhs_ty 
      = tcGRHSs ctxt grhss rhs_ty	-- No result signature

	-- Result type sigs are no longer supported
    tc_grhss _ (Just {}) _ _
      = panic "tc_ghrss"  	-- Rejected by renamer

	-- For (\x -> e), tcExpr has already said "In the expresssion \x->e"
	-- so we don't want to add "In the lambda abstraction \x->e"
    add_match_ctxt match thing_inside
	= case mc_what ctxt of
	    LambdaExpr -> thing_inside
	    m_ctxt     -> addErrCtxt (pprMatchInCtxt m_ctxt match) thing_inside

-------------
tcGRHSs :: TcMatchCtxt -> GRHSs Name -> TcRhoType
	-> TcM (GRHSs TcId)

-- Notice that we pass in the full res_ty, so that we get
-- good inference from simple things like
--	f = \(x::forall a.a->a) -> <stuff>
-- We used to force it to be a monotype when there was more than one guard
-- but we don't need to do that any more

tcGRHSs ctxt (GRHSs grhss binds) res_ty
  = do	{ (binds', grhss') <- tcLocalBinds binds $
			      mapM (wrapLocM (tcGRHS ctxt res_ty)) grhss

	; return (GRHSs grhss' binds') }

-------------
tcGRHS :: TcMatchCtxt -> TcRhoType -> GRHS Name -> TcM (GRHS TcId)

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
          -> SyntaxExpr Name            -- 'return' function for monad
                                        -- comprehensions
	  -> TcRhoType
	  -> TcM (HsExpr TcId)		-- Returns a HsDo
tcDoStmts ListComp stmts body _ res_ty
  = do	{ (coi, elt_ty) <- matchExpectedListTy res_ty
	; (stmts', body') <- tcStmts ListComp (tcLcStmt listTyCon) stmts 
				     elt_ty $
			     tcBody body
	; return $ mkHsWrapCoI coi 
                     (HsDo ListComp stmts' body' noSyntaxExpr (mkListTy elt_ty)) }

tcDoStmts PArrComp stmts body _ res_ty
  = do	{ (coi, elt_ty) <- matchExpectedPArrTy res_ty
	; (stmts', body') <- tcStmts PArrComp (tcLcStmt parrTyCon) stmts 
				     elt_ty $
			     tcBody body
	; return $ mkHsWrapCoI coi 
                     (HsDo PArrComp stmts' body' noSyntaxExpr (mkPArrTy elt_ty)) }

tcDoStmts DoExpr stmts body _ res_ty
  = do	{ (stmts', body') <- tcStmts DoExpr tcDoStmt stmts res_ty $
			     tcBody body
	; return (HsDo DoExpr stmts' body' noSyntaxExpr res_ty) }

tcDoStmts MDoExpr stmts body _ res_ty
  = do  { (stmts', body') <- tcStmts MDoExpr tcDoStmt stmts res_ty $
			     tcBody body
        ; return (HsDo MDoExpr stmts' body' noSyntaxExpr res_ty) }

tcDoStmts MonadComp stmts body return_op res_ty
  = do  { (stmts', (body', return_op')) <- tcStmts MonadComp tcMcStmt stmts res_ty $
                                           tcMcBody body return_op
        ; return $ HsDo MonadComp stmts' body' return_op' res_ty }

tcDoStmts ctxt _ _ _ _ = pprPanic "tcDoStmts" (pprStmtContext ctxt)

tcBody :: LHsExpr Name -> TcRhoType -> TcM (LHsExpr TcId)
tcBody body res_ty
  = do	{ traceTc "tcBody" (ppr res_ty)
	; body' <- tcMonoExpr body res_ty
	; return body' 
        } 
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
		-> TcRhoType			-- Result type for comprehension
	      	-> (TcRhoType -> TcM thing)	-- Checker for what follows the stmt
              	-> TcM (Stmt TcId, thing)

tcStmts :: HsStmtContext Name
	-> TcStmtChecker	-- NB: higher-rank type
        -> [LStmt Name]
	-> TcRhoType
	-> (TcRhoType -> TcM thing)
        -> TcM ([LStmt TcId], thing)

-- Note the higher-rank type.  stmt_chk is applied at different
-- types in the equations for tcStmts

tcStmts _ _ [] res_ty thing_inside
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
    		addErrCtxt (pprStmtInCtxt ctxt stmt)	$
		stmt_chk ctxt stmt res_ty		$ \ res_ty' ->
		popErrCtxt 				$
		tcStmts ctxt stmt_chk stmts res_ty'	$
		thing_inside
	; return (L loc stmt' : stmts', thing) }

--------------------------------
--	Pattern guards
tcGuardStmt :: TcStmtChecker
tcGuardStmt _ (ExprStmt guard _ _ _) res_ty thing_inside
  = do	{ guard' <- tcMonoExpr guard boolTy
	; thing  <- thing_inside res_ty
	; return (ExprStmt guard' noSyntaxExpr noSyntaxExpr boolTy, thing) }

tcGuardStmt ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do	{ (rhs', rhs_ty) <- tcInferRhoNC rhs	-- Stmt has a context already
	; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat rhs_ty $
                            thing_inside res_ty
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcGuardStmt _ stmt _ _
  = pprPanic "tcGuardStmt: unexpected Stmt" (ppr stmt)


--------------------------------
--	List comprehensions and PArrays

tcLcStmt :: TyCon	-- The list/Parray type constructor ([] or PArray)
	 -> TcStmtChecker

-- A generator, pat <- rhs
tcLcStmt m_tc ctxt (BindStmt pat rhs _ _) res_ty thing_inside
 = do	{ pat_ty <- newFlexiTyVarTy liftedTypeKind
        ; rhs'   <- tcMonoExpr rhs (mkTyConApp m_tc [pat_ty])
	; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat pat_ty $
                            thing_inside res_ty
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

-- A boolean guard
tcLcStmt _ _ (ExprStmt rhs _ _ _) res_ty thing_inside
  = do	{ rhs'  <- tcMonoExpr rhs boolTy
	; thing <- thing_inside res_ty
	; return (ExprStmt rhs' noSyntaxExpr noSyntaxExpr boolTy, thing) }

-- A parallel set of comprehensions
--	[ (g x, h x) | ... ; let g v = ...
--		     | ... ; let h v = ... ]
--
-- It's possible that g,h are overloaded, so we need to feed the LIE from the
-- (g x, h x) up through both lots of bindings (so we get the bindLocalMethods).
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

tcLcStmt m_tc ctxt (ParStmt bndr_stmts_s _ _ _) elt_ty thing_inside
  = do	{ (pairs', thing) <- loop bndr_stmts_s
	; return (ParStmt pairs' noSyntaxExpr noSyntaxExpr noSyntaxExpr, thing) }
  where
    -- loop :: [([LStmt Name], [Name])] -> TcM ([([LStmt TcId], [TcId])], thing)
    loop [] = do { thing <- thing_inside elt_ty
		 ; return ([], thing) }		-- matching in the branches

    loop ((stmts, names) : pairs)
      = do { (stmts', (ids, pairs', thing))
		<- tcStmts ctxt (tcLcStmt m_tc) stmts elt_ty $ \ _elt_ty' ->
		   do { ids <- tcLookupLocalIds names
		      ; (pairs', thing) <- loop pairs
		      ; return (ids, pairs', thing) }
	   ; return ( (stmts', ids) : pairs', thing ) }

tcLcStmt m_tc ctxt (TransformStmt stmts binders usingExpr maybeByExpr _ _) elt_ty thing_inside = do
    (stmts', (binders', usingExpr', maybeByExpr', thing)) <- 
        tcStmts (TransformStmtCtxt ctxt) (tcLcStmt m_tc) stmts elt_ty $ \elt_ty' -> do
            let alphaListTy = mkTyConApp m_tc [alphaTy]
                    
            (usingExpr', maybeByExpr') <- 
                case maybeByExpr of
                    Nothing -> do
                        -- We must validate that usingExpr :: forall a. [a] -> [a]
                        let using_ty = mkForAllTy alphaTyVar (alphaListTy `mkFunTy` alphaListTy)
                        usingExpr' <- tcPolyExpr usingExpr using_ty
                        return (usingExpr', Nothing)
                    Just byExpr -> do
                        -- We must infer a type such that e :: t and then check that 
			-- usingExpr :: forall a. (a -> t) -> [a] -> [a]
                        (byExpr', tTy) <- tcInferRhoNC byExpr
                        let using_ty = mkForAllTy alphaTyVar $ 
                                       (alphaTy `mkFunTy` tTy)
                                       `mkFunTy` alphaListTy `mkFunTy` alphaListTy
                        usingExpr' <- tcPolyExpr usingExpr using_ty
                        return (usingExpr', Just byExpr')
            
            binders' <- tcLookupLocalIds binders
            thing <- thing_inside elt_ty'
            
            return (binders', usingExpr', maybeByExpr', thing)

    return (TransformStmt stmts' binders' usingExpr' maybeByExpr' noSyntaxExpr noSyntaxExpr, thing)

tcLcStmt m_tc ctxt (GroupStmt stmts bindersMap by using _ _ _) elt_ty thing_inside
  = do { let (bndr_names, list_bndr_names) = unzip bindersMap

       ; (stmts', (bndr_ids, by', using_ty, elt_ty')) <-
            tcStmts (TransformStmtCtxt ctxt) (tcLcStmt m_tc) stmts elt_ty $ \elt_ty' -> do
	        (by', using_ty) <- 
                   case by of
                     Nothing   -> -- check that using :: forall a. [a] -> [[a]]
                                  return (Nothing, mkForAllTy alphaTyVar $
                                                   alphaListTy `mkFunTy` alphaListListTy)
		     			
		     Just by_e -> -- check that using :: forall a. (a -> t) -> [a] -> [[a]]
		     	          -- where by :: t
                                  do { (by_e', t_ty) <- tcInferRhoNC by_e
                                     ; return (Just by_e', mkForAllTy alphaTyVar $
                                                           (alphaTy `mkFunTy` t_ty) 
                                                           `mkFunTy` alphaListTy 
                                                           `mkFunTy` alphaListListTy) }
                -- Find the Ids (and hence types) of all old binders
                bndr_ids <- tcLookupLocalIds bndr_names
                
                return (bndr_ids, by', using_ty, elt_ty')
        
                -- Ensure that every old binder of type b is linked up with
		-- its new binder which should have type [b]
       ; let list_bndr_ids = zipWith mk_list_bndr list_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` list_bndr_ids
	     -- See Note [GroupStmt binder map] in HsExpr
            
       ; using' <- case using of
                     Left  e -> do { e' <- tcPolyExpr e         using_ty; return (Left  e') }
                     Right e -> do { e' <- tcPolyExpr (noLoc e) using_ty; return (Right (unLoc e')) }

             -- Type check the thing in the environment with 
	     -- these new binders and return the result
       ; thing <- tcExtendIdEnv list_bndr_ids (thing_inside elt_ty')
       ; return (GroupStmt stmts' bindersMap' by' using' noSyntaxExpr noSyntaxExpr noSyntaxExpr, thing) }
  where
    alphaListTy = mkTyConApp m_tc [alphaTy]
    alphaListListTy = mkTyConApp m_tc [alphaListTy]
            
    mk_list_bndr :: Name -> TcId -> TcId
    mk_list_bndr list_bndr_name bndr_id 
      = mkLocalId list_bndr_name (mkTyConApp m_tc [idType bndr_id])
    
tcLcStmt _ _ stmt _ _
  = pprPanic "tcLcStmt: unexpected Stmt" (ppr stmt)
        

--------------------------------
--	Monad comprehensions

tcMcStmt :: TcStmtChecker

-- Generators for monad comprehensions ( pat <- rhs )
--
--   [ body | q <- gen ]  ->  gen :: m a
--                            q   ::   a
--
tcMcStmt ctxt (BindStmt pat rhs bind_op fail_op) res_ty thing_inside
 = do   { rhs_ty     <- newFlexiTyVarTy liftedTypeKind
        ; pat_ty     <- newFlexiTyVarTy liftedTypeKind
        ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
        ; bind_op'   <- tcSyntaxOp MCompOrigin bind_op 
                             (mkFunTys [rhs_ty, mkFunTy pat_ty new_res_ty] res_ty)

                      -- If (but only if) the pattern can fail, 
                      -- typecheck the 'fail' operator
        ; fail_op' <- if isIrrefutableHsPat pat 
                      then return noSyntaxExpr
                      else tcSyntaxOp MCompOrigin fail_op (mkFunTy stringTy new_res_ty)

        ; rhs' <- tcMonoExprNC rhs rhs_ty
        ; (pat', thing) <- tcPat (StmtCtxt ctxt) pat pat_ty $
                           thing_inside new_res_ty

        ; return (BindStmt pat' rhs' bind_op' fail_op', thing) }

-- Boolean expressions.
--
--   [ body | stmts, expr ]  ->  expr :: m Bool
--
tcMcStmt _ (ExprStmt rhs then_op guard_op _) res_ty thing_inside
  = do	{ -- Deal with rebindable syntax:
          --    guard_op :: test_ty -> rhs_ty
          --    then_op  :: rhs_ty -> new_res_ty -> res_ty
          -- Where test_ty is, for example, Bool
          test_ty    <- newFlexiTyVarTy liftedTypeKind
        ; rhs_ty     <- newFlexiTyVarTy liftedTypeKind
        ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
        ; rhs'       <- tcMonoExpr rhs test_ty
        ; guard_op'  <- tcSyntaxOp MCompOrigin guard_op
                                   (mkFunTy test_ty rhs_ty)
        ; then_op'   <- tcSyntaxOp MCompOrigin then_op
		                   (mkFunTys [rhs_ty, new_res_ty] res_ty)
	; thing      <- thing_inside new_res_ty
	; return (ExprStmt rhs' then_op' guard_op' rhs_ty, thing) }

-- Transform statements.
--
--   [ body | stmts, then f ]       ->  f :: forall a. m a -> m a
--   [ body | stmts, then f by e ]  ->  f :: forall a. (a -> t) -> m a -> m a
--
tcMcStmt ctxt (TransformStmt stmts binders usingExpr maybeByExpr return_op bind_op) elt_ty thing_inside
  = do  {
        -- We don't know the types of binders yet, so we use this dummy and
        -- later unify this type with the `m_bndr_ty`
          ty_dummy <- newFlexiTyVarTy liftedTypeKind

        ; (stmts', (binders', usingExpr', maybeByExpr', return_op', bind_op', thing)) <- 
              tcStmts (TransformStmtCtxt ctxt) tcMcStmt stmts ty_dummy $ \elt_ty' -> do
                  { (_, (m_ty, _)) <- matchExpectedAppTy elt_ty'
                  ; (usingExpr', maybeByExpr') <- 
                        case maybeByExpr of
                            Nothing -> do
                                -- We must validate that usingExpr :: forall a. m a -> m a
                                let using_ty = mkForAllTy alphaTyVar $
                                               (m_ty `mkAppTy` alphaTy)
                                               `mkFunTy`
                                               (m_ty `mkAppTy` alphaTy)
                                usingExpr' <- tcPolyExpr usingExpr using_ty
                                return (usingExpr', Nothing)
                            Just byExpr -> do
                                -- We must infer a type such that e :: t and then check that 
                                -- usingExpr :: forall a. (a -> t) -> m a -> m a
                                (byExpr', tTy) <- tcInferRhoNC byExpr
                                let using_ty = mkForAllTy alphaTyVar $ 
                                               (alphaTy `mkFunTy` tTy)
                                               `mkFunTy`
                                               (m_ty `mkAppTy` alphaTy)
                                               `mkFunTy`
                                               (m_ty `mkAppTy` alphaTy)
                                usingExpr' <- tcPolyExpr usingExpr using_ty
                                return (usingExpr', Just byExpr')
                    
                  ; bndr_ids <- tcLookupLocalIds binders

                  -- `return` and `>>=` are used to pass around/modify our
                  -- binders, so we know their types:
                  --
                  --   return :: (a,b,c,..) -> m (a,b,c,..)
                  --   (>>=)  :: m (a,b,c,..)
                  --          -> ( (a,b,c,..) -> m (a,b,c,..) )
                  --          -> m (a,b,c,..)
                  --
                  ; let bndr_ty   = mkChunkified mkBoxedTupleTy $ map idType bndr_ids
                        m_bndr_ty = m_ty `mkAppTy` bndr_ty

                  ; return_op' <- tcSyntaxOp MCompOrigin return_op
                                      (bndr_ty `mkFunTy` m_bndr_ty)

                  ; bind_op'   <- tcSyntaxOp MCompOrigin bind_op $
                                      m_bndr_ty `mkFunTy` (bndr_ty `mkFunTy` elt_ty)
                                                `mkFunTy` elt_ty

                  -- Unify types of the inner comprehension and the binders type
                  ; _ <- unifyType elt_ty' m_bndr_ty

                  -- Typecheck the `thing` with out old type (which is the type
                  -- of the final result of our comprehension)
                  ; thing <- thing_inside elt_ty

                  ; return (bndr_ids, usingExpr', maybeByExpr', return_op', bind_op', thing) }

        ; return (TransformStmt stmts' binders' usingExpr' maybeByExpr' return_op' bind_op', thing) }

-- Grouping statements
--
--   [ body | stmts, then group by e ]
--     ->  e :: t
--   [ body | stmts, then group by e using f ]
--     ->  e :: t
--         f :: forall a. (a -> t) -> m a -> m (m a)
--   [ body | stmts, then group using f ]
--     ->  f :: forall a. m a -> m (m a)
--
tcMcStmt ctxt (GroupStmt stmts bindersMap by using return_op bind_op liftM_op) elt_ty thing_inside
  = do { let (bndr_names, m_bndr_names) = unzip bindersMap

       ; (_,(m_ty,_)) <- matchExpectedAppTy elt_ty
       ; let alphaMTy  = m_ty `mkAppTy` alphaTy
             alphaMMTy = m_ty `mkAppTy` alphaMTy

       -- We don't know the type of the bindings yet. It's not elt_ty!
       ; bndr_ty_dummy <- newFlexiTyVarTy liftedTypeKind

       ; (stmts', (bndr_ids, by', using_ty, return_op', bind_op')) <-
            tcStmts (TransformStmtCtxt ctxt) tcMcStmt stmts bndr_ty_dummy $ \elt_ty' -> do
	        { (by', using_ty) <- 
                     case by of
                       Nothing   -> -- check that using :: forall a. m a -> m (m a)
                                    return (Nothing, mkForAllTy alphaTyVar $
                                                     alphaMTy `mkFunTy` alphaMMTy)

		       Just by_e -> -- check that using :: forall a. (a -> t) -> m a -> m (m a)
		       	            -- where by :: t
                                    do { (by_e', t_ty) <- tcInferRhoNC by_e
                                       ; return (Just by_e', mkForAllTy alphaTyVar $
                                                             (alphaTy `mkFunTy` t_ty) 
                                                             `mkFunTy` alphaMTy 
                                                             `mkFunTy` alphaMMTy) }


                -- Find the Ids (and hence types) of all old binders
                ; bndr_ids <- tcLookupLocalIds bndr_names

                -- 'return' is only used for the binders, so we know its type.
                --
                --   return :: (a,b,c,..) -> m (a,b,c,..)
                --
                ; let bndr_ty   = mkChunkified mkBoxedTupleTy $ map idType bndr_ids
                      m_bndr_ty = m_ty `mkAppTy` bndr_ty
                ; return_op' <- tcSyntaxOp MCompOrigin return_op $ bndr_ty `mkFunTy` m_bndr_ty

                -- '>>=' is used to pass the grouped binders to the rest of the
                -- comprehension.
                --
                --   (>>=) :: m (m a, m b, m c, ..)
                --         -> ( (m a, m b, m c, ..) -> new_elt_ty )
                --         -> elt_ty
                --
                ; let bndr_m_ty   = mkChunkified mkBoxedTupleTy $ map (mkAppTy m_ty . idType) bndr_ids
                      m_bndr_m_ty = m_ty `mkAppTy` bndr_m_ty
                ; new_elt_ty <- newFlexiTyVarTy liftedTypeKind
                ; bind_op'   <- tcSyntaxOp MCompOrigin bind_op $
                                           m_bndr_m_ty `mkFunTy` (bndr_m_ty `mkFunTy` new_elt_ty)
                                                       `mkFunTy` elt_ty

                -- Finally make sure the type of the inner comprehension
                -- represents the types of our binders
                ; _ <- unifyType elt_ty' m_bndr_ty

                ; return (bndr_ids, by', using_ty, return_op', bind_op') }

       ; let mk_m_bndr :: Name -> TcId -> TcId
             mk_m_bndr m_bndr_name bndr_id =
                mkLocalId m_bndr_name (m_ty `mkAppTy` idType bndr_id)

             -- Ensure that every old binder of type `b` is linked up with its
             -- new binder which should have type `m b`
             m_bndr_ids = zipWith mk_m_bndr m_bndr_names bndr_ids
             bindersMap' = bndr_ids `zip` m_bndr_ids

	     -- See Note [GroupStmt binder map] in HsExpr

       ; using' <- case using of
                     Left  e -> do { e' <- tcPolyExpr e         using_ty; return (Left  e') }
                     Right e -> do { e' <- tcPolyExpr (noLoc e) using_ty; return (Right (unLoc e')) }

       -- Type check 'liftM' with 'forall a b. (a -> b) -> m_ty a -> m_ty b'
       ; liftM_op' <- fmap unLoc . tcPolyExpr (noLoc liftM_op) $
                         mkForAllTy alphaTyVar $ mkForAllTy betaTyVar $
                             (alphaTy `mkFunTy` betaTy)
                             `mkFunTy`
                             (m_ty `mkAppTy` alphaTy)
                             `mkFunTy`
                             (m_ty `mkAppTy` betaTy)

       -- Type check the thing in the environment with these new binders and
       -- return the result
       ; thing <- tcExtendIdEnv m_bndr_ids (thing_inside elt_ty)

       ; return (GroupStmt stmts' bindersMap' by' using' return_op' bind_op' liftM_op', thing) }

-- Typecheck `ParStmt`. See `tcLcStmt` for more informations about typechecking
-- of `ParStmt`s.
--
-- Note: The `mzip` function will get typechecked via:
--
--   ParStmt [st1::t1, st2::t2, st3::t3]
--   
--   mzip :: m st1
--        -> (m st2 -> m st3 -> m (st2, st3))   -- recursive call
--        -> m (st1, (st2, st3))
--
tcMcStmt ctxt (ParStmt bndr_stmts_s mzip_op bind_op return_op) elt_ty thing_inside
  = do	{ (_,(m_ty,_)) <- matchExpectedAppTy elt_ty
        ; (pairs', thing) <- loop m_ty bndr_stmts_s

        ; let mzip_ty  = mkForAllTys [alphaTyVar, betaTyVar] $
                         (m_ty `mkAppTy` alphaTy)
                         `mkFunTy`
                         (m_ty `mkAppTy` betaTy)
                         `mkFunTy`
                         (m_ty `mkAppTy` mkBoxedTupleTy [alphaTy, betaTy])
        ; mzip_op' <- unLoc `fmap` tcPolyExpr (noLoc mzip_op) mzip_ty

        -- Typecheck bind:
        ; let tys      = map (mkChunkified mkBoxedTupleTy . map idType . snd) pairs'
              tuple_ty = mk_tuple_ty tys

        ; bind_op' <- tcSyntaxOp MCompOrigin bind_op $
                         (m_ty `mkAppTy` tuple_ty)
                         `mkFunTy`
                         (tuple_ty `mkFunTy` elt_ty)
                         `mkFunTy`
                         elt_ty

        ; return_op' <- fmap unLoc . tcPolyExpr (noLoc return_op) $
                            mkForAllTy alphaTyVar $
                            alphaTy `mkFunTy` (m_ty `mkAppTy` alphaTy)
        ; return (ParStmt pairs' mzip_op' bind_op' return_op', thing) }

 where mk_tuple_ty tys = foldr (\tn tm -> mkBoxedTupleTy [tn, tm]) (last tys) (init tys)

       -- loop :: Type                                  -- m_ty
       --      -> [([LStmt Name], [Name])]
       --      -> TcM ([([LStmt TcId], [TcId])], thing)
       loop _ [] = do { thing <- thing_inside elt_ty
                      ; return ([], thing) }           -- matching in the branches

       loop m_ty ((stmts, names) : pairs)
         = do { -- type dummy since we don't know all binder types yet
                ty_dummy <- newFlexiTyVarTy liftedTypeKind
              ; (stmts', (ids, pairs', thing))
                   <- tcStmts ctxt tcMcStmt stmts ty_dummy $ \elt_ty' ->
                      do { ids <- tcLookupLocalIds names
                         ; _ <- unifyType elt_ty' (m_ty `mkAppTy` (mkChunkified mkBoxedTupleTy) (map idType ids))
                         ; (pairs', thing) <- loop m_ty pairs
                         ; return (ids, pairs', thing) }
              ; return ( (stmts', ids) : pairs', thing ) }

tcMcStmt _ stmt _ _
  = pprPanic "tcMcStmt: unexpected Stmt" (ppr stmt)

-- Typecheck 'body' with type 'a' instead of 'm a' like the rest of the
-- statements, ignore the second type argument coming from the tcStmts loop
tcMcBody :: LHsExpr Name
         -> SyntaxExpr Name
         -> TcRhoType
         -> TcM (LHsExpr TcId, SyntaxExpr TcId)
tcMcBody body return_op res_ty
  = do  { (_, (_, a_ty)) <- matchExpectedAppTy res_ty
        ; body'      <- tcMonoExpr body a_ty
        ; return_op' <- tcSyntaxOp MCompOrigin return_op
                                (a_ty `mkFunTy` res_ty)
        ; return (body', return_op')
        } 


--------------------------------
--	Do-notation
-- The main excitement here is dealing with rebindable syntax

tcDoStmt :: TcStmtChecker

tcDoStmt ctxt (BindStmt pat rhs bind_op fail_op) res_ty thing_inside
  = do	{ 	-- Deal with rebindable syntax:
		--	 (>>=) :: rhs_ty -> (pat_ty -> new_res_ty) -> res_ty
		-- This level of generality is needed for using do-notation
		-- in full generality; see Trac #1537

		-- I'd like to put this *after* the tcSyntaxOp 
                -- (see Note [Treat rebindable syntax first], but that breaks 
		-- the rigidity info for GADTs.  When we move to the new story
                -- for GADTs, we can move this after tcSyntaxOp
          rhs_ty     <- newFlexiTyVarTy liftedTypeKind
        ; pat_ty     <- newFlexiTyVarTy liftedTypeKind
        ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
	; bind_op'   <- tcSyntaxOp DoOrigin bind_op 
			     (mkFunTys [rhs_ty, mkFunTy pat_ty new_res_ty] res_ty)

		-- If (but only if) the pattern can fail, 
		-- typecheck the 'fail' operator
	; fail_op' <- if isIrrefutableHsPat pat 
		      then return noSyntaxExpr
		      else tcSyntaxOp DoOrigin fail_op (mkFunTy stringTy new_res_ty)

        ; rhs' <- tcMonoExprNC rhs rhs_ty
	; (pat', thing) <- tcPat (StmtCtxt ctxt) pat pat_ty $
                           thing_inside new_res_ty

	; return (BindStmt pat' rhs' bind_op' fail_op', thing) }


tcDoStmt _ (ExprStmt rhs then_op _ _) res_ty thing_inside
  = do	{   	-- Deal with rebindable syntax; 
                --   (>>) :: rhs_ty -> new_res_ty -> res_ty
		-- See also Note [Treat rebindable syntax first]
          rhs_ty     <- newFlexiTyVarTy liftedTypeKind
        ; new_res_ty <- newFlexiTyVarTy liftedTypeKind
	; then_op' <- tcSyntaxOp DoOrigin then_op 
			   (mkFunTys [rhs_ty, new_res_ty] res_ty)

        ; rhs' <- tcMonoExprNC rhs rhs_ty
	; thing <- thing_inside new_res_ty
	; return (ExprStmt rhs' then_op' noSyntaxExpr rhs_ty, thing) }

tcDoStmt ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = later_names
                       , recS_rec_ids = rec_names, recS_ret_fn = ret_op
                       , recS_mfix_fn = mfix_op, recS_bind_fn = bind_op }) 
         res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith mkLocalId tup_names tup_elt_tys
	      tup_ty  = mkBoxedTupleTy tup_elt_tys

        ; tcExtendIdEnv tup_ids $ do
        { stmts_ty <- newFlexiTyVarTy liftedTypeKind
        ; (stmts', (ret_op', tup_rets))
                <- tcStmts ctxt tcDoStmt stmts stmts_ty   $ \ inner_res_ty ->
                   do { tup_rets <- zipWithM tcCheckId tup_names tup_elt_tys
                             -- Unify the types of the "final" Ids (which may 
                             -- be polymorphic) with those of "knot-tied" Ids
		      ; ret_op' <- tcSyntaxOp DoOrigin ret_op (mkFunTy tup_ty inner_res_ty)
                      ; return (ret_op', tup_rets) }

	; mfix_res_ty <- newFlexiTyVarTy liftedTypeKind
        ; mfix_op' <- tcSyntaxOp DoOrigin mfix_op
                                 (mkFunTy (mkFunTy tup_ty stmts_ty) mfix_res_ty)

	; new_res_ty <- newFlexiTyVarTy liftedTypeKind
        ; bind_op' <- tcSyntaxOp DoOrigin bind_op 
			         (mkFunTys [mfix_res_ty, mkFunTy tup_ty new_res_ty] res_ty)

        ; thing <- thing_inside new_res_ty
--         ; lie_binds <- bindLocalMethods lie tup_ids
  
        ; let rec_ids = takeList rec_names tup_ids
	; later_ids <- tcLookupLocalIds later_names
	; traceTc "tcdo" $ vcat [ppr rec_ids <+> ppr (map idType rec_ids),
                                 ppr later_ids <+> ppr (map idType later_ids)]
        ; return (RecStmt { recS_stmts = stmts', recS_later_ids = later_ids
                          , recS_rec_ids = rec_ids, recS_ret_fn = ret_op' 
                          , recS_mfix_fn = mfix_op', recS_bind_fn = bind_op'
                          , recS_rec_rets = tup_rets }, thing)
        }}

tcDoStmt _ stmt _ _
  = pprPanic "tcDoStmt: unexpected Stmt" (ppr stmt)
\end{code}

Note [Treat rebindable syntax first]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When typechecking
	do { bar; ... } :: IO ()
we want to typecheck 'bar' in the knowledge that it should be an IO thing,
pushing info from the context into the RHS.  To do this, we check the
rebindable syntax first, and push that information into (tcMonoExprNC rhs).
Otherwise the error shows up when cheking the rebindable syntax, and
the expected/inferred stuff is back to front (see Trac #3613).

\begin{code}
--------------------------------
--	Mdo-notation
-- The distinctive features here are
--	(a) RecStmts, and
--	(b) no rebindable syntax

tcMDoStmt :: (LHsExpr Name -> TcM (LHsExpr TcId, TcType))	-- RHS inference
	  -> TcStmtChecker
tcMDoStmt tc_rhs ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do	{ (rhs', pat_ty) <- tc_rhs rhs
	; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat pat_ty $
                            thing_inside res_ty
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcMDoStmt tc_rhs _ (ExprStmt rhs _ _ _) res_ty thing_inside
  = do	{ (rhs', elt_ty) <- tc_rhs rhs
	; thing 	 <- thing_inside res_ty
	; return (ExprStmt rhs' noSyntaxExpr noSyntaxExpr elt_ty, thing) }

tcMDoStmt tc_rhs ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = laterNames
                               , recS_rec_ids = recNames }) res_ty thing_inside
  = do	{ rec_tys <- newFlexiTyVarTys (length recNames) liftedTypeKind
	; let rec_ids = zipWith mkLocalId recNames rec_tys
	; tcExtendIdEnv rec_ids			$ do
    	{ (stmts', (later_ids, rec_rets))
		<- tcStmts ctxt (tcMDoStmt tc_rhs) stmts res_ty	$ \ _res_ty' ->
			-- ToDo: res_ty not really right
		   do { rec_rets <- zipWithM tcCheckId recNames rec_tys
		      ; later_ids <- tcLookupLocalIds laterNames
		      ; return (later_ids, rec_rets) }

	; thing <- tcExtendIdEnv later_ids (thing_inside res_ty)
		-- NB:	The rec_ids for the recursive things 
		-- 	already scope over this part. This binding may shadow
		--	some of them with polymorphic things with the same Name
		--	(see note [RecStmt] in HsExpr)

        ; return (RecStmt stmts' later_ids rec_ids noSyntaxExpr noSyntaxExpr noSyntaxExpr rec_rets, thing)
	}}

tcMDoStmt _ _ stmt _ _
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
    = failWithTc (vcat [ptext (sLit "Equations for") <+> quotes (ppr fun) <+> 
			  ptext (sLit "have different numbers of arguments"),
			nest 2 (ppr (getLoc match1)),
			nest 2 (ppr (getLoc (head bad_matches)))])
  where
    n_args1 = args_in_match match1
    bad_matches = [m | m <- matches, args_in_match m /= n_args1]

    args_in_match :: LMatch Name -> Int
    args_in_match (L _ (Match pats _ _)) = length pats
checkArgs fun _ = pprPanic "TcPat.checkArgs" (ppr fun) -- Matches always non-empty
\end{code}

