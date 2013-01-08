%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Typecheck arrow notation

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module TcArrows ( tcProc ) where

import {-# SOURCE #-}	TcExpr( tcMonoExpr, tcInferRho, tcSyntaxOp, tcCheckId )

import HsSyn
import TcMatches
import TcHsSyn( hsLPatType )
import TcType
import TcMType
import TcBinds
import TcPat
import TcUnify
import TcRnMonad
import TcEnv
import TcEvidence
import Id( mkLocalId )
import Inst
import Name
import TysWiredIn
import VarSet 
import TysPrim

import SrcLoc
import Outputable
import FastString
import Util

import Control.Monad
\end{code}

%************************************************************************
%*									*
		Proc	
%*									*
%************************************************************************

\begin{code}
tcProc :: InPat Name -> LHsCmdTop Name		-- proc pat -> expr
       -> TcRhoType				-- Expected type of whole proc expression
       -> TcM (OutPat TcId, LHsCmdTop TcId, TcCoercion)

tcProc pat cmd exp_ty
  = newArrowScope $
    do	{ (co, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty 
	; (co1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
	; let cmd_env = CmdEnv { cmd_arr = arr_ty }
        ; (pat', cmd') <- tcPat ProcExpr pat arg_ty $
			  tcCmdTop cmd_env cmd [] res_ty
        ; let res_co = mkTcTransCo co (mkTcAppCo co1 (mkTcReflCo res_ty))
        ; return (pat', cmd', res_co) }
\end{code}


%************************************************************************
%*									*
		Commands
%*									*
%************************************************************************

\begin{code}
type CmdStack = [TcTauType]
data CmdEnv
  = CmdEnv {
	cmd_arr		:: TcType -- arrow type constructor, of kind *->*->*
    }

mkCmdArrTy :: CmdEnv -> TcTauType -> TcTauType -> TcTauType
mkCmdArrTy env t1 t2 = mkAppTys (cmd_arr env) [t1, t2]

---------------------------------------
tcCmdTop :: CmdEnv 
         -> LHsCmdTop Name
         -> CmdStack
	 -> TcTauType	-- Expected result type; always a monotype
                             -- We know exactly how many cmd args are expected,
			     -- albeit perhaps not their types; so we can pass 
			     -- in a CmdStack
        -> TcM (LHsCmdTop TcId)

tcCmdTop env (L loc (HsCmdTop cmd _ _ names)) cmd_stk res_ty
  = setSrcSpan loc $
    do	{ cmd'   <- tcCmd env cmd (cmd_stk, res_ty)
	; names' <- mapM (tcSyntaxName ProcOrigin (cmd_arr env)) names
	; return (L loc $ HsCmdTop cmd' cmd_stk res_ty names') }


----------------------------------------
tcCmd  :: CmdEnv -> LHsCmd Name -> (CmdStack, TcTauType) -> TcM (LHsCmd TcId)
	-- The main recursive function
tcCmd env (L loc cmd) res_ty
  = setSrcSpan loc $ do
	{ cmd' <- tc_cmd env cmd res_ty
	; return (L loc cmd') }

tc_cmd :: CmdEnv -> HsCmd Name  -> (CmdStack, TcTauType) -> TcM (HsCmd TcId)
tc_cmd env (HsCmdPar cmd) res_ty
  = do	{ cmd' <- tcCmd env cmd res_ty
	; return (HsCmdPar cmd') }

tc_cmd env (HsCmdLet binds (L body_loc body)) res_ty
  = do	{ (binds', body') <- tcLocalBinds binds		$
			     setSrcSpan body_loc 	$
			     tc_cmd env body res_ty
	; return (HsCmdLet binds' (L body_loc body')) }

tc_cmd env in_cmd@(HsCmdCase scrut matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut 
      matches' <- tcMatchesCase match_ctxt scrut_ty matches res_ty
      return (HsCmdCase scrut' matches')
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = tcCmd env body (stk, res_ty')

tc_cmd env (HsCmdIf Nothing pred b1 b2) res_ty    -- Ordinary 'if'
  = do  { pred' <- tcMonoExpr pred boolTy
        ; b1'   <- tcCmd env b1 res_ty
        ; b2'   <- tcCmd env b2 res_ty
        ; return (HsCmdIf Nothing pred' b1' b2')
    }

tc_cmd env (HsCmdIf (Just fun) pred b1 b2) res_ty -- Rebindable syntax for if
  = do 	{ pred_ty <- newFlexiTyVarTy openTypeKind
        -- For arrows, need ifThenElse :: forall r. T -> r -> r -> r
        -- because we're going to apply it to the environment, not
        -- the return value.
        ; (_, [r_tv]) <- tcInstSkolTyVars [alphaTyVar]
	; let r_ty = mkTyVarTy r_tv
        ; let if_ty = mkFunTys [pred_ty, r_ty, r_ty] r_ty
        ; checkTc (not (r_tv `elemVarSet` tyVarsOfType pred_ty))
                  (ptext (sLit "Predicate type of `ifThenElse' depends on result type"))
	; fun'  <- tcSyntaxOp IfOrigin fun if_ty
  	; pred' <- tcMonoExpr pred pred_ty
	; b1'   <- tcCmd env b1 res_ty
	; b2'   <- tcCmd env b2 res_ty
        ; return (HsCmdIf (Just fun') pred' b1' b2')
    }

-------------------------------------------
-- 		Arrow application
--     	    (f -< a)   or   (f -<< a)

tc_cmd env cmd@(HsCmdArrApp fun arg _ ho_app lr) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)	$
    do  { arg_ty <- newFlexiTyVarTy openTypeKind
	; let fun_ty = mkCmdArrTy env (foldl mkPairTy arg_ty cmd_stk) res_ty

	; fun' <- select_arrow_scope (tcMonoExpr fun fun_ty)
             -- ToDo: There should be no need for the escapeArrowScope stuff
             -- See Note [Escaping the arrow scope] in TcRnTypes

	; arg' <- tcMonoExpr arg arg_ty

	; return (HsCmdArrApp fun' arg' fun_ty ho_app lr) }
  where
       -- Before type-checking f, use the environment of the enclosing
       -- proc for the (-<) case.  
       -- Local bindings, inside the enclosing proc, are not in scope 
       -- inside f.  In the higher-order case (-<<), they are.
    select_arrow_scope tc = case ho_app of
        HsHigherOrderApp -> tc
        HsFirstOrderApp  -> escapeArrowScope tc

-------------------------------------------
-- 		Command application

tc_cmd env cmd@(HsCmdApp fun arg) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)	$
    do  { arg_ty <- newFlexiTyVarTy openTypeKind

	; fun' <- tcCmd env fun (arg_ty:cmd_stk, res_ty)

	; arg' <- tcMonoExpr arg arg_ty

	; return (HsCmdApp fun' arg') }

-------------------------------------------
-- 		Lambda

tc_cmd env cmd@(HsCmdLam (MG { mg_alts = [L mtch_loc (match@(Match pats _maybe_rhs_sig grhss))] }))
       (cmd_stk, res_ty)
  = addErrCtxt (pprMatchInCtxt match_ctxt match)	$

    do	{ 	-- Check the cmd stack is big enough
	; checkTc (lengthAtLeast cmd_stk n_pats)
		  (kappaUnderflow cmd)

		-- Check the patterns, and the GRHSs inside
	; (pats', grhss') <- setSrcSpan mtch_loc		$
                             tcPats LambdaExpr pats cmd_stk     $
                             tc_grhss grhss res_ty

	; let match' = L mtch_loc (Match pats' Nothing grhss')
              arg_tys = map hsLPatType pats'
	; return (HsCmdLam (MG { mg_alts = [match'], mg_arg_tys = arg_tys
                               , mg_res_ty = res_ty }))
              -- Or should we decompose res_ty?
	}

  where
    n_pats     = length pats
    stk'       = drop n_pats cmd_stk
    match_ctxt = (LambdaExpr :: HsMatchContext Name)	-- Maybe KappaExpr?
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs grhss binds) res_ty
	= do { (binds', grhss') <- tcLocalBinds binds $
				   mapM (wrapLocM (tc_grhs res_ty)) grhss
	     ; return (GRHSs grhss' binds') }

    tc_grhs res_ty (GRHS guards body)
	= do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
				  \ res_ty -> tcCmd env body (stk', res_ty)
	     ; return (GRHS guards' rhs') }

-------------------------------------------
-- 		Do notation

tc_cmd env cmd@(HsCmdDo stmts _) (cmd_stk, res_ty)
  = do 	{ checkTc (null cmd_stk) (nonEmptyCmdStkErr cmd)
	; stmts' <- tcStmts ArrowExpr (tcArrDoStmt env) stmts res_ty 
	; return (HsCmdDo stmts' res_ty) }
  where


-----------------------------------------------------------------
--	Arrow ``forms''	      (| e c1 .. cn |)
--
--	G      |-b  c : [s1 .. sm] s
--	pop(G) |-   e : forall w. b ((w,s1) .. sm) s
--			        -> a ((w,t1) .. tn) t
--	e \not\in (s, s1..sm, t, t1..tn)
--	----------------------------------------------
--	G |-a  (| e c |)  :  [t1 .. tn] t

tc_cmd env cmd@(HsCmdArrForm expr fixity cmd_args) (cmd_stk, res_ty)	
  = addErrCtxt (cmdCtxt cmd)	$
    do	{ cmds_w_tys <- zipWithM new_cmd_ty cmd_args [1..]
        ; (_, [w_tv])     <- tcInstSkolTyVars [alphaTyVar]
	; let w_ty = mkTyVarTy w_tv 	-- Just a convenient starting point

		--  a ((w,t1) .. tn) t
	; let e_res_ty = mkCmdArrTy env (foldl mkPairTy w_ty cmd_stk) res_ty

	 	--   b ((w,s1) .. sm) s
		--   -> a ((w,t1) .. tn) t
	; let e_ty = mkFunTys [mkAppTys b [tup,s] | (_,_,b,tup,s) <- cmds_w_tys] 
			      e_res_ty

  -- ToDo: SLPJ: something is badly wrong here.  
  -- The escapeArrowScope pops the Untouchables.. but that
  -- risks screwing up the skolem-escape check
  -- Moreover, arrowfail001 fails with an ASSERT failure
  -- because a variable gets the wrong level
		-- Check expr
        ; (inner_binds, expr')
               <- checkConstraints ArrowSkol [w_tv] [] $
                  escapeArrowScope (tcMonoExpr expr e_ty)

{-
        ; ((inner_binds, expr'), lie)
               <- captureConstraints $
                  checkConstraints ArrowSkol [w_tv] [] $
                  tcMonoExpr expr e_ty
                                 -- No need for escapeArrowScope in the 
                                 -- type checker.
                                 -- Note [Escaping the arrow scope] in TcRnTypes
        ; (lie, outer_binds) <- solveWantedsTcM lie
        ; emitConstraints lie
-}

		-- OK, now we are in a position to unscramble 
		-- the s1..sm and check each cmd
	; cmds' <- mapM (tc_cmd w_tv) cmds_w_tys

        ; let wrap = WpTyLam w_tv <.> mkWpLet inner_binds
	; return (HsCmdArrForm (mkLHsWrap wrap expr') fixity cmds') }
  where
 	-- Make the types	
	--	b, ((e,s1) .. sm), s
    new_cmd_ty :: LHsCmdTop Name -> Int
	       -> TcM (LHsCmdTop Name, Int, TcType, TcType, TcType)
    new_cmd_ty cmd i
	  = do	{ b_ty   <- newFlexiTyVarTy arrowTyConKind
		; tup_ty <- newFlexiTyVarTy liftedTypeKind
			-- We actually make a type variable for the tuple
			-- because we don't know how deeply nested it is yet    
		; s_ty   <- newFlexiTyVarTy liftedTypeKind
		; return (cmd, i, b_ty, tup_ty, s_ty)
		}

    tc_cmd w_tv (cmd, i, b, tup_ty, s)
      = do { tup_ty' <- zonkTcType tup_ty
	   ; let (corner_ty, arg_tys) = unscramble tup_ty'

		-- Check that it has the right shape:
		-- 	((w,s1) .. sn)
		-- where the si do not mention w
           ; _bogus <- unifyType corner_ty (mkTyVarTy w_tv)
	   ; checkTc (not (w_tv `elemVarSet` tyVarsOfTypes arg_tys))
		     (badFormFun i tup_ty')
     -- JPM: WARNING: this test is utterly bogus; see #5609
     -- We are not using the coercion returned by the unify;
     -- and (even more seriously) the w not in arg_tys test is totally
     -- bogus if there are suspended equality constraints. This code
     -- needs to be re-architected.

	   ; tcCmdTop (env { cmd_arr = b }) cmd arg_tys s }

    unscramble :: TcType -> (TcType, [TcType])
    -- unscramble ((w,s1) .. sn)	=  (w, [s1..sn])
    unscramble ty = unscramble' ty []

    unscramble' ty ss
       = case tcSplitTyConApp_maybe ty of
	    Just (tc, [t,s]) | tc == pairTyCon 
	       ->  unscramble' t (s:ss)
	    _ -> (ty, ss)

-----------------------------------------------------------------
--		Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (vcat [ptext (sLit "The expression"), nest 2 (ppr cmd), 
		      ptext (sLit "was found where an arrow command was expected")])
\end{code}


%************************************************************************
%*									*
		Stmts
%*									*
%************************************************************************

\begin{code}
--------------------------------
--	Mdo-notation
-- The distinctive features here are
--	(a) RecStmts, and
--	(b) no rebindable syntax

tcArrDoStmt :: CmdEnv -> TcCmdStmtChecker
tcArrDoStmt env _ (LastStmt rhs _) res_ty thing_inside
  = do	{ rhs' <- tcCmd env rhs ([], res_ty)
	; thing <- thing_inside (panic "tcArrDoStmt")
	; return (LastStmt rhs' noSyntaxExpr, thing) }

tcArrDoStmt env _ (BodyStmt rhs _ _ _) res_ty thing_inside
  = do	{ (rhs', elt_ty) <- tc_arr_rhs env rhs
	; thing 	 <- thing_inside res_ty
	; return (BodyStmt rhs' noSyntaxExpr noSyntaxExpr elt_ty, thing) }

tcArrDoStmt env ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do	{ (rhs', pat_ty) <- tc_arr_rhs env rhs
	; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat pat_ty $
                            thing_inside res_ty
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcArrDoStmt env ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = later_names
                            , recS_rec_ids = rec_names }) res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith mkLocalId tup_names tup_elt_tys
        ; tcExtendIdEnv tup_ids $ do
        { (stmts', tup_rets)
		<- tcStmtsAndThen ctxt (tcArrDoStmt env) stmts res_ty	$ \ _res_ty' ->
			-- ToDo: res_ty not really right
                   zipWithM tcCheckId tup_names tup_elt_tys

        ; thing <- thing_inside res_ty
		-- NB:	The rec_ids for the recursive things 
		-- 	already scope over this part. This binding may shadow
		--	some of them with polymorphic things with the same Name
		--	(see note [RecStmt] in HsExpr)

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names

        ; let rec_rets = takeList rec_names tup_rets
        ; let ret_table = zip tup_ids tup_rets
        ; let later_rets = [r | i <- later_ids, (j, r) <- ret_table, i == j]

        ; return (emptyRecStmt { recS_stmts = stmts', recS_later_ids = later_ids
                               , recS_later_rets = later_rets
                               , recS_rec_ids = rec_ids, recS_rec_rets = rec_rets
                               , recS_ret_ty = res_ty }, thing)
	}}

tcArrDoStmt _ _ stmt _ _
  = pprPanic "tcArrDoStmt: unexpected Stmt" (ppr stmt)

tc_arr_rhs :: CmdEnv -> LHsCmd Name -> TcM (LHsCmd TcId, TcType)
tc_arr_rhs env rhs = do { ty <- newFlexiTyVarTy liftedTypeKind
		        ; rhs' <- tcCmd env rhs ([], ty)
		        ; return (rhs', ty) }
\end{code}


%************************************************************************
%*									*
		Helpers
%*									*
%************************************************************************


\begin{code}
mkPairTy :: Type -> Type -> Type
mkPairTy t1 t2 = mkTyConApp pairTyCon [t1,t2]

arrowTyConKind :: Kind		--  *->*->*
arrowTyConKind = mkArrowKinds [liftedTypeKind, liftedTypeKind] liftedTypeKind
\end{code}


%************************************************************************
%*									*
		Errors
%*									*
%************************************************************************

\begin{code}
cmdCtxt :: HsCmd Name -> SDoc
cmdCtxt cmd = ptext (sLit "In the command:") <+> ppr cmd

nonEmptyCmdStkErr :: HsCmd Name -> SDoc
nonEmptyCmdStkErr cmd
  = hang (ptext (sLit "Non-empty command stack at command:"))
       2 (ppr cmd)

kappaUnderflow :: HsCmd Name -> SDoc
kappaUnderflow cmd
  = hang (ptext (sLit "Command stack underflow at command:"))
       2 (ppr cmd)

badFormFun :: Int -> TcType -> SDoc
badFormFun i tup_ty'
 = hang (ptext (sLit "The type of the") <+> speakNth i <+> ptext (sLit "argument of a command form has the wrong shape"))
      2 (ptext (sLit "Argument type:") <+> ppr tup_ty')
\end{code}
