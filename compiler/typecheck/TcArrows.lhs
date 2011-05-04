%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Typecheck arrow notation

\begin{code}
module TcArrows ( tcProc ) where

import {-# SOURCE #-}	TcExpr( tcMonoExpr, tcInferRho, tcSyntaxOp, tcCheckId )

import HsSyn
import TcMatches
import TcType
import TcMType
import TcBinds
import TcPat
import TcUnify
import TcRnMonad
import TcEnv
import Coercion
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
       -> TcM (OutPat TcId, LHsCmdTop TcId, CoercionI)

tcProc pat cmd exp_ty
  = newArrowScope $
    do	{ (coi, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty 
	; (coi1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
	; let cmd_env = CmdEnv { cmd_arr = arr_ty }
	; (pat', cmd') <- tcPat ProcExpr pat arg_ty $
			  tcCmdTop cmd_env cmd [] res_ty
        ; let res_coi = mkTransCoI coi (mkAppTyCoI coi1 (IdCo res_ty))
	; return (pat', cmd', res_coi) }
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
tcCmd :: CmdEnv -> LHsExpr Name -> (CmdStack, TcTauType) -> TcM (LHsExpr TcId)
	-- The main recursive function
tcCmd env (L loc expr) res_ty
  = setSrcSpan loc $ do
	{ expr' <- tc_cmd env expr res_ty
	; return (L loc expr') }

tc_cmd :: CmdEnv -> HsExpr Name -> (CmdStack, TcTauType) -> TcM (HsExpr TcId)
tc_cmd env (HsPar cmd) res_ty
  = do	{ cmd' <- tcCmd env cmd res_ty
	; return (HsPar cmd') }

tc_cmd env (HsLet binds (L body_loc body)) res_ty
  = do	{ (binds', body') <- tcLocalBinds binds		$
			     setSrcSpan body_loc 	$
			     tc_cmd env body res_ty
	; return (HsLet binds' (L body_loc body')) }

tc_cmd env in_cmd@(HsCase scrut matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut 
      matches' <- tcMatchesCase match_ctxt scrut_ty matches res_ty
      return (HsCase scrut' matches')
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = tcCmd env body (stk, res_ty')

tc_cmd env (HsIf mb_fun pred b1 b2) (stack_ty,res_ty)
  = do 	{ pred_ty <- newFlexiTyVarTy openTypeKind
	; b_ty <- newFlexiTyVarTy openTypeKind
        ; let if_ty = mkFunTys [pred_ty, b_ty, b_ty] res_ty
	; mb_fun' <- case mb_fun of 
              Nothing  -> return Nothing
              Just fun -> liftM Just (tcSyntaxOp IfOrigin fun if_ty)
  	; pred' <- tcMonoExpr pred pred_ty
	; b1'   <- tcCmd env b1 (stack_ty,b_ty)
	; b2'   <- tcCmd env b2 (stack_ty,b_ty)
	; return (HsIf mb_fun' pred' b1' b2')
    }

-------------------------------------------
-- 		Arrow application
--     	    (f -< a)   or   (f -<< a)

tc_cmd env cmd@(HsArrApp fun arg _ ho_app lr) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)	$
    do  { arg_ty <- newFlexiTyVarTy openTypeKind
	; let fun_ty = mkCmdArrTy env (foldl mkPairTy arg_ty cmd_stk) res_ty

	; fun' <- select_arrow_scope (tcMonoExpr fun fun_ty)

	; arg' <- tcMonoExpr arg arg_ty

	; return (HsArrApp fun' arg' fun_ty ho_app lr) }
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

tc_cmd env cmd@(HsApp fun arg) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)	$
    do  { arg_ty <- newFlexiTyVarTy openTypeKind

	; fun' <- tcCmd env fun (arg_ty:cmd_stk, res_ty)

	; arg' <- tcMonoExpr arg arg_ty

	; return (HsApp fun' arg') }

-------------------------------------------
-- 		Lambda

tc_cmd env cmd@(HsLam (MatchGroup [L mtch_loc (match@(Match pats _maybe_rhs_sig grhss))] _))
       (cmd_stk, res_ty)
  = addErrCtxt (pprMatchInCtxt match_ctxt match)	$

    do	{ 	-- Check the cmd stack is big enough
	; checkTc (lengthAtLeast cmd_stk n_pats)
		  (kappaUnderflow cmd)

		-- Check the patterns, and the GRHSs inside
	; (pats', grhss') <- setSrcSpan mtch_loc		$
			     tcPats LambdaExpr pats cmd_stk	$
			     tc_grhss grhss res_ty

	; let match' = L mtch_loc (Match pats' Nothing grhss')
	; return (HsLam (MatchGroup [match'] res_ty))
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

tc_cmd env cmd@(HsDo do_or_lc stmts _) (cmd_stk, res_ty)
  = do 	{ checkTc (null cmd_stk) (nonEmptyCmdStkErr cmd)
	; stmts' <- tcStmts do_or_lc (tcArrDoStmt env) stmts res_ty 
	; return (HsDo do_or_lc stmts' res_ty) }
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

tc_cmd env cmd@(HsArrForm expr fixity cmd_args) (cmd_stk, res_ty)	
  = addErrCtxt (cmdCtxt cmd)	$
    do	{ cmds_w_tys <- zipWithM new_cmd_ty cmd_args [1..]
        ; [w_tv]     <- tcInstSkolTyVars [alphaTyVar]
	; let w_ty = mkTyVarTy w_tv 	-- Just a convenient starting point

		--  a ((w,t1) .. tn) t
	; let e_res_ty = mkCmdArrTy env (foldl mkPairTy w_ty cmd_stk) res_ty

	 	--   b ((w,s1) .. sm) s
		--   -> a ((w,t1) .. tn) t
	; let e_ty = mkFunTys [mkAppTys b [tup,s] | (_,_,b,tup,s) <- cmds_w_tys] 
			      e_res_ty

		-- Check expr
	; (inst_binds, expr') <- checkConstraints ArrowSkol [w_tv] [] $
                                 escapeArrowScope (tcMonoExpr expr e_ty)

		-- OK, now we are in a position to unscramble 
		-- the s1..sm and check each cmd
	; cmds' <- mapM (tc_cmd w_tv) cmds_w_tys

        ; let wrap = WpTyLam w_tv <.> mkWpLet inst_binds
	; return (HsArrForm (mkLHsWrap wrap expr') fixity cmds') }
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
	   ; checkTc (corner_ty `tcEqType` mkTyVarTy w_tv && 
		      not (w_tv `elemVarSet` tyVarsOfTypes arg_tys))
		     (badFormFun i tup_ty')

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

tcArrDoStmt :: CmdEnv -> TcStmtChecker
tcArrDoStmt env _ (LastStmt rhs _) res_ty thing_inside
  = do	{ rhs' <- tcCmd env rhs ([], res_ty)
	; thing <- thing_inside (panic "tcArrDoStmt")
	; return (LastStmt rhs' noSyntaxExpr, thing) }

tcArrDoStmt env _ (ExprStmt rhs _ _ _) res_ty thing_inside
  = do	{ (rhs', elt_ty) <- tc_arr_rhs env rhs
	; thing 	 <- thing_inside res_ty
	; return (ExprStmt rhs' noSyntaxExpr noSyntaxExpr elt_ty, thing) }

tcArrDoStmt env ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do	{ (rhs', pat_ty) <- tc_arr_rhs env rhs
	; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat pat_ty $
                            thing_inside res_ty
	; return (BindStmt pat' rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcArrDoStmt env ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = laterNames
                            , recS_rec_ids = recNames }) res_ty thing_inside
  = do	{ rec_tys <- newFlexiTyVarTys (length recNames) liftedTypeKind
	; let rec_ids = zipWith mkLocalId recNames rec_tys
	; tcExtendIdEnv rec_ids $ do
    	{ (stmts', (later_ids, rec_rets))
		<- tcStmtsAndThen ctxt (tcArrDoStmt env) stmts res_ty	$ \ _res_ty' ->
			-- ToDo: res_ty not really right
		   do { rec_rets <- zipWithM tcCheckId recNames rec_tys
		      ; later_ids <- tcLookupLocalIds laterNames
		      ; return (later_ids, rec_rets) }

	; thing <- tcExtendIdEnv later_ids (thing_inside res_ty)
		-- NB:	The rec_ids for the recursive things 
		-- 	already scope over this part. This binding may shadow
		--	some of them with polymorphic things with the same Name
		--	(see note [RecStmt] in HsExpr)

        ; return (emptyRecStmt { recS_stmts = stmts', recS_later_ids = later_ids
                               , recS_rec_ids = rec_ids, recS_rec_rets = rec_rets
                               , recS_ret_ty = res_ty }, thing)
	}}

tcArrDoStmt _ _ stmt _ _
  = pprPanic "tcArrDoStmt: unexpected Stmt" (ppr stmt)

tc_arr_rhs :: CmdEnv -> LHsExpr Name -> TcM (LHsExpr TcId, TcType)
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
cmdCtxt :: HsExpr Name -> SDoc
cmdCtxt cmd = ptext (sLit "In the command:") <+> ppr cmd

nonEmptyCmdStkErr :: HsExpr Name -> SDoc
nonEmptyCmdStkErr cmd
  = hang (ptext (sLit "Non-empty command stack at command:"))
       2 (ppr cmd)

kappaUnderflow :: HsExpr Name -> SDoc
kappaUnderflow cmd
  = hang (ptext (sLit "Command stack underflow at command:"))
       2 (ppr cmd)

badFormFun :: Int -> TcType -> SDoc
badFormFun i tup_ty'
 = hang (ptext (sLit "The type of the") <+> speakNth i <+> ptext (sLit "argument of a command form has the wrong shape"))
      2 (ptext (sLit "Argument type:") <+> ppr tup_ty')
\end{code}
