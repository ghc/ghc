%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Typecheck arrow notation

\begin{code}
module TcArrows ( tcProc ) where

#include "HsVersions.h"

import {-# SOURCE #-}	TcExpr( tcMonoExpr, tcInferRho )

import HsSyn
import TcHsSyn

import TcMatches

import TcType
import TcMType
import TcBinds
import TcSimplify
import TcGadt
import TcPat
import TcUnify
import TcRnMonad
import Inst
import Name
import TysWiredIn
import VarSet 
import TysPrim
import Type

import SrcLoc
import Outputable
import Util
\end{code}

%************************************************************************
%*									*
		Proc	
%*									*
%************************************************************************

\begin{code}
tcProc :: InPat Name -> LHsCmdTop Name		-- proc pat -> expr
       -> BoxyRhoType				-- Expected type of whole proc expression
       -> TcM (OutPat TcId, LHsCmdTop TcId)

tcProc pat cmd exp_ty
  = newArrowScope $
    do	{ (exp_ty1, res_ty) <- boxySplitAppTy exp_ty 
	; (arr_ty, arg_ty)  <- boxySplitAppTy exp_ty1
	; let cmd_env = CmdEnv { cmd_arr = arr_ty }
	; (pat', cmd') <- tcLamPat pat arg_ty (emptyRefinement, res_ty) $
			  tcCmdTop cmd_env cmd []
	; return (pat', cmd') }
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
	 -> (Refinement, TcTauType)	-- Expected result type; always a monotype
					-- We know exactly how many cmd args are expected,
					-- albeit perhaps not their types; so we can pass 
					-- in a CmdStack
        -> TcM (LHsCmdTop TcId)

tcCmdTop env (L loc (HsCmdTop cmd _ _ names)) cmd_stk reft_res_ty@(_,res_ty)
  = setSrcSpan loc $
    do	{ cmd'   <- tcGuardedCmd env cmd cmd_stk reft_res_ty
	; names' <- mapM (tcSyntaxName ProcOrigin (cmd_arr env)) names
	; return (L loc $ HsCmdTop cmd' cmd_stk res_ty names') }


----------------------------------------
tcGuardedCmd :: CmdEnv -> LHsExpr Name -> CmdStack
	     -> (Refinement, TcTauType) -> TcM (LHsExpr TcId)
-- A wrapper that deals with the refinement (if any)
tcGuardedCmd env expr stk (reft, res_ty)
  = do	{ let (co, res_ty') = refineResType reft res_ty
    	; body <- tcCmd env expr (stk, res_ty')
	; return (mkLHsWrap co body) }

tcCmd :: CmdEnv -> LHsExpr Name -> (CmdStack, TcTauType) -> TcM (LHsExpr TcId)
	-- The main recursive function
tcCmd env (L loc expr) res_ty
  = setSrcSpan loc $ do
	{ expr' <- tc_cmd env expr res_ty
	; return (L loc expr') }

tc_cmd env (HsPar cmd) res_ty
  = do	{ cmd' <- tcCmd env cmd res_ty
	; return (HsPar cmd') }

tc_cmd env (HsLet binds (L body_loc body)) res_ty
  = do	{ (binds', body') <- tcLocalBinds binds		$
			     setSrcSpan body_loc 	$
			     tc_cmd env body res_ty
	; return (HsLet binds' (L body_loc body')) }

tc_cmd env in_cmd@(HsCase scrut matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd)		$
    addErrCtxt (caseScrutCtxt scrut)	(
      tcInferRho scrut 
    )							`thenM`	\ (scrut', scrut_ty) ->
    tcMatchesCase match_ctxt scrut_ty matches res_ty	`thenM`	\ matches' ->
    returnM (HsCase scrut' matches')
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = tcGuardedCmd env body stk res_ty'

tc_cmd env (HsIf pred b1 b2) res_ty
  = do 	{ pred' <- tcMonoExpr pred boolTy
	; b1'   <- tcCmd env b1 res_ty
	; b2'   <- tcCmd env b2 res_ty
	; return (HsIf pred' b1' b2')
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
-- gaw 2004 FIX?
    do  { arg_ty <- newFlexiTyVarTy openTypeKind

	; fun' <- tcCmd env fun (arg_ty:cmd_stk, res_ty)

	; arg' <- tcMonoExpr arg arg_ty

	; return (HsApp fun' arg') }

-------------------------------------------
-- 		Lambda

tc_cmd env cmd@(HsLam (MatchGroup [L mtch_loc (match@(Match pats maybe_rhs_sig grhss))] _))
       (cmd_stk, res_ty)
  = addErrCtxt (matchCtxt match_ctxt match)	$

    do	{ 	-- Check the cmd stack is big enough
	; checkTc (lengthAtLeast cmd_stk n_pats)
		  (kappaUnderflow cmd)

		-- Check the patterns, and the GRHSs inside
	; (pats', grhss') <- setSrcSpan mtch_loc		$
			     tcLamPats pats cmd_stk res_ty	$
			     tc_grhss grhss

	; let match' = L mtch_loc (Match pats' Nothing grhss')
	; return (HsLam (MatchGroup [match'] res_ty))
	}

  where
    n_pats     = length pats
    stk'       = drop n_pats cmd_stk
    match_ctxt = LambdaExpr 	-- Maybe KappaExpr?
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs grhss binds) res_ty
	= do { (binds', grhss') <- tcLocalBinds binds $
				   mapM (wrapLocM (tc_grhs res_ty)) grhss
	     ; return (GRHSs grhss' binds') }

    tc_grhs res_ty (GRHS guards body)
	= do { (guards', rhs') <- tcStmts pg_ctxt tcGuardStmt guards res_ty $
				  tcGuardedCmd env body stk'
	     ; return (GRHS guards' rhs') }

-------------------------------------------
-- 		Do notation

tc_cmd env cmd@(HsDo do_or_lc stmts body ty) (cmd_stk, res_ty)
  = do 	{ checkTc (null cmd_stk) (nonEmptyCmdStkErr cmd)
	; (stmts', body') <- tcStmts do_or_lc tc_stmt stmts (emptyRefinement, res_ty) $
			     tcGuardedCmd env body []
	; return (HsDo do_or_lc stmts' body' res_ty) }
  where
    tc_stmt = tcMDoStmt tc_rhs
    tc_rhs rhs = do { ty <- newFlexiTyVarTy liftedTypeKind
		    ; rhs' <- tcCmd env rhs ([], ty)
		    ; return (rhs', ty) }


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
	; span       <- getSrcSpanM
	; [w_tv]     <- tcInstSkolTyVars (ArrowSkol span) [alphaTyVar]
	; let w_ty = mkTyVarTy w_tv 	-- Just a convenient starting point

		--  a ((w,t1) .. tn) t
	; let e_res_ty = mkCmdArrTy env (foldl mkPairTy w_ty cmd_stk) res_ty

	 	--   b ((w,s1) .. sm) s
		--   -> a ((w,t1) .. tn) t
	; let e_ty = mkFunTys [mkAppTys b [tup,s] | (_,_,b,tup,s) <- cmds_w_tys] 
			      e_res_ty

		-- Check expr
	; (expr', lie) <- escapeArrowScope (getLIE (tcMonoExpr expr e_ty))
	; inst_binds <- tcSimplifyCheck sig_msg [w_tv] [] lie

		-- Check that the polymorphic variable hasn't been unified with anything
		-- and is not free in res_ty or the cmd_stk  (i.e.  t, t1..tn)
	; checkSigTyVarsWrt (tyVarsOfTypes (res_ty:cmd_stk)) [w_tv] 

		-- OK, now we are in a position to unscramble 
		-- the s1..sm and check each cmd
	; cmds' <- mapM (tc_cmd w_tv) cmds_w_tys

	; returnM (HsArrForm (noLoc $ HsWrap (WpTyLam w_tv) 
					       (unLoc $ mkHsDictLet inst_binds expr')) 
			     fixity cmds')
	}
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

	   ; tcCmdTop (env { cmd_arr = b }) cmd arg_tys (emptyRefinement, s) }

    unscramble :: TcType -> (TcType, [TcType])
    -- unscramble ((w,s1) .. sn)	=  (w, [s1..sn])
    unscramble ty
       = case tcSplitTyConApp_maybe ty of
	    Just (tc, [t,s]) | tc == pairTyCon 
	       ->  let 
		      (w,ss) = unscramble t  
		   in (w, s:ss)
				    
	    other -> (ty, [])

    sig_msg  = ptext SLIT("expected type of a command form")

-----------------------------------------------------------------
--		Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd env cmd _
  = failWithTc (vcat [ptext SLIT("The expression"), nest 2 (ppr cmd), 
		      ptext SLIT("was found where an arrow command was expected")])
\end{code}


%************************************************************************
%*									*
		Helpers
%*									*
%************************************************************************


\begin{code}
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
cmdCtxt cmd = ptext SLIT("In the command:") <+> ppr cmd

caseScrutCtxt cmd
  = hang (ptext SLIT("In the scrutinee of a case command:")) 4 (ppr cmd)

nonEmptyCmdStkErr cmd
  = hang (ptext SLIT("Non-empty command stack at command:"))
	 4 (ppr cmd)

kappaUnderflow cmd
  = hang (ptext SLIT("Command stack underflow at command:"))
	 4 (ppr cmd)

badFormFun i tup_ty'
 = hang (ptext SLIT("The type of the") <+> speakNth i <+> ptext SLIT("argument of a command form has the wrong shape"))
	4 (ptext SLIT("Argument type:") <+> ppr tup_ty')
\end{code}
