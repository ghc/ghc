%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%
Typecheck arrow notation

\begin{code}
{-# LANGUAGE RankNTypes #-}

module TcArrows ( tcProc ) where

import {-# SOURCE #-}   TcExpr( tcMonoExpr, tcInferRho, tcSyntaxOp, tcCheckId, tcPolyExpr )

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
import Coercion ( Role(..) )
import TysWiredIn
import VarSet 
import TysPrim
import BasicTypes( Arity )
import SrcLoc
import Outputable
import FastString
import Util

import Control.Monad
\end{code}

Note [Arrow overivew]
~~~~~~~~~~~~~~~~~~~~~
Here's a summary of arrows and how they typecheck.  First, here's
a cut-down syntax:

  expr ::= ....  
        |  proc pat cmd

  cmd ::= cmd exp                    -- Arrow application
       |  \pat -> cmd                -- Arrow abstraction
       |  (| exp cmd1 ... cmdn |)    -- Arrow form, n>=0
       |  ... -- If, case in the usual way

  cmd_type ::= carg_type --> type

  carg_type ::= ()
             |  (type, carg_type)

Note that
 * The 'exp' in an arrow form can mention only 
   "arrow-local" variables

 * An "arrow-local" variable is bound by an enclosing
   cmd binding form (eg arrow abstraction)

 * A cmd_type is here written with a funny arrow "-->",
   The bit on the left is a carg_type (command argument type)
   which itself is a nested tuple, finishing with ()

 * The arrow-tail operator (e1 -< e2) means
       (| e1 <<< arr snd |) e2


%************************************************************************
%*                                                                      *
                Proc    
%*                                                                      *
%************************************************************************

\begin{code}
tcProc :: InPat Name -> LHsCmdTop Name          -- proc pat -> expr
       -> TcRhoType                             -- Expected type of whole proc expression
       -> TcM (OutPat TcId, LHsCmdTop TcId, TcCoercion)

tcProc pat cmd exp_ty
  = newArrowScope $
    do  { (co, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty 
        ; (co1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
        ; let cmd_env = CmdEnv { cmd_arr = arr_ty }
        ; (pat', cmd') <- tcPat ProcExpr pat arg_ty $
                          tcCmdTop cmd_env cmd (unitTy, res_ty)
        ; let res_co = mkTcTransCo co (mkTcAppCo co1 (mkTcNomReflCo res_ty))
        ; return (pat', cmd', res_co) }
\end{code}


%************************************************************************
%*                                                                      *
                Commands
%*                                                                      *
%************************************************************************

\begin{code}
-- See Note [Arrow overview]      
type CmdType    = (CmdArgType, TcTauType)    -- cmd_type 
type CmdArgType = TcTauType                  -- carg_type, a nested tuple

data CmdEnv
  = CmdEnv {
        cmd_arr :: TcType -- arrow type constructor, of kind *->*->*
    }

mkCmdArrTy :: CmdEnv -> TcTauType -> TcTauType -> TcTauType
mkCmdArrTy env t1 t2 = mkAppTys (cmd_arr env) [t1, t2]

---------------------------------------
tcCmdTop :: CmdEnv 
         -> LHsCmdTop Name
         -> CmdType
         -> TcM (LHsCmdTop TcId)

tcCmdTop env (L loc (HsCmdTop cmd _ _ names)) cmd_ty@(cmd_stk, res_ty)
  = setSrcSpan loc $
    do  { cmd'   <- tcCmd env cmd cmd_ty
        ; names' <- mapM (tcSyntaxName ProcOrigin (cmd_arr env)) names
        ; return (L loc $ HsCmdTop cmd' cmd_stk res_ty names') }
----------------------------------------
tcCmd  :: CmdEnv -> LHsCmd Name -> CmdType -> TcM (LHsCmd TcId)
        -- The main recursive function
tcCmd env (L loc cmd) res_ty
  = setSrcSpan loc $ do
        { cmd' <- tc_cmd env cmd res_ty
        ; return (L loc cmd') }

tc_cmd :: CmdEnv -> HsCmd Name  -> CmdType -> TcM (HsCmd TcId)
tc_cmd env (HsCmdPar cmd) res_ty
  = do  { cmd' <- tcCmd env cmd res_ty
        ; return (HsCmdPar cmd') }

tc_cmd env (HsCmdLet binds (L body_loc body)) res_ty
  = do  { (binds', body') <- tcLocalBinds binds         $
                             setSrcSpan body_loc        $
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
  = do  { pred_ty <- newFlexiTyVarTy openTypeKind
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
--              Arrow application
--          (f -< a)   or   (f -<< a)
--
--   D   |- fun :: a t1 t2
--   D,G |- arg :: t1
--  ------------------------
--   D;G |-a  fun -< arg :: stk --> t2
--
--   D,G |- fun :: a t1 t2
--   D,G |- arg :: t1
--  ------------------------
--   D;G |-a  fun -<< arg :: stk --> t2
--
-- (plus -<< requires ArrowApply)

tc_cmd env cmd@(HsCmdArrApp fun arg _ ho_app lr) (_, res_ty)
  = addErrCtxt (cmdCtxt cmd)    $
    do  { arg_ty <- newFlexiTyVarTy openTypeKind
        ; let fun_ty = mkCmdArrTy env arg_ty res_ty
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
--              Command application
--
-- D,G |-  exp : t
-- D;G |-a cmd : (t,stk) --> res
-- -----------------------------
-- D;G |-a cmd exp : stk --> res

tc_cmd env cmd@(HsCmdApp fun arg) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)    $
    do  { arg_ty <- newFlexiTyVarTy openTypeKind
        ; fun'   <- tcCmd env fun (mkPairTy arg_ty cmd_stk, res_ty)
        ; arg'   <- tcMonoExpr arg arg_ty
        ; return (HsCmdApp fun' arg') }

-------------------------------------------
--              Lambda
--
-- D;G,x:t |-a cmd : stk --> res
-- ------------------------------
-- D;G |-a (\x.cmd) : (t,stk) --> res

tc_cmd env 
       (HsCmdLam (MG { mg_alts = [L mtch_loc (match@(Match pats _maybe_rhs_sig grhss))], mg_origin = origin }))
       (cmd_stk, res_ty)
  = addErrCtxt (pprMatchInCtxt match_ctxt match)        $
    do  { (co, arg_tys, cmd_stk') <- matchExpectedCmdArgs n_pats cmd_stk

                -- Check the patterns, and the GRHSs inside
        ; (pats', grhss') <- setSrcSpan mtch_loc                $
                             tcPats LambdaExpr pats arg_tys     $
                             tc_grhss grhss cmd_stk' res_ty

        ; let match' = L mtch_loc (Match pats' Nothing grhss')
              arg_tys = map hsLPatType pats'
              cmd' = HsCmdLam (MG { mg_alts = [match'], mg_arg_tys = arg_tys
                                  , mg_res_ty = res_ty, mg_origin = origin })
        ; return (mkHsCmdCast co cmd') }
  where
    n_pats     = length pats
    match_ctxt = (LambdaExpr :: HsMatchContext Name)    -- Maybe KappaExpr?
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs grhss binds) stk_ty res_ty
        = do { (binds', grhss') <- tcLocalBinds binds $
                                   mapM (wrapLocM (tc_grhs stk_ty res_ty)) grhss
             ; return (GRHSs grhss' binds') }

    tc_grhs stk_ty res_ty (GRHS guards body)
        = do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
                                  \ res_ty -> tcCmd env body (stk_ty, res_ty)
             ; return (GRHS guards' rhs') }

-------------------------------------------
--              Do notation

tc_cmd env (HsCmdDo stmts _) (cmd_stk, res_ty)
  = do  { co <- unifyType unitTy cmd_stk  -- Expecting empty argument stack
        ; stmts' <- tcStmts ArrowExpr (tcArrDoStmt env) stmts res_ty 
        ; return (mkHsCmdCast co (HsCmdDo stmts' res_ty)) }


-----------------------------------------------------------------
--      Arrow ``forms''       (| e c1 .. cn |)
--
--      D; G |-a1 c1 : stk1 --> r1
--      ...
--      D; G |-an cn : stkn --> rn
--      D |-  e :: forall e. a1 (e, stk1) t1
--                                ...
--                        -> an (e, stkn) tn
--                        -> a  (e, stk) t
--      e \not\in (stk, stk1, ..., stkm, t, t1, ..., tn)
--      ----------------------------------------------
--      D; G |-a  (| e c1 ... cn |)  :  stk --> t

tc_cmd env cmd@(HsCmdArrForm expr fixity cmd_args) (cmd_stk, res_ty)    
  = addErrCtxt (cmdCtxt cmd)    $
    do  { (cmd_args', cmd_tys) <- mapAndUnzipM tc_cmd_arg cmd_args
        ; let e_ty = mkForAllTy alphaTyVar $   -- We use alphaTyVar for 'w'
                     mkFunTys cmd_tys $
                     mkCmdArrTy env (mkPairTy alphaTy cmd_stk) res_ty
        ; expr' <- tcPolyExpr expr e_ty
        ; return (HsCmdArrForm expr' fixity cmd_args') }

  where
    tc_cmd_arg :: LHsCmdTop Name -> TcM (LHsCmdTop TcId, TcType)
    tc_cmd_arg cmd
       = do { arr_ty <- newFlexiTyVarTy arrowTyConKind
            ; stk_ty <- newFlexiTyVarTy liftedTypeKind
            ; res_ty <- newFlexiTyVarTy liftedTypeKind
            ; let env' = env { cmd_arr = arr_ty }
            ; cmd' <- tcCmdTop env' cmd (stk_ty, res_ty)
            ; return (cmd',  mkCmdArrTy env' (mkPairTy alphaTy stk_ty) res_ty) }

-----------------------------------------------------------------
--              Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (vcat [ptext (sLit "The expression"), nest 2 (ppr cmd), 
                      ptext (sLit "was found where an arrow command was expected")])


matchExpectedCmdArgs :: Arity -> TcType -> TcM (TcCoercion, [TcType], TcType)
matchExpectedCmdArgs 0 ty 
  = return (mkTcNomReflCo ty, [], ty)
matchExpectedCmdArgs n ty
  = do { (co1, [ty1, ty2]) <- matchExpectedTyConApp pairTyCon ty  
       ; (co2, tys, res_ty) <- matchExpectedCmdArgs (n-1) ty2
       ; return (mkTcTyConAppCo Nominal pairTyCon [co1, co2], ty1:tys, res_ty) }
\end{code}


%************************************************************************
%*                                                                      *
                Stmts
%*                                                                      *
%************************************************************************

\begin{code}
--------------------------------
--      Mdo-notation
-- The distinctive features here are
--      (a) RecStmts, and
--      (b) no rebindable syntax

tcArrDoStmt :: CmdEnv -> TcCmdStmtChecker
tcArrDoStmt env _ (LastStmt rhs _) res_ty thing_inside
  = do  { rhs' <- tcCmd env rhs (unitTy, res_ty)
        ; thing <- thing_inside (panic "tcArrDoStmt")
        ; return (LastStmt rhs' noSyntaxExpr, thing) }

tcArrDoStmt env _ (BodyStmt rhs _ _ _) res_ty thing_inside
  = do  { (rhs', elt_ty) <- tc_arr_rhs env rhs
        ; thing          <- thing_inside res_ty
        ; return (BodyStmt rhs' noSyntaxExpr noSyntaxExpr elt_ty, thing) }

tcArrDoStmt env ctxt (BindStmt pat rhs _ _) res_ty thing_inside
  = do  { (rhs', pat_ty) <- tc_arr_rhs env rhs
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
                <- tcStmtsAndThen ctxt (tcArrDoStmt env) stmts res_ty   $ \ _res_ty' ->
                        -- ToDo: res_ty not really right
                   zipWithM tcCheckId tup_names tup_elt_tys

        ; thing <- thing_inside res_ty
                -- NB:  The rec_ids for the recursive things 
                --      already scope over this part. This binding may shadow
                --      some of them with polymorphic things with the same Name
                --      (see note [RecStmt] in HsExpr)

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
                        ; rhs' <- tcCmd env rhs (unitTy, ty)
                        ; return (rhs', ty) }
\end{code}


%************************************************************************
%*                                                                      *
                Helpers
%*                                                                      *
%************************************************************************


\begin{code}
mkPairTy :: Type -> Type -> Type
mkPairTy t1 t2 = mkTyConApp pairTyCon [t1,t2]

arrowTyConKind :: Kind          --  *->*->*
arrowTyConKind = mkArrowKinds [liftedTypeKind, liftedTypeKind] liftedTypeKind
\end{code}


%************************************************************************
%*                                                                      *
                Errors
%*                                                                      *
%************************************************************************

\begin{code}
cmdCtxt :: HsCmd Name -> SDoc
cmdCtxt cmd = ptext (sLit "In the command:") <+> ppr cmd
\end{code}
