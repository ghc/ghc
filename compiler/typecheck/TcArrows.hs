{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Typecheck arrow notation
-}

{-# LANGUAGE RankNTypes, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

module TcArrows ( tcProc ) where

import GhcPrelude

import {-# SOURCE #-}   TcExpr( tcCheckMonoExpr, tcInferRho, tcSyntaxOp
                              , tcCheckId, tcCheckPolyExpr )

import GHC.Hs
import TcMatches
import TcHsSyn( hsLPatType )
import TcType
import TcMType
import TcBinds
import TcPat
import TcUnify
import TcRnMonad
import TcEnv
import TcOrigin
import TcEvidence
import Id( mkLocalId )
import Inst
import TysWiredIn
import VarSet
import TysPrim
import BasicTypes( Arity )
import SrcLoc
import Outputable
import Util

import Control.Monad

{-
Note [Arrow overview]
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


************************************************************************
*                                                                      *
                Proc
*                                                                      *
************************************************************************
-}

tcProc :: InPat GhcRn -> LHsCmdTop GhcRn        -- proc pat -> expr
       -> ExpRhoType                            -- Expected type of whole proc expression
       -> TcM (OutPat GhcTcId, LHsCmdTop GhcTcId, TcCoercion)

tcProc pat cmd exp_ty
  = newArrowScope $
    do  { exp_ty <- expTypeToType exp_ty  -- no higher-rank stuff with arrows
        ; (co, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty
        ; (co1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
        ; let cmd_env = CmdEnv { cmd_arr = arr_ty }
        ; (pat', cmd') <- tcPat ProcExpr pat (mkCheckExpType arg_ty) $
                          tcCmdTop cmd_env cmd (unitTy, res_ty)
        ; let res_co = mkTcTransCo co
                         (mkTcAppCo co1 (mkTcNomReflCo res_ty))
        ; return (pat', cmd', res_co) }

{-
************************************************************************
*                                                                      *
                Commands
*                                                                      *
************************************************************************
-}

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
         -> LHsCmdTop GhcRn
         -> CmdType
         -> TcM (LHsCmdTop GhcTcId)

tcCmdTop env (L loc (HsCmdTop names cmd)) cmd_ty@(cmd_stk, res_ty)
  = setSrcSpan loc $
    do  { cmd'   <- tcCmd env cmd cmd_ty
        ; names' <- mapM (tcSyntaxName ProcOrigin (cmd_arr env)) names
        ; return (L loc $ HsCmdTop (CmdTopTc cmd_stk res_ty names') cmd') }
tcCmdTop _ (L _ (XCmdTop nec)) _ = noExtCon nec

----------------------------------------
tcCmd  :: CmdEnv -> LHsCmd GhcRn -> CmdType -> TcM (LHsCmd GhcTcId)
        -- The main recursive function
tcCmd env (L loc cmd) res_ty
  = setSrcSpan loc $ do
        { cmd' <- tc_cmd env cmd res_ty
        ; return (L loc cmd') }

tc_cmd :: CmdEnv -> HsCmd GhcRn  -> CmdType -> TcM (HsCmd GhcTcId)
tc_cmd env (HsCmdPar x cmd) res_ty
  = do  { cmd' <- tcCmd env cmd res_ty
        ; return (HsCmdPar x cmd') }

tc_cmd env (HsCmdLet x (L l binds) (L body_loc body)) res_ty
  = do  { (binds', body') <- tcLocalBinds binds         $
                             setSrcSpan body_loc        $
                             tc_cmd env body res_ty
        ; return (HsCmdLet x (L l binds') (L body_loc body')) }

tc_cmd env in_cmd@(HsCmdCase x scrut matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut
      matches' <- tcMatchesCase match_ctxt scrut_ty matches (mkCheckExpType res_ty)
      return (HsCmdCase x scrut' matches')
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = do { res_ty' <- expTypeToType res_ty'
                              ; tcCmd env body (stk, res_ty') }

tc_cmd env (HsCmdIf x NoSyntaxExprRn pred b1 b2) res_ty    -- Ordinary 'if'
  = do  { pred' <- tcCheckMonoExpr pred boolTy
        ; b1'   <- tcCmd env b1 res_ty
        ; b2'   <- tcCmd env b2 res_ty
        ; return (HsCmdIf x NoSyntaxExprTc pred' b1' b2')
    }

tc_cmd env (HsCmdIf x fun@(SyntaxExprRn {}) pred b1 b2) res_ty -- Rebindable syntax for if
  = do  { pred_ty <- newOpenFlexiTyVarTy
        -- For arrows, need ifThenElse :: forall r. T -> r -> r -> r
        -- because we're going to apply it to the environment, not
        -- the return value.
        ; (_, [r_tv]) <- tcInstSkolTyVars [alphaTyVar]
        ; let r_ty = mkTyVarTy r_tv
        ; checkTc (not (r_tv `elemVarSet` tyCoVarsOfType pred_ty))
                  (text "Predicate type of `ifThenElse' depends on result type")
        ; (pred', fun')
            <- tcSyntaxOp IfOrigin fun (map synKnownType [pred_ty, r_ty, r_ty])
                                       (mkCheckExpType r_ty) $ \ _ ->
               tcCheckMonoExpr pred pred_ty

        ; b1'   <- tcCmd env b1 res_ty
        ; b2'   <- tcCmd env b2 res_ty
        ; return (HsCmdIf x fun' pred' b1' b2')
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

tc_cmd env cmd@(HsCmdArrApp _ fun arg ho_app lr) (_, res_ty)
  = addErrCtxt (cmdCtxt cmd)    $
    do  { arg_ty <- newOpenFlexiTyVarTy
        ; let fun_ty = mkCmdArrTy env arg_ty res_ty
        ; fun' <- select_arrow_scope (tcCheckMonoExpr fun fun_ty)

        ; arg' <- tcCheckMonoExpr arg arg_ty

        ; return (HsCmdArrApp fun_ty fun' arg' ho_app lr) }
  where
       -- Before type-checking f, use the environment of the enclosing
       -- proc for the (-<) case.
       -- Local bindings, inside the enclosing proc, are not in scope
       -- inside f.  In the higher-order case (-<<), they are.
       -- See Note [Escaping the arrow scope] in TcRnTypes
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

tc_cmd env cmd@(HsCmdApp x fun arg) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)    $
    do  { arg_ty <- newOpenFlexiTyVarTy
        ; fun'   <- tcCmd env fun (mkPairTy arg_ty cmd_stk, res_ty)
        ; arg'   <- tcCheckMonoExpr arg arg_ty
        ; return (HsCmdApp x fun' arg') }

-------------------------------------------
--              Lambda
--
-- D;G,x:t |-a cmd : stk --> res
-- ------------------------------
-- D;G |-a (\x.cmd) : (t,stk) --> res

tc_cmd env
       (HsCmdLam x (MG { mg_alts = L l [L mtch_loc
                                   (match@(Match { m_pats = pats, m_grhss = grhss }))],
                         mg_origin = origin }))
       (cmd_stk, res_ty)
  = addErrCtxt (pprMatchInCtxt match)        $
    do  { (co, arg_tys, cmd_stk') <- matchExpectedCmdArgs n_pats cmd_stk

                -- Check the patterns, and the GRHSs inside
        ; (pats', grhss') <- setSrcSpan mtch_loc                                 $
                             tcPats LambdaExpr pats (map mkCheckExpType arg_tys) $
                             tc_grhss grhss cmd_stk' (mkCheckExpType res_ty)

        ; let match' = L mtch_loc (Match { m_ext = noExtField
                                         , m_ctxt = LambdaExpr, m_pats = pats'
                                         , m_grhss = grhss' })
              arg_tys = map hsLPatType pats'
              cmd' = HsCmdLam x (MG { mg_alts = L l [match']
                                    , mg_ext = MatchGroupTc arg_tys res_ty
                                    , mg_origin = origin })
        ; return (mkHsCmdWrap (mkWpCastN co) cmd') }
  where
    n_pats     = length pats
    match_ctxt = (LambdaExpr :: HsMatchContext GhcRn)    -- Maybe KappaExpr?
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs x grhss (L l binds)) stk_ty res_ty
        = do { (binds', grhss') <- tcLocalBinds binds $
                                   mapM (wrapLocM (tc_grhs stk_ty res_ty)) grhss
             ; return (GRHSs x grhss' (L l binds')) }
    tc_grhss (XGRHSs nec) _ _ = noExtCon nec

    tc_grhs stk_ty res_ty (GRHS x guards body)
        = do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
                                  \ res_ty -> tcCmd env body
                                                (stk_ty, checkingExpType "tc_grhs" res_ty)
             ; return (GRHS x guards' rhs') }
    tc_grhs _ _ (XGRHS nec) = noExtCon nec

-------------------------------------------
--              Do notation

tc_cmd env (HsCmdDo _ (L l stmts) ) (cmd_stk, res_ty)
  = do  { co <- unifyType Nothing unitTy cmd_stk  -- Expecting empty argument stack
        ; stmts' <- tcStmts ArrowExpr (tcArrDoStmt env) stmts res_ty
        ; return (mkHsCmdWrap (mkWpCastN co) (HsCmdDo res_ty (L l stmts') )) }


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

tc_cmd env cmd@(HsCmdArrForm x expr f fixity cmd_args) (cmd_stk, res_ty)
  = addErrCtxt (cmdCtxt cmd)    $
    do  { (cmd_args', cmd_tys) <- mapAndUnzipM tc_cmd_arg cmd_args
                              -- We use alphaTyVar for 'w'
        ; let e_ty = mkInvForAllTy alphaTyVar $
                     mkVisFunTys cmd_tys $
                     mkCmdArrTy env (mkPairTy alphaTy cmd_stk) res_ty
        ; expr' <- tcCheckPolyExpr expr e_ty
        ; return (HsCmdArrForm x expr' f fixity cmd_args') }

  where
    tc_cmd_arg :: LHsCmdTop GhcRn -> TcM (LHsCmdTop GhcTcId, TcType)
    tc_cmd_arg cmd
       = do { arr_ty <- newFlexiTyVarTy arrowTyConKind
            ; stk_ty <- newFlexiTyVarTy liftedTypeKind
            ; res_ty <- newFlexiTyVarTy liftedTypeKind
            ; let env' = env { cmd_arr = arr_ty }
            ; cmd' <- tcCmdTop env' cmd (stk_ty, res_ty)
            ; return (cmd',  mkCmdArrTy env' (mkPairTy alphaTy stk_ty) res_ty) }

tc_cmd _ (XCmd nec) _ = noExtCon nec

-----------------------------------------------------------------
--              Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (vcat [text "The expression", nest 2 (ppr cmd),
                      text "was found where an arrow command was expected"])


matchExpectedCmdArgs :: Arity -> TcType -> TcM (TcCoercionN, [TcType], TcType)
matchExpectedCmdArgs 0 ty
  = return (mkTcNomReflCo ty, [], ty)
matchExpectedCmdArgs n ty
  = do { (co1, [ty1, ty2]) <- matchExpectedTyConApp pairTyCon ty
       ; (co2, tys, res_ty) <- matchExpectedCmdArgs (n-1) ty2
       ; return (mkTcTyConAppCo Nominal pairTyCon [co1, co2], ty1:tys, res_ty) }

{-
************************************************************************
*                                                                      *
                Stmts
*                                                                      *
************************************************************************
-}

--------------------------------
--      Mdo-notation
-- The distinctive features here are
--      (a) RecStmts, and
--      (b) no rebindable syntax

tcArrDoStmt :: CmdEnv -> TcCmdStmtChecker
tcArrDoStmt env _ (LastStmt x rhs noret _) res_ty thing_inside
  = do  { rhs' <- tcCmd env rhs (unitTy, res_ty)
        ; thing <- thing_inside (panic "tcArrDoStmt")
        ; return (LastStmt x rhs' noret noSyntaxExpr, thing) }

tcArrDoStmt env _ (BodyStmt _ rhs _ _) res_ty thing_inside
  = do  { (rhs', elt_ty) <- tc_arr_rhs env rhs
        ; thing          <- thing_inside res_ty
        ; return (BodyStmt elt_ty rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcArrDoStmt env ctxt (BindStmt _ pat rhs _ _) res_ty thing_inside
  = do  { (rhs', pat_ty) <- tc_arr_rhs env rhs
        ; (pat', thing)  <- tcPat (StmtCtxt ctxt) pat (mkCheckExpType pat_ty) $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcArrDoStmt env ctxt (RecStmt { recS_stmts = stmts, recS_later_ids = later_names
                            , recS_rec_ids = rec_names }) res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith mkLocalId tup_names tup_elt_tys
        ; tcExtendIdEnv tup_ids $ do
        { (stmts', tup_rets)
                <- tcStmtsAndThen ctxt (tcArrDoStmt env) stmts res_ty   $ \ _res_ty' ->
                        -- ToDo: res_ty not really right
                   zipWithM tcCheckId tup_names (map mkCheckExpType tup_elt_tys)

        ; thing <- thing_inside res_ty
                -- NB:  The rec_ids for the recursive things
                --      already scope over this part. This binding may shadow
                --      some of them with polymorphic things with the same Name
                --      (see note [RecStmt] in GHC.Hs.Expr)

        ; let rec_ids = takeList rec_names tup_ids
        ; later_ids <- tcLookupLocalIds later_names

        ; let rec_rets = takeList rec_names tup_rets
        ; let ret_table = zip tup_ids tup_rets
        ; let later_rets = [r | i <- later_ids, (j, r) <- ret_table, i == j]

        ; return (emptyRecStmtId { recS_stmts = stmts'
                                 , recS_later_ids = later_ids
                                 , recS_rec_ids = rec_ids
                                 , recS_ext = unitRecStmtTc
                                     { recS_later_rets = later_rets
                                     , recS_rec_rets = rec_rets
                                     , recS_ret_ty = res_ty} }, thing)
        }}

tcArrDoStmt _ _ stmt _ _
  = pprPanic "tcArrDoStmt: unexpected Stmt" (ppr stmt)

tc_arr_rhs :: CmdEnv -> LHsCmd GhcRn -> TcM (LHsCmd GhcTcId, TcType)
tc_arr_rhs env rhs = do { ty <- newFlexiTyVarTy liftedTypeKind
                        ; rhs' <- tcCmd env rhs (unitTy, ty)
                        ; return (rhs', ty) }

{-
************************************************************************
*                                                                      *
                Helpers
*                                                                      *
************************************************************************
-}

mkPairTy :: Type -> Type -> Type
mkPairTy t1 t2 = mkTyConApp pairTyCon [t1,t2]

arrowTyConKind :: Kind          --  *->*->*
arrowTyConKind = mkVisFunTys [liftedTypeKind, liftedTypeKind] liftedTypeKind

{-
************************************************************************
*                                                                      *
                Errors
*                                                                      *
************************************************************************
-}

cmdCtxt :: HsCmd GhcRn -> SDoc
cmdCtxt cmd = text "In the command:" <+> ppr cmd
