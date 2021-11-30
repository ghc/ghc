{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeFamilies  #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

-- | Typecheck arrow notation
module GHC.Tc.Gen.Arrow ( tcProc ) where

import GHC.Prelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcCheckMonoExpr, tcInferRho, tcSyntaxOp
                                       , tcCheckPolyExpr )

import GHC.Hs
import GHC.Hs.Syn.Type
import GHC.Tc.Errors.Types
import GHC.Tc.Gen.Match
import GHC.Tc.Gen.Head( tcCheckId )
import GHC.Tc.Utils.Concrete ( hasFixedRuntimeRep )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Gen.Bind
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Core.Multiplicity
import GHC.Types.Id( mkLocalId )
import GHC.Tc.Utils.Instantiate
import GHC.Builtin.Types
import GHC.Types.Var.Set
import GHC.Builtin.Types.Prim
import GHC.Types.Basic( Arity )
import GHC.Types.Error
import GHC.Types.SrcLoc
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Misc

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

tcProc :: LPat GhcRn -> LHsCmdTop GhcRn         -- proc pat -> expr
       -> ExpRhoType                            -- Expected type of whole proc expression
       -> TcM (LPat GhcTc, LHsCmdTop GhcTc, TcCoercion)

tcProc pat cmd@(L loc (HsCmdTop names _)) exp_ty
  = do  { exp_ty <- expTypeToType exp_ty  -- no higher-rank stuff with arrows
        ; (co, (exp_ty1, res_ty)) <- matchExpectedAppTy exp_ty
        ; (co1, (arr_ty, arg_ty)) <- matchExpectedAppTy exp_ty1
        -- start with the names as they are used to desugar the proc itself
        -- See #17423
        ; names' <- setSrcSpanA loc $
            mapM (tcSyntaxName ProcOrigin arr_ty) names
        ; let cmd_env = CmdEnv { cmd_arr = arr_ty }
        ; (pat', cmd') <- newArrowScope
                          $ tcCheckPat (ArrowMatchCtxt ProcExpr) pat (unrestricted arg_ty)
                          $ tcCmdTop cmd_env names' cmd (unitTy, res_ty)
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
        cmd_arr :: TcType -- ^ Arrow type constructor, of kind *->*->*
    }

mkCmdArrTy :: CmdEnv -> TcTauType -> TcTauType -> TcTauType
mkCmdArrTy env t1 t2 = mkAppTys (cmd_arr env) [t1, t2]

---------------------------------------
tcCmdTop :: CmdEnv
         -> CmdSyntaxTable GhcTc -- ^ Type-checked Arrow class methods (arr, (>>>), ...)
         -> LHsCmdTop GhcRn
         -> CmdType
         -> TcM (LHsCmdTop GhcTc)

tcCmdTop env names (L loc (HsCmdTop _names cmd)) cmd_ty@(cmd_stk, res_ty)
  = setSrcSpanA loc $
    do  { cmd' <- tcCmd env cmd cmd_ty
        ; return (L loc $ HsCmdTop (CmdTopTc cmd_stk res_ty names) cmd') }

----------------------------------------
tcCmd  :: CmdEnv -> LHsCmd GhcRn -> CmdType -> TcM (LHsCmd GhcTc)
        -- The main recursive function
tcCmd env (L loc cmd) cmd_ty@(_, res_ty)
  = setSrcSpan (locA loc) $ do
        { cmd' <- tc_cmd env cmd cmd_ty
        ; _concrete_ev <- hasFixedRuntimeRep (FRRArrow $ ArrowCmdResTy cmd) res_ty
        ; return (L loc cmd') }

tc_cmd :: CmdEnv -> HsCmd GhcRn  -> CmdType -> TcM (HsCmd GhcTc)
tc_cmd env (HsCmdPar x lpar cmd rpar) res_ty
  = do  { cmd' <- tcCmd env cmd res_ty
        ; return (HsCmdPar x lpar cmd' rpar) }

tc_cmd env (HsCmdLet x tkLet binds tkIn (L body_loc body)) res_ty
  = do  { (binds', body') <- tcLocalBinds binds         $
                             setSrcSpan (locA body_loc) $
                             tc_cmd env body res_ty
        ; return (HsCmdLet x tkLet binds' tkIn (L body_loc body')) }

tc_cmd env in_cmd@(HsCmdCase x scrut matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut
      matches' <- tcCmdMatches env scrut_ty matches (stk, res_ty)
      return (HsCmdCase x scrut' matches')

tc_cmd env in_cmd@(HsCmdLamCase x matches) (stk, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (co, [scrut_ty], stk') <- matchExpectedCmdArgs 1 stk
      matches' <- tcCmdMatches env scrut_ty matches (stk', res_ty)
      return (mkHsCmdWrap (mkWpCastN co) (HsCmdLamCase x matches'))

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
        ; (_, [r_tv]) <- tcInstSkolTyVars unkSkol [alphaTyVar]
        ; let r_ty = mkTyVarTy r_tv
        ; checkTc (not (r_tv `elemVarSet` tyCoVarsOfType pred_ty))
                  TcRnArrowIfThenElsePredDependsOnResultTy
        ; (pred', fun') <- tcSyntaxOp IfThenElseOrigin fun
                              (map synKnownType [pred_ty, r_ty, r_ty])
                              (mkCheckExpType r_ty) $ \ _ _ ->
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

        ; _concrete_ev <- hasFixedRuntimeRep
                        (FRRArrow $ ArrowCmdArrApp (unLoc fun) (unLoc arg) ho_app)
                        fun_ty

        ; return (HsCmdArrApp fun_ty fun' arg' ho_app lr) }
  where
       -- Before type-checking f, use the environment of the enclosing
       -- proc for the (-<) case.
       -- Local bindings, inside the enclosing proc, are not in scope
       -- inside f.  In the higher-order case (-<<), they are.
       -- See Note [Escaping the arrow scope] in GHC.Tc.Types
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
        ; _concrete_ev <- hasFixedRuntimeRep
                        (FRRArrow $ ArrowCmdApp (unLoc fun) (unLoc arg))
                        arg_ty
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
        ; (pats', grhss') <- setSrcSpanA mtch_loc                                 $
                             tcPats (ArrowMatchCtxt KappaExpr)
                               pats (map (unrestricted . mkCheckExpType) arg_tys) $
                             tc_grhss grhss cmd_stk' (mkCheckExpType res_ty)

        ; let match' = L mtch_loc (Match { m_ext = noAnn
                                         , m_ctxt = ArrowMatchCtxt KappaExpr
                                         , m_pats = pats'
                                         , m_grhss = grhss' })
              arg_tys = map (unrestricted . hsLPatType) pats'

        ; _concrete_evs <-
              zipWithM
                (\ (Scaled _ arg_ty) i ->
                  hasFixedRuntimeRep (FRRArrow $ ArrowCmdLam i) arg_ty)
                arg_tys
                [1..]

        ; let
              cmd' = HsCmdLam x (MG { mg_alts = L l [match']
                                    , mg_ext = MatchGroupTc arg_tys res_ty
                                    , mg_origin = origin })
        ; return (mkHsCmdWrap (mkWpCastN co) cmd') }
  where
    n_pats     = length pats
    match_ctxt = ArrowMatchCtxt KappaExpr
    pg_ctxt    = PatGuard match_ctxt

    tc_grhss (GRHSs x grhss binds) stk_ty res_ty
        = do { (binds', grhss') <- tcLocalBinds binds $
                                   mapM (wrapLocMA (tc_grhs stk_ty res_ty)) grhss
             ; return (GRHSs x grhss' binds') }

    tc_grhs stk_ty res_ty (GRHS x guards body)
        = do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
                                  \ res_ty -> tcCmd env body
                                                (stk_ty, checkingExpType "tc_grhs" res_ty)
             ; return (GRHS x guards' rhs') }

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
        ; let e_ty = mkInfForAllTy alphaTyVar $
                     mkVisFunTysMany cmd_tys $
                     mkCmdArrTy env (mkPairTy alphaTy cmd_stk) res_ty
        ; expr' <- tcCheckPolyExpr expr e_ty
        ; return (HsCmdArrForm x expr' f fixity cmd_args') }

  where
    tc_cmd_arg :: LHsCmdTop GhcRn -> TcM (LHsCmdTop GhcTc, TcType)
    tc_cmd_arg cmd@(L loc (HsCmdTop names _))
       = do { arr_ty <- newFlexiTyVarTy arrowTyConKind
            ; stk_ty <- newFlexiTyVarTy liftedTypeKind
            ; res_ty <- newFlexiTyVarTy liftedTypeKind
            ; names' <- setSrcSpanA loc $
                mapM (tcSyntaxName ArrowCmdOrigin arr_ty) names
            ; let env' = env { cmd_arr = arr_ty }
            ; cmd' <- tcCmdTop env' names' cmd (stk_ty, res_ty)
            ; return (cmd',  mkCmdArrTy env' (mkPairTy alphaTy stk_ty) res_ty) }

-----------------------------------------------------------------
--              Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (TcRnArrowCommandExpected cmd)

-- | Typechecking for case command alternatives. Used for both
-- 'HsCmdCase' and 'HsCmdLamCase'.
tcCmdMatches :: CmdEnv
             -> TcType                           -- ^ type of the scrutinee
             -> MatchGroup GhcRn (LHsCmd GhcRn)  -- ^ case alternatives
             -> CmdType
             -> TcM (MatchGroup GhcTc (LHsCmd GhcTc))
tcCmdMatches env scrut_ty matches (stk, res_ty)
  = tcMatchesCase match_ctxt (unrestricted scrut_ty) matches (mkCheckExpType res_ty)
  where
    match_ctxt = MC { mc_what = ArrowMatchCtxt ArrowCaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = do { res_ty' <- expTypeToType res_ty'
                              ; tcCmd env body (stk, res_ty') }

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

tcArrDoStmt env ctxt (BindStmt _ pat rhs) res_ty thing_inside
  = do  { (rhs', pat_ty) <- tc_arr_rhs env rhs
        ; (pat', thing)  <- tcCheckPat (StmtCtxt ctxt) pat (unrestricted pat_ty) $
                            thing_inside res_ty
        ; return (mkTcBindStmt pat' rhs', thing) }

tcArrDoStmt env ctxt (RecStmt { recS_stmts = L l stmts, recS_later_ids = later_names
                            , recS_rec_ids = rec_names }) res_ty thing_inside
  = do  { let tup_names = rec_names ++ filterOut (`elem` rec_names) later_names
        ; tup_elt_tys <- newFlexiTyVarTys (length tup_names) liftedTypeKind
        ; let tup_ids = zipWith (\n p -> mkLocalId n Many p) tup_names tup_elt_tys -- Many because it's a recursive definition
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

        ; let
            stmt :: Stmt GhcTc (LocatedA (HsCmd GhcTc))
            stmt = emptyRecStmtId
                                 { recS_stmts = L l stmts'
                                 -- { recS_stmts = _ stmts'
                                 , recS_later_ids = later_ids
                                 , recS_rec_ids = rec_ids
                                 , recS_ext = unitRecStmtTc
                                     { recS_later_rets = later_rets
                                     , recS_rec_rets = rec_rets
                                     , recS_ret_ty = res_ty} }
        ; return (stmt, thing)
        }}

tcArrDoStmt _ _ stmt _ _
  = pprPanic "tcArrDoStmt: unexpected Stmt" (ppr stmt)

tc_arr_rhs :: CmdEnv -> LHsCmd GhcRn -> TcM (LHsCmd GhcTc, TcType)
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
arrowTyConKind = mkVisFunTysMany [liftedTypeKind, liftedTypeKind] liftedTypeKind

{-
************************************************************************
*                                                                      *
                Errors
*                                                                      *
************************************************************************
-}

cmdCtxt :: HsCmd GhcRn -> SDoc
cmdCtxt cmd = text "In the command:" <+> ppr cmd
