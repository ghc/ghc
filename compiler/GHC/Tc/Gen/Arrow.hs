{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

-}

{-# LANGUAGE RankNTypes, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

-- | Typecheck arrow notation
module GHC.Tc.Gen.Arrow ( tcProc ) where

import GhcPrelude

import {-# SOURCE #-}   GHC.Tc.Gen.Expr( tcMonoExpr, tcInferRho, tcSyntaxOp, tcCheckId, tcPolyExpr )

import GHC.Hs
import GHC.Tc.Gen.Match
import GHC.Tc.Utils.Zonk( hsLPatType )
import GHC.Tc.Utils.TcType
import GHC.Tc.Utils.TcMType
import GHC.Tc.Gen.Bind
import GHC.Tc.Gen.Pat
import GHC.Tc.Utils.Unify
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Env
import GHC.Tc.Types.Origin
import GHC.Tc.Types.Evidence
import GHC.Types.Id( mkLocalId )
import GHC.Tc.Utils.Instantiate
import GHC.Builtin.Types
import GHC.Types.Var.Set
import GHC.Builtin.Types.Prim
import GHC.Builtin.Types.Literals( arrowStackTupTy, arrowEnvTupTy )
import GHC.Types.Basic( Arity )
import GHC.Types.SrcLoc
import Outputable
import Util

import Control.Monad

{-
Note [Arrow overview]
~~~~~~~~~~~~~~~~~~~~~
Here's a summary of arrows and how they typecheck.  First, here's
a cut-down syntax:

  expr ::= ....
        |  proc pat -> cmd

  cmd ::= cmd exp                    -- Arrow application
       |  \pat -> cmd                -- Arrow abstraction
       |  (| exp cmd1 ... cmdn |)    -- Arrow form, n>=0
       |  ... -- if, case in the usual way

  cmd_type ::= stk_ty --> type
    -- where stk_ty has kind [Type], i.e. a type-level list of types

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
                          tcCmdTop cmd_env cmd (nilTy, res_ty)
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
type CmdArgType = TcTauType                  -- carg_type, a type-level list

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

tc_cmd env (HsCmdLet x (L l binds) (L body_loc body)) (stk_ty, res_ty)
  = nullaryCmd stk_ty $
    do  { (binds', body') <- tcLocalBinds binds         $
                             setSrcSpan body_loc        $
                             tc_cmd env body (nilTy, res_ty)
        ; return (HsCmdLet x (L l binds') (L body_loc body')) }

tc_cmd env in_cmd@(HsCmdCase x scrut matches) (stk_ty, res_ty)
  = nullaryCmd stk_ty $ addErrCtxt (cmdCtxt in_cmd) $ do
      (scrut', scrut_ty) <- tcInferRho scrut
      matches' <- tcCmdMatches env scrut_ty matches res_ty
      return (HsCmdCase x scrut' matches')

tc_cmd env in_cmd@(HsCmdLamCase x matches) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt in_cmd) $ do
      (co, scrut_ty, stk_ty') <- matchConsTy stk_ty
      nullaryCmd stk_ty' $ do
        matches' <- tcCmdMatches env scrut_ty matches res_ty
        return (mkHsCmdWrap (mkWpCastN co) (HsCmdLamCase x matches'))

tc_cmd env (HsCmdIf x NoSyntaxExprRn pred b1 b2) (stk_ty, res_ty) -- Ordinary 'if'
  = nullaryCmd stk_ty $
    do  { pred' <- tcMonoExpr pred (mkCheckExpType boolTy)
        ; b1'   <- tcCmd env b1 (nilTy, res_ty)
        ; b2'   <- tcCmd env b2 (nilTy, res_ty)
        ; return (HsCmdIf x NoSyntaxExprTc pred' b1' b2')
        }

tc_cmd env (HsCmdIf x fun@(SyntaxExprRn {}) pred b1 b2) (stk_ty, res_ty) -- Rebindable syntax for if
  = nullaryCmd stk_ty $
    do  { pred_ty <- newOpenFlexiTyVarTy
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
               tcMonoExpr pred (mkCheckExpType pred_ty)

        ; b1' <- tcCmd env b1 (nilTy, res_ty)
        ; b2' <- tcCmd env b2 (nilTy, res_ty)
        ; return (HsCmdIf x fun' pred' b1' b2')
        }

-------------------------------------------
--              Arrow application
--          (f -< a)   or   (f -<< a)
--
-- G   |- e1 :: a (ArrowStackTup (b ': s)) t
-- G,D |- e2 :: b
-- ----------------------------------------- [AppF]
-- G|D |-a e1 -< e2 :: s --> t
--
-- G,D |- e1 :: a (ArrowStackTup (b ': s)) t
-- G,D |- e2 :: b
-- a ∈ ArrowApply
-- ----------------------------------------- [AppH]
-- G|D |-a e1 -<< e2 :: s --> t

tc_cmd env cmd@(HsCmdArrApp _ fun arg ho_app lr) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { arg_ty <- newOpenFlexiTyVarTy

        ; let args_ty = arrowStackTupTy (consTy arg_ty stk_ty)
              fun_ty = mkCmdArrTy env args_ty res_ty
        ; fun' <- select_arrow_scope (tcMonoExpr fun (mkCheckExpType fun_ty))

        ; arg' <- tcMonoExpr arg (mkCheckExpType arg_ty)

        ; return (HsCmdArrApp (cmd_arr env) fun' arg' ho_app lr) }
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
-- G|D |-a cmd :: (b ': s) --> t
-- G,D |-  e   :: b
-- ----------------------------- [AppC]
-- G|D |-a cmd e :: s --> t

tc_cmd env cmd@(HsCmdApp x fun arg) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { arg_ty <- newOpenFlexiTyVarTy
        ; fun'   <- tcCmd env fun (consTy arg_ty stk_ty, res_ty)
        ; arg'   <- tcMonoExpr arg (mkCheckExpType arg_ty)
        ; return (HsCmdApp x fun' arg') }

-------------------------------------------
--              Lambda
--
-- pat : b => D2
-- G|D1,D2 |-a cmd :: s --> t
-- -------------------------------------- [Abs]
-- G|D1 |-a \pat -> cmd :: (b ': s) --> t

tc_cmd env
       (HsCmdLam x (MG { mg_alts = L l [L mtch_loc
                                   (match@(Match { m_pats = pats, m_grhss = grhss }))],
                         mg_origin = origin }))
       (stk_ty, res_ty)
  = addErrCtxt (pprMatchInCtxt match) $
    do  { (co, arg_tys, stk_ty') <- matchConsTys n_pats stk_ty

                -- Check the patterns, and the GRHSs inside
        ; (pats', grhss') <- setSrcSpan mtch_loc                                 $
                             tcPats LambdaExpr pats (map mkCheckExpType arg_tys) $
                             tc_grhss grhss stk_ty' (mkCheckExpType res_ty)

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

    tc_grhs stk_ty res_ty (GRHS x guards body)
        = do { (guards', rhs') <- tcStmtsAndThen pg_ctxt tcGuardStmt guards res_ty $
                                  \ res_ty -> tcCmd env body
                                                (stk_ty, checkingExpType "tc_grhs" res_ty)
             ; return (GRHS x guards' rhs') }

-------------------------------------------
--              Do notation

tc_cmd env (HsCmdDo _ (L l stmts) ) (stk_ty, res_ty)
  = nullaryCmd stk_ty $
    do  { stmts' <- tcStmts ArrowExpr (tcArrDoStmt env) stmts res_ty
        ; return (HsCmdDo res_ty (L l stmts')) }


-----------------------------------------------------------------
--      Arrow control operators     (| e cmd1 ... cmdn |)
--
-- G |- e : forall w. a1 (ArrowEnvTup w s1) t1
--                            ...
--                 -> an (ArrowEnvTup w sn) tn
--                 -> a0 (ArrowEnvTup w s0) t0
-- G|D |-a1 cmd1 : s1 --> t1
-- ...
-- G|D |-an cmdn : sn --> tn
-- ------------------------------------------ [Op]
-- G|D |-a0 (| e cmd1 ... cmdn |) : s0 --> t0

tc_cmd env cmd@(HsCmdArrForm x expr f fixity cmd_args) (stk_ty, res_ty)
  = addErrCtxt (cmdCtxt cmd) $
    do  { (cmd_args', cmd_tys) <- mapAndUnzipM tc_cmd_arg cmd_args
                              -- We use alphaTyVar for 'w'
        ; let e_ty = mkInvForAllTy alphaTyVar $
                     mkVisFunTys cmd_tys $
                     mkCmdArrTy env (arrowEnvTupTy alphaTy stk_ty) res_ty
        ; expr' <- tcPolyExpr expr e_ty
        ; return (HsCmdArrForm x expr' f fixity cmd_args') }

  where
    tc_cmd_arg :: LHsCmdTop GhcRn -> TcM (LHsCmdTop GhcTcId, TcType)
    tc_cmd_arg cmd
       = do { arr_ty <- newFlexiTyVarTy arrowTyConKind
            ; stk_ty <- newFlexiTyVarTy (mkListTy liftedTypeKind)
            ; res_ty <- newFlexiTyVarTy liftedTypeKind
            ; let env' = env { cmd_arr = arr_ty }
            ; cmd' <- tcCmdTop env' cmd (stk_ty, res_ty)
            ; let arg_ty = arrowEnvTupTy alphaTy stk_ty
            ; return (cmd', mkCmdArrTy env' arg_ty res_ty) }

-----------------------------------------------------------------
--              Base case for illegal commands
-- This is where expressions that aren't commands get rejected

tc_cmd _ cmd _
  = failWithTc (vcat [text "The expression", nest 2 (ppr cmd),
                      text "was found where an arrow command was expected"])

-- | Typechecking for case command alternatives. Used for both
-- 'HsCmdCase' and 'HsCmdLamCase'.
tcCmdMatches :: CmdEnv
             -> TcType                           -- ^ type of the scrutinee
             -> MatchGroup GhcRn (LHsCmd GhcRn)  -- ^ case alternatives
             -> TcType                           -- ^ type of the result
             -> TcM (MatchGroup GhcTcId (LHsCmd GhcTcId))
tcCmdMatches env scrut_ty matches res_ty
  = tcMatchesCase match_ctxt scrut_ty matches (mkCheckExpType res_ty)
  where
    match_ctxt = MC { mc_what = CaseAlt,
                      mc_body = mc_body }
    mc_body body res_ty' = do { res_ty' <- expTypeToType res_ty'
                              ; tcCmd env body (nilTy, res_ty') }

-- | Checks that the stack is '[] for commands that do not accept arguments.
nullaryCmd :: TcType -- ^ type of the stack
           -> TcM (HsCmd GhcTcId)
           -> TcM (HsCmd GhcTcId)
nullaryCmd stk_ty m = do
  co <- unifyType Nothing stk_ty nilTy
  mkHsCmdWrap (mkWpCastN co) <$> m

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
  = do  { rhs' <- tcCmd env rhs (nilTy, res_ty)
        ; thing <- thing_inside (panic "tcArrDoStmt")
        ; return (LastStmt x rhs' noret noSyntaxExpr, thing) }

tcArrDoStmt env _ (BodyStmt _ rhs _ _) res_ty thing_inside
  = do  { (rhs', elt_ty) <- tc_arr_rhs env rhs
        ; thing          <- thing_inside res_ty
        ; return (BodyStmt elt_ty rhs' noSyntaxExpr noSyntaxExpr, thing) }

tcArrDoStmt env ctxt (BindStmt _ pat rhs) res_ty thing_inside
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
                        ; rhs' <- tcCmd env rhs (nilTy, ty)
                        ; return (rhs', ty) }

{-
************************************************************************
*                                                                      *
                Helpers
*                                                                      *
************************************************************************
-}

nilTy :: Type
nilTy = mkTyConApp promotedNilDataCon [liftedTypeKind]

consTy :: Type -> Type -> Type
consTy ty tys = mkTyConApp promotedConsDataCon [liftedTypeKind, ty, tys]

matchConsTy :: TcType -> TcM (TcCoercionN, TcType, TcType)
matchConsTy tys = do
  (co1, [k, ty, tys']) <- matchExpectedTyConApp promotedConsDataCon tys
  k_co <- unifyKind Nothing k liftedTypeKind
  let co2 = mkTcTyConAppCo Nominal promotedConsDataCon
              [k_co, mkTcNomReflCo ty, mkTcNomReflCo tys']
  pure (mkTcTransCo co1 co2, ty, tys')

-- | @'matchConsTys' n ty@ expects @ty@ to be a type-level list of the shape
-- > a0 ': a1 ': ... ': an ': ty'
-- and returns @[a0, a1, ..., an]@ and @ty'@ (plus evidence).
matchConsTys :: Int -> TcType -> TcM (TcCoercionN, [TcType], TcType)
matchConsTys 0 tys = pure (mkTcNomReflCo tys, [], tys)
matchConsTys n tys = do
  (co1, ty1, tys1) <- matchConsTy tys
  (co2, ty2, tys2) <- matchConsTys (n-1) tys1
  pure (mkTcTransCo co1 (mkNomConsCo (mkTcNomReflCo ty1) co2), ty1:ty2, tys2)

mkNomConsCo :: TcCoercionN -> TcCoercionN -> TcCoercionN
mkNomConsCo co1 co2 = mkTcTyConAppCo Nominal promotedConsDataCon
                        [mkTcNomReflCo liftedTypeKind, co1, co2]

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
