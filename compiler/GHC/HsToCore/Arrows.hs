{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections #-}

{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring arrow commands
-}

module GHC.HsToCore.Arrows ( dsProcExpr ) where

import GHC.Prelude

import GHC.HsToCore.Match
import GHC.HsToCore.Utils
import GHC.HsToCore.Monad

import GHC.Hs
import GHC.Hs.Syn.Type

-- NB: The desugarer, which straddles the source and Core worlds, sometimes
--     needs to see source types (newtypes etc), and sometimes not
--     So WATCH OUT; check each use of split*Ty functions.
-- Sigh.  This is a pain.

import {-# SOURCE #-} GHC.HsToCore.Expr ( dsExpr, dsLExpr, dsLocalBinds,
                                          dsSyntaxExpr )

import GHC.Tc.Utils.TcType
import GHC.Core.Multiplicity
import GHC.Tc.Types.Evidence
import GHC.Core
import GHC.Core.FVs
import GHC.Core.Utils
import GHC.Core.Make
import GHC.HsToCore.Binds (dsHsWrapper)


import GHC.Types.Id
import GHC.Core.ConLike
import GHC.Builtin.Types
import GHC.Types.Basic
import GHC.Builtin.Names
import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Types.Var.Set
import GHC.Types.SrcLoc
import GHC.Data.List.SetOps( assocMaybe )
import Data.Foldable (toList)
import Data.List (mapAccumL)
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import GHC.Utils.Misc
import GHC.Types.Unique.DSet

data DsCmdEnv = DsCmdEnv {
        arr_id, compose_id, first_id, app_id, choice_id, loop_id :: CoreExpr
    }

mkCmdEnv :: CmdSyntaxTable GhcTc -> DsM ([CoreBind], DsCmdEnv)
-- See Note [CmdSyntaxTable] in GHC.Hs.Expr
mkCmdEnv tc_meths
  = do { (meth_binds, prs) <- mapAndUnzipM mk_bind tc_meths

       -- NB: Some of these lookups might fail, but that's OK if the
       -- symbol is never used. That's why we use Maybe first and then
       -- panic. An eager panic caused trouble in typecheck/should_compile/tc192
       ; let the_arr_id     = assocMaybe prs arrAName
             the_compose_id = assocMaybe prs composeAName
             the_first_id   = assocMaybe prs firstAName
             the_app_id     = assocMaybe prs appAName
             the_choice_id  = assocMaybe prs choiceAName
             the_loop_id    = assocMaybe prs loopAName

       ; return (meth_binds, DsCmdEnv {
               arr_id     = Var (unmaybe the_arr_id arrAName),
               compose_id = Var (unmaybe the_compose_id composeAName),
               first_id   = Var (unmaybe the_first_id firstAName),
               app_id     = Var (unmaybe the_app_id appAName),
               choice_id  = Var (unmaybe the_choice_id choiceAName),
               loop_id    = Var (unmaybe the_loop_id loopAName)
             }) }
  where
    mk_bind (std_name, expr)
      = do { rhs <- dsExpr expr
           ; id <- newSysLocalMDs (exprType rhs)
           -- no check needed; these are functions
           ; return (NonRec id rhs, (std_name, id)) }

    unmaybe Nothing name = pprPanic "mkCmdEnv" (text "Not found:" <+> ppr name)
    unmaybe (Just id) _  = id

-- arr :: forall b c. (b -> c) -> a b c
do_arr :: DsCmdEnv -> Type -> Type -> CoreExpr -> CoreExpr
do_arr ids b_ty c_ty f = mkApps (arr_id ids) [Type b_ty, Type c_ty, f]

-- (>>>) :: forall b c d. a b c -> a c d -> a b d
do_compose :: DsCmdEnv -> Type -> Type -> Type ->
                CoreExpr -> CoreExpr -> CoreExpr
do_compose ids b_ty c_ty d_ty f g
  = mkApps (compose_id ids) [Type b_ty, Type c_ty, Type d_ty, f, g]

-- first :: forall b c d. a b c -> a (b,d) (c,d)
do_first :: DsCmdEnv -> Type -> Type -> Type -> CoreExpr -> CoreExpr
do_first ids b_ty c_ty d_ty f
  = mkApps (first_id ids) [Type b_ty, Type c_ty, Type d_ty, f]

-- app :: forall b c. a (a b c, b) c
do_app :: DsCmdEnv -> Type -> Type -> CoreExpr
do_app ids b_ty c_ty = mkApps (app_id ids) [Type b_ty, Type c_ty]

-- (|||) :: forall b d c. a b d -> a c d -> a (Either b c) d
-- note the swapping of d and c
do_choice :: DsCmdEnv -> Type -> Type -> Type ->
                CoreExpr -> CoreExpr -> CoreExpr
do_choice ids b_ty c_ty d_ty f g
  = mkApps (choice_id ids) [Type b_ty, Type d_ty, Type c_ty, f, g]

-- loop :: forall b d c. a (b,d) (c,d) -> a b c
-- note the swapping of d and c
do_loop :: DsCmdEnv -> Type -> Type -> Type -> CoreExpr -> CoreExpr
do_loop ids b_ty c_ty d_ty f
  = mkApps (loop_id ids) [Type b_ty, Type d_ty, Type c_ty, f]

-- premap :: forall b c d. (b -> c) -> a c d -> a b d
-- premap f g = arr f >>> g
do_premap :: DsCmdEnv -> Type -> Type -> Type ->
                CoreExpr -> CoreExpr -> CoreExpr
do_premap ids b_ty c_ty d_ty f g
   = do_compose ids b_ty c_ty d_ty (do_arr ids b_ty c_ty f) g

-- construct CoreExpr for \ (a :: a_ty, b :: b_ty) -> a
mkFstExpr :: Type -> Type -> DsM CoreExpr
mkFstExpr a_ty b_ty = do
    a_var <- newSysLocalMDs a_ty
    b_var <- newSysLocalMDs b_ty
    pair_var <- newSysLocalMDs (mkCorePairTy a_ty b_ty)
    return (Lam pair_var
               (coreCasePair pair_var a_var b_var (Var a_var)))

-- construct CoreExpr for \ (a :: a_ty, b :: b_ty) -> b
mkSndExpr :: Type -> Type -> DsM CoreExpr
mkSndExpr a_ty b_ty = do
    a_var <- newSysLocalMDs a_ty
    b_var <- newSysLocalMDs b_ty
    pair_var <- newSysLocalMDs (mkCorePairTy a_ty b_ty)
    return (Lam pair_var
               (coreCasePair pair_var a_var b_var (Var b_var)))

{-
Build case analysis of a tuple.  This cannot be done in the DsM monad,
because the list of variables is typically not yet defined.
-}

-- coreCaseTuple [u1..] v [x1..xn] body
--      = case v of v { (x1, .., xn) -> body }
-- But the matching may be nested if the tuple is very big

coreCaseTuple :: Id -> [Id] -> CoreExpr -> DsM CoreExpr
coreCaseTuple scrut_var vars body
  = mkBigTupleCase vars body (Var scrut_var)

coreCasePair :: Id -> Id -> Id -> CoreExpr -> CoreExpr
coreCasePair scrut_var var1 var2 body
  = Case (Var scrut_var) scrut_var (exprType body)
         [Alt (DataAlt (tupleDataCon Boxed 2)) [var1, var2] body]

mkCorePairTy :: Type -> Type -> Type
mkCorePairTy t1 t2 = mkBoxedTupleTy [t1, t2]

mkCorePairExpr :: CoreExpr -> CoreExpr -> CoreExpr
mkCorePairExpr e1 e2 = mkCoreTup [e1, e2]

mkCoreUnitExpr :: CoreExpr
mkCoreUnitExpr = mkCoreTup []

{- Note [Environment and stack]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The input is divided into

* A local environment, which is a flat tuple (unless it's too big)
  The elements of the local environment can be
  - of kind Type (for ordinary variables), or
  - of kind Constraint (for dictionaries bound by patterns)

* A stack, which is a right-nested pair.
  The elements on the stack are always of kind Type.

So in general, the input has the form

        ((x1,...,xn), (s1,...(sk,())...))

where xi are the environment values, and si the ones on the stack,
with s1 being the "top", the first one to be matched with a lambda.
-}

envStackType :: [Id] -> Type -> Type
envStackType ids stack_ty = mkCorePairTy (mkBigCoreVarTupTy ids) stack_ty

-- splitTypeAt n (t1,... (tn,t)...) = ([t1, ..., tn], t)
splitTypeAt :: Int -> Type -> ([Type], Type)
splitTypeAt n ty
  | n == 0 = ([], ty)
  | otherwise = case tcTyConAppArgs ty of
      [t, ty'] -> let (ts, ty_r) = splitTypeAt (n-1) ty' in (t:ts, ty_r)
      _ -> pprPanic "splitTypeAt" (ppr ty)

----------------------------------------------
--              buildEnvStack
--
--      ((x1,...,xn),stk)

buildEnvStack :: [Id] -> Id -> CoreExpr
buildEnvStack env_ids stack_id
  = mkCorePairExpr (mkBigCoreVarTup env_ids) (Var stack_id)

----------------------------------------------
--              matchEnvStack
--
--      \ ((x1,...,xn),stk) -> body
--      =>
--      \ pair ->
--      case pair of (tup,stk) ->
--      case tup of (x1,...,xn) ->
--      body

matchEnvStack   :: [Id]         -- x1..xn
                -> Id           -- stk
                -> CoreExpr     -- e
                -> DsM CoreExpr
matchEnvStack env_ids stack_id body = do
    tup_var <- newSysLocalMDs (mkBigCoreVarTupTy env_ids)
    match_env <- coreCaseTuple tup_var env_ids body
    pair_id <- newSysLocalMDs (mkCorePairTy (idType tup_var) (idType stack_id))
    return (Lam pair_id (coreCasePair pair_id tup_var stack_id match_env))

----------------------------------------------
--              matchEnv
--
--      \ (x1,...,xn) -> body
--      =>
--      \ tup ->
--      case tup of (x1,...,xn) ->
--      body

matchEnv :: [Id]        -- x1..xn
         -> CoreExpr    -- e
         -> DsM CoreExpr
matchEnv env_ids body = do
    tup_id <- newSysLocalMDs (mkBigCoreVarTupTy env_ids)
    tup_case <- coreCaseTuple tup_id env_ids body
    return (Lam tup_id tup_case)

----------------------------------------------
--              matchVarStack
--
--      case (x1, ...(xn, s)...) -> e
--      =>
--      case z0 of (x1,z1) ->
--      case zn-1 of (xn,s) ->
--      e
matchVarStack :: [Id] -> Id -> CoreExpr -> DsM (Id, CoreExpr)
matchVarStack [] stack_id body = return (stack_id, body)
matchVarStack (param_id:param_ids) stack_id body = do
    (tail_id, tail_code) <- matchVarStack param_ids stack_id body
    pair_id <- newSysLocalMDs (mkCorePairTy (idType param_id) (idType tail_id))
    return (pair_id, coreCasePair pair_id param_id tail_id tail_code)

mkHsEnvStackExpr :: [Id] -> Id -> LHsExpr GhcTc
mkHsEnvStackExpr env_ids stack_id
  = mkLHsTupleExpr [mkLHsVarTuple env_ids noExtField, nlHsVar stack_id]
                   noExtField

-- Translation of arrow abstraction

-- D; xs |-a c : () --> t'      ---> c'
-- --------------------------
-- D |- proc p -> c :: a t t'   ---> premap (\ p -> ((xs),())) c'
--
--              where (xs) is the tuple of variables bound by p

dsProcExpr
        :: LPat GhcTc
        -> LHsCmdTop GhcTc
        -> DsM CoreExpr
dsProcExpr pat (L _ (HsCmdTop (CmdTopTc _unitTy cmd_ty ids) cmd)) = do
    (meth_binds, meth_ids) <- mkCmdEnv ids
    let locals = mkVarSet (collectPatBinders CollWithDictBinders pat)
    (core_cmd, _free_vars, env_ids)
       <- dsfixCmd meth_ids locals unitTy cmd_ty cmd
    let env_ty = mkBigCoreVarTupTy env_ids
    let env_stk_ty = mkCorePairTy env_ty unitTy
    let env_stk_expr = mkCorePairExpr (mkBigCoreVarTup env_ids) mkCoreUnitExpr
    fail_expr <- mkFailExpr (ArrowMatchCtxt ProcExpr) env_stk_ty
    var <- selectSimpleMatchVarL ManyTy pat
    match_code <- matchSimply (Var var) (ArrowMatchCtxt ProcExpr) ManyTy pat env_stk_expr fail_expr
    let pat_ty = hsLPatType pat
    let proc_code = do_premap meth_ids pat_ty env_stk_ty cmd_ty
                    (Lam var match_code)
                    core_cmd
    return (mkLets meth_binds proc_code)

{-
Translation of a command judgement of the form

        D; xs |-a c : stk --> t

to an expression e such that

        D |- e :: a (xs, stk) t
-}

dsLCmd :: DsCmdEnv -> IdSet -> Type -> Type -> LHsCmd GhcTc -> [Id]
       -> DsM (CoreExpr, DIdSet)
dsLCmd ids local_vars stk_ty res_ty cmd env_ids
  = dsCmd ids local_vars stk_ty res_ty (unLoc cmd) env_ids

dsCmd   :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this command
        -> Type                 -- type of the stack (right-nested tuple)
        -> Type                 -- return type of the command
        -> HsCmd GhcTc           -- command to desugar
        -> [Id]           -- list of vars in the input to this command
                                -- This is typically fed back,
                                -- so don't pull on it too early
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet)         -- subset of local vars that occur free

-- D |- fun :: a t1 t2
-- D, xs |- arg :: t1
-- -----------------------------
-- D; xs |-a fun -< arg : stk --> t2
--
--              ---> premap (\ ((xs), _stk) -> arg) fun

dsCmd ids local_vars stack_ty res_ty
        (HsCmdArrApp arrow_ty arrow arg HsFirstOrderApp _)
        env_ids = do
    let
        (a_arg_ty, _res_ty') = tcSplitAppTy arrow_ty
        (_a_ty, arg_ty) = tcSplitAppTy a_arg_ty
    core_arrow <- dsLExpr arrow
    core_arg   <- dsLExpr arg
    stack_id   <- newSysLocalMDs stack_ty
    core_make_arg <- matchEnvStack env_ids stack_id core_arg
    return (do_premap ids
              (envStackType env_ids stack_ty)
              arg_ty
              res_ty
              core_make_arg
              core_arrow,
            exprFreeIdsDSet core_arg `uniqDSetIntersectUniqSet` local_vars)

-- D, xs |- fun :: a t1 t2
-- D, xs |- arg :: t1
-- ------------------------------
-- D; xs |-a fun -<< arg : stk --> t2
--
--              ---> premap (\ ((xs), _stk) -> (fun, arg)) app

dsCmd ids local_vars stack_ty res_ty
        (HsCmdArrApp arrow_ty arrow arg HsHigherOrderApp _)
        env_ids = do
    let
        (a_arg_ty, _res_ty') = tcSplitAppTy arrow_ty
        (_a_ty, arg_ty) = tcSplitAppTy a_arg_ty

    core_arrow <- dsLExpr arrow
    core_arg   <- dsLExpr arg
    stack_id   <- newSysLocalMDs stack_ty
    core_make_pair <- matchEnvStack env_ids stack_id
          (mkCorePairExpr core_arrow core_arg)

    return (do_premap ids
              (envStackType env_ids stack_ty)
              (mkCorePairTy arrow_ty arg_ty)
              res_ty
              core_make_pair
              (do_app ids arg_ty res_ty),
            (exprsFreeIdsDSet [core_arrow, core_arg])
              `uniqDSetIntersectUniqSet` local_vars)

-- D; ys |-a cmd : (t,stk) --> t'
-- D, xs |-  exp :: t
-- ------------------------
-- D; xs |-a cmd exp : stk --> t'
--
--              ---> premap (\ ((xs),stk) -> ((ys),(e,stk))) cmd

dsCmd ids local_vars stack_ty res_ty (HsCmdApp _ cmd arg) env_ids = do
    core_arg <- dsLExpr arg
    let
        arg_ty = exprType core_arg
        stack_ty' = mkCorePairTy arg_ty stack_ty
    (core_cmd, free_vars, env_ids')
             <- dsfixCmd ids local_vars stack_ty' res_ty cmd
    stack_id <- newSysLocalMDs stack_ty
    arg_id <- newSysLocalMDs arg_ty
    -- push the argument expression onto the stack
    let
        stack' = mkCorePairExpr (Var arg_id) (Var stack_id)
        core_body = bindNonRec arg_id core_arg
                        (mkCorePairExpr (mkBigCoreVarTup env_ids') stack')

    -- match the environment and stack against the input
    core_map <- matchEnvStack env_ids stack_id core_body
    return (do_premap ids
                      (envStackType env_ids stack_ty)
                      (envStackType env_ids' stack_ty')
                      res_ty
                      core_map
                      core_cmd,
            free_vars `unionDVarSet`
              (exprFreeIdsDSet core_arg `uniqDSetIntersectUniqSet` local_vars))

dsCmd ids local_vars stack_ty res_ty (HsCmdPar _ cmd) env_ids
  = dsLCmd ids local_vars stack_ty res_ty cmd env_ids

-- D, xs |- e :: Bool
-- D; xs1 |-a c1 : stk --> t
-- D; xs2 |-a c2 : stk --> t
-- ----------------------------------------
-- D; xs |-a if e then c1 else c2 : stk --> t
--
--              ---> premap (\ ((xs),stk) ->
--                       if e then Left ((xs1),stk) else Right ((xs2),stk))
--                     (c1 ||| c2)

dsCmd ids local_vars stack_ty res_ty (HsCmdIf _ mb_fun cond then_cmd else_cmd)
        env_ids = do
    core_cond <- dsLExpr cond
    (core_then, fvs_then, then_ids)
       <- dsfixCmd ids local_vars stack_ty res_ty then_cmd
    (core_else, fvs_else, else_ids)
       <- dsfixCmd ids local_vars stack_ty res_ty else_cmd
    stack_id   <- newSysLocalMDs stack_ty
    either_con <- dsLookupTyCon eitherTyConName
    left_con   <- dsLookupDataCon leftDataConName
    right_con  <- dsLookupDataCon rightDataConName

    let mk_left_expr ty1 ty2 e = mkCoreConApps left_con   [Type ty1,Type ty2, e]
        mk_right_expr ty1 ty2 e = mkCoreConApps right_con [Type ty1,Type ty2, e]

        in_ty = envStackType env_ids stack_ty
        then_ty = envStackType then_ids stack_ty
        else_ty = envStackType else_ids stack_ty
        sum_ty = mkTyConApp either_con [then_ty, else_ty]
        fvs_cond = exprFreeIdsDSet core_cond
                   `uniqDSetIntersectUniqSet` local_vars

        core_left  = mk_left_expr  then_ty else_ty
                       (buildEnvStack then_ids stack_id)
        core_right = mk_right_expr then_ty else_ty
                       (buildEnvStack else_ids stack_id)

    core_if <- case mb_fun of
       NoSyntaxExprTc  -> matchEnvStack env_ids stack_id $
                          mkIfThenElse core_cond core_left core_right
       _ -> do { fun_apps <- dsSyntaxExpr mb_fun
                                      [core_cond, core_left, core_right]
               ; matchEnvStack env_ids stack_id fun_apps }

    return (do_premap ids in_ty sum_ty res_ty
                core_if
                (do_choice ids then_ty else_ty res_ty core_then core_else),
        fvs_cond `unionDVarSet` fvs_then `unionDVarSet` fvs_else)

{-
Note [Desugaring HsCmdCase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Case commands are treated in much the same way as if commands
(see above) except that there are more alternatives.  For example

        case e of { p1 -> c1; p2 -> c2; p3 -> c3 }

is translated to

        premap (\ ((xs)*ts) -> case e of
                p1 -> (Left (Left (xs1)*ts))
                p2 -> Left ((Right (xs2)*ts))
                p3 -> Right ((xs3)*ts))
        ((c1 ||| c2) ||| c3)

The idea is to extract the commands from the case, build a balanced tree
of choices, and replace the commands with expressions that build tagged
tuples, obtaining a case expression that can be desugared normally.
To build all this, we use triples describing segments of the list of
case bodies, containing the following fields:
 * a list of expressions of the form (Left|Right)* ((xs)*ts), to be put
   into the case replacing the commands
 * a sum type that is the common type of these expressions, and also the
   input type of the arrow
 * a CoreExpr for an arrow built by combining the translated command
   bodies with |||.
-}

dsCmd ids local_vars stack_ty res_ty (HsCmdCase _ exp match) env_ids = do
    stack_id <- newSysLocalMDs stack_ty
    (match', core_choices)
      <- dsCases ids local_vars stack_id stack_ty res_ty match
    let MG{ mg_ext = MatchGroupTc _ sum_ty _ } = match'
        in_ty = envStackType env_ids stack_ty

    core_body <- dsExpr (HsCase (ArrowMatchCtxt ArrowCaseAlt) exp match')

    core_matches <- matchEnvStack env_ids stack_id core_body
    return (do_premap ids in_ty sum_ty res_ty core_matches core_choices,
            exprFreeIdsDSet core_body `uniqDSetIntersectUniqSet` local_vars)

{-
\cases and \case are desugared analogously to a case command (see above).
For example

        \cases {p1 q1 -> c1; p2 q2 -> c2; p3 q3 -> c3 }

is translated to

        premap (\ ((xs), (e1, (e2,stk))) -> cases e1 e2 of
                  p1 q1 -> (Left (Left (xs1), stk))
                  p2 q2 -> Left ((Right (xs2), stk))
                  p3 q3 -> Right ((xs3), stk))
        ((c1 ||| c2) ||| c3)

(cases...of is hypothetical notation that works like case...of but with
multiple scrutinees)

-}
dsCmd ids local_vars stack_ty res_ty
        (HsCmdLam _ LamSingle (MG { mg_alts
          = (L _ [L _ (Match { m_pats  = L _ pats
                             , m_grhss = GRHSs _ [L _ (GRHS _ [] body)] _ })]) }))
        env_ids
  = dsCmdLam ids local_vars stack_ty res_ty pats body env_ids

dsCmd ids local_vars stack_ty res_ty
      (HsCmdLam _ lam_variant match@MG { mg_ext = MatchGroupTc {mg_arg_tys = arg_tys} } )
      env_ids = do
    arg_ids <- newSysLocalsDs arg_tys

    let match_ctxt = ArrowLamAlt lam_variant
        pat_vars = mkVarSet arg_ids
        local_vars' = pat_vars `unionVarSet` local_vars
        (pat_tys, stack_ty') = splitTypeAt (length arg_tys) stack_ty

    -- construct and desugar a case expression with multiple scrutinees
    (core_body, free_vars, env_ids') <- trimInput \env_ids -> do
      stack_id <- newSysLocalMDs stack_ty'
      (match', core_choices)
        <- dsCases ids local_vars' stack_id stack_ty' res_ty match

      let MG{ mg_ext = MatchGroupTc _ sum_ty _ } = match'
          in_ty = envStackType env_ids stack_ty'
          discrims = map nlHsVar arg_ids
      (discrim_vars, matching_code)
        <- matchWrapper (ArrowMatchCtxt match_ctxt) (Just discrims) match'
      core_body <- flip (bind_vars discrim_vars) matching_code <$>
        traverse dsLExpr discrims

      core_matches <- matchEnvStack env_ids stack_id core_body
      return (do_premap ids in_ty sum_ty res_ty core_matches core_choices,
              exprFreeIdsDSet core_body `uniqDSetIntersectUniqSet` local_vars')

    param_ids <- newSysLocalsMDs pat_tys
    stack_id' <- newSysLocalMDs stack_ty'

    -- the expression is built from the inside out, so the actions
    -- are presented in reverse order

    let -- build a new environment, plus what's left of the stack
        core_expr = buildEnvStack env_ids' stack_id'
        in_ty = envStackType env_ids stack_ty
        in_ty' = envStackType env_ids' stack_ty'

    -- bind the scrutinees to the parameters
    let match_code = bind_vars arg_ids (map Var param_ids) core_expr

    -- match the parameters against the top of the old stack
    (stack_id, param_code) <- matchVarStack param_ids stack_id' match_code
    -- match the old environment and stack against the input
    select_code <- matchEnvStack env_ids stack_id param_code
    return (do_premap ids in_ty in_ty' res_ty select_code core_body,
            free_vars `uniqDSetMinusUniqSet` pat_vars)
    where
      bind_vars vars exprs expr = foldr (uncurry bindNonRec) expr $ zip vars exprs

-- D; ys |-a cmd : stk --> t
-- ----------------------------------
-- D; xs |-a let binds in cmd : stk --> t
--
--              ---> premap (\ ((xs),stk) -> let binds in ((ys),stk)) c

dsCmd ids local_vars stack_ty res_ty (HsCmdLet _ lbinds@binds body) env_ids = do
    let
        defined_vars = mkVarSet (collectLocalBinders CollWithDictBinders binds)
        local_vars' = defined_vars `unionVarSet` local_vars

    (core_body, _free_vars, env_ids')
       <- dsfixCmd ids local_vars' stack_ty res_ty body
    stack_id <- newSysLocalMDs stack_ty
    -- build a new environment, plus the stack, using the let bindings
    core_binds <- dsLocalBinds lbinds (buildEnvStack env_ids' stack_id)
    -- match the old environment and stack against the input
    core_map <- matchEnvStack env_ids stack_id core_binds
    return (do_premap ids
                        (envStackType env_ids stack_ty)
                        (envStackType env_ids' stack_ty)
                        res_ty
                        core_map
                        core_body,
        exprFreeIdsDSet core_binds `uniqDSetIntersectUniqSet` local_vars)

-- D; xs |-a ss : t
-- ----------------------------------
-- D; xs |-a do { ss } : () --> t
--
--              ---> premap (\ (env,stk) -> env) c

dsCmd ids local_vars stack_ty res_ty (HsCmdDo _ (L _ stmts)) env_ids = do
    (core_stmts, env_ids') <- dsCmdDo ids local_vars res_ty stmts env_ids
    let env_ty = mkBigCoreVarTupTy env_ids
    core_fst <- mkFstExpr env_ty stack_ty
    return (do_premap ids
                (mkCorePairTy env_ty stack_ty)
                env_ty
                res_ty
                core_fst
                core_stmts,
        env_ids')

-- D |- e :: forall e. a1 (e,stk1) t1 -> ... an (e,stkn) tn -> a (e,stk) t
-- D; xs |-a ci :: stki --> ti
-- -----------------------------------
-- D; xs |-a (|e c1 ... cn|) :: stk --> t       ---> e [t_xs] c1 ... cn

dsCmd _ local_vars _stack_ty _res_ty (HsCmdArrForm _ op _ args) env_ids = do
    let env_ty = mkBigCoreVarTupTy env_ids
    core_op <- dsLExpr op
    (core_args, fv_sets) <- mapAndUnzipM (dsTrimCmdArg local_vars env_ids) args
    return (mkApps (App core_op (Type env_ty)) core_args,
            unionDVarSets fv_sets)

dsCmd ids local_vars stack_ty res_ty (XCmd (HsWrap wrap cmd)) env_ids = do
    (core_cmd, env_ids') <- dsCmd ids local_vars stack_ty res_ty cmd env_ids
    dsHsWrapper wrap $ \core_wrap ->
      return (core_wrap core_cmd, env_ids')

-- D; ys |-a c : stk --> t      (ys <= xs)
-- ---------------------
-- D; xs |-a c : stk --> t      ---> premap (\ ((xs),stk) -> ((ys),stk)) c

dsTrimCmdArg
        :: IdSet                -- set of local vars available to this command
        -> [Id]           -- list of vars in the input to this command
        -> LHsCmdTop GhcTc       -- command argument to desugar
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet)         -- subset of local vars that occur free
dsTrimCmdArg local_vars env_ids
                       (L _ (HsCmdTop
                                 (CmdTopTc stack_ty cmd_ty ids) cmd )) = do
    (meth_binds, meth_ids) <- mkCmdEnv ids
    (core_cmd, free_vars, env_ids')
       <- dsfixCmd meth_ids local_vars stack_ty cmd_ty cmd
    stack_id <- newSysLocalMDs stack_ty
    trim_code
      <- matchEnvStack env_ids stack_id (buildEnvStack env_ids' stack_id)
    let
        in_ty = envStackType env_ids stack_ty
        in_ty' = envStackType env_ids' stack_ty
        arg_code = if env_ids' == env_ids then core_cmd else
                do_premap meth_ids in_ty in_ty' cmd_ty trim_code core_cmd
    return (mkLets meth_binds arg_code, free_vars)

-- Given D; xs |-a c : stk --> t, builds c with xs fed back.
-- Typically needs to be prefixed with arr (\(p, stk) -> ((xs),stk))

dsfixCmd
        :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this command
        -> Type                 -- type of the stack (right-nested tuple)
        -> Type                 -- return type of the command
        -> LHsCmd GhcTc         -- command to desugar
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet,         -- subset of local vars that occur free
                [Id])           -- the same local vars as a list, fed back
dsfixCmd ids local_vars stk_ty cmd_ty cmd
  = trimInput (dsLCmd ids local_vars stk_ty cmd_ty cmd)

-- Feed back the list of local variables actually used a command,
-- for use as the input tuple of the generated arrow.

trimInput
        :: ([Id] -> DsM (CoreExpr, DIdSet))
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet,         -- subset of local vars that occur free
                [Id])           -- same local vars as a list, fed back to
                                -- the inner function to form the tuple of
                                -- inputs to the arrow.
trimInput build_arrow
  = fixDs (\ ~(_,_,env_ids) -> do
        (core_cmd, free_vars) <- build_arrow env_ids
        return (core_cmd, free_vars, dVarSetElems free_vars))

-- Desugaring for both HsCmdLam
--
-- D; ys |-a cmd : stk t'
-- -----------------------------------------------
-- D; xs |-a \ p1 ... pk -> cmd : (t1,...(tk,stk)...) t'
--
--              ---> premap (\ ((xs), (p1, ... (pk,stk)...)) -> ((ys),stk)) cmd
dsCmdLam :: DsCmdEnv            -- arrow combinators
         -> IdSet               -- set of local vars available to this command
         -> Type                -- type of the stack (right-nested tuple)
         -> Type                -- return type of the command
         -> [LPat GhcTc]        -- argument patterns to desugar
         -> LHsCmd GhcTc        -- body to desugar
         -> [Id]                -- list of vars in the input to this command
                                -- This is typically fed back,
                                -- so don't pull on it too early
         -> DsM (CoreExpr,      -- desugared expression
                 DIdSet)        -- subset of local vars that occur free
dsCmdLam ids local_vars stack_ty res_ty pats body env_ids = do
    let pat_vars = mkVarSet (collectPatsBinders CollWithDictBinders pats)
    let local_vars' = pat_vars `unionVarSet` local_vars
        (pat_tys, stack_ty') = splitTypeAt (length pats) stack_ty
    (core_body, free_vars, env_ids')
       <- dsfixCmd ids local_vars' stack_ty' res_ty body
    param_ids <- newSysLocalsMDs pat_tys
    stack_id' <- newSysLocalMDs stack_ty'

    -- the expression is built from the inside out, so the actions
    -- are presented in reverse order

    let -- build a new environment, plus what's left of the stack
        core_expr = buildEnvStack env_ids' stack_id'
        in_ty = envStackType env_ids stack_ty
        in_ty' = envStackType env_ids' stack_ty'
        lam_cxt = ArrowMatchCtxt (ArrowLamAlt LamSingle)

    fail_expr <- mkFailExpr lam_cxt in_ty'
    -- match the patterns against the parameters
    match_code <- matchSimplys (map Var param_ids) lam_cxt pats core_expr
                    fail_expr
    -- match the parameters against the top of the old stack
    (stack_id, param_code) <- matchVarStack param_ids stack_id' match_code
    -- match the old environment and stack against the input
    select_code <- matchEnvStack env_ids stack_id param_code
    return (do_premap ids in_ty in_ty' res_ty select_code core_body,
            free_vars `uniqDSetMinusUniqSet` pat_vars)

-- Used for case and \case(s)
-- See Note [Desugaring HsCmdCase]
dsCases :: DsCmdEnv                               -- arrow combinators
        -> IdSet                                  -- set of local vars available to this command
        -> Id                                     -- stack id
        -> Type                                   -- type of the stack (right-nested tuple)
        -> Type                                   -- return type of the command
        -> MatchGroup GhcTc (LHsCmd GhcTc)        -- match group to desugar
        -> DsM (MatchGroup GhcTc (LHsExpr GhcTc), -- match group with choice tree
                CoreExpr)                         -- desugared choices
dsCases ids local_vars stack_id stack_ty res_ty
        (MG { mg_alts = L l matches
            , mg_ext = MatchGroupTc arg_tys _ origin
            }) = do

  -- Extract and desugar the leaf commands in the case, building tuple
  -- expressions that will (after tagging) replace these leaves

  let leaves = concatMap leavesMatch matches
      make_branch (leaf, bound_vars) = do
          (core_leaf, _fvs, leaf_ids)
             <- dsfixCmd ids (bound_vars `unionVarSet` local_vars) stack_ty
                  res_ty leaf
          return ([mkHsEnvStackExpr leaf_ids stack_id],
                  envStackType leaf_ids stack_ty,
                  core_leaf)

  branches <- mapM make_branch leaves
  either_con <- dsLookupTyCon eitherTyConName
  left_con <- dsLookupDataCon leftDataConName
  right_con <- dsLookupDataCon rightDataConName
  void_ty <- mkTyConTy <$> dsLookupTyCon voidTyConName
  let
      left_id  = mkConLikeTc (RealDataCon left_con)
      right_id = mkConLikeTc (RealDataCon right_con)
      left_expr  ty1 ty2 e = noLocA $ HsApp noExtField
                         (noLocA $ mkHsWrap (mkWpTyApps [ty1, ty2]) left_id ) e
      right_expr ty1 ty2 e = noLocA $ HsApp noExtField
                         (noLocA $ mkHsWrap (mkWpTyApps [ty1, ty2]) right_id) e

      -- Prefix each tuple with a distinct series of Left's and Right's,
      -- in a balanced way, keeping track of the types.

      merge_branches :: ([LHsExpr GhcTc], Type, CoreExpr)
                    -> ([LHsExpr GhcTc], Type, CoreExpr)
                    -> ([LHsExpr GhcTc], Type, CoreExpr) -- AZ
      merge_branches (builds1, in_ty1, core_exp1)
                     (builds2, in_ty2, core_exp2)
        = (map (left_expr in_ty1 in_ty2) builds1 ++
              map (right_expr in_ty1 in_ty2) builds2,
           mkTyConApp either_con [in_ty1, in_ty2],
           do_choice ids in_ty1 in_ty2 res_ty core_exp1 core_exp2)
  (leaves', sum_ty, core_choices) <- case nonEmpty branches of
    Just bs -> return $ foldb merge_branches bs
    -- when the case command has no alternatives, the sum type from
    -- Note [Desugaring HsCmdCase] becomes the empty sum type,
    -- i.e. Void. The choices then effectively become `arr absurd`,
    -- implemented as `arr \case {}`.
    Nothing -> ([], void_ty,) . do_arr ids void_ty res_ty <$>
      dsExpr (HsLam noAnn LamCase
        (MG { mg_alts = noLocA []
            , mg_ext = MatchGroupTc [Scaled ManyTy void_ty] res_ty (Generated OtherExpansion SkipPmc)
            }))

      -- Replace the commands in the case with these tagged tuples,
      -- yielding a HsExpr Id we can feed to dsExpr.

  let (_, matches') = mapAccumL (replaceLeavesMatch res_ty) leaves' matches

  -- Note that we replace the MatchGroup result type by sum_ty,
  -- which is the type of matches'
  return (MG { mg_alts = L l matches'
             , mg_ext = MatchGroupTc arg_tys sum_ty origin
             },
          core_choices)

{-
Translation of command judgements of the form

        D |-a do { ss } : t
-}

dsCmdDo :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this statement
        -> Type                 -- return type of the statement
        -> [CmdLStmt GhcTc]     -- statements to desugar
        -> [Id]                 -- list of vars in the input to this statement
                                -- This is typically fed back,
                                -- so don't pull on it too early
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet)         -- subset of local vars that occur free

dsCmdDo _ _ _ [] _ = panic "dsCmdDo"

-- D; xs |-a c : () --> t
-- --------------------------
-- D; xs |-a do { c } : t
--
--              ---> premap (\ (xs) -> ((xs), ())) c

dsCmdDo ids local_vars res_ty [L _ (LastStmt _ body _ _)] env_ids = do
    (core_body, env_ids') <- dsLCmd ids local_vars unitTy res_ty body env_ids
    let env_ty = mkBigCoreVarTupTy env_ids
    env_var <- newSysLocalMDs env_ty
    let core_map = Lam env_var (mkCorePairExpr (Var env_var) mkCoreUnitExpr)
    return (do_premap ids
                        env_ty
                        (mkCorePairTy env_ty unitTy)
                        res_ty
                        core_map
                        core_body,
        env_ids')

dsCmdDo ids local_vars res_ty (stmt:stmts) env_ids = do
    let bound_vars  = mkVarSet (collectLStmtBinders CollWithDictBinders stmt)
    let local_vars' = bound_vars `unionVarSet` local_vars
    (core_stmts, _, env_ids') <- trimInput (dsCmdDo ids local_vars' res_ty stmts)
    (core_stmt, fv_stmt) <- dsCmdLStmt ids local_vars env_ids' stmt env_ids
    return (do_compose ids
                (mkBigCoreVarTupTy env_ids)
                (mkBigCoreVarTupTy env_ids')
                res_ty
                core_stmt
                core_stmts,
              fv_stmt)

{-
A statement maps one local environment to another, and is represented
as an arrow from one tuple type to another.  A statement sequence is
translated to a composition of such arrows.
-}

dsCmdLStmt :: DsCmdEnv -> IdSet -> [Id] -> CmdLStmt GhcTc -> [Id]
           -> DsM (CoreExpr, DIdSet)
dsCmdLStmt ids local_vars out_ids cmd env_ids
  = dsCmdStmt ids local_vars out_ids (unLoc cmd) env_ids

dsCmdStmt
        :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this statement
        -> [Id]                 -- list of vars in the output of this statement
        -> CmdStmt GhcTc        -- statement to desugar
        -> [Id]                 -- list of vars in the input to this statement
                                -- This is typically fed back,
                                -- so don't pull on it too early
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet)         -- subset of local vars that occur free

-- D; xs1 |-a c : () --> t
-- D; xs' |-a do { ss } : t'
-- ------------------------------
-- D; xs  |-a do { c; ss } : t'
--
--              ---> premap (\ ((xs)) -> (((xs1),()),(xs')))
--                      (first c >>> arr snd) >>> ss

dsCmdStmt ids local_vars out_ids (BodyStmt c_ty cmd _ _) env_ids = do
    (core_cmd, fv_cmd, env_ids1) <- dsfixCmd ids local_vars unitTy c_ty cmd
    core_mux <- matchEnv env_ids
        (mkCorePairExpr
            (mkCorePairExpr (mkBigCoreVarTup env_ids1) mkCoreUnitExpr)
            (mkBigCoreVarTup out_ids))
    let
        in_ty = mkBigCoreVarTupTy env_ids
        in_ty1 = mkCorePairTy (mkBigCoreVarTupTy env_ids1) unitTy
        out_ty = mkBigCoreVarTupTy out_ids
        before_c_ty = mkCorePairTy in_ty1 out_ty
        after_c_ty = mkCorePairTy c_ty out_ty
    snd_fn <- mkSndExpr c_ty out_ty
    return (do_premap ids in_ty before_c_ty out_ty core_mux $
                do_compose ids before_c_ty after_c_ty out_ty
                        (do_first ids in_ty1 c_ty out_ty core_cmd) $
                do_arr ids after_c_ty out_ty snd_fn,
              extendDVarSetList fv_cmd out_ids)

-- D; xs1 |-a c : () --> t
-- D; xs' |-a do { ss } : t'            xs2 = xs' - defs(p)
-- -----------------------------------
-- D; xs  |-a do { p <- c; ss } : t'
--
--              ---> premap (\ (xs) -> (((xs1),()),(xs2)))
--                      (first c >>> arr (\ (p, (xs2)) -> (xs'))) >>> ss
--
-- It would be simpler and more consistent to do this using second,
-- but that's likely to be defined in terms of first.

dsCmdStmt ids local_vars out_ids (BindStmt _ pat cmd) env_ids = do
    let pat_ty = hsLPatType pat
    (core_cmd, fv_cmd, env_ids1) <- dsfixCmd ids local_vars unitTy pat_ty cmd
    let pat_vars = mkVarSet (collectPatBinders CollWithDictBinders pat)
    let
        env_ids2 = filterOut (`elemVarSet` pat_vars) out_ids
        env_ty2 = mkBigCoreVarTupTy env_ids2

    -- multiplexing function
    --          \ (xs) -> (((xs1),()),(xs2))

    core_mux <- matchEnv env_ids
        (mkCorePairExpr
            (mkCorePairExpr (mkBigCoreVarTup env_ids1) mkCoreUnitExpr)
            (mkBigCoreVarTup env_ids2))

    -- projection function
    --          \ (p, (xs2)) -> (zs)

    env_id <- newSysLocalMDs env_ty2
    let
       after_c_ty = mkCorePairTy pat_ty env_ty2
       out_ty = mkBigCoreVarTupTy out_ids
    body_expr <- coreCaseTuple env_id env_ids2 (mkBigCoreVarTup out_ids)

    fail_expr <- mkFailExpr (StmtCtxt (HsDoStmt (DoExpr Nothing))) out_ty
    pat_id    <- selectSimpleMatchVarL ManyTy pat
    match_code
      <- matchSimply (Var pat_id) (StmtCtxt (HsDoStmt (DoExpr Nothing))) ManyTy pat body_expr fail_expr
    pair_id   <- newSysLocalMDs after_c_ty
    let
        proj_expr = Lam pair_id (coreCasePair pair_id pat_id env_id match_code)

    -- put it all together
    let
        in_ty = mkBigCoreVarTupTy env_ids
        in_ty1 = mkCorePairTy (mkBigCoreVarTupTy env_ids1) unitTy
        in_ty2 = mkBigCoreVarTupTy env_ids2
        before_c_ty = mkCorePairTy in_ty1 in_ty2
    return (do_premap ids in_ty before_c_ty out_ty core_mux $
                do_compose ids before_c_ty after_c_ty out_ty
                        (do_first ids in_ty1 pat_ty in_ty2 core_cmd) $
                do_arr ids after_c_ty out_ty proj_expr,
              fv_cmd `unionDVarSet` (mkDVarSet out_ids
                                     `uniqDSetMinusUniqSet` pat_vars))

-- D; xs' |-a do { ss } : t
-- --------------------------------------
-- D; xs  |-a do { let binds; ss } : t
--
--              ---> arr (\ (xs) -> let binds in (xs')) >>> ss

dsCmdStmt ids local_vars out_ids (LetStmt _ binds) env_ids = do
    -- build a new environment using the let bindings
    core_binds <- dsLocalBinds binds (mkBigCoreVarTup out_ids)
    -- match the old environment against the input
    core_map <- matchEnv env_ids core_binds
    return (do_arr ids
                        (mkBigCoreVarTupTy env_ids)
                        (mkBigCoreVarTupTy out_ids)
                        core_map,
            exprFreeIdsDSet core_binds `uniqDSetIntersectUniqSet` local_vars)

-- D; ys  |-a do { ss; returnA -< ((xs1), (ys2)) } : ...
-- D; xs' |-a do { ss' } : t
-- ------------------------------------
-- D; xs  |-a do { rec ss; ss' } : t
--
--                      xs1 = xs' /\ defs(ss)
--                      xs2 = xs' - defs(ss)
--                      ys1 = ys - defs(ss)
--                      ys2 = ys /\ defs(ss)
--
--              ---> arr (\(xs) -> ((ys1),(xs2))) >>>
--                      first (loop (arr (\((ys1),~(ys2)) -> (ys)) >>> ss)) >>>
--                      arr (\((xs1),(xs2)) -> (xs')) >>> ss'

dsCmdStmt ids local_vars out_ids
        (RecStmt { recS_stmts = L _ stmts
                 , recS_later_ids = later_ids, recS_rec_ids = rec_ids
                 , recS_ext = RecStmtTc { recS_later_rets = later_rets
                                        , recS_rec_rets = rec_rets } })
        env_ids = do
    let
        later_ids_set = mkVarSet later_ids
        env2_ids = filterOut (`elemVarSet` later_ids_set) out_ids
        env2_id_set = mkDVarSet env2_ids
        env2_ty = mkBigCoreVarTupTy env2_ids

    -- post_loop_fn = \((later_ids),(env2_ids)) -> (out_ids)

    env2_id <- newSysLocalMDs env2_ty
    let
        later_ty = mkBigCoreVarTupTy later_ids
        post_pair_ty = mkCorePairTy later_ty env2_ty
    post_loop_body <- coreCaseTuple env2_id env2_ids (mkBigCoreVarTup out_ids)

    post_loop_fn <- matchEnvStack later_ids env2_id post_loop_body

    --- loop (...)

    (core_loop, env1_id_set, env1_ids)
               <- dsRecCmd ids local_vars stmts later_ids later_rets rec_ids rec_rets

    -- pre_loop_fn = \(env_ids) -> ((env1_ids),(env2_ids))

    let
        env1_ty = mkBigCoreVarTupTy env1_ids
        pre_pair_ty = mkCorePairTy env1_ty env2_ty
        pre_loop_body = mkCorePairExpr (mkBigCoreVarTup env1_ids)
                                        (mkBigCoreVarTup env2_ids)

    pre_loop_fn <- matchEnv env_ids pre_loop_body

    -- arr pre_loop_fn >>> first (loop (...)) >>> arr post_loop_fn

    let
        env_ty = mkBigCoreVarTupTy env_ids
        out_ty = mkBigCoreVarTupTy out_ids
        core_body = do_premap ids env_ty pre_pair_ty out_ty
                pre_loop_fn
                (do_compose ids pre_pair_ty post_pair_ty out_ty
                        (do_first ids env1_ty later_ty env2_ty
                                core_loop)
                        (do_arr ids post_pair_ty out_ty
                                post_loop_fn))

    return (core_body, env1_id_set `unionDVarSet` env2_id_set)

dsCmdStmt _ _ _ _ s = pprPanic "dsCmdStmt" (ppr s)

--      loop (premap (\ ((env1_ids), ~(rec_ids)) -> (env_ids))
--            (ss >>> arr (\ (out_ids) -> ((later_rets),(rec_rets))))) >>>

dsRecCmd
        :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this statement
        -> [CmdLStmt GhcTc]     -- list of statements inside the RecCmd
        -> [Id]                 -- list of vars defined here and used later
        -> [HsExpr GhcTc]       -- expressions corresponding to later_ids
        -> [Id]                 -- list of vars fed back through the loop
        -> [HsExpr GhcTc]       -- expressions corresponding to rec_ids
        -> DsM (CoreExpr,       -- desugared statement
                DIdSet,         -- subset of local vars that occur free
                [Id])           -- same local vars as a list

dsRecCmd ids local_vars stmts later_ids later_rets rec_ids rec_rets = do
    let
        later_id_set = mkVarSet later_ids
        rec_id_set = mkVarSet rec_ids
        local_vars' = rec_id_set `unionVarSet` later_id_set `unionVarSet` local_vars

    -- mk_pair_fn = \ (out_ids) -> ((later_rets),(rec_rets))

    core_later_rets <- mapM dsExpr later_rets
    core_rec_rets <- mapM dsExpr rec_rets
    let
        -- possibly polymorphic version of vars of later_ids and rec_ids
        out_ids = exprsFreeIdsList (core_later_rets ++ core_rec_rets)
        out_ty = mkBigCoreVarTupTy out_ids

        later_tuple = mkBigCoreTup core_later_rets
        later_ty = mkBigCoreVarTupTy later_ids

        rec_tuple = mkBigCoreTup core_rec_rets
        rec_ty = mkBigCoreVarTupTy rec_ids

        out_pair = mkCorePairExpr later_tuple rec_tuple
        out_pair_ty = mkCorePairTy later_ty rec_ty

    mk_pair_fn <- matchEnv out_ids out_pair

    -- ss

    (core_stmts, fv_stmts, env_ids) <- dsfixCmdStmts ids local_vars' out_ids stmts

    -- squash_pair_fn = \ ((env1_ids), ~(rec_ids)) -> (env_ids)

    rec_id <- newSysLocalMDs rec_ty
    let
        env1_id_set = fv_stmts `uniqDSetMinusUniqSet` rec_id_set
        env1_ids = dVarSetElems env1_id_set
        env1_ty = mkBigCoreVarTupTy env1_ids
        in_pair_ty = mkCorePairTy env1_ty rec_ty
        core_body = mkBigCoreTup (map selectVar env_ids)
          where
            selectVar v
                | v `elemVarSet` rec_id_set
                  = mkBigTupleSelector rec_ids v rec_id (Var rec_id)
                | otherwise = Var v

    squash_pair_fn <- matchEnvStack env1_ids rec_id core_body

    -- loop (premap squash_pair_fn (ss >>> arr mk_pair_fn))

    let
        env_ty = mkBigCoreVarTupTy env_ids
        core_loop = do_loop ids env1_ty later_ty rec_ty
                (do_premap ids in_pair_ty env_ty out_pair_ty
                        squash_pair_fn
                        (do_compose ids env_ty out_ty out_pair_ty
                                core_stmts
                                (do_arr ids out_ty out_pair_ty mk_pair_fn)))

    return (core_loop, env1_id_set, env1_ids)

{-
A sequence of statements (as in a rec) is desugared to an arrow between
two environments (no stack)
-}

dsfixCmdStmts
        :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this statement
        -> [Id]                 -- output vars of these statements
        -> [CmdLStmt GhcTc]     -- statements to desugar
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet,         -- subset of local vars that occur free
                [Id])           -- same local vars as a list

dsfixCmdStmts ids local_vars out_ids stmts
  = trimInput (dsCmdStmts ids local_vars out_ids stmts)
   -- TODO: Add representation polymorphism check for the resulting expression.
   -- But I (Richard E.) don't know enough about arrows to do so.

dsCmdStmts
        :: DsCmdEnv             -- arrow combinators
        -> IdSet                -- set of local vars available to this statement
        -> [Id]                 -- output vars of these statements
        -> [CmdLStmt GhcTc]     -- statements to desugar
        -> [Id]                 -- list of vars in the input to these statements
        -> DsM (CoreExpr,       -- desugared expression
                DIdSet)         -- subset of local vars that occur free

dsCmdStmts ids local_vars out_ids [stmt] env_ids
  = dsCmdLStmt ids local_vars out_ids stmt env_ids

dsCmdStmts ids local_vars out_ids (stmt:stmts) env_ids = do
    let bound_vars  = mkVarSet (collectLStmtBinders CollWithDictBinders stmt)
    let local_vars' = bound_vars `unionVarSet` local_vars
    (core_stmts, _fv_stmts, env_ids') <- dsfixCmdStmts ids local_vars' out_ids stmts
    (core_stmt, fv_stmt) <- dsCmdLStmt ids local_vars env_ids' stmt env_ids
    return (do_compose ids
                (mkBigCoreVarTupTy env_ids)
                (mkBigCoreVarTupTy env_ids')
                (mkBigCoreVarTupTy out_ids)
                core_stmt
                core_stmts,
              fv_stmt)

dsCmdStmts _ _ _ [] _ = panic "dsCmdStmts []"

-- Match a list of expressions against a list of patterns, left-to-right.

matchSimplys :: [CoreExpr]              -- Scrutinees
             -> HsMatchContextRn        -- Match kind
             -> [LPat GhcTc]            -- Patterns they should match
             -> CoreExpr                -- Return this if they all match
             -> CoreExpr                -- Return this if they don't
             -> DsM CoreExpr
matchSimplys [] _ctxt [] result_expr _fail_expr = return result_expr
matchSimplys (exp:exps) ctxt (pat:pats) result_expr fail_expr = do
    match_code <- matchSimplys exps ctxt pats result_expr fail_expr
    matchSimply exp ctxt ManyTy pat match_code fail_expr
matchSimplys _ _ _ _ _ = panic "matchSimplys"

-- List of leaf expressions, with set of variables bound in each

leavesMatch :: LMatch GhcTc (LocatedA (body GhcTc))
            -> [(LocatedA (body GhcTc), IdSet)]
leavesMatch (L _ (Match { m_pats = L _ pats
                        , m_grhss = GRHSs _ grhss binds }))
  = let
        defined_vars = mkVarSet (collectPatsBinders CollWithDictBinders pats)
                        `unionVarSet`
                       mkVarSet (collectLocalBinders CollWithDictBinders binds)
    in
    [(body,
      mkVarSet (collectLStmtsBinders CollWithDictBinders stmts)
        `unionVarSet` defined_vars)
    | L _ (GRHS _ stmts body) <- grhss]

-- Replace the leaf commands in a match

replaceLeavesMatch
        :: ( Anno (Match GhcTc (LocatedA (body' GhcTc))) ~ Anno (Match GhcTc (LocatedA (body GhcTc)))
           , Anno (GRHS GhcTc (LocatedA (body' GhcTc))) ~ Anno (GRHS GhcTc (LocatedA (body GhcTc))))
        => Type                                 -- new result type
        -> [LocatedA (body' GhcTc)] -- replacement leaf expressions of that type
        -> LMatch GhcTc (LocatedA (body GhcTc))  -- the matches of a case command
        -> ([LocatedA (body' GhcTc)],            -- remaining leaf expressions
            LMatch GhcTc (LocatedA (body' GhcTc))) -- updated match
replaceLeavesMatch _res_ty leaves
                        (L loc
                          match@(Match { m_grhss = GRHSs x grhss binds }))
  = let
        (leaves', grhss') = mapAccumL replaceLeavesGRHS leaves grhss
    in
    (leaves', L loc (match { m_ext = noAnn, m_grhss = GRHSs x grhss' binds }))

replaceLeavesGRHS
        :: ( Anno (Match GhcTc (LocatedA (body' GhcTc))) ~ Anno (Match GhcTc (LocatedA (body GhcTc)))
           , Anno (GRHS GhcTc (LocatedA (body' GhcTc))) ~ Anno (GRHS GhcTc (LocatedA (body GhcTc))))
        => [LocatedA (body' GhcTc)]  -- replacement leaf expressions of that type
        -> LGRHS GhcTc (LocatedA (body GhcTc))     -- rhss of a case command
        -> ([LocatedA (body' GhcTc)],              -- remaining leaf expressions
            LGRHS GhcTc (LocatedA (body' GhcTc)))  -- updated GRHS
replaceLeavesGRHS (leaf:leaves) (L loc (GRHS x stmts _))
  = (leaves, L loc (GRHS x stmts leaf))
replaceLeavesGRHS [] _ = panic "replaceLeavesGRHS []"

-- Balanced fold of a non-empty list.

foldb :: (a -> a -> a) -> NonEmpty a -> a
foldb _ (x:|[]) = x
foldb f xs = foldb f (fold_pairs xs)
  where
    fold_pairs (x1:|x2:xs) = f x1 x2 :| keep_empty fold_pairs xs
    fold_pairs xs          = xs

    keep_empty :: (NonEmpty a -> NonEmpty a) -> [a] -> [a]
    keep_empty f = maybe [] (toList . f) . nonEmpty
