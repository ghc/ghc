module GHC.Core.OpenWhnf
  ( EvalFailure(..)
  , Whnf(..)
  , Neutral(..)
  , Suspension
  , CaseContinuation(..)
  , openWhnfM, openWhnfWithLocalM, openWhnfTraceM, CaseStep(..)
  ) where

import GHC.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import GHC.Builtin.WiredIn.Types (charDataCon, tupleDataCon, unboxedUnitDataCon, unboxedUnitTy)
import GHC.Core.DataCon (dataConWorkId)
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreAlt, CoreExpr, Expr(..), isTyCoArg)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, substExpr)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type, isLiftedTypeKind, tyCoVarsOfType, typeKind)
import GHC.Core.Utils (exprType, findAlt)
import GHC.Types.Basic (Boxity(Boxed), IntWithInf, intGtLimit)
import GHC.Types.Literal (Literal(LitChar))
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.Var (Var, isId, isTyVar, varType)
import GHC.Types.Var.Env (VarEnv, emptyVarEnv, extendVarEnv, extendVarEnvList, lookupVarEnv,
                          mkInScopeSet)
import GHC.Types.Var.Set (VarSet, elemVarSet, unionVarSet)

-- | A bounded observation could not produce weak head form.
data EvalFailure
  = Exhausted
  | UnsupportedCore
  | MissingAlternative
  deriving (Eq, Show)

type Env = VarEnv Suspension

-- | Save the expression and the environment at its point of origin.
data Suspension = Suspension Env CoreExpr

-- | Preserve a case and its environment while its scrutinee is unknown.
data CaseContinuation = CaseContinuation Env Var Type [CoreAlt]

-- | A weak head observation of a typed expression.
data Whnf
  = Lambda Var Suspension
  | Literal Literal
  | BoxedChar Char
  | BoxedPair Suspension Suspension
  | Neutral Neutral

-- | An unknown head and its suspended eliminations.
data Neutral
  = Rigid Var
  | Unsolved Var
  | Applied Neutral Suspension
  | CaseBlocked Neutral CaseContinuation

-- | A known elimination performed on the observation path.
-- The caller must supply typed Core. A step is not a Core coercion.
data CaseStep
  = SelectBoxedChar Char AltCon
  | SelectBoxedPair AltCon
  | SelectLiteral Literal AltCon
  | SelectLambdaDefault
  | ContinueWithUnit
  deriving (Eq)

-- | Observe weak head form with a demand-driven definition provider.
-- See Note [Observing open Core].
openWhnfM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> VarSet -> CoreExpr
          -> m (Either EvalFailure Whnf)
openWhnfM limit obtain = openWhnfWithLocalM limit obtain (const (pure ()))

-- | Report a lexical variable when its suspended binding is demanded.
openWhnfWithLocalM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> (Var -> m ())
                  -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfWithLocalM limit obtain local =
  openWhnfTraceWithLocalM limit obtain local (const (pure ()))

-- | Observe an expression and report case selections and the unboxed unit
-- continuation step in the order in which the observer performs them.
openWhnfTraceM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr))
              -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceM limit obtain = openWhnfTraceWithLocalM limit obtain (const (pure ()))

openWhnfTraceWithLocalM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> (Var -> m ())
                       -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceWithLocalM limit obtain local record unsolved expr =
  runExceptT $ evalStateT (evaluate limit obtain local record unsolved (Suspension emptyVarEnv expr)) 1

evaluate :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> (Var -> m ())
         -> (CaseStep -> m ()) -> VarSet -> Suspension -> StateT Int (ExceptT EvalFailure m) Whnf
evaluate limit obtain local record unsolved (Suspension env expr) = do
  steps <- get
  if intGtLimit steps limit then lift (throwE Exhausted) else put (steps + 1)
  case expr of
    Var v
      | Just binding <- lookupVarEnv env v -> do
          lift (lift (local v))
          recur binding
      | otherwise -> do
          definition <- lift (lift (obtain v))
          case definition of
            Just rhs -> recur (Suspension emptyVarEnv rhs)
            Nothing
              | v `elemVarSet` unsolved -> pure (Neutral (Unsolved v))
              | otherwise -> pure (Neutral (Rigid v))
    Tick Breakpoint{} body -> recur (Suspension env body)
    Lit lit -> pure (Literal lit)
    Lam binder body -> pure (Lambda binder (Suspension env body))
    App (Var worker) arg
      | worker == dataConWorkId charDataCon -> do
          payload <- recur (Suspension env arg)
          case payload of
            Literal (LitChar c) -> pure (BoxedChar c)
            _ -> lift (throwE UnsupportedCore)
    App (App (App (App (Var worker) (Type a)) (Type b)) x) y
      | worker == pairWorker
      , not (isTyCoArg x), not (isTyCoArg y)
      , isLiftedTypeKind (typeKind a), isLiftedTypeKind (typeKind b)
      , exprType x `eqType` a, exprType y `eqType` b ->
          pure (BoxedPair (Suspension env x) (Suspension env y))
    App fun arg -> do
      headValue <- recur (Suspension env fun)
      case headValue of
        Lambda binder (Suspension closure body)
          | Type argTy <- arg, isTyVar binder ->
              let inScope = mkInScopeSet (exprFreeVars body `unionVarSet` tyCoVarsOfType argTy)
                  subst = extendTvSubst (mkEmptySubst inScope) binder argTy
              in recur (Suspension closure (substExpr subst body))
          | liftedBinder binder ->
              recur (Suspension (extendVarEnv closure binder (Suspension env arg)) body)
          | isId binder
          , varType binder `eqType` unboxedUnitTy
          , Var worker <- arg
          , worker == dataConWorkId unboxedUnitDataCon
          , not (binder `elemVarSet` exprFreeVars body) -> do
              -- See Note [Observing open Core].
              lift (lift (record ContinueWithUnit))
              recur (Suspension closure body)
        Neutral neutral -> pure (Neutral (Applied neutral (Suspension env arg)))
        _ -> lift (throwE UnsupportedCore)
    Let (NonRec binder rhs) body
      | liftedBinder binder ->
          recur (Suspension (extendVarEnv env binder (Suspension env rhs)) body)
    Let (Rec bindings) body
      | all (liftedBinder . fst) bindings ->
          let recEnv = extendVarEnvList env [(binder, Suspension recEnv rhs) | (binder, rhs) <- bindings]
          in recur (Suspension recEnv body)
    Case scrutinee binder resultType alts -> do
      let suspended = Suspension env scrutinee
      headValue <- recur suspended
      case headValue of
        Neutral neutral ->
          pure (Neutral (CaseBlocked neutral (CaseContinuation env binder resultType alts)))
        BoxedChar c
          | Just (Alt (DataAlt con) [field] body) <- findAlt (DataAlt charDataCon) alts
          , con == charDataCon -> do
              lift (lift (record (SelectBoxedChar c (DataAlt con))))
              let payload = Suspension emptyVarEnv (Lit (LitChar c))
                  bodyEnv = extendVarEnvList env [(binder, suspended), (field, payload)]
              recur (Suspension bodyEnv body)
          | Just (Alt DEFAULT [] body) <- findAlt (DataAlt charDataCon) alts -> do
              lift (lift (record (SelectBoxedChar c DEFAULT)))
              recur (Suspension (extendVarEnv env binder suspended) body)
          | otherwise -> lift (throwE MissingAlternative)
        BoxedPair first second
          | Just (Alt key fields body) <- findAlt (DataAlt pairCon) alts
          , Just fieldsEnv <- pairFields key fields first second -> do
              lift (lift (record (SelectBoxedPair key)))
              recur (Suspension (extendVarEnvList env ((binder, suspended) : fieldsEnv)) body)
          | otherwise -> lift (throwE MissingAlternative)
        Literal lit
          | Just (Alt key [] body) <- findAlt (LitAlt lit) alts -> do
              lift (lift (record (SelectLiteral lit key)))
              let bodyEnv = extendVarEnv env binder (Suspension emptyVarEnv (Lit lit))
              recur (Suspension bodyEnv body)
          | otherwise -> lift (throwE MissingAlternative)
        Lambda{}
          | Just (Alt DEFAULT [] body) <- findAlt DEFAULT alts -> do
              lift (lift (record SelectLambdaDefault))
              recur (Suspension (extendVarEnv env binder suspended) body)
        _ -> lift (throwE UnsupportedCore)
    _ -> lift (throwE UnsupportedCore)
  where
    recur = evaluate limit obtain local record unsolved
    pairCon = tupleDataCon Boxed 2
    pairWorker = dataConWorkId pairCon
    pairFields (DataAlt con) [x,y] first second
      | con == pairCon, matches x first, matches y second =
          Just [(x, first), (y, second)]
    pairFields DEFAULT [] _ _ = Just []
    pairFields _ _ _ _ = Nothing
    matches field (Suspension _ body) =
      liftedBinder field && varType field `eqType` exprType body

-- | Only lifted term binders can enter the lazy environment.
liftedBinder :: Var -> Bool
liftedBinder binder = isId binder && isLiftedTypeKind (typeKind (varType binder))

{- Note [Observing open Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Save each operand with its lexical environment when forming an application.
Extend the function's saved environment when entering its body. This keeps
an ignored lifted operand lazy. Use Core's capture-avoiding substitution
for type arguments.

A saturated boxed pair is already in weak head normal form. Preserve both
fields as independent suspensions. A pair alternative binds the fields
without forcing either one. A neutral scrutinee still blocks case
selection. See the ordinary fst and snd definitions in
GHC.Internal.Data.Tuple.

A case must inspect its scrutinee even when it has only a DEFAULT branch.
If the scrutinee is neutral, retain the alternatives, the binder and their
environment. A rigid input stays neutral. An unsolved input can be observed
again with a new definition. This distinction follows Agda's weak-head
reducer and its blockers. See Note [Open source calls] in GHC.Tc.Gen.OpenTypeEval.

GHCi inserts Breakpoint ticks into ordinary imported function bodies.
They identify positions for the runtime debugger. When observing a type,
inspect the expression beneath the tick. The interpreter keeps the
original annotated body for runtime execution.

GHC.HsToCore.Utils.mkFailurePair passes the wired-in nullary unboxed
unit constructor directly to a failure continuation. The observer can
enter this continuation if the binder has exactly the unboxed unit type
and does not occur free in the body. The constructor has no fields to
evaluate. An expression of the same type can still require evaluation,
so the observer does not drop any other unlifted operand.
-}
