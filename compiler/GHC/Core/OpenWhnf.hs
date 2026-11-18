module GHC.Core.OpenWhnf
  ( EvalFailure(..)
  , Whnf(..)
  , Neutral(..)
  , Suspension
  , CaseContinuation(..)
  , openWhnfM
  ) where

import GHC.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreAlt, CoreExpr, Expr(..))
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, substExpr)
import GHC.Core.Type (Type, isLiftedTypeKind, tyCoVarsOfType, typeKind)
import GHC.Core.Utils (findAlt)
import GHC.Types.Basic (IntWithInf, intGtLimit)
import GHC.Types.Literal (Literal)
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
  | Neutral Neutral

-- | An unknown head and its suspended eliminations.
data Neutral
  = Rigid Var
  | Unsolved Var
  | Applied Neutral Suspension
  | CaseBlocked Neutral CaseContinuation

-- | Observe weak head form with a demand-driven definition provider.
-- See Note [Observing open Core].
openWhnfM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> VarSet -> CoreExpr
          -> m (Either EvalFailure Whnf)
openWhnfM limit obtain unsolved expr =
  runExceptT $ evalStateT (evaluate limit obtain unsolved (Suspension emptyVarEnv expr)) 1

evaluate :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> VarSet -> Suspension
         -> StateT Int (ExceptT EvalFailure m) Whnf
evaluate limit obtain unsolved (Suspension env expr) = do
  steps <- get
  if intGtLimit steps limit then lift (throwE Exhausted) else put (steps + 1)
  case expr of
    Var v
      | Just binding <- lookupVarEnv env v -> evaluate limit obtain unsolved binding
      | otherwise -> do
          definition <- lift (lift (obtain v))
          case definition of
            Just rhs -> evaluate limit obtain unsolved (Suspension emptyVarEnv rhs)
            Nothing
              | v `elemVarSet` unsolved -> pure (Neutral (Unsolved v))
              | otherwise -> pure (Neutral (Rigid v))
    Lit lit -> pure (Literal lit)
    Lam binder body -> pure (Lambda binder (Suspension env body))
    App fun arg -> do
      headValue <- evaluate limit obtain unsolved (Suspension env fun)
      case headValue of
        Lambda binder (Suspension closure body)
          | Type argTy <- arg, isTyVar binder ->
              let inScope = mkInScopeSet (exprFreeVars body `unionVarSet` tyCoVarsOfType argTy)
                  subst = extendTvSubst (mkEmptySubst inScope) binder argTy
              in evaluate limit obtain unsolved (Suspension closure (substExpr subst body))
          | liftedBinder binder ->
              evaluate limit obtain unsolved (Suspension (extendVarEnv closure binder (Suspension env arg)) body)
        Neutral neutral -> pure (Neutral (Applied neutral (Suspension env arg)))
        _ -> lift (throwE UnsupportedCore)
    Let (NonRec binder rhs) body
      | liftedBinder binder ->
          evaluate limit obtain unsolved (Suspension (extendVarEnv env binder (Suspension env rhs)) body)
    Let (Rec bindings) body
      | all (liftedBinder . fst) bindings ->
          let recEnv = extendVarEnvList env [(binder, Suspension recEnv rhs) | (binder, rhs) <- bindings]
          in evaluate limit obtain unsolved (Suspension recEnv body)
    Case scrutinee binder resultType alts -> do
      let suspended = Suspension env scrutinee
      headValue <- evaluate limit obtain unsolved suspended
      case headValue of
        Neutral neutral ->
          pure (Neutral (CaseBlocked neutral (CaseContinuation env binder resultType alts)))
        Literal lit
          | Just (Alt _ [] body) <- findAlt (LitAlt lit) alts ->
              let bodyEnv = extendVarEnv env binder (Suspension emptyVarEnv (Lit lit))
              in evaluate limit obtain unsolved (Suspension bodyEnv body)
          | otherwise -> lift (throwE MissingAlternative)
        Lambda{}
          | Just (Alt DEFAULT [] body) <- findAlt DEFAULT alts ->
              evaluate limit obtain unsolved (Suspension (extendVarEnv env binder suspended) body)
        _ -> lift (throwE UnsupportedCore)
    _ -> lift (throwE UnsupportedCore)

-- | Only lifted term binders can enter the lazy environment.
liftedBinder :: Var -> Bool
liftedBinder binder = isId binder && isLiftedTypeKind (typeKind (varType binder))

{- Note [Observing open Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Save each operand with its lexical environment when forming an application.
Extend the function's saved environment when entering its body. This keeps
an ignored lifted operand lazy. Use Core's capture-avoiding substitution
for type arguments.

A case must inspect its scrutinee even when it has only a DEFAULT branch.
If the scrutinee is neutral, retain the alternatives, the binder and their
environment. A rigid input stays neutral. An unsolved input can be observed
again with a new definition. This distinction follows Agda's weak-head
reducer and its blockers. See Note [Open source calls] in GHC.Tc.Gen.OpenTypeEval.
-}
