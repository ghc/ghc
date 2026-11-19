module GHC.Core.OpenWhnf
  ( EvalFailure(..)
  , Whnf(..)
  , Neutral(..)
  , Suspension
  , CaseContinuation(..)
  , openWhnfM, openWhnfWithLocalM, openWhnfTraceM, CaseStep(..)
  , openDataConFieldTypes, openReadbackM, OpenReadbackFailure(..)
  ) where

import GHC.Prelude

import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, mapStateT, put)
import GHC.Builtin.WiredIn.Types
  (charDataCon, mkPromotedPairTy, pairTyCon, tupleDataCon, unboxedUnitDataCon, unboxedUnitTy)
import GHC.Core.DataCon
  ( DataCon, dataConWorkId, dataConTyCon, dataConFullSig, dataConRepArity
  , dataConRepArgBinders, dataConStupidTheta, dataConImplBangs
  , dataConWrapId_maybe, dataConUserTyVars, dataConUnivTyVars, promoteDataCon
  , isBanged, isTypeDataCon )
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreAlt, CoreExpr, Expr(..), collectArgs, isTyCoArg)
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, substExpr)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCon (isBoxedDataTyCon)
import GHC.Core.TyCo.Rep (scaledThing)
import GHC.Core.Type (Kind, Type, isLiftedTypeKind, mkTyConApp, splitTyConApp_maybe, substTyWith, tyCoVarsOfType, typeKind)
import GHC.Core.Utils (exprType, findAlt)
import GHC.Types.Basic (Boxity(Boxed), IntWithInf, intGtLimit)
import GHC.Types.Id (isDataConWorkId_maybe)
import GHC.Types.Literal (Literal(LitChar))
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.Var (Var, isId, isTyVar, varType)
import GHC.Types.Var.Env (VarEnv, emptyVarEnv, extendVarEnv, extendVarEnvList, lookupVarEnv,
                          mkInScopeSet)
import GHC.Types.Var.Set (VarSet, emptyVarSet, elemVarSet, unionVarSet)

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
  | BoxedData DataCon [Type] [Suspension]
  | Neutral Neutral

-- | A weak head result cannot be returned as a checked source type.
data OpenReadbackFailure
  = ReadbackEvaluation EvalFailure
  | ReadbackOpaque Whnf
  | ReadbackHidden DataCon

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
  | SelectBoxedData DataCon AltCon
  | SelectLiteral Literal AltCon
  | SelectLambdaDefault
  | ContinueWithUnit
  deriving (Eq)

-- | Observe weak head form with a demand-driven definition provider.
-- See Note [Observing open Core].
openWhnfM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr)) -> VarSet -> CoreExpr
          -> m (Either EvalFailure Whnf)
openWhnfM limit obtain = openWhnfWithLocalM limit (\v _ -> obtain v) (const (pure ()))

-- | Provide demanded definitions with their first syntactic type argument.
-- Report lexical variables separately when their suspended bindings are demanded.
openWhnfWithLocalM :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
                  -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfWithLocalM limit obtain local =
  openWhnfTraceWithLocalM limit obtain local (const (pure ()))

-- | Observe an expression and report case selections and the unboxed unit
-- continuation step in the order in which the observer performs them.
openWhnfTraceM :: Monad m => IntWithInf -> (Var -> m (Maybe CoreExpr))
              -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceM limit obtain = openWhnfTraceWithLocalM limit (\v _ -> obtain v) (const (pure ()))

openWhnfTraceWithLocalM :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
                       -> (CaseStep -> m ()) -> VarSet -> CoreExpr -> m (Either EvalFailure Whnf)
openWhnfTraceWithLocalM limit obtain local record unsolved expr =
  runExceptT $ evalStateT (evaluate limit obtain local record unsolved Nothing (Suspension emptyVarEnv expr)) 1

-- | Reflect a finite ordinary constructor value or a supplied rigid symbol.
-- The first observation and every demanded field share one reduction budget.
openReadbackM :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr))
              -> (Var -> m ()) -> (Var -> Maybe Type) -> (DataCon -> m Bool)
              -> Kind -> CoreExpr
              -> m (Either OpenReadbackFailure (Type, [DataCon]))
openReadbackM limit obtain local origin visible expected expr =
  runExceptT $ evalStateT (readback [] expected (Suspension emptyVarEnv expr)) 1
  where
    observe = mapStateT (withExceptT ReadbackEvaluation)
            . evaluate limit obtain local (const (pure ())) emptyVarSet Nothing
    readback used kind suspension = do
      whnf <- observe suspension
      case whnf of
        Neutral (Rigid v)
          | Just ty <- origin v, typeKind ty `eqType` kind -> pure (ty, used)
        BoxedPair first@(Suspension _ x) second@(Suspension _ y)
          | Just (con, [ka, kb]) <- splitTyConApp_maybe kind, con == pairTyCon
          , exprType x `eqType` ka, exprType y `eqType` kb -> do
              (a, used') <- readback used ka first
              (b, used'') <- readback used' kb second
              pure (mkPromotedPairTy ka kb a b, used'')
        BoxedData con arguments fields
          | mkTyConApp (dataConTyCon con) arguments `eqType` kind
          , dataConUserTyVars con == dataConUnivTyVars con
          , Just field_kinds <- openDataConFieldTypes con arguments
          , length fields == length field_kinds -> do
              in_scope <- lift (lift (visible con))
              if not in_scope then lift (throwE (ReadbackHidden con)) else do
                (rev_tys, used') <- foldM readField ([], con : used) (zip field_kinds fields)
                let result = mkTyConApp (promoteDataCon con) (arguments ++ reverse rev_tys)
                if typeKind result `eqType` kind
                  then pure (result, used')
                  else lift (throwE (ReadbackOpaque whnf))
        _ -> lift (throwE (ReadbackOpaque whnf))
    readField (rev_tys, used) (kind, field) = do
      (ty, used') <- readback used kind field
      pure (ty : rev_tys, used')

evaluate :: Monad m => IntWithInf -> (Var -> Maybe Type -> m (Maybe CoreExpr)) -> (Var -> m ())
         -> (CaseStep -> m ()) -> VarSet -> Maybe Type -> Suspension
         -> StateT Int (ExceptT EvalFailure m) Whnf
evaluate limit obtain local record unsolved first_type (Suspension env expr) = do
  steps <- get
  if intGtLimit steps limit then lift (throwE Exhausted) else put (steps + 1)
  case expr of
    _ | Just (con, arguments, fields) <- constructorApplication expr, con /= pairCon ->
          pure (BoxedData con arguments (map (Suspension env) fields))
    Var v
      | Just binding <- lookupVarEnv env v -> do
          lift (lift (local v))
          recur binding
      | otherwise -> do
          definition <- lift (lift (obtain v first_type))
          case definition of
            Just rhs@(Var target)
              | varType target `eqType` varType v ->
                  recurHead first_type (Suspension emptyVarEnv rhs)
            Just rhs -> recur (Suspension emptyVarEnv rhs)
            Nothing
              | v `elemVarSet` unsolved -> pure (Neutral (Unsolved v))
              | otherwise -> pure (Neutral (Rigid v))
    Tick Breakpoint{} body -> recurHead first_type (Suspension env body)
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
      let head_type = case arg of
            Type ty -> Just ty
            _ -> Nothing
      headValue <- recurHead head_type (Suspension env fun)
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
        BoxedPair first second ->
          selectData SelectBoxedPair pairCon [first, second] binder suspended alts
        BoxedData con _ fields ->
          selectData (SelectBoxedData con) con fields binder suspended alts
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
    recurHead = evaluate limit obtain local record unsolved
    recur = recurHead Nothing
    pairCon = tupleDataCon Boxed 2
    pairWorker = dataConWorkId pairCon
    selectData step con fields binder suspended alts
      | Just (Alt key binders body) <- findAlt (DataAlt con) alts
      , Just bindings <- altBindings key con binders fields = do
          lift (lift (record (step key)))
          recur (Suspension (extendVarEnvList env ((binder, suspended) : bindings)) body)
      | otherwise = lift (throwE MissingAlternative)

    altBindings (DataAlt chosen) con binders fields
      | chosen == con, length binders == length fields
      , and (zipWith matches binders fields) = Just (zip binders fields)
    altBindings DEFAULT _ [] _ = Just []
    altBindings _ _ _ _ = Nothing

    matches field (Suspension _ body) =
      liftedBinder field && varType field `eqType` exprType body

-- | Read a fully applied ordinary constructor worker and its suspended fields.
constructorApplication :: CoreExpr -> Maybe (DataCon, [Type], [CoreExpr])
constructorApplication expr
  | (Var worker, args) <- collectArgs expr
  , Just con <- isDataConWorkId_maybe worker
  , let (types, fields) = span isTyCoArg args
  , Just parameters <- traverse typeArgument types
  , Just expected <- openDataConFieldTypes con parameters
  , length expected == length fields
  , all (not . isTyCoArg) fields
  , and (zipWith eqType expected (map exprType fields))
  = Just (con, parameters, fields)
  | otherwise = Nothing
  where
    typeArgument (Type ty) = Just ty
    typeArgument _ = Nothing

-- | Recognize lifted, lazy constructor fields with the ordinary representation.
-- Both the source operand decoder and the Core observer use this check.
openDataConFieldTypes :: DataCon -> [Type] -> Maybe [Type]
openDataConFieldTypes con parameters
  | isBoxedDataTyCon (dataConTyCon con), not (isTypeDataCon con)
  , null (dataConRepArgBinders con), null (dataConStupidTheta con)
  , Nothing <- dataConWrapId_maybe con
  , all (not . isBanged) (dataConImplBangs con)
  , (vars, [], [], [], fields, result) <- dataConFullSig con
  , length vars == length parameters
  , and (zipWith eqType (map varType vars) (map typeKind parameters))
  , length fields == dataConRepArity con
  , let instantiate = substTyWith vars parameters
        field_types = map (instantiate . scaledThing) fields
  , isLiftedTypeKind (typeKind (instantiate result))
  , all (isLiftedTypeKind . typeKind) field_types
  = Just field_types
  | otherwise = Nothing

-- | Only lifted term binders can enter the lazy environment.
liftedBinder :: Var -> Bool
liftedBinder binder = isId binder && isLiftedTypeKind (typeKind (varType binder))

{- Note [Observing open Core]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Save each operand with its lexical environment when forming an application.
Extend the function's saved environment when entering its body. This keeps
an ignored lifted operand lazy. Use Core's capture-avoiding substitution
for type arguments.

A definition lookup receives the first syntactic argument when it is a
type. Follow the application spine, transparent ticks and same-typed
variable aliases to find it. Crossing a suspension, entering any other
body, or inspecting a case starts a new observation. A type argument
from the caller cannot justify a specialization of a recursive call.

A saturated worker for an ordinary lazy boxed constructor is in weak
head normal form. Preserve its fields as independent suspensions. Use
the actual DataCon and typed alternative binders to select and bind its
fields. A neutral scrutinee still blocks case selection. The ordinary
source operand decoder also checks this worker shape. It does not
bypass a strict field or a representation-changing wrapper.

Keep the dedicated boxed pair and Char trace steps for existing clients.

Readback needs a finite type. It asks for each suspended field at its
original kind. Only original rigid symbols and checked constructors can
be read back. All of these field observations share the existing budget.
Ordinary case observation continues to leave unselected fields suspended.

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
