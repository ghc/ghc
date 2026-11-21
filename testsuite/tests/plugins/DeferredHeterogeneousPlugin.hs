module DeferredHeterogeneousPlugin (plugin) where

import GHC.Core.TyCo.Compare (eqType)
import GHC.Plugins

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = \_ passes ->
            pure (CoreDoPluginPass "Check homogeneous coercion binders" check : passes)
        }

check :: ModGuts -> CoreM ModGuts
check guts =
    case filter isCoVar (concatMap binders (mg_binds guts)) of
        [] -> error "Expected coercion binders"
        covars
            | any mixedKind covars -> error "Mixed-kind coercion binder"
            | otherwise -> pure guts
  where
    mixedKind cv =
        let (lhs, rhs, _) = coVarTypesRole cv
         in not (eqType (typeKind lhs) (typeKind rhs))

binders :: CoreBind -> [Var]
binders (NonRec binder rhs) = binder : exprBinders rhs
binders (Rec bindings) = concatMap (\(binder, rhs) -> binder : exprBinders rhs) bindings

exprBinders :: CoreExpr -> [Var]
exprBinders (Lam binder rhs) = binder : exprBinders rhs
exprBinders (App fun arg) = exprBinders fun ++ exprBinders arg
exprBinders (Let binding rhs) = binders binding ++ exprBinders rhs
exprBinders (Case value binder _ alts) = binder : exprBinders value ++ concatMap altBinders alts
exprBinders (Cast rhs _) = exprBinders rhs
exprBinders (Tick _ rhs) = exprBinders rhs
exprBinders _ = []

altBinders :: CoreAlt -> [Var]
altBinders (Alt _ vars rhs) = vars ++ exprBinders rhs
