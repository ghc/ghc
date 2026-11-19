-- | Built-in nominal rules that can occur in an interface coercion.
module GHC.Builtin.CoercionRules
  ( builtInCoAxiomRules
  , boxedPairCompositionRule
  , matchBoxedPairComposition
  ) where

import GHC.Prelude

import GHC.Builtin.WiredIn.TypeLits (typeNatCoAxiomRules)
import GHC.Builtin.WiredIn.Types (promotedTupleDataCon, tupleTyCon)
import GHC.Core.Coercion.Axiom
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Data.Pair
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (sourceTableTermOcc)
import GHC.Types.Unique.FM (UniqFM, addToUFM)

import Control.Monad (guard)

-- | Resolve the stable names written to the interface by CoreToIface.
builtInCoAxiomRules :: UniqFM FastString CoAxiomRule
builtInCoAxiomRules = addToUFM typeNatCoAxiomRules
                            boxedPairCompositionName boxedPairCompositionRule

{- Note [Strict boxed pair composition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The certified case for swapping a boxed pair forces the shell but neither
field. Both certified projections force the shell and return one field.
Composing a projection with the swap therefore observes the same shell
and field as projecting the opposite field directly. There is no pair
eta law here. In particular a bottom shell is different from a pair
whose two fields are bottom.

Check all three nominal single-branch axioms. The branch verifier uses
binder identity and exact constructors. The source solver additionally
checks and records all three term owners before it introduces evidence.
Use the same pure check for Lint and for coercion endpoints. Substitution
can make the operand a constructor; the rule remains valid in that case.
-}

boxedPairCompositionName :: FastString
boxedPairCompositionName = fsLit "BoxedPairComposition"

-- | Nominal evidence for composing a certified projection with a certified swap.
boxedPairCompositionRule :: CoAxiomRule
boxedPairCompositionRule =
    BuiltInEqRule boxedPairCompositionName proves
  where
    proves [Pair lhs lhs', Pair rhs rhs'] = do
      guard (lhs `eqType` lhs' && rhs `eqType` rhs')
      _ <- matchBoxedPairComposition lhs rhs
      pure (Pair lhs rhs)
    proves _ = Nothing

-- | The branch bodies used by the strict composition law.
data PairCaseBranch = PickLeft | PickRight | ExchangeFields
  deriving Eq

-- | Check the rule endpoints and return all source family uses.
matchBoxedPairComposition :: Type -> Type -> Maybe [(TyCon, [Type])]
matchBoxedPairComposition lhs rhs = do
    (outer, outer_args, outer_case, inner_ty, outer_a, outer_b) <- appliedPairCase lhs
    (inner, inner_args, inner_case, p, inner_a, inner_b) <- appliedPairCase inner_ty
    (other, other_args, other_case, q, other_a, other_b) <- appliedPairCase rhs
    guard (inner_case == ExchangeFields)
    guard ((outer_case == PickLeft && other_case == PickRight)
           || (outer_case == PickRight && other_case == PickLeft))
    guard (and [p `eqType` q, inner_a `eqType` other_a, inner_b `eqType` other_b,
                outer_a `eqType` inner_b, outer_b `eqType` inner_a,
                typeKind lhs `eqType` typeKind rhs])
    pure [(outer, outer_args), (inner, inner_args), (other, other_args)]

-- | Read the single branch at a saturated application and check its kinds.
appliedPairCase :: Type -> Maybe (TyCon, [Type], PairCaseBranch, Type, Kind, Kind)
appliedPairCase ty = do
    (family, args) <- splitTyConApp_maybe ty
    _ <- sourceTableTermOcc (nameOccName (tyConName family))
    guard (length args == tyConArity family)
    p : rev_kinds <- pure (reverse args)
    visible : invis <- pure (reverse (tyConBinders family))
    guard (isVisibleTyConBinder visible && all isInvisibleTyConBinder invis)
    NotInjective <- pure (tyConInjectivityInfo family)
    ax <- closedFamilyTyConCoAxiom_maybe family
    guard (coAxiomRole ax == Nominal && coAxiomTyCon ax == family)
    [branch] <- pure (fromBranches (coAxiomBranches ax))
    guard (null (coAxBranchCoVars branch) && null (coAxBranchIncomps branch))
    let binders = coAxBranchTyVars branch
    y : x : rev_kind_vars <- pure (reverse binders)
    let kind_vars = reverse rev_kind_vars
        kinds = reverse rev_kinds
    guard (length kind_vars == length kinds)
    guard (coAxBranchRoles branch == replicate (length binders) Nominal)
    guard (all (isLiftedTypeKind . varType) kind_vars && all (isLiftedTypeKind . typeKind) kinds)
    pat : rev_pat_kinds <- pure (reverse (coAxBranchLHS branch))
    guard (eqTypes rev_pat_kinds (mkTyVarTys rev_kind_vars))
    (pc, [ka, kb, xt, yt]) <- splitTyConAppNoView_maybe pat
    guard (pc == promotedTupleDataCon Boxed 2)
    guard (x /= y && repGetTyVar_maybe xt == Just x && repGetTyVar_maybe yt == Just y)
    guard (varType x `eqType` ka && varType y `eqType` kb)
    ka_var <- repGetTyVar_maybe ka
    kb_var <- repGetTyVar_maybe kb
    guard (length kind_vars == if ka_var == kb_var then 1 else 2)
    a <- lookup ka_var (zip kind_vars kinds)
    b <- lookup kb_var (zip kind_vars kinds)
    guard (typeKind p `eqType` mkTyConApp (tupleTyCon Boxed 2) [a,b])
    let rhs = coAxBranchRHS branch
    choice <- if repGetTyVar_maybe rhs == Just x then pure PickLeft
              else if repGetTyVar_maybe rhs == Just y then pure PickRight
              else do
                (rc, rhs_args) <- splitTyConAppNoView_maybe rhs
                guard (rc == pc
                       && map repGetTyVar_maybe rhs_args == map Just [kb_var, ka_var, y, x])
                pure ExchangeFields
    let result = case choice of
          PickLeft -> a
          PickRight -> b
          ExchangeFields -> mkTyConApp (tupleTyCon Boxed 2) [b,a]
    guard (typeKind ty `eqType` result)
    pure (family, args, choice, p, a, b)
