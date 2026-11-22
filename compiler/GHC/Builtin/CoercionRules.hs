-- | Built-in rules that can occur in an interface coercion.
module GHC.Builtin.CoercionRules
  ( builtInCoAxiomRules
  , boxedPairCompositionRule, matchBoxedPairComposition
  , boxedPairReconstructionRule
  , proveBoxedPairProjection, proveScopedSigmaClosure, proveScopedLambdaClosure
  , proveScopedEnumCase
  ) where

import GHC.Prelude

import GHC.Builtin.WiredIn.TypeLits (typeNatCoAxiomRules)
import GHC.Builtin.WiredIn.Types (promotedTupleDataCon, tupleTyCon)
import GHC.Core.Coercion (existsCoRule, matchableAppProjectionRule, mkNomReflCo, mkSymCo)
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon (dataConName, dataConTyCon)
import GHC.Core.TyCo.FVs (tyCoVarsOfType)
import GHC.Core.TyCo.Rep (Coercion(AxiomCo), Type(TyVarTy))
import GHC.Core.TyCo.Compare (eqType, eqTypes, eqScopedSigmaClosures, eqScopedLambdaClosures)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Data.Pair
import GHC.Types.Basic (Boxity(Boxed), LeftOrRight(..))
import GHC.Types.Var (ExistsTyFlag(..), binderVar, tyVarKind)
import GHC.Types.Var.Set
import GHC.Types.Name (nameModule_maybe, nameOccName, stableNameCmp)
import GHC.Types.Name.Occurrence (occNameString, sourceTableTermOcc)
import GHC.Types.Unique.FM (UniqFM, addToUFM)

import GHC.Utils.Misc (equalLength, lengthIs, snocView)

import Control.Monad (guard)
import Data.List (isPrefixOf, sortBy)

-- | Resolve the stable names written to the interface by CoreToIface.
builtInCoAxiomRules :: UniqFM FastString CoAxiomRule
builtInCoAxiomRules =
    foldl' (\rules (name, rule) -> addToUFM rules name rule) typeNatCoAxiomRules
      [ (boxedPairCompositionName, boxedPairCompositionRule)
      , (boxedPairReconstructionName, boxedPairReconstructionRule)
      , existsCoRule ExplicitExists Nominal, existsCoRule ExplicitExists Representational
      , existsCoRule ImplicitExists Nominal, existsCoRule ImplicitExists Representational
      , matchableAppProjectionRule CLeft, matchableAppProjectionRule CRight
      , (scopedSigmaClosureName, scopedSigmaClosureRule)
      , (scopedLambdaClosureName, scopedLambdaClosureRule)
      , (scopedEnumCaseName, scopedEnumCaseRule) ]

scopedSigmaClosureRule, scopedLambdaClosureRule :: CoAxiomRule
scopedSigmaClosureRule = scopedClosureRule scopedSigmaClosureName eqScopedSigmaClosures
scopedLambdaClosureRule = scopedClosureRule scopedLambdaClosureName eqScopedLambdaClosures

scopedEnumCaseRule :: CoAxiomRule
scopedEnumCaseRule = scopedClosureRule scopedEnumCaseName eqScopedEnumCases

scopedClosureRule :: FastString -> (Type -> Type -> Bool) -> CoAxiomRule
scopedClosureRule name equal = BuiltInEqRule name proves
  where
    -- Compare the two left endpoints. Lifting may change each right endpoint.
    -- The second nominal premise connects the result to the rewritten RHS.
    proves [Pair lhs _, Pair rhs result]
      | equal lhs rhs = Just (Pair lhs result)
    proves _ = Nothing

-- | Produce nominal evidence for equal scoped retained-sum codomains.
proveScopedSigmaClosure :: Type -> Type -> Maybe Coercion
proveScopedSigmaClosure lhs rhs
  | eqScopedSigmaClosures lhs rhs
  = Just (AxiomCo scopedSigmaClosureRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Produce nominal evidence for alpha-equivalent generated type lambdas.
proveScopedLambdaClosure :: Type -> Type -> Maybe Coercion
proveScopedLambdaClosure lhs rhs
  | eqScopedLambdaClosures lhs rhs
  = Just (AxiomCo scopedLambdaClosureRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Produce nominal evidence for equal retained enumeration cases.
proveScopedEnumCase :: Type -> Type -> Maybe Coercion
proveScopedEnumCase lhs rhs
  | eqScopedEnumCases lhs rhs
  = Just (AxiomCo scopedEnumCaseRule [mkNomReflCo lhs, mkNomReflCo rhs])
  | otherwise = Nothing

-- | Check a nominal equality of two complete retained enumeration cases.
-- See Note [Equality of retained enumeration cases].
eqScopedEnumCases :: Type -> Type -> Bool
eqScopedEnumCases lhs rhs
  | Just (tc1, as1) <- splitTyConApp_maybe lhs
  , Just (tc2, as2) <- splitTyConApp_maybe rhs
  , tc1 /= tc2
  , is_signature_case tc1, is_signature_case tc2
  , as1 `lengthIs` tyConArity tc1, as2 `lengthIs` tyConArity tc2
  , tyConKind tc1 `eqType` tyConKind tc2
  , as1 `eqTypes` as2
  , Just rows1 <- checked_rows tc1
  , Just rows2 <- checked_rows tc2
  = rows1 == rows2
  | otherwise = False
  where
    is_signature_case tc
      | Just (owner, False) <- closedFamilyScope_maybe tc
      , Just owner_module <- nameModule_maybe owner
      , Just owner_module == nameModule_maybe (tyConName tc)
      , let occurrence = occNameString (nameOccName owner)
      = "$signature-case:" `isPrefixOf` occurrence
        && ("$case:" ++ occurrence ++ ":") `isPrefixOf` occNameString (nameOccName (tyConName tc))
      | otherwise = False

    checked_rows tc = do
      guard (isLiftedTypeKind (tyConResKind tc))
      NotInjective <- pure (tyConInjectivityInfo tc)
      let kinds = map (tyVarKind . binderVar) (tyConBinders tc)
      guard (all (isEmptyVarSet . tyCoVarsOfType) kinds)
      (captures, scrutinee_kind) <- snocView kinds
      (enum, []) <- splitTyConAppNoView_maybe scrutinee_kind
      guard (isEnumerationTyCon enum)
      constructors <- tyConDataCons_maybe enum
      ax <- closedFamilyTyConCoAxiom_maybe tc
      guard (coAxiomTyCon ax == tc && coAxiomRole ax == Nominal)
      guard (null (co_ax_source_proof_rules ax))
      let branches = fromBranches (coAxiomBranches ax)
      guard (equalLength branches constructors)
      rows <- traverse (checked_row enum captures) branches
      let sorted_rows = sortBy (compare_names (dataConName . fst)) rows
      guard (map fst sorted_rows == sortBy (compare_names dataConName) constructors)
      pure sorted_rows

    checked_row enum captures branch = do
      guard (null (cab_cvs branch) && null (cab_eta_tvs branch))
      guard (equalLength (cab_roles branch) (cab_tvs branch) && all (== Nominal) (cab_roles branch))
      (parameters, pattern_ty) <- snocView (cab_lhs branch)
      vars <- traverse plain_var parameters
      guard (equalLength vars (cab_tvs branch) && equalLength vars captures)
      let bound = mkVarSet (cab_tvs branch)
      guard (sizeVarSet bound == length vars && all (`elemVarSet` bound) vars)
      guard (sizeVarSet (mkVarSet vars) == length vars)
      guard (map tyVarKind vars `eqTypes` captures)
      (pattern_tc, []) <- splitTyConAppNoView_maybe pattern_ty
      con <- isPromotedDataCon_maybe pattern_tc
      guard (dataConTyCon con == enum)
      (result, []) <- splitTyConAppNoView_maybe (cab_rhs branch)
      guard (isAlgTyCon result && isLiftedTypeKind (tyConKind result))
      pure (con, result)

    plain_var (TyVarTy var) = Just var
    plain_var _ = Nothing
    compare_names get_name a b = stableNameCmp (get_name a) (get_name b)

{- Note [Equality of retained enumeration cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The checker records the signature-case owner from the renamed source
rows. Each family retains its ordinary reduction axioms. The additional
nominal rule can compare two such families with the same scrutinee when
ordinary rewriting leaves their applications stuck.

Read the immutable axioms on demand. Require one exhaustive split over
the same unindexed enumeration. Each row has only unrelated capture
variables and one exact constructor pattern. Check that each result is
a bare algebraic type constructor. Distinct exact constructors
establish disjointness. Do not force the pairwise overlap caches.
Compare all results by constructor identity.

Recheck this shape when reading the coercion. A substitution can make
the scrutinee known to both families. The same row then reduces on both
sides, so the proof remains valid. Other closed families and ordered
cases continue to use their nominal identities.
-}

{- Note [Strict boxed pair composition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A certified reconstruction of a boxed pair forces the shell but neither
field. Each output field is one of the two input binders. A certified
projection of the reconstruction therefore observes the same shell and
field as a direct projection of the selected input binder. There is no pair
eta law here. In particular a bottom shell is different from a pair
whose two fields are bottom.

Follow each projected field back through strict constructor cases. Its
path ends at a scrutinee which has no such case. Compare the two paths
from that end and use the outermost common scrutinee with the same field.
For example, fst (duplicate q) and snd (duplicate q) can agree on the
first field of q. They need no equation for q itself. The source solver
records the owners of the cases crossed on either side. All steps check
nominal single-branch axioms, binder identity and exact constructors.
Use the same pure check for Lint and for coercion endpoints. Substitution
can make the operand a constructor; the rule remains valid in that case.

Keep the original rule key for exchange-only proofs in existing interfaces.
Write the more general key for other reconstructions. A compiler that
reads the same binary format but lacks the new rule rejects the new key
through tcIfaceAxiomRule.
-}

boxedPairReconstructionName :: FastString
boxedPairReconstructionName = fsLit "BoxedPairReconstruction"

boxedPairCompositionName :: FastString
boxedPairCompositionName = fsLit "BoxedPairComposition"

-- | The original nominal law for a projection through an exchange.
boxedPairCompositionRule :: CoAxiomRule
boxedPairCompositionRule = pairCompositionRule boxedPairCompositionName matchExchange

-- | Nominal evidence for two projections that observe the same field.
boxedPairReconstructionRule :: CoAxiomRule
boxedPairReconstructionRule = pairCompositionRule boxedPairReconstructionName commonField

type PairCaseUse = (TyCon, [Type])

pairCompositionRule :: FastString -> (Projection -> Projection -> Maybe [PairCaseUse])
                    -> CoAxiomRule
pairCompositionRule name match = BuiltInEqRule name proves
  where
    proves [Pair lhs lhs', Pair rhs rhs'] = do
      guard (lhs `eqType` lhs' && rhs `eqType` rhs')
      (l, r) <- projections lhs rhs
      _ <- match l r
      pure (Pair lhs rhs)
    proves _ = Nothing

-- | A position in the input pair.
data PairField = FirstField | SecondField
  deriving Eq

-- | The branch bodies used by the strict composition law.
data PairCaseBranch = Project PairField | Reconstruct PairField PairField

selectField :: PairField -> (a, a) -> a
selectField FirstField = fst
selectField SecondField = snd

-- | The current field and scrutinee, its two field kinds, and the equations used.
data Projection = Projection PairField Type (Kind, Kind) [PairCaseUse]

-- | Check the original oriented exchange law and return its source dependencies.
matchBoxedPairComposition :: Type -> Type -> Maybe [(TyCon, [Type])]
matchBoxedPairComposition lhs rhs = do
    (l, r) <- projections lhs rhs
    matchExchange l r

-- | Preserve the published exchange proof in either orientation.
proveBoxedPairProjection :: Type -> Type -> Maybe (Coercion, [PairCaseUse])
proveBoxedPairProjection lhs rhs = do
    (l, r) <- projections lhs rhs
    choose l r
  where
    evidence rule a b = AxiomCo rule [mkNomReflCo a, mkNomReflCo b]
    choose l r
      | Just uses <- matchExchange l r
      = pure (evidence boxedPairCompositionRule lhs rhs, uses)
      | Just uses <- matchExchange r l
      = pure (mkSymCo (evidence boxedPairCompositionRule rhs lhs), uses)
      | otherwise = do
          uses <- commonField l r
          pure (evidence boxedPairReconstructionRule lhs rhs, uses)

projections :: Type -> Type -> Maybe (Projection, Projection)
projections lhs rhs = do
    l <- project lhs
    r <- project rhs
    guard (typeKind lhs `eqType` typeKind rhs)
    pure (l, r)
  where
    project ty = do
      (fun, args, Project field, p, a, b) <- appliedPairCase ty
      pure (Projection field p (a,b) [(fun, args)])

-- | Cross one certified reconstruction and retain its input field identities.
throughReconstruction :: Projection -> Maybe (Bool, Projection)
throughReconstruction (Projection field p (a,b) uses) = do
    (fun, args, Reconstruct left right, q, x, y) <- appliedPairCase p
    guard (a `eqType` selectField left (x,y) && b `eqType` selectField right (x,y))
    let next = Projection (selectField field (left,right)) q (x,y) ((fun,args):uses)
    pure (left == SecondField && right == FirstField, next)

matchExchange :: Projection -> Projection -> Maybe [PairCaseUse]
matchExchange l r = do
    (True, inner) <- throughReconstruction l
    matchingField inner r

matchingField :: Projection -> Projection -> Maybe [PairCaseUse]
matchingField (Projection f p (a,b) uses) (Projection g q (x,y) other) = do
    guard (f == g && a `eqType` x && b `eqType` y && p `eqType` q)
    pure (reverse uses ++ reverse other)

-- | Stop at the outermost common projection. Its scrutinee can remain opaque.
commonField :: Projection -> Projection -> Maybe [PairCaseUse]
commonField l r
  | Just uses <- matchingField l r = Just uses
  | otherwise = commonPrefix (path l []) (path r [])
  where
    path current outer = case throughReconstruction current of
      Just (_, inner) -> path inner (current:outer)
      Nothing         -> current:outer

    commonPrefix (x:xs) (y:ys) = do
      uses <- matchingField x y
      case commonPrefix xs ys of
        Just shorter -> Just shorter
        Nothing      -> Just uses
    commonPrefix _ _ = Nothing

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
        field ty
          | repGetTyVar_maybe ty == Just x = Just FirstField
          | repGetTyVar_maybe ty == Just y = Just SecondField
          | otherwise = Nothing
        input_kinds = (a, b)
    (choice, result) <- case field rhs of
      Just f -> pure (Project f, selectField f input_kinds)
      Nothing -> do
        (rc, [left_kind, right_kind, left, right]) <- splitTyConAppNoView_maybe rhs
        guard (rc == pc)
        left_field <- field left
        right_field <- field right
        let kind_vars_by_field = (ka_var, kb_var)
        guard (repGetTyVar_maybe left_kind == Just (selectField left_field kind_vars_by_field))
        guard (repGetTyVar_maybe right_kind == Just (selectField right_field kind_vars_by_field))
        let result_kind f = selectField f input_kinds
        pure (Reconstruct left_field right_field,
              mkTyConApp (tupleTyCon Boxed 2) [result_kind left_field, result_kind right_field])
    guard (typeKind ty `eqType` result)
    pure (family, args, choice, p, a, b)
