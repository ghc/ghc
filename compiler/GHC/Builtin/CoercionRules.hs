-- | Built-in rules that can occur in an interface coercion.
module GHC.Builtin.CoercionRules
  ( builtInCoAxiomRules
  , boxedPairCompositionRule, matchBoxedPairComposition
  , boxedPairReconstructionRule
  , proveBoxedPairProjection, proveScopedSigmaClosure, proveScopedLambdaClosure
  , proveScopedConstructorCase
  ) where

import GHC.Prelude

import GHC.Builtin.WiredIn.TypeLits (typeNatCoAxiomRules)
import GHC.Builtin.WiredIn.Types (promotedTupleDataCon, tupleTyCon)
import GHC.Core.Coercion (existsCoRule, matchableAppProjectionRule, mkNomReflCo, mkSymCo)
import GHC.Core.Coercion.Axiom
import GHC.Core.DataCon
  ( DataCon, dataConName, dataConTyCon, dataConUnivTyVars, dataConInstOrigArgTys
  , dataConSourceArity, dataConRepArgBinders, isVanillaDataCon )
import GHC.Core.TyCo.Rep (Coercion(AxiomCo), Type(TyVarTy), scaledThing)
import GHC.Core.TyCo.Compare
  (eqType, eqTypeX, eqTypes, eqVarBndrs, eqScopedSigmaClosures, eqScopedLambdaClosures)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString
import GHC.Data.Pair
import GHC.Types.Basic (Boxity(Boxed), LeftOrRight(..))
import GHC.Types.Var (ExistsTyFlag(..), TyVar)
import GHC.Types.Var.Env (emptyInScopeSet, mkRnEnv2)
import GHC.Types.Var.Set
import GHC.Types.Name (Name, nameModule_maybe, nameOccName, stableNameCmp)
import GHC.Types.Name.Occurrence (occNameString, sourceTableTermOcc)
import GHC.Types.Unique.FM (UniqFM, addToUFM)

import GHC.Utils.Encoding (zEncodeString)
import GHC.Utils.Misc (equalLength, lengthIs, snocView)

import Control.Monad (guard, zipWithM, zipWithM_)
import Data.Char (isDigit)
import Data.List (isPrefixOf, sortBy, stripPrefix)

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
      , (scopedEnumCaseName, scopedEnumCaseRule)
      , (scopedConstructorCaseName, scopedConstructorCaseRule)
      , (scopedCaseResultName, scopedCaseResultRule)
      , (scopedOrderedCaseName, scopedOrderedCaseRule) ]

scopedSigmaClosureRule, scopedLambdaClosureRule :: CoAxiomRule
scopedSigmaClosureRule = scopedClosureRule scopedSigmaClosureName eqScopedSigmaClosures
scopedLambdaClosureRule = scopedClosureRule scopedLambdaClosureName eqScopedLambdaClosures

-- | The least interface requirement for this checked case equality.
data ScopedCaseProof = EnumerationCases | IgnoredFieldCases | ScopedResultCases
  deriving Eq

scopedEnumCaseRule, scopedConstructorCaseRule, scopedCaseResultRule :: CoAxiomRule
scopedEnumCaseRule = scopedClosureRule scopedEnumCaseName (eqScopedCasesUsing EnumerationCases)
scopedConstructorCaseRule = scopedClosureRule scopedConstructorCaseName (eqScopedCasesUsing IgnoredFieldCases)
scopedCaseResultRule = scopedClosureRule scopedCaseResultName (eqScopedCasesUsing ScopedResultCases)

scopedOrderedCaseRule :: CoAxiomRule
scopedOrderedCaseRule = scopedClosureRule scopedOrderedCaseName eqOrderedCases

eqScopedCasesUsing :: ScopedCaseProof -> Type -> Type -> Bool
eqScopedCasesUsing proof lhs rhs =
  checkedScopedCases lhs rhs == Just proof

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

-- | Produce nominal evidence for equal retained constructor cases.
proveScopedConstructorCase :: Type -> Type -> Maybe Coercion
proveScopedConstructorCase lhs rhs = do
  rule <- case checkedScopedCases lhs rhs of
    Just EnumerationCases -> Just scopedEnumCaseRule
    Just IgnoredFieldCases -> Just scopedConstructorCaseRule
    Just ScopedResultCases -> Just scopedCaseResultRule
    Nothing | eqOrderedCases lhs rhs -> Just scopedOrderedCaseRule
    Nothing -> Nothing
  pure (AxiomCo rule [mkNomReflCo lhs, mkNomReflCo rhs])

-- | Check complete constructor cases and select the evidence that they require.
-- See Note [Equality of retained constructor cases].
checkedScopedCases :: Type -> Type -> Maybe ScopedCaseProof
checkedScopedCases lhs rhs
  | Just (tc1, as1) <- splitTyConApp_maybe lhs
  , Just (tc2, as2) <- splitTyConApp_maybe rhs
  , tc1 /= tc2
  , isSignatureCase tc1, isSignatureCase tc2
  , as1 `lengthIs` tyConArity tc1, as2 `lengthIs` tyConArity tc2
  , tyConKind tc1 `eqType` tyConKind tc2
  , as1 `eqTypes` as2
  , Just (fields1, rows1) <- checked_rows tc1
  , Just (fields2, rows2) <- checked_rows tc2
  , fields1 == fields2, equalLength rows1 rows2
  = do
      scoped <- or <$> zipWithM same_row rows1 rows2
      pure $ if scoped then ScopedResultCases
             else if fields1 then IgnoredFieldCases
             else EnumerationCases
  | otherwise = Nothing
  where
    checked_rows tc = do
      guard (isLiftedTypeKind (tyConResKind tc))
      NotInjective <- pure (tyConInjectivityInfo tc)
      let kinds = map (tyVarKind . binderVar) (tyConBinders tc)
      guard (all (isEmptyVarSet . tyCoVarsOfType) kinds)
      (captures, scrutinee_kind) <- snocView kinds
      (parent, parent_args) <- splitTyConAppNoView_maybe scrutinee_kind
      let enumeration = null parent_args && isEnumerationTyCon parent
      guard (enumeration || (isVanillaAlgTyCon parent && isBoxedDataTyCon parent
                             && not (isGadtSyntaxTyCon parent)
                             && null (tyConStupidTheta parent)
                             && isLiftedTypeKind (typeKind scrutinee_kind)))
      guard (parent_args `lengthIs` tyConArity parent)
      constructors <- tyConDataCons_maybe parent
      ax <- closedFamilyTyConCoAxiom_maybe tc
      guard (coAxiomTyCon ax == tc && coAxiomRole ax == Nominal)
      guard (null (co_ax_source_proof_rules ax))
      let branches = fromBranches (coAxiomBranches ax)
      guard (equalLength branches constructors)
      let fields_of = checked_fields enumeration parent_args scrutinee_kind
      rows <- traverse (checked_row parent captures fields_of) branches
      let sorted_rows = sortBy (compare_names (dataConName . fst)) rows
      guard (map fst sorted_rows == sortBy (compare_names dataConName) constructors)
      -- Keep the old wire format for cases which the old checker accepts.
      guard (enumeration || any ((> 0) . dataConSourceArity) constructors)
      pure (not enumeration, sorted_rows)

    checked_row parent captures fields_of branch = do
      guard (null (cab_cvs branch) && null (cab_eta_tvs branch))
      guard (equalLength (cab_roles branch) (cab_tvs branch) && all (== Nominal) (cab_roles branch))
      (parameters, pattern_ty) <- snocView (cab_lhs branch)
      vars <- traverse plain_var parameters
      guard (map tyVarKind vars `eqTypes` captures)
      (pattern_tc, arguments) <- splitTyConAppNoView_maybe pattern_ty
      con <- isPromotedDataCon_maybe pattern_tc
      guard (dataConTyCon con == parent)
      fields <- fields_of con pattern_tc arguments pattern_ty
      let all_vars = vars ++ fields
          bound = mkVarSet (cab_tvs branch)
      guard (all_vars `equalLength` cab_tvs branch)
      guard (sizeVarSet bound == length all_vars && all (`elemVarSet` bound) all_vars)
      guard (sizeVarSet (mkVarSet all_vars) == length all_vars)
      let result = cab_rhs branch
      guard (tyCoVarsOfType result `subVarSet` bound)
      pure (con, (all_vars, result))

    same_row (con1, (vars1, result1)) (con2, (vars2, result2)) = do
      guard (con1 == con2)
      case (bare_result result1, bare_result result2) of
        (Just tycon1, Just tycon2) -> do
          guard (tycon1 == tycon2)
          pure False
        _ -> do
          env <- eqVarBndrs (mkRnEnv2 emptyInScopeSet) vars1 vars2
          guard (eqTypeX env result1 result2)
          pure True

    bare_result ty = do
      (result, []) <- splitTyConAppNoView_maybe ty
      guard (isAlgTyCon result && isLiftedTypeKind (tyConKind result))
      pure result

    checked_fields True _ _ _ _ [] _ = Just []
    checked_fields True _ _ _ _ _ _ = Nothing
    checked_fields False parent_args scrutinee_kind con pattern_tc arguments pattern_ty =
      checkedCaseFields parent_args scrutinee_kind con pattern_tc arguments pattern_ty

    compare_names get_name a b = stableNameCmp (get_name a) (get_name b)

-- | Recover the owner of a checked retained signature case.
signatureCaseOwner :: TyCon -> Maybe Name
signatureCaseOwner tc = do
  (owner, False) <- closedFamilyScope_maybe tc
  owner_module <- nameModule_maybe owner
  guard (Just owner_module == nameModule_maybe (tyConName tc))
  guard ("$signature-case:" `isPrefixOf` occNameString (nameOccName owner))
  pure owner

caseOcc :: TyCon -> String
caseOcc = occNameString . nameOccName . tyConName

isSignatureCase :: TyCon -> Bool
isSignatureCase tc
  | Just owner <- signatureCaseOwner tc
  = ("$case:" ++ occNameString (nameOccName owner) ++ ":") `isPrefixOf` caseOcc tc
  | otherwise = False

plain_var :: Type -> Maybe TyVar
plain_var (TyVarTy var) = Just var
plain_var _ = Nothing

checkedCaseFields :: [Type] -> Type -> DataCon -> TyCon -> [Type] -> Type -> Maybe [TyVar]
checkedCaseFields parent_args scrutinee_kind con pattern_tc arguments pattern_ty = do
  guard (isVanillaDataCon con && null (dataConRepArgBinders con))
  guard (dataConUnivTyVars con `equalLength` parent_args)
  guard (arguments `lengthIs` tyConArity pattern_tc)
  let (parameters, fields) = splitAt (length parent_args) arguments
  guard (parameters `eqTypes` parent_args && typeKind pattern_ty `eqType` scrutinee_kind)
  vars <- traverse plain_var fields
  let field_kinds = map scaledThing (dataConInstOrigArgTys con parent_args)
  guard (map tyVarKind vars `eqTypes` field_kinds)
  pure vars

-- | The owner and exact generated prefix for the nodes of one ordered case.
type OrderedCaseScope = (Name, String)

orderedCaseScope :: TyCon -> Maybe OrderedCaseScope
orderedCaseScope tc = do
  owner <- signatureCaseOwner tc
  let prefix = "$case-node:" ++ occNameString (nameOccName owner) ++ ":"
  if isSignatureCase tc
    then pure (owner, prefix ++ zEncodeString (caseOcc tc) ++ ".")
    else do
      node <- stripPrefix prefix (caseOcc tc)
      (root, '.' : number) <- pure (break (== '.') node)
      guard (not (null root) && not (null number) && all isDigit number)
      pure (owner, prefix ++ root ++ ".")

-- | Check the two decision trees for the same saturated applications.
-- See Note [Equality of ordered retained case trees].
eqOrderedCases :: Type -> Type -> Bool
eqOrderedCases lhs rhs
  | Just (tc1, as1) <- splitTyConApp_maybe lhs
  , Just (tc2, as2) <- splitTyConApp_maybe rhs
  , tc1 /= tc2
  , Just left <- orderedCaseScope tc1
  , Just right <- orderedCaseScope tc2
  , isSignatureCase tc1 == isSignatureCase tc2
  , as1 `lengthIs` tyConArity tc1, as2 `lengthIs` tyConArity tc2
  , as1 `eqTypes` as2
  , Just () <- same_node left right [] tc1 tc2
  = True
  | otherwise = False
  where
    owned_rhs (owner, prefix) ty = do
      (tc, args) <- splitTyConAppNoView_maybe ty
      guard (signatureCaseOwner tc == Just owner)
      number <- stripPrefix prefix (caseOcc tc)
      guard (not (null number) && all isDigit number)
      guard (args `lengthIs` tyConArity tc)
      pure (tc, args)

    same_node left right seen tc1 tc2 = do
      guard (not (any (\(a, b) -> a == tc1 || b == tc2) seen))
      guard (tyConKind tc1 `eqType` tyConKind tc2)
      rows1 <- checkedOrderedRows tc1
      rows2 <- checkedOrderedRows tc2
      guard (equalLength rows1 rows2)
      -- Leave flat signature roots to their existing checker.
      guard (not (isSignatureCase tc1) || any (has_child left) rows1)
      guard (not (isSignatureCase tc2) || any (has_child right) rows2)
      let has_same_rhs env a b =
            case (owned_rhs left a, owned_rhs right b) of
              (Just (child1, args1), Just (child2, args2)) -> do
                guard (equalLength args1 args2 && and (zipWith (eqTypeX env) args1 args2))
                same_node left right ((tc1, tc2) : seen) child1 child2
              (Nothing, Nothing) -> guard (eqTypeX env a b)
              _ -> Nothing
          same_row (test1, vars1, a) (test2, vars2, b) = do
            guard (test1 == test2)
            env <- eqVarBndrs (mkRnEnv2 emptyInScopeSet) vars1 vars2
            has_same_rhs env a b
      zipWithM_ same_row rows1 rows2

    has_child scope (_, _, rhs) =
      case owned_rhs scope rhs of
        Just _ -> True
        Nothing -> False

-- | Recheck that a generated node is an exhaustive constructor test or a leaf.
checkedOrderedRows :: TyCon -> Maybe [(Maybe (Int, DataCon), [TyVar], Type)]
checkedOrderedRows tc = do
  guard (isLiftedTypeKind (tyConResKind tc))
  NotInjective <- pure (tyConInjectivityInfo tc)
  let kinds = map (tyVarKind . binderVar) (tyConBinders tc)
  guard (all (isEmptyVarSet . tyCoVarsOfType) kinds)
  ax <- closedFamilyTyConCoAxiom_maybe tc
  guard (coAxiomTyCon ax == tc && coAxiomRole ax == Nominal)
  guard (null (co_ax_source_proof_rules ax))
  rows <- traverse (checked_row kinds) (fromBranches (coAxiomBranches ax))
  case rows of
    [(Nothing, _, _)] -> pure rows
    (Just (slot, first), _, _) : _ -> do
      let parent = dataConTyCon first
          tests = [ c | (Just (i, c), _, _) <- rows, i == slot, dataConTyCon c == parent ]
          ordered = sortBy (\a b -> stableNameCmp (dataConName a) (dataConName b))
      constructors <- tyConDataCons_maybe parent
      guard (equalLength rows tests && ordered tests == ordered constructors)
      pure rows
    _ -> Nothing
  where
    checked_row kinds branch = do
      guard (null (cab_cvs branch) && null (cab_eta_tvs branch))
      guard (equalLength (cab_roles branch) (cab_tvs branch) && all (== Nominal) (cab_roles branch))
      let pats = cab_lhs branch
      guard (equalLength kinds pats)
      pieces <- zipWithM checked_pattern kinds pats
      let vars = concatMap snd pieces
          tests = [(slot, con) | (slot, (Just con, _)) <- zip [0..] pieces]
          pattern_vars = mkVarSet vars
          bound = mkVarSet (cab_tvs branch)
          result = cab_rhs branch
      guard (vars `equalLength` cab_tvs branch && sizeVarSet pattern_vars == length vars)
      guard (pattern_vars `subVarSet` bound)
      guard (tyCoVarsOfType result `subVarSet` bound)
      case tests of
        [] -> pure (Nothing, vars, result)
        [test] -> pure (Just test, vars, result)
        _ -> Nothing

    checked_pattern kind pat
      | Just var <- plain_var pat = do
          guard (tyVarKind var `eqType` kind)
          pure (Nothing, [var])
      | otherwise = do
          (parent, args) <- splitTyConAppNoView_maybe kind
          guard (isVanillaAlgTyCon parent && isBoxedDataTyCon parent && not (isGadtSyntaxTyCon parent))
          guard (null (tyConStupidTheta parent) && args `lengthIs` tyConArity parent)
          (promoted, fields) <- splitTyConAppNoView_maybe pat
          con <- isPromotedDataCon_maybe promoted
          guard (dataConTyCon con == parent)
          vars <- checkedCaseFields args kind con promoted fields pat
          pure (Just con, vars)

{- Note [Equality of ordered retained case trees]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ordered case builder emits a closed family per decision. Keep the
signature-case owner on the root and its nodes. Compare the emitted
decisions in order. Exchanging two disjoint source rows can change
which field the case forces first. A wildcard can also cover a branch
without forcing another field. Keep the tested column and the actual
arguments passed to each child.

Each checked decision has exactly one constructor column. All of its
rows test distinct constructors of the same ordinary datatype and
cover every constructor. A leaf has one variable-only row. Pair the
variables by pattern position and compare the result under those
binders. Follow only saturated child calls under the exact generated
prefix and owner. Require the same finite shape on both sides and
reject a repeated node on either path. A user family or an unrelated
case can occur in a leaf, where it keeps its ordinary nominal identity.

Rewriting can expose a child before equality solving, for example
when the outer Maybe constructor is known. Its generated prefix
still identifies its tree. Match the same saturated inputs, even
when a later field remains unknown. This cannot select a result for
either side. It only transports the equality between the two trees.

The original case rules still compare flat signature roots. Old home
interfaces can contain unscoped decision nodes. Recheck those
providers to record the owner. An old installed interface still
supports ordinary reduction and needs a rebuild to supply the owner.
The existing scoped family format carries that owner. Only a stored
proof that uses ScopedOrderedCase requires the new interface marker.
-}

{- Note [Equality of retained constructor cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The checker records the signature-case owner from the renamed source
rows. Each family retains its ordinary reduction axioms. The additional
nominal rule can compare two such families with the same scrutinee when
ordinary rewriting leaves their applications stuck.

Read the immutable axioms on demand. Require one exhaustive split over
the same unindexed datatype. Each row has unrelated capture variables
and one exact constructor pattern. Its fields may each bind a distinct
variable. Require a regular boxed datatype and ordinary constructors
when a row binds a field. Check that the constructor parameters match the
scrutinee kind and that each field variable has the instantiated field
kind. Distinct exact constructors establish disjointness. Do not force
the pairwise overlap caches.

Compare bare algebraic results by constructor identity as before.
For another result, check that its free variables are bound by the
row. Pair the capture binders in application order and the fields in
constructor order. Check their kinds with eqVarBndrs and compare the
results with eqTypeX. The order in cab_tvs need not be the pattern
order. In particular, two fields with the same kind are still distinct
positions.

Keep ScopedEnumCase for proofs the original nullary checker accepts
and ScopedConstructorCase for the original ignored-field proofs.
Use ScopedCaseResult when comparison needs the scoped result rule.
The distinct name and interface feature let old readers reject only a
stored proof that requires the new rule.

Recheck this shape when reading the coercion. A substitution can make
the scrutinee known to both families. The same row then reduces on both
sides, so the proof remains valid.
See Note [Equality of ordered retained case trees] for nested cases.
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
