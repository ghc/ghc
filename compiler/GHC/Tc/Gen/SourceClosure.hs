module GHC.Tc.Gen.SourceClosure (publishCheckedSourceClosures) where

import GHC.Prelude

import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, zipWithM, zipWithM_)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Fix (mfix)
import Data.Foldable (traverse_)
import Data.List (foldl', nubBy, sortOn)
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)

import GHC.Builtin.KnownKeys (patErrorIdKey)
import GHC.Builtin.WiredIn.Types (manyDataConTy, unmatchableDataConTy)
import GHC.Core (CoreBind, CoreExpr)
import qualified GHC.Core as Core
import GHC.Core.Coercion (coercionLKind, coercionRKind, coercionRole)
import GHC.Core.Coercion.Axiom
  ( CoAxBranch, ClosedTyFam(CTF), Role(Nominal)
  , coAxBranchLHS, coAxBranchRHS, coAxBranchTyVars )
import qualified GHC.Core.DataCon as DC
import GHC.Core.FamInstEnv (emptyFamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.FVs (exprFreeVars, exprSomeFreeVarsList)
import GHC.Core.OpenWhnf (checkedMultiplicityCast, openDataConFieldTypes, openIndexedDataCon)
import GHC.Core.Predicate (tyCoVarsOfTypesWellScoped)
import GHC.Core.SimpleOpt (simpleOptExpr)
import GHC.Core.Subst (extendTvSubst, mkEmptySubst, mkOpenSubst, substExpr)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.Rep (Coercion(..), MCoercion(MRefl), Scaled(..), Type(TyVarTy), scaledThing)
import GHC.Core.TyCon
  ( FamTyConFlav(ClosedTypeFamilyTyCon), AlgTyConFlav(VanillaAlgTyCon)
  , TyCon, TyConBinder, Injectivity(NotInjective), mkAlgTyCon
  , mkLevPolyDataTyConRhs, mkTyConKind, mkAnonTyConBinder, mkNamedTyConBinder, mkNamedTyConBinders
  , mkFamilyTyCon, mkTyConKindWithMatchability, isAlgTyCon, isPromotedDataCon
  , tyConDataCons, tyConKind, tyConName )
import GHC.Core.Type
  ( Kind, getTyVar_maybe, isLiftedTypeKind, isManyTy, liftedTypeKind, lookupTyVar
  , mkAppTys, mkSpecForAllTys, mkTyConApp, mkTyVarTy, mkVisFunTyManyU
  , mkTyVarTys, splitForAllForAllTyBinders, splitForAllTyVars, splitFunTys, splitTyConApp_maybe, substTyUnchecked, substTyWith
  , tyCoVarsOfTypes, typeKind )
import GHC.Core.Unify (alwaysBindTv, tcMatchTys, tcUnifyTys)
import GHC.Core.Utils (exprType, stripTicksE)
import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.DynFlags (HasDynFlags(getDynFlags))
import GHC.Tc.Gen.SourceTable
  ( hasSourceMethodRule, ordinaryImport, publicCaseType, sourceEquationDatatypes
  , visibleSourceMethodRules )
import GHC.Tc.TyCl.Build (buildDataCon, newImplicitBinder, newTyConRepName)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Monad (TcGblEnv(..), TcM, getGblEnv, getGlobalRdrEnv, newSysName)
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Types.Avail (availsToNameSet)
import GHC.Types.Id (Id, idDetails, idName, isDataConWorkId_maybe, isDataConWrapId_maybe)
import GHC.Types.Id.Info (IdDetails(CheckedClosureSourceTableId, SourceTableId, VanillaId))
import GHC.Types.Basic (SourceTableOrigin(NominalSourceTable))
import GHC.Types.Id.Make (DataConBangOpts(FixedBangOpts))
import GHC.Types.Name
  ( Name, isExternalName, isWiredInName, mkDataOcc, mkInstTyCoOcc, mkSourceTableOcc, mkTcOcc
  , mkTyVarOcc, nameModule_maybe, nameStableString, occNameString )
import GHC.Types.Name.Env (mkNameEnv)
import GHC.Types.Name.Set (elemNameSet, mkNameSet, nameSetElemsStable)
import GHC.Types.SourceText (SourceText(NoSourceText))
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Tickish (GenTickish(Breakpoint))
import GHC.Types.TyThing (TyThing(AnId))
import GHC.Types.TypeEnv (TypeEnv, lookupTypeEnv)
import GHC.Types.Var
  ( ForAllTyFlag(Specified), TyVar, VarBndr(Bndr), isCoVar, isId, isLocalId, isTyVar
  , Var, mkTyVar, mkTyVarBinders, setIdDetails, varType )
import GHC.Types.Unique (hasKey)
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Types.Var.Set (elemVarSet, mkVarSet, subVarSet, unionVarSet)

{- Note [Certifying a checked source closure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A rank-2 substitution callback can replace a prefix of the source
context and cross binders. Private TypeData constructors represent
the first replacement, another replacement over a checked closure,
one-binder shift, and lifting. Two nominal families interpret the
closures over term and variable constructors. The source owner names
a third family that supplies the replacements in binder order.

Check the original and final Core before allocating the families.
A traversal must scrutinize its input, cover every constructor, and
rebuild each lazy field from a recursive call on the corresponding
original field. The call can use the original callback or a checked
extension. Check the kind of every generated branch against the
promoted constructor telescope. A neutral term blocks reduction.

The subtree calls descend through the original fields. Each lookup
under a binder consumes one level of lifting. A shift can traverse
the resulting term, but cannot reintroduce a replacement. An
unavailable scrutinized field leaves the corresponding match stuck.
The rebuilt fields must stay lazy in both traversals.

Check the source datatypes of all generated equations, including the
private interpreters. Their source types and constructors must be
visible under the ordinary source equation policy. The generated
closure datatype belongs to the certificate and is private.

Keep the actual owner and all reachable external helper Names from
both Core graphs in IdDetails. A generated internal helper has no
interface Name and cannot be the target of a downstream rule. Check
its Core and traverse its dependencies before filtering Names.
Withhold publication if any external Name is absent after Tidy or
if a visible rule can rewrite a checked function. See Note [Checked
closure dependencies] in GHC.Tc.Gen.SourceTable.

GHCi inserts breakpoint ticks at source spans, including in the middle
of an application. Inspect the Core with only those ticks removed.
A breakpoint can keep a lifted let between the type and value lambdas
of a callback. After removing the ticks, skip the let in the function
view if the binder has no remaining use. Keep the unmodified Core for
bytecode generation, including the variables used by the debugger.
-}

data CallbackLift = NamedLift Id | InlinedRename | InlinedSubst Id

data FamilyInputs = FamilyInputs Id Int Kind Kind TyCon TyCon TyCon (Id, Id)
                                  DC.DataCon DC.DataCon DC.DataCon [(DC.DataCon, [Int])]

-- | Publish tables for eligible final Core bindings. The caller checks
-- the source-equation opt-in and excludes plugins and local user rules.
publishCheckedSourceClosures :: [CoreBind] -> [CoreBind] -> TypeEnv -> TcM ([Id], [TyCon])
publishCheckedSourceClosures original_binds final_binds final_types = do
    env <- getGblEnv
    rdr_env <- getGlobalRdrEnv
    opts <- initSimpleOpts <$> getDynFlags
    let here = Just (tcg_mod env)
        exported = availsToNameSet (tcg_exports env)
        imported name = isWiredInName name || ordinaryImport rdr_env name
        public = publicCaseType env exported imported
        unoptimized = inspect original_binds
        original = [(v, simpleOptExpr opts rhs) | (v, rhs) <- unoptimized]
        final = inspect final_binds
        inspect binds = [(v, stripTicksE breakpoint rhs) | (v, rhs) <- Core.flattenBinds binds]
        breakpoint Breakpoint{} = True
        breakpoint _ = False
        lookupLocal name = case lookupTypeEnv final_types name of
          Just (AnId actual) | idName actual == name
                             , nameModule_maybe name == here -> Just actual
          _ -> Nothing
        lookupOwner old = do
          let owner = idName old
          guard (isExternalName owner && owner `elemNameSet` exported)
          actual <- lookupLocal owner
          VanillaId <- pure (idDetails actual)
          pure actual
        dependencies include old actual = do
          let helpers = nameSetElemsStable $ mkNameSet $
                filter (\name -> name /= idName old && include name)
                       (reachable original old ++ reachable final actual)
          traverse lookupLocal helpers
        candidate (old, original_rhs) = do
          actual <- lookupOwner old
          original_shape <- familyInputs original old original_rhs
          final_rhs <- lookup actual final
          shape@(FamilyInputs _ _ _ _ term ext index _ _ _ _ _) <- familyInputs final actual final_rhs
          guard (sameShapes original_shape shape)
          guard (all ((== here) . nameModule_maybe . tyConName) [term, ext, index])
          helper_ids <- dependencies isExternalName old actual
          guard (not (null helper_ids))
          pure (actual, (original_shape, shape), helper_ids)
        indexed_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          let decoder = (,,) False <$> indexedSource original old original_rhs
                                   <*> indexedSource final actual final_rhs
              encoder = (,,) True <$> indexedEncoder original old original_rhs
                                  <*> indexedEncoder final actual final_rhs
          shapes@(is_encoder, _, _) <- decoder <|> encoder
          -- The decoder retains every helper identity. The encoder checks
          -- a floated type-only body, which can have an internal Name.
          helper_ids <- dependencies (\name -> not is_encoder || isExternalName name) old actual
          pure (actual, shapes, helper_ids)
        -- See Note [Renaming with an abstract callback].
        renaming_candidate (_, (original_shape, shape), _) = do
          let (rename, extend) = renamingOperations original_shape
              (final_rename, final_extend) = renamingOperations shape
          guard (idName rename == idName final_rename && idName extend == idName final_extend)
          actual_rename <- lookupOwner final_rename
          actual_extend <- lookupOwner final_extend
          original_extension <- lookup extend unoptimized
          final_extension <- lookup actual_extend final
          let helper v = isValueVar v &&
                         not (isJust (isDataConWorkId_maybe v <|> isDataConWrapId_maybe v))
          guard (all (null . exprSomeFreeVarsList helper) [original_extension, final_extension])
          helpers <- dependencies isExternalName rename actual_rename
          guard (actual_extend `elem` helpers)
          pure (actual_rename, (actual_extend, shape), helpers)
        lookup_candidate (old, original_rhs) = do
          actual <- lookupOwner old
          final_rhs <- lookup actual final
          startup <- indexedLookup original old original_rhs
          optimized <- indexedLookup final actual final_rhs
          pure (actual, (startup, optimized), [])
    case (mapMaybe candidate original, mapMaybe indexed_candidate original,
          mapMaybe lookup_candidate original) of
      ([], [], []) -> pure ([], [])
      (closures, indexed, lookups) -> do
        visible <- visibleSourceMethodRules env
        let safe :: (Id, a, [Id]) -> Bool
            safe (owner, _, helpers) = all (not . hasSourceMethodRule visible) (owner:helpers)
        checked_closures <- catMaybes <$> traverse (publish public) (filter safe closures)
        checked_indexed <- catMaybes <$> traverse (publishIndexed public) (filter safe indexed)
        let renamings = filter safe (mapMaybe renaming_candidate closures)
            sameRename (owner, _, _) (other, _, _) = owner == other
            sameExtension (_, (ext, _), _) (_, (other, _), _) = ext == other
        extensions <- catMaybes <$> traverse (publishRenameExtension public) (nubBy sameExtension renamings)
        let extension_tables = [(idName ext, table) | (ext, [table]) <- extensions]
        checked_renamings <- catMaybes <$> traverse (publishRenaming public extension_tables) (nubBy sameRename renamings)
        checked_lookups <- catMaybes <$> traverse (publishIndexedLookup public) (filter safe lookups)
        let accepted = checked_closures ++ checked_indexed ++ extensions ++ checked_renamings ++ checked_lookups
        pure (map fst accepted, concatMap snd accepted)
  where
    publish public (owner, (_, shape), helpers) = do
      generated <- allocateFamilies shape
      pure $ do
        (root, families) <- generated
        guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
        let new = setIdDetails owner (CheckedClosureSourceTableId (idName owner) root (map idName helpers))
        pure (new, families)

{- Note [Checked indexed reconstruction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A unary function can open an existential package and recurse on one of
its fields. In a recursive branch, the new package must contain a
proper subfield of the selected field and the exact other fields.
The recursive call must appear below a lazy result constructor.

The final wrapper can pass the package's fields to a worker. Recover
that worker only when the wrapper forwards all erased and value fields
in order. Read its recursive call as an application of the original
owner to the reconstructed package. Compare the complete equations
from original and final Core before publishing.

Specialize each row with the nominal equalities bound by its actual
constructor alternatives. Check each promoted constructor at the
resulting kind. A cast can disappear only if its specialized endpoints
agree and its evidence comes from those alternatives. The equations
use the original closed family mechanism. They preserve a recursive
call whose argument is still abstract.

First check for a unary function with a case body. An ordinary exported
value can construct a retained package with an unused witness. Computing
the type of its optimized application would require an index for that
witness. The value has no traversal equations to publish.
-}

data IndexedPattern
  = IndexedVariable Id
  | IndexedConstructor Type DC.DataCon [Type] [IndexedPattern]

data IndexedRow = IndexedRow IndexedPattern [Var] CoreExpr

-- One complete case on a recursive result. The operand is a proper
-- subfield of an input alternative.
data IndexedResultCase = IndexedResultCase [Type] Id IndexedPattern CoreExpr

-- The worker's additional type arguments are the package existentials.
data IndexedSource = IndexedSource [TyVar] Type Type (Maybe Id) [(Id, CoreExpr)] [IndexedRow]

-- | Expose exhaustive case paths and one exact worker wrapper.
indexedSource :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedSource
indexedSource bindings owner rhs = do
    (parameters, input, body) <- indexedFunction owner rhs
    [(pack, binders, nested)] <- caseOn input body
    (_, exs@(_:_), [], [], _, _) <- pure (DC.dataConFullSig pack)
    guard (DC.dataConTyCon pack `hasOnly` pack)
    (outer, proofs) <- indexedAlt input pack binders
    let (existentials, fields) = splitAt (length exs) binders
    (worker, tree) <- unwrap (parameters ++ existentials) fields nested
    guard (not (null (exprSomeFreeVarsList (== fromMaybe owner worker) tree)))
    rows <- descend outer proofs tree
    guard (length rows > 1)
    pure (IndexedSource parameters (varType input) (exprType body) worker [] rows)
  where
    tc `hasOnly` con = tyConDataCons tc == [con]

    unwrap _ _ body@Core.Case{} =
      pure (Nothing, body)
    unwrap tys fields body = do
      (Core.Var worker, args) <- pure (Core.collectArgs body)
      guard (worker /= owner && nameModule_maybe (idName worker) == nameModule_maybe (idName owner))
      let (passed_types, passed_fields) = splitAt (length tys) args
      supplied <- traverse indexedTypeArg passed_types
      guard (eqTypes supplied (mkTyVarTys tys) &&
             length passed_fields == length fields &&
             and (zipWith isVar passed_fields fields))
      declaration <- lookup worker bindings
      (worker_tys, worker_fields, tree) <- function declaration
      let binders = worker_tys ++ worker_fields
          free = foldl' unionVarSet (exprFreeVars tree) (map exprFreeVars args)
          expanded = substExpr (mkOpenSubst (mkInScopeSet free) (zip binders args)) tree
      guard (length worker_tys == length tys && length worker_fields == length fields &&
             exprType declaration `eqType` varType worker &&
             exprType expanded `eqType` exprType body)
      pure (Just worker, expanded)

    descend pat proofs expr@(Core.Case (Core.Var scrutinee) binder result alternatives) = do
      guard (length alternatives > 0 && varType binder `eqType` varType scrutinee &&
             result `eqType` exprType expr &&
             scrutinee `elem` indexedFields pat)
      (tc, _) <- splitTyConApp_maybe (varType scrutinee)
      alts <- traverse read_alt alternatives
      guard (map (\(con, _, _) -> con) alts == tyConDataCons tc)
      concat <$> traverse (branch pat proofs scrutinee) alts
    descend pat proofs body = pure [IndexedRow pat proofs body]

    branch pat proofs scrutinee (con, binders, body) = do
      (node, equalities) <- indexedAlt scrutinee con binders
      descend (replace scrutinee node pat) (proofs ++ equalities) body

    read_alt (Core.Alt (Core.DataAlt con) fields body) = Just (con, fields, body)
    read_alt _ = Nothing

    replace v node (IndexedVariable field) | v == field = node
    replace v node (IndexedConstructor ty con source fields) =
      IndexedConstructor ty con source (map (replace v node) fields)
    replace _ _ pat = pat

    isVar (Core.Var actual) expected = actual == expected
    isVar _ _ = False

{- Note [Checked cases on recursive results]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A traversal can inspect the existential package returned by a smaller
call before it constructs its result. A family for the result case
preserves this demand. For a branch with E (There r) = H (E r), H
cannot reduce until it sees the package constructor. The family
telescope includes the indices of the recursive package and the
enclosing result, after specialization by the input branch.

Check one exhaustive case on the direct recursive call. The call
must consume an input field, and the result branch can use only
fields of the package it scrutinizes. Check both Core versions
including the private family equation. The simplifier can float
a constructor expression into a type-only helper. Check the body
of that helper at its exact type application. Do not treat an
arbitrary unfolding or function call as a constructor.
-}

-- | Find an indexed traversal with a complete case over its input.
indexedEncoder :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedSource
indexedEncoder bindings owner rhs = do
    (parameters, input, body) <- indexedFunction owner rhs
    (tc, _) <- splitTyConApp_maybe (varType input)
    alts <- caseOn input body
    guard (length alts > 1 && [con | (con, _, _) <- alts] == tyConDataCons tc)
    rows <- traverse (read_alt input) alts
    guard (length [() | IndexedRow pat _ branch <- rows,
                        isJust (indexedResultCase owner parameters pat branch)] == 1)
    pure (IndexedSource parameters (varType input) (exprType body) Nothing bindings rows)
  where
    read_alt input (con, binders, body) = do
      (pat, proofs) <- indexedAlt input con binders
      pure (IndexedRow pat proofs body)

-- | Check the ordinary unary telescope before reading case branches.
indexedFunction :: Id -> CoreExpr -> Maybe ([TyVar], Id, CoreExpr)
indexedFunction owner rhs = do
    (parameters, [input], body@Core.Case{}) <- function rhs
    guard (exprType rhs `eqType` varType owner)
    let (_, tau) = splitForAllTyVars (exprType rhs)
    (_, [Scaled mult input_kind], output_kind) <- pure (splitFunTys tau)
    guard (isManyTy mult && input_kind `eqType` varType input &&
           output_kind `eqType` exprType body &&
           all (isLiftedTypeKind . typeKind) [input_kind, output_kind])
    pure (parameters, input, body)

-- | Check the case and its direct, structurally smaller scrutinee.
indexedResultCase :: Id -> [TyVar] -> IndexedPattern -> CoreExpr -> Maybe IndexedResultCase
indexedResultCase owner parameters pat expr = do
    Core.Case call binder kind [Core.Alt (Core.DataAlt con) binders body] <- pure expr
    (Core.Var callee, args) <- pure (Core.collectArgs call)
    let (prefix, suffix) = splitAt (length parameters) args
    tys <- traverse indexedTypeArg prefix
    [Core.Var operand] <- pure suffix
    (tc, _) <- splitTyConApp_maybe (varType binder)
    (_, _ : _, [], [], _, _) <- pure (DC.dataConFullSig con)
    guard (callee == owner && length tys == length parameters && operand `elem` indexedFields pat &&
           varType binder `eqType` exprType call && kind `eqType` exprType body &&
           tyConDataCons tc == [con])
    (pattern, []) <- indexedAlt binder con binders
    pure (IndexedResultCase tys operand pattern body)

indexedTypeArg :: CoreExpr -> Maybe Type
indexedTypeArg (Core.Type ty) = Just ty
indexedTypeArg _ = Nothing

indexedFields :: IndexedPattern -> [Id]
indexedFields (IndexedVariable v) = [v]
indexedFields (IndexedConstructor _ _ _ fields) = concatMap indexedFields fields

-- | Check the ordinary alternative telescope before using its equalities.
indexedAlt :: Id -> DC.DataCon -> [Var] -> Maybe (IndexedPattern, [Var])
indexedAlt scrutinee con binders = do
    let kind = varType scrutinee
    (tc, args) <- splitTyConApp_maybe kind
    (universals, exs, specs, [], fields, _) <- pure (DC.dataConFullSig con)
    let (locals, remaining) = splitAt (length exs) binders
        (proofs, values) = splitAt (length specs) remaining
        instantiate = substTyWith (universals ++ exs) (args ++ mkTyVarTys locals)
    guard (tc == DC.dataConTyCon con && length args == length universals &&
           length locals == length exs && all isTyVar locals &&
           eqTypes (map (instantiate . varType) exs) (map varType locals) &&
           length proofs == length specs && all isCoVar proofs &&
           all isValueVar values && eqTypes (map (instantiate . scaledThing) fields) (map varType values) &&
           null (DC.dataConRepArgBinders con) && null (DC.dataConStupidTheta con) &&
           all (not . DC.isBanged) (DC.dataConImplBangs con))
    zipWithM_ (check_proof instantiate) specs proofs
    let source = map (instantiate . mkTyVarTy) (DC.dataConUserTyVars con)
    pure (IndexedConstructor kind con source (map IndexedVariable values), proofs)
  where
    check_proof instantiate spec binder = do
      let (tv, ty) = DC.eqSpecPair spec
          co = CoVarCo binder
      guard (coercionRole co == Nominal &&
             coercionLKind co `eqType` instantiate (mkTyVarTy tv) &&
             coercionRKind co `eqType` instantiate ty)

-- | Publish closed families after both compiler stages justify their rows.
publishIndexed :: (TyCon -> Bool) -> (Id, (Bool, IndexedSource, IndexedSource), [Id])
               -> TcM (Maybe (Id, [TyCon]))
publishIndexed public (owner, (is_encoder, original, final), helpers) = runMaybeT $ do
    let IndexedSource parameters domain result _ _ _ = original
        (flags, _) = splitForAllForAllTyBinders (varType owner)
    guard (length flags == length parameters)
    name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (newImplicitBinder name mkInstTyCoOcc)
    operand <- lift (freshVar "operand" domain)
    let binders = zipWith (\(_, Bndr _ flag) -> mkNamedTyConBinder flag) flags parameters ++ [mkAnonTyConBinder operand]
        make = mkFamily name axiom binders result
        header = make []
    private <- if is_encoder then Just <$> lift allocate_case else pure Nothing
    original_rows <- indexedEquations owner header private original
    final_rows <- indexedEquations owner header private final
    guard (length original_rows == length final_rows)
    zipWithM_ (same_equation header) original_rows final_rows
    let table = make [applyRow row table | (row, _) <- original_rows]
        families = table : [tc | (_, Just (tc, _)) <- original_rows]
        details = case helpers of
          [] -> SourceTableId (idName owner) table NominalSourceTable
          ids -> CheckedClosureSourceTableId (idName owner) table (map idName ids)
    guard (all (\tc -> tc `elem` families || public tc) (sourceEquationDatatypes families))
    pure (setIdDetails owner details, families)
  where
    allocate_case = do
      name <- newImplicitBinder (idName owner) (mkTcOcc . ("$case:" ++) . occNameString)
      axiom <- newImplicitBinder name mkInstTyCoOcc
      pure (name, axiom)

    same_equation header (old, old_case) (new, new_case) = do
      sameIndexedRow (applyRow old header) (applyRow new header)
      case (old_case, new_case) of
        (Nothing, Nothing) -> pure ()
        (Just (left, lhs), Just (right, rhs)) -> do
          guard (tyConKind left `eqType` tyConKind right)
          sameIndexedRow lhs rhs
        _ -> fail "different indexed result cases"

-- | Compare two generated rows with a bijection between their binders.
sameIndexedRow :: CoAxBranch -> CoAxBranch -> MaybeT TcM ()
sameIndexedRow left right = do
    let old_lhs = coAxBranchLHS left
        new_lhs = coAxBranchLHS right
    subst <- MaybeT (pure (tcMatchTys old_lhs new_lhs))
    renamed <- MaybeT $ pure $ traverse (\v -> lookupTyVar subst v >>= getTyVar_maybe) (coAxBranchTyVars left)
    guard (length renamed == length (coAxBranchTyVars right) &&
           mkVarSet renamed == mkVarSet (coAxBranchTyVars right) &&
           substTyUnchecked subst (coAxBranchRHS left) `eqType` coAxBranchRHS right)

data IndexedEquation = IndexedEquation [TyVar] [Type] (TyCon -> Type)

applyRow :: IndexedEquation -> TyCon -> CoAxBranch
applyRow (IndexedEquation vars lhs rhs) family = mkBranch vars lhs (rhs family)

-- | Instantiate only the actual case equalities. Translate the resulting row.
indexedEquations :: Id -> TyCon -> Maybe (Name, Name) -> IndexedSource
                 -> MaybeT TcM [(IndexedEquation, Maybe (TyCon, CoAxBranch))]
indexedEquations owner header private (IndexedSource parameters domain result worker bindings rows) =
    traverse row rows
  where
    row (IndexedRow pat proofs body) = do
      let equalities = map CoVarCo proofs
      subst <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
          (map coercionLKind equalities) (map coercionRKind equalities)
      let specialize = substTyUnchecked subst
          args = map (specialize . mkTyVarTy) parameters
          result_kind = specialize result
      (packed, fields) <- indexedPatternTypes specialize pat
      (value, helper) <- case (private, indexedResultCase owner parameters pat body) of
        (Just names, Just alternative) ->
          result_case names result_kind specialize proofs fields alternative
        _ -> do
          value <- MaybeT $ pure $
            indexedResult bindings owner worker parameters domain header specialize proofs pat fields body
          pure (value, Nothing)
      let lhs = args ++ [packed]
          vars = tyCoVarsOfTypesWellScoped lhs
          branch = IndexedEquation vars lhs value
      guard (all isTyVar vars && typeKind (value header) `eqType` result_kind &&
             validBranch (applyRow branch header))
      pure (branch, helper)

    result_case (name, axiom) result_kind specialize proofs fields
                (IndexedResultCase types operand pat body) = do
      let tys = map specialize types
          input_kind = substTyWith parameters tys domain
          case_kind = substTyWith parameters tys result
          locals = tyCoVarsOfTypesWellScoped [case_kind, result_kind]
          vars = mkTyVarTys locals
      guard (all isTyVar locals)
      argument <- MaybeT (pure (lookup operand fields))
      guard (typeKind argument `eqType` input_kind)
      (packed, case_fields) <- indexedPatternTypes specialize pat
      guard (typeKind packed `eqType` case_kind)
      body_value <- MaybeT $ pure $
        indexedResult bindings owner Nothing parameters domain header specialize proofs pat case_fields body
      let rhs = body_value header
          lhs = vars ++ [packed]
          branch_vars = tyCoVarsOfTypesWellScoped lhs
          branch = mkBranch branch_vars lhs rhs
      guard (all isTyVar branch_vars && typeKind rhs `eqType` result_kind && validBranch branch)
      operand_ty <- lift (freshVar "case" case_kind)
      let binders = mkNamedTyConBinders Specified locals ++ [mkAnonTyConBinder operand_ty]
          helper = mkFamily name axiom binders result_kind [branch]
          value family = mkTyConApp helper (vars ++ [mkTyConApp family (tys ++ [argument])])
      pure (value, Just (helper, branch))

indexedPatternTypes :: (Type -> Type) -> IndexedPattern -> MaybeT TcM (Type, [(Id, Type)])
indexedPatternTypes specialize pat = do
    variables <- lift $ traverse (freshVar "field" . specialize . varType) ids
    let fields = zip ids (mkTyVarTys variables)
        write_pattern (IndexedVariable v) = lookup v fields
        write_pattern (IndexedConstructor kind con source children) =
          checkedPromoted con (specialize kind) (map specialize source) =<< traverse write_pattern children
    packed <- MaybeT (pure (write_pattern pat))
    pure (packed, fields)
  where
    ids = indexedFields pat

-- | Validate a promoted constructor and its exact erased worker layout.
checkedPromoted :: DC.DataCon -> Type -> [Type] -> [Type] -> Maybe Type
checkedPromoted con kind source fields = do
    (tc, parameters) <- splitTyConApp_maybe kind
    guard (tc == DC.dataConTyCon con)
    let names = DC.dataConUserTyVars con
        value = mkTyConApp (DC.promoteDataCon con) (source ++ fields)
        vanilla = do
          expected <- traverse (`lookup` zip (DC.dataConUnivTyVars con) parameters) names
          guard (eqTypes expected source)
          openDataConFieldTypes con parameters
    expected <- vanilla <|> (\(_, _, tys) -> tys) <$> openIndexedDataCon con kind source
    guard (eqTypes (map typeKind fields) expected && typeKind value `eqType` kind)
    pure value

{- Note [Checked lookup over two indexed operands]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An environment lookup first matches the environment, then the position.
Both matches refine the scope index. Check the nominal equalities before
using either set of fields. A recursive call consumes one direct field
from each input, with both kinds at the same specialized index.

A missing environment branch is permissible only when no constructor
of the position can inhabit the corresponding scope. Desugaring emits
a pattern failure there. The simplifier can name this failure, so check
the body behind that local name. Publish only if the original and final
Core give the same equations.
-}

data IndexedLookup = IndexedLookup Id [TyVar] [Type] Type [(IndexedPattern, IndexedRow)]

-- | Read the two nested matches of a structurally decreasing lookup.
indexedLookup :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe IndexedLookup
indexedLookup bindings owner rhs = do
    (parameters@[scope], [left, right], Core.Case (Core.Var input) _ result alts) <- function rhs
    guard (input == left)
    let left_kind = varType left
        right_kind = varType right
    ([(scope_match, declared_scope)], tau) <- pure (splitForAllTyVars (varType owner))
    let instantiate = substTyWith [declared_scope] [mkTyVarTy scope]
    (matches, [Scaled left_mult left_type, Scaled right_mult right_type], result_type) <- pure (splitFunTys tau)
    guard (varType scope `eqType` varType declared_scope &&
           all (`eqType` unmatchableDataConTy) (scope_match : matches) &&
           all isManyTy [left_mult, right_mult] &&
           eqTypes (map instantiate [left_type, right_type, result_type]) [left_kind, right_kind, result])
    (left_tc, [left_scope]) <- splitTyConApp_maybe left_kind
    (right_tc, [right_scope]) <- splitTyConApp_maybe right_kind
    guard (eqTypes [left_scope, right_scope] (map mkTyVarTy [scope, scope]))
    [(con, binders, rest)] <- pure [(dc, bs, e) | Core.Alt (Core.DataAlt dc) bs e <- alts,
                                               not (patternFailure e)]
    guard (all (validOuterAlt con) alts)
    traverse_ (noPosition left_kind right_kind right_tc)
              (filter (/= con) (tyConDataCons left_tc))
    (outer, outer_proofs) <- indexedAlt left con binders
    [(first, first_bs, first_rhs), (next, next_bs, next_rhs)] <- caseOn right rest
    guard (not (null outer_proofs) && [first, next] == tyConDataCons right_tc)
    rows <- traverse (\(dc, bs, e) -> do
        (pat, proofs) <- indexedAlt right dc bs
        guard (not (null proofs))
        pure (outer, IndexedRow pat (outer_proofs ++ proofs) e))
        [(first, first_bs, first_rhs), (next, next_bs, next_rhs)]
    pure (IndexedLookup owner parameters [left_kind, right_kind] result rows)
  where
    validOuterAlt con (Core.Alt alt _ e) = alt == Core.DataAlt con || patternFailure e

    patternFailure (Core.Case scrut _ _ []) = failure scrut
    patternFailure _ = False

    failure (Core.Var v)
      | not (isExternalName (idName v))
      , Just rhs <- lookup v bindings
      = directFailure rhs
    failure expr = directFailure expr

    directFailure expr | (Core.Var v, _) <- Core.collectArgs expr = v `hasKey` patErrorIdKey
    directFailure _ = False

    noPosition left_kind right_kind right_tc con = do
      subst <- tcUnifyTys alwaysBindTv [left_kind] [DC.dataConOrigResTy con]
      (tc, args) <- splitTyConApp_maybe (substTyUnchecked subst right_kind)
      guard (tc == right_tc && all (DC.dataConCannotMatch args) (tyConDataCons right_tc))

-- | Publish the nominal rows after checking the field projection and recursion.
publishIndexedLookup :: (TyCon -> Bool) -> (Id, (IndexedLookup, IndexedLookup), [Id])
                     -> TcM (Maybe (Id, [TyCon]))
publishIndexedLookup public (owner, (original, final), _) = runMaybeT $ do
    let IndexedLookup _ parameters domains result _ = original
        (flags, _) = splitForAllForAllTyBinders (varType owner)
    guard (length flags == length parameters)
    name <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (newImplicitBinder name mkInstTyCoOcc)
    operands <- lift (traverse (freshVar "operand") domains)
    let binders = zipWith (\(_, Bndr _ flag) -> mkNamedTyConBinder flag) flags parameters ++
                  map mkAnonTyConBinder operands
        make = mkFamily name axiom binders result
        header = make []
    startup <- lookupRows header original
    optimized <- lookupRows header final
    zipWithM_ sameIndexedRow (map (`applyRow` header) startup) (map (`applyRow` header) optimized)
    let table = make (map (`applyRow` table) startup)
    guard (all public (sourceEquationDatatypes [table]))
    pure (setIdDetails owner (SourceTableId (idName owner) table NominalSourceTable), [table])

-- | Specialize the nominal scope equalities and read both exact rows.
lookupRows :: TyCon -> IndexedLookup -> MaybeT TcM [IndexedEquation]
lookupRows header (IndexedLookup owner parameters domains result rows) =
    zipWithM row [False, True] rows
  where
    row recursive (outer, IndexedRow inner proofs body) = do
      let equalities = map CoVarCo proofs
      subst <- MaybeT $ pure $ tcUnifyTys alwaysBindTv
          (map coercionLKind equalities) (map coercionRKind equalities)
      let specialize = substTyUnchecked subst
      (left, left_fields) <- indexedPatternTypes specialize outer
      (right, right_fields) <- indexedPatternTypes specialize inner
      let lhs = map (specialize . mkTyVarTy) parameters ++ [left, right]
          vars = tyCoVarsOfTypesWellScoped lhs
          field values (Core.Cast expr co)
            | caseCoercion proofs co
            , specialize (coercionLKind co) `eqType` specialize (coercionRKind co) = field values expr
          field values (Core.Var v) = lookup v values
          field _ _ = Nothing
          readResult False = const <$> field left_fields body
          readResult True = do
            (Core.Var fn, args) <- pure (Core.collectArgs body)
            guard (fn == owner)
            let (prefix, suffix) = splitAt (length parameters) args
            tys <- traverse (fmap specialize . indexedTypeArg) prefix
            [x, y] <- pure suffix
            l <- field left_fields x
            r <- field right_fields y
            guard (length tys == length parameters &&
                   eqTypes (map typeKind [l, r]) (map (substTyWith parameters tys) domains))
            pure (\family -> mkTyConApp family (tys ++ [l, r]))
      rhs <- MaybeT (pure (readResult recursive))
      let branch = IndexedEquation vars lhs rhs
      guard (all isTyVar vars && typeKind (rhs header) `eqType` specialize result &&
             validBranch (applyRow branch header))
      pure branch

-- | Read constructors, bound fields and a structurally smaller self call.
indexedResult :: [(Id, CoreExpr)] -> Id -> Maybe Id -> [TyVar] -> Type -> TyCon -> (Type -> Type)
              -> [Var] -> IndexedPattern -> [(Id, Type)] -> CoreExpr -> Maybe (TyCon -> Type)
indexedResult bindings owner worker parameters domain header specialize proofs pat fields =
    result bindings False
  where
    result floats underneath (Core.Cast body co) = do
      guard (caseCoercion proofs co &&
             specialize (coercionLKind co) `eqType` specialize (coercionRKind co))
      result floats underneath body
    result floats underneath expr = do
      let (fun, args) = Core.collectArgs expr
      Core.Var callee <- pure fun
      if callee == owner || Just callee == worker
        then recursive underneath callee args
        else field callee args <|> constructor floats expr callee args <|> float floats underneath callee expr

    field callee [] = const <$> lookup callee fields
    field _ _ = Nothing

    constructor floats expr callee args = do
      (con, supplied, values) <- indexedConstructor specialize callee args
      guard (null bindings ||
             all (not . DC.isBanged) (DC.dataConImplBangs con) &&
             all (caseCoercion proofs) [co | Core.Coercion co <- args])
      children <- traverse (result floats True) values
      _ <- checkedPromoted con (specialize (exprType expr)) supplied (map ($ header) children)
      pure (\family -> mkTyConApp (DC.promoteDataCon con) (supplied ++ map ($ family) children))

    float floats underneath callee expr = do
      guard (isJust (lookup callee floats))
      result [] underneath (expandTypeHelper floats expr)

    recursive underneath callee args = do
      guard underneath
      IndexedConstructor _ package _ original <- pure pat
      let (prefix, suffix) = splitAt (length parameters) args
      tys <- traverse (fmap specialize . indexedTypeArg) prefix
      guard (length tys == length parameters)
      let kind = substTyWith parameters tys domain
          n_exs = length (DC.dataConExTyCoVars package)
      (supplied, actual_fields, worker_types) <-
        if callee == owner
          then do
            [operand] <- pure suffix
            guard (specialize (exprType operand) `eqType` kind)
            (Core.Var builder, operands) <- pure (Core.collectArgs operand)
            (con, supplied, values) <- indexedConstructor specialize builder operands
            guard (con == package)
            pure (Just supplied, values, [])
          else do
            let (locals, values) = splitAt n_exs suffix
            erased <- traverse (fmap specialize . indexedTypeArg) locals
            guard (length erased == n_exs)
            pure (Nothing, values, erased)
      actual <- traverse get_value actual_fields
      guard (oneSmaller original actual)
      values <- traverse (\v -> lookup v fields) actual
      source <- case supplied of
        Just written -> pure written
        Nothing -> do
          (_, universal_args) <- splitTyConApp_maybe kind
          let universals = DC.dataConUnivTyVars package
              variables = universals ++ DC.dataConExTyCoVars package
          guard (length universal_args == length universals)
          traverse (`lookup` zip variables (universal_args ++ worker_types)) (DC.dataConUserTyVars package)
      packed <- checkedPromoted package kind source values
      pure (\family -> mkTyConApp family (tys ++ [packed]))

    get_value (Core.Var v) = Just v
    get_value _ = Nothing

    oneSmaller original values
      | length original == length values
      , Just changes <- zipWithM compare_field original values
      = length (filter id changes) == 1
      | otherwise = False

    compare_field (IndexedVariable old) new = False <$ guard (old == new)
    compare_field tree@IndexedConstructor{} new = True <$ guard (new `elem` indexedFields tree)

indexedConstructor :: (Type -> Type) -> Id -> [CoreExpr]
                   -> Maybe (DC.DataCon, [Type], [CoreExpr])
indexedConstructor specialize callee args = do
    let (erased, values) = span Core.isTyCoArg args
        (tys, proofs) = span (isJust . indexedTypeArg) erased
    types <- traverse (fmap specialize . indexedTypeArg) tys
    let check_proof (Core.Coercion co) = coercionRole co == Nominal &&
              specialize (coercionLKind co) `eqType` specialize (coercionRKind co)
        check_proof _ = False
    (con, supplied) <- case isDataConWrapId_maybe callee of
      Just con -> (con, types) <$ guard (null proofs)
      Nothing -> do
        con <- isDataConWorkId_maybe callee
        let variables = DC.dataConUnivTyVars con ++ DC.dataConExTyCoVars con
        (_, _, specs, [], _, _) <- pure (DC.dataConFullSig con)
        guard (length variables == length types && length specs == length proofs &&
               all check_proof proofs)
        supplied <- traverse (`lookup` zip variables types) (DC.dataConUserTyVars con)
        pure (con, supplied)
    guard (length supplied == length (DC.dataConUserTyVars con))
    pure (con, supplied, values)

sameShapes :: FamilyInputs -> FamilyInputs -> Bool
sameShapes (FamilyInputs _ arity g a term ext index _ var hit miss rows)
           (FamilyInputs _ arity' g' a' term' ext' index' _ var' hit' miss' rows') =
    arity == arity' && g `eqType` g' && a `eqType` a' && term == term' && ext == ext' && index == index' &&
    var == var' && hit == hit' && miss == miss' && rows == rows'

renamingOperations :: FamilyInputs -> (Id, Id)
renamingOperations (FamilyInputs _ _ _ _ _ _ _ operations _ _ _ _) = operations

familyInputs :: [(Id, CoreExpr)] -> Id -> CoreExpr -> Maybe FamilyInputs
familyInputs bindings owner rhs = do
    (context : types, body : captures, call) <- function rhs
    result : rev_sorts <- pure (reverse types)
    guard (not (null captures) && length captures == length rev_sorts)
    (Core.Var traversal, [callback, Core.Var operand]) <- application call
    guard (operand == body)
    ([callback_sort], [argument], lookup_body) <- function callback
    let sorts = reverse rev_sorts
        context_kind = varType context
        sort_kind = varType result
    (context_tycon, []) <- splitTyConApp_maybe context_kind
    (sort_tycon, []) <- splitTyConApp_maybe sort_kind
    guard (all isAlgTyCon [context_tycon, sort_tycon])
    guard (all (eqType sort_kind . varType) (callback_sort : sorts))
    (term, [body_ctx, TyVarTy body_sort]) <- splitTyConApp_maybe (varType body)
    (ext, [_, _]) <- splitTyConApp_maybe body_ctx
    (index, [index_ctx, TyVarTy index_sort]) <- splitTyConApp_maybe (varType argument)
    let context_ty = mkTyVarTy context
        extend k a = mkTyConApp ext [k, mkTyVarTy a]
        at a = mkTyConApp term [context_ty, mkTyVarTy a]
    guard (isPromotedDataCon ext && all isAlgTyCon [term, index])
    guard (foldl' extend context_ty sorts `eqType` body_ctx && body_sort == result &&
           and (zipWith (\capture sort -> varType capture `eqType` at sort) captures sorts) &&
           index_ctx `eqType` body_ctx && index_sort == callback_sort)
    case_rows <- caseOn argument lookup_body
    [(first, _, _), (second, _, _)] <- pure case_rows
    let index_cons = tyConDataCons index
    guard (first /= second && length index_cons == 2 &&
           first `elem` index_cons && second `elem` index_cons &&
           all lazyConstructor index_cons)
    (hit, miss, var) <-
        outcomes first second [] argument (reverse captures) lookup_body <|>
        outcomes second first [] argument (reverse captures) lookup_body
    guard (DC.dataConTyCon var == term)
    traversal_rhs <- lookup traversal bindings
    ([_, _, _], [sigma, scrutinee], traversal_case) <- function traversal_rhs
    rows <- caseOn scrutinee traversal_case
    [(var_bs, var_rhs)] <- pure [(bs, e) | (con, bs, e) <- rows, con == var]
    [field] <- pure (filter isValueVar var_bs)
    checked <- branchBody var_bs var_rhs
    (Core.Var use, [Core.Var x]) <- application checked
    guard (use == sigma && x == field)
    (operations, recipes) <- checkedTraversals bindings traversal sigma var hit miss rows
    pure (FamilyInputs owner (length captures) context_kind sort_kind term ext index operations var hit miss recipes)

-- Check both case traversals before emitting any nominal equations.
-- The recipe records the number of binder extensions for each field.
checkedTraversals :: [(Id, CoreExpr)]
                  -> Id -> Id
                  -> DC.DataCon -> DC.DataCon -> DC.DataCon
                  -> [(DC.DataCon, [Var], CoreExpr)]
                  -> Maybe ((Id, Id), [(DC.DataCon, [Int])])
checkedTraversals bindings subst sigma var hit miss rows = do
    let constructors = tyConDataCons (DC.dataConTyCon var)
    guard (complete constructors rows)
    lifted <- traverse (checkedRow bindings var hit miss subst sigma) [row | row@(con, _, _) <- rows, con /= var]
    let (recipes, used_extenders) = unzip lifted
        subst_steps = concatMap concat used_extenders
    extender <- allSame [v | NamedLift v <- subst_steps]
    extender_rhs <- lookup extender bindings
    rename <- checkedExtension bindings var hit miss extender_rhs
    guard (all (isSubstLift rename) subst_steps)
    rename_rhs <- lookup rename bindings
    (tvs, [rho, operand], renaming) <- function rename_rhs
    guard (length tvs == 3)
    rename_rows <- caseOn operand renaming
    guard (complete constructors rename_rows)
    [(var_binders, var_rhs)] <- pure [(bs, rhs) | (con, bs, rhs) <- rename_rows, con == var]
    [original] <- pure (filter isValueVar var_binders)
    var_result <- branchBody var_binders var_rhs
    (var_head, [renamed]) <- constructorCall var_result
    guard (var_head == var)
    (Core.Var callback, [Core.Var argument]) <- application renamed
    guard (callback == rho && argument == original)
    renamed_rows <- traverse (checkedRow bindings var hit miss rename rho)
                             [row | row@(con, _, _) <- rename_rows, con /= var]
    let (rename_recipes, rename_extenders) = unzip renamed_rows
    guard (sortOn (nameStableString . DC.dataConName . fst) recipes == sortOn (nameStableString . DC.dataConName . fst) rename_recipes)
    let rename_steps = concatMap concat rename_extenders
    guard (all isRenameLift rename_steps)
    extend_rename <- allSame [v | NamedLift v <- rename_steps]
    extend_rename_rhs <- lookup extend_rename bindings
    checkRenameExtension hit miss extend_rename_rhs
    pure ((rename, extend_rename), recipes)
  where
    complete constructors alts =
      let heads = [con | (con, _, _) <- alts]
      in length heads == length constructors && all (`elem` heads) constructors &&
         all lazyConstructor constructors

    isSubstLift _ (NamedLift _) = True
    isSubstLift rename (InlinedSubst v) = rename == v
    isSubstLift _ InlinedRename = False

    isRenameLift (NamedLift _) = True
    isRenameLift InlinedRename = True
    isRenameLift (InlinedSubst _) = False

    allSame [] = Nothing
    allSame (v : vs) = v <$ guard (all (== v) vs)

-- Check every recursive call on exactly the original corresponding
-- field. The result keeps the same head and lazy field order.
checkedRow :: [(Id, CoreExpr)] -> DC.DataCon -> DC.DataCon -> DC.DataCon -> Id -> Id
           -> (DC.DataCon, [Var], CoreExpr)
           -> Maybe ((DC.DataCon, [Int]), [[CallbackLift]])
checkedRow bindings var hit miss traversal callback (con, binders, rhs) = do
    result <- branchBody binders rhs
    (rebuilt, fields) <- constructorCall result
    let originals = filter isValueVar binders
    guard (rebuilt == con && length fields == length originals)
    owners <- zipWithM recursiveField originals fields
    pure ((con, map length owners), owners)
  where
    recursiveField original field = do
        (Core.Var owner, [captured, Core.Var subterm]) <- application field
        guard (traversal == owner && original == subterm &&
               isLiftedTypeKind (typeKind (varType original)))
        lifts captured

    lifts (Core.Var original)
        | callback == original = Just []
    lifts captured = named captured <|> inlined captured

    named captured = do
        ([index], [], closure) <- function captured
        case Core.collectArgs closure of
          (Core.Var original, [Core.Type (TyVarTy rest)])
            | callback == original && index == rest -> pure []
          (Core.Var extender, [Core.Type _, Core.Type _, Core.Type _, original,
                               Core.Type (TyVarTy rest)])
            | index == rest -> (NamedLift extender :) <$> lifts original
          _ -> Nothing

    -- Simplification can inline one extension around the remaining callback.
    -- Check its constructors and each operand before stripping the lookup.
    inlined captured = do
        ([index], [argument], body) <- function captured
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        fresh <- expandTypeHelper bindings <$> branchBody hit_bs hit_rhs
        forwarded <- branchBody miss_bs miss_rhs
        (lift_step, see_older) <- inlineStep fresh forwarded
        [older] <- pure (filter isValueVar miss_bs)
        let (head_expr, args) = Core.collectArgs see_older
        arg : preceding <- pure (reverse args)
        Core.Var input <- branchBody miss_bs arg
        guard (input == older)
        (lift_step :) <$> lifts (Core.Lam index (foldl' Core.App head_expr (reverse preceding)))

    inlineStep fresh forwarded = do
        (fresh_con, fields) <- constructorCall fresh
        case fields of
          [] -> do
            (forward_con, [lookup_older]) <- constructorCall forwarded
            guard (fresh_con == hit && forward_con == miss)
            pure (InlinedRename, lookup_older)
          [v] -> do
            (fresh_index, []) <- constructorCall v
            guard (fresh_con == var && fresh_index == hit)
            (Core.Var rename, [weakening, lookup_older]) <- application forwarded
            ([_], [], partial) <- weakeningBody weakening >>= function
            (shift, []) <- constructorCall partial
            guard (shift == miss)
            pure (InlinedSubst rename, lookup_older)
          _ -> Nothing

-- Check the function used when a renaming crosses a binder.
checkRenameExtension :: DC.DataCon -> DC.DataCon -> CoreExpr -> Maybe ()
checkRenameExtension hit miss rhs = do
    let (binders, body) = Core.collectBinders rhs
    [g, d, b, capture, sort_var, argument] <- pure binders
    guard (all isTyVar [g,d,b,sort_var] && all isValueVar [capture,argument])
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs &&
           lazyConstructor hit && lazyConstructor miss)
    hit_body <- branchBody hit_bs hit_rhs
    (fresh, []) <- constructorCall hit_body
    guard (fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (successor, [lookup_older]) <- constructorCall miss_body
    guard (successor == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)

lazyConstructor :: DC.DataCon -> Bool
lazyConstructor con =
    not (any DC.isBanged (DC.dataConImplBangs con)) &&
    null (DC.dataConRepArgBinders con) &&
    null (DC.dataConStupidTheta con)

reachable :: [(Id, CoreExpr)] -> Id -> [Name]
reachable bindings root = map idName (walk [] [root])
  where
    local = mkVarSet (map fst bindings)
    walk seen [] = seen
    walk seen (v:rest)
      | v `elem` seen = walk seen rest
      | Just body <- lookup v bindings =
          walk (v:seen) (exprSomeFreeVarsList (\x -> x `elemVarSet` local) body ++ rest)
      | otherwise = walk seen rest

-- A final simplifier can lift a closed constructor-only expression
-- into a local type abstraction. Check at most that one definition.
expandTypeHelper :: [(Id, CoreExpr)] -> CoreExpr -> CoreExpr
expandTypeHelper bindings rhs
  | (Core.Var helper, args) <- Core.collectArgs rhs
  , Just declaration <- lookup helper bindings
  , Just (tvs, [], body) <- function declaration
  , Just tys <- traverse typeArg args
  , length tvs == length tys
  , let in_scope = mkInScopeSet (foldl' unionVarSet (exprFreeVars body)
                                    (map (exprFreeVars . Core.Type) tys))
        substitution = foldl' (\sub (tv, ty) -> extendTvSubst sub tv ty)
                             (mkEmptySubst in_scope) (zip tvs tys)
        expanded = substExpr substitution body
  , exprType expanded `eqType` exprType rhs
  = expanded
  | otherwise = rhs
  where
    typeArg (Core.Type ty) = Just ty
    typeArg _ = Nothing

weakeningBody :: CoreExpr -> Maybe CoreExpr
weakeningBody (Core.Cast body co) = body <$ guard (checkedMultiplicityCast co)
weakeningBody body = Just body

checkedExtension :: [(Id, CoreExpr)]
                 -> DC.DataCon -> DC.DataCon -> DC.DataCon
                 -> CoreExpr -> Maybe Id
checkedExtension bindings var hit miss rhs = do
    let (binders, body) = Core.collectBinders rhs
    [g, d, b, capture, sort_var, argument] <- pure binders
    guard (all isTyVar [g,d,b,sort_var] && all isValueVar [capture,argument])
    rows <- caseOn argument body
    [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
    [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
    guard (length rows == 2 && all (not . isValueVar) hit_bs)
    hit_body <- branchBody hit_bs hit_rhs
    (term, [v]) <- constructorCall (expandTypeHelper bindings hit_body)
    (fresh, []) <- constructorCall v
    guard (term == var && fresh == hit)
    [older] <- pure (filter isValueVar miss_bs)
    miss_body <- branchBody miss_bs miss_rhs
    (Core.Var rename, [weakening, lookup_older]) <- application miss_body
    _ <- lookup rename bindings
    ([_], [], partial) <- weakeningBody weakening >>= function
    (shift, []) <- constructorCall partial
    guard (shift == miss)
    (Core.Var lookup_capture, [arg]) <- application lookup_older
    Core.Var input <- branchBody miss_bs arg
    guard (lookup_capture == capture && input == older)
    pure rename

constructorCall :: CoreExpr -> Maybe (DC.DataCon, [CoreExpr])
constructorCall rhs = do
    (Core.Var make, fields) <- application rhs
    con <- isDataConWorkId_maybe make <|> isDataConWrapId_maybe make
    pure (con, fields)

-- Check one lookup case per replacement. Only case evidence can
-- relate the predecessor lookup to the declared outer context.
outcomes :: DC.DataCon -> DC.DataCon -> [Var] -> Id -> [Id] -> CoreExpr
         -> Maybe (DC.DataCon, DC.DataCon, DC.DataCon)
outcomes hit miss scope argument captures expr = do
    body <- branchBody scope expr
    case captures of
      [] -> do
        (con, [Core.Var x]) <- constructorCall body
        guard (x == argument)
        pure (hit, miss, con)
      capture : rest -> do
        rows <- caseOn argument body
        [(hit_bs, hit_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == hit]
        [(miss_bs, miss_rhs)] <- pure [(bs, e) | (dc, bs, e) <- rows, dc == miss]
        guard (length rows == 2 && all (not . isValueVar) hit_bs)
        Core.Var selected <- branchBody (scope ++ hit_bs) hit_rhs
        guard (selected == capture)
        [older] <- pure (filter isValueVar miss_bs)
        outcomes hit miss (scope ++ miss_bs) older rest miss_rhs

function :: CoreExpr -> Maybe ([TyVar], [Id], CoreExpr)
function expr = do
    let (binders, body) = collect [] expr
        (ty_binders, tm_binders) = span isTyVar binders
    guard (all isValueVar tm_binders)
    pure (ty_binders, tm_binders, body)
  where
    -- See Note [Certifying a checked source closure].
    collect binders (Core.Lam binder body) = collect (binder:binders) body
    collect binders (Core.Let (Core.NonRec binder _) body)
      | isLocalId binder
      , isLiftedTypeKind (typeKind (varType binder))
      , not (binder `elemVarSet` exprFreeVars body)
      = collect binders body
    collect binders body = (reverse binders, body)

application :: CoreExpr -> Maybe (CoreExpr, [CoreExpr])
application expr = do
    let (head_expr, args) = Core.collectArgs expr
        (_, values) = span Core.isTyCoArg args
    guard (all (not . Core.isTyCoArg) values)
    pure (head_expr, values)

caseOn :: Id -> CoreExpr
       -> Maybe [(DC.DataCon, [Var], CoreExpr)]
caseOn target expr = do
    Core.Case (Core.Var scrutinee) _ _ alts <- pure expr
    guard (target == scrutinee)
    traverse dataAlt alts
  where
    dataAlt (Core.Alt (Core.DataAlt con) bs rhs) = Just (con, bs, rhs)
    dataAlt _ = Nothing

branchBody :: [Var] -> CoreExpr -> Maybe CoreExpr
branchBody binders expr = case expr of
    Core.Cast body co
      | caseCoercion (filter isCoVar binders) co -> branchBody binders body
      | otherwise -> Nothing
    body -> Just body

caseCoercion :: [Var] -> Coercion -> Bool
caseCoercion scope co = case co of
    Refl _ -> True
    GRefl _ _ MRefl -> True
    CoVarCo cv -> cv `elem` scope
    TyConAppCo _ _ cs -> all (caseCoercion scope) cs
    TransCo a b -> caseCoercion scope a && caseCoercion scope b
    SubCo c -> caseCoercion scope c
    SymCo c -> caseCoercion scope c
    SelCo _ c -> caseCoercion scope c
    _ -> False

isValueVar :: Id -> Bool
isValueVar v = isId v && not (isCoVar v)

{- Note [Renaming with an abstract callback]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The checked substitution traversal also verifies the renaming and
binder extension operations. Give these operations their own equations
when the provider exports both. The variable row keeps the rank-n
callback application below a lazy constructor. A binder row partially
applies the same checked extension that the client can name. Reduction
stops at an unknown lookup or term.

Check the extension before SimpleOpt and after optimization. Its only
free term Ids can be constructor workers or wrappers. The callback is
bound by the extension. User rules cannot target these constructors,
so the extension table has an empty helper list and checks its owner.

Use the original unmatchable callback kind. A runtime renaming can
map two distinct lookups to the same value. This certificate gives no
injectivity for that application.
-}

-- | Emit the binder extension once, even if several traversals use it.
publishRenameExtension :: (TyCon -> Bool) -> (Id, (Id, FamilyInputs), [Id])
                       -> TcM (Maybe (Id, [TyCon]))
publishRenameExtension public (_, (owner, shape), _) = runMaybeT $ do
    let FamilyInputs _ _ context sort _ ext index _ _ hit miss _ = shape
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    b <- lift (freshVar "binder" sort)
    a <- lift (freshVar "a" sort)
    rank <- lift (freshVar "sort" sort)
    let [gk,dk,bk,ak] = mkTyVarTys [g,d,b,a]
        lookupAt c s = mkTyConApp index [c,s]
        extend c = mkTyConApp ext [c,bk]
        domain = lookupAt (extend gk) ak
        result = lookupAt (extend dk) ak
    rho <- lift (freshVar "rho" (renamingKind index rank gk dk))
    input <- lift (freshVar "input" domain)
    old <- lift (freshVar "index" (lookupAt gk ak))
    let r = mkTyVarTy rho
        x = mkTyVarTy old
        binders = mkNamedTyConBinders Specified [g,d,b] ++
                  [mkAnonTyConBinder rho, mkNamedTyConBinder Specified a, mkAnonTyConBinder input]
        branches = do
          fresh <- conAt hit (lookupAt (extend gk) bk) []
          fresh' <- conAt hit (lookupAt (extend dk) bk) []
          previous <- conAt miss domain [x]
          previous' <- conAt miss result [mkAppTys r [ak,x]]
          pure [renamingBranch [gk,dk,bk,r,bk,fresh] fresh',
                renamingBranch [gk,dk,bk,r,ak,previous] previous']
    publishRenamingTable public owner [] binders result (const branches)

-- | A traversal can extend its callback only at each field's own binders.
publishRenaming :: (TyCon -> Bool) -> [(Name, TyCon)] -> (Id, (Id, FamilyInputs), [Id])
                -> TcM (Maybe (Id, [TyCon]))
publishRenaming public extensions (owner, (extension, shape), helpers) = runMaybeT $ do
    extend <- MaybeT (pure (lookup (idName extension) extensions))
    let FamilyInputs _ _ context sort term ext index _ var _ _ recipes = shape
    g <- lift (freshVar "g" context)
    d <- lift (freshVar "d" context)
    a <- lift (freshVar "a" sort)
    rank <- lift (freshVar "sort" sort)
    let [gk,dk,ak] = mkTyVarTys [g,d,a]
        termAt c s = mkTyConApp term [c,s]
        result = termAt dk ak
    rho <- lift (freshVar "rho" (renamingKind index rank gk dk))
    input <- lift (freshVar "input" (termAt gk ak))
    let binders = mkNamedTyConBinders Specified [g,d,a] ++ map mkAnonTyConBinder [rho,input]
        prepare (con, depths) = do
          let (con_vars, telescope) = splitForAllTyVars (tyConKind (DC.promoteDataCon con))
              (_, fields, codomain) = splitFunTys telescope
          (subst, vars) <- lift (freshenTyVarBndrs (map snd con_vars))
          let kind = substTyUnchecked subst codomain
          args <- lift (traverse (freshVar "field" . substTyUnchecked subst . scaledThing) fields)
          (tc, [source, sort_at]) <- MaybeT (pure (splitTyConApp_maybe kind))
          guard (tc == term)
          local <- lift (freshVar "rho" (renamingKind index rank source dk))
          let r = mkTyVarTy local
              fields = mkTyVarTys args
              lhs = mkTyConApp (DC.promoteDataCon con) (mkTyVarTys vars ++ fields)
              liftField 0 ctx = (dk,r) <$ guard (ctx `eqType` source)
              liftField depth ctx = do
                (cl, [outer, introduced]) <- splitTyConApp_maybe ctx
                guard (cl == ext)
                (target, inherited) <- liftField (depth - 1) outer
                pure (mkTyConApp ext [target,introduced],
                      mkTyConApp extend [outer,target,introduced,inherited])
              rewrite family depth arg = do
                (t, [ctx, at]) <- splitTyConApp_maybe (typeKind arg)
                guard (t == term)
                (target, captured) <- liftField depth ctx
                guard (typeKind captured `eqType` renamingKind index rank ctx target)
                pure (mkTyConApp family [ctx,target,at,captured,arg])
          pure $ \family -> do
            guard (typeKind lhs `eqType` kind)
            output <- if con == var
              then do [x] <- pure fields
                      guard (typeKind x `eqType` mkTyConApp index [source,sort_at])
                      pure [mkAppTys r [sort_at,x]]
              else zipWithM (rewrite family) depths fields
            out <- conAt con (termAt dk sort_at) output
            pure (renamingBranch [source,dk,sort_at,r,lhs] out)
    rows <- traverse prepare ((var,[]) : recipes)
    publishRenamingTable public owner helpers binders result (\family -> traverse ($ family) rows)

-- | Preserve the exact rank-n callback type when generating a table kind.
renamingKind :: TyCon -> TyVar -> Type -> Type -> Kind
renamingKind index rank g d =
    mkSpecForAllTys [(unmatchableDataConTy, rank)] $
      mkVisFunTyManyU (mkTyConApp index [g,mkTyVarTy rank]) (mkTyConApp index [d,mkTyVarTy rank])

renamingBranch :: [Type] -> Type -> CoAxBranch
renamingBranch lhs = mkBranch (tyCoVarsOfTypesWellScoped lhs) lhs

publishRenamingTable :: (TyCon -> Bool) -> Id -> [Id] -> [TyConBinder] -> Kind
                     -> (TyCon -> Maybe [CoAxBranch]) -> MaybeT TcM (Id, [TyCon])
publishRenamingTable public owner helpers binders result rows = do
    root <- lift (newImplicitBinder (idName owner) mkSourceTableOcc)
    axiom <- lift (newImplicitBinder root mkInstTyCoOcc)
    let equations = rows table
        table = mkFamily root axiom binders result (fromMaybe [] equations)
        details = case helpers of
          [] -> SourceTableId (idName owner) table NominalSourceTable
          ids -> CheckedClosureSourceTableId (idName owner) table (map idName ids)
    guard (tyConKind table `eqType` varType owner)
    branches <- MaybeT (pure equations)
    guard (all validBranch branches && all public (sourceEquationDatatypes [table]))
    pure (setIdDetails owner details, [table])

allocateFamilies :: FamilyInputs -> TcM (Maybe (TyCon, [TyCon]))
allocateFamilies (FamilyInputs owner arity context sort term ext lookup_tycon _ var hit miss recipes) = do
    (closure, constructors) <- allocateClosure owner context sort term ext
    case constructors of
      [replace, shift, lift, replace_next] -> do
        apply_name <- localName "$apply:"
        lookup_name <- localName "$lookup:"
        apply_axiom <- newImplicitBinder apply_name mkInstTyCoOcc
        lookup_axiom <- newImplicitBinder lookup_name mkInstTyCoOcc
        root_name <- newImplicitBinder (idName owner) mkSourceTableOcc
        root_axiom <- newImplicitBinder root_name mkInstTyCoOcc
        g <- freshVar "g" context
        d <- freshVar "d" context
        a <- freshVar "a" sort
        b <- freshVar "b" sort
        let gk = mkTyVarTy g
            dk = mkTyVarTy d
            ak = mkTyVarTy a
            bk = mkTyVarTy b
            termAt p k = mkTyConApp term [p,k]
            ctxAt p k = mkTyConApp ext [p,k]
            lookupAt p k = mkTyConApp lookup_tycon [p,k]
            closureAt p q = mkTyConApp closure [p,q]
            ctx_g_a = ctxAt gk ak
            ctx_d_a = ctxAt dk ak
        cv <- freshVar "c" (closureAt gk dk)
        tree <- freshVar "t" (termAt gk bk)
        field <- freshVar "x" (lookupAt gk bk)
        carried <- freshVar "w" (termAt dk ak)
        capture_kinds <- traverse (\_ -> freshVar "captureKind" sort) [1..arity]
        let capture_sorts = mkTyVarTys capture_kinds
            body_context = foldl' ctxAt gk capture_sorts
        body <- freshVar "n" (termAt body_context bk)
        args <- traverse (freshVar "v" . termAt gk) capture_sorts
        x_d <- freshVar "older" (lookupAt dk bk)
        let prepare (ctor, exten) = do
              let (ctor_tvs, tel) = splitForAllTyVars (tyConKind (DC.promoteDataCon ctor))
                  (_, arg_kinds, res) = splitFunTys tel
              (renaming, tvs) <- freshenTyVarBndrs (map snd ctor_tvs)
              vars <- traverse (freshVar "arg" . substTyUnchecked renaming . scaledThing) arg_kinds
              let result = substTyUnchecked renaming res
              env <- freshVar "env" $ case splitTyConApp_maybe result of
                Just (_, [src, _]) -> closureAt src dk
                _ -> liftedTypeKind  -- Rejected by applyBranch.
              pure (ctor, exten, tvs, vars, result, env)
        apply_inputs <- traverse prepare ((var,[]) : recipes)
        let c = mkTyVarTy cv
            x = mkTyVarTy field
            w = mkTyVarTy carried
            outputs = termAt dk bk
            prefix = mkNamedTyConBinders Specified [g,d,b]
            apply_apply s t k env val = mkTyConApp apply [s,t,k,env,val]
            lookup_apply s t k env val = mkTyConApp lookup_fam [s,t,k,env,val]
            apply = mkFamily apply_name apply_axiom (prefix ++ map mkAnonTyConBinder [cv,tree])
                        outputs (fromMaybe [] apply_branches)
            lookup_fam = mkFamily lookup_name lookup_axiom (prefix ++ map mkAnonTyConBinder [cv,field])
                        outputs (fromMaybe [] lookup_branches)
            applyBranch (ctor, exten, tvs, vars, res, env_var) = do
                (actual_term, [src, sort_k]) <- splitTyConApp_maybe res
                guard (actual_term == term && (ctor == var || length exten == length vars))
                let fields = mkTyVarTys vars
                    lhs = mkTyConApp (DC.promoteDataCon ctor) (mkTyVarTys tvs ++ fields)
                    env = mkTyVarTy env_var
                    liftField 0 source = do
                      guard (source `eqType` src)
                      pure (dk, env)
                    liftField depth source = do
                      (scope, [outer, introduced]) <- splitTyConApp_maybe source
                      guard (scope == ext)
                      (target, inner) <- liftField (depth - 1) outer
                      let extended = ctxAt target introduced
                      lifted <- conAt lift (closureAt source extended) [inner]
                      pure (extended, lifted)
                    use depth value = do
                      (same, [source, k]) <- splitTyConApp_maybe (typeKind value)
                      guard (same == term)
                      (target, closure_value) <- liftField depth source
                      pure (apply_apply source target k closure_value value)
                out <- if ctor == var
                         then do [lookup_var] <- pure fields
                                 pure (lookup_apply src dk sort_k env lookup_var)
                         else zipWithM use exten fields >>= conAt ctor (termAt dk sort_k)
                guard (typeKind lhs `eqType` res && typeKind out `eqType` termAt dk sort_k)
                pure (mkBranch (tvs ++ [d,env_var] ++ vars) [src,dk,sort_k,env,lhs] out)
            apply_branches = traverse applyBranch apply_inputs
            lookup_branches = do
                r <- conAt replace (closureAt ctx_d_a dk) [w]
                z_in <- conAt hit (lookupAt ctx_d_a ak) []
                s_in <- conAt miss (lookupAt ctx_d_a bk) [mkTyVarTy x_d]
                v_d <- conAt var (termAt dk bk) [mkTyVarTy x_d]
                s <- conAt shift (closureAt gk ctx_g_a) []
                s_out <- conAt miss (lookupAt ctx_g_a bk) [x]
                v_out <- conAt var (termAt ctx_g_a bk) [s_out]
                l <- conAt lift (closureAt ctx_g_a ctx_d_a) [c]
                z_g <- conAt hit (lookupAt ctx_g_a ak) []
                z_d <- conAt hit (lookupAt ctx_d_a ak) []
                v0 <- conAt var (termAt ctx_d_a ak) [z_d]
                sx <- conAt miss (lookupAt ctx_g_a bk) [x]
                s_d <- conAt shift (closureAt dk ctx_d_a) []
                let weaken = apply_apply dk ctx_d_a bk s_d (lookup_apply gk dk bk c x)
                next <- conAt replace_next (closureAt ctx_g_a dk) [c,w]
                pure [ mkBranch [d,a,carried] [ctx_d_a,dk,ak,r,z_in] w
                     , mkBranch [d,a,b,carried,x_d] [ctx_d_a,dk,bk,r,s_in] v_d
                     , mkBranch [g,a,b,field] [gk,ctx_g_a,bk,s,x] v_out
                     , mkBranch [g,d,a,cv] [ctx_g_a,ctx_d_a,ak,l,z_g] v0
                     , mkBranch [g,d,a,b,cv,field] [ctx_g_a,ctx_d_a,bk,l,sx] weaken
                     , mkBranch [g,d,a,cv,carried] [ctx_g_a,dk,ak,next,z_g] w
                     , mkBranch [g,d,a,b,cv,carried,field] [ctx_g_a,dk,bk,next,sx]
                         (lookup_apply gk dk bk c x) ]
        pure $ do
            apply_equations <- apply_branches
            lookup_equations <- lookup_branches
            (first_sort, first_arg) : rest <- pure (zip capture_sorts (mkTyVarTys args))
            let initial_context = ctxAt gk first_sort
                addReplacement (src, env) (k, value) = do
                    let next_context = ctxAt src k
                    next <- conAt replace_next (closureAt next_context gk) [env,value]
                    pure (next_context,next)
            first <- conAt replace (closureAt initial_context gk) [first_arg]
            (_, replacements) <- foldM addReplacement (initial_context,first) rest
            let result = termAt gk bk
                t = apply_apply body_context gk bk replacements (mkTyVarTy body)
                index_vars = g : capture_kinds ++ [b]
                value_vars = body : args
                vars = index_vars ++ value_vars
                params = mkNamedTyConBinders Specified index_vars ++ map mkAnonTyConBinder value_vars
                root_equation = mkBranch vars (mkTyVarTys vars) t
                root = mkFamily root_name root_axiom params result [root_equation]
            guard (all validBranch (root_equation : apply_equations ++ lookup_equations))
            pure (root, [root,closure,apply,lookup_fam])
      _ -> pure Nothing
  where
    localName prefix = newImplicitBinder (idName owner) (mkTcOcc . (prefix ++) . occNameString)

freshVar :: String -> Kind -> TcM TyVar
freshVar str kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc str)

-- Match the promoted telescope at the intended kind. The order of
-- source quantification can differ from the order of GADT parameters.
conAt :: DC.DataCon -> Type -> [Type] -> Maybe Type
conAt con result fields = do
    let promoted = DC.promoteDataCon con
        (annotated, telescope) = splitForAllTyVars (tyConKind promoted)
        (_, inputs, output) = splitFunTys telescope
    guard (length inputs == length fields)
    subst <- tcMatchTys (output : map scaledThing inputs) (result : map typeKind fields)
    args <- traverse (lookupTyVar subst . snd) annotated
    let value = mkTyConApp promoted (args ++ fields)
    value <$ guard (typeKind value `eqType` result)

mkFamily :: Name -> Name -> [TyConBinder] -> Kind -> [CoAxBranch] -> TyCon
mkFamily name axname binders result branches =
    let family = mkFamilyTyCon name (mkTyConKindWithMatchability unmatchableDataConTy binders result)
                    binders 0 result Nothing (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal axname family branches
    in family

-- Check the restrictions that ordinary family syntax imposes on
-- patterns, including the indices exposed by a promoted constructor.
validBranch :: CoAxBranch -> Bool
validBranch branch =
    all isTyFamFree lhs &&
    tyCoVarsOfTypes (coAxBranchRHS branch : lhs ++ map varType tvs) `subVarSet` mkVarSet tvs
  where
    lhs = coAxBranchLHS branch
    tvs = coAxBranchTyVars branch

mkBranch :: [TyVar] -> [Type] -> Type -> CoAxBranch
mkBranch vars lhs rhs =
    mkCoAxBranch vars [] [] lhs rhs (map (const Nominal) vars) noSrcSpan

allocateClosure :: Id -> Kind -> Kind -> TyCon -> TyCon
                -> TcM (TyCon, [DC.DataCon])
allocateClosure owner context_kind sort_kind term ext = do
    let tag prefix occ = prefix ++ occNameString occ
    closure_name <- newImplicitBinder (idName owner) (mkTcOcc . tag "$closure:")
    closure_rep <- newTyConRepName closure_name
    replace_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$replace:")
    shift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$shift:")
    lift_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$lift:")
    next_name <- newImplicitBinder (idName owner) (mkDataOcc . tag "$replaceNext:")
    let names = [replace_name, shift_name, lift_name, next_name]
        tags = mkNameEnv (zip names [1..])
        ordinary = DC.HsSrcBang NoSourceText DC.NoSrcUnpack DC.NoSrcStrict
    source <- freshVar "source" context_kind
    target <- freshVar "target" context_kind
    incoming <- freshVar "incoming" context_kind
    outgoing <- freshVar "outgoing" context_kind
    binder <- freshVar "binder" sort_kind
    let universals = [source, target]
        source_ty = mkTyVarTy source
        target_ty = mkTyVarTy target
        incoming_ty = mkTyVarTy incoming
        outgoing_ty = mkTyVarTy outgoing
        binder_ty = mkTyVarTy binder
        extend ctx = mkTyConApp ext [ctx, binder_ty]
        binders = map mkAnonTyConBinder universals
        kind = mkTyConKind binders liftedTypeKind
    closure <- mfix $ \result -> do
        let applied a b = mkTyConApp result [a,b]
            ctor name existentials user_tvs equalities fields res = do
              representative <- newTyConRepName name
              buildDataCon emptyFamInstEnvs (FixedBangOpts (map (const DC.HsLazy) fields))
                name False representative (map (const ordinary) fields) [] universals existentials
                (mkTyVarBinders Specified user_tvs) equalities []
                (map (Scaled manyDataConTy) fields) res result tags
        replace <- ctor replace_name [binder] [target,binder]
                     [DC.mkEqSpec source (extend target_ty)]
                     [mkTyConApp term [target_ty,binder_ty]] (applied (extend target_ty) target_ty)
        shift <- ctor shift_name [binder] [source,binder]
                   [DC.mkEqSpec target (extend source_ty)] [] (applied source_ty (extend source_ty))
        lift <- ctor lift_name [incoming,outgoing,binder] [incoming,outgoing,binder]
                  [DC.mkEqSpec source (extend incoming_ty), DC.mkEqSpec target (extend outgoing_ty)]
                  [applied incoming_ty outgoing_ty] (applied (extend incoming_ty) (extend outgoing_ty))
        replace_next <- ctor next_name [incoming,binder] [incoming,target,binder]
                          [DC.mkEqSpec source (extend incoming_ty)]
                          [applied incoming_ty target_ty, mkTyConApp term [target_ty,binder_ty]]
                          (applied (extend incoming_ty) target_ty)
        pure (mkAlgTyCon closure_name kind binders 0 liftedTypeKind [Nominal,Nominal] Nothing []
                (mkLevPolyDataTyConRhs True True [replace,shift,lift,replace_next]) (VanillaAlgTyCon closure_rep) True)
    pure (closure, tyConDataCons closure)
