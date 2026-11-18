-- | Closed type families for checked functions that match one constructor.
module GHC.Tc.Gen.SourceTable (sourceTableTyCons, sourceRuleOwners, sourceTableOccurrence, tcLookupSourceTableId,
                              sourceTableDependencies, sourceTableTypeUses, sourceTableFamilyUse,
                              visibleSourceMethodRules, hasSourceMethodRule, hasSourceMethodRuleAt,
                              sourceRuleOwnersAt, recordSourceMethodUse, recordSourceLiteralUse, sourceLiteralWorkers,
                              checkSourceMethodUses) where

import GHC.Prelude

import GHC.Builtin.KnownOccs (eqClassOpOcc, minusClassOpOcc)
import GHC.Builtin.Modules (gHC_CLASSES, gHC_INTERNAL_BASE, gHC_INTERNAL_NUM_INTEGER, gHC_TYPES, mkGhcInternalModule)
import GHC.Builtin.WiredIn.Types (integerTy, integerTyConName, listTyCon, mkListTy, unmatchableDataConTy)
import GHC.Core (CoreRule(..), isAutoRule, ruleActivation)
import GHC.Core.Rules (RuleEnv(..), emptyRuleBase, emptyRuleEnv, extendRuleBaseList, getRules)
import GHC.Core.Coercion.Axiom (ClosedTyFam(..), CoAxBranch, coAxiomBranches, coAxBranchRHS, fromBranches)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.InstEnv (instanceDFunId, lookupUniqueInstEnv)
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyConsOfTypes)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Data.FastString (fsLit)
import GHC.Data.Bag (isEmptyBag)
import GHC.Driver.Env (hsc_HUG, hsc_home_unit, hugRulesBelow)
import GHC.Hs
import GHC.Iface.Env (lookupOrig)
import GHC.Iface.Load (loadInterfaceForName)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Tc.Errors.Types (RetainedQuantificationError(..), TcRnMessage(..))
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types.ErrCtxt (HsCtxt(ExprCtxt))
import GHC.Tc.Types.Evidence (HsWrapper(..), TcEvBinds(..), isEmptyEvBindsMap)
import GHC.Tc.Types (TcGblEnv(..), tcVisibleOrphanMods)
import GHC.Tc.Utils.Env (tcLookupId, tcLookupIdMaybe, tcGetInstEnvs)
import GHC.Tc.Utils.Monad (TcM, addErr, failWithTc, getEps, getGblEnv, getTcEvBindsMap,
                           getTopEnv, keepAlive, liftZonkM, newSysName, readTcRef, setSrcSpanA, updTcRef, xoptM)
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Tc.Zonk.TcType (zonkTcType, zonkTcTypes)
import GHC.Types.Basic (Origin(..))
import GHC.Types.Id (globaliseId, idName, isClassOpId_maybe, setIdType, sourceTableId_maybe, sourceLiteralCons_maybe)
import GHC.Types.InlinePragma (isNeverActive)
import GHC.Types.Name (Name, isExternalName, mkSourceTableOcc, mkInstTyCoOcc,
                       isVarOcc, mkTyVarOcc, mkVarOcc, nameModule_maybe, nameOccName, setOccNameSpace, sourceTableTermOcc, stableNameCmp, tvName)
import GHC.Types.Name.Env (extendNameEnv_C, lookupNameEnv)
import GHC.Types.Name.Set (NameSet, emptyNameSet, elemNameSet, extendNameSet, extendNameSetList, filterNameSet,
                           intersectNameSet, isEmptyNameSet, minusNameSet, nameSetElemsStable,
                           unionNameSet, unionNameSets, unitNameSet)
import GHC.Types.SrcLoc
import GHC.Types.SourceFile (hscSourceToIsBoot)
import GHC.Unit.External (eps_rule_base)
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Home.Graph (lookupHugByModule)
import GHC.Unit.Home.ModInfo (hm_details)
import GHC.Unit.Module.Env (moduleSetElts)
import GHC.Unit.Module.ModDetails (md_rules)
import GHC.Unit.Module.ModIface (mi_hash_fn)
import GHC.Unit.Types (GenWithIsBoot(..), isInteractiveModule, moduleName, moduleUnitId)
import GHC.Types.TypeEnv (lookupTypeEnv)
import GHC.Types.TyThing (TyThing(..))
import GHC.Types.Unique.FM (plusUFM_C)
import GHC.Types.Unique.Set (mkUniqSet, nonDetEltsUniqSet)
import GHC.Types.Var (Id, VarBndr(..), mkTyVar, varName)
import GHC.Utils.Monad (allM)
import GHC.Utils.Outputable (text)

import Control.Monad (foldM, forM_, guard, unless, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Data.List (find, partition, sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, isNothing, mapMaybe)

-- | Read the current defining Id without registering use of its equations.
-- A local occurrence can still carry the Id from before equation checking.
-- See Note [Source table Ids].
sourceTableOccurrence :: TcGblEnv -> Id -> Maybe Id
sourceTableOccurrence env actual
  | nameModule_maybe name == Just (tcg_semantic_mod env)
  = case lookupTypeEnv (tcg_type_env env) name of
      Just (AnId fun) | fun == actual -> Just fun
      _ -> Nothing
  | otherwise = Just actual
  where
    name = varName actual

-- | Resolve a checked source call through the defining term's identity.
-- See Note [Source table Ids].
tcLookupSourceTableId :: Id -> TcM (Maybe Id)
tcLookupSourceTableId actual = do
    env <- getGblEnv
    case sourceTableOccurrence env actual of
      Just fun | Just table <- sourceTableId_maybe fun -> do
        dependencies <- sourceTableDependencies table
        case dependencies of
          Nothing -> return Nothing
          Just callees -> do
            recordSourceUses [(v, Nothing) | v <- fun : callees]
            forM_ (fun : callees) $ \owner ->
              when (nameModule_maybe (idName owner) == Just (tcg_semantic_mod env)) $
                keepAlive (idName owner)
            updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
            return (Just fun)
      _ -> return Nothing

-- | Recover the exact owner of a source family without recording a use.
sourceTableOwner :: TyCon -> TcM (Maybe Id)
sourceTableOwner table
  | Just occ <- sourceTableTermOcc (nameOccName (tyConName table))
  , Just mod <- nameModule_maybe (tyConName table) = do
      name <- lookupOrig mod occ
      env <- getGblEnv
      owner <- if mod == tcg_semantic_mod env
        then pure $ case lookupTypeEnv (tcg_type_env env) name of
          Just (AnId fun) -> Just fun
          _ -> Nothing
        else do
          absent <- importedSourceOwnerAbsent table
          if absent then pure Nothing else tcLookupIdMaybe name
      pure $ do
        fun <- owner
        actual <- sourceTableId_maybe fun
        guard (tyConName actual == tyConName table)
        pure fun
  | otherwise = pure Nothing

-- | Confirm that an imported interface omits a source table's term owner.
importedSourceOwnerAbsent :: TyCon -> TcM Bool
importedSourceOwnerAbsent table = do
    env <- getGblEnv
    case (sourceTableTermOcc (nameOccName (tyConName table)), nameModule_maybe (tyConName table)) of
      (Just occ, Just mod) | mod /= tcg_semantic_mod env -> do
        iface <- loadInterfaceForName (text "source table owner") (tyConName table)
        pure (isNothing (mi_hash_fn iface occ))
      _ -> pure False

-- | Collect source families in a stable order, including beneath synonyms.
sourceFamilyTyCons :: [Type] -> [TyCon]
sourceFamilyTyCons tys = sortBy (\a b -> stableNameCmp (tyConName a) (tyConName b))
    [tc | tc <- nonDetEltsUniqSet (tyConsOfTypes tys)
        , not (isNothing (sourceTableTermOcc (nameOccName (tyConName tc))))]

-- | Read distinct source families from a closed table's right-hand sides.
sourceTableCallees :: TyCon -> Maybe [TyCon]
sourceTableCallees table = do
    axiom <- closedFamilyTyConCoAxiom_maybe table
    pure [tc | tc <- sourceFamilyTyCons $
                      map coAxBranchRHS (fromBranches (coAxiomBranches axiom))
             , tyConName tc /= tyConName table]

-- | Recover all checked callees from closed source equations.
-- See Note [Composed source equations].
sourceTableDependencies :: TyCon -> TcM (Maybe [Id])
sourceTableDependencies root = walk (unitNameSet (tyConName root)) [] [root]
  where
    walk _ owners [] = pure (Just (reverse owners))
    walk seen owners (table : rest)
      | Just referenced <- sourceTableCallees table = do
          let wanted = [tc | tc <- referenced, not (tyConName tc `elemNameSet` seen)]
          found <- traverse sourceTableOwner wanted
          case sequence found of
            Nothing -> pure Nothing
            Just callees -> walk (extendNameSetList seen (map tyConName wanted))
                                 (reverse callees ++ owners) (wanted ++ rest)
      | otherwise = pure Nothing

-- | Guard source families in a type, including families beneath synonyms.
sourceTableTypeUses :: Type -> TcM ()
sourceTableTypeUses ty = mapM_ sourceTableFamilyUse (sourceFamilyTyCons [ty])

-- | Check a source family when its axiom supplies type equality evidence.
-- See Note [Source family evidence uses].
sourceTableFamilyUse :: TyCon -> TcM ()
sourceTableFamilyUse table
  | isNothing (sourceTableTermOcc (nameOccName (tyConName table))) = pure ()
  | otherwise = do
      owner <- sourceTableOwner table
      checked <- maybe (pure Nothing) tcLookupSourceTableId owner
      when (isNothing checked) $ do
        -- See Note [Legacy private source leaves].
        absent <- importedSourceOwnerAbsent table
        case sourceTableCallees table of
          Just [] | absent -> do
            env <- getGblEnv
            updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
          _ -> failWithTc (TcRnRetainedQuantificationError
                           (RetainedUncheckedSourceCall (maybe (tyConName table) idName owner)))

{- Note [Source family evidence uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An imported value can have a generated source family in its type. A caller
can reduce this family without spelling a source function or enabling
ExperimentalDependentTypes. Check the family owner and its callees when
the solver uses a family reduction. Check only the active family head.
A type in an unused argument does not establish a use of that argument's
axiom. Cache and plugin reductions need the same check.
-}

{- Note [Legacy private source leaves]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An older interface can export a type synonym that mentions a private source
table but omit the private term. Allow this type-only use if the interface
has no declaration for the term and the closed axiom has no distinct source
family in its RHS. Record the table Name for recompilation.

A present term without SourceTableId is not a legacy omission. Reject it.
Retained calls and composed helper edges always require the exact owner.
Do not use this exception when resolving their dependencies.
-}

-- | Load the rules visible to the module, including rules from home boot cycles.
visibleSourceMethodRules :: TcGblEnv -> TcM RuleEnv
visibleSourceMethodRules env = do
    hsc <- getTopEnv
    eps <- getEps
    let mod = tcg_mod env
        visible = tcVisibleOrphanMods env
        -- GHCi's synthetic module has no node in the module graph. Start at
        -- its actual imports, as runTcInteractive does for instance visibility.
        from_import rules m = do
          info <- lookupHugByModule m (hsc_HUG hsc)
          case info of
            Nothing -> pure rules
            Just hmi -> do
              below <- hugRulesBelow hsc (moduleUnitId m) (GWIB (moduleName m) NotBoot)
              pure (extendRuleBaseList (plusUFM_C (++) rules below) (md_rules (hm_details hmi)))
    home_rules <- liftIO $ if isInteractiveModule mod
      then foldM from_import emptyRuleBase (moduleSetElts visible)
      else hugRulesBelow hsc (homeUnitId (hsc_home_unit hsc))
                         (GWIB (moduleName mod) (hscSourceToIsBoot (tcg_src env)))
    pure emptyRuleEnv { re_home_rules = home_rules, re_eps_rules = eps_rule_base eps
                      , re_visible_orphs = visible }

-- | Test for a visible user rewrite on the exact Id. Every class selector
-- has a built-in dictionary selection rule. That rule selects this field.
hasSourceMethodRule :: RuleEnv -> Id -> Bool
hasSourceMethodRule rules = hasSourceMethodRuleAt rules Nothing

-- | Skip rules whose first rough-match type constructor differs from the
-- actual type constructor. For example, eqString cannot rewrite (==) @Integer.
hasSourceMethodRuleAt :: RuleEnv -> Maybe Name -> Id -> Bool
hasSourceMethodRuleAt rules first fun = any user_rule (getRules rules (globaliseId fun))
  where
    user_rule BuiltinRule{} = False
    user_rule Rule{ru_rough = Just other : _} | Just actual <- first, actual /= other = False
    user_rule rule = not (isAutoRule rule || isNeverActive (ruleActivation rule))

-- | Check the selected method at the occurrence, and record its semantic dependencies.
recordSourceMethodUse :: Id -> Id -> Id -> TcM ()
recordSourceMethodUse dfun selector target =
    recordSourceUses [(v, Nothing) | v <- [selector, dfun, target]]

-- | The public implementations used by the standard Integer dictionaries.
-- Guard the comparison bodies that can inline as well as their method heads.
sourceLiteralWorkers :: TcM [Id]
sourceLiteralWorkers = traverse (\(mod, occ) -> lookupOrig mod (mkVarOcc occ) >>= tcLookupId) $
    (gHC_TYPES, "isTrue#") :
    [(gHC_INTERNAL_NUM_INTEGER, occ) | occ <- ["integerLt", "integerEq", "integerSub"
                                             , "integerLt#", "integerEq#", "integerCompare"]]

-- | Check rules on the selected conversion and its standard Integer operations.
-- See Note [Certifying a source literal conversion] in GHC.Tc.TyCl.Instance.
recordSourceLiteralUse :: Id -> Id -> TcM ()
recordSourceLiteralUse dfun selector = do
    let num_module = mkGhcInternalModule (fsLit "GHC.Internal.Num")
        lookup_id mod occ = lookupOrig mod occ >>= tcLookupId
        unchecked :: TcM a
        unchecked = failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall (idName selector)))
    (zero, _) <- maybe unchecked pure (sourceLiteralCons_maybe dfun selector)
    cmp <- lookup_id gHC_CLASSES (mkVarOcc "<")
    eq <- lookup_id gHC_CLASSES eqClassOpOcc
    sub <- lookup_id num_module minusClassOpOcc
    otherwise_id <- lookup_id gHC_INTERNAL_BASE (mkVarOcc "otherwise")
    -- This also loads the Integer module's ground comparison dictionaries.
    workers <- sourceLiteralWorkers
    insts <- tcGetInstEnvs
    let integer_dict op mod = case isClassOpId_maybe op of
          Just cls | Right (inst, []) <- lookupUniqueInstEnv insts cls [integerTy]
                   , let d = instanceDFunId inst
                   , nameModule_maybe (idName d) == Just mod -> pure d
          _ -> unchecked
    ord_d <- integer_dict cmp gHC_INTERNAL_NUM_INTEGER
    eq_d  <- integer_dict eq  gHC_INTERNAL_NUM_INTEGER
    num_d <- integer_dict sub num_module
    recordSourceUses $
       (selector, Just (tyConName (dataConTyCon zero))) :
       [(op, Just integerTyConName) | op <- [cmp, eq, sub, selector]] ++
       [(v, Nothing) | v <- [dfun, ord_d, eq_d, num_d, otherwise_id] ++ workers]

-- | Record rule heads and their ABI dependencies after checking the visible rules.
recordSourceUses :: [(Id, Maybe Name)] -> TcM ()
recordSourceUses ops = do
    env <- getGblEnv
    visible <- visibleSourceMethodRules env
    let changed (v, arg) = idName v `elemNameSet` sourceRuleOwnersAt arg (tcg_rules env)
                           || hasSourceMethodRuleAt visible arg v
        add acc (v, arg) = extendNameEnv_C combine acc (idName v) (maybe emptyNameSet unitNameSet arg)
        combine a b | isEmptyNameSet a || isEmptyNameSet b = emptyNameSet
                    | otherwise = unionNameSet a b
    case find changed ops of
      Just (v, _) -> failWithTc (TcRnRetainedQuantificationError (RetainedSourceRule (idName v)))
      Nothing -> do
        updTcRef (tcg_source_table_uses env) (`extendNameSetList` map (idName . fst) ops)
        updTcRef (tcg_source_rule_types env) (\uses -> foldl' add uses ops)

-- | Check rules declared or imported after an earlier retained application.
-- Report a local rule at its declaration, including one added by a splice.
checkSourceMethodUses :: TcGblEnv -> TcM ()
checkSourceMethodUses env = do
    uses <- filterNameSet (isVarOcc . nameOccName) <$> readTcRef (tcg_source_table_uses env)
    unless (isEmptyNameSet uses) $ do
      types <- readTcRef (tcg_source_rule_types env)
      let arguments name | Just ts <- lookupNameEnv types name, not (isEmptyNameSet ts)
                         = map Just (nameSetElemsStable ts)
                         | otherwise = [Nothing]
          affected rule name = any (\arg -> name `elemNameSet` sourceRuleOwnersAt arg [rule]) (arguments name)
          local = [(rule, filterNameSet (affected rule) (uses `intersectNameSet` sourceRuleOwners [rule]))
                  | rule <- tcg_rules env]
          imported = uses `minusNameSet` unionNameSets (map snd local)
          report name = addErr (TcRnRetainedQuantificationError (RetainedSourceRule name))
      forM_ local $ \(rule, names) ->
        setSrcSpanA (getLoc rule) (mapM_ report (nameSetElemsStable names))
      unless (isEmptyNameSet imported) $ do
        visible <- visibleSourceMethodRules env
        forM_ (nameSetElemsStable imported) $ \name -> do
          v <- tcLookupId name
          when (any (\arg -> hasSourceMethodRuleAt visible arg v) (arguments name)) (report name)

-- | Find source functions that a user rule can rewrite.
sourceRuleOwners :: [LRuleDecl GhcTc] -> NameSet
sourceRuleOwners = sourceRuleOwnersAt Nothing

-- | Exclude a local rule only when its first applied type constructor is known
-- and differs from the actual one. A polymorphic rule can match any argument.
sourceRuleOwnersAt :: Maybe Name -> [LRuleDecl GhcTc] -> NameSet
sourceRuleOwnersAt arg rules = unionNameSets
    [ case headOf (unLoc lhs) of
        Just (_, Just ty) | Just expected <- arg, Just (tc, _) <- tcSplitTyConApp_maybe ty
                          , isGenerativeTyCon tc Nominal, tyConName tc /= expected -> emptyNameSet
        Just (owner, _) -> unitNameSet owner
        Nothing -> lhs_fvs
    | L _ HsRule{rd_ext = (HsRuleRn lhs_fvs _, _), rd_lhs = lhs, rd_act = act} <- rules
    , not (isNeverActive act) ]
  where
    headOf :: HsExpr GhcTc -> Maybe (Name, Maybe Type)
    headOf (HsVar _ (L _ f)) = Just (varName f, Nothing)
    headOf (HsApp _ f _) = headOf (unLoc f)
    headOf (HsAppType ty f _) = add_app (Just ty) <$> headOf (unLoc f)
    headOf (HsPar _ f) = headOf (unLoc f)
    headOf (XExpr (WrapExpr _ f)) | isNothing arg = headOf f
    headOf (XExpr (WrapExpr w f)) = add_app <$> first_arg w <*> headOf f
    headOf (XExpr (ExpandedThingTc (HSE _ expansion))) = headOf (unLoc expansion)
    headOf _ = Nothing

    add_app arg (f, inner) = (f, inner <|> arg)
    first_arg (WpCompose outer inner) = (<|>) <$> first_arg inner <*> first_arg outer
    first_arg (WpTyApp ty) = Just (Just ty)
    first_arg WpHole = Just Nothing
    first_arg WpLet{} = Just Nothing
    first_arg WpEvApp{} = Just Nothing
    first_arg (WpSubType w) = first_arg w
    first_arg _ = Nothing

{- Note [Closed source tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For a definition such as

  flipBit :: Bool -> Bool
  flipBit False = True
  flipBit True  = False

make one closed family with the corresponding two equations. The left
and right sides refer to the promoted DataCons used in the checked
clauses. As in singletons-th, a family axiom justifies each reduction.
An application to an unknown input stays stuck. The term binding keeps
its ordinary Haskell pattern matching.

Each row matches one outer constructor of an ordinary lifted datatype.
For example, the clause previous (S n) = n contributes the equation
T:previous ('S n) = n. As in singletons-th, give each pattern field a
fresh type variable in that equation. Use the checked Id to find the
variable for a returned field. A wildcard gets a fresh variable too.
Matching a lazy field with a variable or wildcard leaves it unevaluated.
A strict field or a nested pattern would require another demand check.

Check the domain for complete, disjoint coverage before creating the
axiom. A row can return one of its fields or apply a result constructor.
It can also call the same function on one field of the input type. For
example, listCount (_ : xs) = S (listCount xs) checks because xs is a
proper subterm of the matched cons. Use the checked Ids for both the
function and the field. A call on a rebuilt constructor does not pass
this test. This is the direct structural decrease used in Agda.

Instantiate the input constructor at the parameters of the checked
domain. For listCount :: [Bool] -> Nat, the promoted cons takes Bool as
its first argument. Its field variables have kinds Bool and [Bool].
These constructor arguments occur in the family pattern. For example,
the promoted nil at [F ()] contains F () even though it has no fields.
Apply the same family-free check as Core Lint to the completed pattern.
The kind of a field variable may mention a family.
Check each RHS before tying the family to its axiom. A successful branch
builder only supplies the family TyCon at verified recursive calls.
Each branch keeps the source span of its equation.

For sourceLength :: forall a. [a] -> Nat, tcPolyCheck binds the body
skolem in a WpTyLam on the FunBind. The variable in the public type
can have a different Unique. Check the public list type with its own
binder. Check the constructor instantiations and the recursive type argument
with the body skolem. An inferred application carries a WpTyApp. A written
application carries the checked type in HsAppType. Give the family and each
branch fresh Core binders for the element type. In the branch LHS and the
recursive application, this element type precedes the list argument.

A declaration splice can introduce a type that uses a function from an
earlier group. At this boundary, simplifyTop has solved the constraints
for the group. Read the evidence bindings and follow the solved type of
each candidate. The final module zonker can run typed splices and default
metavariables, so do not run it on the group's terms here.

Note [Source table Ids]
~~~~~~~~~~~~~~~~~~~~~~~
A retained call needs both the runtime function and its type-level equations.
After checking the rows, keep the public Id and its closed family together.
Install SourceTableId on this Id in the global type environment. Desugaring
copies these details to the matching binder and occurrences. The original
external Name identifies the owner even if Tidy makes its name internal.
A fresh binder with copied details has a different Unique.

Store the family Name in the function's interface declaration. This makes
its ABI depend on the equations. The typechecker reads the same declaration
when it uses a no-code interface or follows a re-export. An hs-boot file has
no checked body and therefore supplies no certificate. Keep a private owner
alive when a type uses its table. Imported type aliases need this owner to
check its rewrite dependencies.

Constructor workers have no rewrite rules. See getRules in GHC.Core.Rules.
Exclude a source rule that can rewrite the owner or a checked callee.
An import cycle through an hs-boot
file can make a home rule for this Id visible to the producer. Check those
rules when rewriting is enabled. A declaration splice can add a rule after
an earlier group used the certificate. Recheck those uses at module finish.
As elsewhere in GHC, Core plugins must preserve the semantics of the program.

Note [Composed source equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A source equation can call another checked monomorphic function:

  times Z     m = Z
  times (S n) m = plus m (times n m)

The self call still decreases the original first operand and preserves the
carried operand. The call to plus uses its own checked termination argument.
Collect local tables in dependency order. Publish a candidate to the collector
only after all its branches pass. A cycle between different owners has no
first checked member, so the collector rejects it and its dependents.
Do not resolve a failed local candidate through an earlier installed table.

Helper calls must be saturated and have no type or evidence arguments.
Check their term argument types against the nominal family binder kinds and
check the result kind before constructing the family application. This also
excludes higher-order operands and polymorphic helper instantiations.

The family RHS records each helper's family Name. Recover its exact owner
through SourceTableId when installing or using a table. Follow this closure
to guard rewrite rules in downstream modules, including type-only uses.
Speculative collection must not record a use or keep a rejected owner alive.

Note [The second argument of a source equation]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The equations for plus can inspect their first argument and carry a
second argument unchanged:

  plus Z     m = m
  plus (S n) m = S (plus n m)

As in Agda's definition of addition, only the first argument decreases.
Compare the second operand of a recursive call with the row's second
pattern Id. For the first operand, look up the Id among the fields of
the matched constructor. When both arguments have type Nat, a call
such as plus m m would otherwise pass the type check without decreasing.

For an infix call such as n + m, tcExpand places the checked prefix
application under an ExpandedThingTc. The ExprCtxt identifies the
original OpApp. Check the expanded function and operands with the same
rules as a written (+) n m. See Note [Desugar OpApp in the typechecker]
in GHC.Tc.Gen.Head.

Represent the second variable by a fresh nominal binder in each axiom
branch. The equation can return this variable or put it in a constructor.
With Strict, desugaring adds a bang to the second variable pattern after
typechecking. Such a function must force its second argument even to
return an outer successor. Skip binary equations in that case.
-}

-- | Derive closed families after solving the group's top-level constraints.
sourceTableTyCons :: LHsBinds GhcTc -> TcM [(Id, TyCon)]
sourceTableTyCons binds = do
    strict <- xoptM LangExt.Strict
    collect strict [] candidates
  where
    candidates = mapMaybe (sourceRows . unLoc) binds
    candidate_names = extendNameSetList emptyNameSet [idName f | (f, _, _, _) <- candidates]
    local_names = extendNameSetList emptyNameSet $
      concatMap (map idName . collectHsBindBinders CollNoDictBinders . unLoc) binds

    collect strict checked pending = do
      let available = extendNameSetList emptyNameSet (map (idName . fst) checked)
          ready (_, _, _, rows) = all (\f -> not (idName f `elemNameSet` candidate_names)
                                               || idName f `elemNameSet` available)
                                    (concatMap rowCalls rows)
          (next, later) = partition ready pending
      if null next then pure checked else do
        tables <- catMaybes <$> traverse (certify strict checked) next
        collect strict (checked ++ tables) later

    rowCalls (SourceRow _ _ _ _ _ rhs) = sourceCalls rhs

    lookupCall checked actual = do
      env <- getGblEnv
      let current = case lookupTypeEnv (tcg_type_env env) (idName actual) of
            Just (AnId fun) | fun == actual -> sourceTableId_maybe fun
            _ -> Nothing
          predecessor = snd <$> find ((== actual) . fst) checked
      table <- if idName actual `elemNameSet` local_names
        then pure predecessor
        else pure $ if nameModule_maybe (idName actual) == Just (tcg_semantic_mod env)
                    then current else sourceTableId_maybe actual
      ty <- liftZonkM (zonkTcType (varType actual))
      pure $ do
        fam <- table
        (args, result) <- ordinaryArrows ty
        guard (not (null args) && length args == tyConArity fam)
        guard (eqTypes args (map tyVarKind (tyConTyVars fam)))
        guard (result `eqType` tyConResKind fam)
        pure (fam, args, result)

    ordinaryArrows ty = case splitFunTy_maybe ty of
      Just (FTF_T_T, ma, mult, arg, rest) -> do
        guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
        _ <- ordinaryDataType arg
        (args, result) <- ordinaryArrows rest
        pure (arg : args, result)
      _ -> do
        _ <- ordinaryDataType ty
        pure ([], ty)

    certify strict checked (fun, body_tv, evidence, rows) = do
      clean <- allM emptyEvidence evidence
      if clean then do
        ty <- liftZonkM (zonkTcType (varType fun))
        case checkedDomains strict body_tv ty rows of
          Nothing -> pure Nothing
          Just (input, carried, result) -> do
            branches <- traverse (sourceBranch (lookupCall checked) input carried result) rows
            traverse (fmap (setIdType fun ty,) . makeFamily (varName fun) input carried result)
                     (sequence branches)
      else pure Nothing

-- | The first constructor pattern, any carried variable, and the checked RHS.
data SourceRow = SourceRow SrcSpan DataCon [Type] [Maybe Id] [Id] SourceRhs

-- | The expression forms allowed in one source equation.
data SourceRhs = Field Id | Construct DataCon [SourceRhs] | Recur [Type] Id
               | Call Id [SourceRhs]

-- | Collect non-self calls, including calls inside operands.
sourceCalls :: SourceRhs -> [Id]
sourceCalls (Field _) = []
sourceCalls (Construct _ args) = concatMap sourceCalls args
sourceCalls (Recur _ _) = []
sourceCalls (Call fun args) = fun : concatMap sourceCalls args

-- | The input type of a closed table, or the body skolem of a polymorphic list table.
data SourceInput = ClosedInput Type [Type] | PolyList TyVar

-- | Collect the constructors and evidence for a candidate source function.
sourceRows :: HsBind GhcTc -> Maybe (Id, Maybe TyVar, [TcEvBinds], [SourceRow])
sourceRows binding = do
    (fun, mono, body_tv, MG { mg_ext = MatchGroupTc { mg_origin = FromSource },
               mg_alts = L _ matches }, evidence) <- case binding of
      XHsBindsLR (AbsBinds { abs_tvs = [], abs_ev_vars = [], abs_ev_binds = evs
                          , abs_exports = [ABE { abe_poly = fun, abe_mono = mono
                                              , abe_wrap = WpHole }]
                          , abs_binds = [L _ body], abs_sig = has_sig }) -> do
        (bound, body_tv, matches) <- funBind body
        guard (bound == mono)
        guard (isNothing body_tv || has_sig)
        pure (fun, mono, body_tv, matches, evs)
      _ -> do
        (fun, Nothing, matches) <- funBind binding
        pure (fun, fun, Nothing, matches, [])
    guard (isExternalName (varName fun))
    let self v = v == fun || v == mono
    (row_evidence, rows) <- unzip <$> traverse (sourceRow self) matches
    pure (fun, body_tv, evidence ++ row_evidence, rows)
  where
    funBind :: HsBind GhcTc -> Maybe (Id, Maybe TyVar, MatchGroup GhcTc (LHsExpr GhcTc))
    funBind (FunBind { fun_ext = (wrap, []), fun_id = L _ f, fun_matches = mg })
      = case wrap of
          WpHole -> Just (f, Nothing, mg)
          WpTyLam tv | isTyVar tv, tcIsLiftedTypeKind (tyVarKind tv) -> Just (f, Just tv, mg)
          _ -> Nothing
    funBind _ = Nothing

-- | Check that the rows cover the domain of the solved function type.
checkedDomains :: Bool -> Maybe TyVar -> Type -> [SourceRow] -> Maybe (SourceInput, [Type], TyCon)
checkedDomains strict body_tv ty rows = do
    (element, tau) <- case body_tv of
      Nothing -> pure (Nothing, ty)
      Just sk -> do
        (ma, Bndr tv (Invisible SpecifiedSpec), tau) <- splitForAllForAllTyBinder_maybe ty
        guard (ma `eqType` unmatchableDataConTy && isTyVar tv && tcIsLiftedTypeKind (tyVarKind tv))
        pure (Just (tv, sk), tau)
    (FTF_T_T, ma, mult, arg_ty, res_ty) <- splitFunTy_maybe tau
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    (domain, input) <- case element of
      Nothing -> do
        (tc, params) <- ordinaryDataType arg_ty
        pure (tc, ClosedInput arg_ty params)
      Just (tv, sk) -> do
        guard (arg_ty `eqType` mkListTy (mkTyVarTy tv))
        pure (listTyCon, PolyList sk)
    (carried, final_ty) <- case splitFunTy_maybe res_ty of
      Nothing -> pure ([], res_ty)
      Just (FTF_T_T, second_ma, second_mult, second_ty, final_ty)
        | ClosedInput{} <- input
        , not strict, second_ma `eqType` unmatchableDataConTy, isManyTy second_mult -> do
            _ <- ordinaryDataType second_ty
            pure ([second_ty], final_ty)
      _ -> Nothing
    (result, []) <- ordinaryDataType final_ty
    let expected = tyConDataCons domain
        actual   = [con | SourceRow _ con _ _ _ _ <- rows]
    guard (length actual == length expected && mkUniqSet actual == mkUniqSet expected)
    pure (input, carried, result)

-- | Recognise a closed instance of a datatype with ordinary lifted parameters.
ordinaryDataType :: Type -> Maybe (TyCon, [Type])
ordinaryDataType ty
  | Just (tc, args) <- splitTyConApp_maybe ty
  , isVanillaAlgTyCon tc, isBoxedDataTyCon tc, isLiftedAlgTyCon tc
  , length args == tyConArity tc, null (tyConStupidTheta tc)
  , all (tcIsLiftedTypeKind . tyVarKind) (tyConTyVars tc), all ordinaryField args
  , let cons = tyConDataCons tc
  , (null args && isEnumerationTyCon tc) ||
    (not (isGadtSyntaxTyCon tc) && not (null cons) && all (ordinaryCon args) cons)
  = Just (tc, args)
  | otherwise = Nothing
  where
    ordinaryCon args con =
      null (dataConRepArgBinders con) && isVanillaDataCon con &&
      length args == length (dataConUnivTyVars con) &&
      dataConUserTyVars con == dataConUnivTyVars con &&
      isNothing (dataConWrapId_maybe con) &&
      not (any isBanged (dataConImplBangs con)) &&
      all (ordinaryField . scaledThing) (dataConInstOrigArgTys con args)

    ordinaryField field =
      tcIsLiftedTypeKind (typeKind field) && noFreeVarsOfType field && isTauTy field

-- | Read one unguarded source equation with no local bindings.
sourceRow :: (Id -> Bool) -> LMatch GhcTc (LHsExpr GhcTc) -> Maybe (TcEvBinds, SourceRow)
sourceRow self (L loc (Match { m_pats = L _ (pat : rest), m_grhss = rhss }))
  | ConPat (ConPatTc params [] [] evs WpHole) (L _ (RealDataCon input)) details <- unParen pat
  , GRHSs _ (L _ (GRHS _ [] (L _ rhs)) :| []) (L _ (EmptyLocalBinds _)) <- rhss
  = do
      fields <- case details of
        PrefixCon _ ps   -> Just ps
        InfixCon _ p1 p2 -> Just [p1,p2]
        _                -> Nothing
      binders <- traverse fieldBinder fields
      carried <- case rest of
        [] -> Just []
        [p] | VarPat _ (L _ v) <- unParen p -> Just [v]
        _ -> Nothing
      output <- sourceRhs self carried rhs
      pure (evs, SourceRow (locA loc) input params binders carried output)
  where
    unParen :: LPat GhcTc -> Pat GhcTc
    unParen (L _ (ParPat _ p)) = unParen p
    unParen (L _ p) = p

    fieldBinder p = case unParen p of
      VarPat _ (L _ v) -> Just (Just v)
      WildPat _        -> Just Nothing
      _                -> Nothing
sourceRow _ _ = Nothing

-- | Recognise constructors, helper calls, and decreasing self calls.
sourceRhs :: (Id -> Bool) -> [Id] -> HsExpr GhcTc -> Maybe SourceRhs
sourceRhs self carried = go []
  where
    go :: [HsExpr GhcTc] -> HsExpr GhcTc -> Maybe SourceRhs
    go args (HsPar _ e)   = go args (unLoc e)
    go args (HsApp _ f x) = go (unLoc x : args) (unLoc f)
    go args (XExpr (ExpandedThingTc (HSE (ExprCtxt (OpApp{})) expansion))) =
      go args (unLoc expansion)
    go args (XExpr (ConLikeTc (RealDataCon con))) = Construct con <$> traverse (go []) args
    go [] (HsVar _ (L _ v)) = Just (Field v)
    go args (HsVar _ (L _ f)) | self f = call [] args
    go args (HsVar _ (L _ f)) | isExternalName (idName f) = Call f <$> traverse (go []) args
    go args (XExpr (WrapExpr (WpTyApp ty) (HsVar _ (L _ f)))) | self f = call [ty] args
    go args (HsAppType ty fun _)
      | HsVar _ (L _ f) <- unLoc (stripParensLHsExpr fun), self f = call [ty] args
    go _ _ = Nothing

    call tys (arg : rest) = do
      variables <- traverse variable rest
      guard (variables == carried)
      Recur tys <$> variable arg
    call _ [] = Nothing

    variable e = do { Field v <- go [] e; pure v }

-- | Check both the mutable evidence after solving and the final zonked evidence.
emptyEvidence :: TcEvBinds -> TcM Bool
emptyEvidence (EvBinds binds) = pure (isEmptyBag binds)
emptyEvidence (TcEvBinds var) = isEmptyEvBindsMap <$> getTcEvBindsMap var

-- | Instantiate the input with fresh Core binders for one family header or equation.
freshInput :: SourceInput -> TcM ([TyVar], Type, [Type])
freshInput (ClosedInput domain params) = pure ([], domain, params)
freshInput (PolyList sk) = do
    name <- newSysName (nameOccName (varName sk))
    let a = mkTyVar name liftedTypeKind
        ty = mkTyVarTy a
    pure ([a], mkListTy ty, [ty])

-- | Check a branch before supplying the family at its recursive calls.
sourceBranch :: (Id -> TcM (Maybe (TyCon, [Type], Type)))
             -> SourceInput -> [Type] -> TyCon -> SourceRow -> TcM (Maybe (TyCon -> CoAxBranch))
sourceBranch lookup_call input carried_types result (SourceRow loc con pat_args fields carried output) = do
    args <- liftZonkM (zonkTcTypes pat_args)
    carried_tys <- liftZonkM (zonkTcTypes (map varType carried))
    same_calls <- checkCalls output
    callees <- traverse (\f -> fmap (f,) <$> lookup_call f) (sourceCalls output)
    if same_calls && eqTypes args pattern_args && eqTypes carried_tys carried_types
       && length fields == dataConSourceArity con then do
      (quantified, domain, params) <- freshInput input
      let field_types = map scaledThing (dataConInstOrigArgTys con params)
          family_args = mkTyVarTys quantified
      tvs <- zipWithM fieldVar fields field_types
      carried_tvs <- zipWithM (fieldVar . Just) carried carried_types
      pure $ do
        checked_callees <- sequence callees
        let lhs = mkTyConApp (promoteDataCon con) (params ++ mkTyVarTys tvs)
            patterns = family_args ++ [lhs] ++ mkTyVarTys carried_tvs
            binders = quantified ++ tvs ++ carried_tvs
            carried_scope = zip (map Just carried) carried_tvs
        guard (all isTyFamFree patterns)
        rhs <- check_rhs checked_callees domain family_args (zip fields tvs) carried_scope (mkTyConTy result) output
        pure (\fam -> mkCoAxBranch binders [] [] patterns (rhs fam)
                        (map (const Nominal) binders) loc)
    else pure Nothing
  where
    (call_args, pattern_args) = case input of
      ClosedInput _ params -> ([], params)
      PolyList sk -> ([mkTyVarTy sk], [mkTyVarTy sk])

    checkCalls (Field _) = pure True
    checkCalls (Construct _ args) = allM checkCalls args
    checkCalls (Call _ args) = allM checkCalls args
    checkCalls (Recur tys _) = eqTypes call_args <$> liftZonkM (zonkTcTypes tys)

    fieldVar field ty = do
      let occ = maybe (mkTyVarOcc "_") (setOccNameSpace tvName . nameOccName . varName) field
      name <- newSysName occ
      pure (mkTyVar name ty)

    check_rhs callees domain family_args fields_scope carried_scope expected rhs = case rhs of
      Field v -> do
        tv <- lookup (Just v) (fields_scope ++ carried_scope)
        guard (tyVarKind tv `eqType` expected)
        pure (const (mkTyVarTy tv))
      Recur _ v -> do
        -- See Note [The second argument of a source equation].
        tv <- lookup (Just v) fields_scope
        guard (tyVarKind tv `eqType` domain && mkTyConTy result `eqType` expected)
        let operands = mkTyVarTys (tv : map snd carried_scope)
        pure (\fam -> mkTyConApp fam (family_args ++ operands))
      Construct dc args -> do
        (tc, []) <- ordinaryDataType expected
        guard (dataConTyCon dc == tc && length args == dataConSourceArity dc)
        let arg_tys = map scaledThing (dataConOrigArgTys dc)
        children <- zipWithM (check_rhs callees domain family_args fields_scope carried_scope) arg_tys args
        pure (\fam -> mkTyConApp (promoteDataCon dc) [child fam | child <- children])
      Call fun args -> do
        (table, arg_tys, res_ty) <- lookup fun callees
        guard (length args == length arg_tys && res_ty `eqType` expected)
        children <- zipWithM (check_rhs callees domain family_args fields_scope carried_scope) arg_tys args
        pure (\fam -> mkTyConApp table [child fam | child <- children])

-- | Give the table the representation of a closed, non-injective family.
makeFamily :: Name -> SourceInput -> [Type] -> TyCon -> [TyCon -> CoAxBranch] -> TcM TyCon
makeFamily fun input carried_types result branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    (quantified, domain, _) <- freshInput input
    args <- zipWithM argVar ["x", "y"] (domain : carried_types)
    let binders = mkNamedTyConBinders (Invisible SpecifiedSpec) quantified ++
                  mkAnonTyConBinders args
        res_kind = mkTyConTy result
        kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map ($ fam_tc) branches)
    pure fam_tc
  where
    argVar occ ty = do { n <- newSysName (mkTyVarOcc occ); pure (mkTyVar n ty) }
