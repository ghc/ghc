-- | Closed type families for checked functions that match one constructor.
module GHC.Tc.Gen.SourceTable (recordSourceUses, recordUnfoldedSourceUses, sourceTableTyCons, sourceCaseTables, finalCoreMappers, verifyFinalCoreMapper, failedFinalListTables, finalCoreSourceFamilies, isFinalCoreIdentity, indexedProjectionSource, isFinalCoreReverseWrapper, sourcePairBuilderField, lookupObservedPairField, lookupPairObserver, lookupPairRecipe, canObservePairSource, sourceRuleOwners, sourceTableOccurrence, tcLookupSourceTableId,
                              sourceTableCallees, sourceTableDependencies, sourceTableTypeUses, sourceTableFamilyUse,
                              sourceTableFamilyAppUse, sourceTableReferenceUse, sourceCaseKindUse, sourceCaseVisibleArity, isSourceConstructorCase, sourceTableOwner, monomorphicEnumCaseArity, groundFirstHead,
                              visibleSourceMethodRules, hasSourceMethodRule, hasSourceMethodRuleAt,
                              sourceBinarySelector, sourceMapSelector, lookupSourceBinaryMethod, lookupSourceContainerMethod, lookupSourceGroundDFun, lookupSourceCheckedMapMethod, publishListFoldMethods, keepCheckedMapWorkers, publishCheckedMapMethods, sourceRuleOwnersAt, recordSourceMethodUse, recordSourceLiteralUse, sourceLiteralWorkers, isSourceFilterGuard,
                              OptionalSourceUses, optionalSourceUses, registerOptionalSourceUses,
                              checkOptionalSourceUses, checkSourceMethodUses,
                              withSourceProofSignature,
                              recordSourceProofAliasUse, sourceProofRulesFor, takeSourceProofRulesFor,
                              captureSourceMethodProof, closedSourceProofArgs,
                              checkAndInstallSourceProofs) where

import GHC.Prelude

import GHC.Builtin.KnownOccs (eqClassOpOcc, minusClassOpOcc)
import GHC.Core.Class (Class, classAllSelIds, classMethods, className, classTyCon, classTyVars)
import GHC.Builtin.Modules (gHC_CLASSES, gHC_INTERNAL_BASE, gHC_INTERNAL_NUM_INTEGER, gHC_TYPES, mkGhcInternalModule)
import GHC.Builtin.WiredIn.Types (nilDataCon, consDataCon, boolTy, falseDataCon, trueDataCon, charTy, charTyCon, typeSymbolKind, stringTy, integerTy, integerTyConName, listTyCon, mkListTy, mkTupleTy, promotedTupleDataCon, tupleDataCon, tupleTyCon, unmatchableDataConTy)
import GHC.Core (CoreRule(..), isAutoRule, ruleActivation)
import qualified GHC.Core as Core
import GHC.Core.CharCase (certOwner, certRows, certify)
import GHC.Core.OpenWhnf (openDataConFieldTypes)
import GHC.Core.Predicate (classMethodInstTy, getClassPredTys_maybe, isPredTy, mkClassPred)
import GHC.Core.Rules (RuleEnv(..), emptyRuleBase, emptyRuleEnv, extendRuleBaseList, getRules)
import GHC.Core.Coercion (abstractNativeIndices, coercionLKind, coercionRKind, coercionRole, mkNomReflCo, mkSymCo, mkTyConAppCo)
import GHC.Core.Coercion.Axiom (ClosedTyFam(..), CoAxBranch, coAxiomBranches, coAxiomRole,
                                coAxBranchCoVars, coAxBranchLHS, coAxBranchRHS, coAxBranchRoles, coAxBranchTyVars,
                                fromBranches)
import GHC.Core.ConLike (ConLike(..))
import GHC.Core.DataCon
import GHC.Core.Dependent (hasNativeDependentExprRhs)
import GHC.Core.FVs (exprSomeFreeVarsList)
import GHC.Core.FamInstEnv (FamInstEnvs, mkBranchedCoAxiom, mkCoAxBranch, reduceTyFamApp_maybe)
import GHC.Core.InstEnv (ClsInst(is_tcs), classInstances, instanceCantMatch, instanceDFunId,
                         isIncoherent, lookupUniqueInstEnv, roughMatchTcs)
import GHC.Core.RoughMap (RoughMatchTc(RM_KnownTc))
import GHC.Core.Multiplicity (scaledThing)
import GHC.Core.Reduction (reductionCoercion, reductionReducedType)
import GHC.Core.SimpleOpt (simpleOptExpr)
import GHC.Core.Tidy (tidyCbvInfoTop)
import GHC.Core.TyCo.Compare (eqType, eqTypes)
import GHC.Core.TyCo.FVs (tyConsOfTypes)
import GHC.Core.TyCo.Rep (Coercion, Type(TyConApp, TermVarTy, CastTy))
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Utils (dataConRepInstPat, exprType, findAlt, mergeAlts, stripTicksE, stripTicksTopE)
import GHC.Data.Graph.Directed (Node(..), SCC(..), stronglyConnCompFromEdgedVerticesUniq)
import GHC.Data.FastString (fsLit)
import GHC.Data.Bag (isEmptyBag)
import GHC.Data.OrdList (fromOL)
import GHC.Driver.Backend (backendGeneratesCode)
import GHC.Driver.Config (initSimpleOpts)
import GHC.Driver.Session (GeneralFlag(Opt_EnableRewriteRules), backend, HasDynFlags(getDynFlags))
import GHC.Driver.Env (hsc_HUG, hsc_home_unit, hugRuleInterfacesBelow, hugRulesBelow)
import GHC.Hs
import {-# SOURCE #-} GHC.HsToCore.Binds (dsTopLHsBinds)
import GHC.HsToCore.Errors.Ppr () -- Diagnostic DsMessage
import GHC.HsToCore.Monad (initDsTc)
import GHC.Iface.Env (lookupOrig)
import GHC.Iface.Load (loadInterfaceForName)
import qualified GHC.LanguageExtensions as LangExt
import GHC.Tc.Errors.Types (RetainedQuantificationError(..), TcRnMessage(..))
import GHC.Tc.Instance.Family (tcGetFamInstEnvs)
import GHC.Tc.TyCl.Build (newImplicitBinder)
import GHC.Tc.Types.ErrCtxt (HsCtxt(ExprCtxt))
import GHC.Tc.Types.Evidence (HsWrapper(..), TcEvBinds(..), EvBindsState(..), isEmptyEvBindsMap)
import GHC.Tc.Types (TcGblEnv(..), SourceRuleUses(..), tcVisibleOrphanMods)
import GHC.Tc.Utils.Instantiate (freshenTyVarBndrs)
import GHC.Tc.Utils.Env (tcLookupGlobal, tcLookupId, tcLookupIdMaybe, tcGetInstEnvs)
import GHC.Tc.Utils.Monad (TcM, addErr, failWithTc, getEps, getGblEnv, getGlobalRdrEnv, getTcEvBindsMap, getTcEvBindsState,
                           checkNoErrs, getTopEnv, goptM, keepAlive, liftZonkM, newSysName, readTcRef, writeTcRef, newTcRef, setGblEnv,
                           recordHomeRuleDependencies, setSrcSpan, setSrcSpanA, updTcRef, xoptM)
import GHC.Tc.Utils.TcType (isTyFamFree)
import GHC.Tc.Zonk.TcType (zonkTcType, zonkTcTypes)
import GHC.Types.Avail (availNames, availsToNameSet)
import GHC.Types.Basic (Origin(..), LeftOrRight(..), SourceTableOrigin(..), UnfoldingSource(VanillaSrc),
                        isMarkedCbv, pickLR)
import GHC.Types.Error (errorsFound)
import GHC.Types.Id.Info (IdDetails(DFunId, SourceTableId, VanillaId), SourceDFun(..), setSourceProofRuleInfo)
import GHC.Types.Id (globaliseId, idCbvMarks_maybe, idCoreRules, idInfo, idName, setIdInfo, idInlinePragma, isClassOpId_maybe, isDataConId_maybe, isDataConWorkId_maybe, isJoinId, idUnfolding, realIdUnfolding, mkVanillaGlobal, setIdType, sourceTableId_maybe, sourceLiteralCons_maybe,
                     setIdExported, setSourceMethodIds, sourceMethodAliases, sourceMethodTarget_maybe,
                     sourceMethodDFunClass)
import GHC.Types.InlinePragma (isNeverActive, isOpaquePragma)
import GHC.Types.Name (Name, OccName, isExternalName, mkSourceTableOcc, mkSourceFstOcc, mkSourceSndOcc, mkInstTyCoOcc, mkTcOcc,
                       isVarOcc, mkTyVarOcc, mkVarOcc, nameModule_maybe, nameOccName, nameSrcSpan, occNameString, setOccNameSpace, sourceTableTermOcc, stableNameCmp, nameStableString, tvName, SourceProofRule, SourceProofArgs, SourceProofArg(..))
import GHC.Core.Opt.Pipeline (pairObservationOcc, pairObservationType, scalarRecipeTableOcc)
import GHC.Types.Name.Reader (gre_imp, is_decl, is_isboot, lookupGRE_Name)
import GHC.Types.Name.Env (delFromNameEnv, extendNameEnv_C, lookupNameEnv, mkNameEnv, plusNameEnv_C, isEmptyNameEnv)
import GHC.Types.Name.Set (NameSet, emptyNameSet, elemNameSet, extendNameSet, extendNameSetList, filterNameSet,
                           intersectsNameSet,
                           intersectNameSet, isEmptyNameSet, minusNameSet, mkNameSet, nameSetElemsStable,
                           unionNameSet, unionNameSets, unitNameSet)
import GHC.Types.SrcLoc
import GHC.Types.Tickish (CoreTickish, GenTickish(Breakpoint))
import GHC.Types.SourceFile (hscSourceToIsBoot, isHsBootOrSig)
import GHC.Unit.External (eps_rule_base)
import GHC.Unit.Home (homeUnitId)
import GHC.Unit.Home.Graph (lookupHugByModule)
import GHC.Unit.Home.ModInfo (hm_details)
import GHC.Unit.Module (Module, getModuleInstantiation, isHoleModule)
import GHC.Unit.Module.Deps (imp_boot_mods)
import GHC.Unit.Module.Env (lookupInstalledModuleEnv, moduleSetElts)
import GHC.Unit.Module.ModDetails (md_rules)
import GHC.Unit.Module.ModIface (mi_hash_fn)
import GHC.Unit.Types (GenWithIsBoot(..), isInteractiveModule, moduleName, moduleUnitId)
import GHC.Types.TypeEnv (lookupTypeEnv, extendTypeEnvWithIds, typeEnvIds)
import GHC.Types.TyThing (TyThing(..))
import GHC.Types.Unique.FM (plusUFM_C)
import GHC.Types.Unique.Set (mkUniqSet, nonDetEltsUniqSet, sizeUniqSet)
import GHC.Types.Unique.Supply (UniqSupply, listSplitUniqSupply, splitUniqSupply, uniqsFromSupply)
import GHC.Types.Var (Id, CoVar, idDetails, idMult, VarBndr(..), isId, isCoVar, mkTyVar, setIdDetails, varName)
import GHC.Types.Var.Set (elemVarSet)
import GHC.Utils.Misc (equalLength, lengthAtLeast, lengthIs)
import GHC.Utils.Monad (allM, anyM, whenM)
import GHC.Utils.Outputable (text)
import GHC.Utils.Panic (panic)

import Control.Monad (filterM, foldM, forM, forM_, guard, unless, void, when, zipWithM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative ((<|>))
import Data.Functor.Identity (Identity(..))
import Data.List (find, findIndex, nubBy, partition, sortBy, stripPrefix, unsnoc)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Semigroup ((<>))

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
tcLookupSourceTableId = tcLookupSourceTableIdAt Nothing

-- | Filter only the exact root owner at a checked specialization.
tcLookupSourceTableIdAt :: Maybe Name -> Id -> TcM (Maybe Id)
tcLookupSourceTableIdAt first actual = do
    env <- getGblEnv
    case sourceTableOccurrence env actual of
      Just fun | Just table <- sourceTableId_maybe fun -> do
        dependencies <- sourceTableDependencies table
        case dependencies of
          Nothing -> return Nothing
          Just callees -> do
            recordSourceUses ((fun, first) : [(v, Nothing) | v <- callees])
            forM_ (fun : callees) $ \owner ->
              when (nameModule_maybe (idName owner) == Just (tcg_semantic_mod env)) $
                keepAlive (idName owner)
            updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
            return (Just fun)
      _ -> return Nothing

-- | Recover the exact owner of a source family without recording a use.
sourceTableOwner :: TyCon -> TcM (Maybe Id)
sourceTableOwner table
  | Just occ <- sourceEquationOwnerOcc (nameOccName (tyConName table))
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
        guard (not (finalCoreMapperOwner fun && isJust (indexedProjectionType (varType fun))) ||
               isJust (indexedProjectionSource fun))
        guard (tyConName actual == tyConName table ||
               (finalCoreMapperOwner fun && isFilterGuardFor actual table &&
                maybe False (any ((== tyConName table) . tyConName)) (sourceTableCallees actual)))
        pure fun
  | otherwise = pure Nothing

-- | Recover the common owner for a checked list function and its guard.
-- See Note [Checked guarded list equations].
sourceEquationOwnerOcc :: OccName -> Maybe OccName
sourceEquationOwnerOcc occ = do
    term <- sourceTableTermOcc occ
    pure (maybe term mkVarOcc (stripPrefix "#guard:" (occNameString term)))

-- | Confirm that an imported interface omits a source table's term owner.
importedSourceOwnerAbsent :: TyCon -> TcM Bool
importedSourceOwnerAbsent table = do
    env <- getGblEnv
    case (sourceEquationOwnerOcc (nameOccName (tyConName table)), nameModule_maybe (tyConName table)) of
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
            Just callees ->
              let separate called fun =
                    not (maybe False (\source -> isFilterGuardFor source called) (sourceTableId_maybe fun))
                  new_owners = [fun | (called, fun) <- zip wanted callees, separate called fun]
              in walk (extendNameSetList seen (map tyConName wanted))
                      (reverse new_owners ++ owners) (wanted ++ rest)
      | otherwise = pure Nothing

-- | Guard source families in a type, including families beneath synonyms.
sourceTableTypeUses :: Type -> TcM ()
sourceTableTypeUses ty
  | TyConApp tc args <- coreFullView ty = do
      unless (isNothing (sourceTableTermOcc (nameOccName (tyConName tc)))) $
        record_or_use tc (Just args)
      mapM_ sourceTableTypeUses args
  | otherwise = mapM_ (\tc -> record_or_use tc Nothing) (sourceFamilyTyCons [ty])

-- | A saturated reference can be stored without consuming an equation.
sourceTableReferenceUse :: TyCon -> TcM ()
sourceTableReferenceUse table = record_or_use table Nothing

record_or_use :: TyCon -> Maybe [Type] -> TcM ()
record_or_use table args = do
    owner <- sourceTableOwner table
    case owner of
      Just fun | finalCoreMapperOwner fun -> do
        env <- getGblEnv
        updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
      _ -> maybe (sourceTableFamilyUse table) (sourceTableFamilyAppUse table) args

-- | Check a source family when its axiom supplies type equality evidence.
-- See Note [Source family evidence uses].
sourceTableFamilyUse :: TyCon -> TcM ()
sourceTableFamilyUse table = sourceTableFamilyUseAt table Nothing

-- | Record evidence use at an application, including cache and plugin reductions.
-- See Note [Source family evidence uses].
sourceTableFamilyAppUse :: TyCon -> [Type] -> TcM ()
sourceTableFamilyAppUse table args
  | not (args `lengthAtLeast` tyConArity table)
  , not (isNothing (sourceTableTermOcc (nameOccName (tyConName table)))) = do
      -- Carry the exact identity through interfaces. A bare callback
      -- does not use an equation or publish a RULE restriction.
      env <- getGblEnv
      updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
  | Just visible <- sourceCaseVisibleArity table <|>
                      (length (dropWhile isInvisibleTyConBinder (tyConBinders table)) <$ sourcePairBuilderField table)
  , lengthIs args (tyConArity table) = do
      kinds <- liftZonkM (zonkTcTypes (take (tyConArity table - visible) args))
      sourceTableFamilyUseAt table (groundFirstHead kinds)
  | otherwise = sourceTableFamilyUse table

-- | Check specialized kinds before running a constructor case operand.
sourceCaseKindUse :: TyCon -> [Kind] -> TcM ()
sourceCaseKindUse table kinds
  | Just visible <- sourceCaseVisibleArity table
  , lengthIs kinds (tyConArity table - visible)
  = sourceTableFamilyUseAt table (groundFirstHead kinds)
  | otherwise = sourceTableFamilyUse table

-- | A ground, family-free first type argument can disprove a rough rule match.
groundFirstHead :: [Type] -> Maybe Name
groundFirstHead args@(first : _)
  | all noFreeVarsOfType args, all isTyFamFree args
  , Just (tc, _) <- tcSplitTyConApp_maybe first
  , isGenerativeTyCon tc Nominal
  = Just (tyConName tc)
groundFirstHead _ = Nothing

-- | Check the exact owner and record its dependency with the available precision.
sourceTableFamilyUseAt :: TyCon -> Maybe Name -> TcM ()
sourceTableFamilyUseAt table first
  | isNothing (sourceTableTermOcc (nameOccName (tyConName table))) = pure ()
  | otherwise = do
      owner <- sourceTableOwner table
      checked <- maybe (pure Nothing) (tcLookupSourceTableIdAt first) owner
      when (isNothing checked) $ do
        -- See Note [Legacy private source leaves].
        absent <- importedSourceOwnerAbsent table
        case sourceTableCallees table of
          Just [] | absent, not (isSourceFilterGuard table) -> do
            env <- getGblEnv
            updTcRef (tcg_source_table_uses env) (`extendNameSet` tyConName table)
          _ -> failWithTc (TcRnRetainedQuantificationError
                           (RetainedUncheckedSourceCall (maybe (tyConName table) idName owner)))

{- Note [Source family evidence uses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Each Flip xs, a partial family can appear as an operand. Export
the identity of Flip so an ordinary reader can instantiate Each. In
the empty branch the reader needs no equation of Flip. In the cons
branch, rewriting Flip x uses an equation. Record that use in the
client itself, including when it has only the original alias interface.
See NativeOpenMap for the changed-clause and matching-rule tests.

An imported value can have a generated source family in its type. A caller
can reduce this family without spelling a source function or enabling
ExperimentalDependentTypes. Check the family owner and its callees when
the solver uses a family reduction. Check only the active family head.
A type in an unused argument does not establish a use of that argument's
axiom. Cache and plugin reductions need the same check.

For a checked constructor case, the final family argument represents the
term scrutinee. The preceding kind arguments instantiate the ordinary function.
Use that prefix when comparing an explicit type argument in a rewrite
rule. A pair of unknown Booleans still rules out a Char specialization.
Before final zonking, the first type argument of a rule with a typed
term binder can be a filled metavariable. Read that argument before
comparing heads. An unresolved argument still permits a match.

An imported synonym can contain two nested source applications. Keep
the arguments at each application when checking the synonym RHS. A
set of family names loses the specialization that excludes a disjoint
rule. Retain the conservative check for families in casts and other
types where this walk does not recover the application.
-}

{- Note [Legacy private source leaves]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An older interface can export a type synonym that mentions a private source
table but omit the private term. Allow this type-only use if the interface
has no declaration for the term and the closed axiom has no distinct source
family in its RHS. Record the table Name for recompilation.

A present term without SourceTableId is not a legacy omission. Reject it.
Retained calls, composed helper edges and generated list guards require
the exact owner. Do not use this exception for those equations.
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
hasSourceMethodRuleAt rules first = hasSourceMethodRuleAtArgs rules first []

-- | An imported rule can also be apart at a checked value argument.
hasSourceMethodRuleAtArgs :: RuleEnv -> Maybe Name -> SourceProofArgs -> Id -> Bool
hasSourceMethodRuleAtArgs rules first checked fun = any user_rule (getRules rules (globaliseId fun))
  where
    user_rule BuiltinRule{} = False
    user_rule Rule{ru_rough = Just other : _, ru_args = Core.Type ty : _}
      | Just actual <- first, actual /= other
      , Just (tc, _) <- tcSplitTyConApp_maybe ty
      , tyConName tc == other, isGenerativeTyCon tc Nominal = False
    user_rule Rule{ru_args = args}
      | not (null checked)
      , dictionary : values <- filter (not . Core.isTypeArg) args
      , not (Core.isTyCoArg dictionary), isPredTy (exprType dictionary)
      , not (any Core.isTyCoArg values)
      , sourceProofArgsApart checked (closedSourceProofArgs (map Just values))
      = False
    user_rule rule = not (isAutoRule rule || isNeverActive (ruleActivation rule))

-- | Retain only syntactic constructor applications at checked value positions.
closedSourceProofArgs :: [Maybe Core.CoreExpr] -> SourceProofArgs
closedSourceProofArgs args = case map (maybe SourceProofUnknown (constructorArg view)) args of
    shapes | all (== SourceProofUnknown) shapes -> []
           | otherwise -> shapes
  where
    view e
      | (Core.Var fun, args) <- Core.collectArgs e
      , Just con <- isDataConWorkId_maybe fun
      , let fields = dropWhile Core.isTypeArg args
      , not (any Core.isTyCoArg fields)
      = Just (con, fields)
      | otherwise = Nothing

-- | Keep only saturated ordinary data constructors.
-- See Note [No RULES on datacons] in GHC.HsToCore.Binds.
constructorArg :: (e -> Maybe (DataCon, [e])) -> e -> SourceProofArg Name
constructorArg view = go
  where
    go expr
      | Just (con, fields) <- view expr
      , let tc = dataConTyCon con
      , isVanillaDataCon con, isVanillaAlgTyCon tc, isBoxedDataTyCon tc
      , isNothing (dataConWrapId_maybe con)
      , lengthIs fields (dataConSourceArity con)
      = SourceProofCon (dataConName con) (map go fields)
      | otherwise = SourceProofUnknown

-- | Prove that the application and rule differ at the same saturated path.
sourceProofArgsApart :: SourceProofArgs -> SourceProofArgs -> Bool
sourceProofArgsApart checked rule =
    lengthIs rule (length checked) && or (zipWith apart checked rule)
  where
    apart (SourceProofCon a as) (SourceProofCon b bs)
      | lengthIs bs (length as) = a /= b || or (zipWith apart as bs)
    apart _ _ = False

-- | Read a local rule before desugaring. Only application wrappers
-- preserve the value positions used by the constructor check.
localSourceRuleApart :: Name -> SourceProofArgs -> LRuleDecl GhcTc -> Bool
localSourceRuleApart owner checked (L _ HsRule{rd_lhs = lhs})
  | not (null checked)
  , (HsVar _ (L _ id), args) <- applications True (unLoc lhs) []
  , idName id == owner
  = sourceProofArgsApart checked (map (constructorArg view) args)
  | otherwise = False
  where
    view expr
      | (XExpr (ConLikeTc (RealDataCon con)), fields) <- applications False expr []
      = Just (con, fields)
      | otherwise = Nothing

    applications allow_evidence expr args = case expr of
      HsApp _ fun arg -> applications allow_evidence (unLoc fun) (unLoc arg : args)
      HsAppType _ fun _ -> applications allow_evidence (unLoc fun) args
      HsPar _ e -> applications allow_evidence (unLoc e) args
      XExpr (WrapExpr wrap e)
        | transparent allow_evidence wrap -> applications allow_evidence e args
      _ -> (expr, args)

    transparent _ WpHole = True
    transparent _ WpTyApp{} = True
    transparent allow WpEvApp{} = allow
    transparent allow WpLet{} = allow
    transparent allow (WpCompose a b) = transparent allow a && transparent allow b
    transparent allow (WpSubType wrap) = transparent allow wrap
    transparent _ _ = False

-- | Check that every visible rule of a certified mapper preserves its two list equations.
-- See Note [Checked list traversal rules].
checkedMapperRules :: RuleEnv -> Id -> Bool
checkedMapperRules env owner = case activeMapperRules env owner of
    [rule] | Just table <- sourceTableId_maybe owner
           , isTwoListCaseTable table
           -> isJust (checkedImportedSpecConstr env owner table rule)
    rules | Just table <- sourceTableId_maybe owner
          , isListFoldTable table || isListZipWithTable table || isReverseWorkerTable table ||
            isJust (reverseWrapperWorker table) ||
            isTwoListCaseTable table -> null rules
    [] -> False
    rules -> all (\rule -> identityTraversal rule || consTraversal rule || composeTraversals rule) rules
  where
    identityTraversal Rule{ru_fn = fn, ru_bndrs = [a, xs],
                           ru_args = [Core.Type input, Core.Type output,
                                      Core.Lam x (Core.Var y), Core.Var arg],
                           ru_rhs = Core.Var result}
      = fn == idName owner && getTyVar_maybe input == Just a && eqType input output
        && isId x && x == y && eqType (varType x) input
        && isId xs && xs == arg && arg == result && eqType (varType xs) (mkListTy input)
    identityTraversal _ = False

    consTraversal Rule{ru_fn = fn, ru_bndrs = binders,
                       ru_args = [Core.Type input, Core.Type output, Core.Var f, spine],
                       ru_rhs = rhs}
      | fn == idName owner
      , Just a <- getTyVar_maybe input, Just b <- getTyVar_maybe output
      , (Core.Var in_cons, [Core.Type in_ty, Core.Var x, Core.Var xs]) <- Core.collectArgs spine
      , in_cons == dataConWorkId consDataCon, eqType in_ty input
      , lengthIs binders 5, mkUniqSet binders == mkUniqSet [a, b, f, x, xs]
      , isId f, isId x, isId xs
      , Just (domain, range) <- caseArrow (varType f)
      , eqType input domain, eqType output range
      , eqType (varType x) input, eqType (varType xs) (mkListTy input)
      , (Core.Var out_cons, [Core.Type out_ty, hd, tl]) <- Core.collectArgs rhs
      , out_cons == dataConWorkId consDataCon, eqType out_ty output
      , (Core.Var callback, [Core.Var h']) <- Core.collectArgs hd
      , callback == f, h' == x
      , Just (a', b', Core.Var f', Core.Var t') <- mapperCall tl
      = eqType a' input && eqType b' output && f' == f && t' == xs
    consTraversal _ = False

    composeTraversals Rule{ru_fn = fn, ru_bndrs = binders,
                           ru_args = [Core.Type middle, Core.Type output, Core.Var f, inner],
                           ru_rhs = rhs}
      | fn == idName owner
      , Just (input, middle', Core.Var g, Core.Var xs) <- mapperCall inner
      , eqType middle' middle
      , Just a <- getTyVar_maybe input
      , Just b <- getTyVar_maybe middle
      , Just c <- getTyVar_maybe output
      , lengthIs binders 6, mkUniqSet binders == mkUniqSet [a, b, c, f, g, xs]
      , isId f, isId g, isId xs
      , Just (f_in, f_out) <- caseArrow (varType f)
      , eqType f_in middle, eqType f_out output
      , Just (g_in, g_out) <- caseArrow (varType g)
      , eqType g_in input, eqType g_out middle
      , eqType (varType xs) (mkListTy input)
      , Just (input', output', Core.Lam x body, Core.Var xs') <- mapperCall rhs
      , eqType input' input, eqType output' output, xs' == xs
      , isId x, x `notElem` binders, eqType (varType x) input
      , (Core.Var f', [g_app]) <- Core.collectArgs body
      , (Core.Var g', [Core.Var x']) <- Core.collectArgs g_app
      = f' == f && g' == g && x' == x
    composeTraversals _ = False

    mapperCall expr = case Core.collectArgs expr of
      (Core.Var mapper, [Core.Type input, Core.Type output, callback, spine])
        | mapper == owner -> Just (input, output, callback, spine)
      _ -> Nothing

activeMapperRules :: RuleEnv -> Id -> [CoreRule]
activeMapperRules env =
    filter (not . isNeverActive . ruleActivation) . getRules env . globaliseId

{- Note [Checked list traversal rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For the checked list mapper, mapping the identity lambda over an empty
list gives the empty list. On a cons it returns the original head and
maps the same lambda over the tail. Neither side forces the head or
the tail. These equations also preserve a divergent spine.

On a cons, a rule can also expose the checked mapper's own equation.
The left side supplies the list constructor, so neither side needs to
force its fields. The right side builds a lazy cons with the same
callback application and the same recursive call on the input tail.
The callback stays a rule parameter. This reasoning covers a divergent
callback and does not require properties of any particular function.

Two traversals can become one with the lambda \x -> f (g x). Both
sides inspect the original spine and return [] for []. For a cons,
both produce the same lazy head f (g x). Their tails satisfy the same
equation on the original input tail. This argument also covers partial
and infinite lists because each observation of a cons needs one step.
In particular, g can diverge when f ignores its argument.

The cons and composition rules introduce no fixed rule head except
the same mapper.
See getRules in GHC.Core.Rules for the list constructor.

Check the binders and both sides of every visible rule. An unverified
rule can change the result or its demand even if the other rules pass.
Recheck after processing declarations and splices.
-}

-- | Check the selected method at the occurrence, and record its semantic dependencies.
recordSourceMethodUse :: Id -> Id -> Id -> TcM ()
recordSourceMethodUse dfun selector target = do
    recordSourceUses (sourceMethodOps dfun selector target)
    captureSourceMethodProof dfun selector target ([], [])

-- | Record the observed method in the current named proof declaration.
-- The caller applies its own checks before publishing the result.
captureSourceMethodProof :: Id -> Id -> Id -> (SourceProofArgs, [Id]) -> TcM ()
captureSourceMethodProof dfun selector target (checked, leaves) = do
    env <- getGblEnv
    let ops = sourceMethodOps dfun selector target
    updTcRef (tcg_source_rule_types env) $ \state ->
      case sourceProofSignature state of
        Just owner ->
          state { sourceProofDefinitions =
                    extendNameEnv_C (++) (sourceProofDefinitions state) owner
                      ([ (idName v, first, finalCoreMapperOwner v, if v == selector then checked else [])
                       | (v, first) <- ops ] ++ [(idName v, Nothing, False, []) | v <- leaves]) }
        Nothing -> state

{- Note [Imported source method proofs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Readback can erase the selected method from a proof's signature. Record
its rule dependencies while checking the source signature. An ordinary
consumer may use the resulting proof without enabling the extension.
Check rules at that occurrence, after all declaration groups have finished.

The checking flag is the consumer's Opt_EnableRewriteRules. If rules are
off, the consumer still records the dependencies of an exported forwarder.
Re-exporting the imported binding preserves its original Name and metadata.
The defining module checked its own rules when it produced the proof.
For new rules, use the stable rule-head identity in RuleEnv. A head can
identify a private method worker with no interface declaration.

This check uses the existing first-type-constructor test and
checkedMapperRules whitelist. Use the same rule policy as for checked
source method calls. For a direct closed selector application, checked
constructor arguments can rule out a match. See Note [Closed rule argument
shapes] in GHC.Tc.Gen.Splice. Keep other uses of the same method in the
dependency list even if one use has checked arguments.

This metadata covers methods checked in complete top-level signatures.
It does not recover the provenance of proofs from earlier interfaces.
-}

-- | Collect method dependencies while checking a complete source signature.
withSourceProofSignature :: Maybe Name -> TcM a -> TcM a
withSourceProofSignature (Just owner) action
  | isExternalName owner = do
      enabled <- xoptM LangExt.ExperimentalDependentTypes
      if not enabled then action else do
        env <- getGblEnv
        let proof_ref = tcg_source_rule_types env
        before <- readTcRef proof_ref
        local <- newTcRef (before { sourceProofSignature = Just owner })
        result <- setGblEnv (env { tcg_source_rule_types = local }) action
        after <- readTcRef local
        writeTcRef proof_ref (after { sourceProofSignature = sourceProofSignature before })
        pure result
withSourceProofSignature _ action = action

-- | Carry the referenced synonym's rule dependencies into a named signature
-- or another synonym before kind checking can discard the reference.
recordSourceProofAliasUse :: Maybe Name -> TyCon -> TcM ()
recordSourceProofAliasUse owner tc
  | deps@(_ : _) <- synTyConSourceProofRules tc = do
      env <- getGblEnv
      updTcRef (tcg_source_rule_types env) $ \state ->
        case owner <|> sourceProofSignature state of
          Just name | isExternalName name ->
            state { sourceProofDefinitions =
                      extendNameEnv_C (++) (sourceProofDefinitions state) name deps }
          _ -> state
  | otherwise = pure ()

-- | Get the checked rule dependencies for a finished source declaration.
sourceProofRulesFor :: Name -> TcM [SourceProofRule]
sourceProofRulesFor name = do
    env <- getGblEnv
    defs <- sourceProofDefinitions <$> readTcRef (tcg_source_rule_types env)
    pure (stableSourceProofRules (fromMaybe [] (lookupNameEnv defs name)))

-- | Finish a family equation before collecting dependencies for another
-- equation under the same axiom name.
takeSourceProofRulesFor :: Name -> TcM [SourceProofRule]
takeSourceProofRulesFor name = do
    rules <- sourceProofRulesFor name
    unless (null rules) $ do
      env <- getGblEnv
      updTcRef (tcg_source_rule_types env) $ \state ->
        state { sourceProofDefinitions = delFromNameEnv (sourceProofDefinitions state) name }
    pure rules

-- | Verify imported proofs and committed synonym reductions in this consumer and retain the dependencies of
-- its exports, including forwards through a private local group.
checkAndInstallSourceProofs :: TcGblEnv -> TcM TcGblEnv
checkAndInstallSourceProofs env = do
    state <- readTcRef (tcg_source_rule_types env)
    let occurrences = sourceProofOccurrences state
        imported = mkNameEnv [(name, ops) | (name, _, ops) <- occurrences]
        -- A Wanted can be solved after checking the binding. Its source
        -- span still belongs to the checked RHS. Attach the observed
        -- dependency to the exact singleton signature binding. A type
        -- reference elsewhere in the module is not evidence of a use.
        rhs_proofs = mkNameEnv
          [ (idName owner, deps)
          | not (null occurrences)
          , L loc (XHsBindsLR (AbsBinds { abs_sig = True
                                       , abs_exports = [ABE { abe_poly = owner }] }))
              <- tcg_binds env
          , let deps = concat [ops | (_, site, ops) <- occurrences
                                   , site `isSubspanOf` locA loc]
          , not (null deps) ]
        definitions = plusNameEnv_C (++) rhs_proofs $
                      plusNameEnv_C (++) (sourceProofDefinitions state) imported
    rewriting <- goptM Opt_EnableRewriteRules
    when (rewriting && not (null occurrences)) $ do
      visible <- visibleSourceMethodRules env
      conflicts <- forM (reverse occurrences) $ \(_, loc, ops) -> do
        conflict <- listToMaybe <$> filterM (changed visible) ops
        forM_ conflict $ \(owner, _, _, _) ->
          setSrcSpan loc (addErr (TcRnRetainedQuantificationError (RetainedFinalCoreRulesEnabled owner)))
        pure conflict
      when (all isNothing conflicts) $ recordCommittedHomeRules env
    if isEmptyNameEnv definitions then pure env else do
      let -- DefUses also contains group-independent uses, such as RULES and
          -- export lists. They cannot make a different exported binding a proof.
          bindings = fromOL (tcg_dus env)
          exports = availsToNameSet (tcg_exports env)
          uses_of v = foldr add_uses (unitNameSet (idName v)) bindings
          add_uses (Just defs, uses) needed
            | defs `intersectsNameSet` needed = uses `unionNameSet` needed
          add_uses _ needed = needed
          proof_id v
            | not rewriting
            , isOpaquePragma (idInlinePragma v)
            , idName v `elemNameSet` sourceProofClosedResults state
            = Nothing
            | idName v `elemNameSet` exports
            , let deps = concatMap (\name -> fromMaybe [] (lookupNameEnv definitions name))
                                   (nameSetElemsStable (uses_of v))
            , not (null deps) =
                Just (setIdInfo v (setSourceProofRuleInfo (idInfo v) (stableSourceProofRules deps)))
            | otherwise = Nothing
          types = tcg_type_env env
      pure env { tcg_type_env = extendTypeEnvWithIds types (mapMaybe proof_id (typeEnvIds types)) }
  where
    changed visible (owner, arg, mapper, checked) = do
      let relevant = filter (not . localSourceRuleApart owner checked) (tcg_rules env)
      local <- sourceRuleOwnersAtTc arg relevant
      if owner `elemNameSet` local then pure True else
        if mapper then do
          -- Use the same whitelist as source formation. Only this mode
          -- requires the checked worker declaration.
          worker <- tcLookupIdMaybe owner
          pure (maybe True (\v -> not (finalCoreMapperOwner v && checkedMapperRules visible v)) worker)
        else
          -- A newly imported rule is in the rule environment. Its head
          -- can be found even when the defining Id is private.
          pure (hasSourceMethodRuleAtArgs visible arg checked (mkVanillaGlobal owner liftedTypeKind))

{- Note [Opaque source proof observations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A rules-off provider can use evidence to compute a closed observation
without exporting the evidence used by its implementation. A client's
rules cannot enter an OPAQUE body by inlining or specialisation. Its
written result must expose only built-in data with no evidence fields
or manifest reflexivity. The grammar admits Bool, Char, Int, String,
and lists of those types, as well as reflexive equalities on literal
Bool constructors. The complete signature cannot bind type variables or
include constraints. Check the renamed signature before a synonym or
family can reduce to one of those types. Other results keep
the dependencies. For example, a user-defined data constructor can
return the evidence used by its implementation as a field.
-}

-- | Preserve stable interface fingerprints when a rule head occurs twice.
stableSourceProofRules :: [SourceProofRule] -> [SourceProofRule]
stableSourceProofRules = nubBy (==) . sortBy (\(a, b, c, args) (d, e, f, other) ->
    stableNameCmp a d <> cmpArg b e <> compare c f <> compare (stableArgs args) (stableArgs other))
  where
    cmpArg Nothing Nothing = EQ
    cmpArg Nothing _ = LT
    cmpArg _ Nothing = GT
    cmpArg (Just a) (Just b) = stableNameCmp a b
    stableArgs = map (fmap nameStableString)

{- Note [Select the dictionary for a promoted method]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An occurrence of Prelude.foldr names a class selector. A list operand
fixes its instance head to Foldable []. A local Given can still supply
the dictionary, for example after a match on Dict (Foldable []).
Only the dictionary that selects the checked worker justifies its equations.

For a binary selector of type a -> a -> a, a retained argument fixes a.
Use the binder's type to look up the dictionary. Check the original
operands, including any written kind annotations, against the worker's table.

For a method of type (a -> b) -> h a -> h b, the kind of the receiver
fixes h. Check that the receiver and result use the class parameter
before using it to find an instance. The selected worker determines
what happens to each constructor.

First find the method field of the visible instance dictionary. Check
its specialized type and its equations. Then ask the ordinary
solver which dictionary the source call uses. GHC.Tc.Gen.HsType supplies
both lexical Givens and the constraints written in the signature. It
disables the constant dictionary shortcut for this check because the
shortcut can bypass a Given. Record the selector, dictionary and worker
so that a change to the provider invalidates the proof.
-}

-- | Read the class parameter shared by both operands and the result.
sourceBinarySelector :: Id -> Maybe Class
sourceBinarySelector selector = do
    cls <- isClassOpId_maybe selector
    [parameter] <- pure (classTyVars cls)
    let a = mkTyVarTy parameter
    (left, rest) <- caseArrow (classMethodInstTy selector [a])
    (right, result) <- caseArrow rest
    guard (all (eqType a) [left, right, result])
    pure cls

-- | Resolve a saturated homogeneous selector to its certified source worker.
-- See Note [Select the dictionary for a promoted method].
lookupSourceBinaryMethod :: Id -> Type -> TcM (Maybe (Id, Id, TyCon))
lookupSourceBinaryMethod selector domain
  | Just cls <- sourceBinarySelector selector
  = do
      selected <- lookupSourceGroundDFun cls domain
      case selected of
        Just dfun | Just name <- sourceMethodTarget_maybe dfun selector -> do
          worker <- tcLookupIdMaybe name
          pure $ do
            method <- worker
            guard (varType method `eqType` classMethodInstTy selector [domain])
            table <- sourceTableId_maybe method
            pure (dfun, method, table)
        _ -> pure Nothing
  | otherwise = pure Nothing

-- | Find the checked worker named by a method of a ground container instance.
-- See Note [Select the dictionary for a promoted method].
lookupSourceContainerMethod :: Id -> Type -> TcM (Maybe (Id, Id, TyCon))
lookupSourceContainerMethod selector domain
  | Just cls <- isClassOpId_maybe selector
  , let fold_method = listFoldSelector cls selector
  , fold_method || sourceMapSelector selector
  = do
      selected <- lookupSourceGroundDFun cls domain
      case selected of
        Just dfun -> do
          target <- case sourceMethodTarget_maybe dfun selector of
            Just term -> tcLookupIdMaybe term
            Nothing -> pure $ do
              Core.DFunUnfolding [] con fields <- pure (idUnfolding dfun)
              slots <- dictionarySlots cls domain con fields
              Core.Var method <- lookup selector slots
              pure method
          pure $ do
            method <- target
            guard (varType method `eqType` classMethodInstTy selector [domain])
            table <- sourceTableId_maybe method
            guard (not fold_method || isListFoldTable table)
            pure (dfun, method, table)
        _ -> pure Nothing
  | otherwise = pure Nothing

-- | Find the checked worker used by a mapping dictionary, including one with
-- erased parameters. See Note [Erased arguments in a checked source dictionary].
lookupSourceCheckedMapMethod :: Class -> Id -> Type -> TcM (Maybe (Id, Type, Id, TyCon))
lookupSourceCheckedMapMethod cls selector domain
  | isTyFamFree domain
  , Just (head_tc, _) <- tcSplitTyConApp_maybe domain
  , isGenerativeTyCon head_tc Nominal = do
      insts <- tcGetInstEnvs
      env <- getGblEnv
      case lookupUniqueInstEnv insts cls [domain] of
        Right (inst, args)
          | noFreeVarsOfType domain ||
            (not (isIncoherent inst) && all (apart inst) (classInstances insts cls))
          , let name = idName (instanceDFunId inst)
          , nameModule_maybe name /= Just (tcg_semantic_mod env) -> do
              candidate <- tcLookupIdMaybe name
              case candidate of
                Just dfun
                  | let predicate = mkClassPred cls [domain]
                  , Just applied <- erasedMethodType (varType dfun) args
                  , applied `eqType` predicate
                  , Just target <- sourceMethodTarget_maybe dfun selector -> do
                      worker <- tcLookupIdMaybe target
                      pure $ do
                        method <- worker
                        method_ty <- erasedMethodType (varType method) args
                        guard (method_ty `eqType` classMethodInstTy selector [domain])
                        table <- sourceTableId_maybe method
                        sig <- constructorCaseType (tyConKind table)
                        guard (isJust (checkedMapPrefix sig) && sourceCaseVisibleArity table == Just 2)
                        pure (dfun, predicate, method, table)
                _ -> pure Nothing
        _ -> pure Nothing
  | otherwise = pure Nothing
  where
    rough = RM_KnownTc (className cls) : roughMatchTcs [domain]
    apart chosen other = instanceDFunId other == instanceDFunId chosen ||
                         instanceCantMatch (is_tcs other) rough

-- | Instantiate a prefix of ordinary erased type parameters.
erasedMethodType :: Type -> [Type] -> Maybe Type
erasedMethodType ty args = do
    let (binders, _) = splitForAllForAllTyBinders ty
        prefix = take (length args) binders
    guard (equalLength prefix args)
    _ <- traverse ordinaryCaseBinder prefix
    pure (piResultTys ty args)

-- | Find a complete, imported dictionary with no instance context.
lookupSourceGroundDFun :: Class -> Type -> TcM (Maybe Id)
lookupSourceGroundDFun cls domain
  | noFreeVarsOfType domain, isTyFamFree domain = do
      insts <- tcGetInstEnvs
      env <- getGblEnv
      case lookupUniqueInstEnv insts cls [domain] of
        Right (inst, [])
          | let name = idName (instanceDFunId inst)
          , nameModule_maybe name /= Just (tcg_semantic_mod env) -> do
              candidate <- tcLookupIdMaybe name
              pure $ do
                dfun <- candidate
                guard (varType dfun `eqType` mkClassPred cls [domain])
                pure dfun
        _ -> pure Nothing
  | otherwise = pure Nothing

-- | The selector's list operand must determine the class parameter.
-- Inspect the unspecialized method type. A method that always takes a list
-- can have the same specialized type without selecting the list dictionary.
listFoldSelector :: Class -> Id -> Bool
listFoldSelector cls selector = isJust $ do
    [parameter] <- pure (classTyVars cls)
    let (_, body) = splitForAllForAllTyBinders
                      (classMethodInstTy selector [mkTyVarTy parameter])
    (_, after_step) <- caseArrow body
    (_, after_seed) <- caseArrow after_step
    (receiver, _) <- caseArrow after_seed
    (head_ty, _) <- splitAppTy_maybe receiver
    guard (getTyVar_maybe head_ty == Just parameter)

-- | The receiver and result of a mapping method use the class parameter.
sourceMapSelector :: Id -> Bool
sourceMapSelector selector = isJust $ do
    cls <- isClassOpId_maybe selector
    [parameter] <- pure (classTyVars cls)
    let (_, body) = splitForAllForAllTyBinders
                      (classMethodInstTy selector [mkTyVarTy parameter])
    (callback, rest) <- caseArrow body
    (input, output) <- caseArrow callback
    (receiver, result) <- caseArrow rest
    (recv_head, recv_arg) <- splitAppTy_maybe receiver
    (res_head, res_arg) <- splitAppTy_maybe result
    guard (getTyVar_maybe recv_head == Just parameter && getTyVar_maybe res_head == Just parameter &&
           eqType recv_arg input && eqType res_arg output)

-- | Pair the actual fields of a complete ground dictionary with its selectors.
dictionarySlots :: Class -> Type -> DataCon -> [Core.CoreExpr] -> Maybe [(Id, Core.CoreExpr)]
dictionarySlots cls domain con (Core.Type applied : fields)
  | applied `eqType` domain, con == tyConSingleDataCon (classTyCon cls)
  , let selectors = classAllSelIds cls
  , lengthIs fields (length selectors)
  = Just (zip selectors fields)
dictionarySlots _ _ _ _ = Nothing

{- Note [Erased arguments in a checked source dictionary]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A context-free dictionary for Functor (Either e) applies its method to e
before supplying the method's operands. Check that its final Core field
passes every erased parameter to the worker in order. Check the worker's
constructor rows and its specialized type.

Ordinary matching at a rigid receiver can leave instances that might
apply after substitution. It can discard incoherent candidates. At a
receiver with free variables, require every other visible instance to be
apart. The solver then checks the actual evidence at the occurrence.

The resulting family names the checked worker. An ordinary client can
use that family even when it introduces an overlapping instance. A new
selector application in that client still uses its own dictionary. Record
the selected dictionary, method and selector as rule dependencies.

Check the receiver and result at the same datatype and fixed parameters.
For example, fmap on These e a can inspect a constructor that contains
both an e and an a. The number of constructors does not determine the
equations. checkedConstructorCase reads each result from the worker and
checks coverage, field types and callback applications.

At the use site, a foreach binder names a runtime Id in the type
checker. Such an Id can supply the callback or a constructor field.
Treat it as an open operand when selecting the checked method.
-}

-- | Return the receiver parameters preserved by the checked mapping signature.
checkedMapPrefix :: ConstructorCaseType -> Maybe [Type]
checkedMapPrefix (ConstructorCaseType _ tc params [callback, _] 1 result)
  | Just (prefix, input) <- unsnoc params
  , not (null prefix)
  , Just (from, to) <- caseArrow callback
  , input `eqType` from, result `eqType` mkTyConApp tc (prefix ++ [to])
  = Just prefix
checkedMapPrefix _ = Nothing

-- | Closed types carried unchanged by a mapping method.
-- See Note [Closed parameters in a checked mapping instance].
fixedMapTypes :: ConstructorCaseType -> [Type]
fixedMapTypes = filter (caseKind []) . fromMaybe [] . checkedMapPrefix

{- Note [Closed parameters in a checked mapping instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For Functor (Row Int Bool), the worker takes a Row Int Bool a and returns
a Row Int Bool b. It can copy an Int field without using its constructor.
Keep the closed prefix types when checking the fields and publishing the
equations. Treat them as opaque at those occurrences. The checker for a
case on a field still requires the actual constructor to be available.
The same check applies when a result builds a constructor.

The kind telescope still quantifies the callback's input and output.
Record only types from the fixed prefix of the same datatype on both
sides of the mapping signature.
See Note [Erased arguments in a checked source dictionary].
-}

-- | Keep the checked workers until Tidy gives their binders external names.
keepCheckedMapWorkers :: UniqSupply -> TcGblEnv -> [Core.CoreBind] -> [Core.CoreBind]
keepCheckedMapWorkers supply env binds
  | isEmptyNameSet targets = binds
  | otherwise = map keep binds
  where
    targets = mkNameSet [idName owner | (_, fields) <- checkedMapWorkers supply env binds
                       , (_, (owner, _, _)) <- fields]
    exported v | idName v `elemNameSet` targets = setIdExported v
               | otherwise = v
    keep (Core.NonRec v rhs) = Core.NonRec (exported v) rhs
    keep (Core.Rec pairs) = Core.Rec [(exported v, rhs) | (v, rhs) <- pairs]

-- | Publish mandatory aliases and nominal equations for the same final workers.
publishCheckedMapMethods :: UniqSupply -> TcGblEnv -> [Core.CoreBind] -> Maybe (TcM ([Id], [TyCon]))
publishCheckedMapMethods supply env binds
  | null candidates = Nothing
  | otherwise = Just $ do
      let owners = nubBy (\(a, _, _) (b, _, _) -> a == b)
                     [entry | (_, fields) <- candidates, (_, entry) <- fields]
      tables <- forM owners $ \(owner, sig, rows) -> do
        table <- publishConstructorCase owner sig rows
        let worker = setIdDetails owner (SourceTableId (idName owner) table NominalSourceTable)
        pure (worker, table)
      let checked = mkNameEnv [(idName owner, owner) | (owner, _) <- tables]
          publish (dfun, fields) = addSourceMethods dfun
            [(selector, target) | (selector, (owner, _, _)) <- fields
            , Just target <- [lookupNameEnv checked (idName owner)]]
      pure (map fst tables ++ map publish candidates, map snd tables)
  where
    candidates = [(dfun, external) | (dfun, fields) <- checkedMapWorkers supply env binds
                 , let external = filter (\(_, (owner, _, _)) -> isExternalName (idName owner)) fields
                 , not (null external)]

-- | Read a complete DFun constructor and certify its actual mapping fields.
checkedMapWorkers :: UniqSupply -> TcGblEnv -> [Core.CoreBind]
                  -> [(Id, [(Id, (Id, ConstructorCaseType, [ConstructorCaseRow]))])]
checkedMapWorkers supply env binds = mapMaybe (uncurry selected) (zip dfuns (listSplitUniqSupply supply))
  where
    bindings = Core.flattenBinds binds
    dfuns = [binding | binding@(v, _) <- bindings, DFunId{} <- [idDetails v]]
    owners = mkNameEnv [(idName v, (v, rhs)) | (v, rhs) <- bindings]
    exported = mkNameSet (concatMap availNames (tcg_exports env))
    visible tc = all available (tyConName tc : map dataConName (tyConDataCons tc))
    available name
      | nameModule_maybe name == Just (tcg_semantic_mod env) = name `elemNameSet` exported
      | Just gre <- lookupGRE_Name (tcg_rdr_env env) name =
          any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
      | otherwise = False
    selected (dfun, rhs) dict_supply = do
      cls <- sourceMethodDFunClass (varType dfun)
      let (binders, body) = Core.collectBinders rhs
      guard (all isTyVar binders && exprType rhs `eqType` varType dfun)
      (_, [domain]) <- getClassPredTys_maybe (exprType body)
      (Core.Var constructor, fields) <- pure (Core.collectArgs body)
      con <- isDataConWorkId_maybe constructor
      slots <- dictionarySlots cls domain con fields
      let methods = catMaybes (zipWith (checkSlot domain binders) slots (listSplitUniqSupply dict_supply))
      (dfun, methods) <$ guard (not (null methods))

    checkSlot domain binders (selector, field) slot_supply = do
      guard (sourceMapSelector selector)
      (Core.Var worker, args) <- pure (Core.collectArgs field)
      types <- traverse (\case Core.Type ty -> Just ty; _ -> Nothing) args
      guard (eqTypes types (mkTyVarTys binders))
      (owner, rhs) <- lookupNameEnv owners (idName worker)
      guard (not (idName owner `elemNameSet` exported) &&
             isNothing (sourceTableId_maybe owner) &&
             exprType field `eqType` classMethodInstTy selector [domain])
      let body = expandMappingDefaults slot_supply (stripTicksE isSourceBreakpoint rhs)
      (sig, rows) <- checkedConstructorCase visible (const Nothing) owner body
      guard (isJust (checkedMapPrefix sig))
      pure (selector, (owner, sig, rows))

-- | Expose the constructors tested by grouped mapping alternatives.
-- See Note [Default alternatives in mapping equations].
expandMappingDefaults :: UniqSupply -> Core.CoreExpr -> Core.CoreExpr
expandMappingDefaults supply (Core.Lam v rhs) = Core.Lam v (expandMappingDefaults supply rhs)
expandMappingDefaults supply (Core.Let bind@(Core.NonRec _ (Core.Var _)) rhs) =
    Core.Let bind (expandMappingDefaults supply rhs)
expandMappingDefaults supply (Core.Case scrut binder result alts) =
    Core.Case scrut binder result (fromMaybe checked (expand checked))
  where
    (pat_supply, rhs_supply) = splitUniqSupply supply
    checked = zipWith descend (listSplitUniqSupply rhs_supply) alts
    descend us (Core.Alt con fields body) = Core.Alt con fields (expandMappingDefaults us body)
    expand (Core.Alt Core.DEFAULT [] body : rest) = do
      (tc, args) <- caseDatatype (varType binder)
      let explicit = [dc | Core.Alt (Core.DataAlt dc) _ _ <- rest]
          missing = filter (`notElem` explicit) (tyConDataCons tc)
      guard (equalLength explicit rest && not (null missing))
      let alternative us dc = do
            _ <- openDataConFieldTypes dc args
            let (ex, fields) = dataConRepInstPat (uniqsFromSupply us) (idMult binder) dc args
            guard (null ex)
            pure (Core.Alt (Core.DataAlt dc) fields body)
      inferred <- sequence (zipWith alternative (listSplitUniqSupply pat_supply) missing)
      pure (mergeAlts rest inferred)
    expand _ = Nothing
expandMappingDefaults _ expr = expr

{- Note [Default alternatives in mapping equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC can merge identical mapping branches into a DEFAULT alternative. For
example, both Empty and Flagged _ _ can map to Empty. A family equation with
a variable for the entire row would also reduce at an unknown row. The Core
case must inspect the row first.

For equation checking, expose a DataAlt for each missing lazy constructor.
Use dataConRepInstPat to bind its fields at their instantiated types. The
DEFAULT body cannot refer to those fresh binders. The ordinary row checker
still checks the visibility, constructor patterns, and result of each branch.
GHCi puts debugger breakpoints on constructor results and callback calls.
Skip these ticks when reading the equations. A breakpoint does not force
the variables that it captures. Keep coverage ticks and strict cases.
The executable worker keeps its original case and ticks. See refineDefaultAlt in
GHC.Core.Utils for the corresponding single-constructor transformation.
-}

addSourceMethods :: Id -> [(Id, Id)] -> Id
addSourceMethods dfun methods =
  case idDetails (setSourceMethodIds dfun methods) of
    DFunId terminating (Just checked) ->
      let selected = map fst (sdf_methods checked)
          aliases = sdf_methods checked ++ filter ((`notElem` selected) . fst) (sourceMethodAliases dfun)
      in setIdDetails dfun (DFunId terminating (Just checked { sdf_methods = aliases }))
    _ -> dfun

{- Note [Mandatory list fold method evidence]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Record just the selector and implementation Names from the final dictionary
constructor. The actual field must be a checked fold at the specialized
method type. Read a local field's current binder after final source-table
validation. A Core occurrence can still carry the pre-validation IdDetails.
Keep older mandatory evidence for other methods.

The ordinary DFun unfolding can disappear when the producer omits interface
pragmas or the reader ignores them. The SourceDFun pair remains available.
It records only the exact method choice and does not supply any equations
by itself. The client still checks its scoped dictionary and visible rules.
-}

-- | Publish exact list fold aliases from the final dictionary bindings.
-- See Note [Mandatory list fold method evidence].
publishListFoldMethods :: Module -> [Core.CoreBind] -> [Core.CoreBind]
publishListFoldMethods mod binds = map publishBind binds
  where
    locals = mkNameEnv [(idName v, v) | (v, _) <- Core.flattenBinds binds]
    publishBind (Core.NonRec v rhs) = Core.NonRec (publish v rhs) rhs
    publishBind (Core.Rec pairs) = Core.Rec [(publish v rhs, rhs) | (v, rhs) <- pairs]
    publish v rhs = fromMaybe v $ do
      DFunId{} <- pure (idDetails v)
      (cls, [domain]) <- getClassPredTys_maybe (varType v)
      guard (domain `eqType` mkTyConTy listTyCon)
      (Core.Var worker, fields) <- pure (Core.collectArgs rhs)
      con <- isDataConWorkId_maybe worker
      slots <- dictionarySlots cls domain con fields
      let methods =
            [ (selector, target)
            | (selector, Core.Var field) <- slots, selector `elem` classMethods cls
            , listFoldSelector cls selector
            , Just target <- [current field]
            , varType target `eqType` classMethodInstTy selector [domain]
            , Just table <- [sourceTableId_maybe target], isListFoldTable table ]
      guard (not (null methods))
      pure (addSourceMethods v methods)

    current field
      | nameModule_maybe (idName field) == Just mod = lookupNameEnv locals (idName field)
      | otherwise = Just field

-- | The dictionary fixes the nominal head of the selector's first argument.
-- The worker and the dictionary itself can have any active rule.
sourceMethodOps :: Id -> Id -> Id -> [(Id, Maybe Name)]
sourceMethodOps dfun selector target =
    [(selector, first), (dfun, Nothing), (target, Nothing)]
  where
    first = do
      (_, [domain]) <- getClassPredTys_maybe (snd (splitForAllForAllTyBinders (varType dfun)))
      (tc, _) <- tcSplitTyConApp_maybe domain
      guard (isGenerativeTyCon tc Nominal)
      return (tyConName tc)

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
    ops <- sourceLiteralUses dfun selector
    maybe (failWithTc (TcRnRetainedQuantificationError (RetainedUncheckedSourceCall (idName selector))))
          recordSourceUses ops

-- | Read the dependencies of a certified literal without recording a use.
sourceLiteralUses :: Id -> Id -> TcM (Maybe [(Id, Maybe Name)])
sourceLiteralUses dfun selector = case sourceLiteralCons_maybe dfun selector of
  Nothing -> pure Nothing
  Just (zero, _) -> do
    let num_module = mkGhcInternalModule (fsLit "GHC.Internal.Num")
        lookup_id mod occ = lookupOrig mod occ >>= tcLookupId
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
                   , nameModule_maybe (idName d) == Just mod -> Just d
          _ -> Nothing
    pure $ do
      ord_d <- integer_dict cmp gHC_INTERNAL_NUM_INTEGER
      eq_d  <- integer_dict eq  gHC_INTERNAL_NUM_INTEGER
      num_d <- integer_dict sub num_module
      pure $ (selector, Just (tyConName (dataConTyCon zero))) :
             [(op, Just integerTyConName) | op <- [cmp, eq, sub, selector]] ++
             [(v, Nothing) | v <- [dfun, ord_d, eq_d, num_d, otherwise_id] ++ workers]

-- | The checked dependencies of one optional alias, before proof-use selection.
data OptionalSourceUses = OptionalSourceUses [Id] [(Id, Maybe Name)] [Name]

-- | Read and validate optional dependencies without recording source uses.
optionalSourceUses :: [Id] -> [(Id, Id, Maybe Id)] -> TcM (Maybe OptionalSourceUses)
optionalSourceUses heads methods = do
    plan <- optionalSourceUsePlan heads methods
    case plan of
      Nothing -> pure Nothing
      Just (owners, ops, tables) -> do
        changed <- changedSourceUse ops
        pure $ case changed of
          Nothing -> Just (OptionalSourceUses owners ops tables)
          Just _ -> Nothing

-- | Register dependencies whose proof use is decided by the committed solver.
-- See Note [Optional source dependencies].
registerOptionalSourceUses :: TcEvBinds -> CoVar -> OptionalSourceUses -> TcM ()
registerOptionalSourceUses evidence proof (OptionalSourceUses owners ops tables) = do
    env <- getGblEnv
    updTcRef (tcg_optional_source_uses env) ((evidence, proof, owners, ops, tables) :)

-- | Resolve the exact source owners, transitive callees, and literal workers.
optionalSourceUsePlan :: [Id] -> [(Id, Id, Maybe Id)]
                      -> TcM (Maybe ([Id], [(Id, Maybe Name)], [Name]))
optionalSourceUsePlan heads methods = do
    env <- getGblEnv
    let targets = [target | (_, _, Just target) <- methods]
        roots = nubBy (\a b -> idName a == idName b) (heads ++ targets)
        source fun = case sourceTableOccurrence env fun of
          Just actual | Just table <- sourceTableId_maybe actual -> do
            deps <- sourceTableDependencies table
            pure ((\callees -> (actual : callees, tyConName table)) <$> deps)
          _ -> pure Nothing
        method (dfun, selector, Just target)
          | sourceMethodTarget_maybe dfun selector == Just (idName target)
          = pure (Just (sourceMethodOps dfun selector target))
          | otherwise = pure Nothing
        method (dfun, selector, Nothing) = sourceLiteralUses dfun selector
    sources <- traverse source roots
    method_ops <- traverse method methods
    pure $ do
      checked <- sequence sources
      checked_methods <- sequence method_ops
      let owners = nubBy (\a b -> idName a == idName b) (concatMap fst checked)
      pure (owners, [(v, Nothing) | v <- owners] ++ concat checked_methods, map snd checked)

-- | Find a currently visible rewrite that can change one selected operation.
changedSourceUse :: [(Id, Maybe Name)] -> TcM (Maybe (Id, Maybe Name))
changedSourceUse ops = do
    env <- getGblEnv
    visible <- visibleSourceMethodRules env
    rewriting <- goptM Opt_EnableRewriteRules
    let check [] = pure Nothing
        check (op@(v, arg) : rest)
          | finalCoreMapperOwner v = do
              let local = sourceRuleOwners (tcg_rules env)
              if rewriting && (idName v `elemNameSet` local || not (checkedMapperRules visible v))
                then pure (Just op) else check rest
          | otherwise = do
              local <- sourceRuleOwnersAtTc arg (tcg_rules env)
              if idName v `elemNameSet` local || hasSourceMethodRuleAt visible arg v
                then pure (Just op)
                else check rest
    check ops

-- | Publish only dependencies selected by final evidence, and retain pending ones.
commitOptionalSourceUses :: TcGblEnv -> TcM ()
commitOptionalSourceUses env = do
    pending <- readTcRef (tcg_optional_source_uses env)
    remaining <- fmap catMaybes $ forM pending $ \entry@(evidence, proof, owners, ops, tables) -> do
      needed <- case evidence of
        TcEvBinds var -> (proof `elemVarSet`) . ebs_needs <$> getTcEvBindsState var
        EvBinds{} -> pure False
      if needed then do
        publishSourceUses ops
        updTcRef (tcg_source_table_uses env) (`extendNameSetList` tables)
        forM_ owners $ \owner ->
          when (nameModule_maybe (idName owner) == Just (tcg_semantic_mod env)) $
            keepAlive (idName owner)
        pure Nothing
      else pure (Just entry)
    updTcRef (tcg_optional_source_uses env) (const remaining)

{- Note [Optional source dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A lazy local alias can use checked source equations only when its body
needs the alias proof. Read the certificates and visible RULES before
checking that body. Cache the exact source owners, transitive callees,
table Names, selected methods, and literal workers without recording use.

The solver records proof use in the original evidence variable's ebs_needs.
Speculative evidence variables do not commit that marker. Before module
finalization or compile-time execution, publish only marked dependencies.
Keep unmarked entries because a later solve can still use their proofs.
Then run the ordinary late-rule check. It reports a local rule at the rule
declaration, including a rule introduced by a splice. An ordinary alias
whose proof is unused records no source dependency or rule restriction.
-}

-- | Record rule heads and their ABI dependencies after checking the visible rules.
recordSourceUses :: [(Id, Maybe Name)] -> TcM ()
recordSourceUses ops = do
    changed <- changedSourceUse ops
    case changed of
      Just (v, _) -> failWithTc (TcRnRetainedQuantificationError
        (if finalCoreMapperOwner v
           then RetainedFinalCoreRulesEnabled (idName v)
           else RetainedSourceRule (idName v)))
      Nothing -> publishSourceUses ops

-- | Use the exact dependencies visited by the open evaluator.
-- See Note [Identity certificates and open unfoldings].
recordUnfoldedSourceUses :: [(Id, Maybe Name)] -> TcM ()
recordUnfoldedSourceUses = recordSourceUses . map ordinary_use
  where
    ordinary_use (v, arg)
      | isFinalCoreIdentity v = (setIdDetails v VanillaId, arg)
      | otherwise = (v, arg)

-- | Record checked operations without changing the location of late-rule errors.
publishSourceUses :: [(Id, Maybe Name)] -> TcM ()
publishSourceUses ops = do
    env <- getGblEnv
    let add acc (v, arg) = extendNameEnv_C combine acc (idName v) (maybe emptyNameSet unitNameSet arg)
        combine a b | isEmptyNameSet a || isEmptyNameSet b = emptyNameSet
                    | otherwise = unionNameSet a b
    updTcRef (tcg_source_table_uses env) (`extendNameSetList` map (idName . fst) ops)
    updTcRef (tcg_source_rule_types env) $ \state ->
      state { sourceRuleTypes = foldl' add (sourceRuleTypes state) ops }

-- | Check rules declared or imported after an earlier retained application.
-- Report a local rule at its declaration, including one added by a splice.
checkSourceMethodUses :: TcGblEnv -> TcM ()
checkSourceMethodUses env = do
    checkSourceMethodUsesWithPending env
    updTcRef (tcg_optional_source_uses env) (const [])

-- | Check pending aliases before execution without discarding later solver work.
checkOptionalSourceUses :: TcGblEnv -> TcM ()
checkOptionalSourceUses env = do
    pending <- readTcRef (tcg_optional_source_uses env)
    unless (null pending) (checkNoErrs (checkSourceMethodUsesWithPending env))

-- | Publish committed optional dependencies and check visible rule conflicts.
checkSourceMethodUsesWithPending :: TcGblEnv -> TcM ()
checkSourceMethodUsesWithPending env = do
    commitOptionalSourceUses env
    uses <- filterNameSet (isVarOcc . nameOccName) <$> readTcRef (tcg_source_table_uses env)
    unfolded <- readTcRef (tcg_evaluated_term_uses env)
    rewriting <- goptM Opt_EnableRewriteRules
    let uses_final_core v = finalCoreMapperOwner v
          && not (isFinalCoreIdentity v && idName v `elemNameSet` unfolded)
    checked_uses <- if rewriting then pure uses else
      mkNameSet <$> filterM (fmap (maybe True (not . uses_final_core)) . tcLookupIdMaybe)
        (nameSetElemsStable uses)
    unless (isEmptyNameSet checked_uses) $ do
      types <- sourceRuleTypes <$> readTcRef (tcg_source_rule_types env)
      let arguments name | Just ts <- lookupNameEnv types name, not (isEmptyNameSet ts)
                         = map Just (nameSetElemsStable ts)
                         | otherwise = [Nothing]
          affected rule name = anyM (\arg -> elemNameSet name <$> sourceRuleOwnersAtTc arg [rule]) (arguments name)
          report name = addErr (TcRnRetainedQuantificationError (RetainedSourceRule name))
      local <- forM (tcg_rules env) $ \rule -> do
        let candidates = checked_uses `intersectNameSet` sourceRuleOwners [rule]
        names <- filterM (affected rule) (nameSetElemsStable candidates)
        pure (rule, mkNameSet names)
      let imported = checked_uses `minusNameSet` unionNameSets (map snd local)
      forM_ local $ \(rule, names) ->
        setSrcSpanA (getLoc rule) (mapM_ report (nameSetElemsStable names))
      unless (isEmptyNameSet imported) $ do
        visible <- visibleSourceMethodRules env
        uses_checked_rules <- forM (nameSetElemsStable imported) $ \name -> do
          v <- tcLookupId name
          let changed | uses_final_core v = not (checkedMapperRules visible v)
                      | otherwise = any (\arg -> hasSourceMethodRuleAt visible arg v) (arguments name)
          when changed (report name)
          pure (uses_final_core v && not changed)
        when (or uses_checked_rules) $ recordCommittedHomeRules env

-- | Retain the home rule environment of an accepted proof.
-- A private rule can change the full interface hash without changing its ABI.
recordCommittedHomeRules :: TcGblEnv -> TcM ()
recordCommittedHomeRules env = do
    let here = tcg_mod env
    unless (isInteractiveModule here || isHsBootOrSig (tcg_src env)) $ do
      hsc <- getTopEnv
      deps <- liftIO $ hugRuleInterfacesBelow hsc (homeUnitId (hsc_home_unit hsc))
                 (GWIB (moduleName here) NotBoot)
      recordHomeRuleDependencies deps

-- | Find source functions that a user rule can rewrite.
sourceRuleOwners :: [LRuleDecl GhcTc] -> NameSet
sourceRuleOwners = sourceRuleOwnersAt Nothing

-- | Exclude a local rule only when its first applied type constructor is known
-- and differs from the actual one. A polymorphic rule can match any argument.
sourceRuleOwnersAt :: Maybe Name -> [LRuleDecl GhcTc] -> NameSet
sourceRuleOwnersAt arg rules = runIdentity (sourceRuleOwnersWith Identity arg rules)

-- | Before final zonking, the inferred type argument of a local rule can
-- still refer to a filled metavariable. See Note [Source family evidence uses].
sourceRuleOwnersAtTc :: Maybe Name -> [LRuleDecl GhcTc] -> TcM NameSet
sourceRuleOwnersAtTc = sourceRuleOwnersWith (liftZonkM . zonkTcType)

-- | Select active rule heads, using the supplied view of their first type argument.
sourceRuleOwnersWith :: Monad m => (Type -> m Type) -> Maybe Name -> [LRuleDecl GhcTc] -> m NameSet
sourceRuleOwnersWith zonk arg rules = unionNameSets <$> sequence
    [ case headOf (unLoc lhs) of
        Just (owner, Just ty) | Just expected <- arg -> do
          actual <- zonk ty
          pure $ case tcSplitTyConApp_maybe actual of
            Just (tc, _) | isGenerativeTyCon tc Nominal, tyConName tc /= expected -> emptyNameSet
            _ -> unitNameSet owner
        Just (owner, _) -> pure (unitNameSet owner)
        Nothing -> pure lhs_fvs
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

An ordinary function between two enumerations can supply equations for an
open callback. A closed call to the same function can already execute its
object code. Mark the certificate ExecutableSourceTable so that a direct
closed call still uses the executable when it is available. An interface
with this mark needs a new file version. An older compiler can read the
original nominal certificates but cannot choose how to use the new ones.

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

-- | Publish checked case and pair equations, with their private field families.
-- See Note [Checked Char cases] and Note [Checked pair cases].
sourceCaseTables :: LHsBinds GhcTc -> TcM ([(Id, TyCon)], [TyCon])
sourceCaseTables binds = do
  env <- getGblEnv
  char_tables <- xoptM LangExt.ExperimentalDependentTypes
  dflags <- getDynFlags
  rdr_env <- getGlobalRdrEnv
  let exported = mkNameSet (concatMap availNames (tcg_exports env))
      ruled = sourceRuleOwners (tcg_rules env)
      enum_enabled = not char_tables
      imported = ordinaryImport rdr_env
      public = publicCaseType env exported imported
      eligible_case = eligibleCase env exported enum_enabled imported
      generates_code = backendGeneratesCode (backend dflags)
      list_case_position owner
        | generates_code
        , let ty = varType owner
        , Just (ConstructorCaseType _ tc _ _ position _) <-
            constructorCaseType ty <|> monomorphicGuardedListCaseType ty
        , tc == listTyCon = Just position
        | otherwise = Nothing
      indexed_candidate owner
        | generates_code
        , Just (IndexedProjection _ _ _ con _ _ _) <- indexedProjectionType (varType owner)
        = publicCaseType env exported imported (dataConTyCon con)
        | otherwise = False
      final_candidate owner = isJust (list_case_position owner) || indexed_candidate owner
      binary_list_candidate owner = list_case_position owner == Just 0
      fold_candidate owner = generates_code && isJust (listFoldType (varType owner))
      zip_candidate owner = generates_code && isJust (listZipWithType (varType owner))
      reverse_wrapper_candidate owner =
        generates_code && isJust (listReverseWrapperType (varType owner))
      identity_candidate owner = generates_code && isIdentitySourceType (varType owner)
      composition_candidate owner = generates_code && isJust (compositionSourceType (varType owner))
      is_exported owner = idName owner `elemNameSet` exported
      eligible owner =
        is_exported owner &&
        (final_candidate owner || not (idName owner `elemNameSet` ruled)) &&
        ((char_tables && varType owner `eqType` charArrow) ||
         not (isNothing (pairCaseType (varType owner))) ||
         not (isNothing (pairBuilderType (varType owner))) ||
         eligible_case owner)
      owners = [ (bind, owner) | bind <- binds
               , owner <- collectHsBindBinders CollNoDictBinders (unLoc bind) ]
      candidates = [(bind, owner) | (bind, owner) <- owners
                    , eligible owner ||
                      (is_exported owner && (identity_candidate owner ||
                       composition_candidate owner || final_candidate owner ||
                       reverse_wrapper_candidate owner || fold_candidate owner ||
                       zip_candidate owner))]
  if isHsBootOrSig (tcg_src env) || null candidates
    then pure ([], [])
    else do
      visible <- visibleSourceMethodRules env
      let changed owner = idName owner `elemNameSet` ruled || hasSourceMethodRule visible owner
          prepare = simpleOptExpr (initSimpleOpts dflags)
      prepared <- fmap catMaybes $ forM candidates $ \(bind, owner) -> do
        let ruled_owner = changed owner
            check_final = isJust (list_case_position owner)
        if ruled_owner && not (final_candidate owner || identity_candidate owner || fold_candidate owner ||
                              zip_candidate owner || reverse_wrapper_candidate owner) then pure Nothing else do
          rhs <- bindingCore bind owner
          pure $ do
            body <- rhs
            let guarded = if check_final
                          then (\(signature, branches) -> (signature, GuardedListBranches branches))
                                 <$> directGuardedList prepare owner body
                               <|> (,GuardedPairBranches) <$> directGuardedListPair prepare [] owner body
                          else Nothing
            pure (owner, body, ruled_owner, guarded)
      let root_cores = [(owner, body) | (owner, body, False, Nothing) <- prepared
                                     , eligible owner]
      final <- forM prepared $ \(owner, body, ruled_owner, guarded) ->
        case guarded of
          Just (signature, branches) -> do
            tables <- publishGuardedList owner signature branches
            case tables of
              Just (table, guard_table) -> pure (Just (finalOwner owner table, table), [guard_table])
              Nothing -> pure (Nothing, [])
          Nothing | Just (signature, rows) <-
                      (guard ruled_owner *> directListMapper owner body) <|>
                      (guard (binary_list_candidate owner) *>
                        (directListAppend owner body <|> directListReverseWorker owner (prepare body) <|>
                         checkedTwoListCase public owner (prepare body))) <|>
                      (guard (fold_candidate owner) *> directListFold owner (prepare body)) <|>
                      (guard (zip_candidate owner) *> checkedListZip public owner (prepare body)) -> do
            table <- publishConstructorCase owner signature rows
            pure (Just (finalOwner owner table, table), [])
          Nothing | indexed_candidate owner
                  , Just (projection, recursion) <- checkedIndexedProjection prepare owner body -> do
            checked <- publishIndexedProjection owner projection recursion
            pure (fmap (\table -> (finalOwner owner table, table)) checked, [])
          Nothing | composition_candidate owner
                  , Just (telescope, domains, result, row) <- checkedCompositionSource owner body -> do
            table <- publishConstructorRows owner telescope domains result [row]
            pure (Just (finalOwner owner table, table), [])
          _ -> pure (Nothing, [])
      let reverse_workers = mkNameEnv
            [(idName worker, table) | (Just (worker, table), _) <- final,
                                      isReverseWorkerTable table]
      reverse_wrappers <- forM
        [ (owner, binders, operand, element, table)
        | (owner, body, _, _) <- prepared
        , reverse_wrapper_candidate owner
        , Just (binders, operand, element, callee) <- [directListReverseWrapper owner (prepare body)]
        , Just table <- [lookupNameEnv reverse_workers (idName callee)]
        ] $ \(owner, binders, operand, element, worker) -> do
          let domain = varType operand
              row = ConstructorCaseRow [CaseField operand]
                      (CaseCall worker [element]
                         [CaseField operand, CaseConstructor nilDataCon [element] []])
          table <- publishConstructorRows owner binders [domain] domain [row]
          pure (finalOwner owner table, table)
      let localCall v = isId v && nameModule_maybe (idName v) == Just (tcg_semantic_mod env)
          references = map idName . exprSomeFreeVarsList localCall
          reachable = concatMap (references . snd)
                        (filter (eligible_case . fst) root_cores)
          local_owners = mkNameEnv [(idName owner, (bind, owner)) | (bind, owner) <- owners]
          helperCore name
            | Just (bind, owner) <- lookupNameEnv local_owners name
            , isExternalName name
            , not (eligible owner)
            , not (name `elemNameSet` ruled)
            , not (hasSourceMethodRule visible owner)
            , Just _ <- constructorHelperType (varType owner)
            = fmap (owner,) <$> bindingCore bind owner
            | otherwise = pure Nothing
      helper_cores <- collectHelpers helperCore references emptyNameSet reachable
      helpers <- foldM (buildHelper env imported public) []
                   (stronglyConnCompFromEdgedVerticesUniq helper_cores)
      certified <- forM root_cores $ \(owner, rhs) ->
        certifyBinding env exported enum_enabled rdr_env helpers owner rhs
      let (root_entries, fields) = unzip certified
          roots = catMaybes root_entries
          helper_tables = mkNameEnv [(tyConName table, table) | (_, table) <- helpers]
          used = helperClosure helper_tables emptyNameSet (calleeNames roots)
          referenced (_, table) = tyConName table `elemNameSet` used
          used_helpers = filter referenced helpers
          helper_owners = mkNameSet (map (idName . fst) used_helpers)
      identities <- forM [(owner, projection)
                         | (owner, body, _, _) <- prepared
                         , identity_candidate owner
                         , not (idName owner `elemNameSet` helper_owners)
                         , Just projection <- [checkedIdentitySource owner body]] $
        \(owner, (telescope, domains, result, row)) -> do
          table <- publishConstructorRows owner telescope domains result [row]
          pure (finalOwner owner table, table)
      let (final_tables, guard_tables) = unzip final
      pure (catMaybes final_tables ++ reverse_wrappers ++ identities ++ roots ++ used_helpers,
            concat guard_tables ++ concat fields)
  where
    charArrow = mkVisFunTyManyU charTy charTy
    finalOwner owner table = setIdDetails owner
      (SourceTableId (idName owner) table FinalCoreSourceTable)

    collectHelpers _ _ _ [] = pure []
    collectHelpers helperCore references seen (name : rest)
      | name `elemNameSet` seen = collectHelpers helperCore references seen rest
      | otherwise = do
          let next = collectHelpers helperCore references (extendNameSet seen name)
          core <- helperCore name
          case core of
            Just (owner, rhs) -> do
              let direct = references rhs
              others <- next (direct ++ rest)
              pure (DigraphNode (owner, rhs) name direct : others)
            Nothing -> next rest

    buildHelper env imported public done (AcyclicSCC (owner, rhs))
      | Just (telescope, domains, result, row) <- checkedConstructorHelper public helper owner rhs
      = publish (publishConstructorRows owner telescope domains result [row])
      | Just (signature, rows) <- checkedConstructorCase public helper owner rhs
      , not (isBinaryListCase signature)
      = publish (publishConstructorCase owner signature rows)
      where
        helper = lookupHelper env imported done
        publish make = do
          table <- make
          pure ((owner, table) : done)
    buildHelper _ _ _ done _ = pure done

    calleeNames = concatMap (callees . snd)
    callees = map tyConName . maybe [] id . sourceTableCallees

    helperClosure _ seen [] = seen
    helperClosure tables seen (name : rest)
      | name `elemNameSet` seen = helperClosure tables seen rest
      | Just table <- lookupNameEnv tables name =
          let children = callees table
          in helperClosure tables (extendNameSet seen name) (children ++ rest)
      | otherwise = helperClosure tables seen rest

    eligibleCase env exported enum_enabled imported owner
      | Just kinds <- monomorphicEnumCaseKinds (varType owner)
      = enum_enabled && all enumVisible kinds
      | Just signature@(ConstructorCaseType _ tc _ domains position _) <- constructorCaseType (varType owner)
      , not (isBinaryListCase signature)
      = (position == 1 && (tc == listTyCon || publicCaseType env exported imported tc)) ||
        (nameModule_maybe (tyConName tc) == Just (tcg_semantic_mod env) &&
         publicCaseType env exported (const False) tc) ||
        (maybe False (\(_, domain, _) -> isRetainedCaseRoot domain tc)
                     (splitCaseOperand position domains) &&
         publicCaseType env exported imported tc)
      | otherwise = False
      where
        enumVisible = maybe False (publicCaseType env exported imported . fst) . caseDatatype

    publicCaseType env exported imported tc =
      all available (tyConName tc : map dataConName (tyConDataCons tc))
      where
        available name
          | nameModule_maybe name == Just (tcg_semantic_mod env)
          = name `elemNameSet` exported
          | otherwise = imported name

    ordinaryImport rdr_env name = case lookupGRE_Name rdr_env name of
      Just gre -> any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
      Nothing -> False

    bindingCore bind owner = do
      (messages, bindings) <- initDsTc (dsTopLHsBinds [bind])
      pure $ case bindings of
        Just core | not (errorsFound messages)
          , [rhs] <- [expr | (ident, expr) <- fromOL core, idName ident == idName owner] ->
            Just rhs
        _ -> Nothing

    certifyBinding env exported enum_enabled rdr_env helpers owner rhs
      | Just build <- checkedPairBuilder owner rhs = build
      | otherwise = (, []) <$>
          case rhs of
            Core.Var target | varType owner `eqType` charArrow -> forwarding env owner target
            _ | Just projection <- checkedPairCase owner rhs ->
                  Just . (owner,) <$> publishPairCase owner projection
              | let imported = ordinaryImport rdr_env
                    enum_arity = monomorphicEnumCaseArity (varType owner)
              , eligibleCase env exported enum_enabled imported owner
              , Just (signature, rows) <- checkedConstructorCase
                  (publicCaseType env exported imported)
                  (lookupHelper env imported helpers) owner rhs
              , enum_arity /= Just 2 || all enum_binary_row rows -> do
                  table <- publishConstructorCase owner signature rows
                  let certified
                        | isJust enum_arity = setIdDetails owner
                            (SourceTableId (idName owner) table ExecutableSourceTable)
                        | otherwise = owner
                  pure (Just (certified, table))
              | Right certificate <- certify owner rhs
              , certOwner certificate == idName owner ->
                  publish owner (certRows certificate) id
              | otherwise -> pure Nothing

    -- A binary enumeration certificate inspects the first argument.
    -- Each row can return the second argument without inspecting it.
    enum_binary_row (ConstructorCaseRow [CaseConstructor{}, CaseField{}] result) =
      case result of
        CaseField{} -> True
        CaseConstructor _ _ [] -> True
        _ -> False
    enum_binary_row _ = False

    lookupHelper env imported helpers actual
      | nameModule_maybe (idName actual) == Just (tcg_semantic_mod env) =
          lookup actual helpers
      | imported (idName actual) = sourceTableId_maybe actual
      | otherwise = Nothing

    forwarding env owner target
      | Just target_mod <- nameModule_maybe (idName target)
      , target_mod /= tcg_semantic_mod env
      , not (isHoleModule target_mod)
      , (installed, Nothing) <- getModuleInstantiation target_mod
      , Just IsBoot /= (gwib_isBoot <$> lookupInstalledModuleEnv
                                      (imp_boot_mods (tcg_imports env)) installed)
      , varType target `eqType` charArrow
      , Just table <- sourceTableId_maybe target
      , [arg] <- tyConTyVars table
      , tyVarKind arg `eqType` charTy
      , tyConResKind table `eqType` charTy
      = do
        dependencies <- sourceTableDependencies table
        case dependencies of
          Just callees -> do
            let uses = [(v, Nothing) | v <- target : callees]
            changed <- changedSourceUse uses
            case changed of
              Nothing -> do
                result <- publish owner [] (\arg_ty -> mkTyConApp table [arg_ty])
                publishSourceUses uses
                pure result
              Just _ -> pure Nothing
          Nothing -> pure Nothing
      | otherwise = pure Nothing

    publish owner rows fallback = do
      let loc = nameSrcSpan (idName owner)
          literal (input, result) _ =
            mkCoAxBranch [] [] [] [mkCharLitTy input] (mkCharLitTy result) [] loc
      var <- flip mkTyVar charTy <$> newSysName (mkTyVarOcc "c")
      let ty = mkTyVarTy var
          default_branch _ = mkCoAxBranch [var] [] [] [ty] (fallback ty) [Nominal] loc
      table <- makeFamily (idName owner) (ClosedInput charTy []) [] charTyCon
                          (map literal rows ++ [default_branch])
      pure (Just (owner, table))

{- Note [Checked Char cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Certify an exported direct Char worker from its solved, desugared binding.
This check runs only at final publication. It provides no table for an
earlier use in the defining module. Speculative desugaring keeps its
diagnostics local. Ordinary module desugaring reports them.

See Note [Character case certificates] in GHC.Core.CharCase for the
accepted Core. Keep literal equations before the default so closed-family apartness
governs reduction when the argument is still rigid. Reject visible rules
for the owner before constructing the family. The usual SourceTableId
publication and solver dependency checks apply to the resulting table.

An exact imported variable can forward to an existing checked table.
Require the target's own SourceTableId and the same Char domain and
codomain. Check the target and all its checked callees for visible rules
before publishing. The default equation references the target family, so
the usual dependency walk checks the same chain where the solver uses it.
The new table belongs to the alias. Re-exporting the target keeps its
original name and requires no forwarding table.
-}

-- | Require a lifted polymorphic signature with ordinary arrows and no contexts.
constructorHelperType :: Type -> Maybe ([ForAllTyBinder], [Type], Type)
constructorHelperType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
    binders <- traverse liftedSourceBinder telescope
    guard (not (null binders))
    let (domains, result) = splitArrows body
        parameters = [mkTyVarTy v | Bndr v _ <- binders]
    guard (not (null domains) && all (caseKind parameters) (result : domains))
    pure (binders, domains, result)
  where
    splitArrows tau
      | Just (FTF_T_T, ma, mult, input, output) <- splitFunTy_maybe tau
      , ma `eqType` unmatchableDataConTy, isManyTy mult
      = let (inputs, result) = splitArrows output in (input : inputs, result)
      | otherwise = ([], tau)

-- | Check an invisible, unmatchable binder over lifted types.
liftedSourceBinder :: (Matchability, ForAllTyBinder) -> Maybe ForAllTyBinder
liftedSourceBinder (ma, binder@(Bndr a (Invisible _)))
  | ma `eqType` unmatchableDataConTy
  , isTyVar a, varType a `eqType` liftedTypeKind = Just binder
liftedSourceBinder _ = Nothing

-- | Check a construction using earlier checked helpers and scoped inputs.
checkedConstructorHelper :: (TyCon -> Bool) -> (Id -> Maybe TyCon) -> Id -> Core.CoreExpr
                         -> Maybe ([ForAllTyBinder], [Type], Type, ConstructorCaseRow)
checkedConstructorHelper public helper owner rhs = do
    let rhs_type = exprType rhs
    guard (rhs_type `eqType` varType owner)
    (telescope, domains, result) <- constructorHelperType rhs_type
    (signature, _, _) <- constructorHelperType (varType owner)
    let (binders, body) = Core.collectBinders rhs
        (parameters, terms) = splitAt (length telescope) binders
        publicKind = casePublicKind public (mkTyVarTys parameters)
        declared = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope signature
    guard (parameters == [v | Bndr v _ <- telescope] &&
           all isId terms && eqTypes (map varType terms) domains)
    guard (all publicKind (result : domains))
    value <- checkCaseResult public publicKind helper
                             (\v -> v <$ guard (v `elem` terms)) (const Nothing) result body
    pure (declared, domains, result, ConstructorCaseRow (map CaseField terms) value)

{- Note [Published polymorphic identity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The constructor helper check derives the equation that returns its
argument. An exported identity can supply this equation even when its
interface omits its Core unfolding. Require a single lifted type
parameter and a single ordinary argument of that type. Check the same
projection before and after optimization.

The optimizer can share a body with another binding. Follow aliases
only within this module's final Core, and check the terminal body.

Keep an identity's private helper table when another source table
depends on it. A provisional final Core table cannot be a dependency
of another table. See Note [Final Core source equations].
-}

-- | Recognize an ordinary lifted polymorphic identity signature.
isIdentitySourceType :: Type -> Bool
isIdentitySourceType ty =
    case constructorHelperType ty of
      Just ([Bndr a _], [domain], result) ->
        getTyVar_maybe domain == Just a && domain `eqType` result
      _ -> False

-- | Check the exact projection using the existing constructor helper.
checkedIdentitySource :: Id -> Core.CoreExpr -> Maybe ([ForAllTyBinder], [Type], Type, ConstructorCaseRow)
checkedIdentitySource owner rhs = do
    guard (isIdentitySourceType (varType owner))
    checkedConstructorHelper (const False) (const Nothing) owner rhs

-- | Recognize an identity whose equation was checked against final Core.
isFinalCoreIdentity :: Id -> Bool
isFinalCoreIdentity v = finalCoreMapperOwner v && isIdentitySourceType (varType v)

-- | Recognize the lifted, unrestricted type of function composition.
compositionSourceType :: Type -> Maybe ([ForAllTyBinder], [Type], Type)
compositionSourceType ty = do
    let (parameters, body) = splitForAllForAllTyBinders ty
    guard (lengthIs parameters 3)
    telescope <- traverse liftedSourceBinder parameters
    (f, rest) <- caseArrow body
    (g, arrow) <- caseArrow rest
    (input, output) <- caseArrow arrow
    (middle, f_output) <- caseArrow f
    (g_input, g_output) <- caseArrow g
    guard (eqTypes [input, middle, output] [g_input, g_output, f_output])
    variables <- traverse getTyVar_maybe [input, middle, output]
    guard (mkUniqSet (binderVars telescope) == mkUniqSet variables)
    pure (telescope, [f, g, input], output)

-- | Check that composition applies the two original callbacks in order.
-- See Note [Checked function composition].
checkedCompositionSource :: Id -> Core.CoreExpr -> Maybe ([ForAllTyBinder], [Type], Type, ConstructorCaseRow)
checkedCompositionSource owner rhs = do
    let rhs_type = exprType rhs
    guard (rhs_type `eqType` varType owner)
    (signature, _, _) <- compositionSourceType (varType owner)
    (telescope, domains, result) <- compositionSourceType rhs_type
    let (binders, body) = Core.collectBinders rhs
    (parameters, terms@[f, g, x]) <- pure (splitAt (length telescope) binders)
    guard (parameters == binderVars telescope && all isId terms && eqTypes (map varType terms) domains)
    Core.App (Core.Var outer) (Core.App (Core.Var inner) (Core.Var arg)) <- pure body
    guard (outer == f && inner == g && arg == x)
    let declared = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope signature
        row = ConstructorCaseRow (map CaseField terms) (CaseApply f [CaseApply g [CaseField x]])
    pure (declared, domains, result, row)

{- Note [Checked function composition]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prelude's (.) binds two functions and an argument. Its body applies
the first function to the result of applying the second to the
argument. Require these exact Core applications before and after
optimization. A case on the inner result would change demand when the
outer function ignores its argument, so that body cannot supply this
equation.

The source table takes all three operands. A type can give it only
the two callbacks and pass the remaining arrow to map. Reducing the
complete application checks the source owner and its visible rules.
-}

{- Note [Identity certificates and open unfoldings]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An identity certificate can supply a bare callback without an unfolding.
It does not record the aliases exposed by optimizer inlining. A consumer
of the certificate must disable rewriting.

Keep the ordinary evaluator for a written application. The expected kind
of a closed call can determine the result of its operand, as in
(id (fromIntegral x) :: Nat). A closed executable call also uses the
ordinary staging rules. See GHC.Rename.Env. The open evaluator reads
each definition it visits, including aliases, and records its instantiated
rule head. Present those identities as ordinary Ids to the rule check.
Their Names, types, and attached rules are the same. The late rule
check can identify them in tcg_evaluated_term_uses. Keep the stricter
rule policy for nominal uses and for non-identity final Core owners.
-}

-- | Recognize a private constructor-case family and its nominal equations.
isSourceConstructorCase :: TyCon -> Bool
isSourceConstructorCase table = not (isNothing (sourceCaseVisibleArity table))

-- | Return the number of value operands of a certified constructor case.
sourceCaseVisibleArity :: TyCon -> Maybe Int
sourceCaseVisibleArity table
  | Just _ <- reverseWrapperWorker table = Just 1
  | isListFoldTable table = Just 3
  | Nothing <- sourceTableTermOcc (nameOccName (tyConName table)) = Nothing
  | Just axiom <- closedFamilyTyConCoAxiom_maybe table
  , coAxiomRole axiom == Nominal
  = let branches = fromBranches (coAxiomBranches axiom)
        kind = tyConKind table
    in case pairCaseType kind of
        Just (telescope, _, _, _) ->
          case traverse (caseBranchConstructor telescope) branches of
            Just [con] | con == tupleDataCon Boxed 2 -> Just 1
            _ -> Nothing
        Nothing -> case constructorCaseType kind <|> monomorphicGuardedListCaseType kind of
          Just (ConstructorCaseType telescope tc _ domains position _)
            | isNothing (monomorphicEnumCaseArity kind)
            , let visible = length domains
            , Just roots <- traverse (projectionBranch telescope tc visible position) branches
            , mkUniqSet roots == mkUniqSet (tyConDataCons tc) -> Just visible
          _ -> Nothing
  | otherwise = Nothing
  where
    projectionBranch telescope tc visible root branch = do
      (patterns, vars) <- branchPatterns telescope branch
      guard (lengthIs patterns visible)
      (before, root_pat, after) <- splitCaseOperand root patterns
      (root_con, _) <- constructorPattern root_pat
      guard (dataConTyCon root_con == tc)
      leaves <- concat <$> traverse patternLeaves patterns
      guard (eqTypes leaves vars && resultPattern (before ++ after) vars False (coAxBranchRHS branch))
      pure root_con

    patternLeaves ty
      | Just _ <- getTyVar_maybe ty = Just [ty]
      | otherwise = do
          (_, fields) <- constructorPattern ty
          concat <$> traverse patternLeaves fields

    resultPattern carried vars under_constructor ty
      | any (eqType ty) vars = True
      | Just (helper, args) <- splitTyConApp_maybe ty
      , isFilterGuardFor table helper
      , lengthIs args (tyConArity helper)
      , [predicate_result, p, x, xs] <- drop (helperInvisibleArity helper) args
      , any (eqType p) carried
      , Just (domain, result) <- caseArrow (typeKind p) =
          domain `eqType` typeKind x && result `eqType` boolTy &&
          eqType predicate_result (mkAppTy p x) && all (\v -> any (eqType v) vars) [p, x, xs]
      | Just (con, fields) <- constructorPattern ty
      = lengthIs fields (dataConRepArity con) && all (resultPattern carried vars True) fields
      | under_constructor, Just (fun, arg) <- splitAppTy_maybe ty
      , any (eqType fun) carried, any (eqType arg) vars
      , Just (domain, _) <- caseArrow (typeKind fun)
      = typeKind arg `eqType` domain
      | Just (helper, args) <- splitTyConApp_maybe ty
      , Just _ <- sourceTableTermOcc (nameOccName (tyConName helper))
      , Just _ <- closedFamilyTyConCoAxiom_maybe helper
      , lengthIs args (tyConArity helper)
      = all (resultPattern carried vars under_constructor) (drop (helperInvisibleArity helper) args)
      | otherwise = False

    caseBranchConstructor telescope branch = fst <$> caseBranch telescope branch
    caseBranch telescope branch = do
      (patterns, vars) <- branchPatterns telescope branch
      [pattern_ty] <- pure patterns
      (con, fields) <- constructorPattern pattern_ty
      guard (eqTypes fields vars)
      pure (con, fields)

    branchPatterns telescope branch = do
      let (params, patterns) = splitAt (length telescope) (coAxBranchLHS branch)
          (qvars, fields) = splitAt (length telescope) (coAxBranchTyVars branch)
      guard (null (coAxBranchCoVars branch) && eqTypes params (mkTyVarTys qvars))
      pure (patterns, mkTyVarTys fields)

    constructorPattern pattern_ty = do
      (promoted, args) <- splitTyConApp_maybe pattern_ty
      con <- isPromotedDataCon_maybe promoted
      let (parameters, fields) = splitAt (length (dataConUserTyVars con)) args
      guard (lengthIs parameters (length (dataConUserTyVars con)))
      unless (null (dataConRepArgBinders con)) $ do
        let domain = mkTyConApp (dataConTyCon con) parameters
        guard (isRetainedCaseRoot domain (dataConTyCon con))
        guard (Core.nativeIndexArgKinds con parameters fields)
      pure (con, fields)

-- | Identify the lifted parameter telescope and result of a boxed pair case.
pairCaseType :: Type -> Maybe ([ForAllTyBinder], TyVar, TyVar, Type)
pairCaseType tau = do
    let (telescope, body) = splitForAllForAllTyBinders tau
    binders <- traverse liftedBinder telescope
    (FTF_T_T, ma, mult, domain, result) <- splitFunTy_maybe body
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult)
    (tuple, [first, second]) <- splitTyConApp_maybe domain
    guard (tuple == tupleTyCon Boxed 2)
    x <- getTyVar_maybe first
    y <- getTyVar_maybe second
    let field ty = ty `eqType` first || ty `eqType` second
    guard (field result || case splitTyConApp_maybe result of
             Just (tc, [a,b]) -> tc == tuple && field a && field b
             _ -> False)
    let parameters = [v | Bndr v _ <- binders]
    guard (x `elem` parameters && y `elem` parameters)
    guard (all (\v -> v == x || v == y) parameters)
    pure (binders, x, y, result)
  where
    liftedBinder (_, binder@(Bndr v (Invisible _)))
      | isTyVar v, varType v `eqType` liftedTypeKind = Just binder
    liftedBinder _ = Nothing

-- | The number of leading invisible arguments to a checked helper family.
helperInvisibleArity :: TyCon -> Int
helperInvisibleArity = length . takeWhile isInvisibleTyConBinder . tyConBinders

-- | The kind telescope, input datatype, value domains, case position and result.
data ConstructorCaseType = ConstructorCaseType [ForAllTyBinder] TyCon [Type] [Type] Int Type

-- | Binary list cases use final Core evidence. Exclude them from ordinary cases.
isBinaryListCase :: ConstructorCaseType -> Bool
isBinaryListCase (ConstructorCaseType _ root _ _ position _) =
    root == listTyCon && position == 0

-- | Identify the two-list case grammar without the specialized append and
-- reverse worker signature. See Note [Checked cases on two lists].
isTwoListCase :: ConstructorCaseType -> Bool
isTwoListCase (ConstructorCaseType binders root _ domains position result) =
    root == listTyCon && position == 0 && twoListDomains domains &&
    not (lengthIs binders 1 && eqTypes domains [result, result])

isTwoListCaseTable :: TyCon -> Bool
isTwoListCaseTable table =
    isJust (sourceTableTermOcc (nameOccName (tyConName table))) &&
    isJust (closedFamilyTyConCoAxiom_maybe table) &&
    maybe False isTwoListCase (constructorCaseType (tyConKind table))

twoListDomains :: [Type] -> Bool
twoListDomains domains =
    lengthIs domains 2 && all (\ty -> fmap fst (caseDatatype ty) == Just listTyCon) domains

-- | Check the ordinary unary or binary signature for a constructor case.
constructorCaseType :: Type -> Maybe ConstructorCaseType
constructorCaseType tau = do
    let (telescope, body) = splitForAllForAllTyBinders tau
    binders <- traverse ordinaryCaseBinder telescope
    let quantified = [v | Bndr v _ <- binders]
        kinds = mkTyVarTys quantified
    (first, rest) <- caseArrow body
    (domain, carried, domains, position, result) <- case (caseArrow first, caseArrow rest) of
      (Just (input, output), Just (second, result))
        | all (caseKind kinds) [input, output] ->
          Just (second, [], [first, second], 1, result)
      (Nothing, Just (second, result)) -> Just (first, [second], [first, second], 0, result)
      (Nothing, Nothing) -> Just (first, [], [first], 0, rest)
      _ -> Nothing
    (tc, args) <- caseDatatype domain
    let signature = ConstructorCaseType binders tc args domains position result
        binder_kinds = map (varType . binderVar) binders
    guard (all isLiftedTypeKind binder_kinds ||
           (isRetainedCaseRoot domain tc && noFreeVarsOfType (mkForAllTys telescope liftedTypeKind)))
    let two_lists = twoListDomains domains
    if two_lists && null binders
      then guard (all (caseKind kinds) args)
      else do
        parameters <- catMaybes <$> traverse (domainParameter (fixedMapTypes signature)) args
        guard ((isJust (monomorphicEnumCaseArity tau) || not (null parameters)) &&
               sizeUniqSet (mkUniqSet parameters) == length parameters)
    guard (tyCoVarsOfTypes domains == mkUniqSet quantified &&
           caseKind kinds result && all (caseKind kinds) carried &&
           (tc /= listTyCon || position == 1 || two_lists) &&
           tc /= tupleTyCon Boxed 2)
    pure signature
  where
    domainParameter fixed ty
      | ty `eqType` liftedTypeKind || any (eqType ty) fixed = Just Nothing
      | otherwise = Just <$> getTyVar_maybe ty

-- | A type variable quantified by an ordinary inferred or specified forall.
ordinaryCaseBinder :: (Type, ForAllTyBinder) -> Maybe ForAllTyBinder
ordinaryCaseBinder (ma, binder@(Bndr v (Invisible _)))
  | ma `eqType` unmatchableDataConTy
  , isTyVar v = Just binder
ordinaryCaseBinder _ = Nothing

-- | Check the exact closed signature of a guarded list.
-- See Note [Monomorphic guarded list equations].
monomorphicGuardedListCaseType :: Type -> Maybe ConstructorCaseType
monomorphicGuardedListCaseType ty = do
    (predicate, rest) <- caseArrow ty
    (input, result) <- caseArrow rest
    (root, [element]) <- caseDatatype input
    guard (root == listTyCon && noFreeVarsOfType element && caseKind [] element &&
           predicate `eqType` mkVisFunTyManyU element boolTy && result `eqType` input)
    pure (ConstructorCaseType [] root [element] [predicate, input] 1 result)

{- Note [Monomorphic guarded list equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The general constructor-case signature tracks type parameters of the
scrutinized datatype. A guarded list can instead have a fixed element
type. Check that it is a closed, lifted datatype and require the exact
callback type for that element. The checker still inspects the same Core
fields and publishes a nominal axiom. Representational equality cannot
substitute a different callback domain.

Use this signature when checking a guarded result before and after
optimization, and when reading its equations. Other constructor cases
use the general signature. The private guard has no invisible kind
parameter for a concrete list. Read its value operands after the actual
invisible binders.
-}

{- Note [Retained field kinds in source equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The structural root admits a checked single-constructor telescope. Match
the actual Core alternative fields against that telescope in order. The
payload Id's type names the witness Id from the same alternative.
Check both fields and the result before publishing an equation.

The axiom has type variables instead of runtime Ids. Publish the witness
before the payload. Use the earlier variables to abstract the payload's
kind. For `DPair a (p :: a -> Type)`, freshen `a` before using it in
the kind of the new `p` binder. Keep the declared arrow matchability.
Match the result Id to the exact pattern leaf. The resulting RHS
does not introduce a free runtime Id or a new axiom about evaluation.
The ordinary visibility, constructor coverage, and RULE checks still
apply. Interface recognition checks the instantiated promoted argument
kinds again before classifying the table as a constructor case.
-}

-- | Check the retained constructor and its actual structural root domain.
-- See Note [Retained field kinds in source equations].
isRetainedCaseRoot :: Type -> TyCon -> Bool
isRetainedCaseRoot domain tc
  | [con] <- tyConDataCons tc
  , dataConHasPromotedRetained con
  = not (isNothing (nativeIndexDataConTyArgs domain con))
  | otherwise = False

-- | Read a lifted ordinary arrow with no dictionary or coercion operand.
caseArrow :: Type -> Maybe (Type, Type)
caseArrow ty = do
    (FTF_T_T, ma, mult, arg, res) <- splitFunTy_maybe ty
    guard (ma `eqType` unmatchableDataConTy && isManyTy mult &&
           all (isLiftedTypeKind . typeKind) [arg, res])
    pure (arg, res)

-- | Separate the scrutinized operand from the other operands.
splitCaseOperand :: Int -> [a] -> Maybe ([a], a, [a])
splitCaseOperand i xs = case splitAt i xs of
    (before, x : after) -> Just (before, x, after)
    _ -> Nothing

-- | The number of value arguments of an ordinary enumeration worker.
monomorphicEnumCaseArity :: Type -> Maybe Int
monomorphicEnumCaseArity = fmap (subtract 1 . length) . monomorphicEnumCaseKinds

-- | The domain and result kinds of a unary or binary enumeration worker.
monomorphicEnumCaseKinds :: Type -> Maybe [Type]
monomorphicEnumCaseKinds ty = do
    (domain, rest) <- caseArrow ty
    let kinds = case caseArrow rest of
          Just (carried, result) -> [domain, carried, result]
          Nothing -> [domain, rest]
    guard (all enumeration kinds)
    pure kinds
  where
    enumeration t = case caseDatatype t of
      Just (tc, []) -> all nullary (tyConDataCons tc)
      _ -> False
    nullary dc =
      case openDataConFieldTypes dc [] of
        Just [] -> True
        _ -> False

-- | Read a saturated ordinary boxed datatype with constructors.
caseDatatype :: Type -> Maybe (TyCon, [Type])
caseDatatype ty = do
    (tc, args) <- splitTyConApp_maybe ty
    guard (isVanillaAlgTyCon tc && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
           lengthIs args (tyConArity tc) && not (null (tyConDataCons tc)))
    pure (tc, args)

-- | Kinds expressible using lifted parameters and ordinary boxed datatypes.
caseKind :: [Type] -> Type -> Bool
caseKind parameters ty
  | ty `eqType` liftedTypeKind || any (eqType ty) parameters = True
  | Just (_, args) <- caseDatatype ty = all (caseKind parameters) args
  | otherwise = False

-- | A checked constructor tree. Pattern leaves bind Core Ids and result
-- leaves refer to those bindings.
data ConstructorCaseTree
  = CaseField Id
  | CaseConstructor DataCon [Type] [ConstructorCaseTree]
  | CaseCall TyCon [Type] [ConstructorCaseTree]
  | CaseSelf [Type] [ConstructorCaseTree]
  | CaseApply Id [ConstructorCaseTree]

-- | A complete path through the Core cases and its checked result.
data ConstructorCaseRow = ConstructorCaseRow [ConstructorCaseTree] ConstructorCaseTree

{- Note [Final Core source equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check lazy list equations and indexed projections at final typechecking
publication. At this point the module has checked all user declarations.
See Note [Nullary indexed source projections].
The new private family is available to neither user declarations nor the
helper collector. A no-code producer does not create it.

Before Tidy the driver checks the final optimized definition of the same
owner with the same structural test. This check covers the actual callback,
constructors, recursive arguments and demand. A changed or missing owner
loses both the provisional IdDetails and family. Ordinary compilation
continues. See keepValidatedSourceMappers in GHC.Driver.Main.Passes.

The final worker check says what the produced binding does. It does not
prove the rules that another module can use to rewrite a call. The distinct
interface origin requires the consumer to disable rewriting or check
every visible rule. See Note [Checked list traversal rules]. A fusion
rule can expose other rule heads and requires a separate proof.

Forming a type records the interface dependency without using the
equation. The solver checks the rules when it uses an equation,
including through an imported alias.

Note [Checked cases on two lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The case tree for zipLength inspects the first list before it inspects
the second. Keep every path in Core alternative order, including the
path that leaves the second operand untouched. The empty second list
does not select an equation when the first list is unknown.

For each recursive call, check each operand against its own original
binder and pattern. Accept the original value or a proper subterm of
that operand with the same type. At least one operand must decrease.
The sum of the sizes of the finite constructor trees decreases on each
call, even when different calls decrease different operands. The
checked case tree determines demand independently of that decrease.

A case binder names its scrutinee. Keep this alias when checking the
recursive operands. When a matched operand stays unchanged, use its
branch pattern as the operand in the type-level equation. This uses
the original value in Core and the known constructor in the equation.

The final checker matches all equations against the provisional axiom.
It uses the axiom's variables at the same positions in each path. This
checks the constructors and operands after alpha-renaming. Drop the
table if optimization changes a pattern or result, even if the new
body also passes the structural check. Check the exact owner for
visible rules at an imported use.

Ignore debugger breakpoints in both checks. Keep coverage ticks because
they record evaluation.
-}

{- Note [Nullary indexed source projections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A nullary GADT constructor can fix an invisible index of a binary
ordinary function. Check that the function cases on its first operand
and that the exact nullary alternative returns the second operand. A
cast has no runtime effect. Use separate nominal evidence for the equation.

The family quantifies the original invisible telescope. Each row
supplies the index from its constructor result and quantifies the
remaining parameters and the carried operand. Build the nominal
result-kind cast using ordinary family axioms. Permit one reduction
per argument under the same boxed result datatype. This check runs
before installing the module's source owners, so it uses the family
matcher directly. The cast records its axioms. Installation and
subsequent evidence use check those owners and their RULES.

The second row can rebuild one lazy constructor. The ordinary GADT
alternative binds its existential predecessor and its two fields. Check
that the result copies that head and suspends the same owner applied to
the strict subpattern and the same carried operand. Keep the other erased
arguments unchanged. Derive the promoted output index from the recursive
call's declared result. The same nominal argument check proves the cast
to the current branch's result kind. This certificate gives a decreasing
left operand and leaves argument definedness to the caller.

Check each published row again in final Core. The typechecker uses the
family for the nullary constructor, a checked recursive constructor with
a symbolic operand, or a bare neutral left operand. Closed recursive calls
keep the executable route. A symbolic field can be either an erased type
variable or a retained value from the type checker's index environment.
Check the original operand after selecting the equation. The length index
alone cannot select a row.
-}

-- | The owner's telescope, visible types, and one specialized index.
-- The constructor arguments use the owner's original type variables.
data IndexedProjection =
  IndexedProjection [ForAllTyBinder] (Type, Type) Type DataCon TyVar Type [Type]

-- | Recognize one nullary indexed constructor in an ordinary function's
-- first domain. The other binders and carried domain must be independent of
-- the refined variable.
indexedProjectionType :: Type -> Maybe IndexedProjection
indexedProjectionType ty = do
  let (binders, body) = splitForAllForAllTyBinders ty
  telescope <- traverse ordinary binders
  (left, more) <- caseArrow body
  (right, result) <- caseArrow more
  (tc, args) <- splitTyConApp_maybe left
  guard (not (null telescope) && isBoxedDataTyCon tc && isLiftedAlgTyCon tc &&
         lengthIs args (tyConArity tc))
  [(con, slot, index, parameters)] <- pure (mapMaybe (refinement args) (tyConDataCons tc))
  let quantified = binderVars telescope
  guard (slot `elem` quantified &&
         not (anyFreeVarsOfType (== slot) right) &&
         all (not . anyFreeVarsOfType (== slot) . varType) quantified)
  pure (IndexedProjection telescope (left, right) result con slot index parameters)
  where
    ordinary (ma, binder@(Bndr v (Invisible _)))
      | ma `eqType` unmatchableDataConTy, isTyVar v = Just binder
    ordinary _ = Nothing

    refinement args con = do
      let (universals, existentials, equalities, context, fields, _) = dataConFullSig con
      guard (null existentials && null context && null fields &&
             null (dataConStupidTheta con) && lengthIs universals (length args))
      [equation] <- pure equalities
      arg <- lookup (eqSpecTyVar equation) (zip universals args)
      slot <- getTyVar_maybe arg
      let index = eqSpecType equation
      guard (noFreeVarsOfType index && isTyFamFree index &&
             typeKind index `eqType` varType slot)
      parameters <- traverse (`lookup` zip universals args) (dataConUserTyVars con)
      pure (con, slot, index, parameters)

-- | Read only the explicit case on the first operand and its selected row.
checkedIndexedProjection :: (Core.CoreExpr -> Core.CoreExpr) -> Id -> Core.CoreExpr
                         -> Maybe (IndexedProjection, Maybe IndexedRecursion)
checkedIndexedProjection prepare owner original = do
  projection@(IndexedProjection binders _ _ con slot _ _) <- indexedProjectionType (varType owner)
  guard (exprType original `eqType` varType owner)
  let (parameters, body) = Core.collectBinders (prepare original)
      (core_types, arguments) = splitAt (length binders) parameters
  [left, right] <- pure arguments
  guard (all isTyVar core_types)
  Core.Case (Core.Var scrutinee) _ _ alts <- pure body
  guard (left == scrutinee && isId right)
  [rhs] <- pure [branch | Core.Alt (Core.DataAlt dc) [evidence] branch <- alts,
                         dc == con, isCoVar evidence]
  case stripIndexedBreakpoints rhs of
    Core.Var v | v == right -> pure ()
    Core.Cast (Core.Var v) _ | v == right -> pure ()
    _ -> Nothing
  let recursion = do
        row <- indexedRecursionType projection
        checkIndexedRecursion owner (binderVars binders) slot core_types right alts row
        pure row
  pure (projection, recursion)

-- | Publish the selected row with actual nominal result-kind evidence.
publishIndexedProjection :: Id -> IndexedProjection -> Maybe IndexedRecursion -> TcM (Maybe TyCon)
publishIndexedProjection owner projection@(IndexedProjection telescope (left, right) result con slot index con_args) recursion = do
    (renaming, parameters) <- freshenTyVarBndrs (binderVars telescope)
    let named = zipWith mkNamedTyConBinder (map binderFlag telescope) parameters
        instantiate = substTyUnchecked renaming . expandTypeSynonyms
        specialize = instantiate . substTyWith [slot] [index]
        hidden = map (specialize . mkTyVarTy) (binderVars telescope)
        branch_params = [v | (old, v) <- zip (binderVars telescope) parameters, old /= slot]
        left_kind = specialize left
        right_kind = specialize right
        result_kind = specialize result
        constructor = mkTyConApp (promoteDataCon con) (map specialize con_args)
    families <- tcGetFamInstEnvs
    case projectionResultCoercion families right_kind result_kind of
      Just cast | typeKind constructor `eqType` left_kind -> do
        carried <- fresh "carried" right_kind
        operands <- mapM (fresh "arg" . instantiate) [left, right]
        let vars = branch_params ++ [carried]
            right_pat = mkTyVarTy carried
            branch _ = mkCoAxBranch vars [] [] (hidden ++ [constructor, right_pat])
              (mkCastTy right_pat cast) (map (const Nominal) vars) (nameSrcSpan term)
        recursive <- maybe (pure Nothing)
          (publishIndexedRecursion families projection parameters instantiate carried) recursion
        Just <$> makeSourceFamily term (named ++ mkAnonTyConBinders operands)
                                  (instantiate result) (branch : maybe [] pure recursive)
      _ -> pure Nothing
  where
    term = idName owner
    fresh occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)

-- | Prove the result kind nominally, with at most one family step per index.
projectionResultCoercion :: FamInstEnvs -> Kind -> Kind -> Maybe Coercion
projectionResultCoercion families from to
  | from `eqType` to = Just (mkNomReflCo from)
  | otherwise = do
      (tc, left) <- splitTyConApp_maybe from
      (result_tc, right) <- splitTyConApp_maybe to
      guard (tc == result_tc && isBoxedDataTyCon tc && lengthIs left (length right))
      mkTyConAppCo Nominal tc <$> zipWithM check left right
  where
    check found expected
      | found `eqType` expected = Just (mkNomReflCo found)
      | otherwise = do
          (fam, args) <- splitTyConApp_maybe expected
          reduction <- reduceTyFamApp_maybe families Nominal fam args
          guard (reductionReducedType reduction `eqType` found)
          pure (mkSymCo (reductionCoercion reduction))

-- | The constructor's predecessor, result index, hidden arguments, and head kind.
-- The input tail has the owner's left domain at the predecessor.
data IndexedRecursion = IndexedRecursion DataCon TyVar Type [Type] Kind

-- | Check one lazy constructor whose only existential is the tail index.
indexedRecursionType :: IndexedProjection -> Maybe IndexedRecursion
indexedRecursionType (IndexedProjection _ (left, _) _ nil slot _ _) = do
    (_, args) <- splitTyConApp_maybe left
    [row] <- pure (mapMaybe (refinement args) (tyConDataCons (dataConTyCon nil)))
    pure row
  where
    refinement args con = do
      let (universals, existentials, equalities, context, fields, _) = dataConFullSig con
          instantiate = substTyWith universals args
      guard (null context && null (dataConStupidTheta con) &&
             lengthIs args (length universals) && isLazyDataConRep con &&
             all (not . isUnpacked) (dataConImplBangs con) &&
             dataConRepArity con == length fields + length equalities)
      [predecessor] <- pure existentials
      guard (isTyVar predecessor && varType predecessor `eqType` varType slot)
      [equation] <- pure equalities
      [head_kind, tail_kind] <- pure (map (instantiate . scaledThing) fields)
      actual_slot <- lookup (eqSpecTyVar equation) (zip universals args)
      let index = instantiate (eqSpecType equation)
      (successor, [argument]) <- splitTyConApp_maybe index
      _ <- isPromotedDataCon_maybe successor
      guard (actual_slot `eqType` mkTyVarTy slot &&
             argument `eqType` mkTyVarTy predecessor &&
             not (anyFreeVarsOfType (\v -> v == predecessor || v == slot) head_kind) &&
             isLiftedTypeKind (typeKind head_kind) &&
             tail_kind `eqType` substTyWith [slot] [argument] left)
      pure (IndexedRecursion con predecessor index
              (map (instantiate . mkTyVarTy) (dataConUserTyVars con)) head_kind)

-- | Verify the original head and the structurally smaller call in the
-- rebuilding branch. Constructor wrapping and its result cast are erased.
checkIndexedRecursion :: Id -> [TyVar] -> TyVar -> [TyVar] -> Id -> [Core.CoreAlt]
                      -> IndexedRecursion -> Maybe ()
checkIndexedRecursion owner parameters slot core_types carried alts (IndexedRecursion con _ _ _ _) = do
    [(predecessor, evidence, hd, tl, rhs)] <-
      pure [(p, co, x, xs, e) | Core.Alt (Core.DataAlt dc) [p, co, x, xs] e <- alts, dc == con]
    guard (isTyVar predecessor && isCoVar evidence && isId hd && isId tl)
    let value = case stripIndexedBreakpoints rhs of
          Core.Cast body _ -> stripIndexedBreakpoints body
          body -> body
    (Core.Var rebuild, args) <- pure (Core.collectArgs value)
    guard (isDataConId_maybe rebuild == Just con)
    [Core.Var first, recurse] <- pure (dropWhile Core.isTyCoArg args)
    guard (first == hd)
    (Core.Var self, arguments) <- pure (Core.collectArgs (stripIndexedBreakpoints recurse))
    let (types, terms) = splitAt (length parameters) arguments
        expected = [mkTyVarTy (if v == slot then predecessor else core_v)
                   | (v, core_v) <- zip parameters core_types]
    actual <- traverse (\case Core.Type ty -> Just ty; _ -> Nothing) types
    [Core.Var tail_arg, Core.Var carried_arg] <- pure terms
    guard (self == owner && eqTypes actual expected && tail_arg == tl && carried_arg == carried)

-- | Read through debugger breakpoints on an indexed result.
stripIndexedBreakpoints :: Core.CoreExpr -> Core.CoreExpr
stripIndexedBreakpoints = stripTicksTopE isSourceBreakpoint

-- | A debugger breakpoint does not evaluate its captured variables.
isSourceBreakpoint :: CoreTickish -> Bool
isSourceBreakpoint Breakpoint{} = True
isSourceBreakpoint _ = False

-- | Check the index of the recursive result and compute the promoted
-- constructor arguments, using the same untouched parameters as the input.
indexedRecursionResult :: IndexedProjection -> IndexedRecursion -> [Type] -> Type
                       -> Maybe ([Type], [Type], Kind)
indexedRecursionResult (IndexedProjection telescope (left, _) result _ slot _ _)
                       (IndexedRecursion _ predecessor index args _) hidden predecessor_ty = do
    (input_tc, input_args) <- splitTyConApp_maybe left
    let parameters = binderVars telescope
        at applied = substTyWith parameters applied
        recur_args = zipWith (\v ty -> if v == slot then predecessor_ty else ty) parameters hidden
        recur_kind = at recur_args result
        match (input, output)
          | getTyVar_maybe input == Just slot = Just (Just output)
          | at hidden input `eqType` output = Just Nothing
          | otherwise = Nothing
    (output_tc, output_args) <- splitTyConApp_maybe recur_kind
    guard (input_tc == output_tc && lengthIs input_args (length output_args))
    [output_index] <- catMaybes <$> traverse match (zip input_args output_args)
    let instantiate = at hidden . substTyWith [predecessor] [output_index]
        result_args = zipWith (\v ty -> if v == slot then instantiate index else ty) parameters hidden
    pure (recur_args, map instantiate args, at result_args left)

-- | Build the second row from the certified smaller call and exact head.
publishIndexedRecursion :: FamInstEnvs -> IndexedProjection -> [TyVar]
                        -> (Type -> Type) -> TyVar -> IndexedRecursion
                        -> TcM (Maybe (TyCon -> CoAxBranch))
publishIndexedRecursion families projection@(IndexedProjection telescope (left, right) result _ slot _ _)
                        parameters instantiate carried
                        row@(IndexedRecursion con predecessor index args head_kind) = do
    p <- fresh "predecessor" (instantiate (varType slot))
    let p_ty = mkTyVarTy p
        specialized = instantiate . substTyWith [predecessor] [p_ty]
        selected_index = specialized index
        branch_arg v | v == slot = selected_index
                     | otherwise = instantiate (mkTyVarTy v)
        hidden = map branch_arg (binderVars telescope)
        specialize = substTyWith (binderVars telescope) hidden
        tail_kind = specialize (substTyWith [slot] [p_ty] left)
    hd <- fresh "head" (specialize (specialized head_kind))
    tl <- fresh "tail" tail_kind
    pure $ do
      (recur_args, result_args, rebuilt_kind) <- indexedRecursionResult projection row hidden p_ty
      cast <- projectionResultCoercion families rebuilt_kind (specialize result)
      let h = mkTyVarTy hd
          t = mkTyVarTy tl
          ys = mkTyVarTy carried
          constructor = promoteDataCon con
          pat = mkTyConApp constructor (map (specialize . specialized) args ++ [h, t])
          vars = [v | (old, v) <- zip (binderVars telescope) parameters, old /= slot] ++ [p, hd, tl, carried]
      guard (typeKind pat `eqType` specialize left && typeKind ys `eqType` specialize right)
      pure $ \family ->
        let call = mkTyConApp family (recur_args ++ [t, ys])
            rhs = mkTyConApp constructor (result_args ++ [h, call])
        in mkCoAxBranch vars [] [] (hidden ++ [pat, ys]) (mkCastTy rhs cast)
                        (map (const Nominal) vars) (nameSrcSpan (tyConName family))
  where
    fresh occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)

-- | Read the exact private family and the constructor heads it checks.
indexedProjectionSource :: Id -> Maybe (TyCon, [DataCon])
indexedProjectionSource owner = do
  guard (finalCoreMapperOwner owner)
  table <- sourceTableId_maybe owner
  guard (tyConKind table `eqType` varType owner)
  projection@(IndexedProjection _ _ _ nil _ _ _) <- indexedProjectionType (varType owner)
  axiom <- closedFamilyTyConCoAxiom_maybe table
  guard (coAxiomRole axiom == Nominal)
  case fromBranches (coAxiomBranches axiom) of
    [empty] -> do
      checkIndexedSourceBranch projection table Nothing empty
      pure (table, [nil])
    [empty, nonempty] -> do
      checkIndexedSourceBranch projection table Nothing empty
      row@(IndexedRecursion con _ _ _ _) <- indexedRecursionType projection
      checkIndexedSourceBranch projection table (Just row) nonempty
      pure (table, [nil, con])
    _ -> Nothing

-- | Check the binders, specialized index, and the nominal RHS cast.
checkIndexedSourceBranch :: IndexedProjection -> TyCon -> Maybe IndexedRecursion -> CoAxBranch -> Maybe ()
checkIndexedSourceBranch projection@(IndexedProjection telescope (left, right) result nil slot index con_args)
                         table recursion branch = do
    let parameters = binderVars telescope
        (hidden, operands) = splitAt (length parameters) (coAxBranchLHS branch)
        specialize = substTyWith parameters hidden
    guard (lengthIs hidden (length parameters) && null (coAxBranchCoVars branch))
    [constructor, carried] <- pure operands
    carried_var <- getTyVar_maybe carried
    [refined] <- pure [arg | (v, arg) <- zip parameters hidden, v == slot]
    hidden_vars <- traverse getTyVar_maybe [arg | (v, arg) <- zip parameters hidden, v /= slot]
    (local_vars, wanted) <- case recursion of
      Nothing -> do
        guard (index `eqType` refined &&
               mkTyConApp (promoteDataCon nil) (map specialize con_args) `eqType` constructor)
        pure ([], carried)
      Just row@(IndexedRecursion con predecessor result_index args head_kind) -> do
        (_, [pred_ty]) <- splitTyConApp_maybe refined
        pred_var <- getTyVar_maybe pred_ty
        (promoted, constructor_args) <- splitTyConApp_maybe constructor
        let inst = specialize . substTyWith [predecessor] [pred_ty]
            (promoted_params, fields) = splitAt (length args) constructor_args
        [hd, tl] <- pure fields
        field_vars <- traverse getTyVar_maybe fields
        guard (promoted == promoteDataCon con && inst result_index `eqType` refined &&
               eqTypes (map inst args) promoted_params &&
               typeKind hd `eqType` inst head_kind &&
               typeKind tl `eqType` specialize (substTyWith [slot] [pred_ty] left))
        (recur_args, result_args, _) <- indexedRecursionResult projection row hidden pred_ty
        let call = mkTyConApp table (recur_args ++ [tl, carried])
        pure (pred_var : field_vars, mkTyConApp promoted (result_args ++ [hd, call]))
    let rhs = coAxBranchRHS branch
        vars = hidden_vars ++ local_vars ++ [carried_var]
    guard (coAxBranchTyVars branch == vars &&
           sizeUniqSet (mkUniqSet vars) == length vars &&
           eqTypes (map typeKind hidden) (map (specialize . varType) parameters) &&
           lengthIs (coAxBranchRoles branch) (length vars) &&
           all (== Nominal) (coAxBranchRoles branch) &&
           typeKind constructor `eqType` specialize left &&
           typeKind carried `eqType` specialize right &&
           typeKind rhs `eqType` specialize result)
    case rhs of
      CastTy payload co -> guard (payload `eqType` wanted && coercionRole co == Nominal &&
                                  coercionLKind co `eqType` typeKind wanted &&
                                  coercionRKind co `eqType` specialize result)
      _ -> guard (rhs `eqType` wanted)

-- | The two equations for a lazy constructor-to-constructor list map.
-- Check the same complete shape before and after optimizing its owner.
directListMapper :: Id -> Core.CoreExpr -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
directListMapper owner body = do
  result@(ConstructorCaseType _ tc _ _ position _, rows) <-
    checkedConstructorCase (const False) (const Nothing) owner body
  guard (tc == listTyCon && position == 1)
  case rows of
    [ ConstructorCaseRow [CaseField _, CaseConstructor nil _ []] (CaseConstructor retNil _ [])
      , ConstructorCaseRow [CaseField callback, CaseConstructor cons _ [CaseField h, CaseField t]]
          (CaseConstructor retCons _ [CaseApply f [CaseField x], CaseSelf _ [CaseField f', CaseField xs]]) ]
      | nil == nilDataCon, retNil == nilDataCon, cons == consDataCon, retCons == consDataCon
      , callback == f, callback == f', h == x, t == xs -> pure result
    _ -> Nothing

-- | Check append's two equations and the demand on both operands.
directListAppend :: Id -> Core.CoreExpr -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
directListAppend owner body = do
  result@(signature, rows) <-
    checkedConstructorCase (const False) (const Nothing) owner body
  guard (isBinaryListCase signature)
  case rows of
    [ ConstructorCaseRow [CaseConstructor nil _ [], CaseField right] (CaseField returned)
      , ConstructorCaseRow [CaseConstructor cons _ [CaseField h, CaseField t], CaseField right']
          (CaseConstructor retCons _ [CaseField head', CaseSelf _ [CaseField tail', CaseField carried]]) ]
      | nil == nilDataCon, cons == consDataCon, retCons == consDataCon
      , right == returned, h == head', t == tail', right' == carried -> pure result
    _ -> Nothing

-- | Check the ordinary, lifted signature of a right list fold.
listFoldType :: Type -> Maybe ConstructorCaseType
listFoldType ty = do
  let (telescope, body) = splitForAllForAllTyBinders ty
  binders <- traverse ordinaryCaseBinder telescope
  guard (all (isLiftedTypeKind . varType . binderVar) binders)
  (callback, rest) <- caseArrow body
  (seed, rest') <- caseArrow rest
  (list, result) <- caseArrow rest'
  (element, callback_rest) <- caseArrow callback
  (state, output) <- caseArrow callback_rest
  (tc, [parameter]) <- caseDatatype list
  guard (tc == listTyCon && element `eqType` parameter &&
         all (eqType seed) [state, output, result])
  case mkTyVarTys (binderVars binders) of
    [] -> guard (noFreeVarsOfType element && noFreeVarsOfType seed)
    [a, b] -> guard (eqTypes [element, seed] [a, b] || eqTypes [element, seed] [b, a])
    _ -> Nothing
  pure (ConstructorCaseType binders tc [element] [callback, seed, list] 2 result)

-- | Check the lifted signature of a list zipper with a curried callback.
listZipWithType :: Type -> Maybe ConstructorCaseType
listZipWithType ty = do
    let (telescope, body) = splitForAllForAllTyBinders ty
    binders <- traverse ordinaryCaseBinder telescope
    guard (all (isLiftedTypeKind . varType . binderVar) binders)
    [a, b, c] <- pure (mkTyVarTys (binderVars binders))
    (callback, rest) <- caseArrow body
    (left, rest') <- caseArrow rest
    (right, result) <- caseArrow rest'
    guard (callback `eqType` mkVisFunTyManyU a (mkVisFunTyManyU b c) &&
           eqTypes [left, right, result] (map mkListTy [a, b, c]))
    pure (ConstructorCaseType binders listTyCon [a] [callback, left, right] 1 result)

-- | Identify a nominal family backed by a checked zipper.
isListZipWithTable :: TyCon -> Bool
isListZipWithTable table = isJust $ do
    _ <- sourceTableTermOcc (nameOccName (tyConName table))
    axiom <- closedFamilyTyConCoAxiom_maybe table
    guard (coAxiomRole axiom == Nominal)
    listZipWithType (tyConKind table)

{- Note [Checked list zippers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the left list first. The left nil branch leaves the right list
and the callback alone. Only the left cons branch checks the right
list. Check the two-cons result with the ordinary lazy constructor
checker. It may build a pair from the heads or apply the carried
callback to both heads. The tail calls the same function on both
original tails.

The recursion can use the owner directly or a single local worker with
the callback in scope. In the local form, check that the owner applies
the worker to its exact list operands, allowing eta reduction. The
recursive call must use that exact worker and both original tails.
Recheck the rows against the axiom after optimization. Use the
existing final Core rule policy for the owner's signature.

The desugarer can put a pattern-failure join around the cases and
test an operand again in a default alternative. Select the reachable
alternative for each list constructor with findAlt. Check both nil
results and the cons result independently. A repeated test can only
inspect an operand already forced on that path. Thus no checked path
can reach the failure join, and the left nil path never tests the right.
-}

-- | Read the three ordered equations of a list zipper, with or without a callback.
checkedListZip :: (TyCon -> Bool) -> Id -> Core.CoreExpr
               -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
checkedListZip visible owner original = do
    let rhs = stripTicksE isSourceBreakpoint original
        rhs_type = exprType rhs
    guard (rhs_type `eqType` varType owner)
    ConstructorCaseType telescope root [a] domains position result <- signature rhs_type
    ConstructorCaseType declared _ _ _ _ _ <- signature (varType owner)
    let (bindings, body) = Core.collectBinders rhs
        (parameters, operands) = splitAt (length telescope) bindings
        (carried, supplied) = splitAt position operands
        types = mkTyVarTys parameters
    [left_kind, right_kind] <- pure (drop position domains)
    (right_tc, [b]) <- caseDatatype right_kind
    (result_tc, [c]) <- caseDatatype result
    guard (right_tc == listTyCon && result_tc == listTyCon)
    guard (parameters == binderVars telescope)
    ([left, right], cases, selfCall) <- case (supplied, body) of
      (_, Core.Let (Core.Rec [(worker, worker_rhs)]) invoke)
        | exactCall worker [] supplied invoke ->
            let (inputs, branches) = Core.collectBinders worker_rhs
            in pure (inputs, branches, \xs ys -> exactCall worker [] [xs, ys])
      ([_, _], _) ->
        pure (supplied, body, \xs ys -> exactCall owner types (carried ++ [xs, ys]))
      _ -> Nothing
    guard (all isId (carried ++ [left, right]) &&
           eqTypes (map varType carried) (take position domains) &&
           eqTypes (map varType [left, right]) [left_kind, right_kind])
    let outer = case cases of
          Core.Let (Core.NonRec j _) rest | isJoinId j -> rest
          _ -> cases
        left_nil = (left, a, nilDataCon)
        left_cons = (left, a, consDataCon)
        right_nil = (right, b, nilDataCon)
        right_cons = (right, b, consDataCon)
    (left_nil_fields, left_empty) <- listAlt result left_nil outer
    (_, empty) <- followCases result [(left_nil, left_nil_fields)] left_empty
    guard (nilResult c empty)
    (left_fields, inner) <- listAlt result left_cons outer
    (right_nil_fields, right_empty) <- listAlt result right_nil inner
    (_, empty') <- followCases result [(left_cons, left_fields), (right_nil, right_nil_fields)] right_empty
    guard (nilResult c empty')
    (right_fields, rest) <- listAlt result right_cons inner
    ([(_, [x, xs]), (_, [y, ys])], nonempty) <-
      followCases result [(left_cons, left_fields), (right_cons, right_fields)] rest
    let binders = zipWith (\v (Bndr _ vis) -> Bndr v vis) parameters declared
        nil element_kind = CaseConstructor nilDataCon [element_kind] []
        cell element_kind h t = CaseConstructor consDataCon [element_kind] [CaseField h, CaseField t]
        row left_pat right_pat = ConstructorCaseRow (map CaseField carried ++ [left_pat, right_pat])
        scoped v = v <$ guard (v `elem` [x, xs, y, ys])
        call expr =
          CaseSelf types (map CaseField (carried ++ [xs, ys])) <$ guard (selfCall xs ys expr) <|>
          do [callback] <- pure carried
             guard (exactCall callback [] [x, y] expr)
             pure (CaseApply callback (map CaseField [x, y]))
    value@(CaseConstructor cons _ [_, CaseSelf{}]) <-
      checkCaseResult public (casePublicKind public types) (const Nothing) scoped call result nonempty
    guard (cons == consDataCon)
    pure (ConstructorCaseType binders root [a] domains position result,
           [ row (nil a) (CaseField right) (nil c)
           , row (cell a x xs) (nil b) (nil c)
           , row (cell a x xs) (cell b y ys) value ])
  where
    signature ty = listZipWithType ty <|> do
      ordinary <- constructorCaseType ty
      ordinary <$ guard (isTwoListCase ordinary)

    public tc = visible tc || tc == listTyCon || isBoxedTupleTyCon tc

    exactCall fun kinds operands expression =
      case Core.collectArgs expression of
        (Core.Var callee, arguments)
          | (types, values) <- span Core.isTypeArg arguments ->
              callee == fun && eqTypes (map Core.exprToType types) kinds &&
              mapM operand values == Just operands
        _ -> False
    operand (Core.Var v) = Just v
    operand _ = Nothing

    nilResult output expression =
      case Core.collectArgs expression of
        (Core.Var nil_worker, [Core.Type nil_kind]) ->
          nil_worker == dataConWorkId nilDataCon && nil_kind `eqType` output
        _ -> False

    listAlt result (scrutinee, input, con) expression = do
      Core.Case (Core.Var actual) _ case_result alts <- pure expression
      guard (actual == scrutinee && case_result `eqType` result)
      Core.Alt alt fields rest <- findAlt (Core.DataAlt con) alts
      guard (all isId fields && eqTypes (map varType fields)
               (if alt == Core.DataAlt consDataCon then [input, mkListTy input] else []))
      pure (fields, rest)

    followCases result known expression
      | Core.Case (Core.Var scrutinee) _ _ _ <- expression = do
          (earlier, (fact, fields) : later) <- pure $
              break (\((v, _, _), _) -> v == scrutinee) known
          (new_fields, rest) <- listAlt result fact expression
          let selected = if null new_fields then fields else new_fields
          followCases result (earlier ++ (fact, selected) : later) rest
      | otherwise = pure (known, expression)

-- | Recognize the compiler-generated nominal family for a right list fold.
isListFoldTable :: TyCon -> Bool
isListFoldTable table = isJust $ do
  _ <- sourceTableTermOcc (nameOccName (tyConName table))
  axiom <- closedFamilyTyConCoAxiom_maybe table
  guard (coAxiomRole axiom == Nominal)
  listFoldType (tyConKind table)

{- Note [Checked right list folds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The empty branch returns the seed. The cons branch applies the original
callback to the original head and the fold of the original tail. Check
both branches and the exact recursive arguments. The cons branch can
pass the recursive result to a callback because the equation preserves
the call and both of its operands. It does not assert that the callback,
its operands, or the fold terminate.

A fold can recurse directly or use a local worker with the callback and
seed in its scope. The installed list binding uses the local worker.
Accept only an exact invocation of that worker on the list; the final
Core may add an eta binder. A monomorphic fold can use closed lifted
element and seed types. The same check runs before and after Core
optimization. In GHCi, read through debugger breakpoints on the empty
result, the callback application, and the recursive call. The executable
Core keeps these ticks. Rules that can replace the fold call remain a
separate obligation. In particular the type of a build producer does
not justify fold/build when the producer can use seq.
-}

-- | Check both defining equations against the actual binding.
directListFold :: Id -> Core.CoreExpr -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
directListFold owner original = do
  let rhs = stripTicksE isSourceBreakpoint original
      rhs_type = exprType rhs
  guard (rhs_type `eqType` varType owner)
  ConstructorCaseType telescope tc [element] domains position result <- listFoldType rhs_type
  ConstructorCaseType declared _ _ _ _ _ <- listFoldType (varType owner)
  let (binders, body) = Core.collectBinders rhs
      (type_binders, operands) = splitAt (length telescope) binders
      type_args = mkTyVarTys type_binders
  guard (type_binders == binderVars telescope)
  callback : seed : remaining <- pure operands
  (input, branches, exact_recur) <- case (remaining, body) of
    ([list], Core.Case{}) -> pure (list, body, directCall type_args callback seed)
    (_, Core.Let (Core.Rec [(worker, Core.Lam list branches)]) invoke)
      | startsWorker remaining worker invoke -> pure (list, branches, localCall worker)
    _ -> Nothing
  guard (all isId [callback, seed, input] && eqTypes (map varType [callback, seed, input]) domains)
  Core.Case (Core.Var actual) _ result_kind
    [ Core.Alt (Core.DataAlt nil) [] (Core.Var returned)
    , Core.Alt (Core.DataAlt cons) [head_arg, tail_arg] application ] <- pure branches
  guard (actual == input && result_kind `eqType` result && nil == nilDataCon &&
         cons == consDataCon && returned == seed && all isId [head_arg, tail_arg] &&
         eqTypes (map varType [head_arg, tail_arg]) [element, varType input])
  (Core.Var combine, [Core.Var hd, recur]) <- pure (Core.collectArgs application)
  guard (combine == callback && hd == head_arg && exact_recur tail_arg recur)
  let binders' = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope declared
      row spine value = ConstructorCaseRow [CaseField callback, CaseField seed, spine] value
      self = CaseSelf type_args (map CaseField [callback, seed, tail_arg])
  pure (ConstructorCaseType binders' tc [element] domains position result,
        [ row (CaseConstructor nil [element] []) (CaseField seed)
        , row (CaseConstructor cons [element] (map CaseField [head_arg, tail_arg]))
              (CaseApply callback [CaseField head_arg, self]) ])
  where
    directCall expected callback seed tail_arg expr = case Core.collectArgs expr of
      (Core.Var callee, args)
        | (types, [Core.Var f, Core.Var z, Core.Var xs]) <- span Core.isTypeArg args ->
            callee == owner && eqTypes (map Core.exprToType types) expected &&
            f == callback && z == seed && xs == tail_arg
      _ -> False
    localCall worker tail_arg expr = case expr of
      Core.App (Core.Var callee) (Core.Var xs) -> callee == worker && xs == tail_arg
      _ -> False
    startsWorker [] worker (Core.Var start) = start == worker
    startsWorker [eta] worker (Core.App (Core.Var start) (Core.Var original)) =
      start == worker && original == eta
    startsWorker _ _ _ = False

{- Note [Checked list reverse]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The worker scrutinizes only its first list. An empty list returns the
accumulator. A cons replaces the first list with its original tail and
adds its original head to the same accumulator. The call is in tail
position. It can return a constructor only after traversing the input
spine. Match the recursive callee, type argument, and both operands.
The simple optimizer removes desugaring aliases before this check.

The wrapper calls that checked local worker with its original input
and an empty accumulator. Publish both equations from the desugared
Core and recheck both at their original names after optimization. A
different target with the same shape cannot justify the wrapper's
equation. The library exposes the worker in GHC.Internal.List so an
ordinary proof can state an invariant for any accumulator.
-}
-- | Check the original tail and the exact lazy cons onto the carried list.
directListReverseWorker :: Id -> Core.CoreExpr
                        -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
directListReverseWorker owner rhs = do
  let rhs_type = exprType rhs
  guard (rhs_type `eqType` varType owner)
  signature@(ConstructorCaseType [binder] _ _ _ _ _) <- constructorCaseType rhs_type
  guard (isBinaryListCase signature)
  ConstructorCaseType [Bndr _ visibility] tc _ _ position _ <-
    constructorCaseType (varType owner)
  let (binders, body) = Core.collectBinders rhs
  [element, input, acc] <- pure binders
  guard (element == binderVar binder && isId input && isId acc)
  Core.Case (Core.Var actual) _ _
    [Core.Alt (Core.DataAlt nil) [] (Core.Var returned),
     Core.Alt (Core.DataAlt cons) [input_head, input_tail] step] <- pure body
  guard (actual == input && returned == acc && nil == nilDataCon && cons == consDataCon)
  (Core.Var self, [Core.Type kind, Core.Var next, pushed]) <- pure (Core.collectArgs step)
  guard (self == owner && kind `eqType` mkTyVarTy element && next == input_tail)
  (Core.Var cell, [Core.Type cell_kind, Core.Var first, Core.Var carried]) <-
    pure (Core.collectArgs pushed)
  guard (cell == dataConWorkId consDataCon && cell_kind `eqType` kind &&
         first == input_head && carried == acc)
  -- Publish with the actual Core type binder and the declared visibility.
  ConstructorCaseType _ _ core_args core_domains _ core_output <- pure signature
  let rows = [ ConstructorCaseRow [CaseConstructor nil [kind] [], CaseField acc] (CaseField acc)
             , ConstructorCaseRow [CaseConstructor cons [kind] [CaseField input_head, CaseField input_tail],
                                   CaseField acc]
                 (CaseSelf [kind] [CaseField input_tail,
                    CaseConstructor cons [kind] [CaseField input_head, CaseField acc]]) ]
  pure (ConstructorCaseType [Bndr element visibility] tc core_args core_domains position core_output, rows)

-- | Recognize the wrapper's ordinary polymorphic list signature.
listReverseWrapperType :: Type -> Maybe [ForAllTyBinder]
listReverseWrapperType ty = do
  (binders@[Bndr element _], [domain], output) <- constructorHelperType ty
  guard (domain `eqType` mkListTy (mkTyVarTy element) && domain `eqType` output)
  pure binders

-- | Require the original operand and an empty accumulator at the same kind.
directListReverseWrapper :: Id -> Core.CoreExpr -> Maybe ([ForAllTyBinder], Id, Type, Id)
directListReverseWrapper owner rhs = do
  let rhs_type = exprType rhs
      (binders, body) = Core.collectBinders rhs
  guard (rhs_type `eqType` varType owner)
  [Bndr _ visibility] <- listReverseWrapperType (varType owner)
  [core_element, input] <- pure binders
  guard (isTyVar core_element && isId input)
  (Core.Var callee, [Core.Type kind, Core.Var operand, empty]) <-
    pure (Core.collectArgs body)
  guard (kind `eqType` mkTyVarTy core_element && operand == input)
  (Core.Var nil, [Core.Type nil_kind]) <- pure (Core.collectArgs empty)
  guard (nil == dataConWorkId nilDataCon && nil_kind `eqType` kind)
  pure ([Bndr core_element visibility], input, kind, callee)

-- | A checked tail-recursive worker has a self-call at the root of its result.
isReverseWorkerTable :: TyCon -> Bool
isReverseWorkerTable table
  | Just _ <- sourceTableTermOcc (nameOccName (tyConName table))
  , Just axiom <- closedFamilyTyConCoAxiom_maybe table
  , coAxiomRole axiom == Nominal
  , Just signature <- constructorCaseType (tyConKind table)
  , isBinaryListCase signature =
      any tail_call (fromBranches (coAxiomBranches axiom))
  | otherwise = False
  where
    tail_call branch
      | Just (callee, _) <- splitTyConApp_maybe (coAxBranchRHS branch) = callee == table
      | otherwise = False

-- | Preserve the executable path for a closed call to the reverse wrapper.
isFinalCoreReverseWrapper :: Id -> Bool
isFinalCoreReverseWrapper owner =
    finalCoreMapperOwner owner && isJust (sourceTableId_maybe owner >>= reverseWrapperWorker)

-- | Read a checked wrapper's exact worker and classify its single equation.
reverseWrapperWorker :: TyCon -> Maybe TyCon
reverseWrapperWorker table = do
  _ <- sourceTableTermOcc (nameOccName (tyConName table))
  _ <- listReverseWrapperType (tyConKind table)
  axiom <- closedFamilyTyConCoAxiom_maybe table
  guard (coAxiomRole axiom == Nominal)
  [branch] <- pure (fromBranches (coAxiomBranches axiom))
  [element, operand] <- pure (coAxBranchTyVars branch)
  let kind = mkTyVarTy element
      left = mkTyVarTy operand
  guard (null (coAxBranchCoVars branch) && eqTypes (coAxBranchLHS branch) [kind, left])
  (worker, [actual, input, nil]) <- splitTyConApp_maybe (coAxBranchRHS branch)
  guard (eqTypes [actual, input, nil]
                 [kind, left, mkTyConApp (promoteDataCon nilDataCon) [kind]] &&
         isReverseWorkerTable worker)
  pure worker

{- Note [Checked guarded list equations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The cons branch must test the original callback on the original head.
Each result returns [], the original cons, the original tail, a call on
that tail, or the original head with that call as its lazy tail. The
empty branch skips the callback. The cons alternative establishes that
the input variable and case binder refer to the same cons. The equation
for returning either variable uses the original head and tail of that cons.

Check the exact body before and after optimization. The result of each
Boolean branch must agree. The simple optimizer removes the desugarer's
local failure continuation and binder aliases before the first check.
It does not apply user rewrite rules.
The optimizer can replace the empty constructor with the binder of the
list case. The empty alternative already establishes its constructor.

GHCi places debugger breakpoints on the predicate call, the constructor
head and the branch results. Read through these ticks in the verifier.
The checks compare the same cases and results as an object-code build.
The executable Core keeps its breakpoints. No-code builds still lack
the final Core check and cannot publish these equations.

The list family tests the spine first. Its cons equation calls a private
family whose first argument is the predicate result. An unknown result
cannot select either branch. The recursive application uses the original
callback and a strict subpattern of the list. Both families share the same
ordinary owner, including when the solver first sees the guard family
through an imported type. Drop both families if the final Core check fails.

The guard name identifies its original term owner. A guard with no
recursive branch can have no source family in its result. It still needs
its owner and cannot use the allowance for a legacy private leaf.
See Note [Legacy private source leaves].
-}

-- | The checked result of a guarded list branch.
data GuardedListResult = GuardNil | GuardInput | GuardTail | GuardRecur | GuardConsRecur

-- | The two list branches or the checked lazy pair construction.
data GuardedListBranches
  = GuardedListBranches (GuardedListResult, GuardedListResult)
  | GuardedPairBranches

-- | Check the predicate case and the result of each alternative.
directGuardedList :: (Core.CoreExpr -> Core.CoreExpr) -> Id -> Core.CoreExpr
                  -> Maybe (ConstructorCaseType, (GuardedListResult, GuardedListResult))
directGuardedList prepare owner original = do
    ConstructorCaseType declared tc [declared_el] [declared_pred, declared_input] 1 declared_result <-
      guarded_signature (varType owner)
    guard (tc == listTyCon && declared_input `eqType` declared_result &&
           declared_pred `eqType` mkVisFunTyManyU declared_el boolTy &&
           not (hasNativeDependentExprRhs original) &&
           exprType original `eqType` varType owner)
    let rhs = stripTicksE isSourceBreakpoint (prepare original)
    ConstructorCaseType telescope _ elements domains _ result <- guarded_signature (exprType rhs)
    [element] <- pure elements
    [pred_ty, input] <- pure domains
    let (binders, body) = Core.collectBinders rhs
        (types, terms) = splitAt (length telescope) binders
        kind_args = mkTyVarTys types
        returned con fields = listConstructor con element fields
    [p, ls] <- pure terms
    guard (types == binderVars telescope && all isId terms &&
           eqTypes (map varType terms) [pred_ty, input])
    (list_binder, x, xs, empty, rejected, accepted) <- guardedListBody p ls element result body
    case empty of
      Core.Var value | value == list_binder -> pure ()
      _ -> void (returned nilDataCon 0 empty)
    let recurse value = do
          (Core.Var f, args) <- pure (Core.collectArgs value)
          let (kind_args', value_args) = splitAt (length types) args
          kinds <- traverse (\case Core.Type ty -> Just ty; _ -> Nothing) kind_args'
          [Core.Var callback, Core.Var tail_arg] <- pure value_args
          guard (f == owner && eqTypes kinds kind_args && callback == p && tail_arg == xs)
        branch (Core.Var v)
          | v == list_binder || v == ls = Just GuardInput
          | v == xs = Just GuardTail
        branch value =
          (GuardNil <$ returned nilDataCon 0 value) <|>
          (GuardRecur <$ recurse value) <|>
          (do [Core.Var head_value, tail_value] <- returned consDataCon 2 value
              guard (head_value == x)
              GuardConsRecur <$ recurse tail_value)
    on_false <- branch rejected
    on_true <- branch accepted
    let kept_binders = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope declared
    pure (ConstructorCaseType kept_binders tc elements domains 1 result, (on_false, on_true))
  where
    guarded_signature ty = constructorCaseType ty <|> monomorphicGuardedListCaseType ty

-- | Read the input spine and the application of the same predicate to its head.
guardedListBody :: Id -> Id -> Type -> Type -> Core.CoreExpr
                -> Maybe (Id, Id, Id, Core.CoreExpr, Core.CoreExpr, Core.CoreExpr)
guardedListBody p input element result body = do
    Core.Case (Core.Var scrutinee) list_binder outer
      [ Core.Alt (Core.DataAlt nil) [] empty
        , Core.Alt (Core.DataAlt cons) [head_id, tail_id] nonempty ] <- pure body
    guard (scrutinee == input && outer `eqType` result && nil == nilDataCon &&
           cons == consDataCon && isId head_id && isId tail_id &&
           eqTypes (map varType [head_id, tail_id]) [element, varType input])
    Core.Case test _ inner
      [ Core.Alt (Core.DataAlt no) [] rejected
        , Core.Alt (Core.DataAlt yes) [] accepted ] <- pure nonempty
    (Core.Var predicate, [Core.Var head_arg]) <- pure (Core.collectArgs test)
    guard (predicate == p && head_arg == head_id && inner `eqType` result &&
           no == falseDataCon && yes == trueDataCon)
    pure (list_binder, head_id, tail_id, empty, rejected, accepted)

{- Note [Lazy recursive guarded list pairs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The input spine and predicate guard the returned pair. The accepting
branch returns a boxed pair with the original head in the first field.
Both field selectors scrutinize the same suspended recursive call.
The recursive call must stay under that suspension. A case on the
recursive pair before the result constructor would force the tail.

CPR can put an unboxed result in a private worker. Check the exported
wrapper's exact call and exact reboxing. In the worker, check the
unboxed constructors and the same lazy selectors. The reboxing on the
recursive edge must remain in the let binding. Check the target and
arguments of both calls. The rules for an ordinary guarded list still
require the original predicate, the original head, and the matched tail.

GHCi can keep a selector in a let binding named by a debugger breakpoint.
Check that exact binding when a result field uses it. The executable
Core keeps the aliases and breakpoints.

Use the installed checked pair projection equations in the accepting
branch. The publisher checks both projections at the list field kind.
Their family applications remain under the returned pair. A neutral
recursive tail therefore leaves the same family application as an
ordinary client call to the projection. The standard source dependency
walk checks the projection owners and their rules.
-}

-- | Check both forms of the guarded pair. See Note [Lazy recursive guarded list pairs].
directGuardedListPair :: (Core.CoreExpr -> Core.CoreExpr) -> [(Id, Core.CoreExpr)]
                      -> Id -> Core.CoreExpr -> Maybe ConstructorCaseType
directGuardedListPair prepare bindings owner original = do
    guard (exprType original `eqType` varType owner && not (hasNativeDependentExprRhs original))
    signature@(ConstructorCaseType telescope tc [element] [predicate, input] 1 result) <-
        constructorCaseType (varType owner)
    guard (tc == listTyCon && predicate `eqType` mkVisFunTyManyU element boolTy &&
           result `eqType` mkTupleTy Boxed [input, input])
    let rhs = stripTicksE isSourceBreakpoint (prepare original)
        operands expr = do
          let (binders, body) = Core.collectBinders expr
              (types, terms) = splitAt (length telescope) binders
          [p, ls] <- pure terms
          (root, [elt]) <- caseDatatype (varType ls)
          guard (root == listTyCon && all isId terms && lengthIs types (length telescope) &&
                 all (\v -> isTyVar v && isLiftedTypeKind (varType v)) types &&
                 varType p `eqType` mkVisFunTyManyU elt boolTy)
          pure (mkTyVarTys types, p, ls, elt, body)

        verify box loop expr = do
          guard (exprType expr `eqType` varType loop)
          (types, p, ls, elt, body) <- operands expr
          let list = varType ls
              result_ty = mkTupleTy box [list, list]
              nil (Core.Var v) list_binder = v == list_binder || v == ls
              nil value _ = isJust (listConstructor nilDataCon elt 0 value)
          (list_binder, x, xs, empty, rejected, accepted) <-
            guardedListBody p ls elt result_ty body
          (nil_left, nil_right) <- pairFields box list empty
          guard (nil nil_left list_binder && nil nil_right list_binder)
          (nil_field, Core.Var returned) <- pairFields box list rejected
          void (listConstructor nilDataCon elt 0 nil_field)
          guard (returned == list_binder || returned == ls)
          Core.Let (Core.NonRec suspended call) value <- pure accepted
          guard (varType suspended `eqType` mkTupleTy Boxed [list, list])
          recursive <- case box of
            Boxed -> pure call
            Unboxed -> reboxedCall list call
          guard (guardedPairCall types p xs recursive == Just loop)
          let (aliases, pair) = selectorLets value
          (prefix, suffix) <- pairFields box list pair
          [Core.Var head_id, tail_value] <- listConstructor consDataCon elt 2 prefix
          guard (head_id == x)
          first <- pairField aliases CLeft list suspended tail_value
          second <- pairField aliases CRight list suspended suffix
          guard (lengthIs aliases (length (catMaybes [first, second])))

        worker_wrapper = do
          (types, p, ls, _, body) <- operands rhs
          call <- reboxedCall (varType ls) body
          worker <- guardedPairCall types p ls call
          guard (worker /= owner)
          expression <- lookup worker bindings
          verify Unboxed worker (stripTicksE isSourceBreakpoint expression)
    verify Boxed owner rhs <|> worker_wrapper
    pure signature
  where
    selectorLets (Core.Let (Core.NonRec alias selector) value) =
      let (aliases, result) = selectorLets value
      in ((alias, selector) : aliases, result)
    selectorLets value = ([], value)

-- | Match a saturated list constructor at the checked element type.
listConstructor :: DataCon -> Type -> Int -> Core.CoreExpr -> Maybe [Core.CoreExpr]
listConstructor con element arity value = case Core.collectArgs value of
    (Core.Var worker, Core.Type ty : fields)
      | worker == dataConWorkId con, ty `eqType` element, lengthIs fields arity -> Just fields
    _ -> Nothing

-- | Match the original predicate, list operand, and visible type arguments.
guardedPairCall :: [Type] -> Id -> Id -> Core.CoreExpr -> Maybe Id
guardedPairCall types predicate input value = do
    (Core.Var function, args) <- pure (Core.collectArgs value)
    let (kind_args, terms) = span Core.isTypeArg args
    [Core.Var p, Core.Var ls] <- pure terms
    guard (p == predicate && ls == input && eqTypes (map Core.exprToType kind_args) types)
    pure function

-- | Read only the two lazy fields of the specified tuple constructor.
pairFields :: Boxity -> Type -> Core.CoreExpr -> Maybe (Core.CoreExpr, Core.CoreExpr)
pairFields box field_kind value = do
    (Core.Var worker, args) <- pure (Core.collectArgs value)
    let (_, terms) = span Core.isTypeArg args
    [first, second] <- pure terms
    guard (worker == dataConWorkId (tupleDataCon box 2) &&
           exprType value `eqType` mkTupleTy box [field_kind, field_kind] &&
           eqTypes (map exprType terms) [field_kind, field_kind])
    pure (first, second)

-- | Read a worker call whose unboxed result is placed in a new boxed pair.
reboxedCall :: Type -> Core.CoreExpr -> Maybe Core.CoreExpr
reboxedCall list expr = do
    Core.Case call _ result [Core.Alt (Core.DataAlt con) [first, second] rhs] <- pure expr
    guard (con == tupleDataCon Unboxed 2 && result `eqType` mkTupleTy Boxed [list, list] &&
           exprType call `eqType` mkTupleTy Unboxed [list, list])
    (Core.Var left, Core.Var right) <- pairFields Boxed list rhs
    guard (left == first && right == second)
    pure call

-- | Match a strict selector inside a lazy result field and report its alias.
pairField :: [(Id, Core.CoreExpr)] -> LeftOrRight -> Type -> Id -> Core.CoreExpr -> Maybe (Maybe Id)
pairField aliases side list suspended field = do
    (alias, value) <- case field of
      Core.Var v -> (Just v,) <$> lookup v aliases
      _ -> pure (Nothing, field)
    Core.Case (Core.Var pair) _ kind
      [Core.Alt (Core.DataAlt con) [first, second] (Core.Var selected)] <- pure value
    guard (con == tupleDataCon Boxed 2 && pair == suspended && kind `eqType` list &&
           selected == pickLR side (first, second))
    pure alias

-- | The auxiliary family shares the filter's source-equation owner.
filterGuardOcc :: OccName -> OccName
filterGuardOcc = mkSourceTableOcc . mkVarOcc . ("#guard:" ++) . occNameString

-- | Recognise a generated Boolean guard for a checked filter.
isSourceFilterGuard :: TyCon -> Bool
isSourceFilterGuard tc = isJust $
    sourceTableTermOcc (nameOccName (tyConName tc)) >>= stripPrefix "#guard:" . occNameString

-- | Compare the exact derived occurrence and module of a filter guard.
isFilterGuardFor :: TyCon -> TyCon -> Bool
isFilterGuardFor primary candidate =
    nameModule_maybe root == nameModule_maybe guard_name &&
    case sourceTableTermOcc (nameOccName root) of
      Just owner -> nameOccName guard_name == filterGuardOcc owner
      Nothing -> False
  where
    root = tyConName primary
    guard_name = tyConName candidate

-- | Resolve and check the pair selectors used by the guarded list equations.
-- See Note [Lazy recursive guarded list pairs].
guardedListProjections :: Type -> TcM (Maybe (TyCon, TyCon))
guardedListProjections list = do
    envs <- tcGetFamInstEnvs
    left <- fresh_field
    right <- fresh_field
    let pair = mkTyConApp (promotedTupleDataCon Boxed 2) [list, list, left, right]
        inputs = [list, list, pair]
        projection occ selected = do
          name <- lookupOrig (mkGhcInternalModule (fsLit "GHC.Internal.Data.Tuple")) (mkVarOcc occ)
          projected <- tcLookupIdMaybe name
          pure $ do
            table <- projected >>= sourceTableId_maybe
            guard (sourceCaseVisibleArity table == Just 1)
            reduced <- reduceTyFamApp_maybe envs Nominal table inputs
            guard (reductionReducedType reduced `eqType` selected)
            pure table
    first <- projection "fst" left
    second <- projection "snd" right
    pure ((,) <$> first <*> second)
  where
    fresh_field = mkTyVarTy . flip mkTyVar list <$> newSysName (mkTyVarOcc "field")

-- | Publish two families for the two tests in a guarded cons equation.
publishGuardedList :: Id -> ConstructorCaseType -> GuardedListBranches -> TcM (Maybe (TyCon, TyCon))
publishGuardedList owner signature@(ConstructorCaseType _ _ _ [_, list] _ _) branches =
    case branches of
      GuardedListBranches results -> Just <$> publishGuardedListWith owner signature (Left results)
      GuardedPairBranches -> do
        selectors <- guardedListProjections list
        traverse (publishGuardedListWith owner signature . Right) selectors
publishGuardedList _ _ _ = panic "publishGuardedList: invalid checked signature"

-- | Share the signature and predicate case across both guarded result shapes.
publishGuardedListWith :: Id -> ConstructorCaseType
                       -> Either (GuardedListResult, GuardedListResult) (TyCon, TyCon)
                       -> TcM (TyCon, TyCon)
publishGuardedListWith owner
    (ConstructorCaseType telescope _ [element] [predicate_domain, list_domain] _ result) branches = do
    (renaming, parameters) <- freshenTyVarBndrs (binderVars telescope)
    let instantiate = substTyUnchecked renaming . expandTypeSynonyms
        args = mkTyVarTys parameters
        kind_binders = zipWith mkNamedTyConBinder (map binderFlag telescope) parameters
        el = instantiate element
        pred_kind = instantiate predicate_domain
        list = instantiate list_domain
        result_kind = instantiate result
        loc = nameSrcSpan term
    operands <- mapM (fresh "arg") [pred_kind, instantiate list_domain]
    guard_operands <- mapM (fresh "arg") [boolTy, pred_kind, el, list]
    p <- fresh "predicate" pred_kind
    x <- fresh "head" el
    xs <- fresh "tail" list
    root_name <- newImplicitBinder term mkSourceTableOcc
    root_axiom <- newImplicitBinder root_name mkInstTyCoOcc
    guard_name <- newImplicitBinder term filterGuardOcc
    guard_axiom <- newImplicitBinder guard_name mkInstTyCoOcc
    let promoted_pair first second =
          mkTyConApp (promotedTupleDataCon Boxed 2) [list, list, first, second]
        branch vars lhs rhs _ =
          mkCoAxBranch (parameters ++ vars) [] [] (args ++ lhs) rhs
                        (map (const Nominal) (parameters ++ vars)) loc
        yes = mkTyConTy (promoteDataCon trueDataCon)
        no = mkTyConTy (promoteDataCon falseDataCon)
    let predicate = mkTyVarTy p
        hd = mkTyVarTy x
        tl = mkTyVarTy xs
        nil = mkTyConApp (promoteDataCon nilDataCon) [el]
        cons first rest = mkTyConApp (promoteDataCon consDataCon) [el, first, rest]
        spine = buildSourceFamily root_name root_axiom
          (kind_binders ++ mkAnonTyConBinders operands) result_kind
          [ branch [p] [predicate, nil] empty
            , branch [p, x, xs] [predicate, cons hd tl]
                (mkTyConApp select (args ++ [mkAppTy predicate hd, predicate, hd, tl])) ]
        select = buildSourceFamily guard_name guard_axiom
          (kind_binders ++ mkAnonTyConBinders guard_operands) result_kind
          [ branch [p, x, xs] [no, predicate, hd, tl] rejected
            , branch [p, x, xs] [yes, predicate, hd, tl] accepted ]
        branch_result = guardedListBranchResult spine args predicate hd tl
        (empty, rejected, accepted) = case branches of
          Left (on_false, on_true) ->
            (nil, branch_result on_false, branch_result on_true)
          Right (first, second) ->
            let recur = mkTyConApp spine (args ++ [predicate, tl])
                project selector = mkTyConApp selector [list, list, recur]
            in (promoted_pair nil nil, promoted_pair nil (cons hd tl),
                promoted_pair (cons hd (project first)) (project second))
    pure (spine, select)
  where
    term = idName owner
    fresh occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)
publishGuardedListWith _ _ _ = panic "publishGuardedListWith: invalid checked signature"

-- | Interpret a checked result using the head and tail of the matched cons.
guardedListBranchResult :: TyCon -> [Type] -> Type -> Type -> Type -> GuardedListResult -> Type
guardedListBranchResult spine params predicate hd tl branch = case branch of
    GuardNil -> mkTyConApp (promoteDataCon nilDataCon) [element]
    GuardInput -> cons tl
    GuardTail -> tl
    GuardRecur -> recur
    GuardConsRecur -> cons recur
  where
    element = typeKind hd
    cons rest = mkTyConApp (promoteDataCon consDataCon) [element, hd, rest]
    recur = mkTyConApp spine (params ++ [predicate, tl])

-- | Collect the provisional families that must leave the interface together.
finalCoreSourceFamilies :: Id -> [TyCon]
finalCoreSourceFamilies owner = do
    primary <- maybe [] pure (sourceTableId_maybe owner)
    primary : filter (isFilterGuardFor primary) (fromMaybe [] (sourceTableCallees primary))

-- | Identify an owner whose nominal equations require disabled caller rules.
-- See Note [Final Core source equations].
finalCoreMapperOwner :: Id -> Bool
finalCoreMapperOwner v = case idDetails v of
  SourceTableId _ _ FinalCoreSourceTable -> True
  _ -> False

-- | Remember exact owners before optimization.
finalCoreMappers :: [Core.CoreBind] -> [Id]
finalCoreMappers = filter finalCoreMapperOwner . map fst . Core.flattenBinds

-- | Confirm the same owner and the same checked equations after optimization.
verifyFinalCoreMapper :: [Core.CoreBind] -> Id -> Bool
verifyFinalCoreMapper binds original =
  case [(v, rhs) | (v, rhs) <- bindings, idName v == idName original] of
    [(owner, expression)]
      | varType owner `eqType` varType original
      , isIdentitySourceType (varType owner)
      -> verifyProjection (unitNameSet (idName owner)) owner expression
      | varType owner `eqType` varType original
      , isJust (compositionSourceType (varType owner))
      -> isJust (checkedCompositionSource owner expression)
      | varType owner `eqType` varType original
      , Just table <- sourceTableId_maybe original
      , isListFoldTable table || isListZipWithTable table ||
          isReverseWorkerTable table || isJust (reverseWrapperWorker table)
                       || isTwoListCaseTable table
      -> checkedFinalListTable bindings table
      | varType owner `eqType` varType original
      , Just (_, constructors) <- indexedProjectionSource original
      -> case checkedIndexedProjection id owner expression of
           Just (_, recursion) -> lengthIs constructors 1 || isJust recursion
           Nothing -> False
      | varType owner `eqType` varType original
      -> case finalCoreSourceFamilies original of
           [_] -> isJust (directListMapper owner expression <|> directListAppend owner expression)
           [spine, _] | Just (tc, _) <- splitTyConApp_maybe (tyConResKind spine)
                      , tc == tupleTyCon Boxed 2 ->
                        isJust (directGuardedListPair id bindings owner expression)
           [spine, select] -> isJust $ do
             (_, (on_false, on_true)) <- directGuardedList id owner expression
             axiom <- closedFamilyTyConCoAxiom_maybe select
             [rejected, accepted] <- pure (fromBranches (coAxiomBranches axiom))
             let check expected branch = do
                   let (params, operands) = splitAt (helperInvisibleArity select) (coAxBranchLHS branch)
                   [_, predicate, hd, tl] <- pure operands
                   guard (coAxBranchRHS branch `eqType`
                          guardedListBranchResult spine params predicate hd tl expected)
             check on_false rejected
             check on_true accepted
           _ -> False
    _ -> False
  where
    bindings = Core.flattenBinds binds
    verifyProjection seen owner (Core.Var target)
      | not (idName target `elemNameSet` seen)
      , varType target `eqType` varType owner
      , Just rhs <- lookup target bindings
      = verifyProjection (extendNameSet seen (idName target)) target rhs
    verifyProjection _ owner expression =
      not (isNothing (checkedIdentitySource owner expression))

-- | Check list tables even if earlier Core simplification dropped Id details.
failedFinalListTables :: [Core.CoreBind] -> [TyCon] -> [TyCon]
failedFinalListTables binds tables =
    [table | table <- tables
           , isListFoldTable table || isListZipWithTable table ||
             isReverseWorkerTable table || isJust (reverseWrapperWorker table)
                            || isTwoListCaseTable table
           , not (checkedFinalListTable bindings table)]
  where
    bindings = Core.flattenBinds binds

-- | Check the same final owner and any exact call from a reverse wrapper.
checkedFinalListTable :: [(Id, Core.CoreExpr)] -> TyCon -> Bool
checkedFinalListTable bindings table = isJust $ do
    (owner, rhs) <- find (owns table . fst) bindings
    guard (sourceTableId_maybe owner == Just table)
    case reverseWrapperWorker table of
      Just worker -> do
        (_, _, _, callee) <- directListReverseWrapper owner rhs
        guard (owns worker callee)
        worker_rhs <- lookup callee bindings
        directListReverseWorker callee worker_rhs
      Nothing | isListFoldTable table -> directListFold owner rhs
              | isListZipWithTable table -> do
                  (signature, rows) <- checkedListZip (const True) owner rhs
                  guard (matchesConstructorCase table signature rows)
                  pure (signature, rows)
              | isTwoListCaseTable table -> do
                  let check specialized = do
                        (signature, rows) <- checkedTwoListCaseWith specialized (const True) owner rhs
                        guard (matchesConstructorCase table signature rows)
                        pure (signature, rows)
                  check Nothing <|> do
                    worker <- checkedSpecConstrWorker bindings table owner
                    check (Just worker)
              | otherwise -> directListReverseWorker owner rhs
  where
    owns family value =
      let family_name = tyConName family
          owner_name = idName value
      in nameModule_maybe owner_name == nameModule_maybe family_name &&
         sourceTableTermOcc (nameOccName family_name) == Just (nameOccName owner_name)

-- | Check a left-first case tree on two ordinary lists.
checkedTwoListCase :: (TyCon -> Bool) -> Id -> Core.CoreExpr
                   -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
checkedTwoListCase = checkedTwoListCaseWith Nothing

checkedTwoListCaseWith :: Maybe Id -> (TyCon -> Bool) -> Id -> Core.CoreExpr
                       -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
checkedTwoListCaseWith specialized public owner rhs = do
    (signature, rows) <-
      checkedConstructorCaseWith specialized public (const Nothing) owner
        (stripTicksE isSourceBreakpoint rhs) <|>
      (guard (isNothing specialized) *> checkedListZip public owner rhs)
    guard (isTwoListCase signature)
    pure (signature, rows)

{- Note [Checked constructor specialization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SpecConstr can fix the first list to a known cons. Its rule can include
the second operand or leave it to a subsequent application. Check the
exact bound type arguments and field order in the rule. Find the exact
worker binder in this module's final Core.

Start the worker check with the captured cons in the first pattern.
Its remaining case must inspect the second list. Compare the rows to
the original cons equations. Each recursive worker call keeps both
captured fields and passes a proper subterm of the second operand.
Check the same field mapping at calls from the wrapper. Compare all
wrapper equations after translating these calls to the original owner.

Tidy uses demand information to choose the call-by-value arguments.
Check the two captured fields with the same Tidy computation. A strict
second list agrees with the checked first case of the worker.

The importer can recover the same worker from the owner's vanilla
unfolding. Tidy regenerates that unfolding from the final binding.
The direct check must fail and the check with the rule's worker must
succeed. Thus the final binding contains a call to this exact worker.
The producer could only keep the table after checking this worker
and its equations. An older producer with the direct check alone
cannot export a table for this wrapper.

Check the imported worker's call-by-value marks and every visible
rule for the worker. A missing vanilla unfolding leaves the rule
unchecked. A stable source unfolding can differ from the final
binding, so it cannot provide this evidence.
-}

-- | Recover the checked worker from the final wrapper's vanilla unfolding.
-- See Note [Checked constructor specialization].
checkedImportedSpecConstr :: RuleEnv -> Id -> TyCon -> CoreRule -> Maybe ()
checkedImportedSpecConstr env owner table rule = do
    worker <- checkSpecConstrRule owner rule
    guard (worker /= owner &&
           nameModule_maybe (idName worker) == nameModule_maybe (idName owner) &&
           lazySpecConstrFields worker && null (activeMapperRules env worker))
    Core.CoreUnfolding { Core.uf_src = VanillaSrc, Core.uf_tmpl = rhs } <-
      pure (realIdUnfolding owner)
    guard (isNothing (checkedTwoListCase (const True) owner rhs))
    (signature, rows) <- checkedTwoListCaseWith (Just worker) (const True) owner rhs
    guard (matchesConstructorCase table signature rows)

lazySpecConstrFields :: Id -> Bool
lazySpecConstrFields = all (not . isMarkedCbv) . take 2 . fromMaybe [] . idCbvMarks_maybe

-- | Validate a local worker specialized to a known cons in the first position.
-- See Note [Checked constructor specialization].
checkedSpecConstrWorker :: [(Id, Core.CoreExpr)] -> TyCon -> Id -> Maybe Id
checkedSpecConstrWorker bindings table owner = do
    ConstructorCaseType telescope _ _ [left, right] 0 result <- constructorCaseType (varType owner)
    [Bndr a _, Bndr b _] <- pure telescope
    guard (eqTypes [left, right] (map (mkListTy . mkTyVarTy) [a, b]))
    [rule@Rule{ru_local = True}] <-
      pure (filter (not . isNeverActive . ruleActivation) (idCoreRules owner))
    target <- checkSpecConstrRule owner rule
    (worker, rhs) <- find ((== target) . fst) bindings
    guard (worker /= owner && not (isExternalName (idName worker)) &&
           eqType (varType target) (varType worker) && eqType (exprType rhs) (varType worker))
    let (args, body) = Core.collectBinders (stripTicksE isSourceBreakpoint rhs)
    (x : y : fields@[hd, tl, other]) <- pure args
    guard (all isTyVar [x, y] && all isId fields &&
           eqTypes (map varType [x, y]) (map varType [a, b]))
    let parameters = mkTyVarTys [x, y]
        instantiate = substTyWith [a, b] parameters
        pattern = CaseConstructor consDataCon [mkTyVarTy x] [CaseField hd, CaseField tl]
    guard (eqTypes (map varType fields) (map instantiate [mkTyVarTy a, left, right]) &&
           maybe True null (idCbvMarks_maybe worker))
    guard (lazySpecConstrFields (tidyCbvInfoTop emptyNameSet worker rhs))
    rows <- checkedConstructorRows True (const True) (const Nothing)
              (checkedSpecializedCall worker other parameters)
              parameters (instantiate result) [pattern, CaseField other] other body
    axiom <- closedFamilyTyConCoAxiom_maybe table
    let cons_branch branch = case drop (length telescope) (coAxBranchLHS branch) of
          [first, _] -> fmap fst (splitTyConApp_maybe first) == Just (promoteDataCon consDataCon)
          _ -> False
        branches = filter cons_branch (fromBranches (coAxiomBranches axiom))
        worker_telescope = zipWith (\v (Bndr _ vis) -> Bndr v vis) [x, y] telescope
    guard (matchesConstructorRows table worker_telescope rows branches)
    pure worker

-- | Read the generated rule for that one specialization.
checkSpecConstrRule :: Id -> CoreRule -> Maybe Id
checkSpecConstrRule owner Rule{ru_fn = fn, ru_origin = origin, ru_auto = True,
                               ru_bndrs = binders, ru_args = lhs, ru_rhs = rhs} = do
    guard (fn == idName owner && Just origin == nameModule_maybe (idName owner))
    (Core.Type left : Core.Type right : first : rest) <- pure lhs
    a <- getTyVar_maybe left
    b <- getTyVar_maybe right
    (Core.Var cons, [Core.Type element, Core.Var hd, Core.Var tl]) <- pure (Core.collectArgs first)
    guard (cons == dataConWorkId consDataCon && element `eqType` left)
    later <- traverse (\case Core.Var v -> Just v; _ -> Nothing) rest
    let captures = hd : tl : later
        variables = a : b : captures
    guard (all isId captures && eqTypes (map varType [hd, tl]) [left, mkListTy left] &&
           (null later || eqTypes (map varType later) [mkListTy right]) &&
           lengthIs binders (length variables) &&
           sizeUniqSet (mkUniqSet binders) == length variables &&
           mkUniqSet binders == mkUniqSet variables)
    (Core.Var worker, Core.Type x : Core.Type y : arguments) <- pure (Core.collectArgs rhs)
    fields <- traverse (\case Core.Var v -> Just v; _ -> Nothing) arguments
    guard (left `eqType` x && right `eqType` y && captures == fields &&
           exprType (Core.mkApps (Core.Var owner) lhs) `eqType` exprType rhs)
    pure worker
checkSpecConstrRule _ _ = Nothing

-- | Translate a saturated call with these exact captured fields.
checkedSpecializedCall :: Id -> Id -> [Type] -> [ConstructorCaseTree] -> [(Id, Id)]
                       -> Core.CoreExpr -> Maybe ConstructorCaseTree
checkedSpecializedCall worker other parameters patterns aliases expr = do
    [first@(CaseConstructor con _ [CaseField hd, CaseField tl]), second] <- pure patterns
    (Core.Var callee, Core.Type x : Core.Type y : operands) <- pure (Core.collectArgs expr)
    guard (con == consDataCon && callee == worker && eqTypes [x, y] parameters)
    [actual_head, actual_tail, actual_other] <-
      traverse (\case Core.Var v -> Just (resolveCaseAlias aliases v); _ -> Nothing) operands
    guard (actual_head == hd && actual_tail == tl)
    (True, smaller) <- recursiveCaseOperand (other, second) actual_other
    pure (CaseSelf parameters [first, smaller])

-- | Compare the final rows with the exact nominal equations for this owner.
matchesConstructorCase :: TyCon -> ConstructorCaseType -> [ConstructorCaseRow] -> Bool
matchesConstructorCase table (ConstructorCaseType telescope _ _ _ _ _) rows =
    case closedFamilyTyConCoAxiom_maybe table of
      Just axiom -> matchesConstructorRows table telescope rows (fromBranches (coAxiomBranches axiom))
      Nothing -> False

matchesConstructorRows :: TyCon -> [ForAllTyBinder] -> [ConstructorCaseRow] -> [CoAxBranch] -> Bool
matchesConstructorRows table telescope rows branches = isJust $ do
    guard (equalLength rows branches)
    zipWithM matches rows branches
  where
    matches (ConstructorCaseRow patterns result) branch = do
      let (parameters, fields) = splitAt (length telescope) (coAxBranchTyVars branch)
          ids = concatMap patternFields patterns
          instantiate = substTyWith (binderVars telescope) (mkTyVarTys parameters)
          toType = constructorCaseTreeType table instantiate (zip ids fields)
      guard (null (coAxBranchCoVars branch) && equalLength parameters telescope &&
             equalLength ids fields &&
             eqTypes (map (instantiate . varType) ids) (map varType fields) &&
             eqTypes (mkTyVarTys parameters ++ map toType patterns) (coAxBranchLHS branch) &&
             toType result `eqType` coAxBranchRHS branch)

    patternFields (CaseField v) = [v]
    patternFields (CaseConstructor _ _ fields) = concatMap patternFields fields
    patternFields _ = panic "matchesConstructorCase: call in pattern"

-- | Read exhaustive cases on exact bound operands and check each result.
checkedConstructorCase :: (TyCon -> Bool) -> (Id -> Maybe TyCon) -> Id -> Core.CoreExpr
                       -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
checkedConstructorCase = checkedConstructorCaseWith Nothing

checkedConstructorCaseWith :: Maybe Id -> (TyCon -> Bool) -> (Id -> Maybe TyCon) -> Id -> Core.CoreExpr
                           -> Maybe (ConstructorCaseType, [ConstructorCaseRow])
checkedConstructorCaseWith specialized visible helper owner rhs = do
    let rhs_type = exprType rhs
    guard (rhs_type `eqType` varType owner)
    signature@(ConstructorCaseType telescope tc args domains position result) <-
      checked_signature
    let (bindings, body) = Core.collectBinders rhs
        (parameters, terms) = splitAt (length telescope) bindings
    guard (parameters == [v | Bndr v _ <- telescope])
    guard (all isId terms && eqTypes (map varType terms) domains)
    (_, scrutinee, _) <- splitCaseOperand position terms
    let kinds = [mkTyVarTy v | Bndr v _ <- telescope] ++ fixedMapTypes signature ++
                [element | isTwoListCase signature
                         , operand <- domains
                         , Just (_, [element]) <- [caseDatatype operand]]
        public_domain ty = case caseArrow ty of
          Just (input, output) -> all (casePublicKind public kinds) [input, output]
          Nothing -> casePublicKind public kinds ty
    guard (all public_domain (result : domains))
    let recursive patterns aliases expr =
          checkedSelfCall signature terms patterns aliases expr <|>
          checkedCallback signature patterns aliases expr <|>
          (do worker <- specialized
              [_, other] <- pure terms
              checkedSpecializedCall worker other (mkTyVarTys (binderVars telescope))
                                     patterns aliases expr)
    rows <- checkedConstructorRows (isTwoListCase signature) public helper recursive
                                  kinds result (map CaseField terms) scrutinee body
    -- The patterns use types from the Core binders. Use the same telescope
    -- for substitution. Keep the owner's declared visibility in the interface.
    ConstructorCaseType declared _ _ _ _ _ <- constructorCaseType (varType owner)
    let binders = zipWith (\(Bndr v _) (Bndr _ vis) -> Bndr v vis) telescope declared
    pure (ConstructorCaseType binders tc args domains position result, rows)
  where
    checked_signature = constructorCaseType (exprType rhs)

    public tc = visible tc || case checked_signature of
      Just (ConstructorCaseType _ root _ _ _ _) -> root == listTyCon && tc == root
      _ -> False

    checkedSelfCall signature@(ConstructorCaseType telescope tc args _ position _) terms patterns aliases expr = do
      (Core.Var callee, arguments) <- pure (Core.collectArgs expr)
      guard (callee == owner)
      let (kind_args, operands) = splitAt (length telescope) arguments
          expected = [mkTyVarTy v | Bndr v _ <- telescope]
      types <- traverse (\case Core.Type ty -> Just ty; _ -> Nothing) kind_args
      guard (eqTypes types expected)
      fields <- traverse (\case Core.Var v -> Just (resolveCaseAlias aliases v); _ -> Nothing) operands
      guard (equalLength terms fields)
      next <- if isTwoListCase signature
        then do
          checked <- zipWithM recursiveCaseOperand (zip terms patterns) fields
          guard (any fst checked)
          pure (map snd checked)
        else do
          (before, CaseConstructor _ _ children, after) <- splitCaseOperand position patterns
          (left, field, right) <- splitCaseOperand position fields
          guard (varType field `eqType` mkTyConApp tc args && any (hasCaseField field) children)
          original <- traverse (\case CaseField v -> Just v; _ -> Nothing) (before ++ after)
          guard (left ++ right == original)
          pure (map CaseField fields)
      pure (CaseSelf types next)

    checkedCallback (ConstructorCaseType _ _ _ _ 1 _) patterns aliases expr = do
      [CaseField carried, root@CaseConstructor{}] <- pure patterns
      (Core.Var fun, [Core.Var arg]) <- pure (Core.collectArgs expr)
      let field = resolveCaseAlias aliases arg
      guard (resolveCaseAlias aliases fun == carried && hasCaseField field root)
      (input, output) <- caseArrow (varType carried)
      guard (input `eqType` varType field && output `eqType` exprType expr)
      pure (CaseApply carried [CaseField field])
    checkedCallback _ _ _ _ = Nothing

-- | Check the remaining cases from the supplied operand patterns.
checkedConstructorRows :: Bool -> (TyCon -> Bool) -> (Id -> Maybe TyCon)
                       -> ([ConstructorCaseTree] -> [(Id, Id)] -> Core.CoreExpr -> Maybe ConstructorCaseTree)
                       -> [Type] -> Type -> [ConstructorCaseTree] -> Id -> Core.CoreExpr
                       -> Maybe [ConstructorCaseRow]
checkedConstructorRows bind_aliases public helper recursive parameters result patterns scrutinee =
    checkBody patterns [] (Just scrutinee)
  where
    checkCase patterns aliases scrutinee
              (Core.Case (Core.Var actual) binder case_result alternatives) = do
      guard (resolveCaseAlias aliases actual == scrutinee &&
             varType binder `eqType` varType actual && case_result `eqType` result)
      (tc, args) <- caseDatatype (varType actual)
      guard (public tc && all (casePublicKind public parameters) args)
      let branch_aliases
            | bind_aliases = (binder, scrutinee) : aliases
            | otherwise = aliases
      branches <- traverse (checkAlt patterns branch_aliases scrutinee tc args)
                           alternatives
      guard (completeCase tc (map fst branches))
      pure (concatMap snd branches)
    checkCase _ _ _ _ = Nothing

    checkAlt patterns aliases scrutinee tc args
             (Core.Alt (Core.DataAlt dc) fields body) = do
      guard (dataConTyCon dc == tc && dataConUserTyVars dc == dataConUnivTyVars dc)
      let domain = mkTyConApp tc args
          kinds = map varType fields
      if isRetainedCaseRoot domain tc then
        do { guard (all isNativeOpaqueValueId fields)
           ; guard (Core.nativeIndexArgKinds dc args (map TermVarTy fields))
           ; guard (all (publicFieldKind parameters (map TermVarTy fields)) kinds) }
      else
        do { expected <- openDataConFieldTypes dc args
           ; guard (all isId fields && eqTypes kinds expected)
           ; guard (all (casePublicKind public parameters) kinds) }
      let replace (CaseField v)
            | v == scrutinee = (True, CaseConstructor dc args (map CaseField fields))
          replace field@CaseField{} = (False, field)
          replace (CaseConstructor con types children) =
            let (flags, children') = unzip (map replace children)
            in (or flags, CaseConstructor con types children')
          replace call = (False, call)
          (replaced, patterns') = unzip (map replace patterns)
      guard (or replaced)
      rows <- checkBody patterns' aliases Nothing body
      pure (dc, rows)
    checkAlt _ _ _ _ _ _ = Nothing

    publicFieldKind parameters fields kind
      | Just (family, index) <- splitAppTy_maybe kind
      , any (eqType family) parameters, any (eqType index) fields
      = True
      | otherwise = casePublicKind public (parameters ++ fields) kind

    checkBody patterns aliases first_case body = case body of
      Core.Case (Core.Var next) _ _ _ ->
        checkCase patterns aliases
          (fromMaybe (resolveCaseAlias aliases next) first_case) body
      Core.Let (Core.NonRec binder (Core.Var target)) rest
        | isId binder, binder /= target, not (scoped binder)
        , isNothing (lookup binder aliases)
        , varType binder `eqType` varType target
        , let selected = resolveCaseAlias aliases target
        , scoped selected ->
            checkBody patterns ((binder, selected) : aliases) first_case rest
      _ -> do
        guard (isNothing first_case)
        value <- checkCaseResult public (casePublicKind public parameters) helper
                    (\v -> let selected = resolveCaseAlias aliases v
                           in selected <$ guard (scoped selected))
                    (recursive patterns aliases) result body
        pure [ConstructorCaseRow patterns value]
      where
        scoped v = any (hasCaseField v) patterns

recursiveCaseOperand :: (Id, ConstructorCaseTree) -> Id -> Maybe (Bool, ConstructorCaseTree)
recursiveCaseOperand (original, pattern) field
  | field == original = Just (False, pattern)
  | CaseConstructor _ _ children <- pattern
  , varType field `eqType` varType original
  , any (hasCaseField field) children = Just (True, CaseField field)
  | otherwise = Nothing

-- | Each alias already names a leaf or its original scrutinee.
resolveCaseAlias :: [(Id, Id)] -> Id -> Id
resolveCaseAlias aliases v = case lookup v aliases of
  Just target -> target
  Nothing -> v

hasCaseField :: Id -> ConstructorCaseTree -> Bool
hasCaseField selected (CaseField v) = selected == v
hasCaseField selected (CaseConstructor _ _ children) = any (hasCaseField selected) children
hasCaseField _ _ = False

-- | Account for the visibility of every datatype in a checked kind.
casePublicKind :: (TyCon -> Bool) -> [Type] -> Type -> Bool
casePublicKind public parameters kind
  | kind `eqType` liftedTypeKind || any (eqType kind) parameters = True
  | Just (tc, args) <- caseDatatype kind =
      public tc && all (casePublicKind public parameters) args
  | otherwise = False

-- | Check a scoped variable, lazy constructor, or an available helper call.
checkCaseResult :: (TyCon -> Bool) -> (Type -> Bool) -> (Id -> Maybe TyCon)
                -> (Id -> Maybe Id) -> (Core.CoreExpr -> Maybe ConstructorCaseTree)
                -> Type -> Core.CoreExpr -> Maybe ConstructorCaseTree
checkCaseResult public publicKind helper scoped self = check False
  where
    check under_constructor expected expr = do
      guard (exprType expr `eqType` expected)
      case Core.collectArgs expr of
        (Core.Var output, [])
          | Just selected <- scoped output
          , varType selected `eqType` expected ->
              pure (CaseField selected)
        (Core.Var worker, arguments)
          | Just dc <- isDataConWorkId_maybe worker -> do
              let tc = dataConTyCon dc
                  universals = dataConUnivTyVars dc
                  (type_args, fields) = splitAt (length universals) arguments
              guard (public tc && dataConUserTyVars dc == universals)
              args <- traverse coreType type_args
              guard (all publicKind args &&
                     mkTyConApp tc args `eqType` expected)
              kinds <- openDataConFieldTypes dc args
              guard (lengthIs fields (length kinds))
              children <- zipWithM (check True) kinds fields
              pure (CaseConstructor dc args children)
        (Core.Var callee, arguments)
          | Just table <- helper callee -> do
              let (type_args, fields) = splitAt (helperInvisibleArity table) arguments
              args <- traverse coreType type_args
              guard (all publicKind args)
              (input_kinds, result_kind) <- helperKinds table args
              guard (result_kind `eqType` expected && lengthIs fields (length input_kinds))
              children <- zipWithM (check under_constructor) input_kinds fields
              pure (CaseCall table args children)
        _ | under_constructor -> self expr
          | otherwise -> Nothing

    helperKinds table args = do
      let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders table)
          vars = [v | Bndr v _ <- invisible]
          result = tyConResKind table
          inst = substTyWith vars args
      guard (lengthIs args (length vars) && not (null visible) &&
             and (zipWith eqType (map typeKind args) (map (inst . varType) vars)))
      kinds <- traverse visibleKind visible
      pure (map inst kinds, inst result)
    visibleKind (Bndr v AnonTCB) = Just (varType v)
    visibleKind _ = Nothing

    coreType (Core.Type ty) = Just ty
    coreType _ = Nothing

-- | Check distinct and complete constructor coverage without a default.
completeCase :: TyCon -> [DataCon] -> Bool
completeCase tc actual =
    lengthIs actual (length expected) && mkUniqSet actual == mkUniqSet expected
  where
    expected = tyConDataCons tc

-- | Publish one nominal equation per complete path through the checked cases.
publishConstructorCase :: Id -> ConstructorCaseType -> [ConstructorCaseRow] -> TcM TyCon
publishConstructorCase owner (ConstructorCaseType telescope _ _ domains _ result) =
    publishConstructorRows owner telescope domains result

-- | Publish checked rows with the original kind telescope and visible domains.
publishConstructorRows :: Id -> [ForAllTyBinder] -> [Type] -> Type -> [ConstructorCaseRow] -> TcM TyCon
publishConstructorRows owner telescope domains result rows = do
    (renaming, quantifiers) <- freshenTyVarBndrs (binderVars telescope)
    let kind_binders = zipWith mkNamedTyConBinder (map binderFlag telescope) quantifiers
        instantiate = substTyUnchecked renaming . expandTypeSynonyms
    operands <- mapM (fresh "arg" . instantiate) domains
    branches <- forM rows $ \(ConstructorCaseRow patterns result_value) -> do
      (lhs, scoped) <- publishPatterns instantiate [] patterns
      let fields = reverse scoped
          vars = quantifiers ++ map snd fields
          branch family = mkCoAxBranch vars [] [] (mkTyVarTys quantifiers ++ lhs)
                      (constructorCaseTreeType family instantiate fields result_value)
                      (map (const Nominal) vars) (nameSrcSpan (idName owner))
      pure branch
    makeSourceFamily (idName owner) (kind_binders ++ mkAnonTyConBinders operands)
                     (instantiate result) branches
  where
    fresh occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)

    -- See Note [Retained field kinds in source equations].
    publishPatterns _ scoped [] = pure ([], scoped)
    publishPatterns instantiate scoped (pattern : patterns) = do
      (pat, scoped') <- publishPattern instantiate scoped pattern
      (pats, scoped'') <- publishPatterns instantiate scoped' patterns
      pure (pat : pats, scoped'')

    publishPattern instantiate scoped (CaseField ident) = do
      let kind = instantiate (varType ident)
          indices = [(TermVarTy runtime, type_var) | (runtime, type_var) <- scoped]
          checked_kind
            | not (anyFreeVarsOfType isId kind) = kind
            | Just abstracted <- abstractNativeIndices indices kind
            , not (anyFreeVarsOfType isId abstracted) = abstracted
            | otherwise = panic "publishConstructorCase: unchecked field kind"
      field <- fresh "field" checked_kind
      pure (mkTyVarTy field, (ident, field) : scoped)
    publishPattern instantiate scoped (CaseConstructor dc parameters children) = do
      (fields, scoped') <- publishPatterns instantiate scoped children
      pure (mkTyConApp (promoteDataCon dc) (map instantiate parameters ++ fields), scoped')
    publishPattern _ _ _ = panic "publishConstructorCase: call in pattern"

-- | Translate a checked case tree under the equation's field binders.
constructorCaseTreeType :: TyCon -> (Type -> Type) -> [(Id, TyVar)] -> ConstructorCaseTree -> Type
constructorCaseTreeType family instantiate fields = translate
  where
    translate (CaseField selected) = fieldType selected
    translate (CaseConstructor dc args children) = application (promoteDataCon dc) args children
    translate (CaseCall table args children) = application table args children
    translate (CaseSelf args operands) = application family args operands
    translate (CaseApply fun args) = mkAppTys (fieldType fun) (map translate args)

    application tc args children = mkTyConApp tc (map instantiate args ++ map translate children)
    fieldType selected = case lookup selected fields of
      Just v -> mkTyVarTy v
      Nothing -> panic "publishConstructorCase: unbound selected field"

{- Note [Reachable constructor helpers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Start from the free local Ids of the desugared case roots. Desugar a
candidate helper only the first time an exact resolved Id reaches it.
Sort the reachable graph with the existing SCC utility. Check callees
before callers. Skip a recursive component, including a self edge.
The result checker can only reference already checked local helpers or
exact imported source tables. A caller of a rejected helper cannot
publish an equation that calls it.

A private helper may also scrutinize an argument. Use the same case
checker as for an exported function. The caller keeps the helper
application under its constructor. Its family can select a branch
when a later client supplies the scrutinee. The checks for public
constructors, strict fields and exhaustive branches still apply.

Free Core Ids find candidates. The checked RHS family applications
determine which helpers actually leave the module. Traverse those
applications from published roots, including under constructors.
The ordinary source-table dependency walk checks the exact retained
owners and their rules. In particular, a private helper has no source
export, but it needs an interface Id and an axiom when a dependent
consumer can reach it through a public family.
-}

{- Note [Checked sum projections]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Start with a case on the first argument of the ordinary function. Each
subsequent case must scrutinize one of the still-visible variables. Check
all alternatives against the lazy boxed constructor representation. Check
that each leaf returns an in-scope field, the carried argument, or a
saturated ordinary constructor applied to checked results. Check each
result's kind against the actual worker and check the parameter order.
Building a lazy constructor introduces no new tests on its fields.
An ordinary callback can precede the scrutinized operand. Only use that
callback on a bound field beneath a lazy result constructor. Publish the
application with an unmatchable arrow and preserve the original value
argument order. An abstract callback is already a type variable. A
specific callback also needs a valid under-saturated family.

A worker can call itself beneath such a constructor. As in
Note [Closed source tables], the first operand must be a proper field
of the matched input. Require the original kind instantiation and
the exact recursive Id. If the worker has a second operand, it must
remain a pattern variable and the call must pass that same Core Id.
Each reduction passes a strict subpattern as the first operand. A
rigid tail remains stuck.
Keep a saturated helper as a reference to its exact checked family,
including inside a constructor field. An imported helper must already
have a source table. Within this producer, a private helper can supply
an equation if its solved Core returns a scoped input variable or
a checked lazy constructor tree over the inputs. A helper cannot inspect
its inputs. It can call another helper using public, checked kinds.
See Note [Reachable constructor helpers].
Publish only the transitive helper dependencies of checked case equations. The same
dependency closure retains the helper Id in stripped interfaces and
checks rules even when a client mentions only an exported alias.
A case removes its scrutinee from the scope of leaves. Pattern matching
can bind a selected variable through a nonrecursive let. Follow a
same-typed alias to an existing leaf without introducing a new field.
Reject casts, contexts, strict fields, defaults and unrelated scrutinees.

Each path contributes one nominal equation, with a constructor at every
point demanded by Core. A branch for Nested (Just x) requires both the
outer Nested and the inner Just. These equations do not match a neutral
scrutinee even when all of its possible branches would agree. The
unselected fields and any unused fallback are not demanded.

Require an exported local input datatype. Any other datatype mentioned
by an emitted pattern, result or field kind must also be public. Its exact
constructor names must be exported locally or visible through ordinary
imports. A poly-kinded datatype may have a kind argument fixed at Type
in the ordinary worker. This fixed argument belongs to the datatype
pattern, not to the worker's invisible telescope. The original
invisible telescope precedes one or two visible arguments. Recognize that prefix when checking applicable rules.
-}

-- | Recognize an ordinary function returning a boxed pair.
pairBuilderType :: Type -> Maybe ([ForAllTyBinder], [Type], Type, Type, Type)
pairBuilderType ty = do
  let (telescope, body) = splitForAllForAllTyBinders ty
      (_, arrows, result) = splitFunTys body
      domains = map scaledThing arrows
  (tc, [left, right]) <- splitTyConApp_maybe result
  guard (tc == tupleTyCon Boxed 2 && not (null domains) &&
         body `eqType` foldr mkVisFunTyManyU result domains &&
         all (isLiftedTypeKind . typeKind) domains)
  binders <- traverse lifted telescope
  pure (binders, domains, result, left, right)
  where
    lifted (_, b@(Bndr v (Invisible _)))
      | isTyVar v && varType v `eqType` liftedTypeKind = Just b
    lifted _ = Nothing

-- | Certify the constructor spine and allocate the suspended field's private family.
-- See Note [A partial pair in a nominal operand].
checkedPairBuilder :: Id -> Core.CoreExpr -> Maybe (TcM (Maybe (Id, TyCon), [TyCon]))
checkedPairBuilder owner rhs = do
  guard (exprType rhs `eqType` varType owner)
  (tvs, domains, result, left, right) <- pairBuilderType (exprType rhs)
  (signature, _, _, _, _) <- pairBuilderType (varType owner)
  let (params, body) = Core.collectBinders rhs
      (types, terms) = splitAt (length tvs) params
  guard (types == [v | Bndr v _ <- tvs] && all isId terms &&
         eqTypes domains (map varType terms))
  (Core.Var worker, [Core.Type l, Core.Type r, x, y]) <- pure (Core.collectArgs body)
  guard (isDataConWorkId_maybe worker == Just (tupleDataCon Boxed 2) &&
         eqTypes [l, r, exprType x, exprType y] [left, right, left, right])
  let input (Core.Var v) | v `elem` terms = Just v
      input _ = Nothing
  pure $ do
    let parameters = zipWith (\v (Bndr _ vis) -> Bndr v (NamedTCB vis)) types signature
        name = idName owner
        copiedPair known side opaque_kind occ = do
          args <- forM domains $ \ty -> flip mkTyVar ty <$> newSysName (mkTyVarOcc "arg")
          let binders = parameters ++ mkAnonTyConBinders args
              vars = types ++ args
              lhs = mkTyVarTys vars
              selected = case lookup known (zip terms args) of
                Just v -> mkTyVarTy v
                Nothing -> panic "checkedPairBuilder: unbound pair field"
          field <- emptyPairField name occ binders opaque_kind
          let suspended = mkTyConApp field lhs
              pair_fields = pickLR side ([selected, suspended], [suspended, selected])
              pair = mkTyConApp (promotedTupleDataCon Boxed 2) ([left, right] ++ pair_fields)
              branch _ = mkCoAxBranch vars [] [] lhs pair (map (const Nominal) vars) (nameSrcSpan name)
          table <- makeSourceFamily name binders result [branch]
          pure (Just (owner, table), [field])
    case (input x, input y) of
      (Just v, Nothing) -> copiedPair v CLeft right mkSourceSndOcc
      (Nothing, Just v) -> copiedPair v CRight left mkSourceFstOcc
      _ | Just _ <- pairObservationType (varType owner) -> do
            key <- flip mkTyVar typeSymbolKind <$> newSysName (mkTyVarOcc "observation")
            let binders = mkAnonTyConBinders [key]
            first <- emptyPairField name (mkObservedFieldOcc CRight) binders left
            second <- emptyPairField name (mkObservedFieldOcc CLeft) binders right
            pure (Nothing, [first, second])
        | otherwise -> pure (Nothing, [])

-- | Reserve the unread field occurrence for the given projection.
mkObservedFieldOcc :: LeftOrRight -> OccName -> OccName
mkObservedFieldOcc side = mkTcOcc . (prefix ++) . occNameString
  where
    prefix = pickLR side ("Q2:", "Q1:")

-- | Allocate an opaque field family in the ordinary producer's interface.
emptyPairField :: Name -> (OccName -> OccName) -> [TyConBinder] -> Kind -> TcM TyCon
emptyPairField owner occ binders result = do
    name <- newImplicitBinder owner occ
    let kind = mkTyConKindWithMatchability unmatchableDataConTy binders result
    pure (mkFamilyTyCon name kind binders 0 result Nothing
            (ClosedTypeFamilyTyCon (CTF Nothing)) Nothing NotInjective)

-- | Find the exact private field of an ordinary imported pair producer.
lookupObservedPairField :: LeftOrRight -> Id -> [Type] -> Kind -> TcM (Maybe TyCon)
lookupObservedPairField side producer params expected
  | Just mod <- nameModule_maybe owner
  , not (isHoleModule mod)
  , (installed, Nothing) <- getModuleInstantiation mod = do
      env <- getGblEnv
      let ordinary = case lookupGRE_Name (tcg_rdr_env env) owner of
            Just gre -> any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
            Nothing -> False
          boot = gwib_isBoot <$> lookupInstalledModuleEnv
                   (imp_boot_mods (tcg_imports env)) installed
      if mod /= tcg_semantic_mod env && ordinary && boot /= Just IsBoot
        then loadField mod
        else pure Nothing
  | otherwise = pure Nothing
  where
    owner = idName producer

    loadField mod = do
      iface <- loadInterfaceForName (text "staged pair producer") owner
      let occ = mkObservedFieldOcc side (nameOccName owner)
      case mi_hash_fn iface occ of
        Nothing -> pure Nothing
        Just _ -> do
          name <- lookupOrig mod occ
          thing <- tcLookupGlobal name
          pure $ do
            ATyCon field <- Just thing
            guard (tyConName field == name)
            ClosedTypeFamilyTyCon (CTF Nothing) <- famTyConFlav_maybe field
            let (invisible, visible) = span isInvisibleTyConBinder (tyConBinders field)
                parameters = [v | Bndr v _ <- invisible]
                inst = substTyWith parameters params
            [Bndr key AnonTCB] <- pure visible
            guard (varType key `eqType` typeSymbolKind &&
                   lengthIs params (length parameters) &&
                   eqTypes (map typeKind params) (map (inst . varType) parameters) &&
                   inst (tyConResKind field) `eqType` expected)
            pure field

-- | Keep ordinary staging if simplification can change the producer call.
canObservePairSource :: Id -> TcM Bool
canObservePairSource owner
  | Core.hasCoreUnfolding (idUnfolding owner) = pure False
  | otherwise = isNothing <$> changedSourceUse [(owner, Nothing)]

-- | Find the exact hidden observer emitted by the final Core pass.
lookupPairObserver :: LeftOrRight -> Id -> Int -> TcM (Maybe Id)
lookupPairObserver side producer arity
  | Just mod <- nameModule_maybe owner =
      lookupRecipeEntry False mod (pairObservationOcc (Just side) arity (nameOccName owner))
  | otherwise = pure Nothing
  where
    owner = idName producer

-- | Check both artifact forms of a scalar relay. An object-only replacement
-- can add the table or replace it with the old per-owner closure.
lookupPairRecipe :: Module -> OccName -> Int -> TcM (Maybe Id, Maybe Id)
lookupPairRecipe mod owner arity =
    (,) <$> lookupRecipeEntry True mod scalarRecipeTableOcc
        <*> lookupRecipeEntry True mod (pairObservationOcc Nothing arity owner)

-- | An optional artifact probe can use the reserved entry type when an old
-- interface has no declaration. This ID is used only in temporary bytecode
-- guarded by the loader's optional symbol check.
lookupRecipeEntry :: Bool -> Module -> OccName -> TcM (Maybe Id)
lookupRecipeEntry allow_missing mod occ = do
    name <- lookupOrig mod occ
    iface <- loadInterfaceForName (text "staged pair recipe") name
    case mi_hash_fn iface occ of
      Nothing
        | allow_missing -> pure (Just (mkVanillaGlobal name stringTy))
        | otherwise -> pure Nothing
      Just _ -> do
        thing <- tcLookupGlobal name
        pure $ case thing of
          AnId entry | idName entry == name, varType entry `eqType` stringTy -> Just entry
          _ -> Nothing

-- | Identify the copied field and its zero-based position among the value inputs.
sourcePairBuilderField :: TyCon -> Maybe (LeftOrRight, Int)
sourcePairBuilderField table = do
    owner <- sourceTableTermOcc (nameOccName (tyConName table))
    axiom <- closedFamilyTyConCoAxiom_maybe table
    [branch] <- pure (fromBranches (coAxiomBranches axiom))
    let lhs = coAxBranchLHS branch
        inputs = drop (length (takeWhile isInvisibleTyConBinder (tyConBinders table))) lhs
        opaque occ ty = case splitTyConApp_maybe ty of
          Just (tc,args) -> nameModule_maybe (tyConName tc) == nameModule_maybe (tyConName table) &&
                            nameOccName (tyConName tc) == occ owner &&
                            eqTypes args lhs && case famTyConFlav_maybe tc of
                              Just (ClosedTypeFamilyTyCon (CTF Nothing)) -> True
                              _ -> False
          _ -> False
    (pair, [_, _, left, right]) <- splitTyConApp_maybe (coAxBranchRHS branch)
    guard (pair == promotedTupleDataCon Boxed 2 && coAxiomRole axiom == Nominal &&
           eqTypes lhs (mkTyVarTys (coAxBranchTyVars branch)) && null (coAxBranchCoVars branch))
    (side, copied) <- if opaque mkSourceSndOcc right then Just (CLeft, left)
                      else if opaque mkSourceFstOcc left then Just (CRight, right)
                      else Nothing
    input <- findIndex (eqType copied) inputs
    pure (side, input)

{- Note [A partial pair in a nominal operand]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For make n = (n, e), the solved Core returns the boxed constructor before
it inspects e. Give the unknown field an empty closed family P2:make
indexed by every type and value argument. The checked equation is

    T:make n = '(n, P2:make n)

The module and position are part of the field's Name. Nothing reduces
P2:make and no equality about e follows from its creation. Checking the
constructor spine needs no unfolding from e. The symmetric equation uses
P1:make. A case or a cast outside the pair does not have this shape.

A direct producer with the executable observer ABI exports a private
Q2 family when it has no copied field. The observer fills the demanded
field using actual executable code. The unread field uses the exact
producer declaration and an observation key.
The key records the full semantic unit and module and an ordinal from that
module's successful observations. Allocate after readback succeeds.
Unlike compiler Uniques or source locations, this numbering is independent
of the Unique supply and distinguishes multiple checks of a single span. Do not add a T equation merely to expose these private fields.
This preserves ordinary name resolution when neither field is copied.
See Note [The types Any and UnusedType] in GHC.Builtin.WiredIn.Types.

The renamer keeps the ordinary producer in scope. A direct nominal
projection can store both checked applications in an interface when
its equation selects the copied field. The solver validates the actual
owners and their rules when it uses either equation. Ordinary executable
calls still use the producer, including calls that select the opaque
field. Quoting all inputs of the field family must also preserve demand.
A computed argument in any other position stays on the executable path.
-}

-- | The original kind-parameter order and result of the alternative.
data PairCase = PairCase [ForAllTyBinder] TyVar TyVar PairCaseResult

-- | The index of a field bound by the checked case.
data PairField = FirstField | SecondField

-- | Return a field or build another boxed pair from the bound fields.
data PairCaseResult = SelectField PairField | BuildPair PairField PairField

-- | Certify a strict pair case that uses only its bound fields.
checkedPairCase :: Id -> Core.CoreExpr -> Maybe PairCase
checkedPairCase owner rhs = do
    let rhs_type = exprType rhs
    guard (rhs_type `eqType` varType owner)
    (binders, x, y, r) <- pairCaseType rhs_type
    (owner_binders, x_owner, y_owner, _) <- pairCaseType (varType owner)
    let (parameters, rest) = Core.collectBinders rhs
        (type_args, term_args) = splitAt (length binders) parameters
    guard (type_args == [v | Bndr v _ <- binders])
    [pair] <- pure term_args
    guard (isId pair && varType pair `eqType` mkTyConApp (tupleTyCon Boxed 2) (mkTyVarTys [x,y]))
    Core.Case (Core.Var scrutinee) case_binder result
      [Core.Alt (Core.DataAlt con) [first, second] alternative] <- pure rest
    guard (scrutinee == pair && varType case_binder `eqType` varType pair)
    guard (con == tupleDataCon Boxed 2 && first /= second)
    guard (isId first && isId second)
    guard (varType first `eqType` mkTyVarTy x && varType second `eqType` mkTyVarTy y)
    guard (result `eqType` r && exprType alternative `eqType` result)
    let field v | v == first  = Just FirstField
                | v == second = Just SecondField
                | otherwise   = Nothing
    selected <- case Core.collectArgs alternative of
      (Core.Var chosen, []) -> SelectField <$> field chosen
      (Core.Var worker, [Core.Type left, Core.Type right, Core.Var a, Core.Var b])
        | worker == dataConWorkId con
        , left `eqType` varType a, right `eqType` varType b ->
            BuildPair <$> field a <*> field b
      _ -> Nothing
    pure (PairCase owner_binders x_owner y_owner selected)

-- | Publish the single constructor equation certified by the ordinary pair case.
publishPairCase :: Id -> PairCase -> TcM TyCon
publishPairCase owner (PairCase telescope first second selected) = do
    kind_binders <- forM telescope $ \(Bndr variable visibility) -> do
      name <- newSysName (nameOccName (varName variable))
      pure (mkNamedTyConBinder visibility (mkTyVar name liftedTypeKind))
    let quantifiers = [v | Bndr v _ <- kind_binders]
        source_variables = [v | Bndr v _ <- telescope]
        instantiate = substTyWith source_variables (mkTyVarTys quantifiers)
        first_kind = instantiate (mkTyVarTy first)
        second_kind = instantiate (mkTyVarTy second)
        pair_kind a b = mkTyConApp (tupleTyCon Boxed 2) [a, b]
        promoted_pair ka kb a b =
          mkTyConApp (promotedTupleDataCon Boxed 2) [ka, kb, a, b]
    pair <- fresh_var "p" (pair_kind first_kind second_kind)
    x <- fresh_var "a" first_kind
    y <- fresh_var "b" second_kind
    let binders = kind_binders ++ mkAnonTyConBinders [pair]
        field FirstField  = (first_kind, mkTyVarTy x)
        field SecondField = (second_kind, mkTyVarTy y)
        (result, rhs) = case selected of
          SelectField f -> field f
          BuildPair a b -> let (ka, ta) = field a
                               (kb, tb) = field b
                           in (pair_kind ka kb, promoted_pair ka kb ta tb)
        pat = promoted_pair first_kind second_kind (mkTyVarTy x) (mkTyVarTy y)
        args = mkTyVarTys quantifiers ++ [pat]
        clause_binders = quantifiers ++ [x,y]
        branch _ = mkCoAxBranch clause_binders [] [] args rhs
                       (map (const Nominal) clause_binders) (nameSrcSpan (idName owner))
    makeSourceFamily (idName owner) binders result [branch]
  where
    fresh_var occ kind = flip mkTyVar kind <$> newSysName (mkTyVarOcc occ)

{- Note [Checked pair cases]
~~~~~~~~~~~~~~~~~~~~~~~~~
A checked boxed case can publish one nominal constructor equation.
Check exported ordinary functions when finalizing the producer module.
The family and its axiom belong to that module. A dependent client can
keep a call on an unknown pair in its interface. A later client can
reduce the application when the pair constructor becomes known. Ordinary
term uses keep the original Id. The dependent client's renamer selects
the family only when ExperimentalDependentTypes is enabled there.
The two bound field types become kind parameters in that equation.
A homogeneous term type shares one kind parameter but still introduces
two independent fields. The actual case determines which fields go into the equation. Returning the exact alternative binder requires no
demand of the other field. Rebuilding a pair from the exact field binders leaves both fields lazy.
Preserve the selected order, including repeated uses of the same field. Both bodies force the
input pair to weak head normal form. With a neutral pair the family
does not choose an alternative and the equality can survive an interface.

An equation for a pair result matches only the input constructor.
In particular, a body that rebuilds the pair in its original order does
not equate a neutral pair with its fieldwise reconstruction. A case
that also forces a field retains the ordinary term entry.
-}

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
  | ConPat (ConPatTc params [] [] evs WpHole []) (L _ (RealDataCon input)) details <- unParen pat
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
    (quantified, domain, _) <- freshInput input
    args <- zipWithM argVar ["x", "y"] (domain : carried_types)
    let binders = mkNamedTyConBinders (Invisible SpecifiedSpec) quantified ++
                  mkAnonTyConBinders args
    makeSourceFamily fun binders (mkTyConTy result) branches
  where
    argVar occ ty = do { n <- newSysName (mkTyVarOcc occ); pure (mkTyVar n ty) }

-- | Build a nominal closed family from an explicit telescope and result kind.
makeSourceFamily :: Name -> [TyConBinder] -> Kind -> [TyCon -> CoAxBranch] -> TcM TyCon
makeSourceFamily fun binders res_kind branches = do
    name <- newImplicitBinder fun mkSourceTableOcc
    ax_name <- newImplicitBinder name mkInstTyCoOcc
    pure (buildSourceFamily name ax_name binders res_kind branches)

-- | Construct a checked family after its name and axiom name are allocated.
buildSourceFamily :: Name -> Name -> [TyConBinder] -> Kind -> [TyCon -> CoAxBranch] -> TyCon
buildSourceFamily name ax_name binders res_kind branches =
    let kind = mkTyConKindWithMatchability unmatchableDataConTy binders res_kind
        fam_tc = mkFamilyTyCon name kind binders 0 res_kind Nothing
                   (ClosedTypeFamilyTyCon (CTF (Just axiom))) Nothing NotInjective
        axiom = mkBranchedCoAxiom Nominal ax_name fam_tc (map ($ fam_tc) branches)
    in fam_tc
