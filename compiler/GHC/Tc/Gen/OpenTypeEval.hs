module GHC.Tc.Gen.OpenTypeEval
  ( OpenTypeFailure(..)
  , openTypeFailureReason
  , observeOpenType
  , instantiateOpenTypeFunction
  ) where

import GHC.Prelude

import qualified Data.Set as Set
import Control.Monad (foldM, guard)
import Data.List (sortBy)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import GHC.Builtin.WiredIn.Types (liftedTypeKind, manyDataConTy, mkBoxedTupleTy,
                                  promotedTupleDataCon, tupleTyCon, unmatchableDataConTy)
import GHC.Core (CoreExpr, Expr(..))
import GHC.Core.DataCon (DataCon, dataConName, dataConTyCon, dataConWorkId)
import GHC.Core.TyCon (isBoxedDataTyCon, isPromotedDataCon_maybe, isTupleTyCon, tyConName)
import GHC.Core.Make (mkCoreBoxedTuple)
import GHC.Core.Type (Kind, Type, FunTyFlag(FTF_T_T), getTyVar_maybe, isLiftedTypeKind, isManyTy,
                      mkTyConApp, mkTyVarTy, mkVisFunTyManyU, splitForAllForAllTyBinders,
                      splitFunTy_maybe, splitTyConApp_maybe, typeKind)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (fsLit)
import GHC.Data.Maybe (MaybeErr(..))
import GHC.Driver.DynFlags (HasDynFlags(getDynFlags), ghcMode, isOneShot, reductionDepth)
import GHC.Driver.Env (HscEnv, hsc_HUG, hsc_all_home_unit_ids, hsc_dflags)
import GHC.Iface.Load (findAndReadIface)
import GHC.Iface.Syntax (IfaceDecl(..), IfaceInfoItem(..), IfaceUnfolding(..))
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Rename.Env (DeprecationWarnings(NoDeprecationWarnings), addUsedGREs)
import GHC.Tc.Gen.SourceTable (groundFirstHead)
import GHC.Tc.Utils.Monad (TcM, getGblEnv, getTopEnv, initIfaceLcl, initIfaceTcRn,
                           liftIO, newTcRef, newUnique, readTcRef, tcg_imports,
                           tcg_rdr_env, tcg_semantic_mod, tcg_local_open_defs, tcg_local_open_owners, updTcRef)
import GHC.Tc.Utils.TcType (TcTyVarDetails(SkolemTv), tcEqType)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Id (Id, idName, idType, mkSysLocal)
import GHC.Types.Name (Name, getOccString, nameModule_maybe, stableNameCmp)
import GHC.Types.Name.Env (emptyNameEnv, extendNameEnv_C, lookupNameEnv, nonDetNameEnvElts)
import GHC.Types.Name.Reader (gre_imp, isLocalGRE, is_decl, is_isboot, lookupGRE_Name)
import GHC.Types.Name.Set (NameSet, emptyNameSet, isEmptyNameSet, mkNameSet, nameSetElemsStable,
                           unionNameSet, unitNameSet)
import GHC.Types.SourceFile (isHsBootOrSig)
import GHC.Types.Var (TyVar, ForAllTyFlag(Invisible), VarBndr(..), isGlobalId, isTcTyVar, isTyVar,
                      tcTyVarDetails, tyVarKind, varName)
import GHC.Types.Var.Env (emptyVarEnv, extendVarEnv, lookupVarEnv)
import GHC.Core.OpenWhnf
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.ModInfo (hm_iface)
import GHC.Unit.Module (GenWithIsBoot(..), InstalledModule, IsBootInterface(..), Module,
                        getModuleInstantiation, isHoleModule, lookupInstalledModuleEnv,
                        moduleUnit, toUnitId)
import GHC.Unit.Module.Deps (imp_boot_mods)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_hsc_src, mi_semantic_module)
import GHC.Utils.Outputable (text)

-- | An open application did not produce a checked source type.
data OpenTypeFailure
  = InvalidInput
  | UnavailableDefinition Name DefinitionFailure
  | CannotReduce EvalFailure
  | RigidCase Name
  | UnexpectedNeutral
  | HiddenConstructor Name

-- | A checked source argument and its lazy constructor fields.
data OpenArgument
  = OpenRigid TyVar
  | OpenPair TyVar TyVar
  | OpenConstructor DataCon [Type] [OpenArgument]

-- | Apply the instantiated function to symbolic operands of the checked kind.
observeOpenType :: CoreExpr -> Kind -> [Type]
                -> TcM (Either OpenTypeFailure (Type, [(Name, Maybe Name)], NameSet))
observeOpenType instantiated result_kind operands
  | Just args <- traverse checkedOperand operands = runExceptT $ do
      limit <- reductionDepth <$> lift getDynFlags
      ((_, origins), revArgs) <- lift $
        foldM buildArgument ((emptyVarEnv, emptyVarEnv), []) args
      let coreArgs = reverse revArgs
          expected = foldr (mkVisFunTyManyU . exprType) result_kind coreArgs
          application = foldl App instantiated coreArgs
      if exprType instantiated `tcEqType` expected
         && exprType application `tcEqType` result_kind
        then do
          definitions <- lift (newTcRef emptyVarEnv)
          uses <- lift (newTcRef emptyNameEnv)
          env <- lift getGblEnv
          let locals = tcg_local_open_defs env
              record name first = lift (updTcRef uses $ \seen ->
                extendNameEnv_C combine seen name (name, maybe emptyNameSet unitNameSet first))
              combine new@(name, kinds) old@(_, old_kinds)
                | isEmptyNameSet kinds = new
                | isEmptyNameSet old_kinds = old
                | otherwise = (name, kinds `unionNameSet` old_kinds)
              local v = case lookupNameEnv (tcg_local_open_owners env) (idName v) of
                Nothing -> pure ()
                Just owner -> record (idName owner) Nothing
              remember v rhs = lift (updTcRef definitions (\cache -> extendVarEnv cache v rhs)) >>
                               pure (Just rhs)
              obtain v first_type = do
                known <- lift (readTcRef definitions)
                result <- case lookupVarEnv known v of
                  Just rhs -> pure (Just rhs)
                  Nothing
                    | Just (_, rhs) <- lookupNameEnv locals (idName v) ->
                        if tcEqType (exprType rhs) (idType v)
                          then remember v rhs
                          else throwE (UnavailableDefinition (idName v) WrongTemplateType)
                    | not (isGlobalId v) -> pure Nothing
                    | otherwise -> do
                        definition <- lift (readOpenDefinition v)
                        case definition of
                          Left failure -> throwE (UnavailableDefinition (idName v) failure)
                          Right rhs -> remember v rhs
                case result of
                  Just _ -> record (idName v) (first_type >>= groundFirstHead . (:[]))
                  Nothing -> pure ()
                pure result
          let origin v = mkTyVarTy <$> lookupVarEnv origins v
              constructorGRE con = lookupGRE_Name (tcg_rdr_env env) (dataConName con)
              visible con = pure $ case constructorGRE con of
                Just gre -> isLocalGRE gre || any ((== NotBoot) . is_isboot . is_decl) (gre_imp gre)
                Nothing -> False
          outcome <- openReadbackM limit obtain local origin visible result_kind application
          case outcome of
            Left (ReadbackEvaluation failure) -> throwE (CannotReduce failure)
            Left (ReadbackHidden con) -> throwE (HiddenConstructor (tyConName (dataConTyCon con)))
            Left (ReadbackOpaque (Neutral (CaseBlocked (Rigid actual) _)))
              | Just skolem <- lookupVarEnv origins actual -> throwE (RigidCase (varName skolem))
            Left (ReadbackOpaque _) -> throwE UnexpectedNeutral
            Right (result, constructors) -> do
              dependencies <- lift (readTcRef uses)
              let ordered = sortBy (\(n, _) (m, _) -> stableNameCmp n m) (nonDetNameEnvElts dependencies)
                  flatten (n, kinds)
                    | isEmptyNameSet kinds = [(n, Nothing)]
                    | otherwise = [(n, Just k) | k <- nameSetElemsStable kinds]
                  declarations = mkNameSet
                    [ name | con <- constructors, name <- [dataConName con, tyConName (dataConTyCon con)] ]
              lift (addUsedGREs NoDeprecationWarnings
                [gre | con <- constructors, Just gre <- [constructorGRE con]])
              pure (result, concatMap flatten ordered, declarations)
        else throwE InvalidInput
  where
    rigid skolem
      | isTcTyVar skolem, SkolemTv{} <- tcTyVarDetails skolem
      = isLiftedTypeKind (typeKind (tyVarKind skolem))
      | otherwise = False

    checkedOperand ty
      | Just skolem <- getTyVar_maybe ty, rigid skolem = Just (OpenRigid skolem)
      | Just (tuple, [ka, kb, a, b]) <- splitTyConApp_maybe ty
      , tuple == promotedTupleDataCon Boxed 2
      , all (`tcEqType` liftedTypeKind) [ka, kb]
      , Just x <- getTyVar_maybe a, rigid x
      , Just y <- getTyVar_maybe b, rigid y
      , tyVarKind x `tcEqType` ka, tyVarKind y `tcEqType` kb
      = Just (OpenPair x y)
      | Just (promoted, args) <- splitTyConApp_maybe ty
      , Just con <- isPromotedDataCon_maybe promoted
      , Just (parent, parameters) <- splitTyConApp_maybe (typeKind ty)
      , parent == dataConTyCon con
      , Just field_types <- openDataConFieldTypes con parameters
      , let (prefix, fields) = splitAt (length parameters) args
      , length prefix == length parameters, length fields == length field_types
      , and (zipWith tcEqType prefix parameters)
      , and (zipWith tcEqType (map typeKind fields) field_types)
      = OpenConstructor con parameters <$> traverse checkedOperand fields
      | otherwise = Nothing

    buildArgument (state, args) operand = do
      (state', arg) <- buildOperand state operand
      pure (state', arg : args)

    buildOperand state (OpenRigid skolem) = symbolFor state skolem
    buildOperand state (OpenPair x y) = do
      (state', first) <- symbolFor state x
      (state'', second) <- symbolFor state' y
      pure (state'', mkCoreBoxedTuple [first, second])
    buildOperand state (OpenConstructor con parameters fields) = do
      (state', revFields) <- foldM buildArgument (state, []) fields
      pure (state', foldl App (Var (dataConWorkId con)) (map Type parameters ++ reverse revFields))

    symbolFor state@(symbols, origins) skolem =
      case lookupVarEnv symbols skolem of
        Just symbol -> pure (state, Var symbol)
        Nothing -> do
          unique <- newUnique
          let symbol = mkSysLocal (fsLit "open-type-operand") unique manyDataConTy (tyVarKind skolem)
          pure ((extendVarEnv symbols skolem symbol, extendVarEnv origins symbol skolem),
                Var symbol)
observeOpenType _ _ _ = pure (Left InvalidInput)

-- | A demanded external definition was unavailable to the observer.
data DefinitionFailure
  = UnavailableModule
  | CannotReadInterface
  | MissingTemplate
  | WrongTemplateType
  deriving (Eq, Show)

-- | The kind of a pair whose fields are both types.
openPairKind :: Type
openPairKind = mkBoxedTupleTy [liftedTypeKind, liftedTypeKind]

-- | Instantiate the type binders and recover the checked input and result kinds.
instantiateOpenTypeFunction :: Id -> Maybe (CoreExpr, [Kind], Kind)
instantiateOpenTypeFunction function
  | not (null binders), all liftedUniversal binders
  = do (domains, result) <- termKinds body
       guard (not (null domains))
       let instantiated = foldl (\expr _ -> App expr (Type liftedTypeKind)) (Var function) binders
       pure (instantiated, domains, result)
  | otherwise = Nothing
  where
    (binders, body) = splitForAllForAllTyBinders (idType function)
    liftedUniversal (_, Bndr v (Invisible _)) =
      isTyVar v && tyVarKind v `tcEqType` liftedTypeKind
    liftedUniversal _ = False
    bound ty = any (\(_, Bndr v _) -> ty `tcEqType` mkTyVarTy v) binders
    domainKind ty
      | bound ty = Just liftedTypeKind
      | Just (tuple, [a,b]) <- splitTyConApp_maybe ty
      , tuple == tupleTyCon Boxed 2
      , bound a, bound b = Just openPairKind
      | Just (con, parameters) <- splitTyConApp_maybe ty
      , isBoxedDataTyCon con
      , all bound parameters
      = Just (mkTyConApp con (map (const liftedTypeKind) parameters))
      | otherwise = Nothing
    termKinds ty
      | Just (FTF_T_T, matchability, multiplicity, domain, result) <- splitFunTy_maybe ty
      , matchability `tcEqType` unmatchableDataConTy
      , isManyTy multiplicity = do
          input <- domainKind domain
          (inputs, output) <- termKinds result
          pure (input : inputs, output)
      | bound ty = Just ([], liftedTypeKind)
      | Just (tuple, [a,b]) <- splitTyConApp_maybe ty
      , tuple == tupleTyCon Boxed 2
      , bound a, bound b = Just ([], openPairKind)
      | Just (con, parameters) <- splitTyConApp_maybe ty
      , isBoxedDataTyCon con, not (isTupleTyCon con), all bound parameters
      = Just ([], mkTyConApp con (map (const liftedTypeKind) parameters))
      | otherwise = Nothing

-- | Read the checked expression for one demanded ordinary import.
-- Use a full interface because EPS may discard the optimizer's IdInfo.
-- The caller records the Id Name when it consumes the returned definition.
readOpenDefinition :: Id -> TcM (Either DefinitionFailure CoreExpr)
readOpenDefinition identifier = do
  hscEnv <- getTopEnv
  tcgEnv <- getGblEnv
  let requested = idName identifier
      reason = text "open term definition"
  case nameModule_maybe requested of
    Just mod
      | mod /= tcg_semantic_mod tcgEnv
      , not (isHoleModule mod)
      , (installed, Nothing) <- getModuleInstantiation mod
      , not (isBootImport tcgEnv installed) -> do
          readResult <- liftIO (openDefinitionIface hscEnv mod installed)
          case readResult of
            Left failure -> pure (Left failure)
            Right iface
              | isHsBootOrSig (mi_hsc_src iface) -> pure (Left UnavailableModule)
              | otherwise ->
                  case [ template
                       | (_, IfaceId { ifName = name, ifIdInfo = info }) <- mi_decls iface
                       , name == requested
                       , HsUnfold _ (IfCoreUnfold _ _ _ template) <- info
                       ] of
                    [template] -> do
                      body <- initIfaceTcRn $
                        initIfaceLcl (mi_semantic_module iface) reason NotBoot (tcIfaceExpr template)
                      pure $ if tcEqType (exprType body) (idType identifier)
                        then Right body
                        else Left WrongTemplateType
                    _ -> pure (Left MissingTemplate)
    _ -> pure (Left UnavailableModule)
  where
    isBootImport env installed =
      case lookupInstalledModuleEnv (imp_boot_mods (tcg_imports env)) installed of
        Just dep -> gwib_isBoot dep == IsBoot
        Nothing -> False

-- | Read a complete interface without repopulating the external package table.
-- The compilation manager already has complete interfaces for its loaded
-- home modules. Do not reopen unloaded home modules from disk in that mode.
openDefinitionIface :: HscEnv -> Module -> InstalledModule -> IO (Either DefinitionFailure ModIface)
openDefinitionIface hscEnv mod installed = do
  fromHome <- HUG.lookupHugByModule mod (hsc_HUG hscEnv)
  case fromHome of
    Just hmi -> pure (Right (hm_iface hmi))
    Nothing
      | not (isOneShot (ghcMode (hsc_dflags hscEnv)))
      , toUnitId (moduleUnit mod) `Set.member` hsc_all_home_unit_ids hscEnv ->
          pure (Left UnavailableModule)
      | otherwise -> do
          loaded <- findAndReadIface hscEnv (text "open term definition") installed mod NotBoot
          case loaded of
            Failed _ -> pure (Left CannotReadInterface)
            Succeeded (iface, _) -> pure (Right iface)

-- | Describe why a candidate cannot elaborate to a type.
openTypeFailureReason :: OpenTypeFailure -> (Maybe Name, String)
openTypeFailureReason InvalidInput =
  (Nothing, "Use rigid variables or ordinary boxed constructors with lazy fields at the declared input kinds.")
openTypeFailureReason (UnavailableDefinition name failure) =
  (Just name, case failure of
    UnavailableModule -> "A referenced definition does not come from an ordinary imported module."
    CannotReadInterface -> "The interface for a referenced definition could not be read."
    MissingTemplate -> "The interface for a referenced definition contains no usable Core unfolding."
    WrongTemplateType -> "The type of a referenced unfolding disagrees with its declared type.")
openTypeFailureReason (CannotReduce failure) =
  (Nothing, case failure of
    Exhausted -> "Reduction exceeded the open evaluation step limit (-freduction-depth)."
    UnsupportedCore -> "Reduction reached an unsupported Core expression."
    MissingAlternative -> "Reduction reached a case without a matching alternative.")
openTypeFailureReason (RigidCase scrutinee) =
  (Nothing, "Reduction stopped at a case on the rigid argument " ++ getOccString scrutinee ++ ".")
openTypeFailureReason (HiddenConstructor name) =
  (Nothing, "A result constructor of " ++ getOccString name ++ " is not in scope through an ordinary import.")
openTypeFailureReason UnexpectedNeutral =
  (Nothing, "Reduction did not produce a supplied rigid argument or a finite constructor value built from supported fields.")

{- Note [Open source calls]
~~~~~~~~~~~~~~~~~~~~~~~~~
Serokell's open evaluator example applies a source function to a rigid
type variable. GHC #27413 describes observation with unknowns. Start by
observing an ordinary Core definition at its declared input kinds.
Admit a lifted invisible forall telescope and ordinary saturated arrows.
An input can use a quantified variable or a boxed pair of them. The
result must use a quantified variable. Instantiate each parameter at Type.
Construct a symbolic pair only when the source supplies a pair constructor.
A variable of pair kind stays neutral. A projection on it must block.
Use a fresh symbolic Id per distinct skolem in the observer only.
Check the application's result kind and reify the returned Id by identity.
The observer emits no residual Core expression or conversion proof.

At -O0, interface loading normally ignores optimizer pragmas. Read the
needed template from the complete interface when the observer demands its
Id. The package interface table can already have dropped mi_decls. Keep
this read separate from optimizer configuration. Record the Name of every
template used, including aliases. Record a local definition when it is
unfolded, too. Scheduling a local argument makes its body available,
but a lazy function can ignore it. Only the bodies the observer used
supply dependencies for the source RULE check. The compiler's usual interface usage
fingerprints include these declaration dependencies. The observer charges
each observation step against the same configurable limit used for type
reduction. A zero command-line value disables the limit, as in the solver.
-}
