module GHC.Tc.Gen.OpenTypeEval
  ( OpenTypeFailure(..)
  , openTypeFailureReason
  , observeOpenType
  , instantiateOpenTypeFunction
  ) where

import GHC.Prelude

import qualified Data.Set as Set
import Control.Monad (foldM, guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (runExceptT, throwE)
import GHC.Builtin.WiredIn.Types (liftedTypeKind, manyDataConTy, mkBoxedTupleTy,
                                  promotedTupleDataCon, tupleTyCon, unmatchableDataConTy)
import GHC.Core (CoreExpr, Expr(..))
import GHC.Core.Make (mkCoreBoxedTuple)
import GHC.Core.Type (Type, FunTyFlag(FTF_T_T), getTyVar_maybe, isManyTy, mkTyVarTy, mkVisFunTyManyU,
                      splitForAllForAllTyBinders, splitFunTy_maybe, splitTyConApp_maybe)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (fsLit)
import GHC.Data.Maybe (MaybeErr(..))
import GHC.Driver.DynFlags (HasDynFlags(getDynFlags), ghcMode, isOneShot, reductionDepth)
import GHC.Driver.Env (HscEnv, hsc_HUG, hsc_all_home_unit_ids, hsc_dflags)
import GHC.Iface.Load (findAndReadIface)
import GHC.Iface.Syntax (IfaceDecl(..), IfaceInfoItem(..), IfaceUnfolding(..))
import GHC.IfaceToCore (tcIfaceExpr)
import GHC.Tc.Utils.Monad (TcM, getGblEnv, getTopEnv, initIfaceLcl, initIfaceTcRn,
                           liftIO, newTcRef, newUnique, readTcRef, tcg_imports,
                           tcg_semantic_mod, tcg_local_open_defs, updTcRef)
import GHC.Tc.Utils.TcType (TcTyVarDetails(SkolemTv), tcEqType)
import GHC.Types.Basic (Boxity(Boxed))
import GHC.Types.Id (Id, idName, idType, mkSysLocal)
import GHC.Types.Name (Name, getOccString, nameModule_maybe)
import GHC.Types.Name.Env (lookupNameEnv)
import GHC.Types.Name.Set (NameSet, emptyNameSet, extendNameSet)
import GHC.Types.SourceFile (isHsBootOrSig)
import GHC.Types.Var (ForAllTyFlag(Invisible), VarBndr(..), isGlobalId, isTcTyVar, isTyVar,
                      tcTyVarDetails, tyVarKind, varName)
import GHC.Types.Var.Env (emptyVarEnv, extendVarEnv, lookupVarEnv)
import GHC.Types.Var.Set (emptyVarSet)
import GHC.Core.OpenWhnf
import qualified GHC.Unit.Home.Graph as HUG
import GHC.Unit.Home.ModInfo (hm_iface)
import GHC.Unit.Module (GenWithIsBoot(..), InstalledModule, IsBootInterface(..), Module,
                        getModuleInstantiation, isHoleModule, lookupInstalledModuleEnv,
                        moduleUnit, toUnitId)
import GHC.Unit.Module.Deps (imp_boot_mods)
import GHC.Unit.Module.ModIface (ModIface, mi_decls, mi_hsc_src, mi_semantic_module)
import GHC.Utils.Outputable (text)

-- | An open application did not produce a checked rigid type.
data OpenTypeFailure
  = InvalidInput
  | UnavailableDefinition Name DefinitionFailure
  | CannotReduce EvalFailure
  | RigidCase Name
  | UnexpectedNeutral

-- | Apply the instantiated function to symbolic operands of the checked kind.
observeOpenType :: CoreExpr -> [Type] -> TcM (Either OpenTypeFailure (Type, NameSet))
observeOpenType instantiated operands
  | Just args <- traverse checkedOperand operands = runExceptT $ do
      limit <- reductionDepth <$> lift getDynFlags
      ((_, origins), revArgs) <- lift $
        foldM buildArgument ((emptyVarEnv, emptyVarEnv), []) args
      let coreArgs = reverse revArgs
          expected = foldr (mkVisFunTyManyU . exprType) liftedTypeKind coreArgs
          application = foldl App instantiated coreArgs
      if exprType instantiated `tcEqType` expected
         && exprType application `tcEqType` liftedTypeKind
        then do
          readDefinitions <- lift (newTcRef (emptyVarEnv, emptyNameSet))
          locals <- lift (tcg_local_open_defs <$> getGblEnv)
          let remember v rhs = do
                lift (updTcRef readDefinitions $ \(cache, used) ->
                  (extendVarEnv cache v rhs, extendNameSet used (idName v)))
                pure (Just rhs)
              obtain v = do
                (known, _) <- lift (readTcRef readDefinitions)
                case lookupVarEnv known v of
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
          outcome <- openWhnfM limit obtain emptyVarSet application
          case outcome of
            Left failure -> throwE (CannotReduce failure)
            Right (Neutral (Rigid actual))
              | Just skolem <- lookupVarEnv origins actual
              , idType actual `tcEqType` liftedTypeKind -> do
                  (_, dependencies) <- lift (readTcRef readDefinitions)
                  pure (mkTyVarTy skolem, dependencies)
            Right (Neutral (CaseBlocked (Rigid actual) _))
              | Just skolem <- lookupVarEnv origins actual ->
                  throwE (RigidCase (varName skolem))
            _ -> throwE UnexpectedNeutral
        else throwE InvalidInput
  where
    rigidAt kind skolem
      | isTcTyVar skolem
      , SkolemTv{} <- tcTyVarDetails skolem
      = tyVarKind skolem `tcEqType` kind
      | otherwise = False

    checkedOperand ty
      | Just skolem <- getTyVar_maybe ty
      , rigidAt liftedTypeKind skolem || rigidAt openPairKind skolem = Just (Left skolem)
      | Just (tuple, [ka, kb, a, b]) <- splitTyConApp_maybe ty
      , tuple == promotedTupleDataCon Boxed 2
      , all (`tcEqType` liftedTypeKind) [ka, kb]
      , Just x <- getTyVar_maybe a, rigidAt liftedTypeKind x
      , Just y <- getTyVar_maybe b, rigidAt liftedTypeKind y
      = Just (Right (x, y))
      | otherwise = Nothing

    buildArgument (state, args) (Left skolem) = do
      (state', arg) <- symbolFor state skolem
      pure (state', arg : args)
    buildArgument (state, args) (Right (x, y)) = do
      (state', first) <- symbolFor state x
      (state'', second) <- symbolFor state' y
      pure (state'', mkCoreBoxedTuple [first, second] : args)

    symbolFor state@(symbols, origins) skolem =
      case lookupVarEnv symbols skolem of
        Just symbol -> pure (state, Var symbol)
        Nothing -> do
          unique <- newUnique
          let symbol = mkSysLocal (fsLit "open-type-operand") unique manyDataConTy (tyVarKind skolem)
          pure ((extendVarEnv symbols skolem symbol, extendVarEnv origins symbol skolem),
                Var symbol)
observeOpenType _ _ = pure (Left InvalidInput)

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

-- | Instantiate the type binders and recover the input kinds of the term arrows.
instantiateOpenTypeFunction :: Id -> Maybe (CoreExpr, [Type])
instantiateOpenTypeFunction function
  | not (null binders), all liftedUniversal binders
  = do domains <- termDomains body
       guard (not (null domains))
       pure (foldl (\expr _ -> App expr (Type liftedTypeKind)) (Var function) binders, domains)
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
      | otherwise = Nothing
    termDomains ty
      | Just (FTF_T_T, matchability, multiplicity, domain, result) <- splitFunTy_maybe ty
      , matchability `tcEqType` unmatchableDataConTy
      , isManyTy multiplicity = (:) <$> domainKind domain <*> termDomains result
      | bound ty = Just []
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
  (Nothing, "The arguments must be rigid variables or boxed pairs of rigid type variables at the declared input kinds.")
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
openTypeFailureReason UnexpectedNeutral =
  (Nothing, "Reduction did not return any of the supplied rigid arguments.")

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
