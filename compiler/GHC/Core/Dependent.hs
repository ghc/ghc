-- | Detect native dependent types in Core and its metadata.
module GHC.Core.Dependent (
    hasNativeDependentProgram,
    supportsNativeGentle,
    hasNativeDependentExpr,
    hasNativeDependentExprRhs,
    hasNativeDependentUnfolding,
    rejectNativeDependentProgram,
    rejectNativeDependentExpr,
) where

import GHC.Prelude

import GHC.Core
import GHC.Core.Type (isNativeIndexType, isNativeIndexFunctionId, isNativeOpaqueValueId)
import GHC.Core.DataCon (dataConRepType)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon (TyCon, synTyConRhs_maybe)
import GHC.Core.TyCon.Set
import GHC.Types.Id (idCoreRules, realIdUnfolding)
import GHC.Types.Literal (literalType)
import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.Var
import GHC.Types.Unique.DSet (uniqDSetToList)
import GHC.Types.Var.Set
import GHC.Utils.Panic

import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Data.Semigroup (Semigroup (..))

-- | Track metadata, variable kinds, and synonyms that the scan has visited.
data ScanState = ScanState
    { scannedMetadata :: VarSet
    , scannedKinds :: VarSet
    , scannedSynonyms :: TyConSet
    , scanMetadata :: MetadataScan
    , scanPurpose :: !ScanPurpose
    }

-- | Choose whether a scan detects native forms or checks the Gentle subset.
data ScanPurpose = DetectNative | CheckGentle

-- | Select which bound-Id metadata a boundary can demand.
data MetadataScan = AllMetadata | StableMetadata | NoMetadata
    deriving Eq

-- | Short-circuit a structural scan when it finds a native type.
newtype NativeScan = NativeScan {runNativeScan :: State ScanState Bool}

instance Semigroup NativeScan where
    NativeScan left <> NativeScan right = NativeScan $ do
        found <- left
        if found then pure True else right

instance Monoid NativeScan where
    mempty = NativeScan (pure False)

{- | Inspect bindings, rules, and metadata on bound identifiers.
Do not follow unfoldings attached to variable occurrences.
-}
hasNativeDependentProgram :: CoreProgram -> [CoreRule] -> Bool
hasNativeDependentProgram binds rules =
    runScan AllMetadata (foldMap scanBind binds <> foldMap scanRule rules)

-- | Admit algebraic index domains and checked opaque function heads.
-- Retained casts with a domain proof and local rules use the restricted pipeline.
supportsNativeGentle :: CoreProgram -> Bool
supportsNativeGentle binds =
    not (runScanFor CheckGentle AllMetadata (foldMap scanBind binds))

-- | Inspect an expression and metadata on its bound identifiers.
hasNativeDependentExpr :: CoreExpr -> Bool
hasNativeDependentExpr = runScan AllMetadata . scanExpr

{- | Inspect one unfolding RHS without forcing bound-Id metadata.
Each nested unfolding has its own deferred validation. See
Note [Native dependent unfolding templates] in GHC.Core.Unfold.Make.
-}
hasNativeDependentExprRhs :: CoreExpr -> Bool
hasNativeDependentExprRhs = runScan NoMetadata . scanExpr

{- | Inspect an RHS and the metadata that occurrence analysis would traverse.
Do not force unstable nested unfolding templates.
-}
hasNativeDependentUnfolding :: CoreExpr -> Bool
hasNativeDependentUnfolding = runScan StableMetadata . scanExpr

-- | Reject native dependent Core before an unsupported program pass.
rejectNativeDependentProgram :: String -> CoreProgram -> [CoreRule] -> IO ()
rejectNativeDependentProgram context binds rules =
    rejectNative context (hasNativeDependentProgram binds rules)

-- | Reject native dependent Core before an unsupported expression pass.
rejectNativeDependentExpr :: String -> CoreExpr -> IO ()
rejectNativeDependentExpr context expr =
    rejectNative context (hasNativeDependentExpr expr)

-- | Run a scan with empty metadata, kind, and synonym caches.
runScan :: MetadataScan -> NativeScan -> Bool
runScan = runScanFor DetectNative

runScanFor :: ScanPurpose -> MetadataScan -> NativeScan -> Bool
runScanFor purpose metadata scan =
    evalState (runNativeScan scan)
        (ScanState emptyVarSet emptyVarSet emptyTyConSet metadata purpose)

-- | A native form qualifies for Gentle only if its children and type qualify.
gentleForm :: NativeScan -> NativeScan
gentleForm scan = NativeScan $ do
    purpose <- gets scanPurpose
    case purpose of
        CheckGentle -> runNativeScan scan
        DetectNative -> pure True

-- | Report an unsupported compiler operation when a scan finds a native type.
rejectNative :: String -> Bool -> IO ()
rejectNative context found =
    when found
        $ throwGhcExceptionIO
            ( Sorry
                ( "Native dependent Core cannot enter "
                    ++ context
                    ++ ". This operation does not support retained dependent types or branch proofs."
                )
            )

-- | Inspect expression annotations without following occurrence unfoldings.
scanExpr :: CoreExpr -> NativeScan
scanExpr (Var v) = scanAnnotations v
scanExpr (Lit lit) = scanType (literalType lit)
scanExpr (App fun arg) = scanExpr fun <> scanExpr arg
scanExpr (Lam b body) = scanBinder b <> scanExpr body
scanExpr (Let bind body) = scanBind bind <> scanExpr body
scanExpr (Case scrut b result alts) =
    scanExpr scrut <> scanBinder b <> scanType result <> foldMap scanAlt alts
scanExpr (Cast body co) = scanExpr body <> scanCo co
scanExpr (Tick tick body) = scanTick tick <> scanExpr body
scanExpr (Type ty) = scanType ty
scanExpr (Coercion co) = scanCo co

-- | Inspect binding annotations, metadata, and right-hand sides.
scanBind :: CoreBind -> NativeScan
scanBind (NonRec b rhs) = scanBinder b <> scanExpr rhs
scanBind (Rec pairs) = foldMap (\(b, rhs) -> scanBinder b <> scanExpr rhs) pairs

-- | Inspect a case alternative and its binders.
scanAlt :: CoreAlt -> NativeScan
scanAlt alt@(Alt con bs rhs)
    | isNativeIndexAliasAlt alt = gentleForm children
    | isNativeIndexAlt alt = gentleForm children
    | otherwise = children
  where
    children = scanAltCon con <> foldMap scanBinder bs <> scanExpr rhs

-- | Inspect the type of a case alternative constructor.
scanAltCon :: AltCon -> NativeScan
scanAltCon DEFAULT = mempty
scanAltCon (LitAlt lit) = scanType (literalType lit)
scanAltCon (DataAlt con) = scanType (dataConRepType con)

-- | Inspect annotations on breakpoint variables.
scanTick :: CoreTickish -> NativeScan
scanTick (Breakpoint _ _ ids) = foldMap scanAnnotations ids
scanTick _ = mempty

-- | Inspect a variable's type and multiplicity.
scanAnnotations :: Var -> NativeScan
scanAnnotations v = scanType (varType v) <> foldMap scanType (varMultMaybe v)

-- | Inspect bound-identifier metadata once per identifier.
scanBinder :: Var -> NativeScan
scanBinder v =
    scanAnnotations v
        <> NativeScan
            ( do
                seen <- gets scannedMetadata
                metadata <- gets scanMetadata
                if metadata == NoMetadata || not (isNonCoVarId v) || v `elemVarSet` seen
                    then pure False
                    else do
                        modify' (\s -> s{scannedMetadata = extendVarSet seen v})
                        purpose <- gets scanPurpose
                        let rules = idCoreRules v
                        case purpose of
                            CheckGentle | not (null rules) -> pure True
                            _ -> runNativeScan (foldMap scanRule rules <> scanUnfolding (realIdUnfolding v))
            )

-- | Inspect the declarative part of a rule.
scanRule :: CoreRule -> NativeScan
scanRule BuiltinRule{} = mempty
scanRule (Rule{ru_bndrs = bs, ru_args = args, ru_rhs = rhs}) =
    foldMap scanBinder bs <> foldMap scanExpr args <> scanExpr rhs

-- | Inspect an unfolding attached to a bound identifier.
scanUnfolding :: Unfolding -> NativeScan
scanUnfolding NoUnfolding = mempty
scanUnfolding BootUnfolding = mempty
scanUnfolding (OtherCon cons) = foldMap scanAltCon cons
scanUnfolding (DFunUnfolding{df_bndrs = bs, df_con = con, df_args = args}) =
    foldMap scanBinder bs <> scanType (dataConRepType con) <> foldMap scanExpr args
scanUnfolding (CoreUnfolding{uf_src = src, uf_tmpl = body}) = NativeScan $ do
    metadata <- gets scanMetadata
    if metadata == AllMetadata || isStableSource src
        then runNativeScan (scanExpr body)
        else pure False

-- | Inspect a referenced variable's annotations once.
scanVarKind :: Var -> NativeScan
scanVarKind v = NativeScan $ do
    seen <- gets scannedKinds
    if v `elemVarSet` seen
        then pure False
        else do
            modify' (\s -> s{scannedKinds = extendVarSet seen v})
            runNativeScan (scanAnnotations v)

-- | Inspect each synonym body once. Application arguments are scanned separately.
-- Substitution can add native forms only through those arguments.
-- Inspect the uninstantiated body to avoid duplicating argument scans.
scanSynonym :: TyCon -> NativeScan
scanSynonym tc
    | Just rhs <- synTyConRhs_maybe tc = NativeScan $ do
        seen <- gets scannedSynonyms
        if tc `elemTyConSet` seen
            then pure False
            else do
                modify' (\s -> s{scannedSynonyms = extendTyConSet seen tc})
                runNativeScan (scanType rhs)
    | otherwise = mempty

-- | Inspect syntax and synonym bodies, including unused synonym arguments.
scanType :: Type -> NativeScan
scanType (TyVarTy tv) = scanVarKind tv
scanType (TermVarTy v) =
    gentleForm (NativeScan (pure (not (isNativeOpaqueValueId v))) <> scanVarKind v)
scanType (TermAppTy f arg) = gentleForm (scanCallHead f <> scanType arg)
scanType (DepFunTy _ b body) =
    gentleForm (scanRetainedIndex b <> scanType body)
scanType (LitTy _) = mempty
scanType (TyConApp tc args) = foldMap scanType args <> scanSynonym tc
scanType (AppTy fun arg) = scanType fun <> scanType arg
scanType (FunTy _ ma mult arg res) =
    scanType ma <> scanType arg <> scanType mult <> scanType res
scanType (ForAllTy ma (Bndr var _) body) =
  scanType ma <> scanType (varType var) <> scanType body
scanType (CastTy ty co) = scanType ty <> scanCo co
scanType (CoercionTy co) = scanCo co

-- | Check the existing algebraic and opaque function index contracts.
scanRetainedIndex :: Var -> NativeScan
scanRetainedIndex v = NativeScan (pure (not (isNativeIndexType (varType v) || isNativeIndexFunctionId v)))

-- | Check the head and its annotation without examining the function body.
scanCallHead :: Id -> NativeScan
scanCallHead f = NativeScan (pure (not (isNativeIndexFunctionId f))) <> scanVarKind f

-- | Inspect coercion annotations and dependencies.
scanCo :: Coercion -> NativeScan
scanCo (DepFunCo _ _ b MRefl body) =
    gentleForm (scanRetainedIndex b <> scanCo body)
scanCo DepFunCo{} = NativeScan (pure True)
scanCo (TermAppCo f co) = gentleForm (scanCallHead f <> scanCo co)
scanCo (Refl ty) = scanType ty
scanCo (GRefl _ ty mco) = scanType ty <> scanMCo mco
scanCo (TyConAppCo _ _ args) = foldMap scanCo args
scanCo (AppCo fun arg) = scanCo fun <> scanCo arg
scanCo (CoVarCo cv) = scanVarKind cv
scanCo (AxiomCo _ args) = foldMap scanCo args
scanCo (HoleCo hole) = scanVarKind (coHoleCoVar hole)
scanCo (SymCo co) = scanCo co
scanCo (TransCo left right) = scanCo left <> scanCo right
scanCo (SelCo _ co) = scanCo co
scanCo (LRCo _ co) = scanCo co
scanCo (InstCo co arg) = scanCo co <> scanCo arg
scanCo (KindCo co) = scanCo co
scanCo (SubCo co) = scanCo co
scanCo (UnivCo{uco_lty = left, uco_rty = right, uco_deps = deps}) =
    scanType left <> scanType right <> scanDeps deps
scanCo (FunCo{fco_ma = ma, fco_mult = mult, fco_arg = arg, fco_res = res}) =
    scanCo ma <> scanCo mult <> scanCo arg <> scanCo res
scanCo (ForAllCo{fco_tcv = var, fco_ma = ma, fco_kind = kind, fco_body = body}) =
    scanCo ma <> scanMCo kind <> scanType (varType var) <> scanCo body

-- | Inspect an optional coercion.
scanMCo :: MCoercion -> NativeScan
scanMCo MRefl = mempty
scanMCo (MCo co) = scanCo co

-- | Inspect dependencies carried by a universal coercion.
scanDeps :: UnivCoDeps -> NativeScan
scanDeps NoUnivCoDeps = mempty
scanDeps (UnivCoDeps{ucd_cvs = cvs, ucd_holes = holes}) =
    foldMap scanVarKind (dVarSetElems cvs)
        <> foldMap (scanVarKind . coHoleCoVar) (uniqDSetToList holes)
