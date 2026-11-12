-- | Detect native dependent types at unsupported compiler boundaries.
module GHC.Core.Dependent (
    hasNativeDependentProgram,
    hasNativeDependentExpr,
    rejectNativeDependentProgram,
    rejectNativeDependentExpr,
) where

import GHC.Prelude

import GHC.Core
import GHC.Builtin.KnownKeys (falseDataConKey, trueDataConKey)
import GHC.Core.DataCon (dataConRepType)
import GHC.Core.TyCo.Rep
import GHC.Core.TyCon (TyCon, synTyConRhs_maybe)
import GHC.Core.TyCon.Set
import GHC.Types.Id (idCoreRules, realIdUnfolding)
import GHC.Types.Literal (literalType)
import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.Var
import GHC.Types.Unique (hasKey)
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
    }

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
    runScan (foldMap scanBind binds <> foldMap scanRule rules)

-- | Inspect an expression and metadata on its bound identifiers.
hasNativeDependentExpr :: CoreExpr -> Bool
hasNativeDependentExpr = runScan . scanExpr

-- | Reject native dependent Core before an unsupported program pass.
rejectNativeDependentProgram :: String -> CoreProgram -> [CoreRule] -> IO ()
rejectNativeDependentProgram context binds rules =
    rejectNative context (hasNativeDependentProgram binds rules)

-- | Reject native dependent Core before an unsupported expression pass.
rejectNativeDependentExpr :: String -> CoreExpr -> IO ()
rejectNativeDependentExpr context expr =
    rejectNative context (hasNativeDependentExpr expr)

-- | Run a scan with empty metadata, kind, and synonym caches.
runScan :: NativeScan -> Bool
runScan scan = evalState (runNativeScan scan) (ScanState emptyVarSet emptyVarSet emptyTyConSet)

-- | Report an unsupported compiler operation when a scan finds a native type.
rejectNative :: String -> Bool -> IO ()
rejectNative context found =
    when found
        $ throwGhcExceptionIO
            ( Sorry
                ( "Native dependent Core cannot enter "
                    ++ context
                    ++ ". This operation does not support retained Boolean types or branch proofs."
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
scanAlt (Alt (DataAlt con) (_ : _) _)
    | con `hasKey` falseDataConKey || con `hasKey` trueDataConKey = NativeScan (pure True)
scanAlt (Alt con bs rhs) =
    scanAltCon con <> foldMap scanBinder bs <> scanExpr rhs

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
                if not (isNonCoVarId v) || v `elemVarSet` seen
                    then pure False
                    else do
                        modify' (\s -> s{scannedMetadata = extendVarSet seen v})
                        runNativeScan (foldMap scanRule (idCoreRules v) <> scanUnfolding (realIdUnfolding v))
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
scanUnfolding (CoreUnfolding{uf_tmpl = body}) = scanExpr body

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
scanType (TermVarTy _) = NativeScan (pure True)
scanType (DepFunTy _ _) = NativeScan (pure True)
scanType (LitTy _) = mempty
scanType (TyConApp tc args) = foldMap scanType args <> scanSynonym tc
scanType (AppTy fun arg) = scanType fun <> scanType arg
scanType (FunTy _ mult arg res) = scanType arg <> scanType mult <> scanType res
scanType (ForAllTy (Bndr var _) body) = scanType (varType var) <> scanType body
scanType (CastTy ty co) = scanType ty <> scanCo co
scanType (CoercionTy co) = scanCo co

-- | Inspect coercion annotations and dependencies.
scanCo :: Coercion -> NativeScan
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
scanCo (FunCo{fco_mult = mult, fco_arg = arg, fco_res = res}) =
    scanCo mult <> scanCo arg <> scanCo res
scanCo (ForAllCo{fco_tcv = var, fco_kind = kind, fco_body = body}) =
    scanMCo kind <> scanType (varType var) <> scanCo body

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
