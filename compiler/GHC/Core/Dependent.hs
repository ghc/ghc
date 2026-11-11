-- | Detect native dependent types at unsupported compiler boundaries.
module GHC.Core.Dependent (
    hasNativeDependentProgram,
    hasNativeDependentExpr,
    rejectNativeDependentProgram,
    rejectNativeDependentExpr,
) where

import GHC.Prelude

import GHC.Core
import GHC.Core.DataCon (dataConRepType)
import GHC.Core.TyCo.Rep
import GHC.Types.Id (idCoreRules, realIdUnfolding)
import GHC.Types.Literal (literalType)
import GHC.Types.Tickish (CoreTickish, GenTickish (..))
import GHC.Types.Var
import GHC.Types.Var.Set
import GHC.Utils.Panic

import Control.Monad (when)
import Control.Monad.Trans.State.Strict
import Data.Semigroup (Semigroup (..))

-- | Track metadata and variable kinds that the scan has already visited.
data ScanState = ScanState
    { scannedMetadata :: VarSet
    , scannedKinds :: VarSet
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

-- | Run a scan with empty metadata and kind caches.
runScan :: NativeScan -> Bool
runScan scan = evalState (runNativeScan scan) (ScanState emptyVarSet emptyVarSet)

-- | Report an unsupported compiler operation when a scan finds a native type.
rejectNative :: String -> Bool -> IO ()
rejectNative context found =
    when found
        $ throwGhcExceptionIO
            ( Sorry
                ( "Native dependent Core cannot enter "
                    ++ context
                    ++ ". Optimization and code generation for DepFunTy/TermVarTy are not implemented."
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

-- | Inspect a type structurally, including closed dependent binders.
scanType :: Type -> NativeScan

-- | Inspect coercion annotations and dependencies.
scanCo :: Coercion -> NativeScan
(scanType, _, scanCo, _) =
    foldTyCo
        TyCoFolder
            { tcf_view = noView
            , tcf_lit = const mempty
            , tcf_tyvar = scanVarKind
            , tcf_covar = scanVarKind
            , tcf_hole = scanVarKind . coHoleCoVar
            , tcf_tycobinder = \_ body -> body
            , tcf_termvar = \_ -> NativeScan (pure True)
            , tcf_termbinder = \_ _ -> NativeScan (pure True)
            }
