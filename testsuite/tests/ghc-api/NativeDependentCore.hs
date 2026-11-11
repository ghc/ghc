{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import GHC (DynFlags, getSessionDynFlags, runGhc)
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion (coercionLKind, mkNomReflCo, mkPiCo)
import GHC.Core.Coercion.Axiom (Role (..))
import GHC.Core.DataCon (dataConWorkId, promoteDataCon)
import GHC.Core.Dependent
import GHC.Core.Lint (lintExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
import GHC.Core.TyCo.Rep (Type (..))
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.Type
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Ppr (showSDoc)
import GHC.Types.Id
import GHC.Types.Id.Info (RuleInfo (..))
import GHC.Types.InlinePragma (ActivationX (AlwaysActive))
import GHC.Types.Name (mkSystemName, nameModule)
import GHC.Types.Name.Occurrence (mkTyVarOcc)
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Utils.Outputable (ppr, vcat)
import System.Environment (getArgs)
import System.Timeout (timeout)

-- | Run the native Core API checks against the supplied GHC library directory.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir] -> do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            runChecks dflags
            putStrLn "native-core-api-ok"
        _ -> fail "Usage: NativeDependentCore GHC_LIBDIR"

-- | Create a stable test unique without a global unique supply.
unique :: Int -> Unique
unique = mkUniqueGrimilyWithTag 'N' . fromIntegral

-- | Create a local unrestricted identifier for a test.
local :: Int -> String -> Type -> Id
local n label = mkSysLocal (fsLit label) (unique n) ManyTy

-- | Build a lifted result that retains the Boolean index in a dictionary type.
indexed :: Type -> Type
indexed index = mkFunctionType ManyTy (mkTyConApp eqTyCon [boolTy, index, index]) intTy

-- | Build an identity function at a supplied type.
identity :: Int -> Type -> CoreExpr
identity n ty = let x = local n "x" ty in Lam x (Var x)

-- | Build the native retained keep function.
keep :: Id -> Int -> CoreExpr
keep b n = Lam b (identity n (indexed (TermVarTy b)))

-- | Build the expected ordinary result of applying keep to an index.
keepResult :: Type -> Type
keepResult index = mkFunctionType ManyTy (indexed index) (indexed index)

-- | Construct an index substitution with an explicit scope.
indexSubst :: [Var] -> Id -> CoreExpr -> Subst.Subst
indexSubst scope b rhs =
    Subst.Subst (mkInScopeSet (mkVarSet scope)) (unitVarEnv b rhs) emptyVarEnv emptyVarEnv

-- | Assert a semantic property and identify its failure precisely.
check :: String -> Bool -> IO ()
check label condition = unless condition (fail ("FAIL: " ++ label))

-- | Compare recovered types and print both sides on failure.
checkType :: DynFlags -> String -> Type -> Type -> IO ()
checkType dflags label expected actual = do
    equal <- evaluate (eqType expected actual)
    unless equal $
        fail
            ( "FAIL: "
                ++ label
                ++ "\nexpected: "
                ++ showSDoc dflags (ppr expected)
                ++ "\nactual: "
                ++ showSDoc dflags (ppr actual)
            )

-- | Require Core Lint to accept an expression without an exception.
lintAccepts :: DynFlags -> String -> [Var] -> CoreExpr -> IO ()
lintAccepts dflags label scope expr = do
    result <- try @SomeException (evaluate (lintExpr (initLintConfig dflags scope) expr))
    case result of
        Left err -> fail ("FAIL: " ++ label ++ "; Lint threw: " ++ displayException err)
        Right Nothing -> pure ()
        Right (Just errors) ->
            fail
                ( "FAIL: "
                    ++ label
                    ++ "; Lint rejected:\n"
                    ++ showSDoc dflags (vcat (bagToList errors))
                )

-- | Require a Lint error, rather than a panic, for malformed Core.
lintRejects :: DynFlags -> String -> [Var] -> CoreExpr -> IO ()
lintRejects dflags label scope expr = do
    result <- try @SomeException (evaluate (lintExpr (initLintConfig dflags scope) expr))
    case result of
        Left err -> fail ("FAIL: " ++ label ++ "; Lint panicked instead of rejecting: " ++ displayException err)
        Right Nothing -> fail ("FAIL: " ++ label ++ "; malformed Core passed Lint")
        Right (Just _) -> pure ()

-- | Require an explicit rejection with the expected diagnostic fragment.
rejectsWith :: String -> String -> IO a -> IO ()
rejectsWith label fragment action = do
    result <- try @SomeException (action >> pure ())
    case result of
        Left err -> check (label ++ " (diagnostic)") (fragment `isInfixOf` displayException err)
        Right () -> fail ("FAIL: " ++ label ++ "; unsupported operation succeeded")

-- | Make an unfolding without invoking the unsupported optimizer.
unfolding :: CoreExpr -> Unfolding
unfolding body =
    CoreUnfolding
        { uf_tmpl = body
        , uf_src = VanillaSrc
        , uf_is_top = False
        , uf_cache = UnfoldingCache False False False False
        , uf_guidance = UnfNever
        }

-- | Check the first native Core increment without executing native code.
runChecks :: DynFlags -> IO ()
runChecks dflags = do
    let b = local 1 "b" boolTy
        c = local 2 "c" boolTy
        x = local 3 "x" (indexed (TermVarTy b))
        n = local 4 "n" intTy
        worker = keep b 5
        workerTy = mkDepFunTy b (keepResult (TermVarTy b))
        trueExpr = Var (dataConWorkId trueDataCon)
        falseExpr = Var (dataConWorkId falseDataCon)
        trueIndex = mkTyConTy (promoteDataCon trueDataCon)
        falseIndex = mkTyConTy (promoteDataCon falseDataCon)
        substituteTrue = indexSubst [] b trueExpr
        substituteFalse = indexSubst [] b falseExpr

    lintAccepts dflags "retained keep lambda" [] worker
    checkType dflags "keep lambda type" workerTy (exprType worker)
    lintAccepts dflags "keep True" [] (App worker trueExpr)
    lintAccepts dflags "keep False" [] (App worker falseExpr)
    checkType dflags "True application substitutes the constructor" (keepResult trueIndex) (exprType (App worker trueExpr))
    checkType dflags "False application substitutes the constructor" (keepResult falseIndex) (exprType (App worker falseExpr))
    checkType dflags "True Id-only type substitution" (indexed trueIndex) (Subst.substTy substituteTrue (indexed (TermVarTy b)))
    checkType dflags "False Id-only type substitution" (indexed falseIndex) (Subst.substTy substituteFalse (indexed (TermVarTy b)))
    let toC = indexSubst [c] b (Var c)
        cToTrue = indexSubst [] c trueExpr
        composed = Subst.composeTCvSubst cToTrue toC
    checkType dflags "composition substitutes a Boolean variable range" trueIndex (Subst.substTy composed (TermVarTy b))
    checkType
        dflags
        "composition preserves an opaque variable range"
        (TermVarTy c)
        (Subst.substTy (Subst.composeTCvSubst Subst.emptySubst toC) (TermVarTy b))
    checkType
        dflags
        "composition preserves a constructor range"
        falseIndex
        (Subst.substTy (Subst.composeTCvSubst cToTrue substituteFalse) (TermVarTy b))
    let both = Subst.Subst (mkInScopeSet (mkVarSet [c])) (mkVarEnv [(b, falseExpr), (c, trueExpr)]) emptyVarEnv emptyVarEnv
    checkType
        dflags
        "composition uses the second environment on overlapping domains"
        trueIndex
        (Subst.substTy (Subst.composeTCvSubst both toC) (TermVarTy b))
    checkType
        dflags
        "composition keeps replacements apply-once"
        (TermVarTy c)
        (Subst.substTy (Subst.composeTCvSubst toC cToTrue) (TermVarTy b))
    let ordinaryExprSubst = indexSubst [] b (Let (NonRec c trueExpr) (Var c))
    checkType
        dflags
        "ordinary expression environments remain valid in type composition"
        intTy
        (Subst.substTy (Subst.composeTCvSubst Subst.emptySubst ordinaryExprSubst) intTy)

    let sharedX = local 6 "sharedX" (indexed (TermVarTy c))
        sharedCall value = App (App worker (Var c)) value
        sharedWorker = Lam c (Lam sharedX (sharedCall (sharedCall (Var sharedX))))
    lintAccepts dflags "two dependent calls share an unknown runtime index" [] sharedWorker
    checkType dflags "shared runtime variable instantiation" (keepResult (TermVarTy c)) (exprType (App worker (Var c)))
    lintAccepts dflags "unknown runtime variable application" [c] (App worker (Var c))
    check "alpha-equivalent retained functions" (eqType workerTy (exprType (keep c 7)))

    let sharedBody = mkFunctionType ManyTy (indexed (TermVarTy b)) (indexed (TermVarTy c))
        bindsB = DepFunTy b sharedBody
        bindsC = DepFunTy c sharedBody
    check "shared body pointer does not equate bound and free indices" (not (eqType bindsB bindsC))
    check "type ordering distinguishes bound and free indices" (nonDetCmpType bindsB bindsC /= EQ)

    let lambdaX = Lam x (Var x)
        rewrittenLambda = CoreSubst.substExpr substituteTrue lambdaX
    lintAccepts dflags "Id-only substitution updates lambda annotations" [] rewrittenLambda
    checkType dflags "Id-only substituted lambda type" (keepResult trueIndex) (exprType rewrittenLambda)
    let (_, clonedX) = CoreSubst.cloneBndr substituteTrue (unique 8) x
    check "cloneBndr gives a fresh identity" (clonedX /= x)
    checkType dflags "cloneBndr substitutes the binder type" (indexed trueIndex) (varType clonedX)
    let (cloningSubst, clonedB) = CoreSubst.cloneBndr (Subst.mkEmptySubst (mkInScopeSet (mkVarSet [b]))) (unique 9) b
        clonedLambda = CoreSubst.substExpr cloningSubst lambdaX
    lintAccepts dflags "cloning a runtime binder updates dependent annotations" [clonedB] clonedLambda
    checkType dflags "runtime clone retains its own index" (keepResult (TermVarTy clonedB)) (exprType clonedLambda)

    let captureSubst = indexSubst [c] b (Var c)
        captured = Subst.substTy captureSubst bindsC
    case captured of
        DepFunTy renamed body -> do
            check "substitution renames a capturing binder" (renamed /= c)
            checkType
                dflags
                "substitution preserves free and bound index identities"
                (mkFunctionType ManyTy (indexed (TermVarTy c)) (indexed (TermVarTy renamed)))
                body
            check "capture avoidance retains the free replacement" (c `elemVarSet` tyCoVarsOfType captured)
            check "capture avoidance removes the renamed bound index" (not (renamed `elemVarSet` tyCoVarsOfType captured))
        _ -> fail "FAIL: capture avoidance lost the retained binder"
    let missingRange = indexSubst [] b (Var c)
    check "missing substitution range fails the scope invariant" (not (Subst.isValidTCvSubst missingRange))
    rejectsWith "unsupported expression substitution" "unsupported runtime Boolean index" $
        evaluate (Subst.substTy (indexSubst [] b (Let (NonRec c trueExpr) (Var c))) (TermVarTy b))
    rejectsWith "composition rejects an unsupported index range" "unsupported runtime Boolean index" $
        evaluate
            ( Subst.substTy
                (Subst.composeTCvSubst (indexSubst [] c (Let (NonRec b trueExpr) (Var b))) toC)
                (TermVarTy b)
            )

    checkType
        dflags
        "vacuous retained binder canonicalizes to an arrow"
        (mkFunctionType ManyTy boolTy intTy)
        (mkDepFunTy b intTy)
    lintRejects dflags "raw vacuous retained type is rejected" [] (identity 10 (DepFunTy b intTy))
    lintRejects dflags "wrong retained argument type is rejected" [n] (App worker (Var n))
    lintRejects dflags "non-Boolean index is rejected" [n] (identity 11 (indexed (TermVarTy n)))
    lintRejects dflags "out-of-scope index is rejected" [] lambdaX
    lintRejects dflags "non-Boolean retained binder is rejected" [] (identity 12 (DepFunTy n (indexed (TermVarTy n))))
    lintRejects dflags "non-Type dependent codomain is rejected" [] (identity 13 (DepFunTy b (TermVarTy b)))
    let erased = mkTyVar (mkSystemName (unique 14) (mkTyVarOcc "erased")) boolTy
        forged = local 14 "forged" boolTy
    lintRejects dflags "type-variable binding cannot supply a runtime index" [erased] (identity 15 (indexed (TermVarTy forged)))
    let linear = setIdMult (local 16 "linear" boolTy) OneTy
        forgedMany = setIdMult linear ManyTy
    lintRejects dflags "linear binding site cannot supply a Many index" [linear] (identity 17 (indexed (TermVarTy forgedMany)))
    let unsupportedArg = Let (NonRec c trueExpr) (Var c)
    lintRejects dflags "unsupported Boolean producer is rejected by Lint" [] (App worker unsupportedArg)

    let dict = local 18 "dict" (mkTyConApp eqTyCon [boolTy, TermVarTy b, TermVarTy b])
        escaping = Let (NonRec b trueExpr) (Let (NonRec x (Lam dict (Var n))) (Var x))
    lintRejects dflags "let-bound runtime index cannot escape" [n] escaping
    rejectsWith "type recovery rejects let-index escape" "let-bound runtime index escapes" (evaluate (exprType escaping))
    checkType
        dflags
        "reflexive dependent coercion lifting retains the binder"
        (mkDepFunTy b (indexed (TermVarTy b)))
        (coercionLKind (mkPiCo Nominal b (mkNomReflCo (indexed (TermVarTy b)))))

    check "scanner detects a closed retained function" (hasNativeDependentExpr worker)
    check "scanner detects an isolated native type" (hasNativeDependentExpr (Type workerTy))
    check "scanner detects a coercion annotation" (hasNativeDependentExpr (Coercion (mkNomReflCo workerTy)))
    check "scanner accepts ordinary Core" (not (hasNativeDependentExpr (Lam b (Var b))))
    let host = local 19 "host" boolTy
        nativeUnfoldingHost = setIdUnfolding host (unfolding (Type workerTy))
    check
        "scanner inspects bound-Id unfolding metadata"
        (hasNativeDependentProgram [NonRec nativeUnfoldingHost trueExpr] [])
    check
        "scanner does not chase occurrence unfoldings"
        (not (hasNativeDependentExpr (Var nativeUnfoldingHost)))
    let rule =
            Rule
                { ru_name = fsLit "native-smoke"
                , ru_act = AlwaysActive
                , ru_fn = varName host
                , ru_rough = []
                , ru_bndrs = []
                , ru_args = [Type workerTy]
                , ru_rhs = trueExpr
                , ru_auto = False
                , ru_origin = nameModule eqTyConName
                , ru_orphan = IsOrphan
                , ru_local = True
                }
        nativeRuleHost = setIdSpecialisation host (RuleInfo [rule] emptyDVarSet)
    check "scanner inspects explicit rule metadata" (hasNativeDependentProgram [] [rule])
    check "scanner inspects bound-Id rule metadata" (hasNativeDependentProgram [NonRec nativeRuleHost trueExpr] [])
    let cyclic = setIdUnfolding host (unfolding (Let (NonRec cyclic trueExpr) (Var cyclic)))
    cycleResult <- timeout 2000000 (evaluate (hasNativeDependentProgram [NonRec cyclic trueExpr] []))
    check "scanner terminates on cyclic bound metadata" (cycleResult == Just False)
    rejectNativeDependentExpr "ordinary smoke" (Lam b (Var b))
    rejectsWith "native expression boundary gate" "Native dependent Core cannot enter" $
        rejectNativeDependentExpr "native smoke" worker
    rejectsWith "native program boundary gate" "Native dependent Core cannot enter" $
        rejectNativeDependentProgram "native smoke" [NonRec (local 20 "keep" workerTy) worker] []
