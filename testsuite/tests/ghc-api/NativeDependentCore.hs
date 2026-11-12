{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import GHC (DynFlags, getSessionDynFlags, runGhc)
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion (coercionLKind, mkAxInstCo, mkCoVarCo, mkFunCoNoFTF, mkNomReflCo, mkPiCo, mkReflCo, mkSubCo, mkSymCo, mkTransCo, mkTyConAppCo)
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam (..), CoAxiom, CoAxiomRule (..), Role (..))
import GHC.Core.DataCon (DataCon, dataConWorkId, promoteDataCon)
import GHC.Core.Dependent
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.SimpleOpt (simpleOptNativeExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
import GHC.Core.TyCo.Rep (Type (..))
import qualified GHC.Core.TyCo.Subst as Subst
import GHC.Core.TyCon (FamTyConFlav (..), Injectivity (..), TyCon, mkAnonTyConBinder, mkFamilyTyCon, mkSynonymTyCon, mkTyConKind)
import GHC.Core.Type
import GHC.Core.Unify (UnifyResultM (..), alwaysBindFam, alwaysBindTv, tcMatchTy, tcMatchTys, tcUnifyTysFG)
import GHC.Core.Utils (exprType)
import GHC.CoreToIface (toIfaceAlt)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Config.Parser (initParserOpts)
import GHC.Driver.Ppr (showSDoc)
import GHC.Driver.Session (parseDynamicFlagsCmdLine)
import GHC.Hs (HsType (..))
import qualified GHC.Parser as Parser
import qualified GHC.Parser.Lexer as Lexer
import GHC.Types.Basic (OccInfo (..), TailCallInfo (..))
import GHC.Types.Id
import GHC.Types.Id.Info (IdDetails (VanillaId), RuleInfo (..))
import GHC.Types.InlinePragma (ActivationX (AlwaysActive))
import GHC.Types.Name (mkSystemName, nameModule)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc, mkVarOcc)
import GHC.Types.SrcLoc (mkRealSrcLoc, noLoc, noSrcSpan, unLoc)
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Var
import GHC.Types.Var.Env
import GHC.Types.Var.Set
import GHC.Utils.Logger (initLogger)
import GHC.Utils.Outputable (ppr, vcat)
import GHC.Utils.Panic (GhcException (..))
import System.Environment (getArgs)
import System.Timeout (timeout)

-- | Run the native Core API checks against the supplied GHC library directory.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir] -> do
            dflags <- runGhc (Just libdir) getSessionDynFlags
            runSyntaxChecks dflags
            runChecks dflags
            runMatchingChecks dflags
            runCaseChecks dflags
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

-- | Check rigid index matching and conservative constructor apartness.
runMatchingChecks :: DynFlags -> IO ()
runMatchingChecks dflags = do
    let b = local 200 "b" boolTy
        other = local 201 "other" boolTy
        index = TermVarTy b
        otherIndex = TermVarTy other
        template = mkTyVar (mkSystemName (unique 202) (mkTyVarOcc "template")) boolTy
        templateTy = mkTyVarTy template
        trueExpr = Var (dataConWorkId trueDataCon)
        trueIndex = constructorIndex trueDataCon
        falseIndex = constructorIndex falseDataCon
        knownTrue = setIdUnfolding b ((unfolding trueExpr){uf_cache = UnfoldingCache True True True True})
        fine lhs rhs = tcUnifyTysFG alwaysBindFam alwaysBindTv lhs rhs
        matches lhs rhs = case tcMatchTy lhs rhs of Just _ -> True; Nothing -> False
        maybeApart lhs rhs = case fine lhs rhs of MaybeApart{} -> True; _ -> False
    check "the same runtime index matches" (matches index index)
    check "distinct runtime indices do not match" (not (matches index otherIndex))
    check "index matching ignores Id unfolding metadata" (matches index (TermVarTy knownTrue))
    check "constructor templates cannot match an opaque runtime index" (not (matches trueIndex index))
    check "a known-True unfolding cannot match the True template" (not (matches trueIndex (TermVarTy knownTrue)))
    check "runtime index versus True is only maybe apart" (maybeApart [index] [trueIndex])
    check "True versus runtime index is only maybe apart" (maybeApart [trueIndex] [index])
    check "False versus runtime index is only maybe apart" (maybeApart [falseIndex] [index])
    check "a known-True unfolding remains only maybe apart from True" (maybeApart [trueIndex] [TermVarTy knownTrue])
    case tcMatchTy templateTy index >>= (`Subst.lookupTyVar` template) of
        Just actual -> checkType dflags "a Bool template variable can bind a runtime index" index actual
        Nothing -> fail "FAIL: a Bool template variable did not bind the runtime index"
    check
        "a repeated template cannot match distinct runtime indices"
        (case tcMatchTys [templateTy, templateTy] [index, otherIndex] of Nothing -> True; Just _ -> False)
    check
        "distinct targets of a repeated template are only maybe apart"
        (maybeApart [templateTy, templateTy] [index, otherIndex])
    check
        "a repeated template matches one shared runtime index"
        (case tcMatchTys [templateTy, templateTy] [index, index] of Just _ -> True; Nothing -> False)

-- | Check retained syntax, printing, and compatibility with ordinary identifiers.
runSyntaxChecks :: DynFlags -> IO ()
runSyntaxChecks dflags = do
    logger <- initLogger
    (nativeFlags, leftovers, _) <-
        parseDynamicFlagsCmdLine logger dflags [noLoc "-XExperimentalDependentTypes"]
    check "retained extension flag" (null leftovers)
    let source = "foreach (b :: Bool) -> Select b -> Select b"
        parseWith flags input =
            case Lexer.unP Parser.parseType (Lexer.initParserState (initParserOpts flags) (stringToStringBuffer input) (mkRealSrcLoc (fsLit "retained-syntax") 1 1)) of
                Lexer.POk _ ty -> pure ty
                Lexer.PFailed _ -> fail ("FAIL: cannot parse type: " ++ input)
        isRetained ty = case unLoc ty of
            HsForEachTy{hst_bndrs = [_]} -> True
            _ -> False
    retained <- parseWith nativeFlags source
    check "retained quantification has a distinct AST node" (isRetained retained)
    reparsed <- parseWith nativeFlags (showSDoc nativeFlags (ppr retained))
    check "retained quantification printing round trip" (isRetained reparsed)
    check "retained binders reject inferred braces during parsing" $
        case Lexer.unP Parser.parseType (Lexer.initParserState (initParserOpts nativeFlags) (stringToStringBuffer "foreach {b} -> Bool") (mkRealSrcLoc (fsLit "retained-syntax") 1 1)) of
            Lexer.PFailed _ -> True
            Lexer.POk{} -> False
    ordinary <- parseWith dflags source
    check "foreach remains an identifier when disabled" $ case unLoc ordinary of
        HsFunTy{} -> True
        _ -> False
    visible <- parseWith nativeFlags "forall a -> a -> a"
    check "visible forall remains erased quantification" $ case unLoc visible of
        HsForAllTy{} -> True
        _ -> False

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

-- | Require an unsupported-operation diagnostic with its compiler phase.
rejectsUnsupported :: String -> String -> IO () -> IO ()
rejectsUnsupported label context action = do
    result <- try @GhcException action
    case result of
        Left (Sorry message) -> do
            check (label ++ " (phase)") (context `isInfixOf` message)
            check (label ++ " (restriction)") ("This operation does not support" `isInfixOf` message)
        Left exception -> fail ("FAIL: " ++ label ++ "; wrong exception: " ++ displayException exception)
        Right () -> fail ("FAIL: " ++ label ++ "; unsupported operation succeeded")

-- | Check simplification against the original type and both Lint boundaries.
simplifyAccepts :: DynFlags -> String -> [Var] -> CoreExpr -> IO CoreExpr
simplifyAccepts dflags label scope expression = do
    lintAccepts dflags (label ++ " (before)") scope expression
    let simplified = simpleOptNativeExpr expression
    lintAccepts dflags (label ++ " (after)") scope simplified
    checkType dflags label (exprType expression) (exprType simplified)
    pure simplified

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
    optimizedTrue <- simplifyAccepts dflags "keep True beta reduction" [] (App worker trueExpr)
    optimizedFalse <- simplifyAccepts dflags "keep False beta reduction" [] (App worker falseExpr)
    optimizedVariable <- simplifyAccepts dflags "keep variable beta reduction" [c] (App worker (Var c))
    optimizedShadow <- simplifyAccepts dflags "keep beta reduction avoids capture" [b] (App worker (Var b))
    let isIdentity (Lam arg (Var result)) = arg == result
        isIdentity _ = False
    check "keep True removes the retained beta redex" (isIdentity optimizedTrue)
    check "keep False removes the retained beta redex" (isIdentity optimizedFalse)
    check "keep variable removes the retained beta redex" (isIdentity optimizedVariable)
    check "keep shadowing removes the retained beta redex" (isIdentity optimizedShadow)
    let g = local 80 "g" workerTy
        f = setIdOccInfo (local 81 "f" workerTy) (ManyOccs (AlwaysTailCalled 1))
        boundWorker = Let (NonRec g worker) (Let (NonRec f (Var g)) (App (Var f) trueExpr))
    optimizedBound <- simplifyAccepts dflags "native simplification preserves ordinary bindings" [] boundWorker
    let noTemplate binder = case realIdUnfolding binder of
            CoreUnfolding{} -> False
            DFunUnfolding{} -> False
            _ -> null (idCoreRules binder)
    check
        "native simplification creates neither join points nor unfolding templates"
        ( case optimizedBound of
            Let (NonRec g' _) (Let (NonRec f' _) _) ->
                not (isJoinId g' || isJoinId f') && noTemplate g' && noTemplate f'
            _ -> False
        )
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
    let hiddenSynonym =
            mkSynonymTyCon
                (mkSystemName (unique 70) (mkTcOcc "HiddenKeep"))
                liftedTypeKind
                []
                liftedTypeKind
                []
                workerTy
                True
                False
                False
                False
        hiddenType = mkTyConTy hiddenSynonym
        unusedParameter = mkTyVar (mkSystemName (unique 71) (mkTyVarOcc "unused")) liftedTypeKind
        unusedBinders = [mkAnonTyConBinder unusedParameter]
        unusedSynonym =
            mkSynonymTyCon
                (mkSystemName (unique 72) (mkTcOcc "Ignore"))
                (mkTyConKind unusedBinders liftedTypeKind)
                unusedBinders
                liftedTypeKind
                [Phantom]
                intTy
                True
                True
                True
                True
        ignoredType = mkTyConApp unusedSynonym [workerTy]
        ignoredHiddenType = mkTyConApp unusedSynonym [hiddenType]
        identityParameter = mkTyVar (mkSystemName (unique 73) (mkTyVarOcc "identity")) liftedTypeKind
        identityBinders = [mkAnonTyConBinder identityParameter]
        identitySynonym =
            mkSynonymTyCon
                (mkSystemName (unique 74) (mkTcOcc "Identity"))
                (mkTyConKind identityBinders liftedTypeKind)
                identityBinders
                liftedTypeKind
                [Representational]
                (mkTyVarTy identityParameter)
                True
                True
                False
                False
        nestedOrdinary = iterate (mkTyConApp identitySynonym . (: [])) boolTy !! 12
    check "scanner expands a hidden retained function synonym" (hasNativeDependentExpr (Type hiddenType))
    check "scanner expands synonyms inside ordinary types" (hasNativeDependentExpr (Type (mkVisFunTyMany intTy hiddenType)))
    check "scanner expands synonyms inside coercions" (hasNativeDependentExpr (Coercion (mkNomReflCo hiddenType)))
    check "scanner preserves unused native synonym arguments" (hasNativeDependentExpr (Type ignoredType))
    check "scanner expands synonyms in unused synonym arguments" (hasNativeDependentExpr (Type ignoredHiddenType))
    check "scanner accepts an ordinary synonym" (not (hasNativeDependentExpr (Type (mkTyConApp unusedSynonym [boolTy]))))
    nestedResult <- timeout 2000000 (evaluate (hasNativeDependentExpr (Type nestedOrdinary)))
    check "scanner terminates on nested ordinary identity synonyms" (nestedResult == Just False)
    rejectsWith "hidden synonym expression boundary gate" "Native dependent Core cannot enter" $
        rejectNativeDependentExpr "synonym smoke" (Type hiddenType)
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
    rejectsUnsupported "native expression boundary gate" "native smoke" $
        rejectNativeDependentExpr "native smoke" worker
    rejectsUnsupported "native program boundary gate" "native smoke" $
        rejectNativeDependentProgram "native smoke" [NonRec (local 20 "keep" workerTy) worker] []
    rejectsUnsupported "native JavaScript boundary diagnostic" "JavaScript code generation" $
        rejectNativeDependentExpr "JavaScript code generation" worker

-- | Build a nominal proof identifier with the specified endpoints.
proof :: Int -> Type -> Type -> CoVar
proof n lhs rhs = mkCoVar (mkSystemName (unique n) (mkVarOcc "cv")) (mkNomEqPred lhs rhs)

-- | Map a Boolean constructor to its promoted index.
constructorIndex :: DataCon -> Type
constructorIndex = mkTyConTy . promoteDataCon

-- | Build a function at a constructor index and transport it through a proof.
dictionaryBranch :: Int -> Id -> DataCon -> CoVar -> CoreExpr
dictionaryBranch n payload con cv = Cast body (mkSymCo funCo)
  where
    concrete = constructorIndex con
    dict = local n "dict" (mkTyConApp eqTyCon [boolTy, concrete, concrete])
    body = Lam dict (Var payload)
    indexCo = mkCoVarCo cv
    predCo = mkTyConAppCo Nominal eqTyCon [mkNomReflCo boolTy, indexCo, indexCo]
    funCo = mkFunCoNoFTF Representational (mkNomReflCo ManyTy) (mkSubCo predCo) (mkReflCo Representational intTy)

-- | Build exhaustive alternatives in the wired Boolean constructor tag order.
dictionaryAlts :: Id -> CoVar -> CoVar -> [CoreAlt]
dictionaryAlts payload falseProof trueProof =
    [ Alt (DataAlt falseDataCon) [falseProof] (dictionaryBranch 20 payload falseDataCon falseProof)
    , Alt (DataAlt trueDataCon) [trueProof] (dictionaryBranch 21 payload trueDataCon trueProof)
    ]

-- | Construct a small closed family and its two ordinary family equations.
selectFamily :: (TyCon, CoAxiom Branched)
selectFamily = (family, axiom)
  where
    parameter = mkTyVar (mkSystemName (unique 100) (mkTyVarOcc "b")) boolTy
    binders = [mkAnonTyConBinder parameter]
    family =
        mkFamilyTyCon
            (mkSystemName (unique 101) (mkTcOcc "Select"))
            (mkTyConKind binders liftedTypeKind)
            binders
            0
            liftedTypeKind
            Nothing
            (ClosedTypeFamilyTyCon (CTF (Just axiom)))
            Nothing
            NotInjective
    axiom =
        mkBranchedCoAxiom
            Nominal
            (mkSystemName (unique 102) (mkTcOcc "SelectAx"))
            family
            [ mkCoAxBranch [] [] [] [constructorIndex trueDataCon] intTy [] noSrcSpan
            , mkCoAxBranch [] [] [] [constructorIndex falseDataCon] boolTy [] noSrcSpan
            ]

-- | Apply the test family to one Boolean index.
selected :: Type -> Type
selected index = mkTyConApp (fst selectFamily) [index]

-- | Compose constructor refinement with an existing closed-family equation.
selectionBranch :: Int -> CoVar -> CoreExpr -> CoreExpr
selectionBranch branch cv payload = Cast payload (mkSubCo (mkSymCo toConcrete))
  where
    refinement = mkTyConAppCo Nominal (fst selectFamily) [mkCoVarCo cv]
    equation = mkAxInstCo Nominal (BranchedAxiom (snd selectFamily) branch) [] []
    toConcrete = mkTransCo refinement equation

-- | Check branch evidence without evaluating native dependent code.
runCaseChecks :: DynFlags -> IO ()
runCaseChecks dflags = do
    let b = local 1 "b" boolTy
        other = local 2 "other" boolTy
        caseBinder = local 3 "case_b" boolTy
        payload = local 4 "n" intTy
        index = TermVarTy b
        falseIndex = constructorIndex falseDataCon
        trueIndex = constructorIndex trueDataCon
        falseExpr = Var (dataConWorkId falseDataCon)
        trueExpr = Var (dataConWorkId trueDataCon)
        falseProof = proof 5 index falseIndex
        trueProof = proof 6 index trueIndex
        result = indexed index
        alts = dictionaryAlts payload falseProof trueProof
        nativeCase = Case (Var b) caseBinder result alts
        whole = Lam b nativeCase
        plainCase = Case (Var b) caseBinder intTy [Alt (DataAlt falseDataCon) [] (Var payload), Alt (DataAlt trueDataCon) [] (Var payload)]
    lintAccepts dflags "nominal branch evidence transports dictionary functions" [b, payload] nativeCase
    lintAccepts dflags "retained Boolean lambda contains a refined case" [payload] whole
    checkType dflags "refined case result keeps the outer index" result (exprType nativeCase)
    checkType dflags "refined lambda retains its Boolean argument" (mkDepFunTy b result) (exprType whole)
    lintAccepts dflags "ordinary Boolean alternatives remain unchanged" [b, payload] plainCase
    lintAccepts dflags "DEFAULT plus one constructor proof covers Bool" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt DEFAULT [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]

    let selectAlts =
            [ Alt (DataAlt falseDataCon) [falseProof] (selectionBranch 1 falseProof falseExpr)
            , Alt (DataAlt trueDataCon) [trueProof] (selectionBranch 0 trueProof (Var payload))
            ]
        selectCase = Case (Var b) caseBinder (selected index) selectAlts
        choose = Lam b selectCase
        reduce branch = mkAxInstCo Representational (BranchedAxiom (snd selectFamily) branch) [] []
    lintAccepts dflags "Select True is Int and Select False is Bool" [payload] choose
    lintRejects dflags "Select False branch rejects an Int payload" [payload] $
        Lam
            b
            ( Case
                (Var b)
                caseBinder
                (selected index)
                [ Alt (DataAlt falseDataCon) [falseProof] (selectionBranch 1 falseProof (Var payload))
                , Alt (DataAlt trueDataCon) [trueProof] (selectionBranch 0 trueProof (Var payload))
                ]
            )
    checkType dflags "choose has a retained Select result" (mkDepFunTy b (selected index)) (exprType choose)
    lintAccepts dflags "choose True followed by its family equation" [payload] (Cast (App choose trueExpr) (reduce 0))
    lintAccepts dflags "choose False followed by its family equation" [payload] (Cast (App choose falseExpr) (reduce 1))
    checkType dflags "True application selects Int" intTy (exprType (Cast (App choose trueExpr) (reduce 0)))
    checkType dflags "False application selects Bool" boolTy (exprType (Cast (App choose falseExpr) (reduce 1)))
    optimizedChooseTrue <- simplifyAccepts dflags "choose True reduces with checked reflexivity" [payload] (App choose trueExpr)
    optimizedChooseFalse <- simplifyAccepts dflags "choose False reduces with checked reflexivity" [payload] (App choose falseExpr)
    check "choose True removes virtual branch proofs" (not (hasNativeDependentExpr optimizedChooseTrue))
    check "choose False removes virtual branch proofs" (not (hasNativeDependentExpr optimizedChooseFalse))
    optimizedChoose <- simplifyAccepts dflags "choose retains a strict variable case" [payload] choose
    check
        "choose keeps constructor discrimination on an opaque index"
        (case optimizedChoose of Lam _ (Case (Var _) _ _ _) -> True; _ -> False)
    let knownUnfolding = (unfolding trueExpr){uf_cache = UnfoldingCache True True True True}
        knownTrue = setIdUnfolding b knownUnfolding
        knownCase =
            CoreSubst.substExpr (indexSubst [knownTrue, payload] b (Var knownTrue)) $
                Case (Var b) (setIdOccInfo caseBinder IAmDead) (selected index) selectAlts
    optimizedKnown <- simplifyAccepts dflags "constructor unfolding preserves opaque proof" [knownTrue, payload] knownCase
    check
        "an unfolding cannot turn an opaque index proof into reflexivity"
        (case optimizedKnown of Case (Var _) _ _ _ -> True; _ -> False)
    let producer = local 82 "producer" (mkFunctionType ManyTy boolTy boolTy)
        producerArgument = App (Var producer) trueExpr
        closedResult =
            Lam b $
                Case
                    (Var b)
                    caseBinder
                    intTy
                    [ Alt (DataAlt falseDataCon) [falseProof] (Var payload)
                    , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
                    ]
    optimizedProducer <- simplifyAccepts dflags "unsupported Boolean beta producer stays lazy" [producer, payload] (App closedResult producerArgument)
    check
        "an unsupported producer keeps its application redex"
        (case optimizedProducer of App (Lam _ _) (App (Var _) _) -> True; _ -> False)

    let keepBool = local 30 "keep_b" boolTy
        keepIndex = TermVarTy keepBool
        keepValue = local 31 "value" (selected keepIndex)
        keepSelect = Lam keepBool (Lam keepValue (Var keepValue))
        again = Lam other (App (App keepSelect (Var other)) (App choose (Var other)))
        mixed = App (App keepSelect (Var b)) (App choose (Var other))
        falsePayload = Cast falseExpr (mkSymCo (reduce 1))
    lintAccepts dflags "keepSelect retains a Select value at its runtime index" [] keepSelect
    checkType
        dflags
        "keepSelect has a retained dependent function type"
        (mkDepFunTy keepBool (mkFunctionType ManyTy (selected keepIndex) (selected keepIndex)))
        (exprType keepSelect)
    lintAccepts dflags "choose and keepSelect compose at one shared runtime index" [payload] again
    checkType
        dflags
        "shared choose and keepSelect composition retains its index"
        (mkDepFunTy other (selected (TermVarTy other)))
        (exprType again)
    lintRejects dflags "independent runtime Boolean indices do not unify" [b, other, payload] mixed
    lintRejects dflags "keepSelect True rejects an explicit Select False payload" [] $
        App (App keepSelect trueExpr) falsePayload

    let substituteTrue = indexSubst [payload] b trueExpr
        substituteOther = indexSubst [other, payload] b (Var other)
        trueCase = CoreSubst.substExpr substituteTrue nativeCase
        otherCase = CoreSubst.substExpr substituteOther nativeCase
        trueSelect = CoreSubst.substExpr substituteTrue selectCase
        otherSelect = CoreSubst.substExpr substituteOther selectCase
        boundChoose = CoreSubst.substExpr substituteTrue choose
    lintAccepts dflags "constructor substitution updates every refined-case proof" [payload] trueCase
    checkType dflags "constructor-substituted case recovers its instantiated type" (indexed trueIndex) (exprType trueCase)
    lintAccepts dflags "variable substitution updates every refined-case proof" [other, payload] otherCase
    checkType dflags "variable-substituted case retains the replacement identity" (indexed (TermVarTy other)) (exprType otherCase)
    lintAccepts dflags "constructor substitution preserves Select branch equations" [payload] trueSelect
    checkType dflags "constructor-substituted Select case recovers its instantiated type" (selected trueIndex) (exprType trueSelect)
    lintAccepts dflags "substituted Select case reduces through its declared equation" [payload] (Cast trueSelect (reduce 0))
    checkType dflags "substituted Select True reduces to Int" intTy (exprType (Cast trueSelect (reduce 0)))
    lintAccepts dflags "variable substitution preserves Select branch equations" [other, payload] otherSelect
    checkType dflags "variable-substituted Select case retains the replacement identity" (selected (TermVarTy other)) (exprType otherSelect)
    lintAccepts dflags "substitution respects the retained binder of choose" [payload] boundChoose
    checkType dflags "bound choose keeps its original dependent type" (mkDepFunTy b (selected index)) (exprType boundChoose)

    lintRejects dflags "proof cannot refine an unrelated scrutinee" [b, other, payload] (Case (Var other) caseBinder result alts)
    lintRejects dflags "opposite constructor proof is rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [trueProof] (Var payload)
            , Alt (DataAlt trueDataCon) [] (Var payload)
            ]
    let reverseProof = proof 7 trueIndex index
    lintRejects dflags "reversed proof endpoints are rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [reverseProof] (Var payload)
            ]
    let representational = mkCoVar (mkSystemName (unique 8) (mkVarOcc "repr")) (mkReprEqPred index trueIndex)
    lintRejects dflags "representational proof cannot refine a nominal index" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [representational] (Var payload)
            ]
    lintRejects dflags "extra proof binder is rejected" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof, proof 9 index trueIndex] (Var payload)
            ]
    let ordinary = setIdDetails (proof 10 index trueIndex) VanillaId
    lintRejects dflags "an ordinary Id cannot replace a proof CoVar" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [ordinary] (Var payload)
            ]
    let erased = mkTyVar (mkSystemName (unique 11) (mkTyVarOcc "erased")) boolTy
    lintRejects dflags "a type variable cannot replace a proof CoVar" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [erased] (Var payload)
            ]
    lintRejects dflags "DEFAULT cannot bind constructor evidence" [b, payload] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt DEFAULT [trueProof] (Var payload)
            , Alt (DataAlt trueDataCon) [] (Var payload)
            ]
    lintRejects dflags "constructor proof cannot shadow an in-scope proof identity" [b, payload, trueProof] $
        Case
            (Var b)
            caseBinder
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]
    lintRejects dflags "missing coverage cannot justify a contradictory proof" [payload] $
        Case
            trueExpr
            caseBinder
            intTy
            [Alt (DataAlt falseDataCon) [proof 18 trueIndex falseIndex] (Var payload)]
    lintRejects dflags "proof cannot escape into another branch" [b, payload] $
        Case
            (Var b)
            caseBinder
            result
            [ Alt (DataAlt falseDataCon) [falseProof] (dictionaryBranch 22 payload trueDataCon trueProof)
            , Alt (DataAlt trueDataCon) [trueProof] (dictionaryBranch 23 payload trueDataCon trueProof)
            ]
    lintRejects dflags "case binder cannot occur in the outer result annotation" [b, payload] $
        Case (Var b) caseBinder (indexed (TermVarTy caseBinder)) alts
    lintRejects dflags "arbitrary producers cannot supply an index" [payload] $
        Case
            (Let (NonRec b trueExpr) (Var b))
            caseBinder
            (indexed trueIndex)
            (dictionaryAlts payload (proof 12 trueIndex falseIndex) (proof 13 trueIndex trueIndex))
    let notBool = local 14 "not_bool" intTy
    lintRejects dflags "non-Boolean scrutinee cannot justify Boolean evidence" [notBool, payload] $
        Case
            (Var notBool)
            (local 15 "case_int" intTy)
            intTy
            [ Alt (DataAlt falseDataCon) [] (Var payload)
            , Alt (DataAlt trueDataCon) [trueProof] (Var payload)
            ]

    let literalCase =
            Case
                trueExpr
                caseBinder
                (indexed trueIndex)
                (dictionaryAlts payload (proof 16 trueIndex falseIndex) (proof 17 trueIndex trueIndex))
    lintAccepts dflags "literal scrutinee supplies branch evidence" [payload] literalCase
    check "scanner detects literal-only virtual proof binders" (hasNativeDependentExpr literalCase)
    check "scanner still detects runtime-index case proofs" (hasNativeDependentExpr nativeCase)
    let literalProofAlt = Alt (DataAlt trueDataCon) [proof 24 trueIndex trueIndex] (Var payload)
    rejectsWith "interfaces reject literal-only Boolean proof alternatives" "native Boolean proof interfaces are not implemented" $
        evaluate (toIfaceAlt literalProofAlt)
