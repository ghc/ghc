module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, isInfixOf)
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam (..), CoAxiom, CoAxiomRule (..))
import GHC.Core.DataCon (dataConWorkId, promoteDataCon)
import GHC.Core.Dependent (hasNativeDependentExpr)
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Make (mkCoreConApps, mkIntExpr)
import GHC.Core.Opt.Arity (etaExpand)
import GHC.Core.Predicate (mkNomEqPred)
import GHC.Core.TyCo.Rep (Type (..))
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (DynFlags (..), gopt_set)
import GHC.Driver.Env (HscEnv, hscInterp)
import GHC.Driver.Flags (GeneralFlag (Opt_ExternalInterpreter))
import GHC.Driver.Main (hscCompileCoreExpr)
import GHC.Driver.Ppr (showSDoc)
import GHC.Runtime.Interpreter (evalString)
import GHC.Types.Error (Diagnostic (diagnosticCode), DiagnosticCode (..), MsgEnvelope (..), getMessages, pprDiagnostic)
import GHC.Types.Id
import GHC.Types.Name (mkSystemName, nameOccName)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc, mkVarOcc, occNameString)
import GHC.Types.SourceError (srcErrorMessages)
import GHC.Types.SrcLoc (isGoodSrcSpan, noSrcSpan)
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Var
import GHC.Unit.Module.ModGuts (ModGuts (mg_binds))
import GHC.Unit.Types (mainUnit, mkModule, mkModuleName)
import GHC.Utils.Outputable (vcat)
import System.Environment (getArgs)

-- | Execute retained Boolean functions with inputs read at runtime.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir] -> GHC.runGhc (Just libdir) runChecks
        _ -> fail "Usage: NativeDependentCoreRun GHC_LIBDIR"

-- | Supply ordinary observations and a lazy runtime input source.
helperSource :: String
helperSource =
    unlines
        [ "module NativeRuntimeHelpers where"
        , "import Data.IORef"
        , "{-# NOINLINE drive #-}"
        , "drive :: (Bool -> IO String) -> Bool -> IO String"
        , "drive k seed = newIORef seed >>= readIORef >>= k"
        , "driveBoth :: (Bool -> IO String) -> IO String"
        , "driveBoth k = do t <- drive k True; f <- drive k False; pure (t ++ \"\\n\" ++ f)"
        , "observeInt :: Int -> IO String"
        , "observeInt x = pure (show x)"
        , "observeBool :: Bool -> IO String"
        , "observeBool x = pure (show x)"
        , "force :: a -> IO String"
        , "force x = x `seq` pure \"forced\""
        , "{-# NOINLINE bottomBool #-}"
        , "bottomBool :: Bool"
        , "bottomBool = error \"native-bottom-bool\""
        ]

-- | Check source elaboration with one shared retained worker and lazy indices.
retainedSource :: String
retainedSource =
    unlines
        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, RankNTypes, TypeOperators #-}"
        , "module NativeSourceKeep where"
        , "import NativeRuntimeHelpers"
        , "entry :: IO String"
        , "entry ="
        , "  let keep :: foreach (b :: Bool) -> ((b ~ b) => Int) -> Int"
        , "      keep b x = x"
        , "  in do both <- driveBoth (\\b -> observeInt (keep b 42))"
        , "        let b = bottomBool"
        , "        lazy <- observeInt (keep b 42)"
        , "        pure (both ++ \"\\n\" ++ lazy)"
        ]

-- | Check a dependent result family and observe a partial application at WHNF.
retainedSelectionSource :: String
retainedSelectionSource =
    unlines
        [ "{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, KindSignatures, TypeFamilies #-}"
        , "module NativeSourceSelection where"
        , "import NativeRuntimeHelpers"
        , "type family Select (b :: Bool) where"
        , "  Select True = Int"
        , "  Select False = Bool"
        , "entry :: IO String"
        , "entry ="
        , "  let keep :: foreach (b :: Bool) -> Select b -> Select b"
        , "      keep b x = x"
        , "  in do t <- observeInt (keep True 42)"
        , "        f <- observeBool (keep False False)"
        , "        let b = bottomBool"
        , "        case keep b of"
        , "          !_ -> pure (t ++ \"\\n\" ++ f ++ \"\\nforced\")"
        ]

-- | Parse an in-memory source target without loading or compiling its bindings.
parseSourceTarget :: String -> String -> GHC.Ghc GHC.ParsedModule
parseSourceTarget moduleName source = do
    flags <- GHC.getSessionDynFlags
    now <- liftIO getCurrentTime
    targets <- GHC.getTargets
    let target =
            GHC.Target
                { GHC.targetId = GHC.TargetFile (moduleName ++ ".hs") Nothing
                , GHC.targetUnitId = GHC.homeUnitId_ flags
                , GHC.targetAllowObjCode = False
                , GHC.targetContents = Just (stringToStringBuffer source, now)
                }
    GHC.setTargets (targets ++ [target])
    void (GHC.depanal [] False)
    summary <- GHC.getModSummary (mkModule mainUnit (mkModuleName moduleName))
    GHC.parseModule summary

{- | Extract a closed entry expression before ordinary module optimisation.
The existing native expression API then checks and lowers that expression.
-}
sourceEntry :: String -> String -> GHC.Ghc CoreExpr
sourceEntry moduleName source = do
    parsed <- parseSourceTarget moduleName source
    checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
    desugared <- GHC.desugarModule checked
    let bindings = flattenBinds (mg_binds (GHC.coreModule desugared))
    case find ((== "entry") . occNameString . nameOccName . varName . fst) bindings of
        Just (entry, expression) ->
            let dependencies = filter ((/= entry) . fst) bindings
             in pure $ if null dependencies then expression else Let (Rec dependencies) expression
        Nothing -> fail "FAIL: source module has no entry binding"

-- | Run retained source code through checking, desugaring, and native lowering.
runSourceChecks :: GHC.Ghc ()
runSourceChecks = do
    expression <- sourceEntry "NativeSourceKeep" retainedSource
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source keep" expression
    unless (lines actual == ["42", "42", "42"]) $
        fail ("FAIL: source keep runtime or laziness observation: " ++ show actual)
    liftIO $ do
        putStrLn "source keep True: 42"
        putStrLn "source keep False: 42"
        putStrLn "source keep bottom (fully applied): 42"
    selectedExpression <- sourceEntry "NativeSourceSelection" retainedSelectionSource
    selectedEnvironment <- GHC.getSession
    selectedActual <- liftIO $ execute selectedEnvironment flags "source keepSelect" selectedExpression
    unless (lines selectedActual == ["42", "False", "forced"]) $
        fail ("FAIL: source keepSelect family or laziness observation: " ++ show selectedActual)
    liftIO $ do
        putStrLn "source keepSelect True: 42"
        putStrLn "source keepSelect False: False"
        putStrLn "source keepSelect bottom (partial WHNF): forced"

-- | Require a located source diagnostic, without catching internal failures.
sourceRejects :: String -> Int -> String -> [String] -> [String] -> GHC.Ghc ()
sourceRejects label code fragment extensions declarations = do
    targets <- GHC.getTargets
    let moduleName = "NativeSourceRejected"
        source =
            unlines $
                ["{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"]
                    ++ map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") extensions
                    ++ [ "module " ++ moduleName ++ " where"
                       , "type family Select (b :: Bool) where"
                       , "  Select True = Int"
                       , "  Select False = Bool"
                       ]
                    ++ declarations
    result <- GHC.handleSourceError (pure . Left) $ do
        parsed <- parseSourceTarget moduleName source
        void (GHC.typecheckModule GHC.StartAndStopTcMPlugins parsed)
        pure (Right ())
    GHC.setTargets targets
    case result of
        Right () -> fail ("FAIL: source accepted " ++ label)
        Left exception -> do
            flags <- GHC.getSessionDynFlags
            let messages = bagToList (getMessages (srcErrorMessages exception))
                expected = Just (DiagnosticCode "GHC" (fromIntegral code))
                matches envelope =
                    diagnosticCode (errMsgDiagnostic envelope) == expected
                        && isGoodSrcSpan (errMsgSpan envelope)
                        && fragment `isInfixOf` showSDoc flags (pprDiagnostic (errMsgDiagnostic envelope))
            unless (any matches messages)
                $ fail
                $ "FAIL: source rejection "
                    ++ label
                    ++ "; expected "
                    ++ show expected
                    ++ " containing "
                    ++ show fragment
                    ++ " at a source location; actual codes: "
                    ++ show (map (diagnosticCode . errMsgDiagnostic) messages)
                    ++ "; got:\n"
                    ++ showSDoc flags (vcat (map (pprDiagnostic . errMsgDiagnostic) messages))

-- | Check unsupported source forms without desugaring or starting plugins twice.
runSourceRejectChecks :: GHC.Ghc ()
runSourceRejectChecks = do
    mapM_
        check
        [ ("wrong Bool domain", 83605, "must have type", [], local [] "foreach (b :: Int) -> Int -> Int" identity "42")
        , ("missing domain annotation", 83605, "explicit domain type", [], local [] "foreach b -> Int -> Int" identity "42")
        , ("anonymous retained binder", 83605, "requires a variable name", [], local [] "foreach (_ :: Bool) -> Int -> Int" identity "42")
        , ("multiple retained binders", 83605, "exactly one argument", [], local [] "foreach (b :: Bool) (c :: Bool) -> Int -> Int" ["keep b c x = x"] "42")
        , ("partial retained signature", 83605, "complete type signature", ["PartialTypeSignatures", "NamedWildCards"], local [] "foreach (b :: Bool) -> _a -> _a" identity "42")
        , ("non-variable retained pattern", 83605, "bare variable pattern", [], local [] selection ["keep _ x = x"] "42")
        , ("multiple retained equations", 83605, "exactly one equation", [], local [] selection (identity ++ identity) "42")
        , ("producer expression argument", 83605, "Boolean constructor or a local Boolean variable", [], local [] consumer identity "keep (not False) 42")
        , ("nonreflexive indexed constraint", 83605, "Only reflexive Boolean equality constraints", [], local [] "foreach (b :: Bool) -> ((b ~ True) => Int) -> Int" identity "42")
        , ("general indexed constraint", 83605, "Only reflexive Boolean equality constraints", ["FlexibleContexts"], local [] "foreach (b :: Bool) -> (Show (Select b) => Int) -> Int" identity "42")
        , ("ImpredicativeTypes", 83605, "Quick Look inference", ["ImpredicativeTypes"], local [] consumer ["keep b x = 42"] "keep True 42")
        , ("Quick Look through ($)", 83605, "Quick Look inference", [], local [] consumer ["keep b x = 42"] "(keep $ True) 42")
        , ("INLINE retained function", 83605, "Optimisation pragmas", [], local ["{-# INLINE keep #-}"] selection identity "42")
        , ("INLINABLE retained function", 83605, "Optimisation pragmas", [], local ["{-# INLINABLE keep #-}"] selection identity "42")
        , ("SPECIALISE type pragma", 83605, "Optimisation pragmas", [], local ["{-# SPECIALISE keep :: Bool -> Int -> Int #-}"] selection identity "42")
        , ("SPECIALISE expression pragma", 83605, "Optimisation pragmas", [], local ["{-# SPECIALISE keep True #-}"] selection identity "42")
        , ("constrained result wrapper", 83605, "requires a wrapper", [], local [] "foreach (b :: Bool) -> ((b ~ b) => Int) -> ((b ~ b) => Int)" identity "42")
        , ("unsigned retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["alias = keep"]) "42")
        , ("unsigned partial retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["index :: Bool", "index = False", "alias = keep index"]) "42")
        , ("nested retained type", 43582, "not supported in this type position", [], ["bad :: Int -> (foreach (b :: Bool) -> Select b -> Select b)", "bad = undefined"])
        , ("retained type synonym", 43582, "not supported in this type position", [], ["type Bad = foreach (b :: Bool) -> Select b -> Select b"])
        , ("unregistered ordinary term name", 37479, "is a term-level binding", [], ["ordinary :: Bool", "ordinary = False", "type Bad = NativeSourceRejected.ordinary"])
        , ("constructor payload mismatch", 83865, "second argument", [], local [] selection identity "keep True False")
        ,
            ( "distinct index from a local alias"
            , 83865
            , "Select"
            , []
            , local [] selection (identity ++ ["f :: " ++ selection, "f b x = let c :: Bool; c = b in keep c x"]) "42"
            )
        , ("True payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = True"] "42")
        , ("False payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = False"] "42")
        , ("numeric payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = 42"] "42")
        ]
    liftIO $ putStrLn "source rejection checks: ok"
  where
    identity = ["keep b x = x"]
    selection = "foreach (b :: Bool) -> Select b -> Select b"
    consumer = "foreach (b :: Bool) -> ((b ~ b) => Int) -> Int"
    local pragmas signature equations body =
        ["entry :: Int", "entry =", "  let"]
            ++ map ("    " ++) (pragmas ++ ["keep :: " ++ signature] ++ equations)
            ++ ["  in " ++ body]
    check (label, code, fragment, extensions, declarations) =
        sourceRejects label code fragment extensions declarations

-- | Load helper bytecode and obtain its ordinary Core identifiers.
loadHelpers :: GHC.Ghc [Id]
loadHelpers = do
    flags <- GHC.getSessionDynFlags
    void
        $ GHC.setSessionDynFlags
        $ gopt_set
            flags{backend = GHC.bytecodeBackend, ghcLink = GHC.LinkInMemory, verbosity = 0}
            Opt_ExternalInterpreter
    now <- liftIO getCurrentTime
    let moduleName = "NativeRuntimeHelpers"
        target =
            GHC.Target
                { GHC.targetId = GHC.TargetFile (moduleName ++ ".hs") Nothing
                , GHC.targetUnitId = GHC.homeUnitId_ flags
                , GHC.targetAllowObjCode = False
                , GHC.targetContents = Just (stringToStringBuffer helperSource, now)
                }
    GHC.setTargets [target]
    loaded <- GHC.load GHC.LoadAllTargets
    unless (GHC.succeeded loaded) (fail "FAIL: ordinary helper module did not load")
    information <- GHC.getModuleInfo (mkModule mainUnit (mkModuleName moduleName))
    case information of
        Nothing -> fail "FAIL: loaded helper module has no module information"
        Just info -> mapM loadedId (GHC.modInfoExports info)
  where
    loadedId name = do
        thing <- GHC.lookupGlobalName name
        case thing of
            Just (GHC.AnId binder)
                | isGlobalId binder -> pure binder
            _ -> fail ("FAIL: loaded helper is not a global Id: " ++ occNameString (nameOccName name))

-- | Find a helper by its source name.
helper :: [Id] -> String -> Id
helper bindings label =
    case find ((== label) . occNameString . nameOccName . varName) bindings of
        Just binder -> binder
        Nothing -> error ("Missing ordinary helper: " ++ label)

-- | Allocate a stable test identifier.
unique :: Int -> Unique
unique = mkUniqueGrimilyWithTag 'R' . fromIntegral

-- | Build an unrestricted local value identifier.
local :: Int -> String -> Type -> Id
local n label = mkSysLocal (fsLit label) (unique n) ManyTy

-- | Construct nominal branch evidence with exact endpoints.
proof :: Int -> Type -> Type -> CoVar
proof n lhs rhs = mkCoVar (mkSystemName (unique n) (mkVarOcc "cv")) (mkNomEqPred lhs rhs)

-- | Return the existing promoted Boolean constructor types.
boolIndex :: Bool -> Type
boolIndex value = mkTyConTy (promoteDataCon (if value then trueDataCon else falseDataCon))

-- | Construct the two ordinary closed-family equations for Select.
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
            [ mkCoAxBranch [] [] [] [boolIndex True] intTy [] noSrcSpan
            , mkCoAxBranch [] [] [] [boolIndex False] boolTy [] noSrcSpan
            ]

-- | Apply Select to an opaque runtime Boolean index.
selected :: Type -> Type
selected index = mkTyConApp (fst selectFamily) [index]

-- | Relate Select at a runtime index to the concrete branch payload type.
selectionCo :: Int -> CoVar -> Coercion
selectionCo branch cv =
    mkSubCo $
        mkTransCo
            (mkTyConAppCo Nominal (fst selectFamily) [mkCoVarCo cv])
            (mkAxInstCo Nominal (BranchedAxiom (snd selectFamily) branch) [] [])

-- | Build choose with checked constructor evidence and ordinary family axioms.
choose :: CoreExpr -> CoreExpr
choose intPayload = Lam b (Case (Var b) caseBinder (selected index) alternatives)
  where
    b = local 10 "choose_b" boolTy
    index = TermVarTy b
    caseBinder = local 11 "choose_case" boolTy
    falseProof = proof 12 index (boolIndex False)
    trueProof = proof 13 index (boolIndex True)
    alternatives =
        [ Alt (DataAlt falseDataCon) [falseProof] (Cast (Var (dataConWorkId falseDataCon)) (mkSymCo (selectionCo 1 falseProof)))
        , Alt (DataAlt trueDataCon) [trueProof] (Cast intPayload (mkSymCo (selectionCo 0 trueProof)))
        ]

-- | Build retained keep without a runtime occurrence of its Boolean binder.
keepSelect :: CoreExpr
keepSelect = Lam b (Lam value (Var value))
  where
    b = local 20 "keep_b" boolTy
    value = local 21 "value" (selected (TermVarTy b))

-- | Consume the selected payload under evidence from the same runtime Boolean.
selectionKernel :: Bool -> CoreExpr -> Id -> Id -> CoreExpr
selectionKernel shared intPayload observeInt observeBool =
    Lam b
        $ Let (NonRec chooseId chooseExpr)
        $ Let (NonRec keepId keepSelect)
        $ Let (NonRec etaKeepId etaKeep)
        $ Let (NonRec value chosen)
        $ Case (Var b) caseBinder ioStringType alternatives
  where
    b = local 1 "runtime_b" boolTy
    index = TermVarTy b
    chooseExpr = choose intPayload
    chooseId = local 30 "choose" (exprType chooseExpr)
    keepId = local 31 "keep" (exprType keepSelect)
    etaKeep = etaExpand 2 (Var keepId)
    etaKeepId = local 33 "eta_keep" (exprType etaKeep)
    value = local 32 "selected" (selected index)
    chosenValue = App (Var chooseId) (Var b)
    chosen
        | shared = App (App (Var etaKeepId) (Var b)) chosenValue
        | otherwise = chosenValue
    caseBinder = local 40 "observe_case" boolTy
    falseProof = proof 41 index (boolIndex False)
    trueProof = proof 42 index (boolIndex True)
    ioStringType = funResultTy (idType observeInt)
    alternatives =
        [ Alt (DataAlt falseDataCon) [falseProof] (App (Var observeBool) (Cast (Var value) (selectionCo 1 falseProof)))
        , Alt (DataAlt trueDataCon) [trueProof] (App (Var observeInt) (Cast (Var value) (selectionCo 0 trueProof)))
        ]

-- | Observe partial keep at WHNF without constructing a Select-bottom value.
partialKeepKernel :: Id -> CoreExpr
partialKeepKernel force = Lam b (Let (NonRec keepId keepSelect) observation)
  where
    b = local 1 "runtime_b" boolTy
    keepId = local 31 "keep" (exprType keepSelect)
    resultType = mkFunctionType ManyTy (selected (TermVarTy b)) (selected (TermVarTy b))
    observation = mkApps (Var force) [Type resultType, App (Var keepId) (Var b)]

-- | Keep a lazy local index whose producer is a global Boolean thunk.
localIndexKeep :: Id -> Id -> CoreExpr
localIndexKeep force producer =
    Let (NonRec b (Var producer)) (Let (NonRec keepId keepSelect) observation)
  where
    b = local 60 "local_index" boolTy
    keepId = local 61 "keep" (exprType keepSelect)
    resultType = mkFunctionType ManyTy (selected (TermVarTy b)) (selected (TermVarTy b))
    observation = mkApps (Var force) [Type resultType, App (Var keepId) (Var b)]

-- | Preserve a lazy local index used only in erased annotations.
typeOnlyLocalIndex :: CoreExpr -> Id -> Id -> CoreExpr
typeOnlyLocalIndex intPayload force producer =
    Let (NonRec b (Var producer)) observation
  where
    b = local 70 "type_only_index" boolTy
    index = TermVarTy b
    dictionary = local 71 "dictionary" (mkTyConApp eqTyCon [boolTy, index, index])
    value = Lam dictionary intPayload
    observation = mkApps (Var force) [Type (indexed index), value]

-- | Execute a constructor application that permits checked case reduction.
literalSelection :: Bool -> CoreExpr -> Id -> Id -> CoreExpr
literalSelection value intPayload observeInt observeBool =
    App (Var observer) (Cast selectedValue reduction)
  where
    constructor = if value then trueDataCon else falseDataCon
    branch = if value then 0 else 1
    observer = if value then observeInt else observeBool
    selectedValue = App (choose intPayload) (Var (dataConWorkId constructor))
    reduction = mkAxInstCo Representational (BranchedAxiom (snd selectFamily) branch) [] []

-- | Observe that choose evaluates its Boolean argument.
strictChooseKernel :: CoreExpr -> Id -> CoreExpr
strictChooseKernel intPayload force = Lam b (Let (NonRec chooseId chooseExpr) observation)
  where
    b = local 1 "runtime_b" boolTy
    chooseExpr = choose intPayload
    chooseId = local 30 "choose" (exprType chooseExpr)
    observation = mkApps (Var force) [Type (selected (TermVarTy b)), App (Var chooseId) (Var b)]

-- | Build an indexed function type inhabited without constructor information.
indexed :: Type -> Type
indexed index = mkFunctionType ManyTy (mkTyConApp eqTyCon [boolTy, index, index]) intTy

-- | Apply retained keep fully and consume its result using reflexive evidence.
dictionaryKeepKernel :: CoreExpr -> Id -> CoreExpr
dictionaryKeepKernel intPayload observeInt = Lam b (Let (NonRec keepId keepExpr) observation)
  where
    b = local 1 "runtime_b" boolTy
    index = TermVarTy b
    keepBool = local 50 "keep_b" boolTy
    keepValue = local 51 "value" (indexed (TermVarTy keepBool))
    keepExpr = Lam keepBool (Lam keepValue (Var keepValue))
    keepId = local 52 "keep_indexed" (exprType keepExpr)
    dictionary = local 53 "dictionary" (mkTyConApp eqTyCon [boolTy, index, index])
    value = Lam dictionary intPayload
    reflexive = mkCoreConApps eqDataCon [Type boolTy, Type index, Type index, Coercion (mkNomReflCo index)]
    kept = App (App (Var keepId) (Var b)) value
    observation = App (Var observeInt) (App kept reflexive)

-- | Lint a closed native expression before it reaches the backend API.
lintNative :: DynFlags -> String -> CoreExpr -> IO ()
lintNative flags label expression = do
    unless (hasNativeDependentExpr expression) (fail ("FAIL: " ++ label ++ "; native forms disappeared before compilation"))
    case lintExpr (initLintConfig flags []) expression of
        Nothing -> pure ()
        Just errors -> fail ("FAIL: " ++ label ++ "; Lint rejected:\n" ++ showSDoc flags (vcat (bagToList errors)))

-- | Compile and observe a checked IO String expression without a host cast.
execute :: HscEnv -> DynFlags -> String -> CoreExpr -> IO String
execute environment flags label expression = do
    lintNative flags label expression
    (value, _, _) <- hscCompileCoreExpr environment noSrcSpan expression
    evalString (hscInterp environment) value

-- | Check runtime results and distinguish both keep laziness observations.
runChecks :: GHC.Ghc ()
runChecks = do
    bindings <- loadHelpers
    flags <- GHC.getSessionDynFlags
    environment <- GHC.getSession
    let drive = helper bindings "drive"
        driveBoth = helper bindings "driveBoth"
        observeInt = helper bindings "observeInt"
        observeBool = helper bindings "observeBool"
        force = helper bindings "force"
        bottomBool = helper bindings "bottomBool"
        payload = mkIntExpr (targetPlatform flags) 42
        run kernel seed = mkApps (Var drive) [kernel, seed]
        expect label expected expression = do
            actual <- execute environment flags label expression
            unless (actual == expected) (fail ("FAIL: " ++ label ++ "; expected " ++ show expected ++ ", got " ++ show actual))
            putStrLn (label ++ ": " ++ actual)
        expectBoth label kernel = do
            actual <- execute environment flags label (App (Var driveBoth) kernel)
            case lines actual of
                [trueResult, falseResult] -> do
                    unless (trueResult == "42" && falseResult == "False") $
                        fail ("FAIL: " ++ label ++ "; runtime results were " ++ show actual)
                    putStrLn (label ++ " True: " ++ trueResult)
                    putStrLn (label ++ " False: " ++ falseResult)
                _ -> fail ("FAIL: " ++ label ++ "; expected two runtime results, got " ++ show actual)
    liftIO $ do
        expectBoth "choose" (selectionKernel False payload observeInt observeBool)
        expectBoth "keep/choose" (selectionKernel True payload observeInt observeBool)
        expect "choose literal True" "42" (literalSelection True payload observeInt observeBool)
        expect "choose literal False" "False" (literalSelection False payload observeInt observeBool)
        expect "keepSelect bottom (partial WHNF)" "forced" (run (partialKeepKernel force) (Var bottomBool))
        expect "keepSelect local bottom (partial WHNF)" "forced" (localIndexKeep force bottomBool)
        expect "type-only local bottom (function WHNF)" "forced" (typeOnlyLocalIndex payload force bottomBool)
        case typeOnlyLocalIndex payload force bottomBool of
            Let (NonRec binder producer) body ->
                expect "type-only producer beta (function WHNF)" "forced" (App (Lam binder body) producer)
            _ -> fail "FAIL: expected a local index in the type-only fixture"
        expect "keep indexed bottom (fully applied)" "42" (run (dictionaryKeepKernel payload observeInt) (Var bottomBool))
        result <- try (execute environment flags "choose bottom" (run (strictChooseKernel payload force) (Var bottomBool))) :: IO (Either SomeException String)
        case result of
            Left exception
                | "native-bottom-bool" `isInfixOf` displayException exception -> putStrLn "choose bottom: raised native-bottom-bool"
                | otherwise -> fail ("FAIL: choose bottom raised another exception: " ++ displayException exception)
            Right actual -> fail ("FAIL: choose bottom returned " ++ show actual)
    runSourceChecks
    runSourceRejectChecks
    liftIO $ putStrLn "native-core-run-ok"
