module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Maybe (isJust, isNothing)
import GHC hiding (exprType)
import GHC.Builtin.KnownOccs (returnIOIdOcc)
import GHC.Builtin.Modules (gHC_INTERNAL_BASE)
import GHC.Builtin.WiredIn.Types
    (boolTy, charTy, falseDataCon, trueDataCon, liftedTypeKind, manyDataConTy, maybeTyCon, mkBoxedTupleTy, mkListTy, promotedTrueDataCon, trueDataConId)
import GHC.Core
    (Alt (..), AltCon (DEFAULT, DataAlt), Bind (NonRec, Rec), CoreExpr, Expr (..), InScopeEnv (ISE), noUnfolding,
     Unfolding (CoreUnfolding, uf_tmpl), UnfoldingCache (..), UnfoldingSource (VanillaSrc), mkApps, mkLams)
import GHC.Core.Coercion (liftCoSubst, mkCoVarCo, mkLiftingContext, mkSubCo)
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Core.Dependent (hasExistentialExpr, supportsNativeGentle)
import GHC.Core.Rules (matchExprs)
import GHC.Core.FamInstEnv (emptyFamInstEnv)
import GHC.Core.Opt.Arity (exprIsDeadEnd)
import GHC.Core.Opt.LiberateCase (LibCaseOpts(lco_threshold), liberateCase)
import GHC.Driver.Config.Core.Opt.LiberateCase (initLiberateCaseOpts)
import GHC.Core.Opt.Exitify (exitifyProgram)
import GHC.Core.Opt.CallArity (callArityRHS)
import GHC.Core.Opt.OccurAnal (SimplCoreMode(RetainedCore))
import GHC.Core.Opt.WorkWrap (wwTopBinds)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Map.Expr (emptyCoreMap, eqCoreExpr, extendCoreMap, lookupCoreMap)
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.Make (mkCharExpr, mkListExpr, mkNothingExpr, mkRuntimeErrorApp, pAT_ERROR_ID)
import GHC.Core.SimpleOpt (simpleOptNativeExpr)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Type (ExistsTy, TermVarTy, TyVarTy))
import GHC.Core.Type (mkTyConApp, mkVisFunTyManyU)
import GHC.Core.Utils (exprType)
import GHC.CoreToStg (coreToStg)
import GHC.CoreToStg.Prep (corePrepExpr)
import GHC.CoreToIface (toIfaceExpr, toIfaceType)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Config.Core.Opt.WorkWrap (initWorkWrapOpts)
import GHC.Driver.Config.CoreToStg (initCoreToStgOpts)
import GHC.Driver.Config.CoreToStg.Prep (initCorePrepConfigFor)
import GHC.Driver.Env (hscInterp)
import GHC.Driver.Env.Types (HscEnv (hsc_NC, hsc_dflags, hsc_logger))
import GHC.Driver.Main (hscCompileCoreExpr')
import GHC.Driver.Session (gopt_set, targetProfile)
import GHC.Iface.Binary
import GHC.Iface.Env (lookupOrig)
import GHC.Iface.Syntax
    (IfGuidance (IfWhen), IfaceDecl (IfaceId), IfaceIdDetails (IfVanillaId),
     IfaceInfoItem (HsUnfold), IfaceUnfolding (IfCoreUnfold))
import GHC.IfaceToCore (tcIfaceDecl, tcIfaceGlobal)
import GHC.Runtime.Interpreter (evalString)
import GHC.Stg.Syntax (GenStgBinding (StgNonRec), GenStgExpr (StgApp, StgCase),
                       StgArg (StgVarArg), GenStgRhs (StgRhsClosure), GenStgTopBinding (StgTopLifted), StgTopBinding)
import GHC.Tc.Utils.Monad (initIfaceLcl, initIfaceLoad)
import GHC.Types.Id (asJoinId, isJoinId, setIdArity, mkExportedVanillaId, mkSysLocal, realIdUnfolding)
import GHC.Types.Name (mkExternalName, mkSysTvName, mkSystemName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Unique (Unique, UniqueTag (PluginTag))
import GHC.Types.Unique.Supply (mkSplitUniqSupply, uniqsFromSupply)
import GHC.Types.Var (ExistsTyFlag (ExplicitExists, ImplicitExists), isTyVar, mkCoVar, mkTyVar, varType)
import GHC.Types.Var.Env (mkInScopeSetList)
import GHC.Unit.Module (IsBootInterface (NotBoot), mkModule)
import GHC.Unit.Module.ModIface (emptyFullModIface, mi_decls, set_mi_decls)
import GHC.Unit.Types (mainUnit)
import GHC.Utils.Fingerprint (fingerprint0)
import GHC.Utils.Outputable (text)
import System.Environment (getArgs)

data Example = Ignore | Force | Defined | ForcePack | Computed | EmitForce

main :: IO ()
main = do
    [libdir, interfaceFile] <- getArgs
    let run :: Bool -> Example -> IO (Either SomeException String)
        run serialized example = try $ do
            -- The producer and consumer have distinct HscEnvs and NameCaches.
            if serialized then session libdir $ \env response ->
                writeConsumerFixture env interfaceFile example response
              else pure ()
            session libdir $ \env response -> do
                expression <- construct (loadConsumer env interfaceFile) env example response
                (closure, _, _) <- hscCompileCoreExpr' env noSrcSpan expression
                evalString (hscInterp env) closure
        returns serialized example label = do
            actual <- run serialized example
            case actual of
              Right "body result" -> putStrLn label
              _ -> fail (label ++ ": " ++ show actual)
        throws serialized example label = do
            actual <- run serialized example
            case actual of
              Left ex | "demanded existential payload" `isInfixOf` displayException ex ->
                putStrLn label
              _ -> fail (label ++ ": " ++ show actual)
    returns True Ignore "imported Unpack ignores a bottom package argument"
    throws True Force "imported Unpack demands the payload only when its body does"
    returns True Defined "imported Unpack returns the body result for a defined package"
    throws False ForcePack "demanding Pack observes payload bottom"
    returns False Computed "Unpack leaves a computed package unevaluated"
    checkLiftingReadback libdir (interfaceFile ++ ".lift")
    checkPackageOptimizers libdir

-- Check package scope in SimpleOpt and worker/wrapper before native lowering.
checkPackageOptimizers :: FilePath -> IO ()
checkPackageOptimizers libdir = session libdir $ \env _ -> do
    schemaId : witnessId : payloadId : topId : otherWitnessId : otherPayloadId : _ <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let tv unique name = mkTyVar (mkSysTvName unique (fsLit name)) liftedTypeKind
        schemaArg = tv schemaId "a"
        witness = tv witnessId "witness"
        payload = mkSysLocal (fsLit "payload") payloadId manyDataConTy (TyVarTy witness)
        true = Var trueDataConId
        package = Pack (ExistsTy ExplicitExists schemaArg (TyVarTy schemaArg)) boolTy true
        ignored = simpleOptNativeExpr (Unpack witness payload package true)
        escaped = Unpack witness payload package (Var payload)
        escaped' = simpleOptNativeExpr escaped
        lint = lintExpr (initLintConfig (hsc_dflags env) [])
    unless (not (hasExistentialExpr ignored) && isNothing (lint ignored)) $
        fail "The well-scoped dead payload survived SimpleOpt"
    unless (hasExistentialExpr escaped' && isJust (lint escaped') && isJust (lint escaped)) $
        fail "SimpleOpt hid an escaping existential witness"
    putStrLn "adjacent package reduction preserves witness scope"
    supply <- mkSplitUniqSupply PluginTag
    let mdl = mkModule mainUnit (mkModuleName "PackageWorker")
        opts = initWorkWrapOpts mdl (hsc_dflags env) (emptyFamInstEnv, emptyFamInstEnv)
        top = mkSysLocal (fsLit "top") topId manyDataConTy boolTy
        rhs = Unpack witness payload package true
        called = callArityRHS rhs
    unless (hasExistentialExpr called && isNothing (lint called)) $
        fail "call arity changed the package or its scope"
    putStrLn "call arity preserves the scope of an opened package"
    outerId : loopId : argId : qId : rId : _ <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let local u name ty = mkSysLocal (fsLit name) u manyDataConTy ty
        outer = local outerId "outer" boolTy
        loopTy = mkVisFunTyManyU boolTy boolTy
        loop = setIdArity (local loopId "loop" loopTy) 1
        arg = local argId "arg" boolTy
        call = App (Var loop) (Var arg)
        inspect v u = Case (Var v) (local u "forced" (varType v)) boolTy [Alt DEFAULT [] call]
        hidden = Pack (ExistsTy ExplicitExists schemaArg (TyVarTy schemaArg)) boolTy (inspect outer qId)
        recBody = Lam arg (Unpack witness payload hidden (inspect payload rId))
        recursive = Lam outer (Let (Rec [(loop, recBody)]) (App (Var loop) true))
    unless (isNothing (lint recursive)) $ fail "Invalid liberate-case fixture"
    -- Size calculation treats a package as too large to duplicate.
    -- Remove the threshold here to exercise the recursive-call transformation.
    let libOpts = (initLiberateCaseOpts (hsc_dflags env)) { lco_threshold = Nothing }
    case liberateCase libOpts [NonRec (local topId "f" loopTy) recursive] of
      [NonRec _ out@(Lam _ (Let (Rec [(_, Lam _ (Unpack _ _ (Pack _ _ packed) body))]) _))]
        | Case _ _ _ [Alt DEFAULT [] (Let Rec{} _)] <- packed
        , Case _ _ _ [Alt DEFAULT [] App{}] <- body
        , isNothing (lint out) ->
          putStrLn "liberate-case distinguishes an outer variable from a hidden payload"
      _ -> fail "liberate-case lost the scope of an opened package"
    case wwTopBinds RetainedCore opts supply [NonRec top called] of
      [NonRec _ result@(Unpack _ _ Pack{} _)] | isNothing (lint result) ->
        putStrLn "worker/wrapper preserves the scope of an opened package"
      _ -> fail "worker/wrapper changed the package or its scope"

    let failure = mkRuntimeErrorApp pAT_ERROR_ID boolTy "demanded hidden Bool"
        deadPack = Pack (ExistsTy ExplicitExists schemaArg (TyVarTy schemaArg)) boolTy failure
    unless (exprIsDeadEnd deadPack && not (exprIsDeadEnd (Unpack witness payload deadPack true))
            && exprIsDeadEnd (Unpack witness payload package failure)) $
        fail "the dead-end check changed the demand of a package"
    putStrLn "the dead-end check distinguishes a lazy opening from a demanded payload"
    -- Make the map branch on both forms. Two singleton lookups alone
    -- exercise only the equality shortcut in GenMap.
    let schema = ExistsTy ExplicitExists schemaArg (TyVarTy schemaArg)
        constantSchema = ExistsTy ExplicitExists schemaArg boolTy
        constant w = Pack constantSchema w true
        repack w x = Unpack w x package (Pack schema (TyVarTy w) (Var x))
        other = tv otherWitnessId "other"
        renamedPayload = mkSysLocal (fsLit "otherPayload") otherPayloadId manyDataConTy (TyVarTy other)
        orig = repack witness payload
        renamed = repack other renamedPayload
        examples = [(package, 1 :: Int), (constant boolTy, 2), (constant charTy, 3), (orig, 4)]
        table = foldl (\m (e, value) -> extendCoreMap m e value) emptyCoreMap examples
    unless (eqCoreExpr orig renamed && lookupCoreMap table renamed == Just 4
            && all (\(e, value) -> lookupCoreMap table e == Just value) examples) $
        fail "expression map confused a package schema, witness or scoped payload"
    putStrLn "expression keys distinguish erased witnesses and identify alpha-renamed openings"
    let hole = mkSysLocal (fsLit "hole") topId manyDataConTy schema
        template = Unpack witness payload package (Var hole)
        closed = Unpack other renamedPayload package package
        rule_env = ISE (mkInScopeSetList [trueDataConId]) (const noUnfolding)
        matches vars lhs rhs = isJust (matchExprs rule_env vars [lhs] [rhs])
    unless (matches [] orig renamed && not (matches [] (constant boolTy) (constant charTy))) $
        fail "Rule matching lost the witness or confused alpha-renamed openings"
    unless (matches [hole] template closed && not (matches [hole] template renamed)) $
        fail "A rule substitution captured an opened witness or payload"
    let openSchema elemTy = ExistsTy ImplicitExists schemaArg
              (mkBoxedTupleTy [TyVarTy schemaArg, elemTy])
        element = TyVarTy witness
        knownElement chosen target = isJust (matchExprs rule_env [witness]
              [Type element, Type (openSchema element)] [Type chosen, Type (openSchema target)])
        capturedElement = isJust (matchExprs rule_env [witness]
              [Type element, Lam schemaArg (Type (openSchema element))]
              [Type (TyVarTy other), Lam other (Type (openSchema (TyVarTy other)))])
    unless (knownElement (TyVarTy other) (TyVarTy other)
            && not capturedElement
            && not (knownElement boolTy charTy)
            && not (matches [witness] (Type (openSchema element)) (Type (openSchema boolTy)))) $
        fail "A rule must fix the element type before comparing whole existential types"
    putStrLn "erased package rules match bound variables and reject escaping substitutions"

    indexId : otherIndexId : polyIndexId : _ <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let index = local indexId "index" boolTy
        otherIndex = local otherIndexId "otherIndex" boolTy
        polyIndex = local polyIndexId "polyIndex" (TyVarTy witness)
        indices = ISE (mkInScopeSetList [index, outer, otherIndex, trueDataConId]) (const noUnfolding)
        matchAt value indexTy = isJust (matchExprs indices [index]
              [Var index, Type (TermVarTy index)] [value, Type indexTy])
        matchPoly valueIndex = isJust (matchExprs indices [witness, polyIndex]
              [Type (TyVarTy witness), Var polyIndex, Type (TermVarTy polyIndex)]
              [Type boolTy, Var outer, Type (TermVarTy valueIndex)])
    unless (matchAt (Var outer) (TermVarTy outer) && matchAt true (mkTyConApp promotedTrueDataCon [])
            && matchAt (Var index) (TermVarTy index) && not (matchAt (Var outer) (TermVarTy index))
            && matchPoly outer && not (matchPoly otherIndex)) $
        fail "a rule must reuse the earlier runtime argument at its instantiated domain"
    putStrLn "retained rules distinguish shared, distinct and shadowed indices"

    -- A lifted exit uses the abstract payload in a strict case. Its type
    -- witness must precede the payload in the new join's telescope.
    ids <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let fallbackId : loopId : switchId : choiceId : forcedId : _ = ids
        local name u ty = mkSysLocal (fsLit name) u manyDataConTy ty
        fallback = local "fallback" fallbackId boolTy
        boolFun = mkVisFunTyManyU boolTy boolTy
        loop = asJoinId (local "loop" loopId boolFun) 1
        switch = local "switch" switchId boolTy
        choice = local "choice" choiceId boolTy
        forced = local "forced" forcedId (varType payload)
        exit = Case (Var payload) forced boolTy [Alt DEFAULT [] (Var fallback)]
        jump = App (Var loop) true
        branch = Case (Var switch) choice boolTy
                   [Alt (DataAlt falseDataCon) [] jump, Alt (DataAlt trueDataCon) [] exit]
        loopRhs = Lam switch (Unpack witness payload package branch)
        program = Lam fallback (Let (Rec [(loop, loopRhs)]) jump)
    unless (isNothing (lint program)) $ fail "Invalid package loop fixture"
    case exitifyProgram [NonRec (local "select" topId boolFun) program] of
      [NonRec _ result@(Lam _ (Let (NonRec join (Lam w (Lam x _))) _))]
        | isJoinId join, isTyVar w, varType x `eqType` TyVarTy w
        , isNothing (lint result) ->
          putStrLn "exitification abstracts the hidden witness before its payload"
      _ -> fail "exitification lost the hidden witness or failed to lift the exit"

-- | Restore a package cast whose only evidence for its free parameter is a Given.
checkLiftingReadback :: FilePath -> FilePath -> IO ()
checkLiftingReadback libdir path = forM_ [(Nominal, mkNomEqPred), (Representational, mkReprEqPred)] $
  \(role, predicate) -> do
    ids <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let l:r:w:t:c:x:top:_ = ids
        tv u n = mkTyVar (mkSysTvName u (fsLit n)) liftedTypeKind
        lhs = tv l "l"
        rhs = tv r "r"
        witness = tv w "w"
        varying = tv t "t"
        schema el = ExistsTy ExplicitExists witness (mkBoxedTupleTy [TyVarTy witness, TyVarTy el])
        given = mkCoVar (mkSystemName c (mkVarOcc "cv")) (predicate (TyVarTy lhs) (TyVarTy rhs))
        co = liftCoSubst role (mkLiftingContext [(varying, mkCoVarCo given)]) (schema varying)
        box = mkSysLocal (fsLit "box") x manyDataConTy (schema lhs)
        core = mkLams [lhs, rhs, given, box] (Cast (Var box) (if role == Nominal then mkSubCo co else co))
        transport = mkSysLocal (fsLit "transport") top manyDataConTy (exprType core)
    unless (supportsNativeGentle [NonRec transport core]) $
        fail "the retained optimizer rejected a checked existential coercion"
    session libdir $ \env _ -> writeConsumer env top path core
    session libdir $ \env _ -> do
        imported <- loadConsumer env path top core
        unless (exprType imported `eqType` exprType core &&
                isNothing (lintExpr (initLintConfig (hsc_dflags env) []) imported)) $
            fail "existential congruence changed role or scope across an interface"

session :: FilePath -> (HscEnv -> CoreExpr -> IO a) -> IO a
session libdir action = runGhc (Just libdir) $ do
    flags <- getSessionDynFlags
    let interactive = flags {backend = bytecodeBackend, ghcLink = LinkInMemory}
    void (setSessionDynFlags (foldl gopt_set interactive
          [Opt_ExternalInterpreter, Opt_DoCoreLinting, Opt_DoStgLinting]))
    env <- getSession
    liftIO (knownResponse env >>= action env)

writeConsumerFixture :: HscEnv -> FilePath -> Example -> CoreExpr -> IO ()
writeConsumerFixture env path example response =
    void (construct write env emit response)
  where
    emit = case example of { Force -> EmitForce; _ -> example }
    write u core = writeConsumer env u path core >> pure core

construct :: (Unique -> CoreExpr -> IO CoreExpr) -> HscEnv -> Example -> CoreExpr -> IO CoreExpr
construct load env example response = do
    us <- uniqsFromSupply <$> mkSplitUniqSupply PluginTag
    let ix : a : p : x : w : f : provider : arg : top : _ = us
        tv u nm = mkTyVar (mkSysTvName u (fsLit nm)) liftedTypeKind
        local u nm = mkSysLocal (fsLit nm) u manyDataConTy
        param = tv ix "param"
        witness = tv a "witness"
        maybeOf ty = mkTyConApp maybeTyCon [ty]
        schema = ExistsTy ExplicitExists param (maybeOf (TyVarTy param))
        input = local p "arbitraryPackage" schema
        value = local x "payload" (maybeOf (TyVarTy witness))
        result = exprType response
        demand name ty argument = Case argument (local w name ty) result [Alt DEFAULT [] response]
        inspect = demand "evaluatedPayload" (varType value) (Var value)
        bottom = mkRuntimeErrorApp pAT_ERROR_ID (maybeOf boolTy) "demanded existential payload"
        packed payload = Pack schema boolTy payload
        package = packed bottom
        direct body = Lam input (Unpack witness value (Var input) body)
        bindApply function actual =
            let name = local f "importedConsumer" (exprType function)
            in Let (NonRec name function) (App (Var name) actual)
        makeComputed =
            let producerBody = Lam (local arg "ignored" boolTy) package
                name = local provider "producer" (exprType producerBody)
            in Let (NonRec name producerBody)
                   (Unpack witness value (App (Var name) (Var trueDataConId)) response)
        consumer checkedBody = load top (direct checkedBody)
    expression <- case example of
        Ignore -> do fn <- consumer response
                     pure (bindApply fn package)
        Force -> do fn <- consumer inspect
                    checkNamedErasure env top fn
                    let receiver = local f "receiver" (exprType fn)
                        supplied = local provider "supplied" (maybeOf boolTy)
                    checkArgumentErasure env top $
                      Lam receiver (Lam supplied (App (Var receiver) (packed (Var supplied))))
                    pure (bindApply fn package)
        Defined -> do fn <- consumer inspect
                      pure (bindApply fn (packed (mkNothingExpr boolTy)))
        EmitForce -> do fn <- consumer inspect
                        pure (bindApply fn package)
        ForcePack -> pure (demand "evaluatedPack" schema package)
        Computed -> pure makeComputed
    unless (isNothing (lintExpr (initLintConfig (hsc_dflags env) []) expression)) $
        fail "input expression did not pass Core Lint"
    pure expression

writeConsumer :: HscEnv -> Unique -> FilePath -> CoreExpr -> IO ()
writeConsumer env u path core = do
    let mdl = mkModule mainUnit (mkModuleName "ExistentialConsumer")
        name = mkExternalName u mdl (mkVarOcc "consumer") noSrcSpan
        cache = UnfoldingCache False False False False
        unfolding = HsUnfold False (IfCoreUnfold VanillaSrc cache (IfWhen 1 False False) (toIfaceExpr core))
        entry = IfaceId name (toIfaceType (exprType core)) IfVanillaId [unfolding]
        iface = set_mi_decls [(fingerprint0, entry)] (emptyFullModIface mdl)
        profile = targetProfile (hsc_dflags env)
    writeBinIface profile QuietBinIFace NormalCompression path iface

loadConsumer :: HscEnv -> FilePath -> Unique -> CoreExpr -> IO CoreExpr
loadConsumer env path _ _ = do
    let mdl = mkModule mainUnit (mkModuleName "ExistentialConsumer")
        profile = targetProfile (hsc_dflags env)
    decoded <- readBinIface profile (hsc_NC env) CheckHiWay QuietBinIFace path
    case mi_decls decoded of
      [(_, signature@IfaceId{})] -> do
        thing <- initIfaceLoad env $
          initIfaceLcl mdl (text "source-free existential consumer") NotBoot (tcIfaceDecl False signature)
        case thing of
          AnId imported -> case realIdUnfolding imported of
            CoreUnfolding {uf_tmpl = template} -> do
              body <- evaluate template
              unless (exprType body `eqType` varType imported) (fail "changed imported signature")
              pure body
            _ -> fail "missing consumer unfolding in interface"
          _ -> fail "missing consumer name in interface"
      _ -> fail "missing consumer unfolding in interface"

-- An already named package binds no intermediate STG payload object.
checkNamedErasure :: HscEnv -> Unique -> CoreExpr -> IO ()
checkNamedErasure env u function = do
    stg <- lowerFunction env u function
    case stg of
      [StgTopLifted (StgNonRec _ (StgRhsClosure _ _ _ [p] (StgCase (StgApp actual []) _ _ _) _))]
        | p == actual -> putStrLn "named Unpack erases to the original STG Id"
      _ -> fail "Unpack inserted an STG wrapper or changed the demanded variable"

checkArgumentErasure :: HscEnv -> Unique -> CoreExpr -> IO ()
checkArgumentErasure env u function = do
    stg <- lowerFunction env u function
    case stg of
      [StgTopLifted (StgNonRec _ (StgRhsClosure _ _ _ [receiver, supplied]
                                               (StgApp call [StgVarArg actual]) _))]
        | receiver == call && supplied == actual ->
            putStrLn "Pack passes the original payload as an STG argument"
      _ -> fail "Pack inserted an STG wrapper or changed the argument variable"

lowerFunction :: HscEnv -> Unique -> CoreExpr -> IO [StgTopBinding]
lowerFunction env u function = do
    cfg <- initCorePrepConfigFor True env
    prepared <- corePrepExpr (hsc_logger env) cfg function
    let opts = initCoreToStgOpts (hsc_dflags env)
        mdl = mkModule mainUnit (mkModuleName "ExistentialStg")
        name = mkExternalName u mdl (mkVarOcc "consumer") noSrcSpan
        binding = mkExportedVanillaId name (exprType function)
    (stg, _, _) <- coreToStg opts mdl (error "unexpected file lookup") [NonRec binding prepared]
    pure stg

-- Build the exact IO String action used by evalString. The generic GHCi
-- statement wrapper would introduce an unrelated compulsory unsafeCoerce#.
knownResponse :: HscEnv -> IO CoreExpr
knownResponse env = do
    thing <- initIfaceLoad env $
      initIfaceLcl gHC_INTERNAL_BASE (text "existential test response") NotBoot $
        lookupOrig gHC_INTERNAL_BASE returnIOIdOcc >>= tcIfaceGlobal
    case thing of
      AnId ret -> pure (mkApps (Var ret)
                    [Type (mkListTy charTy), mkListExpr charTy (map mkCharExpr "body result")])
      _ -> fail "missing returnIO"
