module Main (main) where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import GHC hiding (exprType)
import GHC.Builtin.KnownOccs (returnIOIdOcc)
import GHC.Builtin.Modules (gHC_INTERNAL_BASE)
import GHC.Builtin.WiredIn.Types
    (boolTy, charTy, liftedTypeKind, manyDataConTy, maybeTyCon, mkBoxedTupleTy, mkListTy, trueDataConId)
import GHC.Core
    (Alt (..), AltCon (DEFAULT), Bind (NonRec), CoreExpr, Expr (..),
     Unfolding (CoreUnfolding, uf_tmpl), UnfoldingCache (..), UnfoldingSource (VanillaSrc), mkApps, mkLams)
import GHC.Core.Coercion (liftCoSubst, mkCoVarCo, mkLiftingContext, mkSubCo)
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Core.Lint (lintExpr)
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.Make (mkCharExpr, mkListExpr, mkNothingExpr, mkRuntimeErrorApp, pAT_ERROR_ID)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Type (ExistsTy, TyVarTy))
import GHC.Core.Type (mkTyConApp)
import GHC.Core.Utils (exprType)
import GHC.CoreToStg (coreToStg)
import GHC.CoreToStg.Prep (corePrepExpr)
import GHC.CoreToIface (toIfaceExpr, toIfaceType)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
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
import GHC.Types.Id (mkExportedVanillaId, mkSysLocal, realIdUnfolding)
import GHC.Types.Name (mkExternalName, mkSysTvName, mkSystemName)
import GHC.Types.Name.Occurrence (mkVarOcc)
import GHC.Types.SrcLoc (noSrcSpan)
import GHC.Types.Unique (Unique, UniqueTag (PluginTag))
import GHC.Types.Unique.Supply (mkSplitUniqSupply, uniqsFromSupply)
import GHC.Types.Var (ExistsTyFlag (ExplicitExists), mkCoVar, mkTyVar, varType)
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
