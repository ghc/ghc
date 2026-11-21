{-# LANGUAGE GHC2024 #-}
module Main (main) where

import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import qualified GHC
import GHC.Driver.Env (hsc_plugins)
import GHC.Driver.Monad (modifySession)
import System.Environment (getArgs)
import GHC.Builtin.WiredIn.Types (unmatchableDataConTy)
import GHC.Core.Lint
import GHC.Core.Predicate (mkNomEqPred, mkReprEqPred)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Coercion (InstCo))
import GHC.Core.Utils (exprType)
import GHC.Data.Bag (bagToList)
import GHC.Driver.Config.Diagnostic (initDiagOpts)
import GHC.Plugins
import GHC.Tc.Utils.TcType (noConcreteTyVars)
import GHC.Types.Id.Make (mkDataConWorkId, vanillaDataConBoxer)
import GHC.Types.Var (mkCoVar)

-- Install the Core pass in the same GHC library that supplies Core Lint.
main :: IO ()
main = do
    [libdir] <- getArgs
    GHC.runGhc (Just libdir) $ do
        flags <- GHC.getSessionDynFlags
        _ <- GHC.setSessionDynFlags (gopt_set flags Opt_ForceRecomp)
        modifySession $ \env ->
            let plugins = hsc_plugins env
            in env { hsc_plugins = plugins { staticPlugins = [StaticPlugin (PluginWithArgs plugin []) False] } }
        target <- GHC.guessTarget "CoercionForallWitness.hs" Nothing Nothing
        GHC.setTargets [target]
        result <- GHC.load GHC.LoadAllTargets
        case result of
            Failed -> liftIO (fail "Failed to check the witness module")
            Succeeded -> pure ()

plugin :: Plugin
plugin =
    defaultPlugin
        { installCoreToDos = \_ passes -> pure (CoreDoPluginPass "Lint equality telescope" check : passes)
        , pluginRecompile = purePlugin
        }

check :: ModGuts -> CoreM ModGuts
check guts = do
    flags <- getDynFlags
    let [index] = filter ((== "Index") . getOccString) (mg_tcs guts)
        binders = tyConBinders heqTyCon
        universals = binderVars binders
        [k1, k2, a, b] = mkTyVarTys universals
        kindProof = mkNomEqPred k1 k2
        indexProof ck = mkNomEqPred (mkCastTy a (mkCoVarCo ck)) b
        left = mkTyConApp index [k1, a]
        right = mkTyConApp index [k2, b]
        cfg =
            LintConfig
                { l_diagOpts = initDiagOpts flags
                , l_platform = targetPlatform flags
                , l_flags = LF True True RejectEverywhere True True True True False False False False False
                , l_vars = []
                }
    pairName <- external (mg_module guts) (mkTcOcc "EqualityPair")
    conName <- external (mg_module guts) (mkDataOcc "EqualityPair")
    workName <- external (mg_module guts) (mkVarOcc "$WEqualityPair")
    repName <- external (mg_module guts) (mkVarOcc "$tcEqualityPair")
    conRepName <- external (mg_module guts) (mkVarOcc "$tc'EqualityPair")
    ckTemplate <- coercion "ckTemplate" kindProof
    let pair =
            mkAlgTyCon pairName (mkTyConKind binders liftedTypeKind) binders 0 liftedTypeKind
                (replicate 4 Nominal) Nothing [] (mkDataTyConRhs [con]) (VanillaAlgTyCon repName) True
        pairType = mkTyConApp pair [k1, k2, a, b]
        con =
            mkDataCon conName False conRepName [] [] [NotMarkedStrict] []
                universals [ckTemplate] noConcreteTyVars (mkTyVarBinders Specified universals)
                [] [indexProof ckTemplate] [] pairType NoPromInfo pair 1 [] worker
                (DCR worker vanillaDataConBoxer [mkScaled manyDataConTy (indexProof ckTemplate)])
        worker = mkDataConWorkId workName con
        instWorker = mkTyApps (Var worker) [k1, k2, a, b]
    ck <- coercion "ck" kindProof
    cv <- coercion "cv" (indexProof ck)
    ckAlt <- coercion "ckAlt" kindProof
    cvAlt <- coercion "cvAlt" (indexProof ckAlt)
    wrongKind <- coercion "wrongKind" (mkNomEqPred a a)
    wrongRole <- coercion "wrongRole" (mkReprEqPred k1 k2)
    outOfScope <- coercion "outOfScope" kindProof
    independentlyValid <- coercion "independent" (mkNomEqPred b b)
    let transported proof = mkTyConApp index [k2, mkCastTy a (mkCoVarCo proof)]
    headVar <- mkTyVar <$> freshName "f" <*> pure
        (mkTyCoInvForAllTy unmatchableDataConTy ckTemplate
            (mkVisFunTyMany unmatchableDataConTy (transported ckTemplate) liftedTypeKind))
    argVar <- mkTyVar <$> freshName "arg" <*> pure (transported ck)
    input <- local "input" left
    witness <- local "witness" pairType
    wild <- local "wild" pairType
    let convert kind indexProofCo =
            let kc = mkCoVarCo kind
                indexCo = mkTransCo (mkGReflCo Nominal a (MCo kc)) (mkCoVarCo indexProofCo)
             in Cast (Var input) (mkSubCo (mkTyConAppCo Nominal index [kc, indexCo]))
        lambda = mkLams (universals ++ [ck, cv, input]) (convert ck cv)
        reversed = mkLams (universals ++ [cv, ck, input]) (convert ck cv)
        applied arg = mkLams (universals ++ [ck, cv]) (mkApps instWorker [arg, Coercion (mkCoVarCo cv)])
        constructor = applied (Coercion (mkCoVarCo ck))
        badApp bad =
            mkLams (universals ++ [ck, cv, bad]) $
                mkApps instWorker [Coercion (mkCoVarCo bad), Coercion (mkCoVarCo cv)]
        badSecondApp =
            let args = map (Coercion . mkCoVarCo) [ck, independentlyValid]
             in mkLams (universals ++ [ck, independentlyValid]) (mkApps instWorker args)
        unboundApp = applied (Coercion (mkCoVarCo outOfScope))
        injected = applied (Type (mkCoercionTy (mkCoVarCo ck)))
        matcher =
            mkLams (universals ++ [witness, input]) $
                Case (Var witness) wild right [Alt (DataAlt con) [ckAlt, cvAlt] (convert ckAlt cvAlt)]
        roundTrip =
            mkLams (universals ++ [ck, cv, input]) $
                Case (mkApps instWorker [Coercion (mkCoVarCo ck), Coercion (mkCoVarCo cv)])
                    wild right [Alt (DataAlt con) [ckAlt, cvAlt] (convert ckAlt cvAlt)]
        badAlt args =
            mkLams (universals ++ [witness, input]) $
                Case (Var witness) wild left [Alt (DataAlt con) args (Var input)]
        typeApp =
            mkLams (universals ++ [ck, headVar, argVar]) $
                Coercion (mkNomReflCo (mkAppTys (mkTyVarTy headVar) [mkCoercionTy (mkCoVarCo ck), mkTyVarTy argVar]))
        inst =
            mkLams (universals ++ [ck]) $
                Coercion $
                    InstCo (mkNomReflCo (mkTyCoInvForAllTy unmatchableDataConTy ckTemplate
                        (transported ckTemplate))) (mkNomReflCo (mkCoercionTy (mkCoVarCo ck)))
        sameKind co =
            let (lhs, rhs, role) = coVarTypesRole co
             in role == Nominal && eqType (typeKind lhs) (typeKind rhs)
    unless (all sameKind [ck, cv, ckAlt, cvAlt]) $
        pprPanic "Test constructed nonhomogeneous proof binders" (ppr [ck, cv, ckAlt, cvAlt])
    fn <- local "checked" (exprType lambda)
    let (_, lambdaTypeErrors) = lintCoreBindings cfg [NonRec fn lambda]
    accepted "declared dependent lambda" (bagToList lambdaTypeErrors)
    accepted "dependent lambda" (maybe [] bagToList (lintExpr cfg lambda))
    rejected cfg "reversed binder order" reversed
    checkExpr cfg "dependent type application" typeApp
    checkExpr cfg "InstCo over a named proof" inst
    rejected cfg "type-encoded coercion application" injected
    checkExpr cfg "constructor application" constructor
    checkExpr cfg "constructor match" matcher
    checkExpr cfg "constructor round trip" roundTrip
    rejected cfg "wrong kind at application" (badApp wrongKind)
    rejected cfg "wrong role at application" (badApp wrongRole)
    rejected cfg "out-of-scope application proof" unboundApp
    rejected cfg "wrong second application proof" badSecondApp
    rejected cfg "wrong kind in DataAlt" (badAlt [wrongKind, independentlyValid])
    rejected cfg "wrong role in DataAlt" (badAlt [wrongRole, independentlyValid])
    rejected cfg "out-of-scope DataAlt proof" (badAlt [cvAlt, ckAlt])
    rejected cfg "wrong second DataAlt proof" (badAlt [ckAlt, independentlyValid])
    liftIO (putStrLn "Checked dependent coercion arguments")
    pure guts

checkExpr :: LintConfig -> String -> CoreExpr -> CoreM ()
checkExpr cfg label = accepted label . maybe [] bagToList . lintExpr cfg

accepted :: String -> [SDoc] -> CoreM ()
accepted _ [] = pure ()
accepted label messages = pprPanic ("Rejected " ++ label) (vcat messages)

rejected :: LintConfig -> String -> CoreExpr -> CoreM ()
rejected cfg label expr = case lintExpr cfg expr of
    Nothing -> pprPanic ("Accepted " ++ label) (ppr expr)
    Just _ -> pure ()

external :: Module -> OccName -> CoreM Name
external mdl occ = do
    unique <- getUniqueM
    pure (mkExternalName unique mdl occ noSrcSpan)

coercion :: String -> Type -> CoreM CoVar
coercion label ty = (\name -> mkCoVar name ty) <$> freshName label

freshName :: String -> CoreM Name
freshName label = (\unique -> mkSystemVarName unique (fsLit label)) <$> getUniqueM

local :: String -> Type -> CoreM Id
local label ty = do
    unique <- getUniqueM
    pure (mkSysLocal (fsLit label) unique manyDataConTy ty)
