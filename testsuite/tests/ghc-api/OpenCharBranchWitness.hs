module Main (main) where

import Control.Monad (forM_, unless)
import Control.Monad.Trans.State.Strict (StateT(..), modify', runState)
import GHC (CoreModule(..), compileToCoreModule, getSessionDynFlags, runGhc,
            setSessionDynFlags)
import GHC.Builtin.WiredIn.Ids (unboxedUnitExpr)
import GHC.Builtin.WiredIn.Types (charDataCon, charTy, manyDataConTy, unboxedUnitTy)
import GHC.Core (Alt(..), AltCon(..), Bind(..), CoreExpr, Expr(..), flattenBinds)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Make (mkCharExpr)
import GHC.Core.Map.Expr (eqCoreExpr)
import GHC.Core.OpenWhnf
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.Type (Type, isCharLitTy, mkCharLitTy, mkTyVarTy, mkVisFunTyManyU)
import GHC.Core.Utils (exprType)
import GHC.Data.FastString (fsLit)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (DynFlags(..))
import GHC.Types.Id (Id, idName, idType, mkSysLocal)
import GHC.Types.Literal (Literal(LitChar))
import GHC.Types.Name (Name, getOccString, mkSysTvName, nameModule_maybe)
import GHC.Types.Unique (UniqueTag(LocalTag))
import GHC.Types.Unique.Supply (uniqFromTag)
import GHC.Types.Var (mkTyVar)
import GHC.Types.Var.Env (lookupVarEnv, mkVarEnv)
import GHC.Types.Var.Set (emptyVarSet)
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  [libdir] <- getArgs
  (flags, producer, consumer) <- runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    setSessionDynFlags dflags { verbosity = 0 }
    producer <- compileToCoreModule "OpenCharBranchWitnessProducer.hs"
    consumer <- compileToCoreModule "OpenCharBranchWitnessConsumer.hs"
    pure (dflags, producer, consumer)
  (classifier, classifierBody) <- findBinding producer "classify"
  (probe, probeBody) <- findBinding consumer "probe"
  differentProducer <- findBinding producer "otherClassifier"
  let funcType = mkVisFunTyManyU charTy charTy
      definitions = mkVarEnv [(classifier, classifierBody), (probe, probeBody)]
      provide v = case lookupVarEnv definitions v of
        Nothing -> pure Nothing
        Just rhs -> modify' (v :) >> pure (Just rhs)
      observe arg = runState (openWhnfM 80 provide emptyVarSet (App (Var probe) arg)) []
      checkGround input expected =
        case observe (mkCharExpr input) of
          (Right (BoxedChar actual), uses)
            | actual == expected && classifier `elem` uses -> pure ()
          (result, _) -> badOutcome classifierBody ("ground " ++ show input) result

  check "module and arrow type" $
    nameModule_maybe (idName classifier) == Just (cm_module producer) &&
    nameModule_maybe (idName probe) == Just (cm_module consumer) &&
    cm_module producer /= cm_module consumer &&
    all (`eqType` funcType) [exprType classifierBody, exprType probeBody]
  checkGround 'x' 'X'
  checkGround 'é' 'É'
  checkGround '!' '!'
  putStrLn "checked literal alternatives and the identity default"

  fresh <- uniqFromTag LocalTag
  let rigid = mkSysLocal (fsLit "rigid-char") fresh manyDataConTy charTy
  case observe (Var rigid) of
    (Right (Neutral (CaseBlocked (Rigid scrut) (CaseContinuation _ _ resultTy branches))), uses)
      | scrut == rigid, eqType resultTy charTy
      , classifier `elem` uses
      , any charAlt branches ->
          putStrLn "the imported case stays blocked on the exact rigid Char"
    (result, _) -> badOutcome classifierBody "rigid" result

  tokenUnique <- uniqFromTag LocalTag
  binderUnique <- uniqFromTag LocalTag
  let token = mkSysLocal (fsLit "unused-token") tokenUnique manyDataConTy unboxedUnitTy
      caseBinder = mkSysLocal (fsLit "case-char") binderUnique manyDataConTy charTy
      continuation = Lam token (mkCharExpr 'Z')
      known = App continuation unboxedUnitExpr
      demanded = Case (Var rigid) caseBinder unboxedUnitTy [Alt DEFAULT [] unboxedUnitExpr]
      unknown = App continuation demanded
      observeTerm expr = fst (runState (openWhnfM 80 provide emptyVarSet expr) [])
  check "typed unboxed continuation applications" $
    all (`eqType` charTy) [exprType known, exprType unknown]
  check "the evaluated nullary tuple may enter an unused continuation binder" $
    case observeTerm known of
      Right (BoxedChar 'Z') -> True
      _ -> False
  check "an unlifted operand that still demands a rigid scrutinee cannot be skipped" $
    case observeTerm unknown of
      Left UnsupportedCore -> True
      _ -> False
  functionUnique <- uniqFromTag LocalTag
  let functionBinder = mkSysLocal (fsLit "case-function") functionUnique
                                   manyDataConTy (exprType continuation)
      checkDefault label scrutinee binder step = do
        let expr = Case scrutinee binder charTy [Alt DEFAULT [] (mkCharExpr 'Z')]
            (result, steps) =
              runState (openWhnfTraceM 80 (pure . lookupVarEnv definitions)
                                       (modify' . (:)) emptyVarSet expr) []
        check label $ steps == [step] &&
          case result of
            Right (BoxedChar 'Z') -> True
            _ -> False
  checkDefault "a boxed Char DEFAULT is recorded" (mkCharExpr '!') caseBinder
               (SelectBoxedChar '!' DEFAULT)
  checkDefault "a lambda DEFAULT is recorded" continuation functionBinder SelectLambdaDefault
  putStrLn "only the evaluated unboxed unit can enter the unused failure continuation"

  let definition = (classifier, classifierBody)
      proposal c = either (die . show) pure (recordBranch flags definition (mkCharLitTy c))
  forM_ [('x','X'), ('é','É'), ('!','!')] $ \(input, output) -> do
    witness <- proposal input
    check ("recorded and verified " ++ show input) $
      witnessResult witness `eqType` mkCharLitTy output &&
      verifyBranch flags definition witness == Right ()

  witness <- proposal 'x'
  defaultWitness <- proposal '!'
  check "the default trace used the evaluated failure continuation" $
    ContinueWithUnit `elem` witnessSteps defaultWitness
  check "wrong producer is rejected even when it has the same behavior" $
    verifyBranch flags differentProducer witness == Left WrongProducer
  let altered = (classifier, alterUntakenBranch classifierBody)
  check "editing an untaken branch invalidates the old body anchor" $
    verifyBranch flags altered witness == Left ChangedBody
  check "the independently checked result endpoint must match" $
    verifyBranch flags definition witness { witnessResult = mkCharLitTy 'Z' } == Left WrongResult
  check "selecting DEFAULT for a covered literal is rejected" $
    verifyBranch flags definition witness
      { witnessSteps = map forceDefault (witnessSteps witness) } == Left WrongTrace
  typeUnique <- uniqFromTag LocalTag
  check "a missing step is rejected" $
    verifyBranch flags definition witness { witnessSteps = drop 1 (witnessSteps witness) } == Left WrongTrace
  check "an extra step is rejected" $
    verifyBranch flags definition witness
      { witnessSteps = witnessSteps witness ++ [ContinueWithUnit] } == Left WrongTrace
  let rigidType = mkTyVarTy (mkTyVar (mkSysTvName typeUnique (fsLit "c")) charTy)
  check "a default trace cannot stand in for a rigid operand" $
    verifyBranch flags definition defaultWitness { witnessOperand = rigidType } == Left NonGroundOperand
  let strictBody = Lam rigid unknown
  check "a strict unlifted argument does not validate a claimed continuation step" $
    verifyBranch flags (classifier, strictBody) defaultWitness
      { witnessBody = strictBody, witnessSteps = [ContinueWithUnit] } ==
        Left (CouldNotObserve UnsupportedCore)
  putStrLn "verified case witnesses and rejected stale, forged and rigid claims"
  where
    charAlt (Alt (DataAlt con) [_] _) = con == charDataCon
    charAlt _ = False

-- Alter an output which the witness for 'x' does not select.
alterUntakenBranch :: CoreExpr -> CoreExpr
alterUntakenBranch expr = case expr of
  Lit (LitChar 'É') -> Lit (LitChar 'Q')
  App f a -> App (alterUntakenBranch f) (alterUntakenBranch a)
  Lam binder body -> Lam binder (alterUntakenBranch body)
  Let (NonRec binder rhs) body ->
    Let (NonRec binder (alterUntakenBranch rhs)) (alterUntakenBranch body)
  Case scrut binder ty alts ->
    Case (alterUntakenBranch scrut) binder ty
      [Alt key binders (alterUntakenBranch rhs) | Alt key binders rhs <- alts]
  _ -> expr

forceDefault :: CaseStep -> CaseStep
forceDefault (SelectLiteral literal _) = SelectLiteral literal DEFAULT
forceDefault step = step

findBinding :: CoreModule -> String -> IO (Id, CoreExpr)
findBinding core occ = case filter ((== occ) . getOccString . fst) (flattenBinds (cm_binds core)) of
  [binding] -> pure binding
  _ -> die ("missing or ambiguous Core binding " ++ occ)

check :: String -> Bool -> IO ()
check label passed = unless passed (die ("failed " ++ label))

badOutcome :: CoreExpr -> String -> Either EvalFailure Whnf -> IO a
badOutcome body label result =
  die (label ++ " unexpected observation: " ++ describe result
       ++ "\nproducer Core:\n" ++ showSDocUnsafe (ppr body))
  where
    describe (Left reason) = show reason
    describe (Right (BoxedChar c)) = show c
    describe (Right (Literal l)) = showSDocUnsafe (ppr l)
    describe (Right Lambda{}) = "lambda"
    describe (Right (Neutral _)) = "neutral"

{- Note [Checking a ground character trace]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The test obtains the producer Id and its typed body from GHC. It then
passes a proposed witness and the authoritative binding to verifyBranch
separately. Check the complete body before replaying the selected path.
An edit to an untaken alternative can change the definition even if it
preserves the result at this literal. A client that imports a proof will
need the corresponding interface fingerprint.
-}

-- | In-memory trace for one checked producer and a ground operand.
data Witness = Witness
  { witnessProducer :: Name
  , witnessBody :: CoreExpr
  , witnessOperand :: Type
  , witnessResult :: Type
  , witnessSteps :: [CaseStep]
  }

data Failure
  = WrongProducer | ChangedBody | InvalidDefinition | NonGroundOperand
  | NonGroundResult | WrongTrace | WrongResult | CouldNotObserve EvalFailure
  deriving (Eq, Show)

-- | Obtain a proposed witness for one literal.
recordBranch :: DynFlags -> (Id, CoreExpr) -> Type -> Either Failure Witness
recordBranch flags def@(producer, body) operand = do
  checkDefinition flags def
  character <- maybe (Left NonGroundOperand) Right (isCharLitTy operand)
  let (result, reversed) = runState
        (replay def character (modify' . (:))) []
  resultChar <- checkedResult result
  pure (Witness (idName producer) body operand (mkCharLitTy resultChar) (reverse reversed))

-- | Check a proposed trace against an independently supplied checked definition.
-- Consume each claimed step at the point where its premises are available.
verifyBranch :: DynFlags -> (Id, CoreExpr) -> Witness -> Either Failure ()
verifyBranch flags def@(producer, body) witness = do
  unless (idName producer == witnessProducer witness) (Left WrongProducer)
  checkDefinition flags def
  unless (eqCoreExpr body (witnessBody witness)) (Left ChangedBody)
  character <- maybe (Left NonGroundOperand) Right (isCharLitTy (witnessOperand witness))
  expected <- maybe (Left NonGroundResult) Right (isCharLitTy (witnessResult witness))
  (result, remaining) <- runStateT (replay def character verifyStep) (witnessSteps witness)
  actual <- checkedResult result
  unless (null remaining) (Left WrongTrace)
  unless (actual == expected) (Left WrongResult)
  where
    verifyStep actual = StateT $ \steps -> case steps of
      proposed : rest | proposed == actual -> Right ((), rest)
      _ -> Left WrongTrace

replay :: Monad m => (Id, CoreExpr) -> Char -> (CaseStep -> m ())
       -> m (Either EvalFailure Whnf)
replay (producer, body) character step =
  openWhnfTraceM 80 provide step emptyVarSet (App (Var producer) (mkCharExpr character))
  where
    provide v = pure $ if v == producer then Just body else Nothing

checkDefinition :: DynFlags -> (Id, CoreExpr) -> Either Failure ()
checkDefinition flags (producer, body)
  | Just _ <- lintExpr (initLintConfig flags []) body = Left InvalidDefinition
  | all (`eqType` arrow) [idType producer, exprType body] = Right ()
  | otherwise = Left InvalidDefinition
  where
    arrow = mkVisFunTyManyU charTy charTy

checkedResult :: Either EvalFailure Whnf -> Either Failure Char
checkedResult (Left failure) = Left (CouldNotObserve failure)
checkedResult (Right (BoxedChar char)) = Right char
checkedResult (Right _) = Left NonGroundResult
