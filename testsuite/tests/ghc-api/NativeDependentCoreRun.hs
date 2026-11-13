module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, isInfixOf)
import Data.Time.Clock (getCurrentTime)
import qualified GHC
import GHC.Builtin.WiredIn.Types
import GHC.Core
import GHC.Core.Coercion
import GHC.Core.Coercion.Axiom (Branched, ClosedTyFam (..), CoAxiom, CoAxiomRule (..))
import GHC.Core.DataCon (DataCon, dataConFullSig, dataConName, dataConWorkId, eqSpecTyVar, eqSpecType, promoteDataCon)
import GHC.Core.Dependent (hasNativeDependentExpr)
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Make (mkCoreConApps, mkIntExpr)
import GHC.Core.Opt.Arity (etaExpand)
import GHC.Core.Predicate (mkNomEqPred)
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Type (..), scaledThing)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Utils (dataConRepInstPat, exprType)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Backend (backendWritesFiles)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.DynFlags (DynFlags (..), ExecutableLinkMode (Dynamic), GhcLink (LinkExecutable, NoLink), GhcMode (OneShot), gopt_set, targetProfile)
import GHC.Driver.Env (HscEnv, hscInterp, hsc_NC)
import GHC.Driver.Flags (GeneralFlag (Opt_ExternalInterpreter))
import GHC.Driver.Main (hscCompileCoreExpr, hscSimplify)
import GHC.Driver.Phases (Phase (StopLn), StopPhase (NoStop))
import GHC.Driver.Pipeline (compileFile, oneShot)
import GHC.Driver.Ppr (showSDoc)
import GHC.Iface.Binary (CheckHiWay (CheckHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Runtime.Interpreter (evalString)
import GHC.Types.Error (Diagnostic (diagnosticCode), DiagnosticCode (..), MsgEnvelope (..), getMessages, pprDiagnostic)
import GHC.Types.Id
import GHC.Types.Name (mkSystemName, nameOccName)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc, mkVarOcc, occNameString)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Types.SrcLoc (isGoodSrcSpan, noLoc, noSrcSpan)
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Var
import GHC.Unit.Module.ModGuts (ModGuts (mg_binds))
import GHC.Unit.Module.ModIface (mi_hash_fn, mi_mod_hash)
import GHC.Unit.Types (mainUnit, mkModule, mkModuleName)
import GHC.Utils.Outputable (vcat)
import GHC.Utils.Panic (GhcException (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, makeAbsolute, setModificationTime)
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath (replaceExtension, (</>))
import System.Info (os)
import System.Process (readProcessWithExitCode)

-- | Execute retained Boolean functions with inputs read at runtime.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [libdir] -> do
            GHC.runGhc (Just libdir) runChecks
            runProgramChecks libdir
            runModuleChecks libdir
            runRecompilationChecks libdir
            putStrLn "native-core-run-ok"
        _ -> fail "Usage: NativeDependentCoreRun GHC_LIBDIR"

-- | Supply ordinary observations and a lazy runtime input source.
helperSource :: String
helperSource =
    unlines
        [ "{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}"
        , "module NativeRuntimeHelpers where"
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
        , "data Nat = Z | S Nat"
        , "data Vec a (n :: Nat) where"
        , "  VNil :: Vec a Z"
        , "  VCons :: a -> Vec a n -> Vec a (S n)"
        , "type family Add (n :: Nat) (m :: Nat) :: Nat where"
        , "  Add Z m = m"
        , "  Add (S n) m = S (Add n m)"
        , "appendVec :: Vec Int n -> Vec Int m -> Vec Int (Add n m)"
        , "appendVec VNil ys = ys"
        , "appendVec (VCons x xs) ys = VCons x (appendVec xs ys)"
        , "vecList :: Vec Int n -> [Int]"
        , "vecList VNil = []"
        , "vecList (VCons x xs) = x : vecList xs"
        , "observeVec :: Vec Int n -> IO String"
        , "observeVec xs = let ys = vecList xs in pure (show ys ++ \":\" ++ show (length ys))"
        , "observeVecHead :: Vec Int n -> IO String"
        , "observeVecHead (VCons x _) = pure (show x)"
        , "observeVecHead VNil = error \"empty-native-vector\""
        , "observeVecPrefix :: Vec Int n -> IO String"
        , "observeVecPrefix xs = pure (show (take 4 (vecList xs)))"
        , "{-# NOINLINE driveNat #-}"
        , "driveNat :: (Nat -> IO String) -> Nat -> IO String"
        , "driveNat k seed = newIORef seed >>= readIORef >>= k"
        , "driveNats :: (Nat -> IO String) -> IO String"
        , "driveNats k = unlines <$> mapM (driveNat k) [Z, S (S Z), S (S (S Z))]"
        , "{-# NOINLINE driveNatPair #-}"
        , "driveNatPair :: (Nat -> Nat -> IO String) -> (Nat, Nat) -> IO String"
        , "driveNatPair k seed = newIORef seed >>= readIORef >>= uncurry k"
        , "driveNatPairs :: (Nat -> Nat -> IO String) -> IO String"
        , "driveNatPairs k = unlines <$> mapM (driveNatPair k) [(Z,Z), (Z,S (S (S Z))), (S (S Z),Z), (S (S Z),S (S (S Z)))]"
        , "{-# NOINLINE bottomNat #-}"
        , "bottomNat :: Nat"
        , "bottomNat = error \"native-bottom-nat\""
        , "partialNat :: Nat"
        , "partialNat = S bottomNat"
        , "{-# NOINLINE cyclicNat #-}"
        , "cyclicNat :: Nat"
        , "cyclicNat = S cyclicNat"
        , "bottomFunction :: a"
        , "bottomFunction = error \"native-bottom-function\""
        , "bottomElement :: Int"
        , "bottomElement = error \"native-bottom-element\""
        , "bottomVec :: Vec Int n"
        , "bottomVec = error \"native-bottom-vector\""
        , "data IndexLeft = IndexLeft IndexRight"
        , "data IndexRight = IndexRight IndexLeft"
        , "data BadIndex = BadIndex BadIndex Int"
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
        , "type Ignore (b :: Bool) = Int"
        , "entry :: IO String"
        , "entry ="
        , "  let keep :: foreach (b :: Bool) -> Select b -> Select b"
        , "      keep b x = x"
        , "      f :: foreach (b :: Bool) -> Ignore b -> Int"
        , "      f True x = x"
        , "      f False x = x"
        , "  in do t <- observeInt (keep True 42)"
        , "        falseResult <- observeBool (keep False False)"
        , "        aliasTrue <- observeInt (f True 42)"
        , "        aliasFalse <- observeInt (f False 42)"
        , "        let b = bottomBool"
        , "        case keep b of"
        , "          !_ -> pure (t ++ \"\\n\" ++ falseResult ++ \"\\n\" ++ aliasTrue ++ \"\\n\" ++ aliasFalse ++ \"\\nforced\")"
        ]

-- | Share constructor-refined source workers between runtime observations.
retainedChooseSource :: String -> String -> String
retainedChooseSource moduleName observation =
    unlines
        [ "{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"
        , "module " ++ moduleName ++ " where"
        , "import NativeRuntimeHelpers"
        , "type family Select (b :: Bool) where"
        , "  Select True = Int"
        , "  Select False = Bool"
        , "entry :: IO String"
        , "entry ="
        , "  let choose :: foreach (b :: Bool) -> Select b"
        , "      choose True = 42"
        , "      choose False = False"
        , "      keep :: foreach (b :: Bool) -> Select b -> Select b"
        , "      keep b x = x"
        , "      again :: foreach (b :: Bool) -> Select b"
        , "      again b = keep b (choose b)"
        , "      observe :: foreach (b :: Bool) -> Select b -> String"
        , "      observe False p = if p then \"True\" else \"False\""
        , "      observe True n = show (n :: Int)"
        , "      forceChosen :: foreach (b :: Bool) -> Select b -> ()"
        , "      forceChosen b !x = ()"
        , "      forceIndependent :: foreach (b :: Bool) -> ((b ~ b) => Int) -> ()"
        , "      forceIndependent b !x = ()"
        , "      keepIndependent :: foreach (b :: Bool) -> ((b ~ b) => Int) -> Int"
        , "      keepIndependent b x = x"
        , "      run :: Bool -> IO String"
        , "      run b = pure (observe b (again b))"
        , "  in " ++ observation
        ]

-- | Check retained source workers over the loaded ordinary Nat and Vec types.
retainedNatSource :: String -> String -> String
retainedNatSource moduleName observation =
    unlines
        [ "{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, ScopedTypeVariables, TypeAbstractions, TypeApplications, TypeFamilies #-}"
        , "module " ++ moduleName ++ " where"
        , "import NativeRuntimeHelpers hiding (appendVec)"
        , "type family ConstantNat (n :: Nat) where"
        , "  ConstantNat n = Int"
        , "entry :: IO String"
        , "entry ="
        , "  let appendVec :: forall a n m. Vec a n -> Vec a m -> Vec a (Add n m)"
        , "      appendVec VNil ys = ys"
        , "      appendVec (VCons x xs) ys = VCons x (appendVec xs ys)"
        , "      bottomPolymorphicVec :: forall a n. Vec a n"
        , "      bottomPolymorphicVec = error \"native-bottom-vector\""
        , "      replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n"
        , "      replicateNative Z x = VNil"
        , "      replicateNative (S k) x = VCons x (replicateNative k x)"
        , "      headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a"
        , "      headNative n (VCons x _) = x"
        , "      appendNative :: forall a. foreach (n :: Nat) (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)"
        , "      appendNative n m xs ys = appendVec xs ys"
        , "      appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat) -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)"
        , "      appendAfterSuccessor n = appendNative @Int (S n)"
        , "      nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
        , "      nativeId n xs = xs"
        , "      leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n"
        , "      leftUnit = nativeId"
        , "      leftUnitAppend :: foreach (n :: Nat) (m :: Nat) -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)"
        , "      leftUnitAppend = appendNative @Int"
        , "      ordinaryNat :: Nat -> Int"
        , "      ordinaryNat _ = 42"
        , "      constantAlias :: foreach (n :: Nat) -> ConstantNat n"
        , "      constantAlias = ordinaryNat"
        , "      nested :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a n"
        , "      nested n m xs ys = xs"
        , "      vacuousFirst :: forall a. foreach (unused :: Nat) (n :: Nat) -> Vec a n -> Vec a n"
        , "      vacuousFirst unused n xs = xs"
        , "      vacuousMiddle :: forall a. foreach (n :: Nat) (unused :: Nat) (m :: Nat) -> Vec a n -> Vec a m -> Vec a n"
        , "      vacuousMiddle n unused m xs ys = xs"
        , "      keepErased :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
        , "      keepErased @a n xs = xs"
        , "      run :: Nat -> IO String"
        , "      run n = observeVec (replicateNative @Int n 42)"
        , "      runAppend :: Nat -> Nat -> IO String"
        , "      runAppend n m = observeVec (appendNative @Int n m (replicateNative @Int n 42) (replicateNative @Int m 7))"
        , "      runTelescopes :: Nat -> Nat -> IO String"
        , "      runTelescopes n m ="
        , "        let unused = bottomNat"
        , "        in do a <- observeVec (nested @Int n m (replicateNative @Int n 42) (replicateNative @Int m 7))"
        , "              b <- observeVec (vacuousFirst @Int unused n (replicateNative @Int n 42))"
        , "              c <- observeVec (vacuousMiddle @Int n unused m (replicateNative @Int n 42) (replicateNative @Int m 7))"
        , "              d <- observeVec (keepErased @Int n (replicateNative @Int n 42))"
        , "              pure (unlines [a,b,c,d])"
        , "  in " ++ observation
        ]

-- | Check explicit source cases with dependent results and captured payloads.
retainedCaseSource :: String -> String -> String
retainedCaseSource moduleName observation =
    unlines $ concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"entry :: IO String" =
        [ "type family CaseSelect (b :: Bool) where"
        , "  CaseSelect True = Int"
        , "  CaseSelect False = Bool"
        , line
        ]
    insert line@"      run :: Nat -> IO String" =
        map ("      " ++) retainedCaseDeclarations
            ++ [ "      replicateOriginal :: foreach (n :: Nat) -> Vec Int n"
               , "      replicateOriginal n = case n of { Z -> replicateNative @Int n 42; S k -> replicateNative @Int n 42 }"
               , "      replicateShadow :: foreach (n :: Nat) -> Int -> Vec Int n"
               , "      replicateShadow n x = case n of { Z -> VNil; S n -> VCons x (replicateShadow n x) }"
               , "      chooseCaseNative :: foreach (b :: Bool) -> CaseSelect b"
               , "      chooseCaseNative True = 42"
               , "      chooseCaseNative False = False"
               , "      observeCaseNative :: foreach (b :: Bool) -> CaseSelect b -> String"
               , "      observeCaseNative b x = case (b) of { True -> show (x :: Int); False -> if x then \"True\" else \"False\" }"
               , "      strictCaseNative :: foreach (n :: Nat) -> Vec Int n -> Int -> Int"
               , "      strictCaseNative n xs = case n of { Z -> \\x -> case xs of { VNil -> x }; S k -> \\x -> case xs of { VCons _ _ -> x } }"
               , "      ordinaryCase :: Nat -> Int"
               , "      ordinaryCase n = case n of { Z -> 0; S _ -> 1 }"
               , line
               ]
    insert line = [line]

-- | Share the case worker between source, executable, and interface checks.
retainedCaseDeclarations :: [String]
retainedCaseDeclarations =
    [ "replicateCaseNative :: forall a. foreach (n :: Nat) -> a -> Vec a n"
    , "replicateCaseNative n x = case n of { Z -> VNil; S k -> VCons x (replicateCaseNative k x) }"
    ]

-- | Reuse source workers in a self-contained module with an ordinary main.
retainedProgramSource :: String
retainedProgramSource =
    unlines $
        take 1 sourceLines
            ++ [ "module Main (main) where"
               , "import qualified Control.Exception as E"
               , "import Data.IORef"
               , "import Data.List (isInfixOf)"
               ]
            ++ drop 2 (lines helperSource)
            ++ retainedCaseDeclarations
            ++ drop 3 sourceLines
            ++ ["main :: IO ()", "main = entry >>= putStr"]
  where
    sourceLines = lines $ retainedChooseSource "NativeProgram" $ unlines $ concatMap addCases $ lines retainedProgramObservation
    addCases "       pure (unlines [both, partial, lazy, control, strictText])" =
        [ "       vectors <- driveNats (\\n -> observeVec (replicateCaseNative n (42 :: Int)))"
        , "       pure (unlines [both, partial, lazy, control, strictText] ++ vectors)"
        ]
    addCases line = [line]

-- | Observe shared runtime indices and distinguish lazy and strict consumers.
retainedProgramObservation :: String
retainedProgramObservation =
    unlines
        [ "do"
        , "       both <- driveBoth run"
        , "       let b = bottomBool"
        , "       partial <- case keep b of !_ -> pure \"forced\""
        , "       lazy <- observeInt (keepIndependent b 42)"
        , "       control <- force (forceIndependent b 42)"
        , "       strict <- E.try (force (forceChosen b (choose b))) :: IO (Either E.SomeException String)"
        , "       strictText <- case strict of"
        , "         Left exception | \"native-bottom-bool\" `isInfixOf` E.displayException exception -> pure \"raised native-bottom-bool\""
        , "         Left exception -> fail (\"choose raised another exception: \" ++ E.displayException exception)"
        , "         Right result -> fail (\"choose bottom returned: \" ++ result)"
        , "       pure (unlines [both, partial, lazy, control, strictText])"
        ]

-- | Export native signatures without expression unfoldings.
retainedLibrarySource :: String
retainedLibrarySource =
    unlines
        [ "{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"
        , "module NativeRetainedLibrary (Select, choose, keep, observe, forceChosen, forceIndependent, keepIndependent) where"
        , "type family Select (b :: Bool) where"
        , "  Select True = Int"
        , "  Select False = Bool"
        , "choose :: foreach (b :: Bool) -> Select b"
        , "choose True = 42"
        , "choose False = False"
        , "keep :: foreach (b :: Bool) -> Select b -> Select b"
        , "keep b x = x"
        , "observe :: foreach (b :: Bool) -> Select b -> String"
        , "observe False p = if p then \"True\" else \"False\""
        , "observe True n = show (n :: Int)"
        , "forceChosen :: foreach (b :: Bool) -> Select b -> ()"
        , "forceChosen b !x = ()"
        , "forceIndependent :: foreach (b :: Bool) -> ((b ~ b) => Int) -> ()"
        , "forceIndependent b !x = ()"
        , "keepIndependent :: foreach (b :: Bool) -> ((b ~ b) => Int) -> Int"
        , "keepIndependent b x = x"
        ]

-- | Compose imported native signatures with a runtime Boolean in the consumer.
retainedConsumerSource :: String
retainedConsumerSource =
    unlines $
        take 1 (lines retainedLibrarySource)
            ++ [ "module Main (main) where"
               , "import NativeRetainedLibrary"
               , "import qualified Control.Exception as E"
               , "import Data.IORef"
               , "import Data.List (isInfixOf)"
               ]
            ++ drop 2 (lines helperSource)
            ++ [ "run :: Bool -> IO String"
               , "run b = pure (observe b (keep b (choose b)))"
               , "entry :: IO String"
               , "entry = do"
               , "  concreteTrue <- observeInt (keep True (choose True))"
               , "  concreteFalse <- observeBool (keep False (choose False))"
               , "  if concreteTrue == \"42\" && concreteFalse == \"False\""
               , "    then " ++ retainedProgramObservation
               , "    else fail \"native imported constructor results did not match\""
               , "main :: IO ()"
               , "main = entry >>= putStr"
               ]

-- | Export retained workers and their ordinary datatype declarations together.
retainedNatLibrarySource :: String
retainedNatLibrarySource =
    unlines $
        take 1 (lines (retainedNatSource "NativeRetainedNatLibrary" ""))
            ++ ["module NativeRetainedNatLibrary where"]
            ++ map generalizeAppend (drop 2 (lines helperSource))
            ++ [ "bottomPolymorphicVec :: forall a n. Vec a n"
               , "bottomPolymorphicVec = error \"native-bottom-vector\""
               , "replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n"
               , "replicateNative Z x = VNil"
               , "replicateNative (S k) x = VCons x (replicateNative k x)"
               , "headNative :: forall a. foreach (n :: Nat) -> Vec a (S n) -> a"
               , "headNative n (VCons x _) = x"
               , "appendNative :: forall a. foreach (n :: Nat) (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)"
               , "appendNative n m xs ys = appendVec xs ys"
               , "appendAfterSuccessor :: foreach (n :: Nat) (m :: Nat) -> Vec Int (S n) -> Vec Int m -> Vec Int (Add (S n) m)"
               , "appendAfterSuccessor n = appendNative @Int (S n)"
               , "type family ConstantNat (n :: Nat) where"
               , "  ConstantNat n = Int"
               , "nativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
               , "nativeId n xs = xs"
               , "leftUnit :: foreach (n :: Nat) -> Vec Int (Add Z n) -> Vec Int n"
               , "leftUnit = nativeId"
               , "leftUnitAppend :: foreach (n :: Nat) (m :: Nat) -> Vec Int (Add Z n) -> Vec Int m -> Vec Int (Add n m)"
               , "leftUnitAppend = appendNative @Int"
               , "ordinaryNat :: Nat -> Int"
               , "ordinaryNat _ = 42"
               , "constantAlias :: foreach (n :: Nat) -> ConstantNat n"
               , "constantAlias = ordinaryNat"
               ]
            ++ retainedCaseDeclarations
  where
    generalizeAppend "appendVec :: Vec Int n -> Vec Int m -> Vec Int (Add n m)" =
        "appendVec :: forall a n m. Vec a n -> Vec a m -> Vec a (Add n m)"
    generalizeAppend line = line

-- | Call imported Nat workers without defining retained functions or datatypes.
retainedNatConsumerSource :: String
retainedNatConsumerSource =
    unlines $
        take 1 (lines retainedNatLibrarySource)
            ++ [ "module Main (main) where"
               , "import NativeRetainedNatLibrary"
               , "run :: Nat -> IO String"
               , "run n = observeVec (replicateNative @Int n 42)"
               , "runAppend :: Nat -> Nat -> IO String"
               , "runAppend n m = observeVec (appendNative @Int n m (replicateNative @Int n 42) (replicateNative @Int m 7))"
               , "main :: IO ()"
               , "main = do"
               , "  values <- driveNats run"
               , "  appended <- driveNatPairs runAppend"
               , "  headOnly <- let n = bottomNat in observeInt (headNative @Int n (VCons 42 bottomVec))"
               , "  boolHead <- let n = bottomNat in observeBool (headNative @Bool n (VCons True (bottomPolymorphicVec @Bool)))"
               , "  partialAppend <- let n = bottomNat; m = bottomNat in case appendNative @Int n m of !_ -> pure \"forced\""
               , "  constructed <- driveNats (\\n -> observeVec (replicateNative @Int (S n) 42))"
               , "  constructedAppend <- driveNatPairs (\\n m -> observeVec (appendNative @Int (S n) (S m) (replicateNative @Int (S n) 42) (replicateNative @Int (S m) 7)))"
               , "  constructorHead <- driveNat (\\n -> observeInt (headNative @Int n (replicateNative @Int (S n) 42))) bottomNat"
               , "  nestedPrefix <- driveNat (\\n -> pure (show (take 2 (vecList (replicateNative @Int (S (S n)) 42))))) bottomNat"
               , "  retainedHead <- driveNat (\\n -> case appendNative @Int n of !_ -> pure \"forced\") bottomNat"
               , "  ordinaryHead <- driveNat (\\n -> let m = bottomNat in case appendNative @Int n m of !_ -> pure \"forced\") bottomNat"
               , "  constructedPartial <- driveNat (\\n -> case appendAfterSuccessor n of !_ -> pure \"forced\") bottomNat"
               , "  nestedPartial <- driveNat (\\n -> case appendNative @Int (S (S n)) of !_ -> pure \"forced\") cyclicNat"
               , "  caseValues <- driveNats (\\n -> observeVec (replicateCaseNative @Int n 42))"
               , "  normalized <- driveNats (\\n -> observeVec (leftUnit n (replicateNative @Int n 42)))"
               , "  normalizedAppend <- driveNatPairs (\\n m -> observeVec (leftUnitAppend n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
               , "  constants <- driveNats (\\n -> observeInt (constantAlias n))"
               , "  constantBottom <- driveNat (\\n -> observeInt (constantAlias n)) bottomNat"
               , "  normalizedPartial <- driveNat (\\n -> case leftUnitAppend n of !_ -> pure \"forced\") bottomNat"
               , "  putStr (values ++ appended ++ unlines [headOnly,boolHead,partialAppend] ++ constructed ++ constructedAppend ++ unlines [constructorHead,nestedPrefix,retainedHead,ordinaryHead,constructedPartial,nestedPartial] ++ caseValues ++ normalized ++ normalizedAppend ++ constants ++ unlines [constantBottom,normalizedPartial])"
               ]

-- | Run Boolean and Nat consumers against separately compiled disk artifacts.
runModuleChecks :: FilePath -> IO ()
runModuleChecks libdir = do
    forM_
        [
            ( "bool"
            , "NativeRetainedLibrary"
            , retainedLibrarySource
            , retainedConsumerSource
            , ["42", "False", "forced", "42", "forced", "raised native-bottom-bool"]
            )
        ,
            ( "nat"
            , "NativeRetainedNatLibrary"
            , retainedNatLibrarySource
            , retainedNatConsumerSource
            , -- Keep the first 26 observations. Append 12 normalization observations.
              [ "[]:0"
              , "[42,42]:2"
              , "[42,42,42]:3"
              , "[]:0"
              , "[7,7,7]:3"
              , "[42,42]:2"
              , "[42,42,7,7,7]:5"
              , "42"
              , "True"
              , "forced"
              , "[42]:1"
              , "[42,42,42]:3"
              , "[42,42,42,42]:4"
              , "[42,7]:2"
              , "[42,7,7,7,7]:5"
              , "[42,42,42,7]:4"
              , "[42,42,42,7,7,7,7]:7"
              , "42"
              , "[42,42]"
              , "forced"
              , "forced"
              , "forced"
              , "forced"
              , "[]:0"
              , "[42,42]:2"
              , "[42,42,42]:3"
              ]
                ++ [ "[]:0"
                   , "[42,42]:2"
                   , "[42,42,42]:3"
                   , "[]:0"
                   , "[7,7,7]:3"
                   , "[42,42]:2"
                   , "[42,42,7,7,7]:5"
                   , "42"
                   , "42"
                   , "42"
                   , "42"
                   , "forced"
                   ]
            )
        ]
        $ \(label, moduleName, producer, consumer, expected) ->
            runModuleCase libdir label moduleName producer consumer expected
    putStrLn "module execution checks: ok"

-- | Read a producer interface in a fresh one-shot context and link its object.
runModuleCase :: FilePath -> String -> String -> String -> String -> [String] -> IO ()
runModuleCase libdir label moduleName producer consumer expected = do
    directory <- makeAbsolute ("NativeDependentCoreRun.modules" </> label)
    let producerDirectory = directory </> "producer"
        consumerDirectory = directory </> "consumer"
        artifacts = directory </> "artifacts"
        producerSource = producerDirectory </> (moduleName ++ ".hs")
        consumerSource = consumerDirectory </> "Main.hs"
        executablePath = directory </> ("native-modules" ++ if os == "mingw32" then ".exe" else "")
        configure linkMode output = do
            flags <- GHC.getSessionDynFlags
            logger <- GHC.getLogger
            (configured, leftovers, _) <-
                GHC.parseDynamicFlags logger flags $
                    map
                        noLoc
                        [ "-O0"
                        , "-fasm"
                        , "-fomit-interface-pragmas"
                        , "-fno-write-if-simplified-core"
                        , "-dcore-lint"
                        , "-dstg-lint"
                        , "-fforce-recomp"
                        , "-v0"
                        , "-odir"
                        , artifacts
                        , "-hidir"
                        , artifacts
                        ]
            unless (null leftovers) (fail "FAIL: native module flags were not parsed")
            void $
                GHC.setSessionDynFlags
                    configured
                        { ghcMode = OneShot
                        , ghcLink = linkMode
                        , outputFile_ = output
                        , importPaths = [artifacts]
                        }
            GHC.getSession
        compile environment path = do
            result <- compileFile environment NoStop (path, Nothing)
            case result of
                Just object -> pure object
                Nothing -> fail ("FAIL: native module produced no object: " ++ path)
    forM_ [producerDirectory, consumerDirectory, artifacts] $ createDirectoryIfMissing True
    writeFile producerSource producer
    writeFile consumerSource consumer
    producerObject <- GHC.runGhc (Just libdir) $ do
        environment <- configure NoLink Nothing
        liftIO $ compile environment producerSource
    interfaceExists <- doesFileExist (artifacts </> (moduleName ++ ".hi"))
    unless interfaceExists (fail "FAIL: native producer wrote no interface")
    GHC.runGhc (Just libdir) $ do
        -- OneShot searches only interfaces. The producer source is outside its import path.
        environment <- configure (LinkExecutable Dynamic) (Just executablePath)
        liftIO $ do
            consumerObject <- compile environment consumerSource
            oneShot environment NoStop [(consumerObject, Just StopLn), (producerObject, Just StopLn)]
    (status, actual, errors) <- readProcessWithExitCode executablePath [] ""
    unless (status == ExitSuccess && lines actual == expected) $
        fail ("FAIL: native cross-module observations (" ++ label ++ "): " ++ show (status, actual, errors))

{- Note [Retained signature recompilation checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The consumer imports choose and keep without naming Select. A change to Select
must change both retained declarations' fingerprints and invalidate the consumer.
See Note [Fingerprinting IfaceDecls] in GHC.Iface.Recomp. Rebuilding with fresh
runtime Ids and changing only a function body must preserve the signature ABI.

Use fresh one-shot contexts and disk interfaces. The consumer recompiles without
-fforce-recomp. Set its object and interface timestamps to an earlier value,
after its source timestamp, so an unchanged timestamp proves that GHC skipped
code generation even on a filesystem with coarse timestamp resolution.
-}

-- | Check retained signature fingerprints and incremental consumer recompilation.
runRecompilationChecks :: FilePath -> IO ()
runRecompilationChecks libdir = do
    directory <- makeAbsolute "NativeDependentCoreRun.recompilation"
    let producerDirectory = directory </> "producer"
        consumerDirectory = directory </> "consumer"
        artifacts = directory </> "artifacts"
        producerSource = producerDirectory </> "NativeRetainedLibrary.hs"
        consumerSource = consumerDirectory </> "NativeRetainedUsage.hs"
        consumerInterface = artifacts </> "NativeRetainedUsage.hi"
        configure forceRecomp = do
            flags <- GHC.getSessionDynFlags
            logger <- GHC.getLogger
            (configured, leftovers, _) <-
                GHC.parseDynamicFlags logger flags
                    $ map noLoc
                    $ [ "-O0"
                      , "-fasm"
                      , "-fomit-interface-pragmas"
                      , "-fno-write-if-simplified-core"
                      , "-dcore-lint"
                      , "-dstg-lint"
                      , "-v0"
                      , "-odir"
                      , artifacts
                      , "-hidir"
                      , artifacts
                      ]
                        ++ ["-fforce-recomp" | forceRecomp]
            unless (null leftovers) (fail "FAIL: native recompilation flags were not parsed")
            void $
                GHC.setSessionDynFlags
                    configured{ghcMode = OneShot, ghcLink = NoLink, importPaths = [artifacts]}
            GHC.getSession
        compile environment source = do
            result <- compileFile environment NoStop (source, Nothing)
            case result of
                Just object -> pure object
                Nothing -> fail ("FAIL: native recompilation produced no object: " ++ source)
        producer source = do
            writeFile producerSource source
            GHC.runGhc (Just libdir) $ do
                environment <- configure True
                flags <- GHC.getSessionDynFlags
                liftIO $ do
                    void $ compile environment producerSource
                    readBinIface (targetProfile flags) (hsc_NC environment) CheckHiWay QuietBinIFace $
                        artifacts </> "NativeRetainedLibrary.hi"
        consumer forceRecomp = GHC.runGhc (Just libdir) $ do
            environment <- configure forceRecomp
            liftIO $ compile environment consumerSource
        signatureHashes interface =
            mapM
                ( \name -> case mi_hash_fn interface (mkVarOcc name) of
                    Just (_, hash) -> pure hash
                    Nothing -> fail ("FAIL: retained interface has no fingerprint for " ++ name)
                )
                ["choose", "keep", "keepIndependent"]
        changeBody line
            | line == "choose True = 42" = "choose True = 43"
            | otherwise = line
        changeFamily line = case line of
            "  Select True = Int" -> "  Select True = Bool"
            "choose True = 42" -> "choose True = True"
            "observe True n = show (n :: Int)" -> "observe True n = show (n :: Bool)"
            _ -> line
    forM_ [producerDirectory, consumerDirectory, artifacts] $ createDirectoryIfMissing True
    writeFile consumerSource $
        unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}"
            , "module NativeRetainedUsage where"
            , "import NativeRetainedLibrary (choose, keep)"
            , "use :: Int"
            , "use = keep True (choose True)"
            ]
    original <- producer retainedLibrarySource
    originalHashes <- signatureHashes original
    consumerObject <- consumer True
    rebuilt <- producer retainedLibrarySource
    rebuiltHashes <- signatureHashes rebuilt
    unless (mi_mod_hash rebuilt == mi_mod_hash original && rebuiltHashes == originalHashes) $
        fail "FAIL: fresh retained binder identities changed the signature ABI"
    -- These times precede the generated files and differ by a full day.
    let sourceTime = read "2000-01-01 00:00:00 UTC"
        objectTime = read "2000-01-02 00:00:00 UTC"
    setModificationTime consumerSource sourceTime
    forM_ [consumerObject, consumerInterface] $ \path -> setModificationTime path objectTime
    let checkSkipped = do
            void $ consumer False
            forM_ [consumerObject, consumerInterface] $ \path -> do
                actual <- getModificationTime path
                unless (actual == objectTime) $
                    fail ("FAIL: unchanged native dependency recompiled consumer: " ++ path)
    checkSkipped
    bodyChanged <- producer $ unlines $ map changeBody $ lines retainedLibrarySource
    bodyHashes <- signatureHashes bodyChanged
    unless (mi_mod_hash bodyChanged == mi_mod_hash original && bodyHashes == originalHashes) $
        fail "FAIL: an omitted retained function body changed the signature ABI"
    checkSkipped
    familyChanged <- producer $ unlines $ map changeFamily $ lines retainedLibrarySource
    changedHashes <- signatureHashes familyChanged
    case (originalHashes, changedHashes) of
        ([chooseHash, keepHash, independentHash], [chooseHash', keepHash', independentHash'])
            | mi_mod_hash familyChanged /= mi_mod_hash original
            , chooseHash /= chooseHash'
            , keepHash /= keepHash'
            , independentHash == independentHash' ->
                pure ()
        _ -> fail "FAIL: retained signature fingerprints did not track their family dependency"
    result <- try (consumer False) :: IO (Either SourceError FilePath)
    case result of
        Right _ -> fail "FAIL: stale native consumer was not rechecked after a family change"
        Left exception -> do
            flags <- GHC.runGhc (Just libdir) GHC.getSessionDynFlags
            let messages = bagToList (getMessages (srcErrorMessages exception))
                matches envelope =
                    diagnosticCode (errMsgDiagnostic envelope) == Just (DiagnosticCode "GHC" 83865)
                        && isGoodSrcSpan (errMsgSpan envelope)
                        && all (`isInfixOf` showSDoc flags (pprDiagnostic (errMsgDiagnostic envelope))) ["Int", "Bool", "choose"]
            unless (any matches messages) $
                fail ("FAIL: stale native consumer has the wrong diagnostic: " ++ showSDoc flags (vcat (map (pprDiagnostic . errMsgDiagnostic) messages)))
    putStrLn "module fingerprint/recompilation checks: ok"

-- | Compile a source module, link its object code, and run the binary.
runProgramChecks :: FilePath -> IO ()
runProgramChecks libdir = do
    let directory = "NativeDependentCoreRun.program"
        executableName = "native-program" ++ if os == "mingw32" then ".exe" else ""
    createDirectoryIfMissing True directory
    sourcePath <- makeAbsolute (directory </> "Main.hs")
    executablePath <- makeAbsolute (directory </> executableName)
    let withProgram extraFlags source action = do
            writeFile sourcePath source
            GHC.runGhc (Just libdir) $ do
                flags <- GHC.getSessionDynFlags
                logger <- GHC.getLogger
                (programFlags, leftovers, _) <-
                    GHC.parseDynamicFlags logger flags
                        $ map noLoc
                        $ [ "-O0"
                          , "-fasm"
                          , "-fomit-interface-pragmas"
                          , "-fno-write-if-simplified-core"
                          , "-dcore-lint"
                          , "-dstg-lint"
                          , "-fforce-recomp"
                          , "-v0"
                          ]
                            ++ extraFlags
                unless (null leftovers) (fail "FAIL: native program flags were not parsed")
                let writesFiles = backendWritesFiles (backend programFlags)
                void $
                    GHC.setSessionDynFlags
                        programFlags
                            { ghcLink = if writesFiles then LinkExecutable Dynamic else NoLink
                            , outputFile_ = if writesFiles then Just executablePath else Nothing
                            }
                target <- GHC.guessTarget sourcePath Nothing Nothing
                GHC.setTargets [target]
                action
    withProgram [] retainedProgramSource $ do
        loaded <- GHC.load GHC.LoadAllTargets
        unless (GHC.succeeded loaded) (fail "FAIL: native source program did not compile or link")
    objectExists <- doesFileExist (replaceExtension sourcePath "o")
    unless objectExists (fail "FAIL: native source program produced no object file")
    (status, actual, errors) <- readProcessWithExitCode executablePath [] ""
    unless (status == ExitSuccess) $
        fail ("FAIL: native source program exited with " ++ show status ++ ":\n" ++ errors)
    let expected = ["42", "False", "forced", "42", "forced", "raised native-bottom-bool", "[]:0", "[42,42]:2", "[42,42,42]:3"]
    unless (lines actual == expected) $
        fail ("FAIL: native source program observations: " ++ show actual ++ "; stderr: " ++ errors)
    putStrLn "program keep/choose True: 42"
    putStrLn "program keep/choose False: False"
    putStrLn "program keepSelect bottom (partial WHNF): forced"
    putStrLn "program keep indexed bottom (fully applied): 42"
    putStrLn "program indexed payload control bottom (dictionary WHNF): forced"
    putStrLn "program choose bottom: raised native-bottom-bool"
    let sourceWith declarations = retainedProgramSource ++ unlines declarations
        classBody = drop 1 $ dropWhile (/= "entry =") $ lines $ retainedChooseSource "NativeDefault" "driveBoth run"
        classSource =
            sourceWith $
                [ "class NativeDefault a where"
                , "  method :: a -> IO String"
                , "  method _ ="
                ]
                    ++ map ("  " ++) classBody
        rejections =
            [ ("optimized module", ["-O2"], retainedProgramSource, "optimized module compilation")
            , ("interface pragmas", ["-fno-omit-interface-pragmas"], retainedProgramSource, "interface unfolding generation")
            , ("serialized Core", ["-fwrite-if-simplified-core"], retainedProgramSource, "Core interface serialization")
            , ("late cost centres", ["-fprof-late"], retainedProgramSource, "profiling or late cost-centre insertion")
            , ("INLINE entry", [], sourceWith ["{-# INLINE entry #-}"], "unfolding construction while desugaring binding")
            , ("INLINABLE entry", [], sourceWith ["{-# INLINABLE entry #-}"], "unfolding construction while desugaring binding")
            , ("non-object module", ["-fbyte-code", "-no-link"], retainedProgramSource, "non-object module compilation")
            , ("native class default method", [], classSource, "default-method unfolding construction while desugaring binding")
            ]
    forM_ rejections $ \(label, extraFlags, source, phase) -> do
        -- The make driver catches panics as Failed. Call this phase directly.
        result <-
            try
                ( withProgram extraFlags source $ do
                    void (GHC.depanal [] False)
                    summary <- GHC.getModSummary (mkModule mainUnit (mkModuleName "Main"))
                    parsed <- GHC.parseModule summary
                    checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
                    desugared <- GHC.desugarModule checked
                    environment <- GHC.getSession
                    liftIO $ void $ hscSimplify environment [] (GHC.coreModule desugared)
                ) ::
                IO (Either GhcException ())
        case result of
            Left (Sorry message)
                | phase `isInfixOf` message
                , "This operation does not support" `isInfixOf` message ->
                    pure ()
                | otherwise -> fail ("FAIL: program rejection " ++ label ++ "; wrong phase: " ++ message)
            Left exception -> fail ("FAIL: program rejection " ++ label ++ "; wrong exception: " ++ displayException exception)
            Right () -> fail ("FAIL: native program accepted " ++ label)
    writeFile sourcePath retainedProgramSource
    putStrLn "program rejection checks: ok"

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
    unless (lines selectedActual == ["42", "False", "42", "42", "forced"]) $
        fail ("FAIL: source keepSelect family or laziness observation: " ++ show selectedActual)
    liftIO $ do
        putStrLn "source keepSelect True: 42"
        putStrLn "source keepSelect False: False"
        putStrLn "source alias True: 42"
        putStrLn "source alias False: 42"
        putStrLn "source keepSelect bottom (partial WHNF): forced"
    chosenExpression <- sourceEntry "NativeSourceChoose" (retainedChooseSource "NativeSourceChoose" "driveBoth run")
    chosenEnvironment <- GHC.getSession
    chosenActual <- liftIO $ execute chosenEnvironment flags "source keep/choose" chosenExpression
    unless (lines chosenActual == ["42", "False"]) $
        fail ("FAIL: source keep/choose with shared runtime indices: " ++ show chosenActual)
    liftIO $ do
        putStrLn "source keep/choose True: 42"
        putStrLn "source keep/choose False: False"
    controlExpression <-
        sourceEntry "NativeSourceChooseControl" $
            retainedChooseSource "NativeSourceChooseControl" "let b = bottomBool in force (forceIndependent b 42)"
    controlEnvironment <- GHC.getSession
    controlActual <- liftIO $ execute controlEnvironment flags "source indexed payload control" controlExpression
    unless (controlActual == "forced") $
        fail ("FAIL: source indexed payload control forced its Boolean: " ++ show controlActual)
    liftIO $ putStrLn "source indexed payload control bottom (dictionary WHNF): forced"
    strictExpression <-
        sourceEntry "NativeSourceChooseBottom" $
            retainedChooseSource "NativeSourceChooseBottom" "let b = bottomBool in force (forceChosen b (choose b))"
    strictEnvironment <- GHC.getSession
    result <- liftIO $ try (execute strictEnvironment flags "source choose bottom" strictExpression)
    case result of
        Left exception
            | "native-bottom-bool" `isInfixOf` displayException (exception :: SomeException) ->
                liftIO $ putStrLn "source choose bottom: raised native-bottom-bool"
            | otherwise -> fail ("FAIL: source choose bottom raised another exception: " ++ displayException exception)
        Right actual -> fail ("FAIL: source choose bottom returned " ++ show actual)
    runNatSourceChecks

-- | Observe source telescopes, constructor refinement, and lazy runtime indices.
runNatSourceChecks :: GHC.Ghc ()
runNatSourceChecks = do
    expression <-
        sourceEntry "NativeSourceNat"
            $ retainedNatSource "NativeSourceNat"
            $ unlines
                [ "do values <- driveNats run"
                , "        appended <- driveNatPairs runAppend"
                , "        headOnly <- let n = bottomNat in observeInt (headNative @Int n (VCons 42 bottomVec))"
                , "        boolHead <- let n = bottomNat in observeBool (headNative @Bool n (VCons True (bottomPolymorphicVec @Bool)))"
                , "        empty <- driveNat (\\n -> observeVec (replicateNative @Int n bottomElement)) Z"
                , "        prefix <- driveNat (\\n -> observeVecPrefix (replicateNative @Int n 42)) cyclicNat"
                , "        partialHead <- driveNat (\\n -> observeVecHead (replicateNative @Int n 42)) partialNat"
                , "        partialAppend <- let n = bottomNat; m = bottomNat in case appendNative @Int n m of !_ -> pure \"forced\""
                , "        telescopes <- driveNatPair runTelescopes (S (S Z), S (S (S Z)))"
                , "        pure (values ++ appended ++ unlines [headOnly,boolHead,empty,prefix,partialHead,partialAppend] ++ telescopes)"
                ]
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source Nat/Vec" expression
    let expected =
            [ "[]:0"
            , "[42,42]:2"
            , "[42,42,42]:3"
            , "[]:0"
            , "[7,7,7]:3"
            , "[42,42]:2"
            , "[42,42,7,7,7]:5"
            , "42"
            , "True"
            , "[]:0"
            , "[42,42,42,42]"
            , "42"
            , "forced"
            ]
                ++ replicate 4 "[42,42]:2"
    unless (lines actual == expected) $
        fail ("FAIL: source Nat/Vec runtime or telescope observation: " ++ show actual)
    forM_
        [ ("NativeSourceNatBottom", "driveNat (\\n -> observeVec (replicateNative @Int n 42)) bottomNat", "native-bottom-nat")
        , ("NativeSourceNatPartial", "driveNat (\\n -> observeVec (replicateNative @Int n 42)) partialNat", "native-bottom-nat")
        , ("NativeSourceNatAppendBottom", "let n = bottomNat; m = bottomNat in case appendNative @Int n m bottomVec bottomVec of !_ -> pure \"forced\"", "native-bottom-vector")
        ]
        $ \(moduleName, observation, fragment) -> do
            bottomExpression <- sourceEntry moduleName $ retainedNatSource moduleName observation
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags moduleName bottomExpression)
            case outcome of
                Left exception
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ moduleName ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ moduleName ++ "; expected bottom, got " ++ show result)
    runNatConstructorSourceChecks
    runNatPartialSourceChecks
    runNatNormalizationSourceChecks
    runCaseSourceChecks

-- | Observe refined source cases and reject malformed dependent alternatives.
runCaseSourceChecks :: GHC.Ghc ()
runCaseSourceChecks = do
    let moduleName = "NativeSourceCases"
        observation =
            unlines
                [ "do values <- driveNats (\\n -> observeVec (replicateCaseNative @Int n 42))"
                , "        original <- driveNats (\\n -> observeVec (replicateOriginal n))"
                , "        shadowed <- driveNats (\\n -> observeVec (replicateShadow n 42))"
                , "        selected <- driveBoth (\\b -> pure (observeCaseNative b (chooseCaseNative b)))"
                , "        partialHead <- driveNat (\\n -> observeVecHead (replicateCaseNative @Int n 42)) partialNat"
                , "        cyclicPrefix <- driveNat (\\n -> observeVecPrefix (replicateCaseNative @Int n 42)) cyclicNat"
                , "        strict <- driveNats (\\n -> case strictCaseNative n bottomVec of !_ -> pure \"forced\")"
                , "        strictPartial <- driveNat (\\n -> case strictCaseNative n bottomVec of !_ -> pure \"forced\") partialNat"
                , "        strictCyclic <- driveNat (\\n -> case strictCaseNative n bottomVec of !_ -> pure \"forced\") cyclicNat"
                , "        ordinary <- driveNats (\\n -> observeInt (ordinaryCase n))"
                , "        pure (values ++ original ++ shadowed ++ selected ++ \"\\n\" ++ unlines [partialHead,cyclicPrefix] ++ strict ++ unlines [strictPartial,strictCyclic] ++ ordinary)"
                ]
    expression <- sourceEntry moduleName (retainedCaseSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source dependent cases" expression
    let expected =
            concat (replicate 3 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
                ++ ["42", "False", "42", "[42,42,42,42]"]
                ++ replicate 5 "forced"
                ++ ["0", "1", "1"]
    unless (lines actual == expected) $
        fail ("FAIL: source case refinement, capture, or strictness: " ++ show actual)
    forM_
        [ ("NativeSourceCaseBottom", "driveNat (\\n -> observeVecHead (replicateCaseNative @Int n 42)) bottomNat", "native-bottom-nat")
        , ("NativeSourceCasePartial", "driveNat (\\n -> observeVec (replicateCaseNative @Int n 42)) partialNat", "native-bottom-nat")
        , ("NativeSourceCaseFunctionBottom", "driveNat (\\n -> case strictCaseNative n bottomVec of !_ -> pure \"forced\") bottomNat", "native-bottom-nat")
        , ("NativeSourceCasePayloadBottom", "driveNat (\\n -> observeInt (strictCaseNative n bottomVec 42)) Z", "native-bottom-vector")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedCaseSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)
    forM_
        [ ("NativeSourceCaseIncomplete", 83605, "Dependent case refinement requires", "S k -> VCons x (replicateCaseNative k x)")
        , ("NativeSourceCaseGuarded", 83605, "Dependent case refinement requires", "Z -> VNil; S k | True -> VCons x (replicateCaseNative k x)")
        , ("NativeSourceCaseWildcard", 83605, "Dependent case refinement requires", "Z -> VNil; S _ -> VCons x VNil")
        , ("NativeSourceCaseWrongLength", 83865, "Vec", "Z -> VNil; S k -> VNil")
        , ("NativeSourceCaseEscapingPredecessor", 83865, "Vec", "Z -> VNil; S k -> replicateCaseNative k x")
        ]
        $ \(name, code, fragment, alternatives) -> do
            let original = "      replicateCaseNative n x = case n of { Z -> VNil; S k -> VCons x (replicateCaseNative k x) }"
                replacement = "      replicateCaseNative n x = case n of { " ++ alternatives ++ " }"
                source =
                    unlines
                        $ map (\line -> if line == original then replacement else line)
                        $ lines (retainedCaseSource name "pure \"unused\"")
            sourceModuleRejects name code fragment name source
    let quoteName = "NativeSourceCaseNameQuotation"
        source =
            "{-# LANGUAGE TemplateHaskellQuotes #-}\n"
                ++ unlines
                    (map replace (lines (retainedCaseSource quoteName "pure \"unused\"")))
        replace "      replicateCaseNative n x = case n of { Z -> VNil; S k -> VCons x (replicateCaseNative k x) }" =
            "      replicateCaseNative n x = case n of { Z -> VNil; S k -> let quoted = ''Int in VCons x (replicateCaseNative k x) }"
        replace line = line
    sourceModuleRejects
        "dependent case name quotation"
        83605
        "Dependent case refinement does not support Template Haskell quotations"
        quoteName
        source

-- | Preserve function WHNF while retained binders remain in the result type.
runNatPartialSourceChecks :: GHC.Ghc ()
runNatPartialSourceChecks = do
    let moduleName = "NativeSourcePartialRetained"
        observation =
            unlines
                [ "do inputs <- driveNats (\\n -> case appendNative @Int n of !_ -> pure \"forced\")"
                , "        bottomIndex <- driveNat (\\n -> case appendNative @Int n of !_ -> pure \"forced\") bottomNat"
                , "        cyclicIndex <- driveNat (\\n -> case appendNative @Int n of !_ -> pure \"forced\") cyclicNat"
                , "        mixedArrow <- driveNat (\\n -> case vacuousMiddle @Int n of !_ -> pure \"forced\") bottomNat"
                , "        mixedRetained <- driveNat (\\n -> case vacuousMiddle @Int n bottomNat of !_ -> pure \"forced\") bottomNat"
                , "        lazyPayload <- let n = bottomNat; m = bottomNat in case appendNative @Int n m bottomVec of !_ -> pure \"forced\""
                , "        constructed <- driveNats (\\n -> case appendNative @Int (S n) of !_ -> pure \"forced\")"
                , "        nested <- driveNats (\\n -> case appendNative @Int (S (S n)) of !_ -> pure \"forced\")"
                , "        constructedBottom <- driveNat (\\n -> case appendNative @Int (S n) of !_ -> pure \"forced\") bottomNat"
                , "        constructedCycle <- driveNat (\\n -> case appendNative @Int (S n) of !_ -> pure \"forced\") cyclicNat"
                , "        nestedBottom <- driveNat (\\n -> case appendNative @Int (S (S n)) of !_ -> pure \"forced\") bottomNat"
                , "        nestedCycle <- driveNat (\\n -> case appendNative @Int (S (S n)) of !_ -> pure \"forced\") cyclicNat"
                , "        latePayload <- driveNat (\\n -> let m = bottomNat in case appendAfterSuccessor n m bottomVec of !_ -> pure \"forced\") bottomNat"
                , "        applied <- driveNats (\\n -> observeVec (appendAfterSuccessor n Z (replicateNative @Int (S n) 42) VNil))"
                , "        headOnly <- driveNat (\\n -> observeVecHead (appendAfterSuccessor n Z (VCons 42 bottomVec) VNil)) bottomNat"
                , "        cyclePrefix <- driveNat (\\n -> observeVecPrefix (appendAfterSuccessor n Z (replicateNative @Int (S n) 42) VNil)) cyclicNat"
                , "        pure (inputs ++ unlines [bottomIndex,cyclicIndex,mixedArrow,mixedRetained,lazyPayload] ++ constructed ++ nested ++ unlines [constructedBottom,constructedCycle,nestedBottom,nestedCycle,latePayload] ++ applied ++ unlines [headOnly,cyclePrefix])"
                ]
    expression <- sourceEntry moduleName (retainedNatSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source partial retained functions" expression
    let expected = replicate 19 "forced" ++ ["[42]:1", "[42,42,42]:3", "[42,42,42,42]:4", "42", "[42,42,42,42]"]
    unless (lines actual == expected) $
        fail ("FAIL: source partial retained function demanded an index or payload: " ++ show actual)
    let emptyName = "NativeSourceRetainedEmpty"
        emptySource =
            unlines
                [ "{-# LANGUAGE EmptyCase #-}"
                , "{-# OPTIONS_GHC -Werror=incomplete-patterns #-}"
                ]
                ++ retainedNatSource
                    emptyName
                    "driveNat (\\n -> case appendNative @Int n of {}) bottomNat"
    targets <- GHC.getTargets
    result <- GHC.handleSourceError (pure . Left) $ do
        void (sourceEntry emptyName emptySource)
        pure (Right ())
    GHC.setTargets targets
    checkSourceRejection "empty case on a retained function" 62161 "non-exhaustive" result
    let boundaryName = "NativeSourceConstructorPiBoundary"
    boundary <-
        sourceEntry boundaryName $
            retainedNatSource
                boundaryName
                "let n = bottomNat in case appendNative @Int (S n) of !_ -> pure \"forced\""
    boundaryEnvironment <- GHC.getSession
    boundaryActual <- liftIO $ execute boundaryEnvironment flags "source remaining retained result" boundary
    unless (boundaryActual == "forced") $
        fail ("FAIL: constructed partial function demanded its predecessor: " ++ show boundaryActual)
    let unsignedName = "NativeSourceUnsignedConstructedPartial"
    sourceModuleRejects
        "unsigned constructed partial alias"
        83605
        "requires an explicit signature"
        unsignedName
        (retainedNatSource unsignedName "driveNat (\\n -> let f = appendNative @Int (S n) in case f of !_ -> pure \"forced\") Z")
    forM_
        [ ("NativeSourcePartialPayloadBottom", "driveNat (\\n -> observeVec (appendAfterSuccessor n Z bottomVec VNil)) Z", "native-bottom-vector")
        , ("NativeSourcePartialPredecessorBottom", "driveNat (\\n -> observeVec (appendAfterSuccessor n Z (replicateNative @Int (S n) 42) VNil)) bottomNat", "native-bottom-nat")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatSource name body)
            bottomEnvironment <- GHC.getSession
            result <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case result of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)

-- | Normalize signed retained aliases without forcing their indices or payloads.
runNatNormalizationSourceChecks :: GHC.Ghc ()
runNatNormalizationSourceChecks = do
    let moduleName = "NativeSourceNatNormalization"
        observation =
            unlines
                [ "do values <- driveNats (\\n -> observeVec (leftUnit n (replicateNative @Int n 42)))"
                , "        appended <- driveNatPairs (\\n m -> observeVec (leftUnitAppend n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
                , "        constants <- driveNats (\\n -> observeInt (constantAlias n))"
                , "        constantBottom <- driveNat (\\n -> observeInt (constantAlias n)) bottomNat"
                , "        partial <- driveNat (\\n -> case leftUnitAppend n of !_ -> pure \"forced\") bottomNat"
                , "        identityPartial <- driveNat (\\n -> case leftUnit n of !_ -> pure \"forced\") bottomNat"
                , "        cyclicPartial <- driveNat (\\n -> case leftUnitAppend n of !_ -> pure \"forced\") cyclicNat"
                , "        latePayload <- let n = bottomNat; m = bottomNat in case leftUnitAppend n m bottomVec of !_ -> pure \"forced\""
                , "        headOnly <- driveNat (\\n -> observeVecHead (leftUnit n (replicateNative @Int n 42))) partialNat"
                , "        prefix <- driveNat (\\n -> observeVecPrefix (leftUnit n (replicateNative @Int n 42))) cyclicNat"
                , "        pure (values ++ appended ++ constants ++ unlines [constantBottom,partial,identityPartial,cyclicPartial,latePayload,headOnly,prefix])"
                ]
    expression <- sourceEntry moduleName (retainedNatSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source retained normalization" expression
    let expected =
            [ "[]:0"
            , "[42,42]:2"
            , "[42,42,42]:3"
            , "[]:0"
            , "[7,7,7]:3"
            , "[42,42]:2"
            , "[42,42,7,7,7]:5"
            ]
                ++ replicate 4 "42"
                ++ replicate 4 "forced"
                ++ ["42", "[42,42,42,42]"]
    unless (lines actual == expected) $
        fail ("FAIL: source retained normalization changed runtime behavior: " ++ show actual)
    forM_
        [ ("NativeSourceNormalizedPayloadBottom", "driveNat (\\n -> observeVec (leftUnit n bottomVec)) Z", "native-bottom-vector")
        , ("NativeSourceNormalizedAppendBottom", "let n = bottomNat; m = bottomNat in case leftUnitAppend n m bottomVec bottomVec of !_ -> pure \"forced\"", "native-bottom-vector")
        , ("NativeSourceNormalizedPredecessorBottom", "driveNat (\\n -> observeVec (leftUnit n (replicateNative @Int n 42))) partialNat", "native-bottom-nat")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)
    let rightUnitName = "NativeSourceRetainedRightUnit"
        rightUnitSource =
            retainedNatSource rightUnitName "pure \"unused\""
                ++ unlines
                    [ "rightNativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
                    , "rightNativeId n xs = xs"
                    , "rightUnit :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n"
                    , "rightUnit = rightNativeId"
                    ]
    sourceModuleRejects "retained normalization keeps right unit stuck" 83865 "Add" rightUnitName rightUnitSource
    let loopName = "NativeSourceRetainedNormalizationDepth"
        loopSource =
            unlines ["{-# LANGUAGE UndecidableInstances #-}", "{-# OPTIONS_GHC -freduction-depth=12 #-}"]
                ++ retainedNatSource loopName "pure \"unused\""
                ++ unlines
                    [ "type family LoopNat (n :: Nat) :: Nat where"
                    , "  LoopNat n = LoopNat n"
                    , "loopNativeId :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
                    , "loopNativeId n xs = xs"
                    , "loopAlias :: foreach (n :: Nat) -> Vec Int (LoopNat n) -> Vec Int n"
                    , "loopAlias = loopNativeId"
                    ]
    sourceModuleRejects "retained normalization reduction depth" 40404 "Reduction stack overflow" loopName loopSource

-- | Exercise constructor naming with dependent payloads and lazy nested fields.
runNatConstructorSourceChecks :: GHC.Ghc ()
runNatConstructorSourceChecks = do
    let moduleName = "NativeSourceNatConstructors"
        observation =
            unlines
                [ "do values <- driveNats (\\n -> observeVec (replicateNative @Int (S n) 42))"
                , "        nestedValues <- driveNats (\\n -> observeVec (replicateNative @Int (S (S n)) 42))"
                , "        appended <- driveNatPairs (\\n m -> observeVec (appendNative @Int (S n) (S m) (replicateNative @Int (S n) 42) (replicateNative @Int (S m) 7)))"
                , "        headFromConstructor <- driveNat (\\n -> observeInt (headNative @Int n (replicateNative @Int (S n) 42))) bottomNat"
                , "        headAtConstructor <- driveNat (\\n -> observeInt (headNative @Int (S n) (VCons 42 bottomVec))) bottomNat"
                , "        keptConstructor <- driveNat (\\n -> observeVecHead (keepErased @Int (S n) (VCons 42 bottomVec))) bottomNat"
                , "        nestedPrefix <- driveNat (\\n -> pure (show (take 2 (vecList (replicateNative @Int (S (S n)) 42))))) bottomNat"
                , "        cyclicPrefix <- driveNat (\\n -> observeVecPrefix (replicateNative @Int (S n) 42)) cyclicNat"
                , "        partialPrefix <- driveNat (\\n -> pure (show (take 2 (vecList (replicateNative @Int (S n) 42))))) partialNat"
                , "        partialAppend <- let n = bottomNat; m = bottomNat in case appendNative @Int (S n) (S m) of !_ -> pure \"forced\""
                , "        pure (values ++ nestedValues ++ appended ++ unlines [headFromConstructor,headAtConstructor,keptConstructor,nestedPrefix,cyclicPrefix,partialPrefix,partialAppend])"
                ]
    expression <- sourceEntry moduleName (retainedNatSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source Nat constructor arguments" expression
    let expected =
            [ "[42]:1"
            , "[42,42,42]:3"
            , "[42,42,42,42]:4"
            , "[42,42]:2"
            , "[42,42,42,42]:4"
            , "[42,42,42,42,42]:5"
            , "[42,7]:2"
            , "[42,7,7,7,7]:5"
            , "[42,42,42,7]:4"
            , "[42,42,42,7,7,7,7]:7"
            , "42"
            , "42"
            , "42"
            , "[42,42]"
            , "[42,42,42,42]"
            , "[42,42]"
            , "forced"
            ]
    unless (lines actual == expected) $
        fail ("FAIL: source constructor argument transport or laziness: " ++ show actual)
    forM_
        [ ("NativeSourceConstructorBottom", "driveNat (\\n -> observeVec (replicateNative @Int (S n) 42)) bottomNat", "native-bottom-nat")
        , ("NativeSourceConstructorPartial", "driveNat (\\n -> observeVec (replicateNative @Int (S n) 42)) partialNat", "native-bottom-nat")
        , ("NativeSourceConstructorAppendBottom", "let n = bottomNat; m = bottomNat in case appendNative @Int (S n) (S m) bottomVec bottomVec of !_ -> pure \"forced\"", "native-bottom-vector")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedNatSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)

-- | Require a located source diagnostic, without catching internal failures.
sourceRejects :: String -> Int -> String -> [String] -> [String] -> GHC.Ghc ()
sourceRejects label code fragment extensions declarations = do
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
    sourceModuleRejects label code fragment moduleName source

-- | Require a located diagnostic from a complete source module.
sourceModuleRejects :: String -> Int -> String -> String -> String -> GHC.Ghc ()
sourceModuleRejects label code fragment moduleName source = do
    targets <- GHC.getTargets
    result <- GHC.handleSourceError (pure . Left) $ do
        parsed <- parseSourceTarget moduleName source
        void (GHC.typecheckModule GHC.StartAndStopTcMPlugins parsed)
        pure (Right ())
    GHC.setTargets targets
    checkSourceRejection label code fragment result

-- | Inspect source errors without accepting an unlocated error or a panic.
checkSourceRejection :: String -> Int -> String -> Either SourceError () -> GHC.Ghc ()
checkSourceRejection label code fragment result =
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

-- | Reject index mismatches and malformed retained Nat constructor equations.
runNatSourceRejectChecks :: GHC.Ghc ()
runNatSourceRejectChecks = do
    forM_
        [
            ( "NativeSourceNatSwap"
            , 83865
            , "Vec"
            , "      runAppend n m = observeVec (appendNative @Int n m (replicateNative @Int n 42) (replicateNative @Int m 7))"
            , ["      runAppend n m = observeVec (appendNative @Int n m (replicateNative @Int m 42) (replicateNative @Int n 7))"]
            )
        ,
            ( "NativeSourceNatPredecessor"
            , 83865
            , "Vec"
            , "      replicateNative (S k) x = VCons x (replicateNative k x)"
            , ["      replicateNative (S k) x = replicateNative k x"]
            )
        ,
            ( "NativeSourceNatEscape"
            , 25897
            , "Vec"
            , "      replicateNative (S k) x = VCons x (replicateNative k x)"
            ,
                [ "      replicateNative (S k) x = VCons x (replicateNative k x)"
                , "      leak :: forall a m. foreach (n :: Nat) -> a -> Vec a m"
                , "      leak Z x = bottomPolymorphicVec"
                , "      leak (S k) x = replicateNative k x"
                ]
            )
        ,
            ( "NativeSourceNatCoverage"
            , 83605
            , "one unguarded equation for each constructor"
            , "      replicateNative Z x = VNil"
            , []
            )
        ,
            ( "NativeSourceNatWildcard"
            , 83605
            , "Retained constructor fields and later arguments"
            , "      replicateNative (S k) x = VCons x (replicateNative k x)"
            , ["      replicateNative (S _) x = VCons x VNil"]
            )
        ]
        $ \(moduleName, code, fragment, old, replacement) ->
            sourceModuleRejects moduleName code fragment moduleName $
                replace_line old replacement (retainedNatSource moduleName "pure \"unused\"")
    let moduleName = "NativeSourceNatAmbiguous"
        source =
            retainedNatSource moduleName "pure \"unused\""
                ++ unlines
                    [ "type family Payload a where"
                    , "  Payload Int = Int"
                    , "  Payload Bool = Int"
                    , "ambiguous :: forall a. foreach (n :: Nat) -> Vec (Payload a) n"
                    , "ambiguous n = undefined"
                    ]
    sourceModuleRejects "ambiguous erased payload" 83865 "ambiguity check" moduleName source
  where
    replace_line old replacement =
        unlines . concatMap (\line -> if line == old then replacement else [line]) . lines

-- | Check unsupported source forms without desugaring or starting plugins twice.
runSourceRejectChecks :: GHC.Ghc ()
runSourceRejectChecks = do
    mapM_
        check
        [ ("unsupported algebraic domain", 83605, "supported closed first-order algebraic domain", [], local [] "foreach (b :: Int) -> Int -> Int" identity "42")
        , ("missing domain annotation", 83605, "explicit domain type", [], local [] "foreach b -> Int -> Int" identity "42")
        , ("anonymous retained binder", 83605, "requires a variable name", [], local [] "foreach (_ :: Bool) -> Int -> Int" identity "42")
        , ("partial retained signature", 83605, "complete type signature", ["PartialTypeSignatures", "NamedWildCards"], local [] "foreach (b :: Bool) -> _a -> _a" identity "42")
        , ("inner erased forall", 83605, "within the scope of a retained binder", ["RankNTypes"], local [] "foreach (b :: Bool) -> forall a. a -> Select b" ["keep b x = x"] "42")
        , ("non-variable retained pattern", 83605, "bare variable pattern", [], local [] selection ["keep _ x = x"] "42")
        , ("multiple variable equations", 83605, "one unguarded equation for each constructor", [], local [] selection (identity ++ identity) "42")
        , ("producer expression argument", 83605, "local index variable or a supported constructor tree", [], local [] consumer identity "keep (not False) 42")
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
        , ("swapped constructor results", 83865, "Int", [], local [] chooser ["keep True = False", "keep False = 42"] "42")
        , ("missing constructor equation", 83605, "one unguarded equation for each constructor", [], local [] chooser ["keep True = 42"] "42")
        , ("duplicate constructor equation", 83605, "one unguarded equation for each constructor", [], local [] chooser ["keep True = 42", "keep True = 43"] "42")
        , ("guarded constructor equation", 83605, "one unguarded equation for each constructor", [], local [] chooser ["keep True | True = 42", "keep False = False"] "42")
        , ("unsupported later constructor argument", 83605, "bare variable patterns and unrestricted multiplicities", [], local [] selection ["keep True _ = 42", "keep False x = x"] "42")
        , ("linear constructor argument", 83605, "bare variable patterns and unrestricted multiplicities", ["LinearTypes"], local [] "foreach (b :: Bool) -> Select b %1 -> Select b" ["keep True x = x", "keep False x = x"] "42")
        ,
            ( "constructor argument with a synonym-hidden forall"
            , 83605
            , "Retained constructor refinement does not support this type"
            , []
            , ["type Poly (b :: Bool) = forall (a :: Bool). (a ~ b) => Int"]
                ++ local [] "foreach (b :: Bool) -> Poly b -> Int" ["keep True x = 42", "keep False x = 42"] "42"
            )
        ,
            ( "independent choose and keep indices"
            , 83865
            , "second argument"
            , []
            ,
                [ "bad :: Bool -> Bool -> String"
                , "bad b c ="
                , "  let choose :: foreach (i :: Bool) -> Select i"
                , "      choose True = 42"
                , "      choose False = False"
                , "      keep :: foreach (i :: Bool) -> Select i -> Select i"
                , "      keep i x = x"
                , "      observe :: foreach (i :: Bool) -> Select i -> String"
                , "      observe True n = show (n :: Int)"
                , "      observe False p = if p then \"True\" else \"False\""
                , "  in observe b (keep b (choose c))"
                ]
            )
        ]
    runNatSourceRejectChecks
    liftIO $ putStrLn "source rejection checks: ok"
  where
    identity = ["keep b x = x"]
    selection = "foreach (b :: Bool) -> Select b -> Select b"
    chooser = "foreach (b :: Bool) -> Select b"
    consumer = "foreach (b :: Bool) -> ((b ~ b) => Int) -> Int"
    local pragmas signature equations body =
        ["entry :: Int", "entry =", "  let"]
            ++ map ("    " ++) (pragmas ++ ["keep :: " ++ signature] ++ equations)
            ++ ["  in " ++ body]
    check (label, code, fragment, extensions, declarations) =
        sourceRejects label code fragment extensions declarations

-- | Load helper bytecode and obtain its ordinary Core identifiers.
loadHelpers :: GHC.Ghc ([Id], [TyCon])
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
        Just info -> do
            things <- mapM loadedThing (GHC.modInfoExports info)
            pure ([binder | GHC.AnId binder <- things, isGlobalId binder], [tycon | GHC.ATyCon tycon <- things])
  where
    loadedThing name = do
        thing <- GHC.lookupGlobalName name
        case thing of
            Just loaded -> pure loaded
            Nothing -> fail ("FAIL: loaded helper has no global definition: " ++ occNameString (nameOccName name))

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

-- | Build recursive retained Core using the loaded Vec constructor equalities.
natReplication :: TyCon -> TyCon -> DataCon -> DataCon -> DataCon -> DataCon -> IO (Id, CoreExpr)
natReplication nat vec zero successor nil cons = do
    nilValue <- constructor nil [] zeroProof []
    consValue <- constructor cons [TermVarTy predecessor] succProof [Var value, recursive]
    pure
        ( worker
        , Lam
            n
            ( Lam
                value
                ( Case
                    (Var n)
                    caseBinder
                    result
                    [ Alt (DataAlt zero) [zeroProof] nilValue
                    , Alt (DataAlt successor) [predecessor, succProof] consValue
                    ]
                )
            )
        )
  where
    natTy = mkTyConTy nat
    n = local 200 "replicate_n" natTy
    value = local 201 "replicate_value" intTy
    predecessor = local 202 "predecessor" natTy
    caseBinder = local 203 "replicate_case" natTy
    index = TermVarTy n
    result = mkTyConApp vec [intTy, index]
    worker = local 204 "replicate_native" (mkDepFunTy n (mkFunctionType ManyTy intTy result))
    zeroProof = proof 205 index (mkTyConTy (promoteDataCon zero))
    succProof = proof 206 index (mkTyConApp (promoteDataCon successor) [TermVarTy predecessor])
    recursive = mkApps (Var worker) [Var predecessor, Var value]

    constructor con existentialArgs cv fields = do
        let (universals, existentials, equations, theta, arguments, _) = dataConFullSig con
            tyvars = universals ++ existentials
            types = [intTy, index] ++ existentialArgs
            instantiate = substTyWith tyvars types
            description = occNameString (nameOccName (dataConName con))
        unless
            ( length universals == 2
                && length existentials == length existentialArgs
                && all isTyVar existentials
                && null theta
                && length arguments == length fields
            )
            $ fail ("FAIL: unexpected Vec constructor shape: " ++ description)
        case equations of
            [equation] -> do
                let expected = instantiate (mkNomEqPred (mkTyVarTy (eqSpecTyVar equation)) (eqSpecType equation))
                unless (coercionType (mkCoVarCo cv) `eqType` expected) $
                    fail ("FAIL: Vec constructor proof has different endpoints: " ++ description)
            _ -> fail ("FAIL: Vec constructor does not have one length equality: " ++ description)
        unless (and (zipWith (\argument field -> instantiate (scaledThing argument) `eqType` exprType field) arguments fields)) $
            fail ("FAIL: Vec constructor fields have different types: " ++ description)
        let expression = mkApps (Var (dataConWorkId con)) (map Type types ++ [Coercion (mkCoVarCo cv)] ++ fields)
        unless (exprType expression `eqType` result) $
            fail ("FAIL: Vec worker did not retain its actual runtime index: " ++ description)
        pure expression

-- | Keep the ordinary erased GADT predecessor separate from the runtime index.
nativeVecHead :: TyCon -> TyCon -> DataCon -> DataCon -> IO (Id, CoreExpr)
nativeVecHead nat vec successor cons = do
    case (existentials, arguments) of
        ([erased], [cv, first, rest])
            | isTyVar erased
            , isCoVar cv
            , tyVarKind erased `eqType` natTy
            , varType first `eqType` intTy
            , varType rest `eqType` mkTyConApp vec [intTy, mkTyVarTy erased]
            , coercionRole (mkCoVarCo cv) == Nominal
            , coercionLKind (mkCoVarCo cv) `eqType` successorIndex
            , coercionRKind (mkCoVarCo cv) `eqType` mkTyConApp (promoteDataCon successor) [mkTyVarTy erased] ->
                pure
                    ( worker
                    , Lam
                        n
                        ( Lam
                            vector
                            ( Case
                                (Var vector)
                                caseBinder
                                intTy
                                [Alt (DataAlt cons) (existentials ++ arguments) (Var first)]
                            )
                        )
                    )
        _ -> fail "FAIL: native head did not receive an ordinary erased Vec predecessor"
  where
    natTy = mkTyConTy nat
    n = local 220 "head_n" natTy
    successorIndex = mkTyConApp (promoteDataCon successor) [TermVarTy n]
    vectorTy = mkTyConApp vec [intTy, successorIndex]
    vector = local 221 "head_vector" vectorTy
    caseBinder = local 222 "head_case" vectorTy
    worker = local 223 "head_native" (mkDepFunTy n (mkFunctionType ManyTy vectorTy intTy))
    (existentials, arguments) = dataConRepInstPat (map unique [230 .. 239]) ManyTy cons [intTy, successorIndex]

-- | Check the Core intermediate with runtime inputs and lazy recursive fields.
runNatChecks :: HscEnv -> DynFlags -> [Id] -> [TyCon] -> IO ()
runNatChecks environment flags bindings tycons = do
    nat <- namedTyCon "Nat"
    vec <- namedTyCon "Vec"
    add <- namedTyCon "Add"
    zero <- namedDataCon nat "Z"
    successor <- namedDataCon nat "S"
    nil <- namedDataCon vec "VNil"
    cons <- namedDataCon vec "VCons"
    (worker, rhs) <- natReplication nat vec zero successor nil cons
    (headWorker, headRhs) <- nativeVecHead nat vec successor cons
    left <- namedTyCon "IndexLeft"
    right <- namedTyCon "IndexRight"
    bad <- namedTyCon "BadIndex"
    unless (isNativeIndexType (mkTyConTy left) && isNativeIndexType (mkTyConTy right)) $
        fail "FAIL: mutually recursive ordinary index domains were rejected"
    unless (not (isNativeIndexType (mkTyConTy bad))) $
        fail "FAIL: an index-domain cycle hid its reachable Int payload"
    let input = local 210 "runtime_nat" (mkTyConTy nat)
        payload = mkIntExpr (targetPlatform flags) 42
        replicated = mkApps (Var worker) [Var input, payload]
        kernel observer = Let (Rec [(worker, rhs)]) (Lam input (mkApps (Var (helper bindings observer)) [Type (TermVarTy input), replicated]))
        drive observer seed = mkApps (Var (helper bindings "driveNat")) [kernel observer, Var (helper bindings seed)]
        expect label expected expression = do
            actual <- execute environment flags label expression
            unless (actual == expected) $
                fail ("FAIL: " ++ label ++ "; expected " ++ show expected ++ ", got " ++ show actual)
        expectBottom label expression = do
            outcome <- try (execute environment flags label expression) :: IO (Either SomeException String)
            case outcome of
                Left exception | "native-bottom-nat" `isInfixOf` displayException exception -> pure ()
                Left exception -> fail ("FAIL: " ++ label ++ "; wrong exception: " ++ displayException exception)
                Right actual -> fail ("FAIL: " ++ label ++ "; expected bottom, got " ++ show actual)
    expect
        "runtime Nat replication at zero/two/three"
        (unlines ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        (App (Var (helper bindings "driveNats")) (kernel "observeVec"))
    expect "replication head does not force predecessor" "42" (drive "observeVecHead" "partialNat")
    expectBottom "replication tail forces partial predecessor" (drive "observeVec" "partialNat")
    expect "replication of cyclic Nat has a finite prefix" "[42,42,42,42]" (drive "observeVecPrefix" "cyclicNat")
    expectBottom "replication inspects its Nat constructor" (drive "observeVecHead" "bottomNat")
    let emptyKernel =
            Let (Rec [(worker, rhs)]) $
                Lam
                    input
                    ( mkApps
                        (Var (helper bindings "observeVec"))
                        [ Type (TermVarTy input)
                        , mkApps (Var worker) [Var input, Var (helper bindings "bottomElement")]
                        ]
                    )
    expect "zero replication does not force its element" "[]:0" $
        mkApps (Var (helper bindings "driveNat")) [emptyKernel, Var (dataConWorkId zero)]
    let lazyIndex = local 224 "lazy_head_index" (mkTyConTy nat)
        index = TermVarTy lazyIndex
        lengthIndex = mkTyConApp (promoteDataCon successor) [index]
        tailValue = App (Var (helper bindings "bottomVec")) (Type index)
        vectorValue =
            mkApps
                (Var (dataConWorkId cons))
                [Type intTy, Type lengthIndex, Type index, Coercion (mkNomReflCo lengthIndex), payload, tailValue]
        headCall =
            Let (NonRec lazyIndex (Var (helper bindings "bottomNat")))
                $ Let (NonRec headWorker headRhs)
                $ App (Var (helper bindings "observeInt")) (mkApps (Var headWorker) [Var lazyIndex, vectorValue])
    expect "native head ignores its retained index and vector tail" "42" headCall
    let natTy = mkTyConTy nat
        appendN = local 240 "append_n" natTy
        appendM = local 241 "append_m" natTy
        nIndex = TermVarTy appendN
        mIndex = TermVarTy appendM
        leftType = mkTyConApp vec [intTy, nIndex]
        rightType = mkTyConApp vec [intTy, mIndex]
        resultType = mkTyConApp vec [intTy, mkTyConApp add [nIndex, mIndex]]
        xs = local 242 "append_xs" leftType
        ys = local 243 "append_ys" rightType
        appendRhs =
            Lam
                appendN
                ( Lam
                    appendM
                    ( Lam
                        xs
                        ( Lam
                            ys
                            (mkApps (Var (helper bindings "appendVec")) [Type nIndex, Type mIndex, Var xs, Var ys])
                        )
                    )
                )
        appendType =
            mkDepFunTy
                appendN
                ( mkDepFunTy
                    appendM
                    (mkFunctionType ManyTy leftType (mkFunctionType ManyTy rightType resultType))
                )
        appendWorker = local 244 "append_native" appendType
        secondInput = local 245 "second_runtime_nat" natTy
        appendedIndex = mkTyConApp add [TermVarTy input, TermVarTy secondInput]
        leftVector = mkApps (Var worker) [Var input, payload]
        rightVector = mkApps (Var worker) [Var secondInput, mkIntExpr (targetPlatform flags) 7]
        appended = mkApps (Var appendWorker) [Var input, Var secondInput, leftVector, rightVector]
        appendKernel =
            Let (Rec [(worker, rhs)])
                $ Let (NonRec appendWorker appendRhs)
                $ Lam
                    input
                    ( Lam
                        secondInput
                        (mkApps (Var (helper bindings "observeVec")) [Type appendedIndex, appended])
                    )
    unless (exprType appendRhs `eqType` appendType) $
        fail "FAIL: native append lost its two retained runtime binders"
    expect
        "runtime Nat append at zero/zero, zero/three, two/zero, two/three"
        (unlines ["[]:0", "[7,7,7]:3", "[42,42]:2", "[42,42,7,7,7]:5"])
        (App (Var (helper bindings "driveNatPairs")) appendKernel)
    let lazyN = local 246 "lazy_append_n" natTy
        lazyM = local 247 "lazy_append_m" natTy
        partialAppend = mkApps (Var appendWorker) [Var lazyN, Var lazyM]
        forcePartial =
            Let (NonRec lazyN (Var (helper bindings "bottomNat")))
                $ Let (NonRec lazyM (Var (helper bindings "bottomNat")))
                $ Let (NonRec appendWorker appendRhs)
                $ mkApps (Var (helper bindings "force")) [Type (exprType partialAppend), partialAppend]
    expect "partial append does not force either retained Nat" "forced" forcePartial
    let bottomAt binder = App (Var (helper bindings "bottomVec")) (Type (TermVarTy binder))
        fullAppend = mkApps partialAppend [bottomAt lazyN, bottomAt lazyM]
        forceFull =
            Let (NonRec lazyN (Var (helper bindings "bottomNat")))
                $ Let (NonRec lazyM (Var (helper bindings "bottomNat")))
                $ Let (NonRec appendWorker appendRhs)
                $ mkApps (Var (helper bindings "force")) [Type (exprType fullAppend), fullAppend]
    fullOutcome <- try (execute environment flags "append demands a vector before either index" forceFull) :: IO (Either SomeException String)
    case fullOutcome of
        Left exception | "native-bottom-vector" `isInfixOf` displayException exception -> pure ()
        Left exception -> fail ("FAIL: append forced an unexpected argument: " ++ displayException exception)
        Right actual -> fail ("FAIL: append returned without inspecting its bottom vector: " ++ show actual)
    let successorIndex = mkTyConApp (promoteDataCon successor) [TermVarTy input]
        successorValue = App (Var (dataConWorkId successor)) (Var input)
        successorKernel observer =
            Let (Rec [(worker, rhs)]) $
                Lam
                    input
                    ( mkApps
                        (Var (helper bindings observer))
                        [Type successorIndex, mkApps (Var worker) [successorValue, payload]]
                    )
        constructorArgument observer seed =
            mkApps
                (Var (helper bindings "driveNat"))
                [successorKernel observer, seed]
        nestedIndex = mkTyConApp (promoteDataCon successor) [successorIndex]
        nestedValue = App (Var (dataConWorkId successor)) successorValue
        nestedKernel =
            Let (Rec [(worker, rhs)]) $
                Lam
                    input
                    ( mkApps
                        (Var (helper bindings "observeVec"))
                        [Type nestedIndex, mkApps (Var worker) [nestedValue, payload]]
                    )
    expect "constructed retained Nat argument" "[42]:1" $
        constructorArgument "observeVec" (Var (dataConWorkId zero))
    expect "constructed retained Nat does not force its field" "42" $
        constructorArgument "observeVecHead" (Var (helper bindings "bottomNat"))
    expectBottom "constructed retained Nat tail demands its partial field" $
        constructorArgument "observeVec" (Var (helper bindings "partialNat"))
    expect "nested constructed retained Nat argument" "[42,42]:2" $
        mkApps (Var (helper bindings "driveNat")) [nestedKernel, Var (dataConWorkId zero)]
    let partialConstructed = App (Var appendWorker) successorValue
        partialConstructedKernel =
            Lam input
                $ Let (NonRec appendWorker appendRhs)
                $ mkApps (Var (helper bindings "force")) [Type (exprType partialConstructed), partialConstructed]
        retainedResultBoundary =
            mkApps
                (Var (helper bindings "driveNat"))
                [partialConstructedKernel, Var (dataConWorkId zero)]
    expect "constructed argument with retained result" "forced" retainedResultBoundary
    let savedPartial = local 254 "saved_constructed_partial" (exprType partialConstructed)
        zeroValue = Var (dataConWorkId zero)
        savedApplication =
            mkApps
                (Var savedPartial)
                [zeroValue, mkApps (Var worker) [successorValue, payload], mkApps (Var worker) [zeroValue, payload]]
        savedResultIndex = mkTyConApp add [successorIndex, mkTyConTy (promoteDataCon zero)]
        savedKernel =
            Let (Rec [(worker, rhs)])
                $ Lam input
                $ Let (NonRec appendWorker appendRhs)
                $ Let (NonRec savedPartial partialConstructed)
                $ mkApps (Var (helper bindings "observeVec")) [Type savedResultIndex, savedApplication]
    expect
        "saved constructed partial function accepts later arguments"
        (unlines ["[42]:1", "[42,42,42]:3", "[42,42,42,42]:4"])
        (App (Var (helper bindings "driveNats")) savedKernel)
    let bottomIndex = local 250 "bottom_function_index" natTy
        bottomPayload = local 251 "bottom_function_payload" (mkTyConApp vec [intTy, TermVarTy bottomIndex])
        bottomFunctionType = exprType (Lam bottomIndex (Lam bottomPayload (Var bottomPayload)))
        bottomFunction = local 252 "bottom_retained_function" bottomFunctionType
        bottomFunctionRhs = App (Var (helper bindings "bottomFunction")) (Type bottomFunctionType)
        bottomPartial = App (Var bottomFunction) successorValue
        bottomFunctionKernel =
            Lam input
                $ Let (NonRec bottomFunction bottomFunctionRhs)
                $ mkApps (Var (helper bindings "force")) [Type (exprType bottomPartial), bottomPartial]
    bottomOutcome <-
        try
            ( execute environment flags "constructor transport does not eta-expand a bottom function" $
                mkApps (Var (helper bindings "driveNat")) [bottomFunctionKernel, Var (dataConWorkId zero)]
            ) ::
            IO (Either SomeException String)
    case bottomOutcome of
        Left exception | "native-bottom-function" `isInfixOf` displayException exception -> pure ()
        Left exception -> fail ("FAIL: constructed bottom function raised another exception: " ++ displayException exception)
        Right value -> fail ("FAIL: constructor transport hid a bottom function: " ++ show value)
    let natTy = mkTyConTy nat
        keepN = local 211 "unused_nat" natTy
        indexedNat index = mkFunctionType ManyTy (mkTyConApp eqTyCon [natTy, index, index]) intTy
        keepValue = local 212 "keep_value" (indexedNat (TermVarTy keepN))
        keepExpr = Lam keepN (Lam keepValue (Var keepValue))
        keepId = local 213 "keep_nat" (exprType keepExpr)
        index = TermVarTy input
        dictionary = local 214 "nat_dictionary" (mkTyConApp eqTyCon [natTy, index, index])
        value = Lam dictionary payload
        reflexive = mkCoreConApps eqDataCon [Type natTy, Type index, Type index, Coercion (mkNomReflCo index)]
        kept = mkApps (Var keepId) [Var input, value, reflexive]
        keepKernel = Lam input (Let (NonRec keepId keepExpr) (App (Var (helper bindings "observeInt")) kept))
    expect "unused retained Nat does not force its input" "42" $
        mkApps (Var (helper bindings "driveNat")) [keepKernel, Var (helper bindings "bottomNat")]
  where
    namedTyCon label = case find ((== label) . occNameString . nameOccName . tyConName) tycons of
        Just tycon -> pure tycon
        Nothing -> fail ("FAIL: missing loaded type constructor: " ++ label)
    namedDataCon tycon label = case find ((== label) . occNameString . nameOccName . dataConName) (tyConDataCons tycon) of
        Just constructor -> pure constructor
        Nothing -> fail ("FAIL: missing loaded data constructor: " ++ label)

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
    (bindings, tycons) <- loadHelpers
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
    liftIO $ runNatChecks environment flags bindings tycons
    runSourceChecks
    runSourceRejectChecks
