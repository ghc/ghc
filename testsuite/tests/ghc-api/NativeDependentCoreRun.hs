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
import GHC.Core.FVs (exprFreeVars)
import GHC.Core.FamInstEnv (mkBranchedCoAxiom, mkCoAxBranch)
import GHC.Core.Lint (lintExpr)
import GHC.Core.Make (mkCoreConApps, mkIntExpr)
import GHC.Core.Opt.Arity (etaExpand)
import GHC.Core.Predicate (mkNomEqPred)
import qualified GHC.Core.Subst as CoreSubst
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
import GHC.Types.Var.Env (mkInScopeSet)
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
    unlines $
        [ "{-# LANGUAGE DataKinds, FlexibleInstances, GADTs, KindSignatures, TypeFamilies #-}"
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
            ++ retainedNatContextClasses

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

-- | Share lifted arithmetic proofs between local and imported source checks.
retainedNatProofDeclarations :: [String]
retainedNatProofDeclarations =
    [ "rightZero :: foreach (n :: Nat) -> Add n Z :~: n"
    , "rightZero Z = Refl"
    , "rightZero (S k) = case rightZero k of Refl -> Refl"
    , "rightUnit :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n"
    , "rightUnit n xs = case rightZero n of Refl -> xs"
    , "transportWith :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
    , "transportWith n p xs = case p of Refl -> xs"
    , "ignoreProof :: foreach (n :: Nat) -> (Add n Z :~: n) -> Int"
    , "ignoreProof n _ = 42"
    , "bottomProof :: forall n. Add n Z :~: n"
    , "bottomProof = error \"native-bottom-proof\""
    ]

-- | Add proof syntax and local workers without changing other Nat source cases.
retainedNatProofSource :: String -> String -> String
retainedNatProofSource moduleName observation =
    unlines $ "{-# LANGUAGE TypeOperators #-}" : concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"import NativeRuntimeHelpers hiding (appendVec)" =
        [line, "import Data.Type.Equality ((:~:)(Refl))"]
    insert line@"      run :: Nat -> IO String" =
        map ("      " ++) retainedNatProofDeclarations ++ [line]
    insert line = [line]

-- | Observe checked proofs and unused proof thunks through runtime input drivers.
retainedNatProofObservation :: [String]
retainedNatProofObservation =
    [ "proofs <- driveNats (\\n -> case rightZero n of Refl -> pure \"Refl\")"
    , "transported <- driveNats (\\n -> observeVec (rightUnit n (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "supplied <- driveNats (\\n -> observeVec (transportWith n (rightZero n) (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "ignored <- driveNats (\\n -> observeInt (ignoreProof n (rightZero n)))"
    , "ignoredBottom <- driveNat (\\n -> observeInt (ignoreProof n (rightZero n))) bottomNat"
    , "ignoredProofBottom <- driveNat (\\n -> observeInt (ignoreProof n bottomProof)) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (ignoreProof n (rightZero n))) cyclicNat"
    , "partialBottom <- driveNat (\\n -> case rightUnit n of !_ -> pure \"forced\") bottomNat"
    , "partialCyclic <- driveNat (\\n -> case rightUnit n of !_ -> pure \"forced\") cyclicNat"
    , "pure (proofs ++ transported ++ supplied ++ ignored ++ unlines [ignoredBottom,ignoredProofBottom,ignoredCyclic,partialBottom,partialCyclic])"
    ]

-- | Preserve proof evaluation and transport results across source and disk imports.
retainedNatProofExpected :: [String]
retainedNatProofExpected =
    replicate 3 "Refl"
        ++ concat (replicate 2 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ replicate 6 "42"
        ++ replicate 2 "forced"

-- | Use ordinary polymorphic consumers with retained proof arguments.
retainedNatGenericDeclarations :: [String]
retainedNatGenericDeclarations =
    [ "ignoreAny :: a -> Int"
    , "ignoreAny _ = 42"
    , "forceSupplied :: foreach (n :: Nat) -> (Add n Z :~: n) -> IO String"
    , "forceSupplied n p = force p"
    , "linearId :: a %1 -> a"
    , "linearId x = x"
    , "nativeProofLinear :: foreach (n :: Nat) -> (Add n Z :~: n) %1 -> Int"
    , "nativeProofLinear n p = case linearId p of Refl -> 42"
    ]

-- | Enable linear syntax only for the generic proof consumer source cases.
retainedNatGenericSource :: String -> String -> String
retainedNatGenericSource moduleName observation =
    unlines $ "{-# LANGUAGE LinearTypes #-}" : concatMap insert (lines (retainedNatProofSource moduleName observation))
  where
    insert line@"import Data.Type.Equality ((:~:)(Refl))" =
        [line, "import GHC.Types (Multiplicity(Many))"]
    insert line@"      run :: Nat -> IO String" =
        map (("      " ++) . localDeclaration) retainedNatGenericDeclarations ++ [line]
    insert line = [line]

    -- The local helper must support a consumer that is called many times.
    -- Its argument still has multiplicity One.
    localDeclaration "linearId x = x" = "%'Many linearId = \\x -> x"
    localDeclaration line = line

-- | Check inferred consumer types and preserve lazy argument evaluation.
retainedNatGenericObservation :: [String]
retainedNatGenericObservation =
    [ "proofs <- driveNats (\\n -> force (rightZero n))"
    , "identities <- driveNats (\\n -> force (id (rightZero n)))"
    , "wrapped <- driveNats (\\n -> force (Just (rightZero n)))"
    , "constants <- driveNats (\\n -> force (const (rightZero n) True))"
    , "emptyConstants <- driveNats (\\n -> force (const (rightZero n) Nothing))"
    , "ignored <- driveNats (\\n -> observeInt (ignoreAny (rightZero n)))"
    , "vectors <- driveNats (\\n -> force (replicateNative @Int n 42))"
    , "linear <- driveNats (\\n -> observeInt (nativeProofLinear n (rightZero n)))"
    , "ignoredBottom <- driveNat (\\n -> observeInt (ignoreAny (rightZero n))) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (ignoreAny (rightZero n))) cyclicNat"
    , "wrappedBottom <- driveNat (\\n -> force (Just (rightZero n))) bottomNat"
    , "wrappedCyclic <- driveNat (\\n -> force (Just (rightZero n))) cyclicNat"
    , "constantBottom <- driveNat (\\n -> force (const True (rightZero n))) bottomNat"
    , "constantCyclic <- driveNat (\\n -> force (const True (rightZero n))) cyclicNat"
    , "functionBottom <- driveNat (\\n -> force (appendNative @Int n)) bottomNat"
    , "functionCyclic <- driveNat (\\n -> force (appendNative @Int n)) cyclicNat"
    , "functionIdentityBottom <- driveNat (\\n -> force (id (appendNative @Int n))) bottomNat"
    , "functionIdentityCyclic <- driveNat (\\n -> force (id (appendNative @Int n))) cyclicNat"
    , "pure (proofs ++ identities ++ wrapped ++ constants ++ emptyConstants ++ ignored ++ vectors ++ linear ++ unlines [ignoredBottom,ignoredCyclic,wrappedBottom,wrappedCyclic,constantBottom,constantCyclic,functionBottom,functionCyclic,functionIdentityBottom,functionIdentityCyclic])"
    ]

-- | Keep the generic observations in the same order for local and imported code.
retainedNatGenericExpected :: [String]
retainedNatGenericExpected =
    replicate 15 "forced"
        ++ replicate 3 "42"
        ++ replicate 3 "forced"
        ++ replicate 3 "42"
        ++ ["42", "42", "forced", "forced", "forced", "forced"]
        ++ replicate 4 "forced"

-- | Share ordinary class dictionaries and checked native proof consumers.
retainedNatClassDeclarations :: [String]
retainedNatClassDeclarations =
    [ "instance Show a => Show (Vec a n) where"
    , "  showsPrec _ VNil = showString \"VNil\""
    , "  showsPrec d (VCons x xs) = showParen (d > 10) (showString \"VCons \" . showsPrec 11 x . showChar ' ' . showsPrec 11 xs)"
    , "ignoreShow :: Show a => a -> Int"
    , "ignoreShow _ = 42"
    , "renderPair :: (Show a, Show b) => a -> b -> String"
    , "renderPair p q = show p ++ \"/\" ++ show q"
    , "sameValue :: forall a b. (a ~ b) => a -> b"
    , "sameValue x = x"
    , "viaEquality :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
    , "viaEquality n p xs = case p of Refl -> sameValue xs"
    , "classId :: forall a. Show a => a -> a"
    , "classId x = x"
    , "taggedClassId :: forall tag a. Show a => a -> a"
    , "taggedClassId x = x"
    , "lateClassId :: Bool -> forall a. Show a => a -> a"
    , "lateClassId _ x = x"
    , "checkedVta :: foreach (n :: Nat) -> (Add n Z :~: n) -> (Add n Z :~: n)"
    , "checkedVta n p = taggedClassId @Bool p"
    , "checkedLate :: foreach (n :: Nat) -> (Add n Z :~: n) -> (Add n Z :~: n)"
    , "checkedLate n p = lateClassId True p"
    , "showSupplied :: foreach (n :: Nat) -> (Add n Z :~: n) -> String"
    , "showSupplied n p = show p"
    , "ignoreSuppliedShow :: foreach (n :: Nat) -> (Add n Z :~: n) -> Int"
    , "ignoreSuppliedShow n p = ignoreShow p"
    , "showProofTwice :: foreach (n :: Nat) -> (Add n Z :~: n) -> String"
    , "showProofTwice n p = renderPair p p"
    , "showProofPair :: foreach (n :: Nat) (m :: Nat) -> (Add n Z :~: n) -> (Add m Z :~: m) -> String"
    , "showProofPair n m p q = renderPair p q"
    , "showProofWithInt :: foreach (n :: Nat) -> (Add n Z :~: n) -> String"
    , "showProofWithInt n p = let answer = (42 :: Int) in renderPair answer p"
    ]

-- | Put the class instance at module scope and retain the local Nat workers.
retainedNatClassSource :: String -> String -> String
retainedNatClassSource moduleName observation =
    unlines $ "{-# LANGUAGE RankNTypes #-}" : concatMap insert (lines (retainedNatProofSource moduleName observation))
  where
    insert line@"entry :: IO String" = retainedNatClassDeclarations ++ [line]
    insert line = [line]

-- | Resolve dictionaries without changing proof or vector evaluation.
retainedNatClassObservation :: [String]
retainedNatClassObservation =
    [ "proofs <- driveNats (\\n -> pure (show (rightZero n)))"
    , "vectors <- driveNats (\\n -> pure (show (replicateNative @Int n 42)))"
    , "nested <- driveNats (\\n -> pure (show (Just (rightZero n))))"
    , "ignored <- driveNats (\\n -> observeInt (ignoreShow (rightZero n)))"
    , "repeated <- driveNats (\\n -> pure (showProofTwice n (rightZero n)))"
    , "distinct <- driveNatPairs (\\n m -> pure (showProofPair n m (rightZero n) (rightZero m)))"
    , "mixed <- driveNats (\\n -> pure (showProofWithInt n (rightZero n)))"
    , "returned <- driveNats (\\n -> pure (show (classId (rightZero n))))"
    , "visible <- driveNats (\\n -> pure (show (checkedVta n (rightZero n))))"
    , "late <- driveNats (\\n -> pure (show (checkedLate n (rightZero n))))"
    , "localRendered <- driveNats (\\n -> let rendered = show (rightZero n) in pure rendered)"
    , "equalities <- driveNats (\\n -> observeVec (viaEquality n (rightZero n) (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "ignoredBottom <- driveNat (\\n -> observeInt (ignoreShow (rightZero n))) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (ignoreShow (rightZero n))) cyclicNat"
    , "ignoredProofBottom <- driveNat (\\n -> observeInt (ignoreSuppliedShow n bottomProof)) bottomNat"
    , "ignoredEqualityBottom <- driveNat (\\n -> observeInt (ignoreShow (viaEquality n bottomProof bottomVec))) bottomNat"
    , "ignoredEqualityCyclic <- driveNat (\\n -> observeInt (ignoreShow (viaEquality n bottomProof bottomVec))) cyclicNat"
    , "pure (proofs ++ vectors ++ nested ++ ignored ++ repeated ++ distinct ++ mixed ++ returned ++ visible ++ late ++ localRendered ++ equalities ++ unlines [ignoredBottom,ignoredCyclic,ignoredProofBottom,ignoredEqualityBottom,ignoredEqualityCyclic])"
    ]

-- | Preserve class observations in local source and interface-only consumers.
retainedNatClassExpected :: [String]
retainedNatClassExpected =
    replicate 3 "Refl"
        ++ ["VNil", "VCons 42 (VCons 42 VNil)", "VCons 42 (VCons 42 (VCons 42 VNil))"]
        ++ replicate 3 "Just Refl"
        ++ replicate 3 "42"
        ++ replicate 7 "Refl/Refl"
        ++ replicate 3 "42/Refl"
        ++ replicate 12 "Refl"
        ++ ["[]:0", "[42,42]:2", "[42,42,42]:3"]
        ++ replicate 5 "42"

-- | Keep class selectors global when source entry expressions are localized.
retainedNatContextClasses :: [String]
retainedNatContextClasses =
    [ "class Contextual (n :: Nat) where"
    , "  contextLabel :: Vec Int n -> String"
    , "instance Contextual Z where"
    , "  contextLabel _ = \"zero\""
    , "instance Contextual (S n) where"
    , "  contextLabel _ = \"successor\""
    , "class OnlyGiven (n :: Nat) where"
    , "  onlyLabel :: Vec Int n -> String"
    ]

-- | Keep incoming dictionaries inside retained variable and constructor scopes.
retainedNatContextDeclarations :: [String]
retainedNatContextDeclarations =
    [ "renderContext :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderContext n xs = show xs"
    , "renderCtor :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderCtor Z xs = show xs"
    , "renderCtor (S k) xs = show xs"
    , "renderGuarded :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderGuarded n xs | True = rendered"
    , "  where"
    , "    rendered :: String"
    , "    rendered = renderContext n xs"
    , "renderView :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderView n (renderContext n -> rendered) = rendered"
    , "renderLambda :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderLambda n = \\xs -> show xs"
    , "renderFull :: foreach (n :: Nat) -> Show (Vec Int (S n)) => Vec Int (S n) -> String"
    , "renderFull n xs@(VCons _ _) = show xs"
    , "forwardContext :: foreach (n :: Nat) -> Contextual n => Vec Int n -> String"
    , "forwardContext n xs = contextLabel xs"
    , "provideContext :: foreach (n :: Nat) -> Vec Int n -> String"
    , "provideContext Z xs = forwardContext Z xs"
    , "provideContext (S k) xs = forwardContext (S k) xs"
    , "onlyGivenContext :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "onlyGivenContext n xs = onlyLabel xs"
    , "onlyGivenCtor :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "onlyGivenCtor Z xs = onlyLabel xs"
    , "onlyGivenCtor (S k) xs = onlyLabel xs"
    , "onlyGivenWhereCtor :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "onlyGivenWhereCtor Z xs = rendered"
    , "  where rendered :: String; rendered = onlyLabel xs"
    , "onlyGivenWhereCtor (S k) xs = rendered"
    , "  where rendered :: String; rendered = onlyLabel xs"
    , "onlyGivenGuardedPartial :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "onlyGivenGuardedPartial n | True = \\xs -> rendered ++ onlyLabel xs"
    , "  where rendered :: String; rendered = onlyGivenContext n bottomVec"
    , "lateOnlyGiven :: foreach (n :: Nat) -> Vec Int n -> OnlyGiven n => String"
    , "lateOnlyGiven n xs = onlyLabel xs"
    , "lateOnlyGivenCtor :: foreach (n :: Nat) -> Vec Int n -> OnlyGiven n => String"
    , "lateOnlyGivenCtor Z xs = rendered"
    , "  where rendered :: String; rendered = onlyLabel xs"
    , "lateOnlyGivenCtor (S k) xs = rendered"
    , "  where rendered :: String; rendered = onlyLabel xs"
    , "renderContexts :: foreach (n :: Nat) -> foreach (m :: Nat) -> (Show (Vec Int n), Show (Vec Int m)) => Vec Int n -> Vec Int m -> String"
    , "renderContexts n m xs ys = renderContext n xs ++ \"/\" ++ renderContext m ys"
    , "equalityContext :: foreach (n :: Nat) -> (Add n Z ~ n) => Vec Int (Add n Z) -> Vec Int n"
    , "equalityContext n xs = xs"
    , "equalityCtor :: foreach (n :: Nat) -> (Add n Z ~ n) => Vec Int (Add n Z) -> Vec Int n"
    , "equalityCtor Z xs = xs"
    , "equalityCtor (S k) xs = xs"
    , "ignoreContext :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> Int"
    , "ignoreContext n _ = 42"
    , "ignoreEqualityContext :: foreach (n :: Nat) -> (n ~ n) => Vec Int n -> Int"
    , "ignoreEqualityContext n _ = 42"
    , "legacyContext :: foreach (unused :: Bool) -> foreach (b :: Bool) -> (b ~ b) => Int"
    , "legacyContext _ b = 42"
    , "legacyBangContext :: foreach (b :: Bool) -> Int -> (b ~ b) => Int"
    , "legacyBangContext b !x = x"
    , "wildcardContext :: foreach (unused :: Bool) -> foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "wildcardContext _ n xs = show xs"
    , "keepConstrained :: foreach (b :: Bool) -> ((b ~ b) => Int) -> ((b ~ b) => Int)"
    , "keepConstrained b x = x"
    , "type ContextPair (n :: Nat) = (OnlyGiven n, Show (Vec Int n))"
    , "pairContext :: foreach (n :: Nat) -> ContextPair n => Vec Int n -> String"
    , "pairContext n xs = onlyLabel xs ++ show xs"
    , "pairCtorContext :: foreach (n :: Nat) -> ContextPair n => Vec Int n -> String"
    , "pairCtorContext Z xs = onlyLabel xs ++ show xs"
    , "pairCtorContext (S k) xs = onlyLabel xs ++ show xs"
    , "caseOnlyGiven :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "caseOnlyGiven n xs = case n of { Z -> onlyLabel xs; S k -> onlyLabel xs }"
    , "casePairGiven :: foreach (n :: Nat) -> ContextPair n => Vec Int n -> String"
    , "casePairGiven n xs = case n of { Z -> onlyLabel xs ++ show xs; S k -> onlyLabel xs ++ show xs }"
    , "caseOnlyWhere :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "caseOnlyWhere n xs = case n of"
    , "  Z -> rendered where rendered :: String; rendered = onlyLabel xs"
    , "  S k -> rendered where rendered :: String; rendered = onlyLabel xs"
    , "caseNestedOnlyGiven :: foreach (n :: Nat) -> foreach (m :: Nat) -> (OnlyGiven n, OnlyGiven m) => Vec Int n -> Vec Int m -> String"
    , "caseNestedOnlyGiven n m xs ys = case n of"
    , "  Z -> case m of { Z -> onlyLabel xs ++ onlyLabel ys; S n -> onlyLabel xs ++ onlyLabel ys }"
    , "  S n -> case m of { Z -> onlyLabel xs ++ onlyLabel ys; S m -> onlyLabel xs ++ onlyLabel ys }"
    , "renderCaseContext :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> String"
    , "renderCaseContext n xs = case n of { Z -> show xs; S k -> show xs }"
    , "renderCasePair :: foreach (n :: Nat) -> foreach (m :: Nat) -> (Show (Vec Int n), Show (Vec Int m)) => Vec Int n -> Vec Int m -> String"
    , "renderCasePair n m xs ys = case n of"
    , "  Z -> case m of { Z -> show xs ++ \"/\" ++ show ys; S n -> show xs ++ \"/\" ++ show ys }"
    , "  S n -> case m of { Z -> show xs ++ \"/\" ++ show ys; S m -> show xs ++ \"/\" ++ show ys }"
    , "caseEqualityContext :: foreach (n :: Nat) -> (Add n Z ~ n) => Vec Int (Add n Z) -> Vec Int n"
    , "caseEqualityContext n xs = case n of { Z -> xs; S k -> xs }"
    , "casePrimitiveEquality :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
    , "casePrimitiveEquality n proof xs = case proof of Refl -> case n of { Z -> xs; S k -> xs }"
    , "casePartial :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> Int -> String"
    , "casePartial n xs = case n of { Z -> \\_ -> show xs; S k -> \\_ -> show xs }"
    , "noCasePartial :: foreach (n :: Nat) -> Show (Vec Int n) => Vec Int n -> Int -> String"
    , "noCasePartial n xs _ = show xs"
    , "outerOnlyGivenArrow :: foreach (n :: Nat) -> OnlyGiven n => () -> String"
    , "outerOnlyGivenArrow n = proc () -> (case n of { Z -> \\() -> onlyGivenContext n bottomVec; S k -> \\() -> onlyGivenContext n bottomVec }) -< ()"
    ]
        ++ retainedNatLambdaDeclarations
        ++ retainedNatClosureDeclarations

-- | Check retained lambdas against complete signatures without inferring a Pi.
retainedNatLambdaDeclarations :: [String]
retainedNatLambdaDeclarations =
    [ "lambdaId :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
    , "lambdaId = \\n xs -> xs"
    , "lambdaNested :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
    , "lambdaNested @a = \\n -> \\xs -> xs"
    , "lambdaShadow :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m"
    , "lambdaShadow = \\n -> \\n xs ys -> ys"
    , "lambdaGiven :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
    , "lambdaGiven = \\n xs -> onlyLabel xs"
    , "lambdaEquality :: foreach (n :: Nat) -> (Add n Z ~ n) => Vec Int (Add n Z) -> Vec Int n"
    , "lambdaEquality = \\n xs -> xs"
    , "lambdaCase :: foreach (n :: Nat) -> Vec Int n -> String"
    , "lambdaCase = \\case { Z -> \\xs -> show xs; S k -> \\xs -> show xs }"
    , "lambdaCases :: foreach (n :: Nat) -> Vec Int n -> String"
    , "lambdaCases = \\cases { Z xs -> show xs; (S k) xs -> show xs }"
    , "lambdaLinear :: foreach (n :: Nat) -> (Add n Z :~: n) %1 -> Int"
    , "lambdaLinear = \\n p -> case p of Refl -> 42"
    , "lambdaIgnore :: foreach (n :: Nat) -> Vec Int n -> Int"
    , "lambdaIgnore = \\n _ -> 42"
    , "lambdaCapture :: () %1 -> Int"
    , "lambdaCapture p = let worker :: foreach (n :: Nat) -> Vec Int n -> (); worker = \\n xs -> p in case worker Z VNil of () -> 42"
    , "data LambdaVoid"
    , "type family LambdaAbsurd (v :: LambdaVoid)"
    , "lambdaEmpty :: foreach (v :: LambdaVoid) -> LambdaAbsurd v"
    , "lambdaEmpty = \\case {}"
    , "explicitEmptyCase :: foreach (v :: LambdaVoid) -> LambdaAbsurd v"
    , "explicitEmptyCase v = case v of {}"
    , "ordinaryEmptyLambda :: LambdaVoid -> Int"
    , "ordinaryEmptyLambda = \\case {}"
    ]

-- | Preserve lambda arity and observe unused indices through runtime inputs.
retainedNatLambdaObservation :: [String]
retainedNatLambdaObservation =
    [ "identities <- driveNats (\\n -> observeVec (lambdaId @Int n (replicateNative @Int n 42)))"
    , "nested <- driveNats (\\n -> observeVec (lambdaNested @Int n (replicateNative @Int n 42)))"
    , "shadowed <- driveNatPairs (\\n m -> observeVec (lambdaShadow n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
    , "equalities <- driveNats (\\n -> case rightZero n of Refl -> observeVec (lambdaEquality n (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "onePattern <- driveNats (\\n -> pure (lambdaCase n (replicateNative @Int n 42)))"
    , "twoPatterns <- driveNats (\\n -> pure (lambdaCases n (replicateNative @Int n 42)))"
    , "linear <- driveNats (\\n -> observeInt (lambdaLinear n (rightZero n)))"
    , "nestedBottom <- driveNat (\\n -> force (lambdaNested @Int n)) bottomNat"
    , "casesBottom <- driveNat (\\n -> force (lambdaCases n)) bottomNat"
    , "caseCyclic <- driveNat (\\n -> force (lambdaCase n)) cyclicNat"
    , "casePartialField <- driveNat (\\n -> force (lambdaCase n)) partialNat"
    , "ignoredBottom <- driveNat (\\n -> observeInt (lambdaIgnore n bottomVec)) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (lambdaIgnore n bottomVec)) cyclicNat"
    , "firstClass <- force (id (lambdaId @Int))"
    , "captured <- observeInt (lambdaCapture ())"
    , "pure (identities ++ nested ++ shadowed ++ equalities ++ onePattern ++ twoPatterns ++ linear ++ unlines [nestedBottom,casesBottom,caseCyclic,casePartialField,ignoredBottom,ignoredCyclic,firstClass,captured])"
    ]

-- | Append lambda results after the existing source-context observations.
retainedNatLambdaExpected :: [String]
retainedNatLambdaExpected =
    concat (replicate 2 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ ["[]:0", "[7,7,7]:3", "[]:0", "[7,7,7]:3"]
        ++ ["[]:0", "[42,42]:2", "[42,42,42]:3"]
        ++ concat (replicate 2 ["VNil", "VCons 42 (VCons 42 VNil)", "VCons 42 (VCons 42 (VCons 42 VNil))"])
        ++ replicate 3 "42"
        ++ ["forced", "forced", "forced", "forced", "42", "42", "forced", "42"]

-- | Check ordinary closure arguments with explicitly supplied retained types.
retainedNatClosureDeclarations :: [String]
retainedNatClosureDeclarations =
    [ "applyAt :: forall a. foreach (n :: Nat) -> (foreach (m :: Nat) -> Vec a m -> Vec a m) -> Vec a n -> Vec a n"
    , "applyAt @a n f xs = f n xs"
    , "applyFirst :: forall a. (foreach (m :: Nat) -> Vec a m -> Vec a m) -> foreach (n :: Nat) -> Vec a n -> Vec a n"
    , "applyFirst @a f n xs = f n xs"
    , "applyCaptured :: foreach (n :: Nat) -> foreach (m :: Nat) -> (foreach (k :: Nat) -> Vec Int n -> Vec Int k -> Vec Int n) -> Vec Int n -> Vec Int m -> Vec Int n"
    , "applyCaptured n m f xs ys = f m xs ys"
    , "ignoreClosure :: foreach (n :: Nat) -> (foreach (m :: Nat) -> Vec Int m -> Vec Int m) -> Vec Int n -> Int"
    , "ignoreClosure n _ _ = 42"
    , "bottomClosure :: foreach (m :: Nat) -> Vec Int m -> Vec Int m"
    , "bottomClosure = bottomFunction"
    , "erasedRuntimeShadow :: forall (n :: Nat). foreach (n :: Nat) -> Vec Int n -> Vec Int n"
    , "erasedRuntimeShadow @n = \\n xs -> xs"
    , "nativeViaId :: forall a. foreach (m :: Nat) -> Vec a m -> Vec a m"
    , "nativeViaId = id (\\m xs -> xs)"
    , "bumpVec :: Vec Int n -> Vec Int n"
    , "bumpVec VNil = VNil"
    , "bumpVec (VCons x xs) = VCons (x + 1) (bumpVec xs)"
    , "caseClosure :: foreach (n :: Nat) -> (foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int n) -> Vec Int n -> Vec Int n"
    , "caseClosure Z f xs = f Z xs VNil"
    , "caseClosure (S k) f xs = f Z xs VNil"
    , "caseClosureBody :: foreach (n :: Nat) -> (foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int n) -> Vec Int n -> Vec Int n"
    , "caseClosureBody n f xs = case n of { Z -> f Z xs VNil; S k -> f Z xs VNil }"
    , "workerAlias :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
    , "workerAlias = lambdaId"
    , "workerPolyAlias :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
    , "workerPolyAlias = lambdaId"
    , "workerOrdinaryAlias :: (foreach (m :: Nat) -> Vec Int m -> Vec Int m) -> foreach (n :: Nat) -> Vec Int n -> Vec Int n"
    , "workerOrdinaryAlias = applyFirst"
    , "workerAppliedAlias :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
    , "workerAppliedAlias = applyFirst lambdaId"
    , "genericNestedWorker :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a n"
    , "genericNestedWorker n m xs ys = xs"
    , "workerNested :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int n"
    , "workerNested = genericNestedWorker"
    , "genericContextWorker :: forall a. foreach (n :: Nat) -> OnlyGiven n => a -> Vec Int n -> String"
    , "genericContextWorker n _ xs = onlyLabel xs"
    , "workerContextAlias :: foreach (n :: Nat) -> OnlyGiven n => Int -> Vec Int n -> String"
    , "workerContextAlias = genericContextWorker"
    , "workerContextClient :: foreach (n :: Nat) -> OnlyGiven n => Int -> Vec Int n -> String"
    , "workerContextClient n dummy xs = workerContextAlias n dummy xs"
    , "genericBottomWorker :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
    , "genericBottomWorker = bottomFunction"
    , "workerBottomAlias :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
    , "workerBottomAlias = genericBottomWorker"
    ]

-- | Pass named and checked closures through shared runtime index producers.
retainedNatClosureObservation :: [String]
retainedNatClosureObservation =
    [ "named <- driveNats (\\n -> observeVec (applyAt @Int n (erasedRuntimeShadow @Z) (replicateNative @Int n 42)))"
    , "anonymous <- driveNats (\\n -> observeVec (applyAt @Int n (\\m xs -> xs) (replicateNative @Int n 42)))"
    , "ordinaryFirst <- driveNats (\\n -> observeVec (applyFirst @Int (lambdaId @Int) n (replicateNative @Int n 42)))"
    , "captured <- driveNatPairs (\\n m -> observeVec (applyCaptured n m (\\k outer _ -> outer) (replicateNative @Int n 42) (replicateNative @Int m 7)))"
    , "partialBottom <- driveNat (\\n -> force (applyAt @Int n (lambdaId @Int))) bottomNat"
    , "partialCyclic <- driveNat (\\n -> force (applyAt @Int n (lambdaId @Int))) cyclicNat"
    , "ignoredBottom <- driveNat (\\n -> observeInt (ignoreClosure n bottomClosure bottomVec)) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (ignoreClosure n bottomClosure bottomVec)) cyclicNat"
    , "throughId <- driveNats (\\n -> observeVec (nativeViaId @Int n (replicateNative @Int n 42)))"
    , "transformed <- driveNats (\\n -> observeVec (applyAt @Int n (\\m xs -> bumpVec xs) (replicateNative @Int n 42)))"
    , "constructorClosures <- driveNats (\\n -> observeVec (caseClosure n (\\m outer _ -> bumpVec outer) (replicateNative @Int n 42)))"
    , "explicitCaseClosures <- driveNats (\\n -> observeVec (caseClosureBody n (\\m outer _ -> bumpVec outer) (replicateNative @Int n 42)))"
    , "implicitWorker <- driveNats (\\n -> observeVec (applyAt @Int n lambdaId (replicateNative @Int n 42)))"
    , "aliasedWorker <- driveNats (\\n -> observeVec (workerAlias n (replicateNative @Int n 42)))"
    , "polymorphicWorker <- driveNats (\\n -> observeVec (workerPolyAlias @Int n (replicateNative @Int n 42)))"
    , "ordinaryWorker <- driveNats (\\n -> observeVec (workerOrdinaryAlias lambdaId n (replicateNative @Int n 42)))"
    , "appliedWorker <- driveNats (\\n -> observeVec (workerAppliedAlias n (replicateNative @Int n 42)))"
    , "nestedWorker <- driveNatPairs (\\n m -> observeVec (workerNested n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
    , "implicitPartial <- driveNat (\\n -> force (applyAt @Int n lambdaId)) bottomNat"
    , "implicitBottom <- driveNat (\\n -> observeInt (ignoreClosure n genericBottomWorker bottomVec)) bottomNat"
    , "implicitCyclic <- driveNat (\\n -> observeInt (ignoreClosure n genericBottomWorker bottomVec)) cyclicNat"
    , "pure (named ++ anonymous ++ ordinaryFirst ++ captured ++ unlines [partialBottom,partialCyclic,ignoredBottom,ignoredCyclic] ++ throughId ++ transformed ++ constructorClosures ++ explicitCaseClosures ++ implicitWorker ++ aliasedWorker ++ polymorphicWorker ++ ordinaryWorker ++ appliedWorker ++ nestedWorker ++ unlines [implicitPartial,implicitBottom,implicitCyclic])"
    ]

-- | Preserve previous observations before checking closure applications.
retainedNatClosureExpected :: [String]
retainedNatClosureExpected =
    concat (replicate 3 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ ["[]:0", "[]:0", "[42,42]:2", "[42,42]:2"]
        ++ ["forced", "forced", "42", "42"]
        ++ ["[]:0", "[42,42]:2", "[42,42,42]:3"]
        ++ ["[]:0", "[43,43]:2", "[43,43,43]:3"]
        ++ concat (replicate 2 ["[]:0", "[43,43]:2", "[43,43,43]:3"])
        ++ concat (replicate 5 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ ["[]:0", "[]:0", "[42,42]:2", "[42,42]:2"]
        ++ ["forced", "42", "42"]

-- | Check contextual functions with imported classes and local native workers.
retainedNatContextSource :: String -> String -> String
retainedNatContextSource moduleName observation =
    unlines $ "{-# LANGUAGE Arrows, ConstraintKinds, EmptyCase, EmptyDataDecls, FlexibleContexts, LambdaCase, LinearTypes, ViewPatterns #-}" : concatMap insert (lines (retainedNatClassSource moduleName observation))
  where
    insert line@"entry :: IO String" = retainedNatContextDeclarations ++ [line]
    insert line = [line]

-- | Observe dictionary forwarding, pattern scope, and lazy partial applications.
retainedNatContextObservation :: [String]
retainedNatContextObservation =
    [ "rendered <- driveNats (\\n -> pure (renderContext n (replicateNative @Int n 42)))"
    , "constructors <- driveNats (\\n -> pure (renderCtor n (replicateNative @Int n 42)))"
    , "guarded <- driveNats (\\n -> pure (renderGuarded n (replicateNative @Int n 42)))"
    , "viewed <- driveNats (\\n -> pure (renderView n (replicateNative @Int n 42)))"
    , "lambdas <- driveNats (\\n -> pure (renderLambda n (replicateNative @Int n 42)))"
    , "fullPatterns <- driveNats (\\n -> pure (renderFull n (replicateNative @Int (S n) 42)))"
    , "forwarded <- driveNats (\\n -> pure (provideContext n (replicateNative @Int n 42)))"
    , "ignoredVectors <- driveNats (\\n -> pure (provideContext n bottomVec))"
    , "equalities <- driveNats (\\n -> case rightZero n of Refl -> observeVec (equalityContext n (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "constructorEqualities <- driveNats (\\n -> case rightZero n of Refl -> observeVec (equalityCtor n (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "ignoredBottom <- driveNat (\\n -> observeInt (ignoreContext n bottomVec)) bottomNat"
    , "ignoredCyclic <- driveNat (\\n -> observeInt (ignoreContext n bottomVec)) cyclicNat"
    , "ignoredEqualityBottom <- driveNat (\\n -> observeInt (ignoreEqualityContext n bottomVec)) bottomNat"
    , "ignoredEqualityCyclic <- driveNat (\\n -> observeInt (ignoreEqualityContext n bottomVec)) cyclicNat"
    , "variablePartialBottom <- driveNat (\\n -> force (renderContext n)) bottomNat"
    , "variablePartialCyclic <- driveNat (\\n -> force (renderContext n)) cyclicNat"
    , "constructorPartialBottom <- driveNat (\\n -> force (renderCtor n)) bottomNat"
    , "constructorPartialCyclic <- driveNat (\\n -> force (renderCtor n)) cyclicNat"
    , "pairs <- driveNatPairs (\\n m -> pure (renderContexts n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
    , "legacy <- drive (\\b -> observeInt (legacyContext (error \"unused-context-prefix\") b)) False"
    , "legacyBang <- drive (\\b -> observeInt (legacyBangContext b 43)) True"
    , "wildcard <- driveNat (\\n -> pure (wildcardContext (error \"unused-context-prefix\") n (replicateNative @Int n 42))) Z"
    , "constrained <- drive (\\b -> observeInt (keepConstrained b 44)) False"
    , "caseRendered <- driveNats (\\n -> pure (renderCaseContext n (replicateNative @Int n 42)))"
    , "caseEqualities <- driveNats (\\n -> case rightZero n of Refl -> observeVec (caseEqualityContext n (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "casePrimitiveEqualities <- driveNats (\\n -> observeVec (casePrimitiveEquality n (rightZero n) (appendNative @Int n Z (replicateNative @Int n 42) VNil)))"
    , "casePairs <- driveNatPairs (\\n m -> pure (renderCasePair n m (replicateNative @Int n 42) (replicateNative @Int m 7)))"
    , "caseFull <- driveNats (\\n -> pure (casePartial n (replicateNative @Int n 42) 0))"
    , "caseCyclic <- driveNat (\\n -> force (casePartial n bottomVec)) cyclicNat"
    , "casePartialField <- driveNat (\\n -> force (casePartial n bottomVec)) partialNat"
    , "noCaseBottom <- driveNat (\\n -> force (noCasePartial n bottomVec)) bottomNat"
    , "lambdaValues <- do"
    ]
        ++ map ("  " ++) retainedNatLambdaObservation
        ++ ["closureValues <- do"]
        ++ map ("  " ++) retainedNatClosureObservation
        ++ ["pure (rendered ++ constructors ++ guarded ++ viewed ++ lambdas ++ fullPatterns ++ forwarded ++ ignoredVectors ++ equalities ++ constructorEqualities ++ unlines [ignoredBottom,ignoredCyclic,ignoredEqualityBottom,ignoredEqualityCyclic,variablePartialBottom,variablePartialCyclic,constructorPartialBottom,constructorPartialCyclic] ++ pairs ++ unlines [legacy,legacyBang,wildcard,constrained] ++ caseRendered ++ caseEqualities ++ casePrimitiveEqualities ++ casePairs ++ caseFull ++ unlines [caseCyclic,casePartialField,noCaseBottom] ++ lambdaValues ++ closureValues)"]

-- | Preserve the contextual observations after all existing imported results.
retainedNatContextExpected :: [String]
retainedNatContextExpected =
    concat (replicate 5 ["VNil", "VCons 42 (VCons 42 VNil)", "VCons 42 (VCons 42 (VCons 42 VNil))"])
        ++ ["VCons 42 VNil", "VCons 42 (VCons 42 (VCons 42 VNil))", "VCons 42 (VCons 42 (VCons 42 (VCons 42 VNil)))"]
        ++ concat (replicate 2 ["zero", "successor", "successor"])
        ++ concat (replicate 2 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ replicate 4 "42"
        ++ replicate 4 "forced"
        ++ [ "VNil/VNil"
           , "VNil/VCons 7 (VCons 7 (VCons 7 VNil))"
           , "VCons 42 (VCons 42 VNil)/VNil"
           , "VCons 42 (VCons 42 VNil)/VCons 7 (VCons 7 (VCons 7 VNil))"
           ]
        ++ ["42", "43", "VNil", "44"]
        ++ ["VNil", "VCons 42 (VCons 42 VNil)", "VCons 42 (VCons 42 (VCons 42 VNil))"]
        ++ concat (replicate 2 ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        ++ [ "VNil/VNil"
           , "VNil/VCons 7 (VCons 7 (VCons 7 VNil))"
           , "VCons 42 (VCons 42 VNil)/VNil"
           , "VCons 42 (VCons 42 VNil)/VCons 7 (VCons 7 (VCons 7 VNil))"
           ]
        ++ ["VNil", "VCons 42 (VCons 42 VNil)", "VCons 42 (VCons 42 (VCons 42 VNil))"]
        ++ replicate 3 "forced"
        ++ retainedNatLambdaExpected
        ++ retainedNatClosureExpected

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
            ++ [ "{-# LANGUAGE FlexibleInstances #-}"
               , "module Main (main) where"
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
            ++ [ "{-# LANGUAGE FlexibleInstances #-}"
               , "module Main (main) where"
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
            ++ [ "{-# LANGUAGE Arrows, ConstraintKinds, EmptyCase, EmptyDataDecls, FlexibleContexts, FlexibleInstances, LambdaCase, LinearTypes, RankNTypes, TypeOperators, ViewPatterns #-}"
               , "module NativeRetainedNatLibrary where"
               , "import Data.Type.Equality ((:~:)(Refl))"
               ]
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
            ++ retainedNatProofDeclarations
            ++ retainedNatGenericDeclarations
            ++ retainedNatClassDeclarations
            ++ retainedNatContextDeclarations
  where
    generalizeAppend "appendVec :: Vec Int n -> Vec Int m -> Vec Int (Add n m)" =
        "appendVec :: forall a n m. Vec a n -> Vec a m -> Vec a (Add n m)"
    generalizeAppend line = line

-- | Call imported Nat workers without defining retained functions or datatypes.
retainedNatConsumerSource :: String
retainedNatConsumerSource =
    unlines $
        take 1 (lines retainedNatLibrarySource)
            ++ [ "{-# LANGUAGE TypeOperators #-}"
               , "module Main (main) where"
               , "import NativeRetainedNatLibrary"
               , "import Data.Type.Equality ((:~:)(Refl))"
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
               , "  proofValues <- do"
               ]
            ++ map ("    " ++) retainedNatProofObservation
            ++ ["  genericValues <- do"]
            ++ map ("    " ++) retainedNatGenericObservation
            ++ ["  classValues <- do"]
            ++ map ("    " ++) retainedNatClassObservation
            ++ ["  contextValues <- do"]
            ++ map ("    " ++) retainedNatContextObservation
            ++ ["  putStr (values ++ appended ++ unlines [headOnly,boolHead,partialAppend] ++ constructed ++ constructedAppend ++ unlines [constructorHead,nestedPrefix,retainedHead,ordinaryHead,constructedPartial,nestedPartial] ++ caseValues ++ normalized ++ normalizedAppend ++ constants ++ unlines [constantBottom,normalizedPartial] ++ proofValues ++ genericValues ++ classValues ++ contextValues)"]

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
            , -- Keep the first 131 observations. Append contextual observations.
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
                ++ retainedNatProofExpected
                ++ retainedNatGenericExpected
                ++ retainedNatClassExpected
                ++ retainedNatContextExpected
            )
        ]
        $ \(label, moduleName, producer, consumer, expected) ->
            runModuleCase libdir label moduleName producer consumer expected
    packedProducer <- readFile "PackedVecLibrary.hs"
    packedConsumer <- readFile "Main.hs"
    runModuleCase
        libdir
        "packed-vector"
        "PackedVecLibrary"
        packedProducer
        packedConsumer
        [ "runtime package: (0,[])"
        , "runtime package: (2,[42,42])"
        , "runtime package: (3,[42,42,42])"
        , "constructor with bottom fields: ok"
        , "repack with bottom fields: ok"
        , "repack with cyclic fields: ok"
        , "ignore bottom fields: 42"
        , "ignore cyclic fields: 42"
        , "partial length payload head: 42"
        , "cyclic length payload head: 42"
        , "demand payload before length: packed-bottom-payload"
        , "demand stored length: packed-bottom-length"
        , "ignore bottom package: packed-bottom-package"
        , "repack bottom package: packed-bottom-package"
        , "unused whole package: 42"
        , "runtime filter: (3,[1,2,3])"
        , "runtime filter: (2,[2,3])"
        , "runtime filter: (0,[])"
        , "filter empty ignores predicate: (0,[])"
        , "filter rejects bottom element: (0,[])"
        , "filter retains bottom element: ok"
        , "filter retained length: 1"
        , "filter demands retained element: packed-filter-bottom-element"
        , "filter payload before length: packed-bottom-payload"
        , "filter predicate before tail: packed-filter-bottom-predicate"
        , "filter scans partial tail: packed-bottom-length"
        , "filter demands package: packed-bottom-package"
        , "unused filtered package: 42"
        , "left section with bottom operator: ok"
        , "right section with bottom operator: ok"
        , "left section with bottom operand: ok"
        , "right section with bottom operand: ok"
        , "applied left section demands operator: packed-section-bottom-operator"
        , "applied right section demands operator: packed-section-bottom-operator"
        , "applied left section demands operand: packed-filter-bottom-element"
        , "applied right section demands operand: packed-filter-bottom-element"
        , "runtime bounded filter: (3,[1,2,3],3)"
        , "runtime bounded filter: (2,[2,3],2)"
        , "runtime bounded filter: (0,[],0)"
        , "bounded empty ignores predicate: (0,[],0)"
        , "bounded rejects bottom element: (0,[],0)"
        , "bounded retains bottom element: ok"
        , "bounded retained proof depth: 1"
        , "bounded retained length: 1"
        , "bounded demands retained element: packed-filter-bottom-element"
        , "bounded constructor with bottom proof: ok"
        , "bounded length ignores proof: 1"
        , "bounded vector ignores proof: (1,[42])"
        , "bounded demands proof: bounded-bottom-proof"
        , "bounded partial proof constructor: ok"
        , "bounded demands recursive proof: bounded-bottom-proof"
        , "bounded constructor with weakened proof: ok"
        , "bounded vector ignores weakened proof: 42"
        , "bounded demands weakened proof: bounded-bottom-proof"
        , "bounded vector ignores cyclic proof: 42"
        , "bounded predicate before tail: packed-filter-bottom-predicate"
        , "bounded scans partial tail: packed-bottom-length"
        , "unused bounded filter: 42"
        ]
    putStrLn "packed vector module checks: ok"
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
        Just (entry, expression) -> do
            let dependencies = filter ((/= entry) . fst) bindings
            unless (all (isLocalId . fst) dependencies) $
                fail "FAIL: source entry dependency is not a local Id"
            -- Install local binders before substitution updates their occurrences.
            -- Imported GlobalIds remain external references.
            let closed = case dependencies of
                    [] -> expression
                    _ -> Let (Rec [(GHC.Types.Id.setIdNotExported (localiseId binder), rhs) | (binder, rhs) <- dependencies]) expression
                subst = CoreSubst.mkEmptySubst (mkInScopeSet (exprFreeVars closed))
            pure (CoreSubst.substExpr subst closed)
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
    runNatProofSourceChecks
    runNatGenericSourceChecks
    runNatClassSourceChecks
    runNatContextSourceChecks
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

-- | Force lifted arithmetic proofs while preserving unused proof and index thunks.
runNatProofSourceChecks :: GHC.Ghc ()
runNatProofSourceChecks = do
    let moduleName = "NativeSourceNatProofs"
        observation = unlines ("do" : map ("        " ++) retainedNatProofObservation)
    expression <- sourceEntry moduleName (retainedNatProofSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source lifted Nat proofs" expression
    unless (lines actual == retainedNatProofExpected) $
        fail ("FAIL: source lifted Nat proofs changed transport or laziness: " ++ show actual)
    forM_
        [ ("NativeSourceProofBottomIndex", "driveNat (\\n -> case rightZero n of Refl -> pure \"forced\") bottomNat", "native-bottom-nat")
        , ("NativeSourceProofPartialIndex", "driveNat (\\n -> case rightZero n of Refl -> pure \"forced\") partialNat", "native-bottom-nat")
        , ("NativeSourceProofBottomBeforePayload", "driveNat (\\n -> observeVec (transportWith n bottomProof bottomVec)) bottomNat", "native-bottom-proof")
        , ("NativeSourceProofIndexBeforePayload", "driveNat (\\n -> observeVec (rightUnit n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceProofPayloadBottom", "driveNat (\\n -> observeVec (rightUnit n bottomVec)) Z", "native-bottom-vector")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatProofSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)
    forM_
        [
            ( "NativeSourceUnprovedRightZero"
            , "Add"
            ,
                [ "badProof :: foreach (n :: Nat) -> Add n Z :~: n"
                , "badProof n = Refl"
                ]
            )
        ,
            ( "NativeSourceFalseSuccessorProof"
            , "S n"
            ,
                [ "badProof :: foreach (n :: Nat) -> n :~: S n"
                , "badProof n = Refl"
                ]
            )
        ,
            ( "NativeSourceUnusedEqualityProof"
            , "Add"
            ,
                [ "badTransport :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
                , "badTransport n p xs = xs"
                ]
            )
        ,
            ( "NativeSourceEscapedEqualityProof"
            , "Add"
            ,
                [ "badTransport :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
                , "badTransport n p xs = let checked :: (); checked = case p of Refl -> () in xs"
                ]
            )
        ]
        $ \(name, fragment, declarations) ->
            sourceModuleRejects name 83865 fragment name $
                retainedNatProofSource name "pure \"unused\"" ++ unlines declarations

-- | Instantiate ordinary consumers without duplicating linear argument usage.
runNatGenericSourceChecks :: GHC.Ghc ()
runNatGenericSourceChecks = do
    let moduleName = "NativeSourceGenericNatProofs"
        observation = unlines ("do" : map ("        " ++) retainedNatGenericObservation)
    expression <- sourceEntry moduleName (retainedNatGenericSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source generic Nat proof consumers" expression
    unless (lines actual == retainedNatGenericExpected) $
        fail ("FAIL: generic Nat proof consumers changed types, usage, or laziness: " ++ show actual)
    forM_
        [ ("NativeSourceGenericProofBottom", "driveNat (\\n -> force (rightZero n)) bottomNat", "native-bottom-nat")
        , ("NativeSourceGenericProofPartial", "driveNat (\\n -> force (rightZero n)) partialNat", "native-bottom-nat")
        , ("NativeSourceGenericIdProofBottom", "driveNat (\\n -> force (id (rightZero n))) bottomNat", "native-bottom-nat")
        , ("NativeSourceGenericSuppliedProofBottom", "driveNat (\\n -> forceSupplied n bottomProof) bottomNat", "native-bottom-proof")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatGenericSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)
    let duplicateName = "NativeSourceDuplicatedLinearProof"
        duplicateSource =
            retainedNatGenericSource duplicateName "pure \"unused\""
                ++ unlines
                    [ "linearId :: a %1 -> a"
                    , "linearId x = x"
                    , "badLinearProof :: foreach (n :: Nat) -> (Add n Z :~: n) %1 -> Int"
                    , "badLinearProof n p = case linearId p of Refl -> case linearId p of Refl -> 42"
                    ]
    sourceModuleRejects "duplicated linear proof argument" 18872 "multiplicity" duplicateName duplicateSource

-- | Check scoped class evidence, constrained results, and proof evaluation.
runNatClassSourceChecks :: GHC.Ghc ()
runNatClassSourceChecks = do
    let moduleName = "NativeSourceNatClasses"
        observation = unlines ("do" : map ("        " ++) retainedNatClassObservation)
    expression <- sourceEntry moduleName (retainedNatClassSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source Nat class consumers" expression
    unless (lines actual == retainedNatClassExpected) $
        fail ("FAIL: Nat class consumers changed evidence scope or laziness: " ++ show actual)
    forM_
        [ ("NativeSourceShowProofBottom", "driveNat (\\n -> pure (show (rightZero n))) bottomNat", "native-bottom-nat")
        , ("NativeSourceShowProofPartial", "driveNat (\\n -> pure (show (rightZero n))) partialNat", "native-bottom-nat")
        , ("NativeSourceShowSuppliedBottom", "driveNat (\\n -> pure (showSupplied n bottomProof)) bottomNat", "native-bottom-proof")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatClassSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)
    forM_
        [
            ( "NativeSourceMissingNativeInstance"
            , 39999
            , "Missing"
            ,
                [ "class Missing a"
                , "needMissing :: Missing a => a -> Int"
                , "needMissing _ = 42"
                , "badMissing :: foreach (n :: Nat) -> (Add n Z :~: n) -> Int"
                , "badMissing n p = needMissing p"
                ]
            )
        ,
            ( "NativeSourceMissingLocalStringInstance"
            , 39999
            , "Missing"
            ,
                [ "class Missing a"
                , "needMissingString :: Missing a => a -> String"
                , "needMissingString _ = \"unused\""
                , "badMissingString :: foreach (n :: Nat) -> (Add n Z :~: n) -> String"
                , "badMissingString n p = let rendered = needMissingString p in rendered"
                ]
            )
        ,
            ( "NativeSourceEqualityWithoutProof"
            , 18872
            , "Add"
            ,
                [ "badEquality :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n"
                , "badEquality n xs = sameValue xs"
                ]
            )
        ,
            ( "NativeSourceClassDoesNotRefine"
            , 83865
            , "Add"
            ,
                [ "badTransport :: foreach (n :: Nat) -> (Add n Z :~: n) -> Vec Int (Add n Z) -> Vec Int n"
                , "badTransport n p xs = let rendered :: String; rendered = show p in xs"
                ]
            )
        ]
        $ \(name, code, fragment, declarations) ->
            sourceModuleRejects name code fragment name $
                retainedNatClassSource name "pure \"unused\"" ++ unlines declarations

-- | Check scoped source contexts without forcing unused indices or arguments.
runNatContextSourceChecks :: GHC.Ghc ()
runNatContextSourceChecks = do
    let moduleName = "NativeSourceNatContexts"
        observation = unlines ("do" : map ("        " ++) retainedNatContextObservation)
    expression <- sourceEntry moduleName (retainedNatContextSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source retained contexts" expression
    unless (lines actual == retainedNatContextExpected) $
        fail ("FAIL: retained contexts changed dictionary scope, patterns, or laziness: " ++ show actual)
    sourceRejects
        "strict retained context prefix"
        83605
        "strict prefix"
        ["Strict"]
        [ "badStrict :: foreach (b :: Bool) -> (b ~ b) => Int -> Int"
        , "badStrict b x = x"
        ]
    sourceRejects
        "explicit strict retained context prefix"
        83605
        "strict prefix"
        ["BangPatterns"]
        [ "badBang :: foreach (unused :: Bool) -> foreach (b :: Bool) -> (b ~ b) => Int -> Int"
        , "badBang !x b y = y"
        ]
    sourceModuleRejects "irreducible retained context" 83605 "Only class and equality constraints" "NativeSourceIrreducibleContext" $
        unlines
            [ "{-# LANGUAGE ConstraintKinds, DataKinds, ExperimentalDependentTypes, FlexibleContexts, TypeFamilies #-}"
            , "module NativeSourceIrreducibleContext where"
            , "import Data.Kind (Constraint)"
            , "import NativeRuntimeHelpers"
            , "type family NativePred (n :: Nat) :: Constraint"
            , "badIrreducible :: foreach (n :: Nat) -> NativePred n => Vec Int n -> String"
            , "badIrreducible n xs = \"unused\""
            ]
    sourceModuleRejects "irreducible constraint-tuple component" 83605 "Only class and equality constraints" "NativeSourceIrreducibleTuple" $
        unlines
            [ "{-# LANGUAGE ConstraintKinds, DataKinds, ExperimentalDependentTypes, FlexibleContexts, TypeFamilies, TypeOperators #-}"
            , "module NativeSourceIrreducibleTuple where"
            , "import Data.Kind (Constraint)"
            , "import NativeRuntimeHelpers"
            , "type family Pending (n :: Nat) :: Constraint"
            , "type PairC (n :: Nat) = (Show (Vec Int n), (Pending n, n ~ n))"
            , "badTuple :: foreach (n :: Nat) -> PairC n => Vec Int n -> String"
            , "badTuple n xs = \"unused\""
            ]
    forM_
        [ ("NativeSourceContextConstructorBottom", "driveNat (\\n -> pure (renderCtor n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceContextPayloadBottom", "driveNat (\\n -> pure (renderCtor n bottomVec)) Z", "native-bottom-vector")
        , ("NativeSourceContextCasePartialBottom", "driveNat (\\n -> force (casePartial n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceLambdaCaseBottom", "driveNat (\\n -> force (lambdaCase n)) bottomNat", "native-bottom-nat")
        , ("NativeSourceLambdaEmptyBottom", "let v :: LambdaVoid; v = error \"native-bottom-empty\" in force (lambdaEmpty v)", "native-bottom-empty")
        , ("NativeSourceExplicitEmptyCaseBottom", "let v :: LambdaVoid; v = error \"native-bottom-empty\" in force (explicitEmptyCase v)", "native-bottom-empty")
        , ("NativeSourceClosureBottom", "force bottomClosure", "native-bottom-function")
        , ("NativeSourceWorkerBottom", "force workerBottomAlias", "native-bottom-function")
        ]
        $ \(name, body, marker) -> do
            bottomExpression <- sourceEntry name (retainedNatContextSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | marker `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right value -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show value)
    forM_
        [
            ( "NativeSourceMissingOnlyGiven"
            , 39999
            , "OnlyGiven"
            , []
            ,
                [ "badOnlyGiven :: foreach (n :: Nat) -> Vec Int n -> String"
                , "badOnlyGiven n xs = onlyGivenContext n xs"
                ]
            )
        ,
            ( "NativeSourceMissingPredecessorGiven"
            , 39999
            , "OnlyGiven"
            , []
            ,
                [ "badPredecessorGiven :: foreach (n :: Nat) -> OnlyGiven n => Vec Int n -> String"
                , "badPredecessorGiven n xs = case n of { Z -> onlyLabel xs; S k -> onlyGivenContext k bottomVec }"
                ]
            )
        ,
            ( "NativeSourceArrowPatternGivenEscape"
            , 39999
            , "OnlyGiven"
            , []
            ,
                [ "data HasGiven (n :: Nat) where HasGiven :: OnlyGiven n => HasGiven n"
                , "badArrowGiven :: foreach (n :: Nat) -> HasGiven n -> String"
                , "badArrowGiven n = proc x -> case () of { () | HasGiven <- x -> (case n of { Z -> \\() -> onlyGivenContext n bottomVec; S k -> \\() -> onlyGivenContext n bottomVec }) -< () }"
                ]
            )
        ,
            ( "NativeSourceForgottenEqualityContext"
            , 18872
            , "Add"
            , []
            ,
                [ "badEqualityContext :: foreach (n :: Nat) -> Vec Int (Add n Z) -> Vec Int n"
                , "badEqualityContext n xs = equalityContext n xs"
                ]
            )
        ,
            ( "NativeSourceQuantifiedRetainedContext"
            , 83605
            , "forall"
            , ["{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}"]
            ,
                [ "badQuantified :: foreach (n :: Nat) -> (forall a. Show a => Show (Vec a n)) => Vec Int n -> String"
                , "badQuantified n xs = show xs"
                ]
            )
        ,
            ( "NativeSourceLambdaMissingConstructor"
            , 83605
            , "one unguarded clause for each constructor"
            , []
            ,
                [ "badLambda :: foreach (n :: Nat) -> Vec Int n -> String"
                , "badLambda = \\case { Z -> \\xs -> show xs }"
                ]
            )
        ,
            ( "NativeSourceLambdaLaterPattern"
            , 83605
            , "bare variable patterns"
            , []
            ,
                [ "badLambda :: foreach (n :: Nat) -> Vec Int n -> String"
                , "badLambda = \\cases { Z VNil -> \"zero\"; (S k) (VCons x xs) -> \"successor\" }"
                ]
            )
        ,
            ( "NativeSourceLambdaWrongIndex"
            , 83865
            , "Vec"
            , []
            ,
                [ "badLambda :: foreach (n :: Nat) -> Vec Int n"
                , "badLambda = \\n -> VNil"
                ]
            )
        ,
            ( "NativeSourceLambdaDuplicatedLinearProof"
            , 18872
            , "multiplicity"
            , []
            ,
                [ "badLambda :: foreach (n :: Nat) -> (Add n Z :~: n) %1 -> Int"
                , "badLambda = \\n p -> case p of Refl -> case p of Refl -> 42"
                ]
            )
        ,
            ( "NativeSourceClosureWrongLength"
            , 83865
            , "Vec"
            , []
            ,
                [ "badClosure :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
                , "badClosure n xs = applyAt @Int n (\\m ys -> VCons 42 ys) xs"
                ]
            )
        ,
            ( "NativeSourceClosureIndexEscape"
            , 83865
            , "Vec"
            , []
            ,
                [ "badClosure :: foreach (n :: Nat) -> Vec Int n -> Vec Int n"
                , "badClosure n xs = applyAt @Int n (\\m ys -> xs) xs"
                ]
            )
        ,
            ( "NativeSourceFunctionIndexDomain"
            , 83605
            , "supported closed first-order algebraic domain"
            , []
            ,
                [ "badFunctionIndex :: foreach (f :: Nat -> Nat) -> Int"
                , "badFunctionIndex f = 42"
                ]
            )
        ,
            ( "NativeSourceClosureInference"
            , 83605
            , "explicit signature"
            , []
            , ["badClosureAlias = applyAt @Int"]
            )
        ,
            ( "NativeSourceClosureAnonymousWildcard"
            , 83605
            , "complete type signature"
            , ["{-# LANGUAGE PartialTypeSignatures #-}"]
            ,
                [ "badClosureWildcard :: _ -> (foreach (m :: Nat) -> Vec Int m -> Vec Int m) -> Int"
                , "badClosureWildcard _ _ = 42"
                ]
            )
        ,
            ( "NativeSourceClosureNamedWildcard"
            , 83605
            , "complete type signature"
            , ["{-# LANGUAGE NamedWildCards, PartialTypeSignatures #-}"]
            ,
                [ "badClosureWildcard :: _payload -> (foreach (m :: Nat) -> Vec Int m -> Vec Int m) -> Int"
                , "badClosureWildcard _ _ = 42"
                ]
            )
        ,
            ( "NativeSourceWorkerPayloadMismatch"
            , 83865
            , "Vec"
            , []
            ,
                [ "badWorker :: foreach (n :: Nat) -> Vec Int n -> Vec Bool n"
                , "badWorker = lambdaId"
                ]
            )
        ,
            ( "NativeSourceWorkerIndexCapture"
            , 83865
            , "Vec"
            , []
            ,
                [ "badWorker :: foreach (n :: Nat) -> Vec (Vec Int n) n -> Vec (Vec Int n) n"
                , "badWorker = lambdaId"
                ]
            )
        ,
            ( "NativeSourceWorkerDomainMismatch"
            , 83865
            , "Bool"
            , []
            ,
                [ "badWorker :: foreach (b :: Bool) -> b :~: b"
                , "badWorker = lambdaId"
                ]
            )
        ,
            ( "NativeSourceWorkerInference"
            , 83605
            , "explicit signature"
            , []
            , ["badWorkerAlias = lambdaId"]
            )
        ]
        $ \(name, code, fragment, pragmas, declarations) ->
            sourceModuleRejects name code fragment name $
                unlines pragmas ++ retainedNatContextSource name "pure \"unused\"" ++ unlines declarations

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
        checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
        void (GHC.desugarModule checked)
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

-- | Check source package restrictions before or during desugaring.
runPackageSourceRejectChecks :: GHC.Ghc ()
runPackageSourceRejectChecks = do
    forM_
        [ ("StrictData", 83605, "StrictData", ["StrictData"], [], package)
        , ("TypeData", 83605, "retained constructor", ["TypeData"], [], ["type data PackedVec a where", "  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a"])
        , ("Existential", 83605, "existential binders", [], [], declaration "forall a hidden. foreach (n :: Nat) -> Vec a n -> hidden -> PackedVec a")
        , ("Equality", 83605, "contexts", [], [], declaration "forall a. (a ~ Int) => foreach (n :: Nat) -> Vec a n -> PackedVec a")
        , ("RefinedUniversal", 83605, "equality specifications", [], [], declaration "foreach (n :: Nat) -> Vec Int n -> PackedVec Int")
        , ("Promotion", 83605, "Promotion of constructors with retained fields", [], [], package ++ ["type Bad = 'PackVec"])
        , ("ConstructorField", 83605, "bare variable fields", [], [], package ++ ["bad :: PackedVec a -> Int", "bad (PackVec Z xs) = 0"])
        , ("LazyPattern", 83605, "Lazy patterns and pattern bindings", [], [], package ++ ["bad :: PackedVec a -> Int", "bad ~(PackVec n xs) = 0"])
        , ("LetPattern", 83605, "Lazy patterns and pattern bindings", [], [], package ++ ["bad :: PackedVec a -> Int", "bad p = let PackVec n xs = p in 0"])
        , ("IndexMismatch", 83865, "Couldn't match", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> PackedVec Int", "bad n m xs = PackVec @Int m xs"])
        , ("IndexEscape", 83865, "Couldn't match", [], [], package ++ ["bad :: foreach (m :: Nat) -> PackedVec Int -> Vec Int m", "bad m (PackVec n xs) = xs"])
        , ("LinearClause", 18872, "arising from a pattern", ["LinearTypes"], [], package ++ ["bad :: PackedVec a %1 -> PackedVec a", "bad (PackVec n xs) = PackVec n xs"])
        , ("LinearCase", 18872, "arising from multiplicity", ["LinearTypes"], [], package ++ ["bad :: PackedVec a %1 -> PackedVec a", "bad p = case p of PackVec n xs -> PackVec n xs"])
        , ("RigidMultiplicity", 25897, "is a rigid type variable", ["LinearTypes"], ["import GHC.Types (Multiplicity)"], package ++ ["bad :: forall (p :: Multiplicity) a. PackedVec a %p -> PackedVec a", "bad (PackVec n xs) = PackVec n xs"])
        , ("FilterWrongLength", 5617, "PackVec m (VCons x ys)", [], [], package ++ ["filterVec :: forall a n. (a -> Bool) -> Vec a n -> PackedVec a", "filterVec predicate VNil = PackVec Z VNil", "filterVec predicate (VCons x xs) =", "  if predicate x", "    then case filterVec predicate xs of", "      PackVec m ys -> PackVec m (VCons x ys)", "    else filterVec predicate xs"])
        , ("BoundedVectorLength", 5617, "VCons x ys", [], [], bounded "PackBounded len (VCons x ys) (weakenRight proof)" dropBound)
        , ("BoundedProofLength", 5617, "weakenRight proof", [], [], bounded "PackBounded (S len) (VCons x ys) (weakenRight proof)" dropBound)
        , ("BoundedUpperBound", 25897, "PackBounded len ys proof", [], [], bounded keepBound "PackBounded len ys proof")
        , ("SharedEquations", 94726, "one equation with bare variable fields", [], [], package ++ ["bad :: PackedVec a -> Int", "bad (PackVec n xs) = 0", "bad (PackVec m ys) = 1"])
        ]
        $ \(name, code, fragment, extensions, imports, declarations) -> do
            let moduleName = "NativeSourcePackage" ++ name
                source =
                    unlines $
                        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll #-}"
                        , "{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables #-}"
                        , "{-# LANGUAGE TypeApplications, TypeOperators #-}"
                        ]
                            ++ map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") extensions
                            ++ ["module " ++ moduleName ++ " where"]
                            ++ imports
                            ++ [ "data Nat = Z | S Nat"
                               , "data Vec a (n :: Nat) where"
                               , "  VNil :: Vec a Z"
                               , "  VCons :: a -> Vec a n -> Vec a (S n)"
                               ]
                            ++ declarations
            sourceModuleRejects name code fragment moduleName source
    liftIO $ putStrLn "package source rejection checks: ok"
  where
    declaration signature = ["data PackedVec a where", "  PackVec :: " ++ signature]
    package = declaration "foreach (n :: Nat) -> Vec a n -> PackedVec a"
    keepBound = "PackBounded (S len) (VCons x ys) (LES proof)"
    dropBound = "PackBounded len ys (weakenRight proof)"
    bounded retained rejected =
        [ "data LE (n :: Nat) (m :: Nat) where"
        , "  LEZ :: LE Z m"
        , "  LES :: LE n m -> LE (S n) (S m)"
        , "data BoundedVec a (limit :: Nat) where"
        , "  PackBounded :: foreach (len :: Nat) -> Vec a len -> LE len limit -> BoundedVec a limit"
        , "weakenRight :: LE n m -> LE n (S m)"
        , "weakenRight LEZ = LEZ"
        , "weakenRight (LES proof) = LES (weakenRight proof)"
        , "filterBounded :: forall a limit. (a -> Bool) -> Vec a limit -> BoundedVec a limit"
        , "filterBounded predicate VNil = PackBounded Z VNil LEZ"
        , "filterBounded predicate (VCons x xs) ="
        , "  if predicate x"
        , "    then case filterBounded predicate xs of"
        , "      PackBounded len ys proof -> " ++ retained
        , "    else case filterBounded predicate xs of"
        , "      PackBounded len ys proof -> " ++ rejected
        ]

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
            , "one unguarded clause for each constructor"
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
        , ("multiple variable equations", 83605, "one unguarded clause for each constructor", [], local [] selection (identity ++ identity) "42")
        , ("producer expression argument", 83605, "local index variable or a supported constructor tree", [], local [] consumer identity "keep (not False) 42")
        , ("nonreflexive indexed constraint", 18872, "True", [], local [] "foreach (b :: Bool) -> ((b ~ True) => Int) -> Int" identity "42")
        , ("general indexed constraint", 39999, "Show", ["FlexibleContexts"], local [] "foreach (b :: Bool) -> (Show (Select b) => Int) -> Int" identity "42")
        , ("ImpredicativeTypes", 83605, "Quick Look inference", ["ImpredicativeTypes"], local [] consumer ["keep b x = 42"] "keep True 42")
        , ("Quick Look through ($)", 83605, "Quick Look inference", [], local [] consumer ["keep b x = 42"] "(keep $ True) 42")
        , ("INLINE retained function", 83605, "Optimisation pragmas", [], local ["{-# INLINE keep #-}"] selection identity "42")
        , ("INLINABLE retained function", 83605, "Optimisation pragmas", [], local ["{-# INLINABLE keep #-}"] selection identity "42")
        , ("SPECIALISE type pragma", 83605, "Optimisation pragmas", [], local ["{-# SPECIALISE keep :: Bool -> Int -> Int #-}"] selection identity "42")
        , ("SPECIALISE expression pragma", 83605, "Optimisation pragmas", [], local ["{-# SPECIALISE keep True #-}"] selection identity "42")
        , ("unsigned retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["alias = keep"]) "42")
        , ("unsigned partial retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["index :: Bool", "index = False", "alias = keep index"]) "42")
        , ("retained type in a type-constructor argument", 43582, "not supported in this type position", [], ["bad :: Maybe (foreach (b :: Bool) -> Select b -> Select b)", "bad = undefined"])
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
        , ("missing constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True = 42"] "42")
        , ("duplicate constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True = 42", "keep True = 43"] "42")
        , ("guarded constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True | True = 42", "keep False = False"] "42")
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
    runPackageSourceRejectChecks
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
