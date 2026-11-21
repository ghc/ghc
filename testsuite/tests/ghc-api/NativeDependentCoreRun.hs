module Main (main) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Maybe (isNothing)
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
import GHC.Core.SimpleOpt (simpleOptNativeExpr)
import qualified GHC.Core.Subst as CoreSubst
import GHC.Core.TyCo.Compare (eqType)
import GHC.Core.TyCo.Rep (Type (..), Coercion (..), scaledThing)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Core.Utils (dataConRepInstPat, exprIsTrivial, exprType)
import GHC.CoreToStg.Prep (CorePrepConfig (..), corePrepExpr)
import GHC.Data.Bag (bagToList)
import GHC.Data.FastString (fsLit)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Driver.Backend (backendWritesFiles)
import GHC.Driver.Config.Core.Lint (initLintConfig)
import GHC.Driver.Config.CoreToStg.Prep (initCorePrepConfig)
import GHC.Driver.DynFlags (DynFlags (..), ExecutableLinkMode (Dynamic), GhcLink (LinkExecutable, NoLink), GhcMode (OneShot), gopt_set, targetProfile)
import GHC.Driver.Env (HscEnv, hscInterp, hsc_NC, hsc_logger)
import GHC.Driver.Flags (GeneralFlag (Opt_ExternalInterpreter))
import GHC.Driver.Main (hscCompileCoreExpr, hscGenHardCode, hscSimplify, hscTidy)
import GHC.Driver.Phases (Phase (StopLn), StopPhase (NoStop))
import GHC.Driver.Pipeline (compileFile, oneShot)
import GHC.Driver.Ppr (showSDoc)
import GHC.Iface.Binary (CheckHiWay (CheckHiWay), TraceBinIFace (QuietBinIFace), readBinIface)
import GHC.Iface.Syntax (IfaceDecl(IfaceAxiom, ifAxSourceSpans))
import GHC.Runtime.Interpreter (evalString)
import GHC.Types.Error (Diagnostic (diagnosticCode), DiagnosticCode (..), MsgEnvelope (..), getMessages, pprDiagnostic)
import GHC.Types.Id
import GHC.Types.Id.Info (IdDetails (VanillaId))
import GHC.Types.InlinePragma (neverInlinePragma)
import GHC.Types.Name (mkExternalName, mkSystemName, nameOccName)
import GHC.Types.Name.Occurrence (mkTcOcc, mkTyVarOcc, mkVarOcc, occNameString)
import GHC.Types.SourceError (SourceError, srcErrorMessages)
import GHC.Types.SrcLoc
    ( SrcSpan (RealSrcSpan), isGoodSrcSpan, noLoc, noSrcSpan
    , srcSpanStartLine, srcSpanEndLine, srcSpanStartCol, srcSpanEndCol, srcSpanFile
    )
import GHC.Types.Unique (Unique, mkUniqueGrimilyWithTag)
import GHC.Types.Var
import GHC.Types.Var.Env (mkInScopeSet)
import GHC.Types.Var.Set
    ( CoVarSet, TyVarSet, allVarSet, delVarSet, disjointVarSet, elemVarSet
    , emptyVarSet, extendVarSet, filterVarSet, lookupVarSet, sizeVarSet, unionVarSet
    )
import GHC.Unit.Module.ModGuts (CgGuts (cg_binds), ModGuts (mg_binds, mg_tcs))
import GHC.Unit.Module.ModIface (mi_decls, mi_hash_fn, mi_mod_hash)
import GHC.Unit.Types (mainUnit, mkModule, mkModuleName)
import GHC.Utils.Outputable (ppr, vcat)
import GHC.Utils.Panic (GhcException (..))
import System.Directory (createDirectoryIfMissing, doesFileExist, getModificationTime, makeAbsolute, removeFile, setModificationTime)
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
            runDomainConversionObjectCheck libdir
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
        , "import Data.Kind (Type)"
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
        , "data DomainToken (a :: Type) = DomainToken"
        , "{-# NOINLINE bottomDomainToken #-}"
        , "bottomDomainToken :: DomainToken Bool"
        , "bottomDomainToken = error \"native-bottom-domain-token\""
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
        , "-- | Keep this producer opaque to native index conversion."
        , "{-# NOINLINE opaqueNat #-}"
        , "opaqueNat :: Nat -> Nat"
        , "opaqueNat n = n"
        , "-- | Ignore a runtime argument without exposing the body to Core."
        , "{-# NOINLINE constantZeroNat #-}"
        , "constantZeroNat :: Nat -> Nat"
        , "constantZeroNat _ = Z"
        , "-- | Supply an opaque function whose input and output domains differ."
        , "{-# NOINLINE boolToNat #-}"
        , "boolToNat :: Bool -> Nat"
        , "boolToNat True = S Z"
        , "boolToNat False = Z"
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

-- | Add computed-call cases without changing the inherited source declarations.
retainedCallSource :: String -> String -> String
retainedCallSource moduleName observation =
    unlines $ concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "toList :: forall size. Vec Int size -> [Int]"
        , "toList = vecList"
        , "callObserve :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> IO String"
        , "callObserve f n original = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in observeVec ys; S k -> let { ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) } in observeVec ys }"
        , "callHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> IO String"
        , "callHead f n original = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in pure (show (length (toList ys))); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) } in pure (show (head (toList ys))) }"
        , "callPrefix :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> IO String"
        , "callPrefix f n original = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in pure (show (take 4 (toList ys))); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) } in pure (show (take 4 (toList ys))) }"
        , "callCached :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "callCached f n original = let { consume :: Vec Int (f n) -> Int; consume cached = case f n of { Z -> length (toList cached); S predecessor -> length (toList cached) } } in case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in consume ys; S actual -> let { ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int actual 42) } in consume ys }"
        , "callFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int"
        , "callFunction f n original = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in \\answer -> length (toList ys) + answer; S k -> let { ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) } in \\answer -> length (toList ys) + answer }"
        , "ordinaryCallWildcard :: (Nat -> Nat) -> Nat -> Int"
        , "ordinaryCallWildcard f n = case f n of { _ -> 42 }"
        , "ordinaryCallVariable :: (Nat -> Nat) -> Nat -> Int"
        , "ordinaryCallVariable f n = case f n of { ignored -> 42 }"
        ]

-- | Isolate Strict call refinement from unrelated lazy source declarations.
strictCallSource :: String -> String -> String
strictCallSource moduleName observation =
    unlines
        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, ScopedTypeVariables, Strict #-}"
        , "module " ++ moduleName ++ " where"
        , "import NativeRuntimeHelpers"
        , "strictCaptured :: (Nat -> Nat) -> Nat -> Int"
        , "strictCaptured f n = let { consume :: Vec Int (f n) -> Int; consume ~cached = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in length (vecList ys); S k -> length (vecList cached) } } in consume (error \"strict-capture\")"
        , "strictHead :: (Nat -> Nat) -> Nat -> Int"
        , "strictHead f n = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in length (vecList ys); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (error \"strict-tail\") } in head (vecList ys) }"
        , "strictPredecessor :: (Nat -> Nat) -> Nat -> Int"
        , "strictPredecessor f n = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in length (vecList ys); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (error \"strict-tail\") } in k `seq` head (vecList ys) }"
        , "strictIgnored :: (Nat -> Nat) -> Nat -> Int"
        , "strictIgnored f ~n = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in length (vecList ys); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (error \"strict-tail\") } in head (vecList ys) }"
        , "strictBare :: (Nat -> Nat) -> Nat -> Int"
        , "strictBare f n = case f n of { Z -> let { ys :: Vec Int (f n); ys = VNil } in length (vecList ys); S k -> let { ys :: Vec Int (f n); ys = VCons 42 (error \"strict-tail\") } in head (vecList ys) }"
        , "entry :: IO String"
        , "entry = observeInt (" ++ observation ++ ")"
        ]

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
    runInvisibleRetainedModuleChecks libdir
    runMultiPackageSourceChecks libdir
    runFunctionPackageModuleChecks libdir
    runFunctionFieldModuleChecks libdir
    runGenericListModuleChecks libdir
    runOrdinaryRetainedModuleChecks libdir
    runGenericSafeIndexModuleChecks libdir
    runBoundedFamilyModuleChecks libdir
    runExportedFunctionSignatureModuleChecks libdir
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
    runModuleCase
        libdir
        "sum-declaration"
        "NativeSumDeclaration"
        ( unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, TypeFamilies, TypeOperators #-}"
            , "module NativeSumDeclaration where"
            , "import Data.Type.Equality ((:~:))"
            , "data Nat = Z | S Nat"
            , "type family Add (n :: Nat) (m :: Nat) :: Nat where"
            , "  Add Z m = m"
            , "  Add (S n) m = S (Add n m)"
            , "data Sum (n :: Nat) (m :: Nat) where"
            , "  Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m"
            , "data SumUnused (n :: Nat) (m :: Nat) where"
            , "  SumUnused :: foreach (unused :: Nat) (p :: Nat) -> (p :~: Add n m) -> SumUnused n m"
            ]
        )
        ( unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeApplications, TypeOperators #-}"
            , "module Main where"
            , "import NativeSumDeclaration"
            , "import Data.IORef"
            , "import Data.Type.Equality ((:~:)(Refl))"
            , "zero :: foreach (m :: Nat) -> Sum Z m"
            , "zero m = Sum m Refl"
            , "zeroVTA :: foreach (m :: Nat) -> Sum Z m"
            , "zeroVTA m = Sum @Z m Refl"
            , "alias :: forall (n :: Nat) (m :: Nat). foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m"
            , "alias p proof = Sum p proof"
            , "zeroAlias :: foreach (m :: Nat) -> Sum Z m"
            , "zeroAlias m = alias m Refl"
            , "zeroUnused :: foreach (m :: Nat) -> SumUnused Z m"
            , "zeroUnused m = SumUnused Z m Refl"
            , "render :: Nat -> String"
            , "render Z = \"Z\""
            , "render (S n) = \"S \" ++ render n"
            , "observe :: Sum n m -> String"
            , "observe (Sum p Refl) = render p"
            , "observeUnused :: SumUnused n m -> String"
            , "observeUnused (SumUnused unused p Refl) = render p"
            , "main :: IO ()"
            , "main = do"
            , "  cell <- newIORef (S Z)"
            , "  m <- readIORef cell"
            , "  putStrLn (observe (zero m))"
            , "  putStrLn (observe (zeroVTA m))"
            , "  putStrLn (observe (zeroAlias m))"
            , "  putStrLn (observeUnused (zeroUnused m))"
            ]
        )
        ["S Z", "S Z", "S Z", "S Z"]
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
        , "runtime nested patterns: (Just 1,3)"
        , "runtime nested patterns: (Just 2,2)"
        , "runtime nested patterns: (Nothing,0)"
        , "nested empty head: Nothing"
        , "nested partial tail head: Just 42"
        , "nested bottom element constructor: ok"
        , "nested demands element: packed-filter-bottom-element"
        , "nested payload before index: packed-bottom-payload"
        , "nested proof before fields: bounded-bottom-proof"
        , "nested partial proof: bounded-bottom-proof"
        , "wildcard proof repack: ok"
        , "wildcard proof ignores bottom: Just 42"
        , "wildcard proof ignores cyclic: Just 42"
        , "early catchall ignores payload: 42"
        , "reversed catchall demands payload: packed-bottom-payload"
        , "runtime guarded repack: (2,[42,42])"
        , "runtime guarded repack: (2,[42,42])"
        , "guard fallback bottom fields: ok"
        , "guard fallback cyclic fields: ok"
        , "nested head bottom package: packed-bottom-package"
        , "nested proof bottom package: bounded-bottom-package"
        , "wildcard repack bottom package: bounded-bottom-package"
        , "guard fallback bottom package: packed-bottom-package"
        , "runtime uncons: Nothing"
        , "runtime uncons: 42:(1,[42])"
        , "runtime uncons: 42:(2,[42,42])"
        , "runtime drain filtered: [1,2,3]"
        , "runtime drain filtered: [2,3]"
        , "runtime drain filtered: []"
        , "uncons zero ignores payload: Nothing"
        , "drain zero ignores payload: []"
        , "uncons index before payload: packed-bottom-length"
        , "uncons successor demands vector: packed-bottom-payload"
        , "uncons partial result: ok"
        , "uncons partial pair: ok"
        , "uncons partial tail package: ok"
        , "uncons demands head: packed-filter-bottom-element"
        , "uncons demands predecessor: packed-bottom-length"
        , "uncons demands tail payload: packed-bottom-payload"
        , "uncons cyclic predecessor result: ok"
        , "uncons cyclic predecessor tail package: ok"
        , "uncons cyclic predecessor head: 42"
        , "uncons cyclic bounded prefix: [42,42,42]"
        , "uncons bottom package: packed-bottom-package"
        , "runtime equality zip: same Just (0,[])"
        , "runtime equality zip: different Nothing"
        , "runtime equality zip: different Nothing"
        , "runtime equality zip: same Just (2,[(42,7),(42,7)])"
        , "runtime equality zip: different Nothing"
        , "runtime equality zip: different Nothing"
        , "runtime equality zip: same Just (3,[(42,7),(42,7),(42,7)])"
        , "runtime filtered zip: Just (3,[(1,10),(2,20),(3,30)])"
        , "runtime filtered zip: Just (2,[(2,20),(3,30)])"
        , "runtime filtered zip: Nothing"
        , "runtime filtered zip: Just (0,[])"
        , "equality zero successor ignores predecessor: Nothing"
        , "equality successor zero ignores predecessor: Nothing"
        , "equality cyclic successor zero: Nothing"
        , "equality demands left length: packed-bottom-length"
        , "equality demands right length: equality-right-length"
        , "equality same index still demands length: packed-bottom-length"
        , "equality demands left predecessor: packed-bottom-length"
        , "equality demands right predecessor: equality-right-length"
        , "zip mismatch ignores vectors: Nothing"
        , "zip reversed mismatch ignores vectors: Nothing"
        , "zip demands left package first: packed-bottom-package"
        , "zip demands right package: zip-right-package"
        , "zip equal result ignores vectors: ok"
        , "zip equal package ignores vectors: ok"
        , "zip length ignores vectors: 1"
        , "zip vector demands left tag: zip-left-vector"
        , "zip vector demands right tag: zip-right-vector"
        , "zip tuple ignores components: ok"
        , "zip demands left head: packed-filter-bottom-element"
        , "zip demands right head: zip-right-head"
        , "zip tail package ignores tails: ok"
        , "zip demands left tail: zip-left-vector"
        , "zip demands right tail: zip-right-vector"
        , "runtime append: (0,[])"
        , "runtime append: (2,[7,7])"
        , "runtime append: (2,[42,42])"
        , "runtime append: (4,[42,42,7,7])"
        , "runtime append: (5,[42,42,7,7,7])"
        , "runtime append: (5,[42,42,42,7,7])"
        , "runtime append: (6,[42,42,42,7,7,7])"
        , "runtime filtered append: (6,[1,2,3,10,20,30])"
        , "runtime filtered append: (4,[2,3,20,30])"
        , "runtime filtered append: (5,[2,3,10,20,30])"
        , "runtime filtered append: (0,[])"
        , "partial sum keeps bottom left: ok"
        , "partial sum keeps cyclic left: ok"
        , "sum zero ignores right bottom: ok"
        , "sum successors ignore right bottom: ok"
        , "sum ignores cyclic right: ok"
        , "sum demands left length: packed-bottom-length"
        , "sum demands left predecessor: packed-bottom-length"
        , "sum proof ignores right bottom: ok"
        , "sum length head ignores right bottom: S"
        , "sum length demands right bottom: equality-right-length"
        , "sum constructor ignores bottom fields: ok"
        , "sum stored length demand: packed-bottom-length"
        , "sum stored proof demand: append-bottom-proof"
        , "sum length ignores proof: 0"
        , "unused append: 42"
        , "append demands left package first: packed-bottom-package"
        , "append demands right package before lengths: append-right-package"
        , "append demands left length before vectors: packed-bottom-length"
        , "append scans first length: packed-bottom-length"
        , "append zero ignores right length and vector: ok"
        , "append zero demands right vector: zip-right-vector"
        , "append zero length demands right length: equality-right-length"
        , "append package ignores vectors: ok"
        , "append length ignores vectors: 2"
        , "append vector demands left tag: zip-left-vector"
        , "append first head ignores right vector: 42"
        , "append tail demands right vector: zip-right-vector"
        , "append package ignores left element: ok"
        , "append demands left element: packed-filter-bottom-element"
        , "append first head ignores left tail: 42"
        , "append demands left tail: zip-left-vector"
        , "append finite left ignores cyclic right: 42"
        , "append cyclic right bounded prefix: [42,42,42]"
        , "runtime equal tail patterns: (0,[])"
        , "runtime equal tail patterns: (1,[42])"
        , "equal patterns demand proof: append-bottom-proof"
        , "equal patterns ignore predecessors before proof: append-bottom-proof"
        , "equal patterns keep partial tail package: ok"
        , "equal patterns demand tail vector: packed-bottom-length"
        , "equal patterns demand tail length: packed-bottom-length"
        , "equal patterns demand first index: packed-bottom-length"
        , "equal patterns demand second index: equality-right-length"
        , "sum zero pattern ignores proof: 0"
        , "sum successor pattern ignores predecessor and proof: 1"
        , "sum pattern demands stored index: packed-bottom-length"
        , "index zero vector pattern: True"
        , "explicit zero vector pattern demands payload: packed-bottom-payload"
        , "index pattern before payload: packed-bottom-length"
        , "successor pattern ignores predecessor and vector: False"
        , "index pattern before later argument: packed-bottom-length"
        , "zero pattern then later argument: pattern-flag"
        , "vector pattern before later argument: packed-bottom-payload"
        , "zero pattern selects first row: 0"
        , "successor pattern selects element: 1"
        , "later argument mismatch reaches catchall: 42"
        , "index pattern before later view: packed-bottom-length"
        , "zero pattern then later view: pattern-view"
        , "index mismatch skips later view: 1"
        , "index guard ignores cyclic fields: 42"
        , "index guard failure reaches catchall: 99"
        , "index mismatch skips guard argument: 0"
        , "index pattern before guard: packed-bottom-length"
        , "runtime reverse: (0,[])"
        , "runtime reverse: (2,[42,42])"
        , "runtime reverse: (3,[42,42,42])"
        , "runtime filtered reverse: (3,[3,2,1])"
        , "runtime filtered reverse: (2,[3,2])"
        , "runtime filtered reverse: (0,[])"
        , "runtime reverse append: (5,[3,2,1,7,7])"
        , "runtime reverse append: (3,[3,2,7])"
        , "runtime reverse append: (2,[7,7])"
        , "runtime reverse arithmetic proofs: Refl"
        , "reverse constructor with bottom fields: ok"
        , "reverse constructor with cyclic fields: ok"
        , "reverse keeps stored length: 2"
        , "reverse keeps partial stored constructor: 42"
        , "reverse stored length demands index: packed-bottom-length"
        , "reverse index before payload: packed-bottom-length"
        , "reverse partial index before payload: packed-bottom-length"
        , "reverse zero inspects payload: packed-bottom-payload"
        , "reverse successor inspects payload: packed-bottom-payload"
        , "reverse complete spine before head: packed-bottom-payload"
        , "reverse lazy element result: ok"
        , "reverse demands element: packed-filter-bottom-element"
        , "reverse preserves last element: 7"
        , "reverse later element demand: packed-filter-bottom-element"
        , "reverse bottom package: packed-bottom-package"
        , "unused reverse package: 42"
        , "partial reverse ignores bottom index: ok"
        , "partial reverse ignores cyclic index: ok"
        , "partial reverse append ignores indices: ok"
        , "partial reverse append ignores payload: ok"
        , "empty reverse append inspects first vector: zip-left-vector"
        , "empty reverse append ignores right index: zip-right-vector"
        , "successor proof zero ignores right index: Refl"
        , "successor proof demands first index: packed-bottom-length"
        , "successor proof demands remaining index: packed-bottom-length"
        , "right zero proof demands index: packed-bottom-length"
        , "ignored reverse proof stays lazy: 42"
        , "reverse append payload before predecessor: packed-bottom-payload"
        , "reverse append proof before tail: packed-bottom-length"
        , "reverse append ignores right fields: 42"
        , "runtime later indices: (2,[7,7])|(3,[9,9,9])|42"
        , "runtime later indices: (3,[7,7,7])|(0,[])|42"
        , "runtime later evidence: ([],[9,9])"
        , "runtime later evidence: ([7,7],[9,9,9])"
        , "later partial ignores first index: ok"
        , "later partial ignores cyclic first index: ok"
        , "later partial ignores all supplied fields: ok"
        , "later tuple ignores later fields: ok"
        , "later left package ignores partial index: ok"
        , "later right package ignores cyclic first index: ok"
        , "later first index before payloads: packed-bottom-length"
        , "later left stored index demand: packed-bottom-length"
        , "later right stored index demand: equality-right-length"
        , "later left payload demand: zip-left-vector"
        , "later right payload demand: zip-right-vector"
        , "later ordinary shadowing: 42"
        , "later evidence partial ignores indices: ok"
        , "later evidence first index before payloads: packed-bottom-length"
        , "runtime leading context: ([],[7])"
        , "runtime leading context: ([42,42],[7])"
        , "runtime equality matrix (0,0): same"
        , "runtime equality matrix (0,1): different"
        , "runtime equality matrix (0,2): different"
        , "runtime equality matrix (0,3): different"
        , "runtime equality matrix (1,0): different"
        , "runtime equality matrix (1,1): same"
        , "runtime equality matrix (1,2): different"
        , "runtime equality matrix (1,3): different"
        , "runtime equality matrix (2,0): different"
        , "runtime equality matrix (2,1): different"
        , "runtime equality matrix (2,2): same"
        , "runtime equality matrix (2,3): different"
        , "runtime equality matrix (3,0): different"
        , "runtime equality matrix (3,1): different"
        , "runtime equality matrix (3,2): different"
        , "runtime equality matrix (3,3): same"
        , "matrix partial ignores bottom index: ok"
        , "matrix partial ignores cyclic index: ok"
        , "matrix first index before second: packed-bottom-length"
        , "matrix zero demands second index: equality-right-length"
        , "matrix zero successor ignores predecessor: different"
        , "matrix successor zero ignores predecessor: different"
        , "matrix recursive left before right: packed-bottom-length"
        , "matrix recursive right demand: equality-right-length"
        , "matrix shared bottom still demanded: packed-bottom-length"
        , "matrix cyclic successor zero mismatch: different"
        , "runtime payload matrix (0,0): ([],[])"
        , "runtime payload matrix (0,1): ([],[9])"
        , "runtime payload matrix (1,0): ([7],[])"
        , "runtime payload matrix (1,1): ([7],[9])"
        , "payload matrix partial ignores fields: ok"
        , "payload matrix first index before payloads: packed-bottom-length"
        , "runtime fallback package (0,0): (0,[])|(0,[])"
        , "runtime fallback package (0,1): (0,[])|(1,[9])"
        , "runtime fallback package (1,0): (1,[7])|(0,[])"
        , "runtime fallback package (1,1): (1,[7])|(1,[9])"
        , "runtime zero-only fallback (0,0): same"
        , "runtime zero-only fallback (0,1): different"
        , "runtime zero-only fallback (1,0): different"
        , "runtime zero-only fallback (1,1): different"
        , "fallback skips second index: different"
        , "fallback zero demands second index: equality-right-length"
        , "fallback first index before second: packed-bottom-length"
        , "fallback partial ignores first index: ok"
        , "fallback zero successor packages stay lazy: ok"
        , "fallback successor zero packages stay lazy: ok"
        , "fallback package partial ignores fields: ok"
        , "runtime wildcard fallback (0,0): (0,[])|(0,[])|same"
        , "runtime wildcard fallback (0,1): (0,[])|(1,[9])|different"
        , "runtime wildcard fallback (1,0): (1,[7])|(0,[])|different"
        , "runtime wildcard fallback (1,1): (1,[7])|(1,[9])|different"
        , "wildcard skips second index: different"
        , "wildcard cyclic first skips second: different"
        , "wildcard zero demands second index: equality-right-length"
        , "wildcard first index before second: packed-bottom-length"
        , "wildcard partial ignores first index: ok"
        , "wildcard left package stays lazy: ok"
        , "wildcard right package stays lazy: ok"
        , "wildcard left partial ignores fields: ok"
        , "wildcard right partial ignores fields: ok"
        , "wildcard left retains payload demand: packed-bottom-payload"
        , "wildcard right retains payload demand: packed-bottom-payload"
        , "runtime local signature 0: (0,[],[],True)"
        , "runtime local signature 1: (1,[7],[7],True)"
        , "runtime local signature 2: (2,[7,7],[7,7],True)"
        , "runtime local signature 3: (3,[7,7,7],[7,7,7],True)"
        , "local signature shadowed indices: ([7],[9,9])"
        , "local signature erased variable precedence: [42]"
        , "local signature underscore index: [7]"
        , "local signature bottom index package: ok"
        , "local signature cyclic prefix: [7,7]"
        , "local signature partial index head: 7"
        , "local signature payload demand: packed-bottom-payload"
        , "local signature IO action: (0,1,[7])"
        , "runtime action binds: (0,[[],[],[],[],[],[]],[1,2,3,4,5,6])"
        , "runtime action binds: (1,[[7],[7],[7],[7],[7],[7]],[1,2,3,4,5,6])"
        , "runtime action binds: (2,[[7,7],[7,7],[7,7],[7,7],[7,7],[7,7]],[1,2,3,4,5,6])"
        , "runtime action binds: (3,[[7,7,7],[7,7,7],[7,7,7],[7,7,7],[7,7,7],[7,7,7]],[1,2,3,4,5,6])"
        , "action construction has no effects: 0"
        , "action ignores bottom index and payload: 42"
        , "action executes once: 1"
        , "saved computation executes again: 2"
        , "saved action ignores bottom payload: 42"
        , "action retains original payload error: packed-bottom-payload"
        , "action effect before payload error: 1"
        , "bottom action executes: native-action-bottom"
        , "action partial index prefix: [7]"
        , "action cyclic index prefix: [7,7]"
        , "inline action ignores bottom index: 42"
        , "explicit bind ignores bottom payload: 42"
        , "action repeated payload sharing: True"
        , "runtime source inference: (0,[[],[],[]],[0,1,2,3])"
        , "runtime source inference: (1,[[7],[7],[7]],[0,1,2,3])"
        , "runtime source inference: (2,[[7,7],[7,7],[7,7]],[0,1,2,3])"
        , "runtime source inference: (3,[[7,7,7],[7,7,7],[7,7,7]],[0,1,2,3])"
        , "inferred construction has no effects: 0"
        , "inferred result ignores bottom fields: 42"
        , "inferred action executes once: 1"
        , "inferred computation executes again: 2"
        , "inferred vector retains index demand: packed-bottom-length"
        , "inferred result ignores bottom payload: 42"
        , "inferred result retains payload error: packed-bottom-payload"
        , "inferred payload demand follows effects: 2"
        , "inferred bottom action executes: native-inferred-action-bottom"
        , "inferred partial index prefix: [7]"
        , "inferred cyclic index prefix: [7,7]"
        , "inferred computation shares vector: True"
        , "inferred vector shares original payload: True"
        , "runtime tuple inference: (0,[(42,[]),(42,[]),(42,[])],[0,1,2,3])"
        , "runtime tuple inference: (1,[(42,[7]),(42,[7]),(42,[7])],[0,1,2,3])"
        , "runtime tuple inference: (2,[(42,[7,7]),(42,[7,7]),(42,[7,7])],[0,1,2,3])"
        , "runtime tuple inference: (3,[(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7])],[0,1,2,3])"
        , "tuple construction has no effects: 0"
        , "tuple WHNF ignores bottom index: ok"
        , "tuple first field ignores bottom index: 42"
        , "tuple action executes once: 1"
        , "tuple vector retains index demand: packed-bottom-length"
        , "tuple WHNF ignores bottom fields: ok"
        , "tuple effect precedes field demand: 1"
        , "tuple first field retains error: native-tuple-first-bottom"
        , "tuple second field retains payload error: packed-bottom-payload"
        , "tuple partial index prefix: [7]"
        , "tuple cyclic index prefix: [7,7]"
        , "tuple later field ignores native first field: 42"
        , "tuple computations share original vector: True"
        , "tuple vector shares original payload: True"
        , "runtime tuple sections: (0,[(42,[]),(43,[]),(42,[]),(43,[]),(42,[])],[0,1,2,3])"
        , "runtime tuple sections: (1,[(42,[7]),(43,[7]),(42,[7]),(43,[7]),(42,[7])],[0,1,2,3])"
        , "runtime tuple sections: (2,[(42,[7,7]),(43,[7,7]),(42,[7,7]),(43,[7,7]),(42,[7,7])],[0,1,2,3])"
        , "runtime tuple sections: (3,[(42,[7,7,7]),(43,[7,7,7]),(42,[7,7,7]),(43,[7,7,7]),(42,[7,7,7])],[0,1,2,3])"
        , "section construction has no effects: 0"
        , "section WHNF ignores bottom index: ok"
        , "section tuple WHNF ignores bottom index: ok"
        , "section later field ignores bottom index: 42"
        , "section action executes once: 1"
        , "section vector retains index demand: packed-bottom-length"
        , "section checks native missing field: (42,[7,7])"
        , "section WHNF ignores bottom present field: ok"
        , "section tuple WHNF ignores bottom present field: ok"
        , "section later field ignores bottom present field: 42"
        , "section first field retains payload error: packed-bottom-payload"
        , "section second field retains payload error: packed-bottom-payload"
        , "section hole order: ([7,7],42,True)"
        , "partial section ignores present field: ok"
        , "section partial index prefix: [7]"
        , "section cyclic index prefix: [7,7]"
        , "section applications share captured vector: True"
        , "section vector shares original payload: True"
        , "runtime generic overapplication: (0,[(42,[]),(42,[]),(42,[]),(42,[]),(42,[])],[0,1,2,3,4,5])"
        , "runtime generic overapplication: (1,[(42,[7]),(42,[7]),(42,[7]),(42,[7]),(42,[7])],[0,1,2,3,4,5])"
        , "runtime generic overapplication: (2,[(42,[7,7]),(42,[7,7]),(42,[7,7]),(42,[7,7]),(42,[7,7])],[0,1,2,3,4,5])"
        , "runtime generic overapplication: (3,[(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7])],[0,1,2,3,4,5])"
        , "generic application construction has no effects: 0"
        , "generic application action executes once: 1"
        , "generic application action executes again: 2"
        , "generic projection WHNF ignores index: ok"
        , "generic application WHNF ignores index: ok"
        , "generic application later field ignores index: 42"
        , "generic application retains index demand: packed-bottom-length"
        , "generic application ignores bottom payload: 42"
        , "generic application retains payload error: packed-bottom-payload"
        , "unused generic projection ignores bottom function: 42"
        , "generic application demands bottom function: native-projection-bottom-function"
        , "unused generic projection ignores bottom pair: 42"
        , "generic application demands bottom pair: native-projection-bottom-pair"
        , "constrained projection retains tag demand: native-projection-bottom-tag"
        , "generic application partial index prefix: [7]"
        , "generic application cyclic index prefix: [7,7]"
        , "generic applications share captured vector: True"
        , "generic application vector shares original payload: True"
        , "runtime leading VTA: (0,[(42,[]),(42,[]),(42,[]),(42,[]),(42,[])],[0,1,2,3,4,5])"
        , "runtime leading VTA: (1,[(42,[7]),(42,[7]),(42,[7]),(42,[7]),(42,[7])],[0,1,2,3,4,5])"
        , "runtime leading VTA: (2,[(42,[7,7]),(42,[7,7]),(42,[7,7]),(42,[7,7]),(42,[7,7])],[0,1,2,3,4,5])"
        , "runtime leading VTA: (3,[(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7]),(42,[7,7,7])],[0,1,2,3,4,5])"
        , "leading VTA construction has no effects: 0"
        , "leading VTA action executes once: 1"
        , "leading VTA action executes again: 2"
        , "leading VTA function WHNF ignores index: ok"
        , "leading VTA result WHNF ignores index: ok"
        , "leading VTA later field ignores index: 42"
        , "leading VTA retains index demand: packed-bottom-length"
        , "leading VTA ignores bottom payload: 42"
        , "leading VTA retains payload error: packed-bottom-payload"
        , "unconstrained VTA ignores bottom tag: 42"
        , "constrained VTA demands bottom tag: native-vta-bottom-tag"
        , "unused VTA selection ignores bottom function: 42"
        , "leading VTA demands bottom function: native-vta-bottom-function"
        , "leading VTA partial index prefix: [7]"
        , "leading VTA cyclic index prefix: [7,7]"
        , "leading VTA selections share captured vector: True"
        , "leading VTA vector shares original payload: True"
        , "full ordinary VTA control: 43"
        , "runtime index VTA: (0,[42,42],[],[],True,1,[0,1,2])"
        , "runtime index VTA: (1,[42,42],[7],[7],True,2,[0,1,2])"
        , "runtime index VTA: (2,[42,42],[7,7],[7,7],True,3,[0,1,2])"
        , "runtime index VTA: (3,[42,42],[7,7,7],[7,7,7],True,4,[0,1,2])"
        , "index VTA construction has no effects: 0"
        , "index VTA ignores bottom fields: 42"
        , "index VTA action executes once: 1"
        , "index VTA action executes again: 2"
        , "index VTA retains vector error: packed-bottom-payload"
        , "index VTA demands index through vector: packed-bottom-length"
        , "index VTA spine ignores element: 1"
        , "index VTA retains element error: packed-filter-bottom-element"
        , "index VTA partial prefix: [7]"
        , "index VTA cyclic prefix: [7,7]"
        , "index VTA first vector before second: zip-left-vector"
        , "index VTA second vector demand: zip-right-vector"
        , "index VTA shares named vector: True"
        , "index VTA shares original payload: True"
        , "runtime joint index inference: (0,[],[(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9])],[43,43,43,43],[0,1,2])"
        , "runtime joint index inference: (1,[7],[(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9])],[43,43,43,43],[0,1,2])"
        , "runtime joint index inference: (2,[7,7],[(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9])],[43,43,43,43],[0,1,2])"
        , "runtime joint index inference: (3,[7,7,7],[(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9]),(42,[9])],[43,43,43,43],[0,1,2])"
        , "joint selection construction has no effects: 0"
        , "joint selection action executes once: 1"
        , "joint selection action executes again: 2"
        , "joint selected function ignores prefix index: ok"
        , "joint selected later field ignores prefix index: 42"
        , "joint payload keeps its independent index: [9]"
        , "joint selection preserves payload error: packed-bottom-payload"
        , "joint selection preserves payload index error: packed-bottom-length"
        , "unused joint selection ignores bottom function: 42"
        , "joint application demands selected function: native-joint-bottom-function"
        , "joint projection demands containing pair: native-joint-bottom-pair"
        , "joint context preserves tag demand: native-joint-bottom-tag"
        , "joint selection partial payload prefix: [9]"
        , "joint selection partial payload tail: packed-bottom-length"
        , "joint selection cyclic payload prefix: [9,9]"
        , "joint selection spine ignores element: 1"
        , "joint selection preserves element error: packed-filter-bottom-element"
        , "joint selections share named vector: True"
        , "joint selections share original payload: True"
        , "joint selection applies retained function: [7,7]"
        , "runtime Pi Given: (0,[],[],[])"
        , "runtime Pi Given: (1,[7],[7],[7])"
        , "runtime Pi Given: (2,[7,7],[7,7],[7,7])"
        , "runtime Pi Given: (3,[7,7,7],[7,7,7],[7,7,7])"
        , "direct Pi Given preserves bottom function: native-pi-bottom-function"
        , "eta Pi Given returns a function: ok"
        , "eta Pi Given demands applied function: native-pi-bottom-function"
        , "unused Pi Given ignores bottom function: 42"
        , "unused Pi Given ignores bottom index: 42"
        , "unused Pi Given ignores cyclic index: 42"
        , "Pi Given vector retains index demand: packed-bottom-length"
        , "Pi Given vector spine ignores element: 1"
        , "Pi Given retains element error: packed-filter-bottom-element"
        , "unused Pi Given ignores bottom vector: 42"
        , "Pi Given vector error precedes index: packed-bottom-payload"
        , "Pi Given partial index prefix: [7]"
        , "Pi Given cyclic index prefix: [7,7]"
        , "Pi Given shares original function: True"
        , "Pi Given shares named result: True"
        , "Pi Given shares original payload: True"
        , "Pi Given transport demands proof: native-pi-bottom-proof"
        , "Pi Given proof precedes bottom function: native-pi-bottom-proof"
        , "unused Pi Given ignores bottom proof: 42"
        , "Pi Given Refl preserves bottom function: native-pi-bottom-function"
        , "runtime outer-index Given: (0,1,[],[True],True)"
        , "runtime outer-index Given: (2,1,[9,9],[True],True)"
        , "runtime outer-index Given: (3,2,[9,9,9],[True,True],True)"
        , "outer-index Given ignores bottom index: ok"
        , "outer-index Given ignores unused payload: []"
        , "runtime Pi operand: (0,[],[],[])"
        , "runtime Pi operand: (1,[7],[7],[7])"
        , "runtime Pi operand: (2,[7,7],[7,7],[7,7])"
        , "runtime Pi operand: (3,[7,7,7],[7,7,7],[7,7,7])"
        , "direct Pi operand preserves bottom function: native-pi-operand-bottom-function"
        , "eta Pi operand returns a function: ok"
        , "eta Pi operand demands applied function: native-pi-operand-bottom-function"
        , "unused Pi operand ignores bottom function: 42"
        , "Pi operand proof precedes bottom function: native-pi-operand-bottom-proof"
        , "unused Pi operand ignores bottom proof: 42"
        , "Pi operand Refl preserves bottom function: native-pi-operand-bottom-function"
        , "unused Pi operand ignores bottom index: 42"
        , "unused Pi operand ignores cyclic index: 42"
        , "Pi operand vector retains index demand: packed-bottom-length"
        , "Pi operand partial index prefix: [7]"
        , "Pi operand cyclic index prefix: [7,7]"
        , "Pi operand shares original function: True"
        , "Pi operand shares named result: True"
        , "Pi operand shares original IORef payload: True"
        , "runtime outer Pi operand: (0,1,[],[True],True)"
        , "runtime outer Pi operand: (2,3,[9,9],[True,True,True],True)"
        , "outer Pi operand ignores bottom index: ok"
        , "outer Pi operand ignores unused payload: []"
        , "Pi operand vector error precedes index: packed-bottom-payload"
        , "runtime lifted Pi operand: (0,0,[],[])"
        , "runtime lifted Pi operand: (1,2,[7,7],[True])"
        , "runtime lifted Pi operand: (2,1,[7],[True,True])"
        , "runtime lifted Pi operand: (3,3,[7,7,7],[True,True,True])"
        , "lifted Pi operand shares outer function: True"
        , "lifted Pi operand projection ignores bottom outer index: ok"
        ]
    putStrLn "packed vector module checks: ok"
    putStrLn "module execution checks: ok"

-- | Read a producer interface in a fresh one-shot context and link its object.
runModuleCase :: FilePath -> String -> String -> String -> String -> [String] -> IO ()
runModuleCase libdir label moduleName producer =
    runModuleCases libdir label [(moduleName, producer)] []

-- | Compile ordered producers, then link a consumer against their interfaces.
runModuleCases :: FilePath -> String -> [(String, String)] -> [String] -> String -> [String] -> IO ()
runModuleCases libdir label producers extraFlags consumer expected = do
    directory <- makeAbsolute ("NativeDependentCoreRun.modules" </> label)
    let producerDirectory = directory </> "producer"
        consumerDirectory = directory </> "consumer"
        artifacts = directory </> "artifacts"
        producerSources = [(name, producerDirectory </> (name ++ ".hs"), source) | (name, source) <- producers]
        consumerSource = consumerDirectory </> "Main.hs"
        executablePath = directory </> ("native-modules" ++ if os == "mingw32" then ".exe" else "")
        configure linkMode output = nativeModuleEnvironment artifacts extraFlags linkMode output
        compile environment path = do
            result <- compileFile environment NoStop (path, Nothing)
            case result of
                Just object -> pure object
                Nothing -> fail ("FAIL: native module produced no object: " ++ path)
    forM_ [producerDirectory, consumerDirectory, artifacts] $ createDirectoryIfMissing True
    forM_ producerSources $ \(_, path, source) -> writeFile path source
    writeFile consumerSource consumer
    producerObjects <- mapM (\(_, path, _) -> GHC.runGhc (Just libdir) $ do
        environment <- configure NoLink Nothing
        liftIO $ compile environment path) producerSources
    forM_ producers $ \(name, _) -> do
        interfaceExists <- doesFileExist (artifacts </> (name ++ ".hi"))
        unless interfaceExists (fail "FAIL: native producer wrote no interface")
    GHC.runGhc (Just libdir) $ do
        -- OneShot searches only interfaces. The producer source is outside its import path.
        environment <- configure (LinkExecutable Dynamic) (Just executablePath)
        liftIO $ do
            consumerObject <- compile environment consumerSource
            oneShot environment NoStop [(object, Just StopLn) | object <- consumerObject : producerObjects]
    (status, actual, errors) <- readProcessWithExitCode executablePath [] ""
    unless (status == ExitSuccess && lines actual == expected) $
        fail ("FAIL: native cross-module observations (" ++ label ++ "): " ++ show (status, actual, errors))

-- | Check omitted and explicit invisible runtime arguments through an interface.
runInvisibleRetainedModuleChecks :: FilePath -> IO ()
runInvisibleRetainedModuleChecks libdir = do
    vectors <- readFile "PackedVecLibrary.hs"
    let extensions = "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ScopedTypeVariables, TypeApplications, TypeFamilies #-}"
        library = unlines
            [ extensions
            , "module NativeInvisibleRetained where"
            , "import PackedVecLibrary"
            , "observeIndex :: forall a. foreach (n :: Nat). Bool -> Vec a n -> Int"
            , "observeIndex _ _ = natToInt n"
            , "ignoreIndex :: forall a. foreach (n :: Nat). Vec a n -> Int"
            , "ignoreIndex _ = 42"
            , "compareIndices :: foreach (n :: Nat). Vec Int n -> Vec Int n -> Int"
            , "compareIndices _ _ = natToInt n"
            , "missingIndex :: foreach (n :: Nat). Bool -> Int"
            , "missingIndex _ = 42"
            , "noWrittenArguments :: foreach (n :: Nat). Int"
            , "noWrittenArguments = natToInt n"
            , "shadowIndex :: foreach (n :: Nat). Nat -> Vec Int n -> Int"
            , "shadowIndex n _ = natToInt n"
            , "lengthCode :: foreach (n :: Nat). foreach (m :: Nat). Vec Int n -> Vec Int m -> Int"
            , "lengthCode _ _ = 10 * natToInt n + natToInt m"
            , "emptyVector :: Vec Int Z"
            , "emptyVector = VNil"
            , "type family Forget (n :: Nat) :: Nat where"
            , "    Forget Z = Z"
            , "    Forget (S k) = Z"
            , "observeForgotten :: foreach (n :: Nat). Vec Int (Forget n) -> Int"
            , "observeForgotten _ = natToInt n"
            , "withAnchor :: foreach (n :: Nat). Vec Int (Forget n) -> Vec Int n -> Int"
            , "withAnchor _ _ = natToInt n"
            , "controls :: foreach (m :: Nat) -> Vec Int (Forget m) -> Vec Int m -> (Int, Int)"
            , "controls m xs ys = (observeForgotten @{m} xs, withAnchor xs ys)"
            , "missingSecond :: foreach (n :: Nat). foreach (m :: Nat). Vec Int n -> Int"
            , "missingSecond _ = natToInt m"
            , "refined :: foreach (n :: Nat) -> Vec Int n -> Int"
            , "refined n xs = case n of { Z -> observeIndex False xs; S _pred -> observeIndex False xs }"
            ]
        ordinary = unlines
            [ "{-# OPTIONS_GHC -Werror=unused-imports #-}"
            , "module VisibleIndexConstructor where"
            , "import NativeInvisibleRetained (observeIndex, emptyVector)"
            , "import PackedVecLibrary (Nat(Z))"
            , "observedEmpty :: Int"
            , "observedEmpty = observeIndex False emptyVector"
            ]
        consumer = unlines
            [ extensions
            , "module Main where"
            , "import Data.IORef"
            , "import VisibleIndexConstructor (observedEmpty)"
            , "import NativeInvisibleRetained"
            , "import PackedVecLibrary"
            , "main :: IO ()"
            , "main = do"
            , "    n <- newIORef (S (S Z)) >>= readIORef"
            , "    let xs :: Vec Int n"
            , "        xs = replicateNative @Int n 7"
            , "    print (observeIndex False xs)"
            , "    print (observeIndex @Int False xs)"
            , "    print (observeIndex @Int @{n} False xs)"
            , "    print (observeIndex @{n} False xs)"
            , "    print (noWrittenArguments @{S (S Z)})"
            , "    print (shadowIndex Z xs)"
            , "    bottom <- newIORef (error \"invisible-index-bottom\" :: Nat) >>= readIORef"
            , "    let ys :: Vec Int bottom"
            , "        ys = replicateNative @Int bottom 9"
            , "    print (ignoreIndex ys)"
            , "    print (ignoreIndex @Int @{bottom} ys)"
            , "    m <- newIORef (S Z) >>= readIORef"
            , "    let zs :: Vec Int m"
            , "        zs = replicateNative @Int m 9"
            , "        joined :: Vec Int (Add n m)"
            , "        joined = appendHidden xs zs"
            , "    print (toList joined)"
            , "    print (toList (appendHidden @Int xs zs))"
            , "    print (toList (appendHidden @{n} xs zs))"
            , "    print (toList (appendHidden @Int @{n} @{m} xs zs))"
            , "    print (lengthCode xs zs, lengthCode zs xs, lengthCode xs xs)"
            , "    print (refined n xs, refined Z emptyVector)"
            , "    let lazySucc :: Vec Int (S bottom)"
            , "        lazySucc = error \"lazy constructor vector\""
            , "    print (ignoreIndex lazySucc, observedEmpty)"
            , "    print (controls (S Z) emptyVector (VCons 7 VNil))"
            ]
        label = "invisible-retained"
    runModuleCases libdir label
        [("PackedVecLibrary", vectors), ("NativeInvisibleRetained", library), ("VisibleIndexConstructor", ordinary)] []
        consumer (["2", "2", "2", "2", "2", "0", "42", "42"] ++ replicate 4 "[7,7,9]" ++ ["(21,12,22)", "(2,0)", "(42,0)", "(1,1)"])
    directory <- makeAbsolute ("NativeDependentCoreRun.modules" </> label)
    let artifacts = directory </> "artifacts"
        reject moduleName flags code fragment declarations = do
            let path = directory </> (moduleName ++ ".hs")
            writeFile path $ unlines $
                [flags, "module " ++ moduleName ++ " where", "import NativeInvisibleRetained"]
                    ++ declarations
            GHC.runGhc (Just libdir) $ do
                environment <- nativeModuleEnvironment artifacts [] NoLink Nothing
                result <- GHC.handleSourceError (pure . Left) $ do
                    liftIO $ void $ compileFile environment NoStop (path, Nothing)
                    pure (Right ())
                checkSourceRejection moduleName code fragment result
    forM_
        [ ("MissingInvisibleIndex", extensions, 83605, "Cannot infer the invisible retained argument",
            ["bad :: Int", "bad = missingIndex False"])
        , ("ConflictingInvisibleIndices", extensions, 83605, "Cannot infer the invisible retained argument",
            [ "bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Int"
            , "bad n m xs ys = compareIndices xs ys"
            ])
        , ("WrongExplicitIndex", extensions, 83865, "Couldn't match type",
            [ "bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Int"
            , "bad n m xs = observeIndex @Int @{m} False xs"
            ])
        , ("MissingSecondIndex", extensions, 83605, "invisible retained argument ‘m’",
            [ "bad :: foreach (n :: Nat) -> Vec Int n -> Int"
            , "bad n xs = missingSecond xs"
            ])
        , ("NonInjectiveInvisibleIndex", extensions, 83605, "Cannot infer the invisible retained argument",
            [ "bad :: foreach (m :: Nat) -> Vec Int (Forget m) -> Int"
            , "bad m xs = observeForgotten xs"
            ])
        , ("NoInvisibleBinder", extensions, 83605, "requires an invisible retained binder",
            ["bad :: Nat -> Nat", "bad = id @{Z}"])
        , ("ExplicitIndexExtensionOff", "", 83605, "Illegal explicit retained argument",
            ["bad :: Int", "bad = noWrittenArguments @{Z}"])
        ] $ \(moduleName, flags, code, fragment, declarations) ->
            reject moduleName flags code fragment ("import PackedVecLibrary" : declarations)
    reject "HiddenIndexConstructor" extensions 83605 "Cannot infer the invisible retained argument"
        ["bad :: Int", "bad = observeIndex False emptyVector"]

-- | Configure the shared one-shot compiler for object and diagnostic checks.
nativeModuleEnvironment :: FilePath -> [String] -> GhcLink -> Maybe FilePath -> GHC.Ghc HscEnv
nativeModuleEnvironment artifacts extraFlags linkMode output = do
    flags <- GHC.getSessionDynFlags
    logger <- GHC.getLogger
    (configured, leftovers, _) <-
        GHC.parseDynamicFlags logger flags $ map noLoc $
            [ "-O0", "-fasm", "-fomit-interface-pragmas", "-fno-write-if-simplified-core"
            , "-dcore-lint", "-dstg-lint", "-fforce-recomp", "-v0"
            , "-odir", artifacts, "-hidir", artifacts
            ] ++ extraFlags
    unless (null leftovers) (fail "FAIL: native module flags were not parsed")
    void $ GHC.setSessionDynFlags configured
        { ghcMode = OneShot
        , ghcLink = linkMode
        , outputFile_ = output
        , importPaths = [artifacts]
        }
    GHC.getSession

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
    -- Moving a clause changes its source location. Changing its result
    -- invalidates the proof, even when the interface omits unfoldings.
    let withFlipBit body = unlines
            [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "module NativeRetainedLibrary (flipBit) where"
            , "flipBit :: Bool -> Bool"
            , body
            ]
        table falseRow = withFlipBit (falseRow ++ "\nflipBit True = False")
        proofSource = unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}"
            , "module NativeRetainedUsage where"
            , "import NativeRetainedLibrary (flipBit)"
            , "import Data.Type.Equality"
            , "proof :: flipBit 'False :~: 'True"
            , "proof = Refl"
            ]
    first <- producer (table "flipBit False = True")
    writeFile consumerSource proofSource
    void $ consumer True
    setModificationTime consumerSource sourceTime
    forM_ [consumerObject, consumerInterface] $ \path -> setModificationTime path objectTime
    moved <- producer (table "\nflipBit False = True")
    let equationLines iface =
            [ srcSpanStartLine loc
            | (_, IfaceAxiom{ifAxSourceSpans = spans}) <- mi_decls iface
            , RealSrcSpan loc _ <- spans
            ]
    unless (equationLines first == [4, 5] && equationLines moved == [5, 6]) $
        fail "FAIL: source equation locations did not survive an interface round trip"
    unless (mi_mod_hash first == mi_mod_hash moved) $
        fail "FAIL: moving a source equation changed the module ABI"
    checkSkipped
    void $ producer (table "flipBit False = False")
    wrong <- try (void (consumer False))
    GHC.runGhc (Just libdir) $ checkSourceRejection "changed source equation" 83865 "False" wrong
    -- An absent table permits implicit quantification over the same spelling.
    void $ producer (withFlipBit "flipBit b = b")
    writeFile consumerSource $ unlines
        [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
        , "module NativeRetainedUsage where"
        , "import NativeRetainedLibrary (flipBit)"
        , "p :: flipBit () -> flipBit ()"
        , "p x = x"
        ]
    void $ consumer True
    void $ producer (table "flipBit False = True")
    newTable <- try (void (consumer False))
    GHC.runGhc (Just libdir) $ checkSourceRejection "new source table" 83865 "Bool" newTable
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
    let sourceWith declarations = retainedProgramSource ++ unlines declarations
        inlineSource = sourceWith ["{-# INLINE entry #-}"]
    forM_ [(optimization, source) | optimization <- [[], ["-O2"]],
                                     source <- [retainedProgramSource, inlineSource]] $ \(optimization, source) -> do
        withProgram optimization source $ do
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
    let classBody = drop 1 $ dropWhile (/= "entry =") $ lines $ retainedChooseSource "NativeDefault" "driveBoth run"
        classSource =
            sourceWith $
                [ "class NativeDefault a where"
                , "  method :: a -> IO String"
                , "  method _ ="
                ]
                    ++ map ("  " ++) classBody
        rejections =
            [ ("custom optimization", ["-O2", "-fno-spec-constr"], retainedProgramSource, "optimized module compilation")
            , ("interface pragmas", ["-fno-omit-interface-pragmas"], retainedProgramSource, "interface unfolding generation")
            , ("serialized Core", ["-fwrite-if-simplified-core"], retainedProgramSource, "Core interface serialization")
            , ("late cost centres", ["-fprof-late"], retainedProgramSource, "profiling or late cost-centre insertion")
            , ("no-code backend", ["-fno-code", "-no-link"], retainedProgramSource, "module compilation without an object or bytecode backend")
            , ("native class default method", [], classSource, "default-method unfolding construction while desugaring binding")
            ]
        -- The make driver catches panics as Failed. Call this phase directly.
        checkProgram = do
            void (GHC.depanal [] False)
            summary <- GHC.getModSummary (mkModule mainUnit (mkModuleName "Main"))
            parsed <- GHC.parseModule summary
            checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
            desugared <- GHC.desugarModule checked
            environment <- GHC.getSession
            liftIO $ void $ hscSimplify environment [] (GHC.coreModule desugared)
    forM_ rejections $ \(label, extraFlags, source, phase) -> do
        result <- try (withProgram extraFlags source checkProgram) :: IO (Either GhcException ())
        case result of
            Left (Sorry message)
                | phase `isInfixOf` message
                , "This operation does not support" `isInfixOf` message ->
                    pure ()
                | otherwise -> fail ("FAIL: program rejection " ++ label ++ "; wrong phase: " ++ message)
            Left exception -> fail ("FAIL: program rejection " ++ label ++ "; wrong exception: " ++ displayException exception)
            Right () -> fail ("FAIL: native program accepted " ++ label)
    withProgram [] (sourceWith ["{-# INLINABLE entry #-}"]) $ do
        result <- GHC.handleSourceError (pure . Left) (Right <$> checkProgram)
        checkSourceRejection "INLINABLE entry" 92256 "The INLINABLE unfolding of" result
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
    -- Strict forces the anonymous argument. The case must retain its index.
    void $ sourceEntry "NativeSourceStrictWildcard" $ unlines
        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, Strict #-}"
        , "module NativeSourceStrictWildcard where"
        , "import Data.Proxy"
        , "tag :: foreach (b :: Bool) -> Proxy b"
        , "tag _ = Proxy"
        , "entry :: Proxy True"
        , "entry = tag True"
        ]
    void $ sourceEntry "NativeSourceOpaqueInt" $ unlines
        [ "{-# LANGUAGE ExperimentalDependentTypes, TypeOperators #-}"
        , "module NativeSourceOpaqueInt where"
        , "import Data.Type.Equality ((:~:)(Refl))"
        , "entry :: foreach (n :: Int) -> n :~: n"
        , "entry n = Refl"
        ]
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
    runPolyPayloadSourceCheck
    runNatSourceChecks

-- | Check nominal refinement through a rank-2 payload without forcing it.
runPolyPayloadSourceCheck :: GHC.Ghc ()
runPolyPayloadSourceCheck = do
    expression <- sourceEntry "NativeSourcePolyPayload" $ unlines
        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeOperators #-}"
        , "module NativeSourcePolyPayload where"
        , "type Poly (b :: Bool) = forall (a :: Bool). (a ~ b) => Int"
        , "keep :: foreach (b :: Bool) -> Poly b -> Int"
        , "keep True x = 42"
        , "keep False x = 42"
        , "entry :: IO String"
        , "entry = pure (show (keep True (error \"poly payload forced\" :: Poly True))"
        , "              ++ \":\" ++ show (keep False (error \"poly payload forced\" :: Poly False)))"
        ]
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source polymorphic payload" expression
    unless (actual == "42:42") $
        fail ("FAIL: source polymorphic payload refinement or laziness: " ++ show actual)

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
    runOpaqueCallSourceChecks
    runConstructorCallSourceChecks
    runBetaCallSourceChecks
    runFunctionBinderSourceChecks
    runFunctionBodyComparisonSourceChecks
    runCompleteFunctionArgumentSourceChecks
    runCallApplicationSourceChecks
    runCallActivationSourceChecks
    runConstructedCallActivationSourceChecks

-- | Add fixed-function-domain payload comparisons to the existing Nat source.
retainedFunctionBodyComparisonSource :: String -> [String] -> String -> String
retainedFunctionBodyComparisonSource moduleName extra observation =
    unlines $ "{-# LANGUAGE NoImpredicativeTypes, RankNTypes, TypeOperators #-}" : concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) (declarations ++ extra) ++ [line]
    insert line = [line]
    declarations =
        [ "polyWorker :: forall a. Num a => foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)"
        , "polyWorker f n = case f n of"
        , "    Z -> let { xs :: Vec a (f n); xs = VNil } in xs"
        , "    S k -> let { xs :: Vec a (f n); xs = VCons 42 (replicateNative @a k 42) } in xs"
        , "specialize :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m)"
        , "specialize = polyWorker"
        , "specializeAllocated :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m)"
        , "specializeAllocated = polyWorker @_"
        , "givenTransport :: forall a b. a ~ b => (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)) -> (foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec b (g m))"
        , "givenTransport original = original"
        , "polyUnknown :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n)"
        , "polyUnknown f n = polyUnknown @a f n"
        ]

{- Note [Fixed function-domain source comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The default comparison can select an ordinary payload from its expected type.
The wildcard comparison allocates that payload choice before retained-body
comparison. The function domain stays Nat -> Nat. Each runtime head is a
signed local Var. The existing Nat driver supplies the actual scalar Id.

The Given control can become reflexive through existing rewriting. It is not
evidence that the nonreflexive body solver ran. The escape control keeps the
payload choice outside the inner runtime head and scalar scopes. Do not fill
that choice with a type that contains those bound Ids.
-}

-- | Check ordinary payload choices and reject escaping native body choices.
runFunctionBodyComparisonSourceChecks :: GHC.Ghc ()
runFunctionBodyComparisonSourceChecks = do
    let moduleName = "NativeSourceFunctionBodyComparisons"
        observation =
            unlines
                [ "do defaults <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (vecList (specialize f n))))"
                , "        allocated <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (vecList (specializeAllocated f n))))"
                , "        given <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (vecList (givenTransport @Int @Int (polyWorker @Int) f n)))) (S (S Z))"
                , "        pure (defaults ++ allocated ++ unlines [given])"
                ]
        expected = ["[]", "[42,42]", "[42,42,42]", "[]", "[42,42]", "[42,42,42]", "[42,42]"]
    expression <- sourceEntry moduleName (retainedFunctionBodyComparisonSource moduleName [] observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "fixed function-domain body comparison" expression
    unless (lines actual == expected) $
        fail ("FAIL: fixed function-domain source payloads: " ++ show actual)
    forM_
        [ ( "NativeFunctionBodyWrongPayload"
          , "original"
          , [ "bad :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> (foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Bool (g m))"
            , "bad original = original"
            ]
          )
        , ( "NativeFunctionBodyEscapingPayload"
          , "polyUnknown"
          , [ "bad :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec (Vec Int (f n)) (f n)"
            , "bad f = polyUnknown @_ f"
            ]
          )
        , ( "NativeFunctionBodyChangedDomain"
          , "original"
          , [ "bad :: (foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n)) -> (foreach (g :: Bool -> Nat) -> foreach (b :: Bool) -> Vec Int (g b))"
            , "bad original = original"
            ]
          )
        ]
        $ \(name, fragment, declarations) ->
            sourceModuleRejects name 83865 fragment name $
                retainedFunctionBodyComparisonSource name declarations "pure \"unused\""

-- | Check explicit complete worker types without changing worker evaluation.
runCompleteFunctionArgumentSourceChecks :: GHC.Ghc ()
runCompleteFunctionArgumentSourceChecks = do
    let phi = "(foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n))"
        declarations =
            [ "raiseExplicit :: forall a. String -> a"
            , "raiseExplicit = error"
            , "selectExplicit :: " ++ phi ++ " -> " ++ phi
            , "selectExplicit worker = id @" ++ phi ++ " worker"
            , "forceExplicit :: " ++ phi ++ " -> IO String"
            , "forceExplicit worker = seq @" ++ phi ++ " worker (pure \"forced\")"
            , "forceNested :: " ++ phi ++ " -> IO String"
            , "forceNested worker = seq @(Maybe " ++ phi ++ ") (Just worker) (pure \"wrapped\")"
            , "ambient :: foreach (n :: Nat) -> IO String"
            , "ambient n = let { worker :: foreach (f :: Nat -> Nat) -> Vec Int (f n); worker f = polyWorker @Int f n } in force @(foreach (f :: Nat -> Nat) -> Vec Int (f n)) worker"
            ]
        moduleName = "NativeCompleteFunctionArguments"
        source name extra observation =
            retainedFunctionBodyComparisonSource name (declarations ++ extra) observation
        observation =
            unlines
                [ "do values <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (vecList (selectExplicit specialize f n))))"
                , "        whole <- forceExplicit specialize"
                , "        nested <- forceNested (error \"unused nested worker\")"
                , "        ignored <- ambient bottomNat"
                , "        pure (values ++ unlines [whole,nested,ignored])"
                ]
    expression <- sourceEntry moduleName (source moduleName [] observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "complete function arguments" expression
    unless (lines actual == ["[]", "[42,42]", "[42,42,42]", "forced", "wrapped", "forced"]) $
        fail ("FAIL: complete function arguments: " ++ show actual)
    let bottomName = "NativeExplicitBottomWorker"
    bottomExpression <-
        sourceEntry bottomName $
            source bottomName [] ("forceExplicit (raiseExplicit @" ++ phi ++ " \"explicit-bottom-worker\")")
    bottomEnvironment <- GHC.getSession
    outcome <- liftIO $ try (execute bottomEnvironment flags bottomName bottomExpression)
    case outcome of
        Left exception
            | "explicit-bottom-worker" `isInfixOf` displayException (exception :: SomeException) -> pure ()
        _ -> fail "FAIL: explicit complete worker did not preserve bottom"
    let partialName = "NativeRequiredPartialFunctionArgument"
        partialSource =
            "{-# LANGUAGE RequiredTypeArguments, ExplicitNamespaces #-}\n"
                ++ source
                    partialName
                    [ "ignoreType :: forall a -> ()"
                    , "ignoreType (type a) = ()"
                    , "bad = ignoreType (Either (type " ++ phi ++ ") _)"
                    ]
                    "pure \"unused\""
    sourceModuleRejects partialName 83605 "must be complete" partialName partialSource

-- | Add head-only retained function workers as a separate source corpus.
retainedFunctionBinderSource :: String -> String -> String
retainedFunctionBinderSource moduleName observation =
    unlines $ concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "-- | Ignore all three runtime arguments, including the opaque function head."
        , "keepCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Int"
        , "keepCall f n payload = 42"
        , ""
        , "-- | Return the original payload at the same actual function and scalar indices."
        , "idCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Vec Int (f n)"
        , "idCall f n payload = payload"
        , ""
        , "-- | Fix a lifted error result before a visible native type argument."
        , "raiseFunctionError :: forall a. String -> a"
        , "raiseFunctionError = error"
        , ""
        , "-- | Supply an ignored payload without evaluating its index or head."
        , "ignoreCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "ignoreCall f n ="
        , "    let payload :: Vec Int (f n)"
        , "        payload = raiseFunctionError @(Vec Int (f n)) \"function-payload\""
        , "     in keepCall f n payload"
        , ""
        , "-- | Demand the original payload through a fully instantiated identity caller."
        , "demandCallPayload :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "demandCallPayload f n ="
        , "    let caller :: Vec Int (f n) -> Vec Int (f n)"
        , "        caller = idCall f n"
        , "        payload :: Vec Int (f n)"
        , "        payload = raiseFunctionError @(Vec Int (f n)) \"function-payload\""
        , "     in length (vecList (caller payload))"
        , ""
        , "-- | Transport an already-instantiated caller through a case on its whole index."
        , "observeFunctionCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]"
        , "observeFunctionCall f n ="
        , "    let caller :: Vec Int (f n) -> Vec Int (f n)"
        , "        caller = idCall f n"
        , "     in case f n of"
        , "            Z -> let payload :: Vec Int (f n); payload = VNil in vecList (caller payload)"
        , "            S k -> let payload :: Vec Int (f n); payload = VCons 42 (replicateNative @Int k 42) in vecList (caller payload)"
        , ""
        , "-- | Keep independently closed input and result domains on a function binder."
        , "keepMixedCall :: foreach (f :: Bool -> Nat) -> foreach (b :: Bool) -> Vec Int (f b) -> Int"
        , "keepMixedCall f b payload = 42"
        , ""
        , "-- | Ignore a mixed-domain payload through the exact actual head."
        , "ignoreMixedCall :: foreach (f :: Bool -> Nat) -> foreach (b :: Bool) -> Int"
        , "ignoreMixedCall f b ="
        , "    let payload :: Vec Int (f b)"
        , "        payload = raiseFunctionError @(Vec Int (f b)) \"mixed-payload\""
        , "     in keepMixedCall f b payload"
        , ""
        , "makeFunctionHead :: Int -> Nat -> Nat"
        , "makeFunctionHead token = error \"producer-head\""
        ]

-- | Preserve payload and head demand without extending inherited totals.
runFunctionBinderSourceChecks :: GHC.Ghc ()
runFunctionBinderSourceChecks = do
    let moduleName = "NativeSourceFunctionBinders"
        observation =
            unlines
                [ "do finite <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (observeFunctionCall f n, ignoreCall f n)))"
                , "        ignored <- driveNat (\\n -> let { f :: Nat -> Nat; f = bottomFunction } in observeInt (ignoreCall f n)) bottomNat"
                , "        produced <- driveNat (\\n -> let { f :: Nat -> Nat; f = makeFunctionHead 7 } in observeInt (ignoreCall f n)) bottomNat"
                , "        constants <- driveNat (\\n -> let { f :: Nat -> Nat; f = constantZeroNat } in pure (show (observeFunctionCall f Z,observeFunctionCall f (S Z)))) Z"
                , "        headOnly <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (take 1 (observeFunctionCall f (S n))))) bottomNat"
                , "        cyclicPrefix <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (take 4 (observeFunctionCall f n)))) cyclicNat"
                , "        localAlias <- driveNat (\\n -> let { f :: Nat -> Nat; f = \\x -> x } in pure (show (observeFunctionCall f n))) (S Z)"
                , "        mixed <- drive (\\b -> let { f :: Bool -> Nat; f = \\x -> case x of {False -> Z;True -> S Z} } in observeInt (ignoreMixedCall f b)) bottomBool"
                , "        pure (finite ++ unlines [ignored,produced,constants,headOnly,cyclicPrefix,localAlias,mixed])"
                ]
        expected = ["([],42)", "([42,42],42)", "([42,42,42],42)", "42", "42", "([],[])", "[42]", "[42,42,42,42]", "[42]", "42"]
    expression <- sourceEntry moduleName (retainedFunctionBinderSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source function binders" expression
    unless (lines actual == expected) $ fail ("FAIL: source function binder demand: " ++ show actual)
    forM_
        [ ("NativeFunctionBinderPayload", "driveNat (\\n -> let { f :: Nat -> Nat; f = bottomFunction } in observeInt (demandCallPayload f n)) bottomNat", "function-payload")
        , ("NativeFunctionBinderHead", "driveNat (\\n -> let { f :: Nat -> Nat; f = bottomFunction } in observeInt (length (observeFunctionCall f n))) bottomNat", "native-bottom-function")
        , ("NativeFunctionBinderIndex", "driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (length (observeFunctionCall f n))) bottomNat", "native-bottom-nat")
        , ("NativeFunctionBinderProducer", "driveNat (\\n -> let { f :: Nat -> Nat; f = makeFunctionHead 7 } in observeInt (length (observeFunctionCall f n))) bottomNat", "producer-head")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedFunctionBinderSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                Left exception -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)

{- Note [Direct retained call applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These quiet checks keep the M46 source corpus separate. Exact branch-local
annotations activate the existing opaque-call case path. Instantiate the
retained worker inside that branch instead of capturing a signed caller first.
Ordinary argument and result checking emit equality Wanteds after actual
retained substitution. The existing row Given can solve an exact whole-call
equality. A partial recursive worker and an unrelated remaining Pi must retain
their binders. See Note [Source opaque call refinement] in GHC.Tc.Gen.Match.
Keep ordinary payload checking and the existing Lint paths.
-}

-- | Add direct branch-local applications without changing inherited sources.
retainedCallApplicationSource :: String -> String -> String
retainedCallApplicationSource moduleName observation =
    unlines $ concatMap insert (lines (retainedFunctionBinderSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "-- | Instantiate the original worker in both constructor-refined rows."
        , "directCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]"
        , "directCall f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = VNil in vecList (idCall f n payload)"
        , "    S k -> let payload :: Vec Int (f n); payload = VCons 42 (replicateNative @Int k 42) in vecList (idCall f n payload)"
        , "-- | Expose a native result immediately after the two retained operands."
        , "callResult :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m)"
        , "callResult g m = raiseFunctionError @(Vec Int (g m)) \"direct-result\""
        , "-- | Check the result-only route while leaving its bottom value unused."
        , "directResult :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "directResult f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = callResult f n in 42"
        , "    S k -> let payload :: Vec Int (f n); payload = callResult f n in 42"
        , "-- | Return a function without demanding the supplied vector."
        , "callPartial :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m) -> Int -> Int"
        , "callPartial g m payload token = token"
        , "-- | Keep the checked partial result lazy in its payload."
        , "directPartial :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int -> Int"
        , "directPartial f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = raiseFunctionError @(Vec Int (f n)) \"direct-payload\" in callPartial f n payload"
        , "    S k -> let payload :: Vec Int (f n); payload = raiseFunctionError @(Vec Int (f n)) \"direct-payload\" in callPartial f n payload"
        , "-- | Demand the directly transported payload only after the call case."
        , "directDemand :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "directDemand f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = raiseFunctionError @(Vec Int (f n)) \"direct-payload\" in length (vecList (idCall f n payload))"
        , "    S k -> let payload :: Vec Int (f n); payload = raiseFunctionError @(Vec Int (f n)) \"direct-payload\" in length (vecList (idCall f n payload))"
        , "-- | Keep a recursive partial worker under its remaining retained binder."
        , "walkCall :: foreach (g :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (g n) -> Vec Int (g n)"
        , "walkCall g n payload = case g n of"
        , "    Z -> let { again :: foreach (m :: Nat) -> Vec Int (g m) -> Vec Int (g m); again = walkCall g } in case again of !_ -> idCall g n payload"
        , "    S k -> idCall g n payload"
        , "-- | Observe the recursive partial worker with concrete branch payloads."
        , "directRecursive :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]"
        , "directRecursive f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = VNil in vecList (walkCall f n payload)"
        , "    S k -> let payload :: Vec Int (f n); payload = VCons 42 (replicateNative @Int k 42) in vecList (walkCall f n payload)"
        , "-- | Retain an unrelated index while the free call remains in later domains."
        , "pendingCall :: foreach (g :: Nat -> Nat) -> foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int m -> Vec Int (g n) -> Vec Int (g n)"
        , "pendingCall g n m witness payload = payload"
        , "-- | Force only that partial function, without casting under its Pi."
        , "directPending :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "directPending f n = case f n of"
        , "    Z -> let payload :: Vec Int (f n); payload = VNil in case pendingCall f n of !_ -> 42"
        , "    S k -> let payload :: Vec Int (f n); payload = VCons 42 (replicateNative @Int k 42) in case pendingCall f n of !_ -> 42"
        ]

-- | Check direct call transport, payload demand, and independent proof scopes.
runCallApplicationSourceChecks :: GHC.Ghc ()
runCallApplicationSourceChecks = do
    let moduleName = "NativeSourceCallApplications"
        observation =
            unlines
                [ "do finite <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (directCall f n)))"
                , "        result <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (directResult f n)) (S Z)"
                , "        partial <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in case directPartial f n of !_ -> pure \"forced\") Z"
                , "        constant <- driveNat (\\n -> let { f :: Nat -> Nat; f = constantZeroNat } in pure (show (directCall f n))) bottomNat"
                , "        constructed <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (directCall f (S n)))) (S Z)"
                , "        headOnly <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (take 1 (directCall f (S n))))) bottomNat"
                , "        cyclicPrefix <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (take 4 (directCall f n)))) cyclicNat"
                , "        recursive <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in pure (show (directRecursive f n))) Z"
                , "        pending <- driveNat (\\n -> let { f :: Nat -> Nat; f = constantZeroNat } in observeInt (directPending f n)) bottomNat"
                , "        pure (finite ++ unlines [result,partial,constant,constructed,headOnly,cyclicPrefix,recursive,pending])"
                ]
        expected = ["[]", "[42,42]", "[42,42,42]", "42", "forced", "[]", "[42,42]", "[42]", "[42,42,42,42]", "[]", "42"]
    expression <- sourceEntry moduleName (retainedCallApplicationSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source call applications" expression
    unless (lines actual == expected) $ fail ("FAIL: source call application demand: " ++ show actual)
    forM_
        [ ("NativeCallApplicationHead", "driveNat (\\n -> let { f :: Nat -> Nat; f = bottomFunction } in observeInt (length (directCall f n))) bottomNat", "native-bottom-function")
        , ("NativeCallApplicationIndex", "driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (length (directCall f n))) bottomNat", "native-bottom-nat")
        , ("NativeCallApplicationPayload", "driveNat (\\n -> let { f :: Nat -> Nat; f = constantZeroNat } in observeInt (directDemand f n)) bottomNat", "direct-payload")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedCallApplicationSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                Left exception -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)
    forM_
        [ ("NativeCallApplicationSiblingRow", 5617, ["badCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int", "badCall f n = case f n of { Z -> let { payload :: Vec Int (f n); payload = idCall f n VNil } in length (vecList payload); S k -> let { payload :: Vec Int (f n); payload = idCall f n VNil } in length (vecList payload) }"])
        , ("NativeCallApplicationDistinctIndex", 5617, ["badCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> foreach (m :: Nat) -> Int", "badCall f n m = case f n of { Z -> let { payload :: Vec Int (f n); payload = idCall f m VNil } in length (vecList payload); S k -> 42 }"])
        , ("NativeCallApplicationDistinctHead", 5617, ["badCall :: foreach (f :: Nat -> Nat) -> foreach (g :: Nat -> Nat) -> foreach (n :: Nat) -> Int", "badCall f g n = case f n of { Z -> let { payload :: Vec Int (f n); payload = idCall g n VNil } in length (vecList payload); S k -> 42 }"])
        ]
        $ \(name, code, declarations) -> do
            let source =
                    unlines
                        $ concatMap (\line -> if line == "      run :: Nat -> IO String" then map ("      " ++) declarations ++ [line] else [line])
                        $ lines (retainedCallApplicationSource name "pure \"unused\"")
            sourceModuleRejects name code "Vec" name source

{- Note [Source activation by a known retained worker]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These quiet checks use Int results and no branch-local index annotation.
The existing local worker signature identifies the exact call after actual
retained substitution. The ordinary row Given checks payload and result types.
Keep guarded and incomplete ordinary cases on their existing path.
See Note [Source opaque call refinement] in GHC.Tc.Gen.Match.
-}

-- | Add cases whose direct worker call activates whole-call refinement.
retainedCallActivationSource :: String -> String -> String
retainedCallActivationSource moduleName observation =
    unlines $ concatMap insert (lines (retainedFunctionBinderSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "-- | Infer the direct payload type in both constructor rows."
        , "activateCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "activateCall f n = case f n of"
        , "    Z -> length (vecList (idCall f n VNil))"
        , "    S k -> length (vecList (idCall f n (VCons 42 (replicateNative @Int k 42))))"
        , "-- | Check an unsigned row binding inside the same ordinary Given scope."
        , "activateWhere :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "activateWhere f n = case f n of"
        , "    Z -> length (vecList output)"
        , "        where output = idCall f n VNil"
        , "    S k -> length (vecList output)"
        , "        where output = idCall f n (VCons 42 (replicateNative @Int k 42))"
        , "-- | Keep an unused undefined payload lazy after selecting the row."
        , "activateIgnored :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "activateIgnored f n = case f n of"
        , "    Z -> const 42 (idCall f n undefined)"
        , "    S k -> const 42 (idCall f n undefined)"
        , "-- | Preserve ordinary guarded alternatives without a native worker use."
        , "activationGuarded :: (Nat -> Nat) -> Nat -> Int"
        , "activationGuarded f n = case f n of { Z | True -> 42; _ -> 7 }"
        , "-- | Preserve an ordinary incomplete case for the covered input."
        , "activationIncomplete :: (Nat -> Nat) -> Nat -> Int"
        , "activationIncomplete f n = case f n of { Z -> 42 }"
        ]

-- | Check no-annotation activation and ordinary case compatibility quietly.
runCallActivationSourceChecks :: GHC.Ghc ()
runCallActivationSourceChecks = do
    let moduleName = "NativeSourceCallActivation"
        observation =
            unlines
                [ "do direct <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (activateCall f n))"
                , "        bound <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (activateWhere f n))"
                , "        ignored <- driveNat (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (activateIgnored f n)) (S Z)"
                , "        pure (direct ++ bound ++ unlines [ignored,show (activationGuarded opaqueNat Z),show (activationGuarded opaqueNat (S Z)),show (activationIncomplete opaqueNat Z)])"
                ]
        expected = ["0", "2", "3", "0", "2", "3", "42", "42", "7", "42"]
    expression <- sourceEntry moduleName (retainedCallActivationSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source call activation" expression
    unless (lines actual == expected) $
        fail ("FAIL: source call activation or ordinary case compatibility: " ++ show actual)

-- | Add signed beta-call workers as a separate source corpus.
retainedBetaCallSource :: String -> String -> String
retainedBetaCallSource moduleName observation =
    unlines $ concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "-- | Check the canonical index of betaCallIdentity without unfolding the opaque head."
        , "betaCallIdentity :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallIdentity f n original = case f (((\\x -> x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f n); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallSuccessor without unfolding the opaque head."
        , "betaCallSuccessor :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallSuccessor f n original = case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallNested without unfolding the opaque head."
        , "betaCallNested :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallNested f n original = case f (((\\x -> S (((\\y -> S y) :: Nat -> Nat) x)) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S (S n))); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S (S n))); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallConstant without unfolding the opaque head."
        , "betaCallConstant :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallConstant f n original = case f (((\\x -> Z) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f Z); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f Z); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallHead without unfolding the opaque head."
        , "betaCallHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallHead f n original = case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in head (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallPrefix without unfolding the opaque head."
        , "betaCallPrefix :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> [Int]"
        , "betaCallPrefix f n original = case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in take 4 (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in take 4 (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallSeq without unfolding the opaque head."
        , "betaCallSeq :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallSeq f n original = case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in k `seq` head (vecList ys)"
        , ""
        , "-- | Check the canonical index of betaCallFunction without unfolding the opaque head."
        , "betaCallFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int"
        , "betaCallFunction f n original = case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in \\x -> length (vecList ys) + x"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in \\x -> head (vecList ys) + x"
        , ""
        , "-- | Transport a consumer that captures the canonical successor call."
        , "betaCallCaptured :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallCaptured f n original ="
        , "    let consume :: Vec Int (f (S n)) -> Int"
        , "        consume cached = case f (S n) of"
        , "            Z -> length (vecList cached)"
        , "            S k -> length (vecList cached)"
        , "     in case f (((\\x -> S x) :: Nat -> Nat) n) of"
        , "            Z -> let ys :: Vec Int (f (S n)); ys = VNil in consume ys"
        , "            S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in consume ys"
        , ""
        , "-- | Check a signed Bool endofunction in a mixed-domain call."
        , "betaCallBool :: (Bool -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "betaCallBool f n original = case f (((\\b -> b) :: Bool -> Bool) True) of"
        , "    Z -> let ys :: Vec Int (f True); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f True); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        ]

-- | Check signed beta calls without changing inherited source totals or goldens.
runBetaCallSourceChecks :: GHC.Ghc ()
runBetaCallSourceChecks = do
    let moduleName = "NativeSourceBetaCalls"
        observation =
            unlines
                [ "do finite <- driveNats (\\n -> pure (show (betaCallIdentity opaqueNat n bottomVec,betaCallSuccessor opaqueNat n bottomVec,betaCallNested opaqueNat n bottomVec,betaCallCaptured opaqueNat n bottomVec)))"
                , "        constant <- driveNat (\\n -> observeInt (betaCallConstant opaqueNat n bottomVec)) bottomNat"
                , "        ignored <- driveNat (\\n -> observeInt (betaCallIdentity constantZeroNat n bottomVec)) bottomNat"
                , "        headOnly <- driveNat (\\n -> observeInt (betaCallHead opaqueNat n bottomVec)) bottomNat"
                , "        cyclicHead <- driveNat (\\n -> observeInt (betaCallHead opaqueNat n bottomVec)) cyclicNat"
                , "        prefix <- driveNat (\\n -> pure (show (betaCallPrefix opaqueNat n bottomVec))) cyclicNat"
                , "        function <- driveNat (\\n -> case betaCallFunction opaqueNat n bottomVec of !_ -> pure \"forced\") bottomNat"
                , "        applied <- observeInt (betaCallFunction opaqueNat Z VNil 7)"
                , "        boolean <- driveNat (\\n -> observeInt (betaCallBool (\\b -> case b of {False -> Z;True -> S Z}) n bottomVec)) bottomNat"
                , "        pure (finite ++ unlines [constant,ignored,headOnly,cyclicHead,prefix,function,applied,boolean])"
                ]
        expected = ["(0,1,2,1)", "(2,3,4,3)", "(3,4,5,4)", "0", "0", "42", "42", "[42,42,42,42]", "forced", "49", "1"]
    expression <- sourceEntry moduleName (retainedBetaCallSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source beta calls" expression
    unless (lines actual == expected) $ fail ("FAIL: source beta call demand: " ++ show actual)
    forM_
        [ ("NativeSourceBetaPredecessor", "driveNat (\\n -> observeInt (betaCallSeq opaqueNat n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceBetaHead", "driveNat (\\n -> observeInt (betaCallHead bottomFunction n bottomVec)) bottomNat", "native-bottom-function")
        , ("NativeSourceBetaOperand", "driveNat (\\n -> observeInt (betaCallIdentity opaqueNat n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceBetaFunction", "driveNat (\\n -> case betaCallFunction bottomFunction n bottomVec of !_ -> pure \"forced\") bottomNat", "native-bottom-function")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedBetaCallSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                Left exception -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)

-- | Add constructor-call workers without changing the inherited source corpus.
retainedConstructorCallSource :: String -> String -> String
retainedConstructorCallSource moduleName observation =
    unlines $ "{-# LANGUAGE TypeOperators #-}" : concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line@"import NativeRuntimeHelpers hiding (appendVec)" = [line, "import Data.Type.Equality ((:~:)(Refl))"]
    insert line = [line]
    declarations =
        [ "-- | Observe an opaque call with a nullary constructor argument."
        , "constructorZero :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorZero f n original = case f Z of"
        , "    Z -> let ys :: Vec Int (f Z); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f Z); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Observe a call whose argument contains the retained runtime index."
        , "constructorLength :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorLength f n original = case f (S n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check a second constructor layer without evaluating its predecessor early."
        , "constructorTwice :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorTwice f n original = case f (S (S n)) of"
        , "    Z -> let ys :: Vec Int (f (S (S n))); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S (S n))); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Transport a consumer captured before the outer call case."
        , "constructorCaptured :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorCaptured f n original ="
        , "    let consume :: Vec Int (f (S n)) -> Int"
        , "        consume cached = case f (S n) of"
        , "            Z -> length (vecList cached)"
        , "            S k -> length (vecList cached)"
        , "     in case f (S n) of"
        , "            Z -> let ys :: Vec Int (f (S n)); ys = VNil in consume ys"
        , "            S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in consume ys"
        , ""
        , "-- | Read the new head without demanding the constructor argument's tail."
        , "constructorHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorHead f n original = case f (S n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in head (vecList ys)"
        , ""
        , "-- | Read a finite prefix from a cyclic constructor argument."
        , "constructorPrefix :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> [Int]"
        , "constructorPrefix f n original = case f (S n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in take 4 (vecList ys)"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in take 4 (vecList ys)"
        , ""
        , "-- | Demand the actual result predecessor explicitly."
        , "constructorSeq :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorSeq f n original = case f (S n) of"
        , "    Z -> 0"
        , "    S k -> k `seq` 42"
        , ""
        , "-- | Keep the call case outside a returned ordinary function."
        , "constructorFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int"
        , "constructorFunction f n original = case f (S n) of"
        , "    Z -> let ys :: Vec Int (f (S n)); ys = VNil in \\x -> length (vecList ys) + x"
        , "    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in \\x -> head (vecList ys) + x"
        , ""
        , "-- | Check a Bool constructor argument with a different result domain."
        , "constructorBool :: (Bool -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int"
        , "constructorBool f n original = case f True of"
        , "    Z -> let ys :: Vec Int (f True); ys = VNil in length (vecList ys)"
        , "    S k -> let ys :: Vec Int (f True); ys = VCons 42 (replicateNative @Int k 42) in length (vecList ys)"
        , ""
        , "-- | Check a constructor argument when the call returns Bool."
        , "constructorMixed :: (Nat -> Bool) -> foreach (n :: Nat) -> Vec Int n -> Bool"
        , "constructorMixed f n original = case f (S n) of"
        , "    False -> let witness :: f (S n) :~: 'False; witness = Refl in case witness of Refl -> False"
        , "    True -> let witness :: f (S n) :~: 'True; witness = Refl in case witness of Refl -> True"
        ]

-- | Keep constructor-call observations separate from the M43 source checks.
runConstructorCallSourceChecks :: GHC.Ghc ()
runConstructorCallSourceChecks = do
    let moduleName = "NativeSourceConstructorCalls"
        observation =
            unlines
                [ "do finite <- driveNats (\\n -> pure (show (constructorLength opaqueNat n bottomVec, constructorTwice opaqueNat n bottomVec, constructorCaptured opaqueNat n bottomVec)))"
                , "        zero <- driveNat (\\n -> observeInt (constructorZero opaqueNat n bottomVec)) bottomNat"
                , "        constant <- driveNat (\\n -> observeInt (constructorLength constantZeroNat n bottomVec)) bottomNat"
                , "        lazyHead <- driveNat (\\n -> observeInt (constructorHead opaqueNat n bottomVec)) bottomNat"
                , "        cyclicHead <- driveNat (\\n -> observeInt (constructorHead opaqueNat n bottomVec)) cyclicNat"
                , "        prefix <- driveNat (\\n -> pure (show (constructorPrefix opaqueNat n bottomVec))) cyclicNat"
                , "        boolean <- driveNat (\\n -> observeInt (constructorBool (\\b -> case b of { False -> Z; True -> S Z }) n bottomVec)) bottomNat"
                , "        mixed <- driveNat (\\n -> observeBool (constructorMixed (\\x -> True) n bottomVec)) bottomNat"
                , "        function <- driveNat (\\n -> case constructorFunction opaqueNat n bottomVec of !_ -> pure \"forced\") bottomNat"
                , "        applied <- observeInt (constructorFunction opaqueNat Z VNil 7)"
                , "        constants <- pure (show (constructorLength constantZeroNat Z VNil, constructorLength constantZeroNat (S Z) (VCons 7 VNil)))"
                , "        pure (finite ++ unlines [zero,constant,lazyHead,cyclicHead,prefix,boolean,mixed,function,applied,constants])"
                ]
        expected = ["(1,2,1)", "(3,4,3)", "(4,5,4)", "0", "0", "42", "42", "[42,42,42,42]", "1", "True", "forced", "49", "(0,0)"]
    expression <- sourceEntry moduleName (retainedConstructorCallSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source constructor calls" expression
    unless (lines actual == expected) $
        fail ("FAIL: source constructor call refinement or demand: " ++ show actual)
    forM_
        [ ("NativeSourceConstructorPredecessor", "driveNat (\\n -> observeInt (constructorSeq opaqueNat n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceConstructorHead", "driveNat (\\n -> observeInt (constructorHead bottomFunction n bottomVec)) bottomNat", "native-bottom-function")
        , ("NativeSourceConstructorLength", "driveNat (\\n -> observeInt (constructorLength opaqueNat n bottomVec)) bottomNat", "native-bottom-nat")
        , ("NativeSourceConstructorFunction", "driveNat (\\n -> case constructorFunction bottomFunction n bottomVec of !_ -> pure \"forced\") bottomNat", "native-bottom-function")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedConstructorCallSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)

-- | Check source call refinement, captured consumers and strict case demand.
runOpaqueCallSourceChecks :: GHC.Ghc ()
runOpaqueCallSourceChecks = do
    let moduleName = "NativeSourceOpaqueCalls"
        observation =
            unlines
                [ "do identityValues <- driveNats (\\n -> callObserve opaqueNat n bottomVec)"
                , "        constantValues <- driveNats (\\n -> callObserve constantZeroNat n bottomVec)"
                , "        captured <- driveNats (\\n -> observeInt (callCached opaqueNat n bottomVec))"
                , "        functions <- driveNats (\\n -> case callFunction opaqueNat n bottomVec of !_ -> pure \"forced\")"
                , "        ignoredIndex <- driveNat (\\n -> callObserve constantZeroNat n bottomVec) bottomNat"
                , "        lazyHead <- driveNat (\\n -> callHead opaqueNat n bottomVec) partialNat"
                , "        cyclicPrefix <- driveNat (\\n -> callPrefix opaqueNat n bottomVec) cyclicNat"
                , "        wildcard <- observeInt (ordinaryCallWildcard bottomFunction bottomNat)"
                , "        variable <- observeInt (ordinaryCallVariable bottomFunction bottomNat)"
                , "        pure (identityValues ++ constantValues ++ captured ++ functions ++ unlines [ignoredIndex,lazyHead,cyclicPrefix,wildcard,variable])"
                ]
        expected =
            [ "[]:0"
            , "[42,42]:2"
            , "[42,42,42]:3"
            , "[]:0"
            , "[]:0"
            , "[]:0"
            , "0"
            , "2"
            , "3"
            , "forced"
            , "forced"
            , "forced"
            , "[]:0"
            , "42"
            , "[42,42,42,42]"
            , "42"
            , "42"
            ]
    expression <- sourceEntry moduleName (retainedCallSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "source opaque calls" expression
    unless (lines actual == expected) $
        fail ("FAIL: source opaque call refinement, capture or demand: " ++ show actual)
    forM_
        [ ("NativeSourceOpaqueHeadBottom", "driveNat (\\n -> callHead bottomFunction n bottomVec) bottomNat", "native-bottom-function")
        , ("NativeSourceOpaqueArgumentBottom", "driveNat (\\n -> callHead opaqueNat n bottomVec) bottomNat", "native-bottom-nat")
        , ("NativeSourceOpaqueFunctionResultBottom", "driveNat (\\n -> case callFunction bottomFunction n bottomVec of !_ -> pure \"forced\") bottomNat", "native-bottom-function")
        ]
        $ \(name, body, fragment) -> do
            bottomExpression <- sourceEntry name (retainedCallSource name body)
            bottomEnvironment <- GHC.getSession
            outcome <- liftIO $ try (execute bottomEnvironment flags name bottomExpression)
            case outcome of
                Left exception
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ name ++ "; wrong exception: " ++ displayException exception)
                Right result -> fail ("FAIL: " ++ name ++ "; expected bottom, got " ++ show result)

    runStrictOpaqueCallSourceChecks

-- | Preserve source Strict demand without forcing administrative captures.
runStrictOpaqueCallSourceChecks :: GHC.Ghc ()
runStrictOpaqueCallSourceChecks =
    forM_
        [ ("NativeSourceStrictCallCaptureIgnored", "strictCaptured constantZeroNat Z", Right "0")
        , ("NativeSourceStrictCallCaptureDemanded", "strictCaptured opaqueNat (S Z)", Left "strict-capture")
        , ("NativeSourceStrictCallHead", "strictHead opaqueNat partialNat", Right "42")
        , ("NativeSourceStrictCallPredecessor", "strictPredecessor opaqueNat partialNat", Left "native-bottom-nat")
        , ("NativeSourceStrictCallIgnoredIndex", "strictIgnored constantZeroNat bottomNat", Right "0")
        , ("NativeSourceStrictCallBareIndex", "strictBare constantZeroNat bottomNat", Left "native-bottom-nat")
        ]
        $ \(moduleName, observation, expected) -> do
            expression <- sourceEntry moduleName (strictCallSource moduleName observation)
            environment <- GHC.getSession
            flags <- GHC.getSessionDynFlags
            outcome <- liftIO $ try (execute environment flags moduleName expression)
            case (expected, outcome) of
                (Right wanted, Right actual)
                    | actual == wanted -> pure ()
                    | otherwise -> fail ("FAIL: " ++ moduleName ++ "; expected " ++ wanted ++ ", got " ++ show actual)
                (Left fragment, Left exception)
                    | fragment `isInfixOf` displayException (exception :: SomeException) -> pure ()
                    | otherwise -> fail ("FAIL: " ++ moduleName ++ "; wrong exception: " ++ displayException exception)
                (Right _, Left exception) -> fail ("FAIL: " ++ moduleName ++ "; unexpected exception: " ++ displayException exception)
                (Left fragment, Right actual) -> fail ("FAIL: " ++ moduleName ++ "; expected " ++ fragment ++ ", got " ++ show actual)

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
        , ("NativeSourceCaseWildcard", 5617, "Vec", "Z -> VNil; S _ -> VCons x VNil")
        , ("NativeSourceCaseWrongLength", 83865, "Vec", "Z -> VNil; S k -> VNil")
        , ("NativeSourceCaseEscapingPredecessor", 5617, "Vec", "Z -> VNil; S k -> replicateCaseNative k x")
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
            ( "NativeSourceUnsupportedFunctionIndexDomain"
            , 83605
            , "unsupported function domain"
            , []
            ,
                [ "badFunctionIndex :: foreach (f :: Nat -> Int) -> Int"
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
sourceModuleRejects label code fragment = sourceModuleRejectsWith label [(code, fragment)]

-- | Require specified located diagnostics from one source compilation.
sourceModuleRejectsWith :: String -> [(Int, String)] -> String -> String -> GHC.Ghc ()
sourceModuleRejectsWith label diagnostics moduleName source = do
    targets <- GHC.getTargets
    result <- GHC.handleSourceError (pure . Left) $ do
        parsed <- parseSourceTarget moduleName source
        checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
        void (GHC.desugarModule checked)
        pure (Right ())
    GHC.setTargets targets
    forM_ diagnostics $ \(code, fragment) -> checkSourceRejection label code fragment result

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

{- Note [Qualified retained Pi source checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ImpredicativeTypes admits these complete qualified Pi equality operands.
Conversion must retain each constraint arrow and its predicate. A known
monomorphic variable can use its checked type without Quick Look inference.
The unsigned local alias still requires a complete signature. Other negative
head cases record current inference restrictions. They do not specify that
these source forms must always reject.
Alias checks distinguish an unused synonym parameter from an occurrence in
the expanded predicate.

The unsigned local alias has one located explicit-signature diagnostic.
The standalone matrix records its source position.
See Note [Known native variables with ImpredicativeTypes]
in GHC.Tc.Gen.App and Note [Rigid retained indices] in GHC.Tc.Utils.Unify.

Container clients load one ImpredicativeTypes provider. It imports the actual
PackedVecLibrary types. Compile the provider to object code without linking.
Native bytecode module compilation remains unsupported. Most clients disable
ImpredicativeTypes. Ground application clients enable it after these defaults.
They retain the written type applications and check the saved invisible
arguments as well as the residual type. Complete Refl witnesses use the
ordinary constructor checker. No equality axiom is added.
See Note [Ground retained applications] in GHC.Tc.Gen.App.
Known-argument clients use checked local or global types to select erased
parameters. Linear controls check that a known argument is used once.
Negative controls retain predicate and nominal equality checks. Some source
forms still stop at the Quick Look guard. They do not test later solving.
The standalone matrix owns the once-only cached-prefix trace check.
Each client checks its complete source without a local Nat or Vec declaration.
Ordinary tau controls
keep their existing inference path. Rejections record current guard and
inference boundaries; they do not establish permanent source invalidity.
The standalone matrix owns exact source positions and full pipeline checks.
See Note [Known retained function instantiation] in GHC.Tc.Gen.App.
-}

-- | Check source package restrictions before or during desugaring.
runPackageSourceRejectChecks :: GHC.Ghc ()
runPackageSourceRejectChecks = do
    forM_
        [ ("OpaqueIntField", 83605, "Its declared domain is: Int", [], [],
            declaration "forall a. foreach (n :: Int) -> Bool -> PackedVec a")
        , ("StrictData", 83605, "StrictData", ["StrictData"], [], package)
        , ("TypeData", 83605, "retained constructor", ["TypeData"], [], ["type data PackedVec a where", "  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a"])
        , ("Existential", 83605, "existential binders", [], [], declaration "forall a hidden. foreach (n :: Nat) -> Vec a n -> hidden -> PackedVec a")
        , ("Equality", 83605, "contexts", [], [], declaration "forall a. (a ~ Int) => foreach (n :: Nat) -> Vec a n -> PackedVec a")
        , ("RefinedUniversal", 83605, "equality specifications", [], [], declaration "foreach (n :: Nat) -> Vec Int n -> PackedVec Int")
        , ("Promotion", 83605, "no supported promoted field telescope", [], [],
            declaration "foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a m -> PackedVec a" ++ ["type Bad = 'PackVec"])
        , ("PatternWrongPredecessor", 5617, "rest", [], [], package ++ ["bad :: PackedVec Int -> PackedVec Int", "bad (PackVec (S k) (VCons x rest)) = PackVec (S k) rest"])
        , ("IndependentWildcards", 83865, "bad _ _ xs = xs", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m", "bad _ _ xs = xs"])
        , ("PatternIndependent", 5617, "rest", [], [], package ++ ["bad :: foreach (m :: Nat) -> PackedVec Int -> PackedVec Int", "bad m (PackVec (S k) (VCons x rest)) = PackVec m rest"])
        , ("PatternGuardScope", 83865, "xs", [], [], package ++ ["bad :: PackedVec Int -> PackedVec Int", "bad (PackVec (S k) (VCons x rest)) | False = PackVec k rest", "bad (PackVec n xs) = PackVec (S n) xs"])
        , ("PatternFieldEscape", 25897, "rest", [], [], package ++ ["bad :: PackedVec Int -> Vec Int n", "bad (PackVec (S k) (VCons x rest)) = rest"])
        , ("PatternNestedField", 83605, "bare variable patterns", [], [], package ++ ["bad :: PackedVec Int -> Int", "bad (PackVec (S (S k)) xs) = 0"])
        , ("PatternWrongDomain", 83605, "index domain", [], [], package ++ ["bad :: PackedVec Int -> Int", "bad (PackVec True xs) = 0"])
        , ("PatternWrongArity", 83605, "Retained fields require bare variables", [], [], package ++ ["bad :: PackedVec Int -> Int", "bad (PackVec (S k j) xs) = 0"])
        , ("RetainedLazyField", 83605, "Retained fields require bare variables", [], [], package ++ ["bad :: PackedVec a -> Int", "bad (PackVec ~_ xs) = 0"])
        , ("LazyPattern", 83605, "Lazy patterns and pattern bindings", [], [], package ++ ["bad :: PackedVec a -> Int", "bad ~(PackVec n xs) = 0"])
        , ("LetPattern", 83605, "Lazy patterns and pattern bindings", [], [], package ++ ["bad :: PackedVec a -> Int", "bad p = let PackVec n xs = p in 0"])
        , ("IndexMismatch", 83865, "Couldn't match", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> PackedVec Int", "bad n m xs = PackVec @Int m xs"])
        , ("IndexEscape", 83865, "Couldn't match", [], [], package ++ ["bad :: foreach (m :: Nat) -> PackedVec Int -> Vec Int m", "bad m (PackVec n xs) = xs"])
        , ("NestedIndexEscape", 5617, "VCons x xs", [], [], package ++ ["bad :: foreach (m :: Nat) -> PackedVec Int -> Vec Int m", "bad m (PackVec n (VCons x xs)) = VCons x xs"])
        , ("UnconsOriginalLength", 5617, "rest", [], [], package ++ ["bad :: forall a. PackedVec a -> Maybe (a, PackedVec a)", "bad (PackVec n xs) = case n of", "  Z -> Nothing", "  S k -> case xs of", "    VCons x rest -> Just (x, PackVec n rest)"])
        , ("UnconsIndependentLength", 5617, "rest", [], [], package ++ ["bad :: forall a. foreach (m :: Nat) -> PackedVec a -> Maybe (a, PackedVec a)", "bad m (PackVec n xs) = case n of", "  Z -> Nothing", "  S k -> case xs of", "    VCons x rest -> Just (x, PackVec m rest)"])
        , ("IndependentRefl", 83865, "Just Refl", [], equalityImports, package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad n m = Just Refl"])
        , ("ZipWithoutEquality", 83865, "zipVec xs ys", [], [], package ++ zipper ++ ["bad :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))", "bad (PackVec n xs) (PackVec m ys) = Just (PackVec n (zipVec xs ys))"])
        , ("ZipUnmatchedProof", 83865, "zipVec xs ys", [], equalityImports, package ++ equality ++ zipper ++ ["bad :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))", "bad (PackVec n xs) (PackVec m ys) = case sameNat n m of", "  Nothing -> Nothing", "  Just proof -> Just (PackVec n (zipVec xs ys))"])
        , ("ZipNothingBranch", 83865, "zipVec xs ys", [], equalityImports, package ++ equality ++ zipper ++ ["bad :: PackedVec a -> PackedVec b -> Maybe (PackedVec (a, b))", "bad (PackVec n xs) (PackVec m ys) = case sameNat n m of", "  Just Refl -> Just (PackVec n (zipVec xs ys))", "  Nothing -> Just (PackVec n (zipVec xs ys))"])
        , ("LinearIndex", 83605, "unrestricted multiplicity Many", ["LinearTypes"], [], package ++ ["bad :: Nat %1 -> PackedVec Int", "bad n = PackVec n (error \"unused\")"])
        , ("ReverseWrongLength", 83865, "reverseNative", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a (S n)", "bad n xs = reverseNative @a n xs"])
        , ("ReverseMissingStep", 5617, "reverseAppendNative", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)", "bad n m xs acc = case n of", "  Z -> case xs of VNil -> acc", "  S k -> case xs of VCons x rest -> reverseAppendNative @a k (S m) rest (VCons x acc)"])
        , ("ReverseMissingZero", 83865, "reverseAppendNative", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n", "bad n xs = reverseAppendNative @a n Z xs VNil"])
        , ("AddSuccForged", 83865, "Refl", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Add n (S m) :~: S (Add n m)", "bad n m = Refl"])
        , ("RightZeroForged", 83865, "Refl", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: foreach (n :: Nat) -> Add n Z :~: n", "bad n = Refl"])
        , ("ReverseIndexEscape", 83865, "reverseNative", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: foreach (m :: Nat) -> PackedVec Int -> Vec Int m", "bad m (PackVec n xs) = reverseNative @Int n xs"])
        , ("ReverseWrongPredecessor", 5617, "rest", ["TypeFamilies"], equalityImports, package ++ sums ++ reversal ++ ["bad :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)", "bad n m xs acc = case n of", "  Z -> case xs of VNil -> acc", "  S k -> case xs of VCons x rest -> reverseAppendNative @a (S k) m rest acc"])
        , ("LaterBang", 83605, "bare variable pattern", ["BangPatterns"], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "bad Z !_ xs ys = ys", "bad (S k) later xs ys = ys"])
        , ("LaterConstructor", 83605, "constructor patterns in both selected index columns", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "bad Z Z xs ys = ys", "bad (S k) later xs ys = ys"])
        , ("MatrixIncomplete", 83605, "each pair of constructors", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad Z (S right) = Nothing", "bad (S left) Z = Nothing"])
        , ("MatrixDuplicate", 83605, "matched by an earlier equation", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad Z (S right) = Nothing", "bad (S left) Z = Nothing", "bad (S left) (S right) = Nothing", "bad Z Z = Nothing"])
        , ("MatrixWrongDomain", 83605, "constructor patterns in both selected index columns", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z True = Nothing", "bad Z (S right) = Nothing", "bad (S left) Z = Nothing", "bad (S left) (S right) = Nothing"])
        , ("MatrixNestedField", 83605, "bare variable", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad Z (S (S right)) = Nothing", "bad (S left) Z = Nothing", "bad (S left) (S right) = Nothing"])
        , ("MatrixFalseZeroSuccessor", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad Z (S right) = Just Refl", "bad (S left) Z = Nothing", "bad (S left) (S right) = Nothing"])
        , ("MatrixFalseSuccessorZero", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad Z (S right) = Nothing", "bad (S left) Z = Just Refl", "bad (S left) (S right) = Nothing"])
        , ("MatrixMissingRecursiveEvidence", 5617, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad Z (S right) = Nothing", "bad (S left) Z = Nothing", "bad (S left) (S right) = Just Refl"])
        , ("MatrixContext", 83605, "Constraints within a retained constructor matrix", ["FlexibleContexts"], equalityImports, ["bad :: foreach (n :: Nat) -> Show Int => foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad Z (S right) = Nothing", "bad (S left) Z = Nothing", "bad (S left) (S right) = Nothing"])
        , ("MatrixFallbackNonterminal", 83605, "only one final equation", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad first second = Nothing", "bad (S left) (S right) = Nothing"])
        , ("MatrixFallbackRepeated", 83605, "only one final equation", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad first second = Nothing", "bad another final = Nothing"])
        , ("MatrixFallbackMixedWildcard", 83605, "constructor patterns in both selected index columns", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad _ Z = Nothing", "bad _ _ = Nothing"])
        , ("MatrixFallbackWildcardsFalseRefl", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad _ _ = Just Refl"])
        , ("MatrixFallbackLeftVariableFalseRefl", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad first _ = Just Refl"])
        , ("MatrixFallbackRightVariableFalseRefl", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad _ second = Just Refl"])
        , ("MatrixFallbackFalseRefl", 83865, "Just Refl", [], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Just Refl", "bad first second = Just Refl"])
        , ("MatrixFallbackWildcardsPayloadMismatch", 83865, "xs", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> PackedVec Int", "bad Z Z xs ys = PackVec Z xs", "bad (S left) (S right) xs ys = PackVec (S left) xs", "bad _ _ xs ys = PackVec Z xs"])
        , ("MatrixFallbackLeftVariablePayloadMismatch", 83865, "ys", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> PackedVec Int", "bad Z Z xs ys = PackVec Z xs", "bad (S left) (S right) xs ys = PackVec (S left) xs", "bad first _ xs ys = PackVec first ys"])
        , ("MatrixFallbackRightVariablePayloadMismatch", 83865, "xs", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> PackedVec Int", "bad Z Z xs ys = PackVec Z xs", "bad (S left) (S right) xs ys = PackVec (S left) xs", "bad _ second xs ys = PackVec second xs"])
        , ("ActionIndependentIndices", 83865, "xs", [], [], localActions ++ ["bad :: Nat -> Nat -> IO (PackedVec Int)", "bad n m = let { action :: IO (Vec Int n); action = pure (localVector n) } in action >>= \\xs -> pure (PackVec m xs)"])
        , ("ActionWrongIOMonad", 83865, "IO", [], [], localActions ++ ["bad :: Nat -> Maybe [Int]", "bad n = let { action :: IO (Vec Int n); action = pure (localVector n) } in action >>= \\xs -> Just (toListLocal xs)"])
        , ("ActionWrongMaybeMonad", 83865, "Maybe", [], [], localActions ++ ["bad :: Nat -> IO [Int]", "bad n = let { action :: Maybe (Vec Int n); action = Just (localVector n) } in do { xs <- action; pure (toListLocal xs) }"])
        , ("ActionConflictingArguments", 83865, "second", [], [], localActions ++ ["consume :: forall a. IO a -> IO a -> IO [Int]", "consume first second = pure []", "bad :: Nat -> Nat -> IO [Int]", "bad n m = let { first :: IO (Vec Int n); first = pure (localVector n); second :: IO (Vec Int m); second = pure (localVector m) } in consume first second"])
        , ("ActionLinearCapture", 18872, "multiplicity", ["LinearTypes"], [], localActions ++ ["bad :: () %1 -> Nat -> IO [Int]", "bad token n = let { action :: IO (Vec Int n); action = pure (localVector n) } in action >>= \\xs -> pure (case token of () -> toListLocal xs)"])
        , ("SourceInferenceCallbackLocalIndexEscape", 83605, "outside the application scope", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> let { k :: Nat; k = n } in pure (localVector k) } in pure 42"])
        , ("SourceInferenceCallbackInputIndexEscape", 83605, "outside the application scope", [], [], localActions ++ ["withAux :: forall a b c. IO a -> IO c -> (a -> IO b) -> IO b", "withAux action auxiliary callback = action >>= callback", "bad :: Nat -> IO Int", "bad n = let compute = withAux (pure (S Z)) (pure (localVector n)) (\\k -> pure (localVector k)) in pure 42"])
        , ("SourceInferenceIndependentIndex", 83865, "xs", [], [], localActions ++ ["bad :: Nat -> Nat -> IO (PackedVec Int)", "bad n m = let { action = pure (localVector n); compute = action >>= \\xs -> pure xs } in compute >>= \\xs -> pure (PackVec m xs)"])
        , ("SourceInferenceNativeFunctionAlias", 83605, "explicit signature", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let alias = localVector in pure 42"])
        , ("SourceInferenceGadtEscape", 46956, "escape its scope", [], [], localActions ++ ["data Hidden where Hidden :: forall a. a -> Hidden", "bad :: Nat -> Hidden -> IO Int", "bad n hidden = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> case hidden of Hidden value -> pure value } in pure 42"])
        , ("SourceInferenceLocalRhsEscape", 83605, "out of scope here", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let xs = (let { m :: Nat; m = n } in localVector m) in pure 42"])
        , ("SourceInferenceRecursiveNativeValue", 83865, "untouchable", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let xs = const (localVector n) xs in pure 42"])
        , ("SourceInferencePatternNativeValue", 83605, "explicit signature", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let (xs) = localVector n in pure 42"])
        , ("SourceInferenceGeneralizedNativeValue", 83605, "explicit signature", ["NoMonoLocalBinds"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let xs = localVector n in pure 42"])
        , ("SourceInferencePartialNativeValue", 83865, "untouchable", ["PartialTypeSignatures"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let { xs :: _; xs = localVector n } in pure 42"])
        , ("SourceInferenceCallbackLinearCapture", 18872, "multiplicity", ["LinearTypes"], [], localActions ++ ["bad :: Nat -> Int %1 -> IO [Int]", "bad n value = let { action = pure (localVector n); compute = action >>= \\xs -> const (pure xs) value } in compute >>= \\ys -> pure (toListLocal ys)"])
        , ("BoxedTupleCallbackLocalPairEscape", 83605, "outside the application scope", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> let { k :: Nat; k = n } in pure (k, localVector k) } in pure 42"])
        , ("BoxedTupleCallbackInputPairEscape", 83605, "outside the application scope", [], [], localActions ++ ["withAux :: forall a b c. IO a -> IO c -> (a -> IO b) -> IO b", "withAux action auxiliary callback = action >>= callback", "bad :: Nat -> IO Int", "bad n = let compute = withAux (pure (S Z)) (pure (localVector n)) (\\k -> pure (k, localVector k)) in pure 42"])
        , ("BoxedTupleIndependentIndex", 83865, "snd pair", [], [], localActions ++ ["bad :: Nat -> Nat -> IO (PackedVec Int)", "bad n m = let { action = pure (localVector n); compute = action >>= \\xs -> pure ((42 :: Int), xs) } in compute >>= \\pair -> pure (PackVec m (snd pair))"])
        , ("BoxedTupleLocalTupleEscape", 83605, "out of scope here", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let pair = (let { k :: Nat; k = n } in (k, localVector k)) in pure 42"])
        , ("BoxedTupleLinearCapture", 18872, "multiplicity", ["LinearTypes"], [], localActions ++ ["bad :: Nat -> Int %1 -> IO Int", "bad n value = let compute = pure (localVector n) >>= \\xs -> pure (value, xs) in compute >>= \\pair -> pure 42"])
        , ("BoxedTupleLinearDuplicate", 18872, "multiplicity", ["LinearTypes"], [], localActions ++ ["bad :: foreach (n :: Nat) -> Vec Int n %1 -> (Vec Int n, Vec Int n)", "bad n xs = let pair = (xs, xs) in pair"])
        , ("BoxedTupleGeneralizedTuple", 83605, "explicit signature", ["NoMonoLocalBinds"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let pair = ((42 :: Int), localVector n) in pure 42"])
        , ("BoxedTupleTupleFieldMismatch", 83865, "Char", [], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> pure (('x' :: Int), xs) } in pure 42"])
        , ("BoxedTupleTupleGadtEscape", 83865, "untouchable", [], [], localActions ++ ["data Hidden where Hidden :: forall a. a -> Hidden", "bad :: Nat -> Hidden -> IO Int", "bad n hidden = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> case hidden of Hidden value -> pure (value, xs) } in pure 42"])
        , ("TupleSectionCallbackLocalSectionEscape", 83605, "outside the application scope", ["TupleSections"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let { action :: IO (Vec Int n); action = pure (localVector n); compute = action >>= \\xs -> let { k :: Nat; k = n } in pure (k, localVector k,) } in pure 42"])
        , ("TupleSectionLocalSectionEscape", 83605, "out of scope here", ["TupleSections"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let section = (let { k :: Nat; k = n } in (localVector k,)) in pure 42"])
        , ("TupleSectionIndependentIndex", 83865, "fst pair", ["TupleSections"], [], localActions ++ ["bad :: Nat -> Nat -> IO (PackedVec Int)", "bad n m = let { action = pure (localVector n); compute = action >>= \\xs -> pure ((xs,) (42 :: Int)) } in compute >>= \\pair -> pure (PackVec m (fst pair))"])
        , ("TupleSectionWrongMissingIndex", 83865, "m", ["TupleSections"], [], localActions ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> (Vec Int n, Vec Int n)", "bad n m xs = (xs,)"])
        , ("TupleSectionWrongPresentIndex", 83865, "xs", ["TupleSections"], [], localActions ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Int -> (Vec Int m, Int)", "bad n m xs = (xs,)"])
        , ("TupleSectionLinearCaptureTwice", 18872, "multiplicity", ["TupleSections", "LinearTypes"], [], localActions ++ ["bad :: foreach (n :: Nat) -> Vec Int n %1 -> ((Vec Int n, Int), (Vec Int n, Int))", "bad n xs = let section = (xs,) in (section (42 :: Int), section (43 :: Int))"])
        , ("TupleSectionLinearMissingDuplicate", 18872, "multiplicity", ["TupleSections", "LinearTypes"], [], localActions ++ ["bad :: foreach (n :: Nat) -> Vec Int n %1 -> ((Int, Vec Int n), (Int, Vec Int n))", "bad n xs = let { fill :: Vec Int n %1 -> (Int, Vec Int n); fill = ((42 :: Int),) } in (fill xs, fill xs)"])
        , ("TupleSectionGeneralizedSection", 83605, "explicit signature", ["TupleSections", "NoMonoLocalBinds"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let section = (localVector n,) in pure 42"])
        , ("TupleSectionUnliftedAnchoredField", 83865, "IntRep", ["TupleSections", "MagicHash"], ["import GHC.Exts (Int#)"], localActions ++ ["unboxed :: forall a. a -> Int#", "unboxed _ = 7#", "bad :: Nat -> IO Int", "bad n = let section = (unboxed (localVector n),) in pure 42"])
        , ("TupleSectionNativePolyBoundary", 83605, "forall within the scope", ["TupleSections", "ImpredicativeTypes"], [], [] ++ ["good :: foreach (n :: Nat) -> Vec Int n -> Int -> (forall a. a -> a, Vec Int n, Int)", "good n xs = (id, xs,)"])
        , ("GenericOverapplicationWrongExtraArgument", 83865, "Char", ["TupleSections"], [], localActions ++ ["bad :: foreach (n :: Nat) -> Vec Int n -> (Vec Int n,Int)", "bad n xs = fst ((xs,),True) 'x'"])
        , ("GenericOverapplicationWrongFinalIndex", 83865, "fst", ["TupleSections"], [], localActions ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> (Vec Int m,Int)", "bad n m xs = fst ((xs,),True) (42 :: Int)"])
        , ("GenericOverapplicationSelectedNonFunction", 83865, "localVector n", ["TupleSections"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = pure (fst (localVector n,True) (42 :: Int))"])
        , ("GenericOverapplicationConflictingPrefix", 83865, "localVector m", ["TupleSections"], [], localActions ++ ["same :: forall a. a -> a -> a", "same left _ = left", "bad :: Nat -> Nat -> IO Int", "bad n m = pure (snd (same (localVector n,) (localVector m,) (42 :: Int)))"])
        , ("GenericOverapplicationAnchorOnlyAfterPrefix", 83865, "localVector n", ["TupleSections"], [], localActions ++ ["unknownResult :: forall a. Int -> a", "unknownResult _ = error \"unknown-result\"", "bad :: Nat -> IO Int", "bad n = pure (unknownResult (42 :: Int) (localVector n))"])
        , ("GenericOverapplicationCallbackLocalEscape", 83605, "outside the application scope", ["TupleSections"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let compute = pure (localVector n) >>= \\xs -> let { k :: Nat; k = n } in pure (fst ((localVector k,),True) (42 :: Int)) in pure 42"])
        , ("GenericOverapplicationLinearCaptureTwice", 18872, "multiplicity", ["TupleSections", "LinearTypes"], [], localActions ++ ["linearIdentity :: forall a. a %1 -> a", "linearIdentity value = value", "bad :: foreach (n :: Nat) -> Vec Int n %1 -> ((Vec Int n,Int),(Vec Int n,Int))", "bad n xs = let section = (xs,) in (linearIdentity section (42 :: Int),linearIdentity section (43 :: Int))"])
        , ("GenericOverapplicationHigherRankWrongExtra", 25897, "rigid type variable", ["TupleSections"], [], localActions ++ ["acceptPoly :: (forall a. a -> a) -> Int", "acceptPoly value = value 42", "bad :: Nat -> IO Int", "bad n = pure (fst (acceptPoly,localVector n) (const (42 :: Int)))"])
        , ("GenericOverapplicationLocalResultEscape", 83605, "out of scope here", ["TupleSections"], [], localActions ++ ["bad :: Nat -> IO Int", "bad n = let result = let { k :: Nat; k = n } in fst ((localVector k,),True) (42 :: Int) in pure 42"])
        , ("GenericOverapplicationMissingDictionary", 39999, "Eq NoEq", ["TupleSections"], [], localActions ++ ["selectEq :: forall a b. Eq b => (a,b) -> a", "selectEq (value,tag) = if tag == tag then value else value", "data NoEq = NoEq", "bad :: Nat -> IO Int", "bad n = pure (snd (selectEq ((localVector n,),NoEq) (42 :: Int)))"])
        , ("LeadingVTAWrongExtraArgument", 83865, "fourth argument", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: foreach (n :: Nat) -> Vec Int n -> (Vec Int n,Int)", "bad n xs = selectVTA @Bool True ((xs,),False) 'x'"])
        , ("LeadingVTAWrongFinalIndex", 83865, "Vec Int m", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> (Vec Int m,Int)", "bad n m xs = selectVTA @Bool True ((xs,),False) (42 :: Int)"])
        , ("LeadingVTAWrongTag", 83865, "second argument", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: Nat -> IO (PackedVec Int)", "bad n = pure (PackVec n (fst (selectVTA @Bool (7 :: Int) ((localVector n,),False) (42 :: Int))))"])
        , ("LeadingVTAWrongKind", 83865, "Expecting one more argument", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: Nat -> IO (PackedVec Int)", "bad n = pure (PackVec n (fst (selectVTA @Maybe True ((localVector n,),False) (42 :: Int))))"])
        , ("LeadingVTAConflictingPrefix", 83865, "untouchable", ["TupleSections", "TypeApplications"], [], localActions ++ ["chooseTagged :: forall tag value. tag -> value -> value -> value", "chooseTagged _ _ right = right", "bad :: Nat -> Nat -> IO Int", "bad n m = pure (snd (chooseTagged @Bool True (localVector n,) (localVector m,) (42 :: Int)))"])
        , ("LeadingVTACallbackLocalEscape", 83605, "outside the application scope", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: Nat -> IO Int", "bad n = let compute = pure (localVector n) >>= \\xs -> let { k :: Nat; k = n } in pure (selectVTA @Bool True ((localVector k,),False) (42 :: Int)) in pure 42"])
        , ("LeadingVTALocalResultEscape", 83605, "out of scope here", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: Nat -> IO Int", "bad n = let result = let { k :: Nat; k = n } in selectVTA @Bool True ((localVector k,),False) (42 :: Int) in pure 42"])
        , ("LeadingVTAMissingDictionary", 39999, "Eq NoEq", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectEq :: forall tag. Eq tag => forall value. tag -> (value,Bool) -> value", "selectEq tag (value,_) = if tag == tag then value else value", "data NoEq = NoEq", "bad :: Nat -> IO Int", "bad n = pure (snd (selectEq @NoEq NoEq ((localVector n,),False) (42 :: Int)))"])
        , ("LeadingVTALinearCaptureTwice", 18872, "multiplicity", ["TupleSections", "TypeApplications", "LinearTypes"], [], localActions ++ ["selectLinear :: forall tag value. tag -> value %1 -> value", "selectLinear _ value = value", "bad :: foreach (n :: Nat) -> Vec Int n %1 -> ((Vec Int n,Int),(Vec Int n,Int))", "bad n xs = let section = (xs,) in (selectLinear @Bool True section (42 :: Int),selectLinear @Bool True section (43 :: Int))"])
        , ("LeadingVTAHigherRankWrongExtra", 25897, "rigid type variable", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectFirst :: forall tag a b. tag -> (a,b) -> a", "selectFirst _ (first,_) = first", "acceptPoly :: (forall a. a -> a) -> Int", "acceptPoly value = value 42", "bad :: Nat -> IO Int", "bad n = pure (selectFirst @Bool True (acceptPoly,localVector n) (const (42 :: Int)))"])
        , ("LeadingVTAInterleavedVTA", 83865, "untouchable", ["TupleSections", "TypeApplications"], [], localActions ++ ["interleaved :: forall tag. tag -> forall value. (value,Bool) -> value", "interleaved _ (value,_) = value", "bad :: Nat -> IO (PackedVec Int)", "bad n = pure (PackVec n (fst (interleaved @Bool True @_ ((localVector n,),False) (42 :: Int))))"])
        , ("LeadingVTAGeneratedTypeArgWrongExtra", 18872, "selecting the field", ["TupleSections", "TypeApplications", "OverloadedRecordDot"], [], ["data Holder = Holder { field :: Int -> Int }", "bad :: Int", "bad = (Holder (+1)).field True"])
        , ("LeadingVTAQuickLookGate", 83605, "Quick Look inference", ["TupleSections", "TypeApplications", "ImpredicativeTypes"], [], ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: foreach (n :: Nat) -> Vec Int n -> (Vec Int n,Int)", "bad n xs = selectVTA @Bool True ((xs,),False) (42 :: Int)"])
        , ("LeadingVTAInferredPrefixAnnotation", 83865, "'x' :: Bool", ["TupleSections", "TypeApplications"], [], localActions ++ ["selectVTA :: forall tag value. tag -> (value,Bool) -> value", "selectVTA _ (value,_) = value", "bad :: Nat -> IO (PackedVec Int)", "bad n = pure (PackVec n (fst (selectVTA @Bool True ((localVector n,),('x' :: Bool)) (42 :: Int))))"])
        , ("IndexVTAWrongElementTypeArgument", 83865, "Vec Bool", ["TypeApplications"], [], localActions ++ ["observe :: forall a (index :: Nat). Vec a index -> Int", "observe _ = 42", "bad :: Nat -> IO Int", "bad n = pure (observe @Bool (localVector n))"])
        , ("IndexVTAWrongKind", 83865, "Expecting one more argument", ["TypeApplications"], [], localActions ++ ["observe :: forall a (index :: Nat). Vec a index -> Int", "observe _ = 42", "bad :: Nat -> IO Int", "bad n = pure (observe @Maybe (localVector n))"])
        , ("IndexVTAConflictingIndices", 83865, "untouchable", ["TypeApplications"], [], localActions ++ ["observeSame :: forall a (index :: Nat). Vec a index -> Vec a index -> Int", "observeSame _ _ = 42", "bad :: Nat -> Nat -> IO Int", "bad n m = pure (observeSame @Int (localVector n) (localVector m))"])
        , ("IndexVTAWrongResultIndex", 83865, "Vec Int m", ["TypeApplications"], [], localActions ++ ["keepIndex :: forall a (index :: Nat). Vec a index -> Vec a index", "keepIndex xs = xs", "bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m", "bad n m xs = keepIndex @Int xs"])
        , ("IndexVTAMissingEqualityDictionary", 39999, "Eq NoEq", ["TypeApplications"], [], localActions ++ ["data NoEq = NoEq", "observeEq :: forall a (index :: Nat). Eq a => Vec a index -> Int", "observeEq _ = 42", "bad :: foreach (n :: Nat) -> Vec NoEq n -> Int", "bad n xs = observeEq @NoEq xs"])
        , ("IndexVTACallbackLocalIndexEscape", 83605, "outside the application scope", ["TypeApplications"], [], localActions ++ ["keepIndex :: forall a (index :: Nat). Vec a index -> Vec a index", "keepIndex xs = xs", "bad :: Nat -> IO Int", "bad n = let compute = pure (localVector n) >>= \\xs -> let { k :: Nat; k = n } in pure (keepIndex @Int (localVector k)) in pure 42"])
        , ("IndexVTALocalResultIndexEscape", 83605, "out of scope here", ["TypeApplications"], [], localActions ++ ["keepIndex :: forall a (index :: Nat). Vec a index -> Vec a index", "keepIndex xs = xs", "bad :: Nat -> IO Int", "bad n = let result = let { k :: Nat; k = n } in keepIndex @Int (localVector k) in pure 42"])
        , ("IndexVTALinearCaptureTwice", 18872, "multiplicity", ["TypeApplications", "LinearTypes"], [], localActions ++ ["linearKeep :: forall a (index :: Nat). Vec a index %1 -> Vec a index", "linearKeep xs = xs", "bad :: foreach (n :: Nat) -> Vec Int n %1 -> (Vec Int n, Vec Int n)", "bad n xs = (linearKeep @Int xs, linearKeep @Int xs)"])
        , ("IndexVTAInterleavedTypeApplicationBoundary", 83865, "untouchable", ["TypeApplications"], [], localActions ++ ["interleavedIndex :: forall a. a -> forall (index :: Nat). Vec a index -> Int", "interleavedIndex _ _ = 42", "bad :: Nat -> IO Int", "bad n = pure (interleavedIndex @Int (0 :: Int) @_ (localVector n))"])
        , ("IndexVTAWrongClosedIndexArgument", 83865, "third argument", ["TypeApplications"], [], localActions ++ ["observe :: forall a (index :: Nat). Vec a index -> Int", "observe _ = 42", "bad :: Nat -> IO Int", "bad n = pure (observe @Int @Z (localVector n))"])
        , ("IndexVTACallbackInputIndexEscape", 83605, "outside the application scope", ["TypeApplications"], [], localActions ++ ["keepIndex :: forall a (index :: Nat). Vec a index -> Vec a index", "keepIndex xs = xs", "withAux :: forall a b c. IO a -> IO c -> (a -> IO b) -> IO b", "withAux action auxiliary callback = action >>= callback", "bad :: Nat -> IO Int", "bad n = let compute = withAux (pure (S Z)) (pure (localVector n)) (\\k -> pure (keepIndex @Int (localVector k))) in pure 42"])
        , ("IndexVTAWrongRemainingOrdinaryArgument", 83865, "Char", ["TypeApplications"], [], localActions ++ ["inspectTagged :: forall a (index :: Nat) tag. (tag, Vec a index) -> tag -> Int", "inspectTagged _ _ = 42", "bad :: Nat -> IO Int", "bad n = pure (inspectTagged @Int (True, localVector n) 'x')"])
        , ("JointPrefixConflictingNativeIndexChoices", 83865, "untouchable", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectSameIndex :: forall a (index :: Nat) value. Vec a index -> Vec a index -> value -> value", "selectSameIndex _ _ value = value", "bad :: Nat -> Nat -> IO Int", "bad n m = pure (selectSameIndex @Int (localVector n) (localVector m) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixIndependentPayloadResultMismatch", 83865, "Vec Int n", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int n", "bad n m xs ys = selectIndex @Int xs ys"])
        , ("JointPrefixUndeterminedNativeIndex", 83865, "untouchable", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectUnknownIndex :: forall a (index :: Nat) (unknown :: Nat) value. Vec a index -> value -> value", "selectUnknownIndex _ value = value", "bad :: Nat -> IO Int", "bad n = pure (selectUnknownIndex @Int (localVector n) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixNativeAnchorOnlyInExtraArgument", 83865, "untouchable", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "zeroVector :: Vec Int Z", "zeroVector = VNil", "bad :: Nat -> IO (PackedVec Int)", "bad n = pure (PackVec n (selectIndex @Int zeroVector id (localVector n)))"])
        , ("JointPrefixWrongExtraArgument", 83865, "fourth argument", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Int", "bad n = pure (selectIndex @Int (localVector n) ((+ 1) :: Int -> Int) True)"])
        , ("JointPrefixWrongFinalResult", 83865, "Bool", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Bool", "bad n = pure (selectIndex @Int (localVector n) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixWrongElementTypeArgument", 83865, "Vec Bool", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Int", "bad n = pure (selectIndex @Bool (localVector n) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixWrongTypeArgumentKind", 83865, "Expecting one more argument", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Int", "bad n = pure (selectIndex @Maybe (localVector n) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixMissingEqualityDictionary", 39999, "Eq NoEq", ["TypeApplications", "TupleSections"], [], localActions ++ ["data NoEq = NoEq", "selectEqIndex :: forall a. Eq a => forall (index :: Nat) value. Vec a index -> value -> value", "selectEqIndex _ value = value", "bad :: foreach (n :: Nat) -> Vec NoEq n -> Int", "bad n xs = selectEqIndex @NoEq xs ((+ 1) :: Int -> Int) 42"])
        , ("JointPrefixLinearPayloadReuse", 18872, "multiplicity", ["TypeApplications", "TupleSections", "LinearTypes"], [], localActions ++ ["selectLinearIndex :: forall a (index :: Nat) value. Vec a index -> value %1 -> value", "selectLinearIndex _ value = value", "bad :: foreach (n :: Nat) -> Vec Int n -> Vec Int n %1 -> ((Vec Int n, Int), (Vec Int n, Int))", "bad n witness xs = (selectLinearIndex @Int witness (xs,) (42 :: Int), selectLinearIndex @Int witness (xs,) (43 :: Int))"])
        , ("JointPrefixLocalResultIndexEscape", 83605, "out of scope here", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Int", "bad n = let result = let { k :: Nat; k = n } in selectIndex @Int (localVector n) (localVector k,) (42 :: Int) in pure 42"])
        , ("JointPrefixCallbackResultIndexEscape", 83605, "outside the application scope", ["TypeApplications", "TupleSections"], [], localActions ++ ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: Nat -> IO Int", "bad n = let compute = pure (localVector n) >>= \\xs -> let { k :: Nat; k = n } in pure (selectIndex @Int xs (localVector k,) (42 :: Int)) in pure 42"])
        , ("JointPrefixInterleavedTypeApplicationBoundary", 83865, "untouchable", ["TypeApplications", "TupleSections"], [], localActions ++ ["interleavedIndex :: forall a. a -> forall (index :: Nat) value. Vec a index -> value -> value", "interleavedIndex _ _ value = value", "bad :: Nat -> IO Int", "bad n = pure (interleavedIndex @Int (0 :: Int) @_ @_ (localVector n) ((+ 1) :: Int -> Int) 42)"])
        , ("JointPrefixQuickLookBoundary", 83605, "Quick Look inference", ["TypeApplications", "TupleSections", "ImpredicativeTypes"], [], ["selectIndex :: forall a (index :: Nat) value. Vec a index -> value -> value", "selectIndex _ value = value", "bad :: foreach (n :: Nat) -> Vec Int n -> (Vec Int n, Int)", "bad n xs = selectIndex @Int xs (xs,) (42 :: Int)"])
        , ("PiGivenNoGivenRetainedPi", 83865, "Couldn't match", ["TypeFamilies"], [], ["transportWithoutProof :: forall a b. (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)", "transportWithoutProof f = f"])
        , ("RetainedDomainReductionDepth", 40404, "Reduction stack overflow", ["TypeFamilies", "UndecidableInstances"], ["import Data.Kind (Type)"], ["data Token (a :: Type) = Token", "type family Loop (a :: Type) :: Type where Loop a = Loop a", "type A = Token (Loop Bool)", "type B = Token Bool", "data PA (n :: A) = PA", "data PB (n :: B) = PB", "bad :: (foreach (n :: A) -> PA n) -> foreach (n :: B) -> PB n", "bad original = original"])
        , ("PiGivenUnrelatedGivenCannotTransport", 5617, "Could not deduce", ["TypeFamilies"], [], ["transportWrongProof :: forall a b c. a ~ b => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec c n)", "transportWrongProof f = f"])
        , ("PiGivenBranchGivenCannotEscape", 83865, "Couldn't match", ["TypeFamilies"], ["import Data.Type.Equality ((:~:)(Refl))"], ["transportMaybe :: forall a b. Maybe (a :~: b) -> (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)", "transportMaybe proof f = case proof of { Just Refl -> f; Nothing -> f }"])
        , ("PiGivenRepresentationalEvidenceIsNotNominal", 5617, "Could not deduce", ["TypeFamilies"], ["import Data.Coerce (Coercible)"], ["transportCoercible :: forall a b. Coercible a b => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)", "transportCoercible f = f"])
        , ("PiGivenOuterProofCannotIdentifyInnerIndex", 5617, "Could not deduce", ["TypeFamilies"], [], ["wrongInner :: forall a. foreach (k :: Nat) -> (a ~ Vec Int k) => (foreach (n :: Nat) -> (a, Vec Bool n)) -> (foreach (n :: Nat) -> (Vec Int n, Vec Bool n))", "wrongInner k f = f"])
        , ("PiGivenNativeFamilyOccursCycleCannotTransport", 5617, "Could not deduce", ["TypeFamilies"], [], ["type family Cycle a (n :: Nat)", "wrongCycle :: forall a. foreach (k :: Nat) -> (a ~ Maybe (Cycle a k)) => (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec Bool n)", "wrongCycle k f = f"])
        , ("PiOperandMissingRetainedPiProof", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts"], [], ["withoutPiProof :: forall value. value -> (foreach (n :: Nat) -> Vec Int n)", "withoutPiProof f = f"])
        , ("PiOperandUnrelatedProofCannotExposePi", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts", "AllowAmbiguousTypes"], [], ["fromWrongPi :: forall left right. left ~ (foreach (n :: Nat) -> Vec Int n) => right -> (foreach (n :: Nat) -> Vec Int n)", "fromWrongPi f = f"])
        , ("PiOperandBranchProofCannotEscape", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts"], ["import Data.Type.Equality ((:~:)(Refl))"], ["fromScopedProof :: forall value. Bool -> (value :~: (foreach (n :: Nat) -> Vec Int n)) -> value -> (foreach (n :: Nat) -> Vec Int n)", "fromScopedProof flag proof f = case flag of { True -> case proof of { Refl -> f }; False -> f }"])
        , ("PiOperandFamilyCycleCannotProveUnrelatedPi", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts"], [], ["type family Cycle value (index :: Nat)", "wrongCycle :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Vec (Cycle value k) n) => value -> (foreach (n :: Nat) -> Vec Bool n)", "wrongCycle k f = f"])
        , ("PiOperandDistinctOuterRuntimeIndices", 5617, "Could not deduce", ["TypeFamilies", "FlexibleContexts"], [], ["wrongOuterPi :: forall value. foreach (k :: Nat) -> foreach (m :: Nat) -> value ~ (foreach (n :: Nat) -> (Vec Int k, Vec Bool n)) => value -> (foreach (n :: Nat) -> (Vec Int m, Vec Bool n))", "wrongOuterPi k m f = f"])
        , ("PiOperandRepresentationalEvidenceCannotSupplyNominalProof", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts", "AllowAmbiguousTypes"], ["import Data.Coerce (Coercible)"], ["wrongRolePi :: forall left right. (Coercible left right, left ~ (foreach (n :: Nat) -> Vec Int n)) => right -> (foreach (n :: Nat) -> Vec Int n)", "wrongRolePi f = f"])
        , ("PiOperandLinearOuterIndexCaptureRejected", 83605, "already-bound unrestricted local value", ["TypeFamilies", "FlexibleContexts", "LinearTypes"], [], ["consumeNat :: Nat %1 -> Int", "consumeNat Z = 42", "consumeNat (S rest) = consumeNat rest", "bad :: Nat %1 -> Int", "bad k = let { keep :: forall value. value ~ (foreach (n :: Nat) -> (Vec Int k, Vec Bool n)) => value -> value; keep f = f } in consumeNat k"])
        , ("PiOperandUnsupportedPiDomainRejected", 83605, "A retained value argument requires a lifted type parameter or a supported algebraic type.", ["TypeFamilies", "FlexibleContexts"], [], ["newtype Wrapped = Wrapped Int", "bad :: forall value. value ~ (foreach (n :: Wrapped) -> Int) => value -> Int -> Int", "bad f = f"])
        , ("PiOperandPartialPiOperandRejected", 83605, "complete type signature", ["TypeFamilies", "FlexibleContexts", "PartialTypeSignatures"], [], ["bad :: forall value. value ~ (foreach (n :: Nat) -> Vec _ n) => value -> (foreach (n :: Nat) -> Vec Int n)", "bad f = f"])
        , ("PiOperandQuotedPiOperandCaptureRejected", 83605, "Template Haskell", ["TypeFamilies", "FlexibleContexts", "TemplateHaskell"], ["import Language.Haskell.TH (Q, Exp)"], ["bad :: Nat -> Q Exp", "bad k = [| let { keep :: forall value. value ~ (foreach (n :: Nat) -> (Vec Int k, Vec Bool n)) => value -> value; keep f = f } in (42 :: Int) |]"])
        , ("PiOperandEqualityOperandKindMismatch", 83865, "Expected kind", ["TypeFamilies", "FlexibleContexts"], [], ["bad :: forall (value :: Nat). value ~ (foreach (n :: Nat) -> Vec Int n) => Int", "bad = 42"])
        , ("PiOperandErasedForallArgumentUnchanged", 91510, "Illegal polymorphic type", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["bad :: Maybe (forall a. a -> a) -> Int", "bad _ = 42"])
        , ("PiOperandPiKindUnchanged", 43582, "not supported in this type position", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], ["import Data.Proxy (Proxy(..))"], ["bad :: forall (a :: foreach (n :: Nat) -> Vec Int n). Proxy a", "bad = Proxy"])
        , ("PiOperandRecursivePiCannotProveUnrelated", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts"], [], ["bad :: forall value. value ~ (foreach (n :: Nat) -> Vec value n) => value -> (foreach (n :: Nat) -> Vec Bool n)", "bad f = f"])
        , ("PiOperandNamedHoleInsidePiArgument", 83605, "must be complete", ["TypeApplications", "NamedWildCards"], [], ["bad :: Maybe (foreach (n :: Nat) -> Vec Int n) -> Maybe (foreach (n :: Nat) -> Vec Int n)", "bad f = id @(Maybe (foreach (n :: Nat) -> Vec _a n)) f"])
        , ("PiOperandAnonymousHoleBesidePiArgument", 83605, "must be complete", ["TypeApplications", "NamedWildCards"], [], ["bad :: Either Int (foreach (n :: Nat) -> Vec Int n) -> Either Int (foreach (n :: Nat) -> Vec Int n)", "bad f = id @(Either _ (foreach (n :: Nat) -> Vec Int n)) f"])
        , ("PiOperandSplicedHoleBesidePiArgument", 83605, "must be complete", ["TypeApplications", "TemplateHaskell"], ["import Language.Haskell.TH (Type(WildCardT))"], ["bad :: Either Int (foreach (n :: Nat) -> Vec Int n) -> Either Int (foreach (n :: Nat) -> Vec Int n)", "bad f = id @(Either $(pure WildCardT) (foreach (n :: Nat) -> Vec Int n)) f"])
        , ("PiOperandRecursivePiCannotRewriteBeneathBinder", 5617, "Could not deduce", ["TypeFamilies", "FlexibleContexts"], [], ["bad :: forall value. value ~ (foreach (n :: Nat) -> Vec value n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Vec Bool n), Vec Bool m))", "bad f = f"])
        , ("PiOperandSynonymPiCannotSupplyMissingProof", 91028, "Cannot equate type variable", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["type Pi = foreach (n :: Nat) -> Vec Int n", "bad :: forall value. value -> Pi", "bad f = f"])
        , ("PiOperandPiKindViaAlias", 91028, "Cannot instantiate unification variable", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], ["import Data.Proxy (Proxy(..))"], ["type Pi = foreach (n :: Nat) -> Vec Int n", "bad :: forall (a :: Pi). Proxy a", "bad = Proxy"])
        , ("PiVTAIndependentRuntimeIndexMismatch", 83865, "Couldn't match", ["TypeApplications"], [], localActions ++ ["bad :: Nat -> Nat -> Int", "bad k m = length (toListLocal (id @(Vec Int k) (localVector m)))"])
        , ("PiVTALexicalShadowRejectsOuter", 83865, "Couldn't match", ["TypeApplications"], [], localActions ++ ["bad :: Nat -> Nat -> Int", "bad k input = let { outer :: Vec Int k; outer = localVector k; apply :: Nat -> Int; apply k = length (toListLocal (id @(Vec Int k) outer)) } in apply input"])
        , ("PiVTASplicedWildcardCannotCaptureIndex", 83605, "must be complete", ["TypeApplications", "TemplateHaskell"], ["import Language.Haskell.TH (Type(WildCardT))"], localActions ++ ["bad :: Nat -> Int", "bad k = length (toListLocal (id @(Vec $(pure WildCardT) k) (localVector k)))"])
        , ("PiVTAWrongRuntimeIndexKind", 83865, "Expected kind", ["TypeApplications"], [], localActions ++ ["bad :: Int -> Int", "bad k = length (toListLocal (id @(Vec Int k) VNil))"])
        , ("PiVTAGlobalTermIsNotCaptured", 76037, "globalIndex", ["TypeApplications"], [], localActions ++ ["globalIndex :: Nat", "globalIndex = Z", "bad :: Int", "bad = length (toListLocal (id @(Vec Int globalIndex) VNil))"])
        , ("PiVTAUnavailableTermIsNotCaptured", 76037, "unavailableIndex", ["TypeApplications"], [], localActions ++ ["bad :: Int", "bad = length (toListLocal (id @(Vec Int unavailableIndex) VNil))"])
        , ("PiVTAPartialNativeTypeArgument", 83605, "must be complete", ["TypeApplications"], [], localActions ++ ["bad :: Nat -> Int", "bad k = length (toListLocal (id @(Vec _ k) (localVector k)))"])
        , ("PiVTANamedWildcardIsNotCaptured", 83865, "Couldn't match", ["TypeApplications", "NamedWildCards"], [], localActions ++ ["bad :: Nat -> Int", "bad _k = length (toListLocal (id @(Vec Int _k) (localVector _k)))"])
        , ("PiVTAQuotationRuntimeCapture", 83605, "Template Haskell", ["TypeApplications", "TemplateHaskell"], ["import Language.Haskell.TH (Q, Exp)"], ["bad :: Nat -> Q Exp", "bad k = [| id @(Vec Int k) undefined |]"])
        , ("PiVTALinearRuntimeIndexRejected", 83605, "visible type argument", ["TypeApplications", "LinearTypes"], [], localActions ++ ["bad :: Nat %1 -> Int", "bad k = length (toListLocal (id @(Vec Int k) (localVector k)))"])
        , ("LocalSignatureWrongIndex", 83865, "localVector m", [], [], localSignatures ++ ["bad :: Nat -> Nat -> PackedVec Int", "bad n m = let { xs :: Vec Int n; xs = localVector m } in PackVec n xs"])
        , ("LocalSignatureAliasDistinctIndex", 5617, "localVector m", [], [], localSignatures ++ ["bad :: Nat -> Nat -> PackedVec Int", "bad n other = let m = other in let { xs :: Vec Int n; xs = localVector m } in PackVec n xs"])
        , ("LocalSignatureSameGroup", 83605, "already-bound unrestricted local value", [], [], localSignatures ++ ["bad :: Nat -> PackedVec Int", "bad outer = let { n :: Nat; n = outer; xs :: Vec Int n; xs = localVector n } in PackVec n xs"])
        , ("LocalSignatureGlobalIndex", 25897, "localVector globalIndex", [], [], localSignatures ++ ["globalIndex :: Nat", "globalIndex = Z", "bad :: PackedVec Int", "bad = let { xs :: Vec Int globalIndex; xs = localVector globalIndex } in PackVec globalIndex xs"])
        , ("LocalSignatureWrongIndexKind", 83865, "Expected kind", [], [], localSignatures ++ ["bad :: Int -> PackedVec Int", "bad n = let { xs :: Vec Int n; xs = VNil } in PackVec Z xs"])
        , ("LocalSignatureLinearIndex", 83605, "already-bound unrestricted local value", ["LinearTypes"], [], localSignatures ++ ["bad :: Nat %1 -> PackedVec Int", "bad n = let { xs :: Vec Int n; xs = localVector n } in PackVec n xs"])
        , ("LocalSignatureAnonymousPartial", 83605, "complete type signature", ["PartialTypeSignatures"], [], localSignatures ++ ["bad :: Nat -> PackedVec Int", "bad n = let { xs :: Vec _ n; xs = localVector n } in PackVec n xs"])
        , ("LocalSignatureUnderscoreNamedHole", 83865, "localVector _n", ["NamedWildCards", "PartialTypeSignatures"], [], localSignatures ++ ["bad :: Nat -> PackedVec Int", "bad _n = let { xs :: Vec Int _n; xs = localVector _n } in PackVec _n xs"])
        , ("LocalSignatureNamedPartial", 83605, "complete type signature", ["PartialTypeSignatures", "NamedWildCards"], [], localSignatures ++ ["bad :: Nat -> PackedVec Int", "bad n = let { xs :: Vec _element n; xs = localVector n } in PackVec n xs"])
        , ("LocalSignatureTypedQuotation", 83605, "Template Haskell", ["TemplateHaskellQuotes"], ["import Language.Haskell.TH.Syntax (Code, Q)"], localSignatures ++ ["bad :: Nat -> Code Q (PackedVec Int)", "bad n = [|| let { xs :: Vec Int n; xs = localVector n } in PackVec n xs ||]"])
        , ("LocalSignatureUntypedQuotation", 83605, "Template Haskell", ["TemplateHaskellQuotes"], ["import Language.Haskell.TH.Syntax (Exp, Q)"], localSignatures ++ ["bad :: Nat -> Q Exp", "bad n = [| let { xs :: Vec Int n; xs = localVector n } in PackVec n xs |]"])
        , ("MatrixFallbackPayloadMismatch", 83865, "xs", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> PackedVec Int", "bad Z Z xs ys = PackVec Z xs", "bad (S left) (S right) xs ys = PackVec (S left) xs", "bad first second xs ys = PackVec second xs"])
        , ("QualifiedPiMissingProof", 91028, "Cannot equate type variable", qualifiedPiExtensions, [], ["withoutQualifiedProof :: forall value. value -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "withoutQualifiedProof f = f"])
        , ("QualifiedPiRecursiveAssumption", 91028, "Cannot equate type variable", qualifiedPiExtensions, [], ["type family Cycle value (index :: Nat)", "wrongQualifiedCycle :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Eq Int => Vec (Cycle value k) n) => value -> (foreach (n :: Nat) -> Eq Int => Vec Bool n)", "wrongQualifiedCycle k f = f"])
        , ("QualifiedPiKnownNativeLocalAlias", 83605, "requires an explicit signature", qualifiedPiExtensions, [], ["localKnown :: (foreach (n :: Nat) -> Eq Int => Vec Int n) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "localKnown f = let saved = f in saved"])
        , ("QualifiedPiUnrelatedGiven", 91028, "Cannot equate type variable", qualifiedPiExtensions, [], ["unrelated :: forall value other. other ~ (foreach (n :: Nat) -> Eq Int => Vec Int n) => value -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "unrelated f = f"])
        , ("QualifiedPiRepresentationalGiven", 91028, "Cannot equate type variable", qualifiedPiExtensions, ["import Data.Coerce (Coercible)"], ["representation :: forall value. Coercible value (foreach (n :: Nat) -> Eq Int => Vec Int n) => value -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "representation f = f"])
        , ("QualifiedPiPredicateMismatch", 5617, "Could not deduce", qualifiedPiExtensions, [], ["wrongDictionary :: forall value. value ~ (foreach (n :: Nat) -> Eq Int => Vec Int n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Eq Bool => Vec Int n), Vec Bool m))", "wrongDictionary f = f"])
        , ("MatrixFallbackUntypedQuote", 83605, "Constructor equations with later retained arguments", ["TemplateHaskellQuotes"], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad (S left) (S right) = Nothing", "bad first second = let ignored = [| 42 :: Int |] in Nothing"])
        , ("MatrixFallbackTypedQuote", 83605, "Constructor equations with later retained arguments", ["TemplateHaskellQuotes"], equalityImports, ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)", "bad Z Z = Nothing", "bad (S left) (S right) = Nothing", "bad first second = let ignored = [|| 42 :: Int ||] in Nothing"])
        , ("LaterUntypedQuote", 83605, "Constructor equations with later retained arguments", ["TemplateHaskellQuotes"], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "bad Z right xs ys = let ignored = [| 42 :: Int |] in ys", "bad (S k) later xs ys = ys"])
        , ("LaterTypedQuote", 83605, "Constructor equations with later retained arguments", ["TemplateHaskellQuotes"], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "bad Z right xs ys = let ignored = [|| 42 :: Int ||] in ys", "bad (S k) later xs ys = ys"])
        , ("LaterWrongResult", 5617, "xs", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "bad Z right xs ys = xs", "bad (S k) later xs ys = ys"])
        , ("LaterIndependentIndex", 5617, "ys", [], [], package ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> foreach (j :: Nat) -> Vec Int n -> Vec Int m -> Vec Int j -> Vec Int j", "bad Z first second xs ys zs = zs", "bad (S k) later final xs ys zs = ys"])
        , ("LaterForgedSuccessor", 5617, "Refl", ["TypeFamilies"], equalityImports, package ++ sums ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Add n (S m) :~: S (Add n m)", "bad Z right = Refl", "bad (S predecessor) later = Refl"])
        , ("SumForged", 83865, "Refl", ["TypeFamilies"], equalityImports, sums ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> foreach (p :: Nat) -> Sum n m", "bad n m p = Sum p Refl"])
        , ("SumWrongEvidence", 83865, "proof", ["TypeFamilies"], equalityImports, sums ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> foreach (p :: Nat) -> (p :~: n) -> Sum n m", "bad n m p proof = Sum p proof"])
        , ("AppendUnmatchedProof", 83865, "appendNative n m xs ys", ["TypeFamilies"], equalityImports, package ++ sums ++ ["bad :: PackedVec a -> PackedVec a -> PackedVec a", "bad (PackVec n xs) (PackVec m ys) = case sumNat n m of", "  Sum p proof -> PackVec p (appendNative n m xs ys)"])
        , ("SumSwapped", 5617, "Sum p Refl", ["TypeFamilies"], equalityImports, sums ++ ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum m n", "bad n m = case sumNat n m of", "  Sum p Refl -> Sum p Refl"])
        , ("SumEscape", 5617, "replicateNative p 42", ["TypeFamilies"], equalityImports, sums ++ ["bad :: foreach (q :: Nat) -> Sum n m -> Vec Int q", "bad q (Sum p Refl) = replicateNative p 42"])
        , ("SumAliasWrongEvidence", 83865, "alias m Refl", ["TypeFamilies"], equalityImports, sums ++ ["alias :: forall (n :: Nat) (m :: Nat). foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m", "alias p proof = Sum p proof", "bad :: foreach (m :: Nat) -> Sum (S Z) m", "bad m = alias m Refl"])
        , ("SumPartial", 83865, "Sum m", ["TypeFamilies"], equalityImports, sums ++ ["bad :: foreach (m :: Nat) -> (m :~: m) -> Sum Z m", "bad m = Sum m"])
        , ("LinearClause", 18872, "arising from a pattern", ["LinearTypes"], [], package ++ ["bad :: PackedVec a %1 -> PackedVec a", "bad (PackVec n xs) = PackVec n xs"])
        , ("LinearCase", 18872, "arising from multiplicity", ["LinearTypes"], [], package ++ ["bad :: PackedVec a %1 -> PackedVec a", "bad p = case p of PackVec n xs -> PackVec n xs"])
        , ("RigidMultiplicity", 25897, "is a rigid type variable", ["LinearTypes"], ["import GHC.Types (Multiplicity)"], package ++ ["bad :: forall (p :: Multiplicity) a. PackedVec a %p -> PackedVec a", "bad (PackVec n xs) = PackVec n xs"])
        , ("FilterWrongLength", 5617, "PackVec m (VCons x ys)", [], [], package ++ ["filterVec :: forall a n. (a -> Bool) -> Vec a n -> PackedVec a", "filterVec predicate VNil = PackVec Z VNil", "filterVec predicate (VCons x xs) =", "  if predicate x", "    then case filterVec predicate xs of", "      PackVec m ys -> PackVec m (VCons x ys)", "    else filterVec predicate xs"])
        , ("BoundedVectorLength", 5617, "VCons x ys", [], [], bounded "PackBounded len (VCons x ys) (weakenRight proof)" dropBound)
        , ("BoundedProofLength", 5617, "weakenRight proof", [], [], bounded "PackBounded (S len) (VCons x ys) (weakenRight proof)" dropBound)
        , ("BoundedUpperBound", 25897, "PackBounded len ys proof", [], [], bounded keepBound "PackBounded len ys proof")
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
    -- Check accepted source forms and immutable proof transport without adding output.
    forM_
        [ ("OrdinaryArrowGiven", ["TypeFamilies", "FlexibleContexts"], [], ["ordinaryArrow :: forall value. value ~ (Int -> Int) => value -> Int -> Int", "ordinaryArrow f = f"])
        , ("LaterOrdinaryPattern", [], [], package ++ ["good :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> Vec Int m", "good Z right VNil ys = ys", "good (S k) later xs ys = ys"])
        , ("LocalSignatureAliasIndex", [], [], localSignatures ++ ["good :: Nat -> PackedVec Int", "good n = let m = n in let { xs :: Vec Int n; xs = localVector m } in PackVec n xs"])
        , ("IdenticalRetainedPi", ["TypeFamilies", "FlexibleContexts"], [], ["identicalPi :: (foreach (n :: Nat) -> Vec Int n) -> (foreach (n :: Nat) -> Vec Int n)", "identicalPi f = f"])
        , ("GivenRetainedPiRhs", ["TypeFamilies", "FlexibleContexts"], [], ["fromGivenPi :: forall value. value ~ (foreach (n :: Nat) -> Vec Int n) => value -> (foreach (n :: Nat) -> Vec Int n)", "fromGivenPi f = f"])
        , ("ReflRetainedPiRhs", ["TypeFamilies", "FlexibleContexts"], ["import Data.Type.Equality ((:~:)(Refl))"], ["fromReflPi :: forall value. (value :~: (foreach (n :: Nat) -> Vec Int n)) -> value -> (foreach (n :: Nat) -> Vec Int n)", "fromReflPi proof f = case proof of Refl -> f"])
        , ("PiGivenGadtGivenBeforePi", ["TypeFamilies"], ["import Data.Type.Equality ((:~:)(Refl))"], ["transportRefl :: forall a b. (a :~: b) -> (foreach (n :: Nat) -> Vec a n) -> (foreach (n :: Nat) -> Vec b n)", "transportRefl Refl f = f"])
        , ("GivenOuterIndexRetainedPiRhs", ["TypeFamilies", "FlexibleContexts"], [], ["fromOuterGivenPi :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Vec (Vec Int k) n) => value -> (foreach (n :: Nat) -> Vec (Vec Int k) n)", "fromOuterGivenPi k f = f"])
        , ("SymmetricRetainedPiOperand", ["TypeFamilies", "FlexibleContexts"], [], ["fromSymmetricPi :: forall value. (foreach (n :: Nat) -> Vec Int n) ~ value => value -> (foreach (n :: Nat) -> Vec Int n)", "fromSymmetricPi f = f"])
        , ("TransitiveRetainedPiOperand", ["TypeFamilies", "FlexibleContexts", "AllowAmbiguousTypes"], [], ["fromTransitivePi :: forall left right. (left ~ right, right ~ (foreach (n :: Nat) -> Vec Int n)) => left -> (foreach (n :: Nat) -> Vec Int n)", "fromTransitivePi f = f"])
        , ("NestedFixedDomainPiOperand", ["TypeFamilies", "FlexibleContexts"], [], ["fromNestedPi :: forall value. value ~ (foreach (n :: Nat) -> foreach (m :: Nat) -> (Vec Int n, Vec Bool m)) => value -> (foreach (n :: Nat) -> foreach (m :: Nat) -> (Vec Int n, Vec Bool m))", "fromNestedPi f = f"])
        , ("VacuousPiMatchesOrdinaryArrow", ["TypeFamilies", "FlexibleContexts"], [], ["fromVacuousPi :: forall value. value ~ (foreach (n :: Nat) -> Int) => value -> Nat -> Int", "fromVacuousPi f = f"])
        , ("UnusedFamilyCycleAssumption", ["TypeFamilies", "FlexibleContexts"], [], ["type family Cycle value (index :: Nat)", "preserveCycle :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Vec (Cycle value k) n) => value -> value", "preserveCycle k f = f"])
        , ("GenericDatatypePi", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["ignore :: Maybe (foreach (n :: Nat) -> Vec Int n) -> Int", "ignore _ = 42"])
        , ("ExplicitPiArgument", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["same :: (foreach (n :: Nat) -> Vec Int n) -> (foreach (n :: Nat) -> Vec Int n)", "same f = id @(foreach (n :: Nat) -> Vec Int n) f"])
        , ("UnusedRecursivePiAssumption", ["TypeFamilies", "FlexibleContexts"], [], ["unused :: forall value. value ~ (foreach (n :: Nat) -> Vec value n) => value -> value", "unused f = f"])
        , ("OrdinarySplicedHoleUnchanged", ["TypeApplications", "TemplateHaskell"], ["import Language.Haskell.TH (Type(WildCardT))"], ["good :: Int", "good = id @($(pure WildCardT)) (42 :: Int)"])
        , ("OrdinaryNamedHoleUnchanged", ["TypeApplications", "NamedWildCards"], [], ["good :: Int", "good = id @_a (42 :: Int)"])
        , ("RetainedTypeSynonymBoundary", [], [], ["type Pi = foreach (n :: Nat) -> Vec Int n"])
        , ("PiGivenBeneathRetainedBinder", ["TypeFamilies", "FlexibleContexts"], [], ["liftGiven :: forall value. value ~ (foreach (n :: Nat) -> Vec Int n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Vec Int n), Vec Bool m))", "liftGiven f = f"])
        , ("OuterPiGivenBeneathRetainedBinder", ["TypeFamilies", "FlexibleContexts"], [], ["liftOuter :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> (Vec Int k, Vec Bool n)) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> (Vec Int k, Vec Bool n)), Vec Bool m))", "liftOuter k f = f"])
        , ("SynonymPiGiven", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["type Pi = foreach (n :: Nat) -> Vec Int n", "fromAlias :: forall value. value ~ Pi => value -> Pi", "fromAlias f = f"])
        , ("SynonymExplicitPiArgument", ["TypeFamilies", "FlexibleContexts", "TypeApplications"], [], ["type Pi = foreach (n :: Nat) -> Vec Int n", "same :: Pi -> Pi", "same f = id @Pi f"])
        , ("ExplicitPiKindViaAlias", ["TypeApplications", "PolyKinds", "StandaloneKindSignatures"], ["import Data.Kind (Type)"], ["type Pi = foreach (n :: Nat) -> Vec Int n", "type P :: forall k. k -> Type", "data P a = MkP", "bad :: forall (a :: Pi). P @Pi a", "bad = MkP @Pi @a"])
        , ("FormerRetainedSynonymBoundary", [], [], ["type family Select (b :: Bool) where", "  Select True = Int", "  Select False = Bool", "type Bad = foreach (b :: Bool) -> Select b -> Select b"])
        , ("OrdinaryQualifiedArrowControl", ["TypeFamilies", "FlexibleContexts", "ConstraintKinds", "AllowAmbiguousTypes"], [], ["ordinaryQualified :: (Nat -> Eq Int => Int) -> Nat -> Eq Int => Int", "ordinaryQualified f = f"])
        , ("DirectQualifiedPiGiven", qualifiedPiExtensions, [], ["fromQualifiedPi :: forall value. value ~ (foreach (n :: Nat) -> Eq Int => Vec Int n) => value -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "fromQualifiedPi f = f"])
        , ("QualifiedPiGivenBeneathRetainedBinder", qualifiedPiExtensions, [], ["liftQualifiedPi :: forall value. value ~ (foreach (n :: Nat) -> Eq Int => Vec Int n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Eq Int => Vec Int n), Vec Bool m))", "liftQualifiedPi f = f"])
        , ("QualifiedPiPredicateOccurrence", qualifiedPiExtensions, [], ["predicateCycle :: forall value. value ~ (foreach (n :: Nat) -> Eq value => Vec Int n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Eq value => Vec Int n), Vec Bool m))", "predicateCycle f = f"])
        , ("QualifiedAliasPredicateOccurrence", qualifiedPiExtensions, [], ["type Cycle value = foreach (n :: Nat) -> Eq value => Vec Int n", "badAliasCycle :: forall value. value ~ Cycle value => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Eq value => Vec Int n), Vec Bool m))", "badAliasCycle f = f"])
        , ("RetainedIdenticalRecursiveDomain", qualifiedPiExtensions ++ ["UndecidableInstances"], ["import Data.Kind (Type)"], ["data Token (a :: Type) = Token", "type family Loop (a :: Type) :: Type where Loop a = Loop a", "type A = Token (Loop Bool)", "data PA (n :: A) = PA", "same :: forall value. value ~ (foreach (n :: A) -> Eq value => PA n) => (foreach (m :: A) -> (value, PA m)) -> foreach (m :: A) -> ((foreach (n :: A) -> Eq value => PA n), PA m)", "same original = original"])
        , ("ReflQualifiedPiWitness", qualifiedPiExtensions, equalityImports, ["fromQualifiedRefl :: forall value. (value :~: (foreach (n :: Nat) -> Eq Int => Vec Int n)) -> value -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "fromQualifiedRefl proof f = case proof of Refl -> f"])
        , ("OrdinaryQualifiedGivenControl", qualifiedPiExtensions, [], ["ordinaryGiven :: forall value. value ~ (Nat -> Eq Int => Int) => value -> (Nat -> Eq Int => Int)", "ordinaryGiven f = f"])
        , ("QualifiedIdentity", qualifiedPiExtensions, [], ["qualifiedIdentity :: (foreach (n :: Nat) -> Eq Int => Vec Int n) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "qualifiedIdentity f = f"])
        , ("KnownNativeCase", qualifiedPiExtensions, [], ["caseKnown :: (foreach (n :: Nat) -> Eq Int => Vec Int n) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "caseKnown f = case f of saved -> saved"])
        , ("KnownNativeProofCopy", qualifiedPiExtensions, equalityImports, ["copyProof :: forall value. (value :~: (foreach (n :: Nat) -> Eq Int => Vec Int n)) -> (value :~: (foreach (n :: Nat) -> Eq Int => Vec Int n))", "copyProof proof = proof"])
        , ("CapturedQualifiedGiven", qualifiedPiExtensions, [], ["capturedGiven :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Eq (Vec Int k) => Vec Int n) => value -> (foreach (n :: Nat) -> Eq (Vec Int k) => Vec Int n)", "capturedGiven k f = f"])
        , ("CapturedQualifiedGivenBeneathBinder", qualifiedPiExtensions, [], ["capturedLift :: forall value. foreach (k :: Nat) -> value ~ (foreach (n :: Nat) -> Eq (Vec Int k) => Vec Int n) => (foreach (m :: Nat) -> (value, Vec Bool m)) -> (foreach (m :: Nat) -> ((foreach (n :: Nat) -> Eq (Vec Int k) => Vec Int n), Vec Bool m))", "capturedLift k f = f"])
        , ("OrdinaryImpredicativeList", qualifiedPiExtensions, [], ["ids :: [forall a. a -> a]", "ids = [id, id]"])
        , ("QualifiedAliasBeneathBinder", qualifiedPiExtensions, [], ["type Q = foreach (n :: Nat) -> Eq Int => Vec Int n", "type Lift = foreach (m :: Nat) -> Vec Bool m -> Q", "aliasLift :: forall value. value ~ Q => (foreach (m :: Nat) -> Vec Bool m -> value) -> Lift", "aliasLift f = f"])
        , ("ForgetfulQualifiedAlias", qualifiedPiExtensions, [], ["type Forget value = foreach (n :: Nat) -> Eq Int => Vec Int n", "forgetful :: forall value. value ~ Forget value => (foreach (m :: Nat) -> Vec Bool m -> value) -> (foreach (m :: Nat) -> Vec Bool m -> Forget value)", "forgetful f = f"])
        , ("QualifiedPiGroundPolymorphicHead", qualifiedPiExtensions, [], ["polyHead :: (foreach (n :: Nat) -> Eq Int => Vec Int n)", "polyHead = undefined"])
        , ("QualifiedPiGroundAppliedHead", qualifiedPiExtensions, [], ["sameQ :: (foreach (n :: Nat) -> Eq Int => Vec Int n) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "sameQ f = f", "applied :: (foreach (n :: Nat) -> Eq Int => Vec Int n) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "applied f = sameQ f"])
        , ("QualifiedPiGroundLeadingDictionary", qualifiedPiExtensions, [], ["leading :: (Eq Int => (foreach (n :: Nat) -> Eq Int => Vec Int n)) -> (foreach (n :: Nat) -> Eq Int => Vec Int n)", "leading f = f"])
        , ("IndexVTAKnownNativeArgument", ["TypeApplications", "ImpredicativeTypes"], [], ["observe :: forall a (index :: Nat). Vec a index -> Int", "observe _ = 42", "bad :: foreach (n :: Nat) -> Vec Int n -> Int", "bad n xs = observe @Int xs"])
        ]
        $ \(name, extensions, imports, declarations) -> do
            let moduleName = "NativeSourcePiOperand" ++ name
                source =
                    unlines $
                        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll #-}"
                        , "{-# LANGUAGE GADTs, KindSignatures, RankNTypes, ScopedTypeVariables #-}"
                        , "{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}"
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
                            ++ ["entry :: Int", "entry = 42"]
            void $ sourceEntry moduleName source
    -- Keep the ordinary spliced wildcard on its existing inference path.
    void
        $ sourceEntry "NativeSourcePackageOrdinarySplicedWildcard"
        $ unlines
        $ [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures #-}"
          , "{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications, TemplateHaskell #-}"
          , "module NativeSourcePackageOrdinarySplicedWildcard where"
          , "import Language.Haskell.TH (Type(WildCardT))"
          , "data Nat = Z | S Nat"
          , "data Vec a (n :: Nat) where"
          , "  VNil :: Vec a Z"
          , "  VCons :: a -> Vec a n -> Vec a (S n)"
          ]
            ++ localActions
            ++ [ "entry :: Int"
               , "entry = length (toListLocal (id @(Vec $(pure WildCardT) Z) VNil))"
               ]
    -- Discard earlier inference-only targets before loading the provider graph.
    flags <- GHC.getSessionDynFlags
    logger <- GHC.getLogger
    (containerFlags, leftovers, _) <-
        GHC.parseDynamicFlags logger flags $
            map noLoc ["-O0", "-fobject-code", "-fasm", "-fforce-recomp", "-fomit-interface-pragmas", "-fno-write-if-simplified-core", "-dcore-lint", "-dstg-lint", "-v0"]
    unless (null leftovers) (fail "FAIL: native container flags were not parsed")
    void $ GHC.setSessionDynFlags containerFlags{ghcLink = GHC.NoLink}
    now <- liftIO getCurrentTime
    let target =
            GHC.Target
                { GHC.targetId = GHC.TargetFile "NativePiContainerProvider.hs" Nothing
                , GHC.targetUnitId = GHC.homeUnitId_ containerFlags
                , GHC.targetAllowObjCode = True
                , GHC.targetContents = Just (stringToStringBuffer containerProvider, now)
                }
    GHC.setTargets [target]
    loaded <- GHC.load GHC.LoadAllTargets
    unless (GHC.succeeded loaded) (fail "FAIL: native container provider did not load")
    forM_ containerClients $ \(label, diagnostic, extensions, imports, declarations) -> do
        let moduleName = "NativeSourceContainer" ++ label
            source =
                unlines $
                    map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") (containerExtensions ++ extensions)
                        ++ [ "module " ++ moduleName ++ " where"
                           , "import PackedVecLibrary"
                           ]
                        ++ imports
                        ++ declarations
                        ++ ["entry :: Int", "entry = 42"]
        case diagnostic of
            Nothing -> void $ sourceEntry moduleName source
            Just (code, fragment) -> sourceModuleRejects label code fragment moduleName source
    void (GHC.setSessionDynFlags flags)
    liftIO $ putStrLn "package source rejection checks: ok"
  where
    containerExtensions = ["DataKinds", "ExperimentalDependentTypes", "ExplicitForAll", "GADTs", "KindSignatures", "RankNTypes", "ScopedTypeVariables", "TypeFamilies", "TypeOperators", "FlexibleContexts", "ConstraintKinds", "AllowAmbiguousTypes", "TypeApplications", "NoImpredicativeTypes"]
    containerProvider =
        unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, ExplicitForAll, GADTs, KindSignatures, RankNTypes, ScopedTypeVariables, TypeFamilies, TypeOperators, FlexibleContexts, ConstraintKinds, AllowAmbiguousTypes, TypeApplications, ImpredicativeTypes, PolyKinds, UnboxedTuples, MagicHash #-}"
            , "module NativePiContainerProvider where"
            , "import PackedVecLibrary"
            , "import Data.Type.Equality ((:~:)(Refl))"
            , "import Data.Proxy (Proxy(..))"
            , "import GHC.Exts (Int#)"
            , "type QPi = foreach (n :: Nat) -> Eq Int => Vec Int n"
            , "withQ :: forall value. (value :~: QPi) -> value -> QPi"
            , "withQ proof f = case proof of Refl -> f"
            , "type ProofQ = QPi :~: QPi"
            , "data ProofBox = ProofBox ProofQ"
            , "withKnownQ :: ProofQ -> QPi -> QPi"
            , "withKnownQ proof f = case proof of Refl -> f"
            , "fixedQ :: QPi"
            , "fixedQ n = replicateNative @Int n 7"
            , "taggedWithQ :: forall tag value. tag -> (value :~: QPi) -> value -> QPi"
            , "taggedWithQ _ proof f = case proof of Refl -> f"
            , "ordinaryIgnore :: forall value. value -> Int"
            , "ordinaryIgnore _ = 42"
            , "taggedIgnore :: forall tag value. tag -> value -> Int"
            , "taggedIgnore _ _ = 42"
            , "observeIndex :: forall a (index :: Nat). Vec a index -> Int"
            , "observeIndex _ = 42"
            , "prefixProbe :: forall value. value -> () -> value"
            , "prefixProbe value _ = value"
            , "groundKindIdentity :: forall {kind} (a :: kind). Proxy a -> Proxy a"
            , "groundKindIdentity p = p"
            , "groundSpecifiedKindIdentity :: forall kind (a :: kind). Proxy a -> Proxy a"
            , "groundSpecifiedKindIdentity p = p"
            , "groundClass :: forall tag value. Eq tag => tag -> (value :~: QPi) -> value -> QPi"
            , "groundClass _ proof f = case proof of Refl -> f"
            , "type ScopedQ (k :: Nat) = foreach (n :: Nat) -> Vec Bool n -> (Vec Int k, Vec Bool n)"
            , "withScoped :: forall (index :: Nat) value. (value :~: ScopedQ index) -> value -> ScopedQ index"
            , "withScoped proof f = case proof of Refl -> f"
            , "exposeQ :: forall value. value ~ QPi => value -> QPi"
            , "exposeQ f = f"
            , "contextConsumer :: forall value. value ~ QPi => value -> Int"
            , "contextConsumer _ = 42"
            , "taggedContextConsumer :: forall tag value. value ~ QPi => tag -> value -> Int"
            , "taggedContextConsumer _ _ = 42"
            , "nestedContextConsumer :: forall tag. Eq tag => forall value. value ~ QPi => tag -> value -> Int"
            , "nestedContextConsumer _ _ = 42"
            , "bothQ :: forall value. (value :~: QPi) -> value -> value -> QPi"
            , "bothQ proof first _ = case proof of Refl -> first"
            , "requiresShowQ :: forall value. Show value => (value :~: QPi) -> value -> QPi"
            , "requiresShowQ proof f = case proof of Refl -> f"
            , "type NativePi = foreach (n :: Nat) -> Vec Int n"
            , "type QualifiedPair = (QPi, QPi)"
            , "headTuple :: forall a. (a, a) -> a"
            , "headTuple (first, _) = first"
            , "acceptPair :: QualifiedPair -> QPi"
            , "acceptPair (first, _) = first"
            , "acceptNested :: Maybe (QualifiedPair, Int) -> QPi -> QPi"
            , "acceptNested _ f = f"
            , "acceptIndexedPair :: foreach (k :: Nat) -> (Vec Int k, QPi) -> QPi"
            , "acceptIndexedPair k pair = case pair of (_, f) -> f"
            , "type QualifiedSibling = (QPi, Eq Int => Int)"
            , "acceptSibling :: QualifiedSibling -> Int"
            , "acceptSibling _ = 42"
            , "type ErasedSibling = (NativePi, forall a. a -> a)"
            , "acceptErasedSibling :: ErasedSibling -> Int"
            , "acceptErasedSibling _ = 42"
            , "bottomValue :: forall a. a"
            , "bottomValue = error \"native-pi-container-bottom-witness\""
            , "proxyValue :: forall k (a :: k). Proxy a"
            , "proxyValue = Proxy"
            , "acceptClassProxy :: Proxy (Eq NativePi) -> Int"
            , "acceptClassProxy _ = 42"
            , "type family Stuck a"
            , "acceptStuck :: Stuck NativePi -> Int"
            , "acceptStuck _ = 42"
            , "acceptUnsaturatedProxy :: Proxy (Either NativePi) -> Int"
            , "acceptUnsaturatedProxy _ = 42"
            , "acceptUnliftedProxy :: Proxy (# NativePi, Int# #) -> Int"
            , "acceptUnliftedProxy _ = 42"
            , "newtype ContainerShell a = ContainerShell a"
            , "openShell :: ContainerShell QPi -> QPi"
            , "openShell (ContainerShell f) = f"
            ]
    containerClients =
        [ ("ReflClient", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["viaWitness :: QPi -> QPi", "viaWitness = withQ Refl"])
        , ("OriginalBottomWitnessPartial", Nothing, [], ["import NativePiContainerProvider"], ["viaWitness :: QPi -> QPi", "viaWitness = withQ (error \"native-q-bottom-proof\")"])
        , ("OriginalBottomWitnessApplied", Nothing, [], ["import NativePiContainerProvider"], ["viaWitness :: QPi -> QPi", "viaWitness f = withQ (error \"native-q-bottom-proof\") f"])
        , ("WorkerFullRefl", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["viaWitness :: QPi -> QPi", "viaWitness f = withQ Refl f"])
        , ("WorkerEqualityGivenFullCall", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = exposeQ f"])
        , ("WorkerFullCallThenNativeIndex", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: foreach (n :: Nat) -> QPi -> Vec Int n", "via n f = withQ Refl f n"])
        , ("WorkerIncomingNamedProof", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: ProofBox -> QPi -> QPi", "via (ProofBox proof) f = withQ proof f"])
        , ("WorkerContextOnlyChoice", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> Int", "via f = contextConsumer f"])
        , ("WorkerNestedThetaBeforeValues", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: forall tag. Eq tag => tag -> QPi -> Int", "via tag f = nestedContextConsumer tag f"])
        , ("WorkerLeadingVTAChoice", Nothing, [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> Int", "via f = taggedContextConsumer @Int 42 f"])
        , ("WorkerForgedBoolWitness", Just (83865, "Couldn't match type"), [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi", "bad = withQ (Refl :: Bool :~: Bool) True"])
        , ("WorkerConflictingArguments", Just (91028, "Cannot instantiate unification variable"), [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> (foreach (n :: Nat) -> Eq Int => Vec Bool n) -> QPi", "bad f g = bothQ Refl f g"])
        , ("WorkerMissingClass", Just (39999, "No instance for"), [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> QPi", "bad f = requiresShowQ Refl f"])
        , ("WorkerErasedPolymorphicArgument", Just (91028, "Cannot instantiate unification variable"), [], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: (forall a. a) -> QPi", "bad f = withQ Refl f"])
        , ("OrdinaryGenericBottomWitness", Nothing, [], ["import NativePiContainerProvider"], ["viaWitness :: QPi -> QPi", "viaWitness = withQ bottomValue"])
        , ("ImportedHeadTupleQualifiedPair", Nothing, [], ["import NativePiContainerProvider"], ["tupleClient :: QPi -> QPi -> QPi", "tupleClient f g = acceptPair (headTuple ((f, g), (g, f)))"])
        , ("ImportedHeadTupleNestedContainer", Nothing, [], ["import NativePiContainerProvider"], ["nestedClient :: QPi -> QPi -> QPi", "nestedClient f g = acceptNested (headTuple (Just ((f, g), 42), Nothing)) f"])
        , ("CapturedIndexInsideContainer", Nothing, [], ["import NativePiContainerProvider"], ["capturePair :: foreach (k :: Nat) -> Vec Int k -> QPi -> QPi", "capturePair k xs f = acceptIndexedPair k (headTuple ((xs, f), (xs, f)))"])
        , ("IndependentIndexInsideContainer", Just (83865, "Actual: Vec Int m"), [], ["import NativePiContainerProvider"], ["wrongPair :: foreach (n :: Nat) -> foreach (m :: Nat) -> Vec Int n -> Vec Int m -> QPi -> QPi", "wrongPair n m xs ys f = acceptIndexedPair n (headTuple ((ys, f), (ys, f)))"])
        , ("WorkerNestedContainerResult", Nothing, [], ["import NativePiContainerProvider"], ["worker :: forall a. foreach (n :: Nat) -> Vec a n -> Maybe (foreach (m :: Nat) -> Vec a m -> Vec a m)", "worker n xs = Nothing", "specialized :: foreach (n :: Nat) -> Vec Int n -> Maybe (foreach (m :: Nat) -> Vec Int m -> Vec Int m)", "specialized = worker"])
        , ("WorkerContainerAfterOrdinaryPrefix", Nothing, [], ["import NativePiContainerProvider"], ["worker :: forall a. foreach (n :: Nat) -> Vec a n -> Maybe (foreach (m :: Nat) -> Vec a m -> Vec a m)", "worker n xs = Nothing", "taggedWorker :: forall a. Int -> foreach (n :: Nat) -> Vec a n -> Maybe (foreach (m :: Nat) -> Vec a m -> Vec a m)", "taggedWorker _ = worker @a", "specialized :: foreach (n :: Nat) -> Vec Int n -> Maybe (foreach (m :: Nat) -> Vec Int m -> Vec Int m)", "specialized = taggedWorker 42"])
        , ("IndependentQualifiedSibling", Just (83865, "Eq Int => Int"), [], ["import NativePiContainerProvider"], ["sibling :: QPi -> (Eq Int => Int) -> Int", "sibling f g = acceptSibling (headTuple ((f, g), (f, g)))"])
        , ("ErasedForallSibling", Just (91028, "polytypes: ErasedSibling"), [], ["import NativePiContainerProvider"], ["erasedSibling :: NativePi -> (forall a. a -> a) -> Int", "erasedSibling f g = acceptErasedSibling (headTuple ((f, g), (f, g)))"])
        , ("ClassArgumentGuard", Just (91028, "polytypes: Eq NativePi"), [], ["import NativePiContainerProvider"], ["classGuard :: Int", "classGuard = acceptClassProxy proxyValue"])
        , ("StuckFamilyGuard", Just (83865, "untouchable"), [], ["import NativePiContainerProvider"], ["familyGuard :: Int", "familyGuard = acceptStuck bottomValue"])
        , ("UnsaturatedArgumentGuard", Just (91028, "polytypes: Either NativePi"), [], ["import NativePiContainerProvider"], ["unsaturatedGuard :: Int", "unsaturatedGuard = acceptUnsaturatedProxy proxyValue"])
        , ("NonliftedArgumentGuard", Just (91028, "(# NativePi"), [], ["import NativePiContainerProvider"], ["nonliftedGuard :: Int", "nonliftedGuard = acceptUnliftedProxy proxyValue"])
        , ("OrdinaryTauTupleControl", Nothing, [], ["import NativePiContainerProvider"], ["ordinaryHead :: forall a. (a,a) -> a", "ordinaryHead (first,_) = first", "ordinaryPair :: (Int, Bool)", "ordinaryPair = ordinaryHead ((42, True), (43, False))"])
        , ("OrdinaryTauClassProxyControl", Nothing, ["PolyKinds"], ["import NativePiContainerProvider", "import Data.Proxy (Proxy)"], ["ordinaryClass :: Proxy (Eq Int)", "ordinaryClass = proxyValue"])
        , ("OrdinaryTauFamilyControl", Nothing, [], ["import NativePiContainerProvider"], ["type family OrdinaryFamily a", "ordinary :: OrdinaryFamily Int -> OrdinaryFamily Int", "ordinary x = id x"])
        , ("OrdinaryTauUnsaturatedControl", Nothing, ["PolyKinds"], ["import NativePiContainerProvider", "import Data.Proxy (Proxy)"], ["ordinaryHigher :: Proxy (Either Int)", "ordinaryHigher = proxyValue"])
        , ("OrdinaryTauUnliftedProxyControl", Nothing, ["PolyKinds", "UnboxedTuples", "MagicHash"], ["import NativePiContainerProvider", "import Data.Proxy (Proxy)", "import GHC.Exts (Int#)"], ["ordinaryUnlifted :: Proxy (# Int, Int# #)", "ordinaryUnlifted = proxyValue"])
        , ("NominalContainerGiven", Nothing, [], ["import NativePiContainerProvider hiding (NativePi)"], ["type NativePi = foreach (n :: Nat) -> Vec Int n", "nominal :: forall value. value ~ (NativePi, NativePi) => value -> (NativePi, NativePi)", "nominal x = x"])
        , ("RepresentationalEvidenceIsNotNominal", Just (91028, "Cannot equate type variable"), [], ["import NativePiContainerProvider hiding (NativePi)", "import Data.Coerce (Coercible)"], ["type NativePi = foreach (n :: Nat) -> Vec Int n", "representation :: forall value. Coercible value (NativePi, NativePi) => value -> (NativePi, NativePi)", "representation x = x"])
        , ("NewtypeContainer", Nothing, [], ["import NativePiContainerProvider"], ["shellClient :: QPi -> QPi", "shellClient f = openShell (headTuple (ContainerShell f, ContainerShell f))"])
        , ("GroundApplicationImpredExplicitVTARefl", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withQ @QPi Refl f"])
        , ("GroundApplicationImpredExplicitVTABottomProof", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withQ @QPi (error \"native-worker-bottom-proof\") f"])
        , ("GroundApplicationImpredExplicitVTAIncomingProof", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: ProofQ -> QPi -> QPi", "via proof f = withQ @QPi proof f"])
        , ("GroundApplicationImpredKnownMonomorphicProofHead", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withKnownQ Refl f"])
        , ("GroundApplicationClosedGroundRefl", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["closed :: QPi :~: QPi", "closed = Refl"])
        , ("GroundApplicationGroundInferredKindParameter", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Proxy (Proxy)"], ["via :: Proxy QPi -> Proxy QPi", "via p = groundKindIdentity @QPi p"])
        , ("GroundApplicationGroundSpecifiedKindParameter", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Proxy (Proxy)", "import Data.Kind (Type)"], ["via :: Proxy QPi -> Proxy QPi", "via p = groundSpecifiedKindIdentity @Type @QPi p"])
        , ("GroundApplicationGroundClassAndLeadingVTA", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = groundClass @Bool @QPi True Refl f"])
        , ("GroundApplicationGroundPhantomInferredKindPrefix", Nothing, ["ImpredicativeTypes", "PolyKinds"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Kind (Type)"], ["phantomKind :: forall {kind} (value :: Type). Int", "phantomKind = 42", "via :: Int", "via = phantomKind @QPi"])
        , ("GroundApplicationGroundFilledInferredKindPrefix", Nothing, ["ImpredicativeTypes", "PolyKinds"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Proxy (Proxy (..))"], ["filledKind :: forall {kind} (value :: kind). Proxy value -> Int", "filledKind _ = 42", "via :: Int", "via = filledKind @QPi Proxy"])
        , ("GroundApplicationGroundDirectPhantomValuePrefix", Nothing, ["ImpredicativeTypes", "PolyKinds"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["phantomValue :: forall value. Int", "phantomValue = 42", "via :: Int", "via = phantomValue @QPi"])
        , ("GroundApplicationGroundWrongProof", Just (83865, "Couldn't match type"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: (Bool :~: QPi) -> QPi -> QPi", "bad proof f = withQ @QPi proof f"])
        , ("GroundApplicationGroundRepresentationalGiven", Just (91028, "Cannot equate type variable"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Coerce (Coercible)"], ["bad :: forall value. Coercible value QPi => value -> QPi", "bad f = withQ @value Refl f"])
        , ("GroundApplicationGroundMissingClass", Just (39999, "No instance for"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> QPi", "bad f = requiresShowQ @QPi Refl f"])
        , ("GroundApplicationGroundScopedWrongIndex", Just (83865, "Couldn't match type"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> ScopedQ n -> ScopedQ m", "bad n m f = withScoped @m @(ScopedQ n) Refl f"])
        , ("KnownArgumentLocalKnownRefl", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withQ Refl f"])
        , ("KnownArgumentGlobalKnownRefl", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi", "via = withQ Refl fixedQ"])
        , ("KnownArgumentLocalKnownBottomProof", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withQ (error \"known-bottom-proof\") f"])
        , ("KnownArgumentLocalKnownNamedProof", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: ProofQ -> QPi -> QPi", "via proof f = withQ proof f"])
        , ("KnownArgumentPartialExpectedArrow", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via = withQ Refl"])
        , ("KnownArgumentPartialBottomExpectedArrow", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via = withQ (error \"known-bottom-proof\")"])
        , ("KnownArgumentPartialLeadingVTAExpectedArrow", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via = taggedWithQ @Bool True Refl"])
        , ("KnownArgumentLeadingVTAKnownArgument", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = taggedWithQ @Bool True Refl f"])
        , ("KnownArgumentLeadingVTAIncomingProof", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: ProofQ -> QPi -> QPi", "via proof f = taggedWithQ @Bool True proof f"])
        , ("KnownArgumentContextOnlyKnownLocal", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> Int", "via f = contextConsumer f"])
        , ("KnownArgumentContextOnlyLeadingVTA", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> Int", "via f = taggedContextConsumer @Bool True f"])
        , ("KnownArgumentNominalGivenKnownValue", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: forall value. value ~ QPi => value -> QPi", "via f = withQ Refl f"])
        , ("KnownArgumentKnownNativeVector", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: foreach (n :: Nat) -> Vec Int n -> Int", "via n xs = ordinaryIgnore xs"])
        , ("KnownArgumentKnownNativeVectorLeadingVTA", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: foreach (n :: Nat) -> Vec Int n -> Int", "via n xs = taggedIgnore @Bool True xs"])
        , ("KnownArgumentKnownNativeVectorIndexKind", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: foreach (n :: Nat) -> Vec Int n -> Int", "via n xs = observeIndex @Int xs"])
        , ("KnownArgumentOrdinaryIdentityKnownFunction", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = id f"])
        , ("KnownArgumentOrdinaryIgnoreKnownFunction", Nothing, ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> Int", "via f = ordinaryIgnore f"])
        , ("KnownArgumentLinearDeclaredOnce", Nothing, ["ImpredicativeTypes", "LinearTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["data LinearBox value where", "  LinearBox :: value %1 -> LinearBox value", "linearHold :: forall value. value %1 -> LinearBox value", "linearHold value = LinearBox value", "via :: QPi %1 -> QPi", "via f = case linearHold f of LinearBox g -> g"])
        , ("KnownArgumentContradictoryFunctions", Just (83605, "Quick Look inference"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> (foreach (n :: Nat) -> Eq Int => Vec Bool n) -> QPi", "bad f wrong = bothQ Refl f wrong"])
        , ("KnownArgumentErasedPolymorphicOperandBoundary", Just (83605, "Quick Look inference"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["boundary :: (forall a. a) -> QPi", "boundary f = withQ Refl f"])
        , ("KnownArgumentIndependentScopedIndices", Just (83865, "Couldn't match type"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: foreach (n :: Nat) -> foreach (m :: Nat) -> ScopedQ n -> ScopedQ m", "bad n m f = withScoped Refl f"])
        , ("KnownArgumentPackageIndexEscape", Just (83865, "Couldn't match type"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: PackedVec Int -> ScopedQ Z", "bad (PackVec k xs) =", "  let worker :: ScopedQ k", "      worker n ys = (xs, ys)", "  in withScoped Refl worker"])
        , ("KnownArgumentMissingClassConstraint", Just (39999, "No instance for"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> QPi", "bad f = requiresShowQ Refl f"])
        , ("KnownArgumentRepresentationalGiven", Just (91028, "Cannot equate type variable"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))", "import Data.Coerce (Coercible)"], ["bad :: forall value. Coercible value QPi => value -> QPi", "bad f = withQ Refl f"])
        , ("KnownArgumentWrongNominalWitness", Just (83605, "Quick Look inference"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: (Bool :~: Bool) -> QPi -> QPi", "bad proof f = withQ proof f"])
        , ("KnownArgumentSignedWitnessBoundary", Just (83605, "Quick Look inference"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = withQ (Refl :: ProofQ) f"])
        , ("KnownArgumentAppliedHeadBoundary", Just (83605, "Quick Look inference"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["via :: QPi -> QPi", "via f = (id withQ) Refl f"])
        , ("KnownArgumentCacheOwnershipFailure", Just (83865, "Couldn't match type"), ["ImpredicativeTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["bad :: QPi -> (foreach (n :: Nat) -> Eq Int => Vec Bool n) -> QPi", "bad f wrong = bothQ Refl (prefixProbe f ()) wrong"])
        , ("KnownArgumentLinearDeclaredTwice", Just (18872, "multiplicity"), ["ImpredicativeTypes", "LinearTypes"], ["import NativePiContainerProvider", "import Data.Type.Equality ((:~:)(Refl))"], ["data LinearBox value where", "  LinearBox :: value %1 -> LinearBox value", "linearHold :: forall value. value %1 -> LinearBox value", "linearHold value = LinearBox value", "bad :: QPi %1 -> (QPi,QPi)", "bad f = (case linearHold f of LinearBox g -> g, f)"])
        ]
    qualifiedPiExtensions = ["TypeFamilies", "FlexibleContexts", "ConstraintKinds", "AllowAmbiguousTypes", "ImpredicativeTypes"]
    declaration signature = ["data PackedVec a where", "  PackVec :: " ++ signature]
    package = declaration "foreach (n :: Nat) -> Vec a n -> PackedVec a"
    localSignatures =
        package
            ++ [ "localVector :: foreach (n :: Nat) -> Vec Int n"
               , "localVector Z = VNil"
               , "localVector (S k) = VCons 7 (localVector k)"
               ]
    localActions =
        localSignatures
            ++ [ "toListLocal :: Vec a index -> [a]"
               , "toListLocal VNil = []"
               , "toListLocal (VCons x xs) = x : toListLocal xs"
               ]
    equalityImports = ["import Data.Type.Equality ((:~:)(Refl))"]
    sums =
        [ "type family Add (n :: Nat) (m :: Nat) :: Nat where"
        , "  Add Z m = m"
        , "  Add (S n) m = S (Add n m)"
        , "data Sum (n :: Nat) (m :: Nat) where"
        , "  Sum :: foreach (p :: Nat) -> (p :~: Add n m) -> Sum n m"
        , "sumNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Sum n m"
        , "sumNat n m = case n of"
        , "  Z -> Sum m Refl"
        , "  S k -> case sumNat k m of"
        , "    Sum p Refl -> Sum (S p) Refl"
        , "appendVec :: Vec a n -> Vec a m -> Vec a (Add n m)"
        , "appendVec VNil ys = ys"
        , "appendVec (VCons x xs) ys = VCons x (appendVec xs ys)"
        , "appendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)"
        , "appendNative n m xs ys = appendVec xs ys"
        , "replicateNative :: forall a. foreach (n :: Nat) -> a -> Vec a n"
        , "replicateNative Z x = VNil"
        , "replicateNative (S k) x = VCons x (replicateNative k x)"
        ]
    equality =
        [ "sameNat :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)"
        , "sameNat Z Z = Just Refl"
        , "sameNat (S left) (S right) = case sameNat left right of"
        , "  Nothing -> Nothing"
        , "  Just Refl -> Just Refl"
        , "sameNat _ _ = Nothing"
        ]
    zipper =
        [ "zipVec :: Vec a n -> Vec b n -> Vec (a, b) n"
        , "zipVec VNil VNil = VNil"
        , "zipVec (VCons x xs) (VCons y ys) = VCons (x, y) (zipVec xs ys)"
        ]
    reversal =
        [ "addSucc :: foreach (n :: Nat) -> foreach (m :: Nat) -> Add n (S m) :~: S (Add n m)"
        , "addSucc Z right = Refl"
        , "addSucc (S k) later = case addSucc k later of"
        , "    Refl -> Refl"
        , "rightZero :: foreach (n :: Nat) -> Add n Z :~: n"
        , "rightZero Z = Refl"
        , "rightZero (S k) = case rightZero k of"
        , "    Refl -> Refl"
        , "reverseAppendNative :: forall a. foreach (n :: Nat) -> foreach (m :: Nat) -> Vec a n -> Vec a m -> Vec a (Add n m)"
        , "reverseAppendNative Z right xs acc = case xs of"
        , "    VNil -> acc"
        , "reverseAppendNative (S k) later xs acc = case xs of"
        , "    VCons x rest -> case addSucc k later of"
        , "        Refl -> reverseAppendNative @a k (S later) rest (VCons x acc)"
        , "reverseNative :: forall a. foreach (n :: Nat) -> Vec a n -> Vec a n"
        , "reverseNative n xs = case rightZero n of"
        , "    Refl -> reverseAppendNative @a n Z xs VNil"
        ]
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
            , 5617
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
            , 5617
            , "Vec"
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
    -- A complete container result determines the immutable instantiation.
    -- See Note [Known retained function instantiation] in GHC.Tc.Gen.App.
    void $ sourceEntry "NativeSourceContainerUndefined" $
        unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators, NoImpredicativeTypes #-}"
            , "module NativeSourceContainerUndefined where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            , "bad :: Maybe (foreach (b :: Bool) -> Select b -> Select b)"
            , "bad = undefined"
            , "entry :: Int"
            , "entry = 42"
            ]
    -- Keep the former Quick Look boundary as the same checked source.
    void $ sourceEntry "NativeSourceGroundImpredicative" $
        unlines $
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"
            , "{-# LANGUAGE ImpredicativeTypes #-}"
            , "module NativeSourceGroundImpredicative where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            ]
                ++ local [] consumer ["keep b x = 42"] "keep True 42"
    -- The signed alias names the same runtime index as its RHS.
    void $ sourceEntry "NativeSourceSignedBoolAlias" $
        unlines $
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"
            , "module NativeSourceSignedBoolAlias where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            ]
                ++ local [] selection
                     (identity ++ ["f :: " ++ selection, "f b x = let c :: Bool; c = b in keep c x"])
                     "f True 42"
    -- Ordinary literal patterns use the refined argument type and keep row order.
    void $ sourceEntry "NativeSourceLaterOrdinaryLiteral" $
        unlines $
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, KindSignatures, RankNTypes, TypeFamilies, TypeOperators #-}"
            , "module NativeSourceLaterOrdinaryLiteral where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            ]
                ++ local [] selection
                     ["keep True 0 = 42", "keep True x = x", "keep False x = x"]
                     "keep True 0 + keep True 7"
    -- The installed Boolean negation has checked equations. Its result
    -- selects Int before the literal is passed to the retained function.
    void $ sourceEntry "NativeSourceCheckedBooleanProducer" $
        unlines $
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}"
            , "module NativeSourceCheckedBooleanProducer where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            ] ++ local [] selection identity "keep (not False) (42 :: Int)"
    forM_ [("Inline", "INLINE"), ("Inlinable", "INLINABLE")] $ \(suffix, pragma) -> do
        let name = "NativeSource" ++ suffix
        void $ sourceEntry name $ unlines $
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeFamilies #-}"
            , "module " ++ name ++ " where"
            , "type family Select (b :: Bool) where"
            , "  Select True = Int"
            , "  Select False = Bool"
            ] ++ local ["{-# " ++ pragma ++ " keep #-}"] selection identity "keep True 42"
    mapM_
        check
        [ ("unsupported newtype domain", 83605, "A retained value argument requires a lifted type parameter or a supported algebraic type.", [],
           ["newtype Wrapped = Wrapped Int"] ++ local [] "foreach (b :: Wrapped) -> Int -> Int" identity "42")
        , ("missing domain annotation", 83605, "explicit domain type", [], local [] "foreach b -> Int -> Int" identity "42")
        , ("anonymous retained binder", 83605, "requires a variable name", [], local [] "foreach (_ :: Bool) -> Int -> Int" identity "42")
        , ("partial retained signature", 83605, "complete type signature", ["PartialTypeSignatures", "NamedWildCards"], local [] "foreach (b :: Bool) -> _a -> _a" identity "42")
        , ("inner erased forall", 83605, "within the scope of a retained binder", ["RankNTypes"], local [] "foreach (b :: Bool) -> forall a. a -> Select b" ["keep b x = x"] "42")
        , ("strict retained pattern", 83605, "bare variable pattern", ["BangPatterns"], local [] selection ["keep !_ x = x"] "42")
        , ("multiple variable equations", 83605, "one unguarded clause for each constructor", [], local [] selection (identity ++ identity) "42")
        , ("uncertified producer expression", 83605, "No checked source equations are available for", [],
           ["partial :: Bool -> Bool", "partial flag = if flag then False else error \"undefined index\""]
           ++ local [] selection identity "keep (partial False) (42 :: Int)")
        , ("nonreflexive indexed constraint", 18872, "True", [], local [] "foreach (b :: Bool) -> ((b ~ True) => Int) -> Int" identity "42")
        , ("general indexed constraint", 39999, "Show", ["FlexibleContexts"], local [] "foreach (b :: Bool) -> (Show (Select b) => Int) -> Int" identity "42")
        , ("Quick Look through ($)", 83605, "Quick Look inference", [], local [] consumer ["keep b x = 42"] "(keep $ True) 42")
        , ("SPECIALISE type pragma", 83605, "SPECIALISE pragmas", [], local ["{-# SPECIALISE keep :: Bool -> Int -> Int #-}"] selection identity "42")
        , ("SPECIALISE expression pragma", 83605, "SPECIALISE pragmas", [], local ["{-# SPECIALISE keep True #-}"] selection identity "42")
        , ("unsigned retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["alias = keep"]) "42")
        , ("unsigned partial retained alias", 83605, "requires an explicit signature", [], local [] selection (identity ++ ["index :: Bool", "index = False", "alias = keep index"]) "42")
        , ("unregistered ordinary term name", 37479, "is a term-level binding", [], ["ordinary :: Bool", "ordinary = False", "type Bad = NativeSourceRejected.ordinary"])
        , ("constructor payload mismatch", 83865, "second argument", [], local [] selection identity "keep True False")
        ,
            ( "distinct index from a local alias"
            , 5617
            , "Select"
            , []
            , local [] selection
                (identity ++ ["f :: foreach (b :: Bool) -> foreach (other :: Bool) -> Select b -> Select b"
                             , "f b other x = let c :: Bool; c = other in keep c x"]) "42"
            )
        , ("True payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = True"] "42")
        , ("False payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = False"] "42")
        , ("numeric payload at an unknown index", 83865, "Select", [], local [] "foreach (b :: Bool) -> Select b" ["keep b = 42"] "42")
        , ("swapped constructor results", 83865, "Int", [], local [] chooser ["keep True = False", "keep False = 42"] "42")
        , ("missing constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True = 42"] "42")
        , ("duplicate constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True = 42", "keep True = 43"] "42")
        , ("guarded constructor equation", 83605, "one unguarded clause for each constructor", [], local [] chooser ["keep True | True = 42", "keep False = False"] "42")
        , ("wrong later constructor argument", 83865, "False", [], local [] selection ["keep True False = 42", "keep False x = x"] "42")
        , ("linear constructor argument", 83605, "unrestricted multiplicities", ["LinearTypes"], local [] "foreach (b :: Bool) -> Select b %1 -> Select b" ["keep True x = x", "keep False x = x"] "42")
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
-- See Note [Native API arrow classification] in NativeDependentCore.hs.
selectFamily :: (TyCon, CoAxiom Branched)
selectFamily = (family, axiom)
  where
    parameter = mkTyVar (mkSystemName (unique 100) (mkTyVarOcc "b")) boolTy
    binders = [mkAnonTyConBinder parameter]
    family =
        mkFamilyTyCon
            (mkSystemName (unique 101) (mkTcOcc "Select"))
            (mkTyConKindWithMatchability unmatchableDataConTy binders liftedTypeKind)
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
    resultType = mkFunctionType unmatchableDataConTy ManyTy (selected (TermVarTy b)) (selected (TermVarTy b))
    observation = mkApps (Var force) [Type resultType, App (Var keepId) (Var b)]

-- | Keep a lazy local index whose producer is a global Boolean thunk.
localIndexKeep :: Id -> Id -> CoreExpr
localIndexKeep force producer =
    Let (NonRec b (Var producer)) (Let (NonRec keepId keepSelect) observation)
  where
    b = local 60 "local_index" boolTy
    keepId = local 61 "keep" (exprType keepSelect)
    resultType = mkFunctionType unmatchableDataConTy ManyTy (selected (TermVarTy b)) (selected (TermVarTy b))
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
indexed index = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [boolTy, index, index]) intTy

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
    worker = local 204 "replicate_native" (mkDepFunTy n (mkFunctionType unmatchableDataConTy ManyTy intTy result))
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
    worker = local 223 "head_native" (mkDepFunTy n (mkFunctionType unmatchableDataConTy ManyTy vectorTy intTy))
    (existentials, arguments) = dataConRepInstPat (map unique [230 .. 239]) ManyTy cons [intTy, successorIndex]

{- Note [Opaque call runtime checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The strict-call kernel takes an ordinary function and a runtime index.
Its constructor alternatives provide the exact equality for TermAppTy.
Cast a vector at the observed constructor index to the opaque call index,
then consume the vector with an ordinary observer. The DEFAULT kernel
instead transports a vector at the actual case binder. Neither kernel
evaluates an index by interpreting a type or inspecting an unfolding.

Check ordinary function application, constructor argument naming and proof
transport together. A type-only call must retain both free dependencies
without evaluating either value. These are Core tests; source call indices
and new interface forms remain outside this increment.
-}

-- | Check adjacent constructor cases before backend lowering.
runNativeConstructorCaseChecks :: DynFlags -> [Id] -> TyCon -> IO ()
runNativeConstructorCaseChecks flags bindings nat = do
    let natTy = mkTyConTy nat
        maybeNat = mkTyConApp maybeTyCon [natTy]
        input = local 3190 "constructor_input" natTy
        boxed = local 3191 "constructor_binding" maybeNat
        caseBinder = local 3192 "constructor_case" maybeNat
        field = local 3193 "constructor_field" natTy
        just expression = mkCoreConApps justDataCon [Type natTy, expression]
        choose scrutinee result nothing yes =
            Case scrutinee caseBinder result
                [ Alt (DataAlt nothingDataCon) [] nothing
                , Alt (DataAlt justDataCon) [field] yes
                ]
        check label expression accepts = do
            lintCheckedCore flags (label ++ " input") expression
            let simplified = simpleOptNativeExpr expression
            lintCheckedCore flags (label ++ " output") simplified
            unless (exprType expression `eqType` exprType simplified) $
                fail ("FAIL: " ++ label ++ "; result type changed")
            unless (accepts simplified) $
                fail ("FAIL: " ++ label ++ "; unexpected simplified Core: " ++ showSDoc flags (ppr simplified))
        identityCase = Lam input $ Let (NonRec boxed (just (Var input))) $
            choose (Var boxed) natTy (Var input) (Var field)
    check "adjacent constructor case returns the original field" identityCase $ \expression ->
        case expression of
            Lam actual (Var returned) -> actual == input && returned == input
            Lam actual (Let (NonRec alias (Var original)) (Var returned)) ->
                actual == input && original == input && returned == alias
            _ -> False

    let opaque = helper bindings "opaqueNat"
        producer = App (Var opaque) (Var input)
        listTy = mkListTy natTy
        twice = mkCoreConApps consDataCon
            [ Type natTy, Var field
            , mkCoreConApps consDataCon [Type natTy, Var field, mkCoreConApps nilDataCon [Type natTy]]
            ]
        sharingCase = Lam input $ Let (NonRec boxed (just producer)) $
            choose (Var boxed) listTy (mkCoreConApps nilDataCon [Type natTy]) twice
    check "constructor case shares a nontrivial field" sharingCase $ \expression ->
        case expression of
            Lam actual (Let (NonRec shared (App (Var original) (Var argument))) body)
                | actual == input, original == opaque, argument == input
                , (Var outer, [Type _, Var first, tail]) <- collectArgs body
                , outer == dataConWorkId consDataCon
                , (Var inner, [Type _, Var second, _]) <- collectArgs tail
                -> inner == outer && first == shared && second == shared
            _ -> False

    let parameter = mkTyVar (mkSystemName (unique 3194) (mkTyVarOcc "boxed_index")) maybeNat
        consume = local 3195 "consume_boxed_index" $
            mkForAllTy unmatchableDataConTy (Bndr parameter Specified) (mkVisFunTyManyU natTy natTy)
        indexed value = mkApps (Var consume) [Type (TermVarTy boxed), value]
        indexedCase = Lam consume $ Lam input $ Let (NonRec boxed (just (Var input))) $
            choose (Var boxed) natTy (indexed (Var input)) (indexed (Var field))
    check "constructor binding used only in a native index keeps its case" indexedCase $ \expression ->
        case expression of
            Lam _ (Lam _ (Let (NonRec retained _) body@Case{})) ->
                retained == boxed && boxed `elemVarSet` exprFreeVars body
            _ -> False

    let unknown = local 3196 "unknown_constructor" maybeNat
        unknownCase = Lam unknown $ Lam input $
            choose (Var unknown) natTy (Var input) (Var field)
    check "unknown constructor keeps its case" unknownCase $ \expression ->
        case expression of
            Lam actual (Lam _ (Case (Var scrutinee) _ _ _)) ->
                actual == unknown && scrutinee == unknown
            _ -> False

-- | Check the Core intermediate with runtime inputs and lazy recursive fields.
runNatChecks :: HscEnv -> DynFlags -> [Id] -> [TyCon] -> IO ()
runNatChecks environment flags bindings tycons = do
    nat <- namedTyCon "Nat"
    runNativeConstructorCaseChecks flags bindings nat
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
    let betaIndex = local 3100 "beta_index" (mkTyConTy nat)
        betaIdentity = App (Lam betaIndex (Var betaIndex)) (Var input)
        betaSuccessor = App (Lam betaIndex (App (Var (dataConWorkId successor)) (Var betaIndex))) (Var input)
        betaConstant = App (Lam betaIndex (Var (dataConWorkId zero))) (Var input)
        betaIndexType = TermVarTy input
        betaSuccType = mkTyConApp (promoteDataCon successor) [betaIndexType]
        betaZeroType = mkTyConTy (promoteDataCon zero)
        betaKernel observer argument resultIndex element =
            Let (Rec [(worker, rhs)])
                $ Lam input
                $ mkApps
                    (Var (helper bindings observer))
                    [Type resultIndex, mkApps (Var worker) [argument, element]]
        betaDrive observer argument resultIndex seed =
            mkApps
                (Var (helper bindings "driveNat"))
                [betaKernel observer argument resultIndex payload, Var (helper bindings seed)]
    forM_ [(betaIdentity, betaIndexType), (betaSuccessor, betaSuccType), (betaConstant, betaZeroType)] $ \(argument, resultIndex) -> do
        let applied = mkApps (Var worker) [argument, payload]
            expected = mkTyConApp vec [intTy, resultIndex]
        unless (eqType (exprType applied) expected) $
            fail "FAIL: beta replication recovered the wrong dependent result type"
    expect
        "beta identity at zero/two/three"
        (unlines ["[]:0", "[42,42]:2", "[42,42,42]:3"])
        (App (Var (helper bindings "driveNats")) (betaKernel "observeVec" betaIdentity betaIndexType payload))
    expect
        "beta successor at zero/two/three"
        (unlines ["[42]:1", "[42,42,42]:3", "[42,42,42,42]:4"])
        (App (Var (helper bindings "driveNats")) (betaKernel "observeVec" betaSuccessor betaSuccType payload))
    expect "beta constant ignores bottom index and payload" "[]:0" $
        mkApps
            (Var (helper bindings "driveNat"))
            [betaKernel "observeVec" betaConstant betaZeroType (Var (helper bindings "bottomElement")), Var (helper bindings "bottomNat")]
    expectBottom "beta identity demands original bottom index" (betaDrive "observeVecHead" betaIdentity betaIndexType "bottomNat")
    expect "beta successor head ignores bottom predecessor" "42" (betaDrive "observeVecHead" betaSuccessor betaSuccType "bottomNat")
    expectBottom "beta successor tail demands original predecessor" (betaDrive "observeVec" betaSuccessor betaSuccType "bottomNat")
    expect "beta identity retains a finite cyclic prefix" "[42,42,42,42]" (betaDrive "observeVecPrefix" betaIdentity betaIndexType "cyclicNat")
    cpConfig <- initCorePrepConfig environment
    let nativeCpConfig = cpConfig{cp_allowNativeDependent = True, cp_arityOpts = Nothing, cp_specEval = False, cp_specEvalDFun = False}
    forM_
        [ ("identity", betaKernel "observeVec" betaIdentity betaIndexType payload)
        , ("successor", betaKernel "observeVecHead" betaSuccessor betaSuccType payload)
        , ("constant", betaKernel "observeVec" betaConstant betaZeroType (Var (helper bindings "bottomElement")))
        ]
        $ \(label, rawKernel) -> do
            prepared <- corePrepExpr (hsc_logger environment) nativeCpConfig rawKernel
            unless (eqType (exprType rawKernel) (exprType prepared)) $
                fail ("FAIL: direct beta CorePrep changed the " ++ label ++ " kernel type")
            lintNative flags ("direct beta CorePrep " ++ label) prepared
            let seed = if label == "identity" then Var (dataConWorkId zero) else Var (helper bindings "bottomNat")
                expected = if label == "successor" then "42" else "[]:0"
            expect ("direct beta CorePrep " ++ label) expected $
                mkApps (Var (helper bindings "driveNat")) [prepared, seed]
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
                    (mkFunctionType unmatchableDataConTy ManyTy leftType (mkFunctionType unmatchableDataConTy ManyTy rightType resultType))
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
    let bottomBetaPartial = App (Var bottomFunction) betaSuccessor
        bottomBetaKernel =
            Lam input
                $ Let (NonRec bottomFunction bottomFunctionRhs)
                $ mkApps (Var (helper bindings "force")) [Type (exprType bottomBetaPartial), bottomBetaPartial]
    betaBottomOutcome <-
        try
            ( execute environment flags "beta argument preserves bottom function WHNF" $
                mkApps (Var (helper bindings "driveNat")) [bottomBetaKernel, Var (helper bindings "bottomNat")]
            ) ::
            IO (Either SomeException String)
    case betaBottomOutcome of
        Left exception | "native-bottom-function" `isInfixOf` displayException exception -> pure ()
        Left exception -> fail ("FAIL: beta argument changed bottom function demand: " ++ displayException exception)
        Right value -> fail ("FAIL: beta argument hid a bottom function: " ++ show value)
    let natTy = mkTyConTy nat
        keepN = local 211 "unused_nat" natTy
        indexedNat index = mkFunctionType unmatchableDataConTy ManyTy (mkTyConApp eqTyCon [natTy, index, index]) intTy
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
    let betaKept = mkApps (Var keepId) [betaIdentity, value, reflexive]
        betaKeepKernel = Lam input (Let (NonRec keepId keepExpr) (App (Var (helper bindings "observeInt")) betaKept))
    lintNative flags "raw beta keep kernel" betaKeepKernel
    preparedKeep <- corePrepExpr (hsc_logger environment) nativeCpConfig betaKeepKernel
    unless (eqType (exprType betaKeepKernel) (exprType preparedKeep)) $
        fail "FAIL: direct beta keep CorePrep changed its kernel type"
    lintNative flags "direct beta keep CorePrep" preparedKeep
    expect "direct beta keep CorePrep does not force its input" "42" $
        mkApps (Var (helper bindings "driveNat")) [preparedKeep, Var (helper bindings "bottomNat")]
    let aliasActual = local 3101 "alias_actual" natTy
        aliasIndex = local 3102 "alias_index" natTy
        aliasType = TermVarTy aliasIndex
        aliasProducer = App (Var (helper bindings "opaqueNat")) (Var input)
        -- | Check the native simplifier before backend alias substitution.
        prepareAlias label body = do
            let original = Lam input (Let (NonRec aliasActual aliasProducer) (Let (NonRec aliasIndex (Var aliasActual)) body))
                simplified = simpleOptNativeExpr original
            lintNative flags (label ++ " input") original
            unless (eqType (exprType original) (exprType simplified)) $
                fail ("FAIL: " ++ label ++ "; alias substitution changed the result type")
            lintNative flags (label ++ " output") simplified
            case simplified of
                Lam _ (Let (NonRec actual producer) body') -> do
                    unless (actual == aliasActual && isNothing (exprToNativeIndex producer)) $
                        fail ("FAIL: " ++ label ++ "; opaque producer binding changed")
                    case body' of
                        Let (NonRec binder _) _ | binder == aliasIndex ->
                            fail ("FAIL: " ++ label ++ "; native variable alias was retained")
                        _ -> pure ()
                    unless (actual `elemVarSet` exprFreeVars body' && not (aliasIndex `elemVarSet` exprFreeVars body')) $
                        fail ("FAIL: " ++ label ++ "; alias substitution lost its scoped actual variable")
                _ -> fail ("FAIL: " ++ label ++ "; lazy producer binding was removed")
            pure simplified
        aliasReplication observer =
            Let (Rec [(worker, rhs)]) $
                mkApps (Var (helper bindings observer)) [Type aliasType, mkApps (Var worker) [Var aliasIndex, payload]]
    aliasFinite <- prepareAlias "direct variable alias replication" (aliasReplication "observeVec")
    expect "variable alias replication at zero/two/three" (unlines ["[]:0", "[42,42]:2", "[42,42,42]:3"]) $
        App (Var (helper bindings "driveNats")) aliasFinite
    aliasHead <- prepareAlias "direct variable alias head" (aliasReplication "observeVecHead")
    expectBottom "variable alias demands its original index" $
        mkApps (Var (helper bindings "driveNat")) [aliasHead, Var (helper bindings "bottomNat")]
    aliasPrefix <- prepareAlias "direct variable alias cyclic prefix" (aliasReplication "observeVecPrefix")
    expect "variable alias preserves a finite cyclic prefix" "[42,42,42,42]" $
        mkApps (Var (helper bindings "driveNat")) [aliasPrefix, Var (helper bindings "cyclicNat")]
    let aliasDictionary = local 3103 "alias_dictionary" (mkTyConApp eqTyCon [natTy, aliasType, aliasType])
        aliasValue = Lam aliasDictionary payload
        aliasReflexive = mkCoreConApps eqDataCon [Type natTy, Type aliasType, Type aliasType, Coercion (mkNomReflCo aliasType)]
        aliasKept = mkApps (Var keepId) [Var aliasIndex, aliasValue, aliasReflexive]
        aliasKeepBody = Let (NonRec keepId keepExpr) (App (Var (helper bindings "observeInt")) aliasKept)
    aliasKeep <- prepareAlias "direct variable alias keep" aliasKeepBody
    expect "variable alias keep ignores bottom" "42" $
        mkApps (Var (helper bindings "driveNat")) [aliasKeep, Var (helper bindings "bottomNat")]
    let aliasPartial = App (Var bottomFunction) (Var aliasIndex)
        aliasBottomBody =
            Let (NonRec bottomFunction bottomFunctionRhs) $
                mkApps (Var (helper bindings "force")) [Type (exprType aliasPartial), aliasPartial]
    aliasBottom <- prepareAlias "direct variable alias bottom function" aliasBottomBody
    aliasBottomOutcome <-
        try
            (execute environment flags "variable alias preserves bottom function WHNF" $ mkApps (Var (helper bindings "driveNat")) [aliasBottom, Var (helper bindings "bottomNat")]) ::
            IO (Either SomeException String)
    case aliasBottomOutcome of
        Left exception | "native-bottom-function" `isInfixOf` displayException exception -> pure ()
        Left exception -> fail ("FAIL: variable alias changed bottom function demand: " ++ displayException exception)
        Right value' -> fail ("FAIL: variable alias hid a bottom function: " ++ show value')
    let opaqueHead = local 3300 "opaque_head" (mkFunctionType unmatchableDataConTy ManyTy natTy natTy)
        opaqueResult = local 3301 "opaque_result" natTy
        opaquePredecessor = local 3302 "opaque_predecessor" natTy
        predecessorIndex = TermVarTy opaquePredecessor
        branchSuccessor = mkTyConApp (promoteDataCon successor) [predecessorIndex]
        nilAtZero =
            mkApps
                (Var (dataConWorkId nil))
                [Type intTy, Type betaZeroType, Coercion (mkNomReflCo betaZeroType)]
        consAtSuccessor =
            mkApps
                (Var (dataConWorkId cons))
                [ Type intTy
                , Type branchSuccessor
                , Type predecessorIndex
                , Coercion (mkNomReflCo branchSuccessor)
                , payload
                , mkApps (Var worker) [Var opaquePredecessor, payload]
                ]
        -- | Consume exact strict-call evidence through an indexed vector cast.
        opaqueKernel useDefault function parameter argument observer =
            Let (Rec [(worker, rhs)])
                $ Lam function
                $ Lam parameter
                $ Case (App (Var function) argument) opaqueResult observationType alternatives
          where
            callIndex = case exprToNativeCaseIndex (App (Var function) argument) of
                Just index' -> index'
                Nothing -> error "Missing checked opaque-call index in runtime fixture"
            zeroCv = proof 3303 callIndex betaZeroType
            successorCv = proof 3304 callIndex branchSuccessor
            aliasCv = proof 3305 (TermVarTy opaqueResult) callIndex
            observationType = exprType (App (Var (helper bindings "observeInt")) payload)
            observe indexCo vector =
                mkApps
                    (Var (helper bindings observer))
                    [ Type callIndex
                    , Cast vector (mkTyConAppCo Representational vec [mkReflCo Representational intTy, indexCo])
                    ]
            alternatives
                | useDefault =
                    [ Alt DEFAULT [aliasCv] $
                        observe (mkCoVarCo aliasCv) (mkApps (Var worker) [Var opaqueResult, payload])
                    ]
                | otherwise =
                    [ Alt (DataAlt zero) [zeroCv] (observe (mkSymCo (mkCoVarCo zeroCv)) nilAtZero)
                    , Alt (DataAlt successor) [opaquePredecessor, successorCv] (observe (mkSymCo (mkCoVarCo successorCv)) consAtSuccessor)
                    ]
        callKernel = opaqueKernel False opaqueHead input (Var input)
        defaultKernel = opaqueKernel True opaqueHead input
        callWith function kernel' seed =
            mkApps (Var (helper bindings "driveNat")) [App kernel' function, seed]
        identityHead = Var (helper bindings "opaqueNat")
        zeroHead = Var (helper bindings "constantZeroNat")
        successorHead = Var (dataConWorkId successor)
        bottomHead = App (Var (helper bindings "bottomFunction")) (Type (varType opaqueHead))
    expect "opaque identity call at zero/two/three" (unlines ["[]:0", "[42,42]:2", "[42,42,42]:3"]) $
        App (Var (helper bindings "driveNats")) (App (callKernel "observeVec") identityHead)
    expect "opaque constant is not injective in its argument" (unlines ["[]:0", "[]:0", "[]:0"]) $
        App (Var (helper bindings "driveNats")) (App (callKernel "observeVec") zeroHead)
    expect "opaque constant ignores its bottom argument" "[]:0" $
        callWith zeroHead (callKernel "observeVec") (Var (helper bindings "bottomNat"))
    expectBottom "opaque identity demands its bottom argument" $
        callWith identityHead (callKernel "observeVec") (Var (helper bindings "bottomNat"))
    expect "opaque successor leaves its predecessor lazy" "42" $
        callWith successorHead (callKernel "observeVecHead") (Var (helper bindings "bottomNat"))
    expectBottom "opaque successor tail demands its predecessor" $
        callWith successorHead (callKernel "observeVec") (Var (helper bindings "bottomNat"))
    expect "opaque identity preserves a finite cyclic prefix" "[42,42,42,42]" $
        callWith identityHead (callKernel "observeVecPrefix") (Var (helper bindings "cyclicNat"))
    expect "opaque identity leaves its partial predecessor lazy at the head" "42" $
        callWith identityHead (callKernel "observeVecHead") (Var (helper bindings "partialNat"))
    expectBottom "opaque identity inspects a partial predecessor only in the tail" $
        callWith identityHead (callKernel "observeVec") (Var (helper bindings "partialNat"))
    let constructedCall = opaqueKernel False opaqueHead input successorValue "observeVecHead"
    forM_
        [ ("variable", callKernel "observeVecHead")
        , ("constructor", constructedCall)
        , ("nested constructor", opaqueKernel False opaqueHead input nestedValue "observeVecHead")
        , ("beta successor", opaqueKernel False opaqueHead input betaSuccessor "observeVecHead")
        , ("DEFAULT constructor", defaultKernel successorValue "observeVecHead")
        ]
        $ \(label, rawKernel) -> do
            lintNative flags ("opaque call " ++ label ++ " input") rawKernel
            let simplified = simpleOptNativeExpr rawKernel
            unless (eqType (exprType rawKernel) (exprType simplified)) $
                fail ("FAIL: opaque call simplification changed the " ++ label ++ " kernel type")
            lintNative flags ("opaque call " ++ label ++ " simplified") simplified
            prepared <- corePrepExpr (hsc_logger environment) nativeCpConfig simplified
            unless (eqType (exprType rawKernel) (exprType prepared)) $
                fail ("FAIL: opaque call CorePrep changed the " ++ label ++ " kernel type")
            lintNative flags ("opaque call " ++ label ++ " prepared") prepared
            unless (hasPreparedCall prepared) $
                fail ("FAIL: opaque call CorePrep removed its actual strict call: " ++ label)
            let function = if label == "variable" then successorHead else identityHead
            expect ("prepared opaque call " ++ label ++ " leaves constructor fields lazy") "42" $
                callWith function prepared (Var (helper bindings "bottomNat"))
    forM_ [("bottom argument", callKernel "observeVec"), ("constructor field", constructedCall)] $ \(label, kernel') -> do
        bottomCallOutcome <-
            try
                (execute environment flags ("opaque function demand precedes " ++ label) $ callWith bottomHead kernel' (Var (helper bindings "bottomNat"))) ::
                IO (Either SomeException String)
        case bottomCallOutcome of
            Left exception | "native-bottom-function" `isInfixOf` displayException exception -> pure ()
            Left exception -> fail ("FAIL: opaque call changed function demand order: " ++ displayException exception)
            Right actual -> fail ("FAIL: opaque call did not demand its bottom function: " ++ show actual)
    expect "opaque DEFAULT evidence retains the computed value" (unlines ["[]:0", "[42,42]:2", "[42,42,42]:3"]) $
        App (Var (helper bindings "driveNats")) (App (defaultKernel (Var input) "observeVec") identityHead)
    let typeOnlyIndex = TermAppTy opaqueHead (TermVarTy input)
        typeOnlyDictionary = local 3306 "opaque_type_only_dictionary" (mkTyConApp eqTyCon [natTy, typeOnlyIndex, typeOnlyIndex])
        typeOnlyProof = mkCoreConApps eqDataCon [Type natTy, Type typeOnlyIndex, Type typeOnlyIndex, Coercion (mkNomReflCo typeOnlyIndex)]
        typeOnlyBody = App (Var (helper bindings "observeInt")) (App (Lam typeOnlyDictionary payload) typeOnlyProof)
        typeOnlyKernel = Lam opaqueHead (Lam input typeOnlyBody)
    unless (opaqueHead `elemVarSet` exprFreeVars typeOnlyBody && input `elemVarSet` exprFreeVars typeOnlyBody) $
        fail "FAIL: type-only opaque call lost its head or argument dependency"
    expect "type-only opaque call ignores its bottom function and argument" "42" $
        callWith bottomHead typeOnlyKernel (Var (helper bindings "bottomNat"))
    let mixedHead = local 3310 "mixed_opaque_head" (mkFunctionType unmatchableDataConTy ManyTy boolTy natTy)
        mixedInput = local 3311 "mixed_opaque_input" boolTy
        mixedKernel = opaqueKernel False mixedHead mixedInput (Var mixedInput) "observeVec"
    expect "opaque Bool-to-Nat call preserves distinct domains" "[42]:1\n[]:0" $
        App (Var (helper bindings "driveBoth")) (App mixedKernel (Var (helper bindings "boolToNat")))
  where
    -- | Find a checked strict call whose argument has reached ANF.
    hasPreparedCall expression = case expression of
        Case (App (Var function) argument) _ _ alternatives
            | isNativeIndexFunctionId function
            , exprIsTrivial argument
            , any (\alt -> isNativeIndexAlt alt || isNativeIndexAliasAlt alt) alternatives ->
                True
        Case scrut _ _ alternatives -> hasPreparedCall scrut || any (\(Alt _ _ body) -> hasPreparedCall body) alternatives
        Lam _ body -> hasPreparedCall body
        Let (NonRec _ value) body -> hasPreparedCall value || hasPreparedCall body
        Let (Rec pairs) body -> any (hasPreparedCall . snd) pairs || hasPreparedCall body
        App function argument -> hasPreparedCall function || hasPreparedCall argument
        Cast body _ -> hasPreparedCall body
        Tick _ body -> hasPreparedCall body
        _ -> False

    namedTyCon label = case find ((== label) . occNameString . nameOccName . tyConName) tycons of
        Just tycon -> pure tycon
        Nothing -> fail ("FAIL: missing loaded type constructor: " ++ label)
    namedDataCon tycon label = case find ((== label) . occNameString . nameOccName . dataConName) (tyConDataCons tycon) of
        Just constructor -> pure constructor
        Nothing -> fail ("FAIL: missing loaded data constructor: " ++ label)

{- Note [Retained domain conversion runtime checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use an actual compiled token constructor and nominal family evidence between
its two closed instances. The retained result mentions an opaque call, so
its argument cannot disappear from the function type. Prepare the raw casted
applications before the ordinary execution entry can simplify their lambdas.
Keep the ignored worker as a lazy let. Native SimpleOpt cannot inline it.
The object control requires native forms after simplification.
These quiet checks preserve the existing stdout golden.
-}

-- | Build closed conversion controls against the actual module identifiers.
-- The first list has values; the second list has expected exception markers.
domainConversionCases :: DynFlags -> [Id] -> [TyCon] -> IO ([(String, String, CoreExpr)], [(String, String, CoreExpr)])
domainConversionCases flags bindings tycons = do
    token <- named "DomainToken"
    nat <- named "Nat"
    tokenCon <- case tyConDataCons token of
        [con] -> pure con
        _ -> fail "FAIL: domain conversion token has no unique constructor"
    zero <- case tyConDataCons nat of
        con : _ -> pure con
        _ -> fail "FAIL: domain conversion Nat has no constructor"
    let parameter = mkTyVar (mkSystemName (unique 12000) (mkTyVarOcc "domain_parameter")) liftedTypeKind
        parameters = [mkAnonTyConBinder parameter]
        family =
            mkFamilyTyCon
                (mkSystemName (unique 12001) (mkTcOcc "RuntimeDomainF"))
                (mkTyConKindWithMatchability unmatchableDataConTy parameters liftedTypeKind)
                parameters
                0
                liftedTypeKind
                Nothing
                (ClosedTypeFamilyTyCon (CTF (Just axiom)))
                Nothing
                NotInjective
        axiom =
            mkBranchedCoAxiom
                Nominal
                (mkSystemName (unique 12002) (mkTcOcc "RuntimeDomainFAx"))
                family
                [mkCoAxBranch [] [] [] [boolTy] boolTy [] noSrcSpan]
        delta = mkTyConAppCo Nominal token [mkAxInstCo Nominal (BranchedAxiom axiom 0) [] []]
        domainA = mkTyConApp token [mkTyConApp family [boolTy]]
        domainB = mkTyConApp token [boolTy]
        natTy = mkTyConTy nat
        x = local 12003 "domain_source" domainA
        y = local 12004 "domain_actual" domainB
        function = local 12005 "domain_opaque_head" (mkFunctionType unmatchableDataConTy ManyTy domainA natTy)
        headInput = local 12006 "domain_head_input" domainA
        headRhs = Lam headInput (Var (dataConWorkId zero))
        call index = TermAppTy function index
        predicate index = mkTyConApp eqTyCon [natTy, call index, call index]
        result index = mkFunctionType unmatchableDataConTy ManyTy (predicate index) intTy
        dictionary = local 12007 "domain_dictionary" (predicate (TermVarTy x))
        answer = mkIntExpr (targetPlatform flags) 42
        ignoredWorker = local 12009 "domain_ignored_worker" originalType
        ignored = Let (NonRec ignoredWorker (Lam x (Lam dictionary answer))) (Var ignoredWorker)
        caseBinder = local 12008 "domain_demand" domainA
        demanded =
            Lam
                x
                ( Case
                    (Var x)
                    caseBinder
                    (result (TermVarTy x))
                    [Alt (DataAlt tokenCon) [] (Lam dictionary answer)]
                )
        conversion = mkDepFunCoWithDomain x (MCo delta) (mkReflCo Representational (result (TermVarTy x)))
        converted body = Cast body conversion
        originalType = mkDepFunTy x (result (TermVarTy x))
        back = CastTy (TermVarTy y) (mkSymCo delta)
        evidence = mkCoreConApps eqDataCon [Type natTy, Type (call back), Type (call back), Coercion (mkNomReflCo (call back))]
        observed body = mkApps (Var (helper bindings "observeInt")) [App (App (converted body) (Var y)) evidence]
        closed body = Let (NonRec function headRhs) body
        apply body argument = closed (App (Lam y (observed body)) argument)
        realToken = App (Var (dataConWorkId tokenCon)) (Type boolTy)
        bottomToken = Var (helper bindings "bottomDomainToken")
        force body = mkApps (Var (helper bindings "force")) [Type (exprType body), body]
        bottomFunction = App (Var (helper bindings "bottomFunction")) (Type originalType)
    unless (isNativeIndexType domainA) $
        fail ("FAIL: runtime domain A is unsupported; token arity " ++ show (tyConArity token) ++ "; kind " ++ showSDoc flags (ppr (tyConKind token)))
    unless (isNativeIndexType domainB) $
        fail "FAIL: runtime domain B is unsupported"
    unless (not (eqType domainA domainB)) $
        fail "FAIL: runtime domain conversion became reflexive"
    unless (exprType (App (converted ignored) (Var y)) `eqType` result back) $
        fail "FAIL: converted retained application has the wrong dependent result"
    pure
        ( [ ("converted retained function accepts an actual token", "42", apply ignored realToken)
          , ("converted retained function ignores bottom token", "42", apply ignored bottomToken)
          , ("converted retained function demands an actual token", "42", apply demanded realToken)
          , ("converted retained partial application ignores bottom token", "forced", closed (App (Lam y (force (App (converted ignored) (Var y)))) bottomToken))
          , ("converted retained function remains a function at WHNF", "forced", closed (force (converted ignored)))
          ]
        , [ ("converted retained function demands bottom token", "native-bottom-domain-token", apply demanded bottomToken)
          , ("converted bottom function remains bottom at WHNF", "native-bottom-function", closed (force (converted bottomFunction)))
          ]
        )
  where
    named label = case find ((== label) . occNameString . nameOccName . tyConName) tycons of
        Just tycon -> pure tycon
        Nothing -> fail ("FAIL: missing domain conversion type: " ++ label)

-- | Execute nominal domain conversions without eta expansion or extra demand.
runDomainConversionChecks :: HscEnv -> DynFlags -> [Id] -> [TyCon] -> IO ()
runDomainConversionChecks environment flags bindings tycons = do
    (values, bottoms) <- domainConversionCases flags bindings tycons
    config <- initCorePrepConfig environment
    let nativeConfig = config{cp_allowNativeDependent = True, cp_arityOpts = Nothing, cp_specEval = False, cp_specEvalDFun = False}
        prepare label expression = do
            lintNative flags (label ++ " original") expression
            prepared <- corePrepExpr (hsc_logger environment) nativeConfig expression
            lintNative flags (label ++ " prepared") prepared
            unless (exprType expression `eqType` exprType prepared) $
                fail ("FAIL: " ++ label ++ "; CorePrep changed the result type")
            pure prepared
        expect label expected expression = do
            prepared <- prepare label expression
            actual <- execute environment flags label prepared
            unless (actual == expected) $
                fail ("FAIL: " ++ label ++ "; expected " ++ show expected ++ ", got " ++ show actual)
        expectBottom label marker expression = do
            prepared <- prepare label expression
            outcome <- try (execute environment flags label prepared) :: IO (Either SomeException String)
            case outcome of
                Left exception | marker `isInfixOf` displayException exception -> pure ()
                Left exception -> fail ("FAIL: " ++ label ++ "; wrong exception: " ++ displayException exception)
                Right actual -> fail ("FAIL: " ++ label ++ "; returned " ++ show actual)
    forM_ values $ \(label, expected, expression) -> expect label expected expression
    forM_ bottoms $ \(label, marker, expression) -> expectBottom label marker expression

-- | Compile scalar-domain conversion and an opaque whole-Pi cast through NCG.
runDomainConversionObjectCheck :: FilePath -> IO ()
runDomainConversionObjectCheck libdir = do
    directory <- makeAbsolute "NativeDependentCoreRun.domain-object"
    createDirectoryIfMissing True directory
    let sourcePath = directory </> "Main.hs"
        assemblyPath = directory </> "domain-conversion.s"
        executablePath = directory </> ("domain-conversion" ++ if os == "mingw32" then ".exe" else "")
        prepDumpPrefix = directory </> "opaque-whole-pi"
        prepDumpPath = prepDumpPrefix ++ ".dump-prep"
        source =
            unlines (map mainHeader (lines helperSource))
                ++ unlines
                    [ "{-# NOINLINE entry #-}"
                    , "entry :: IO String"
                    , "entry = pure \"placeholder\""
                    , "{-# NOINLINE opaqueEntry #-}"
                    , "opaqueEntry :: IO String"
                    , "opaqueEntry = pure \"opaque-placeholder\""
                    , "{-# NOINLINE visibilityWorker #-}"
                    , "visibilityWorker :: Bool -> Bool -> Int"
                    , "visibilityWorker strict b = if strict then b `seq` 42 else 42"
                    , "{-# NOINLINE hiddenEntry #-}"
                    , "hiddenEntry :: Bool -> IO String"
                    , "hiddenEntry _ = pure \"hidden-placeholder\""
                    , "{-# NOINLINE hiddenWhnf #-}"
                    , "hiddenWhnf :: IO String"
                    , "hiddenWhnf = pure \"whnf-placeholder\""
                    , "reportBottom :: String -> IO String -> IO ()"
                    , "reportBottom marker action = do"
                    , "    result <- E.try (action >>= putStrLn) :: IO (Either E.SomeException ())"
                    , "    case result of"
                    , "        Left exception | marker `isInfixOf` E.displayException exception -> putStrLn marker"
                    , "        _ -> fail (\"expected \" ++ marker)"
                    , "main :: IO ()"
                    , "main = do"
                    , "    entry >>= putStrLn"
                    , "    opaqueEntry >>= putStrLn"
                    , "    observeInt (visibilityWorker False bottomBool) >>= putStrLn"
                    , "    hiddenEntry False >>= putStrLn"
                    , "    reportBottom \"native-bottom-bool\" (observeInt (visibilityWorker True bottomBool))"
                    , "    reportBottom \"native-bottom-bool\" (hiddenEntry True)"
                    , "    reportBottom \"native-bottom-function\" (force (bottomFunction :: Bool -> Int))"
                    , "    reportBottom \"native-bottom-function\" hiddenWhnf"
                    ]
        mainHeader "module NativeRuntimeHelpers where" =
            unlines ["module Main where", "import qualified Control.Exception as E", "import Data.List (isInfixOf)"]
        mainHeader line = line
    writeFile sourcePath source
    previousDump <- doesFileExist prepDumpPath
    if previousDump then removeFile prepDumpPath else pure ()
    GHC.runGhc (Just libdir) $ do
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
                    , directory
                    , "-hidir"
                    , directory
                    , "-ddump-prep"
                    , "-ddump-to-file"
                    , "-ddump-file-prefix"
                    , prepDumpPrefix
                    , "-fno-dump-with-ways"
                    , "-dsuppress-uniques"
                    , "-dsuppress-module-prefixes"
                    , "-dno-suppress-coercions"
                    , "-dno-suppress-type-signatures"
                    ]
        unless (null leftovers && backendWritesFiles (backend configured)) $
            fail "FAIL: domain conversion object backend flags"
        void $ GHC.setSessionDynFlags configured{ghcLink = LinkExecutable Dynamic, outputFile_ = Just executablePath}
        target <- GHC.guessTarget sourcePath Nothing Nothing
        GHC.setTargets [target]
        void (GHC.depanal [] False)
        summary <- GHC.getModSummary (mkModule mainUnit (mkModuleName "Main"))
        parsed <- GHC.parseModule summary
        checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
        desugared <- GHC.desugarModule checked
        environment <- GHC.getSession
        let guts = GHC.coreModule desugared
            bindings = map fst (flattenBinds (mg_binds guts))
            named label binder = occNameString (nameOccName (idName binder)) == label
            isEntry = named "entry"
        forM_ ["entry", "opaqueEntry", "hiddenEntry", "hiddenWhnf"] $ \label ->
            unless (length (filter (named label) bindings) == 1) $
                fail ("FAIL: object entry is not unique: " ++ label)
        liftIO $ do
            (values, _) <- domainConversionCases configured bindings (mg_tcs guts)
            expression <- case find (\(label, _, _) -> label == "converted retained function ignores bottom token") values of
                Just (_, _, expression) -> pure expression
                Nothing -> fail "FAIL: missing domain conversion object control"
            nat <- case find ((== "Nat") . occNameString . nameOccName . tyConName) (mg_tcs guts) of
                Just tycon -> pure tycon
                Nothing -> fail "FAIL: missing opaque object Nat"
            vec <- case find ((== "Vec") . occNameString . nameOccName . tyConName) (mg_tcs guts) of
                Just tycon -> pure tycon
                Nothing -> fail "FAIL: missing opaque object Vec"
            let natTy = mkTyConTy nat
                fnTy = mkFunctionType unmatchableDataConTy ManyTy natTy natTy
                a = mkTyVar (mkSystemName (unique 16100) (mkTyVarOcc "opaque_obj_a")) liftedTypeKind
                b = mkTyVar (mkSystemName (unique 16101) (mkTyVarOcc "opaque_obj_b")) liftedTypeKind
                templateF = local 16102 "object_phi_f" fnTy
                templateN = local 16103 "object_phi_n" natTy
                vector element function index =
                    mkTyConApp vec [element, TermAppTy function (TermVarTy index)]
                phi element =
                    mkDepFunTy templateF
                        $ mkDepFunTy templateN
                        $ mkFunctionType unmatchableDataConTy ManyTy (vector element templateF templateN)
                        $ mkFunctionType unmatchableDataConTy ManyTy element element
                q = proof 16104 (phi (mkTyVarTy a)) (phi (mkTyVarTy b))
                h = local 16105 "object_opaque_h" (phi (mkTyVarTy a))
                f = local 16106 "object_actual_f" fnTy
                n = local 16107 "object_actual_n" natTy
                kernelRhs =
                    mkLams [a, b, q, h, f, n] $
                        mkApps (Cast (Var h) (SubCo (mkCoVarCo q))) [Var f, Var n]
                kernelTy =
                    mkInfForAllTys [(unmatchableDataConTy, a), (unmatchableDataConTy, b)]
                        $ mkFunctionType unmatchableDataConTy ManyTy (varType q)
                        $ mkFunctionType unmatchableDataConTy ManyTy (phi (mkTyVarTy a)) (phi (mkTyVarTy b))
                kernelName =
                    mkExternalName
                        (unique 16108)
                        (mkModule mainUnit (mkModuleName "Main"))
                        (mkVarOcc "opaqueWholePiKernel")
                        noSrcSpan
                kernel =
                    setIdUnfolding
                        (setInlinePragma (mkExportedVanillaId kernelName kernelTy) neverInlinePragma)
                        NoUnfolding
                workerF = local 16109 "object_worker_f" fnTy
                workerN = local 16110 "object_worker_n" natTy
                workerVector = local 16111 "object_worker_vector" (vector intTy workerF workerN)
                workerAnswer = local 16112 "object_worker_answer" intTy
                keep = mkLams [workerF, workerN, workerVector, workerAnswer] (Var workerAnswer)
                actualF = local 16113 "object_closed_f" fnTy
                actualN = local 16114 "object_closed_n" natTy
                bottomF = App (Var (helper bindings "bottomFunction")) (Type fnTy)
                bottomVector =
                    App
                        (Var (helper bindings "bottomVec"))
                        (Type (TermAppTy actualF (TermVarTy actualN)))
                closedCall =
                    mkApps
                        (Var kernel)
                        [ Type intTy
                        , Type intTy
                        , Coercion (mkNomReflCo (phi intTy))
                        , keep
                        , Var actualF
                        , Var actualN
                        , bottomVector
                        , mkIntExpr (targetPlatform configured) 42
                        ]
                opaqueExpression =
                    Let (NonRec actualF bottomF)
                        $ Let (NonRec actualN (Var (helper bindings "bottomNat")))
                        $ App (Var (helper bindings "observeInt")) closedCall
                visibilityBinder = local 17220 "object_visibility_binder" boolTy
                visibilityCo = mkDepFunCoWithVis DepFunVisible DepFunInvisible visibilityBinder MRefl (mkReflCo Representational intTy)
                hiddenTy = coercionRKind visibilityCo
                visibleTy = coercionLKind visibilityCo
                strict = local 17221 "object_visibility_strict" boolTy
                actual = local 17222 "object_visibility_argument" boolTy
                hiddenExpression =
                    Lam strict $
                        Let (NonRec actual (Var (helper bindings "bottomBool"))) $
                            App (Var (helper bindings "observeInt")) $
                                App (Cast (App (Var (helper bindings "visibilityWorker")) (Var strict)) visibilityCo) (Var actual)
                hiddenWhnf =
                    mkApps
                        (Var (helper bindings "force"))
                        [ Type hiddenTy
                        , Cast (App (Var (helper bindings "bottomFunction")) (Type visibleTy)) visibilityCo
                        ]
                checkKernel stage program = case filter (named "opaqueWholePiKernel" . fst) (flattenBinds program) of
                    [(binder, rhs)] ->
                        unless
                            ( idType binder `eqType` kernelTy
                                && exprType rhs `eqType` kernelTy
                                && hasNativeDependentExpr rhs
                                && opaquePiCastIsScoped rhs
                            )
                            $ fail ("FAIL: opaque object kernel type or scoped cast after " ++ stage)
                    _ -> fail ("FAIL: opaque object kernel is not unique after " ++ stage)
            unless (templateF /= f && templateN /= n && a /= b) $
                fail "FAIL: opaque object endpoint and actual binders are not distinct"
            let entries =
                    [ ("entry", expression)
                    , ("opaqueEntry", opaqueExpression)
                    , ("hiddenEntry", hiddenExpression)
                    , ("hiddenWhnf", hiddenWhnf)
                    ]
            forM_ (("opaque kernel", kernelRhs) : entries) $ \(label, rhs) -> do
                unless (hasNativeDependentExpr rhs) $
                    fail ("FAIL: " ++ label ++ " lost native forms")
                case lintExpr (initLintConfig configured (kernel : bindings)) rhs of
                    Nothing -> pure ()
                    Just errors -> fail ("FAIL: " ++ label ++ " Lint: " ++ showSDoc configured (vcat (bagToList errors)))
            unless (exprType kernelRhs `eqType` kernelTy) $
                fail "FAIL: abstract object kernel type mismatch"
            forM_ entries $ \(label, rhs) ->
                unless (exprType rhs `eqType` idType (helper bindings label)) $
                    fail ("FAIL: object entry type mismatch: " ++ label)
            let replacePair (binder, rhs)
                    | Just replacement <- lookup (occNameString (nameOccName (idName binder))) entries
                    = (setIdUnfolding binder NoUnfolding, replacement)
                    | otherwise = (binder, rhs)
                replace (NonRec binder rhs) = uncurry NonRec (replacePair (binder, rhs))
                replace (Rec pairs) = Rec (map replacePair pairs)
                injected = guts{mg_binds = NonRec kernel kernelRhs : map replace (mg_binds guts)}
            checkKernel "injection" (mg_binds injected)
            simplified <- hscSimplify environment [] injected
            unless (any (\(binder, rhs) -> isEntry binder && hasNativeDependentExpr rhs) (flattenBinds (mg_binds simplified))) $
                fail "FAIL: domain conversion object entry lost native forms before CorePrep"
            checkKernel "simplification" (mg_binds simplified)
            (code, _) <- hscTidy environment simplified
            checkKernel "tidying" (cg_binds code)
            (assembly, stub, foreigns, _, _) <- hscGenHardCode environment code (GHC.ms_location summary) assemblyPath
            existsDump <- doesFileExist prepDumpPath
            unless existsDump $ fail "FAIL: opaque object actual CorePrep dump is missing"
            actualPrepDump <- readFile prepDumpPath
            let startsKernel line = line == "opaqueWholePiKernel" || "opaqueWholePiKernel =" `isPrefixOf` line
                kernelDump = unlines $ takeWhile (not . null) $ dropWhile (not . startsKernel) (lines actualPrepDump)
            unless
                ( all
                    (`isInfixOf` kernelDump)
                    ["opaqueWholePiKernel", "@opaque_obj_a", "@opaque_obj_b", "foreach", "`cast`", "Sub"]
                )
                $ fail "FAIL: actual object CorePrep dump lost the abstract opaque kernel cast"
            writeFile (directory </> "opaque-whole-pi-prep-binding.core") kernelDump
            unless (isNothing stub && null foreigns) $
                fail "FAIL: unexpected domain conversion object foreign output"
            object <- compileFile environment NoStop (assembly, Nothing)
            case object of
                Just path -> do
                    exists <- doesFileExist path
                    unless exists (fail "FAIL: domain conversion produced no object file")
                    oneShot environment NoStop [(path, Just StopLn)]
                Nothing -> fail "FAIL: domain conversion assembly produced no object path"
    (status, actual, errors) <- readProcessWithExitCode executablePath [] ""
    let expected =
            unlines
                [ "42"
                , "42"
                , "42"
                , "42"
                , "native-bottom-bool"
                , "native-bottom-bool"
                , "native-bottom-function"
                , "native-bottom-function"
                ]
    unless (status == ExitSuccess && actual == expected && null errors) $
        fail ("FAIL: domain conversion object result: " ++ show (status, actual, errors))

-- | Lint a closed native expression before it reaches the backend API.
lintNative :: DynFlags -> String -> CoreExpr -> IO ()
lintNative flags label expression = do
    unless (hasNativeDependentExpr expression) (fail ("FAIL: " ++ label ++ "; native forms disappeared before compilation"))
    lintCheckedCore flags label expression

-- | Check an expression from a module that can contain ordinary and dependent bindings.
lintCheckedCore :: DynFlags -> String -> CoreExpr -> IO ()
lintCheckedCore flags label expression =
    case lintExpr (initLintConfig flags []) expression of
        Nothing -> pure ()
        Just errors -> fail ("FAIL: " ++ label ++ "; Lint rejected:\n" ++ showSDoc flags (vcat (bagToList errors)))

-- | Compile and observe a checked IO String expression without a host cast.
execute :: HscEnv -> DynFlags -> String -> CoreExpr -> IO String
execute environment flags label expression = do
    lintNative flags label expression
    (value, _, _) <- hscCompileCoreExpr environment noSrcSpan expression
    evalString (hscInterp environment) value

-- | Check that a change in retained visibility preserves argument demand.
runDepFunVisibilityChecks :: HscEnv -> DynFlags -> [Id] -> IO ()
runDepFunVisibilityChecks environment flags bindings = do
    let index = local 17210 "hidden_bool" boolTy
        actual = local 17211 "hidden_actual" boolTy
        scrutinee = local 17212 "hidden_scrutinee" boolTy
        payload = mkIntExpr (targetPlatform flags) 42
        bridge = mkDepFunCoWithVis DepFunVisible DepFunInvisible index MRefl (mkReflCo Representational intTy)
        observe body =
            Let (NonRec actual (Var (helper bindings "bottomBool"))) $
                App (Var (helper bindings "observeInt"))
                    (App (Cast (Lam index body) bridge) (Var actual))
    ignored <- execute environment flags "hidden retained keep" (observe payload)
    unless (ignored == "42") $ fail "FAIL: hidden retained keep returned a different value"
    demanded <-
        try (execute environment flags "hidden retained demand" (observe (Case (Var index) scrutinee intTy [Alt DEFAULT [] payload])))
            :: IO (Either SomeException String)
    case demanded of
        Left exception
            | "native-bottom-bool" `isInfixOf` displayException exception -> pure ()
            | otherwise -> fail ("FAIL: hidden retained demand raised another exception: " ++ displayException exception)
        Right actualResult -> fail ("FAIL: hidden retained demand returned " ++ show actualResult)

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
    liftIO $ runDepFunVisibilityChecks environment flags bindings
    liftIO $ runDomainConversionChecks environment flags bindings tycons
    liftIO $ runOpaquePiWholeCastChecks environment flags bindings tycons
    runSourceCallIndexChecks
    runSourceChecks
    runSourceRejectChecks

-- | Check source-call certificates against their actual Core bindings.
runSourceCallIndexChecks :: GHC.Ghc ()
runSourceCallIndexChecks = do
    parsed <- parseSourceTarget "NativeSourceCallIndices" $ unlines
        [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes #-}"
        , "module NativeSourceCallIndices where"
        , "flipNative :: Bool -> Bool"
        , "flipNative False = True"
        , "flipNative True = False"
        , "type CheckedFlip = flipNative 'False"
        , "stayNative :: Bool -> Bool"
        , "stayNative False = False"
        , "stayNative True = True"
        ]
    checked <- GHC.typecheckModule GHC.StartAndKeepRunningTcMPlugins parsed
    desugared <- GHC.desugarModule checked
    flags <- GHC.getSessionDynFlags
    let pairs = flattenBinds (mg_binds (GHC.coreModule desugared))
        binding name = case find ((== name) . occNameString . nameOccName . varName . fst) pairs of
            Just pair -> pure pair
            Nothing -> fail ("FAIL: missing source-call fixture binding " ++ name)
    (certified, rhs) <- binding "flipNative"
    (outer, outerRhs) <- binding "stayNative"
    family <- case sourceTableId_maybe certified of
        Just table -> pure table
        Nothing -> fail "FAIL: checked source function lost its equation certificate"
    liftIO $ do
        let operand = local 17900 "source_call_operand" boolTy
            retained = local 17901 "source_call_retained" boolTy
            index = TermVarTy retained
            dictionary = local 17902 "source_call_dictionary" (mkTyConApp eqTyCon [boolTy, index, index])
            keep = Lam retained (Lam dictionary (mkIntExpr (targetPlatform flags) 42))
            call fun argument = App (Var fun) (Var argument)
            expected = mkTyConApp family [TermVarTy operand]
            bound = GHC.Types.Id.setIdNotExported (localiseId certified)
            expression binder value occurrence argument =
                Let (NonRec binder value) (Lam argument (App keep (call occurrence argument)))
            baseline = expression bound rhs bound operand
            renamed = setIdUnique bound (unique 17903)
            plain = setIdDetails bound VanillaId
            listOperand = local 17904 "source_call_list_operand" (mkListTy boolTy)
            wrongRhs = Lam listOperand (Var (dataConWorkId falseDataCon))
            wrongDomain = GHC.Types.Id.setIdType bound (exprType wrongRhs)
            reject label fragment value = case lintExpr (initLintConfig flags []) value of
                Just errors | fragment `isInfixOf` showSDoc flags (vcat (bagToList errors)) -> pure ()
                Just errors -> fail ("FAIL: " ++ label ++ "; wrong Lint errors:\n" ++ showSDoc flags (vcat (bagToList errors)))
                Nothing -> fail ("FAIL: Lint accepted " ++ label)
            checkIndex label value = case value of
                Just actual | actual `eqType` expected -> pure ()
                _ -> fail ("FAIL: " ++ label ++ "; source call selected another index")
        checkIndex "source call" (nativeSourceCallIndex (call bound operand))
        checkIndex "retained source call" (retainedArgIndex (call bound operand))
        let sourceCall = call bound operand
            trueWorker = dataConWorkId trueDataCon
            -- Include the LocalId call head in the expression range's scope.
            first = CoreSubst.extendIdSubst
                (CoreSubst.mkEmptySubst (mkInScopeSet (exprFreeVars sourceCall))) retained sourceCall
            second = CoreSubst.extendIdSubst
                (CoreSubst.mkEmptySubst (mkInScopeSet emptyVarSet)) operand (Var trueWorker)
            composed = composeTCvSubst second first
            sequentialIndex = CoreSubst.substTy second (CoreSubst.substTy first index)
            composedIndex = CoreSubst.substTy composed index
            expectedTrue = mkTyConApp family [boolIndex True]
            sequentialCall = CoreSubst.substExpr (CoreSubst.extendSubstInScope second bound)
                (CoreSubst.substExpr first (Var retained))
            composedCall = CoreSubst.substExpr composed (Var retained)
        unless (composedIndex `eqType` sequentialIndex && composedIndex `eqType` expectedTrue) $
            fail "FAIL: composed source-call substitution selected another family index"
        case (composedCall, sequentialCall) of
            (App (Var actual) (Var value), App (Var sequential) (Var sequentialValue))
                | actual == bound, sequential == bound
                , sourceTableId_maybe actual == Just family
                , sourceTableId_maybe sequential == Just family
                , value == trueWorker, sequentialValue == trueWorker -> pure ()
            _ -> fail "FAIL: composed source-call substitution lost its actual runtime owner"
        unless (isNothing (sourceTableId_maybe renamed)
                && isNothing (nativeSourceCallIndex (call renamed operand))) $
            fail "FAIL: copied equation certificate accepted a different Name"
        unless (isNothing (nativeSourceCallIndex (call wrongDomain listOperand))
                && isNothing (retainedArgIndex (call wrongDomain listOperand))) $
            fail "FAIL: source call accepted a different family domain"
        lintNative flags "certified source-call baseline" baseline
        let outerBound = GHC.Types.Id.setIdNotExported (localiseId outer)
            nestedCall = App (Var outerBound) sourceCall
            nested innerBinder =
                Let (NonRec outerBound outerRhs) $
                Let (NonRec innerBinder rhs) $
                Lam operand (App keep nestedCall)
        outerFamily <- maybe (fail "FAIL: missing outer source certificate") pure (sourceTableId_maybe outer)
        unless (maybe False (\actual -> actual `eqType` mkTyConApp outerFamily [expected])
                (nativeComputedIndex nestedCall)) $
            fail "FAIL: nested source call lost the inner family index"
        let wrapped = mkCoreConApps justDataCon [Type boolTy, nestedCall]
            wrappedIndex = mkTyConApp (promoteDataCon justDataCon)
                [boolTy, mkTyConApp outerFamily [expected]]
        unless (isNothing (nativeSourceCallIndex wrapped) &&
                maybe False (`eqType` wrappedIndex) (retainedArgIndex wrapped)) $
            fail "FAIL: constructor around nested source calls selected a different index"
        let nestedSubst = CoreSubst.extendIdSubst
                (CoreSubst.mkEmptySubst (mkInScopeSet (exprFreeVars nestedCall))) retained nestedCall
            nestedComposed = composeTCvSubst second nestedSubst
            indexAtTrue = mkTyConApp outerFamily [expectedTrue]
        unless (CoreSubst.substTy nestedComposed index `eqType` indexAtTrue &&
                maybe False (`eqType` indexAtTrue)
                    (nativeComputedIndex (CoreSubst.substExpr nestedComposed (Var retained)))) $
            fail "FAIL: substitution lost a nested source call or its operand"
        lintNative flags "nested source-call baseline" (nested bound)
        reject "uncertified inner source binding" "different equations at its binding" (nested plain)
        let fieldType = varType bound
            boxType = mkTyConApp maybeTyCon [fieldType]
            box = local 17905 "source_function_box" boxType
            boxIndex = TermVarTy box
            evidence = local 17906 "box_dictionary" (mkTyConApp eqTyCon [boxType, boxIndex, boxIndex])
            ignoreBox = Lam box (Lam evidence (mkIntExpr (targetPlatform flags) 42))
            field = mkCoreConApps justDataCon [Type fieldType, Var bound]
            fieldIndex = mkTyConApp (promoteDataCon justDataCon) [fieldType, TermVarTy plain]
            reflexive = mkCoreConApps eqDataCon
                [Type boxType, Type fieldIndex, Type fieldIndex, Coercion (mkNomReflCo fieldIndex)]
        -- Passing the function as a field uses its identity but never calls its equations.
        lintNative flags "uncalled source function in a constructor field"
            (Let (NonRec plain rhs) (mkApps ignoreBox [field, reflexive]))
        reject "uncertified source binding" "different equations at its binding"
            (expression plain rhs bound operand)
        reject "copied source certificate" "Unsupported retained runtime argument"
            (expression renamed rhs renamed operand)
        reject "source-call family domain mismatch" "Unsupported retained runtime argument"
            (expression wrongDomain wrongRhs wrongDomain listOperand)
        let simplified = simpleOptNativeExpr baseline
        lintNative flags "computed source-call beta after SimpleOpt" simplified
        unless (exprType baseline `eqType` exprType simplified) $
            fail "FAIL: computed source-call beta changed its dependent result"
        case simplified of
            Let (NonRec _ _) (Lam _ (App (Lam _ _) argument)) ->
                checkIndex "shared source-call beta argument" (nativeSourceCallIndex argument)
            _ -> fail "FAIL: SimpleOpt did not retain the computed source-call beta argument"

{- Note [Constructed call activation source checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check fresh worker applications at one and two constructor layers without
payload annotations. Keep this group separate from the inherited bare-Var
activation observations and from the registered stdout oracle.
-}

-- | Add unannotated constructed-call workers to the local complete group.
retainedConstructedActivationSource :: String -> String -> String
retainedConstructedActivationSource moduleName observation =
    unlines $ concatMap insert (lines (retainedFunctionBinderSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) declarations ++ [line]
    insert line = [line]
    declarations =
        [ "activateConstructed :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "activateConstructed f n = case f (S n) of"
        , "    Z -> length (vecList (idCall f (S n) VNil))"
        , "    S k -> length (vecList (idCall f (S n) (VCons 42 (replicateNative @Int k 42))))"
        , "activateNested :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Int"
        , "activateNested f n = case f (S (S n)) of"
        , "    Z -> length (vecList (idCall f (S (S n)) VNil))"
        , "    S k -> length (vecList (idCall f (S (S n)) (VCons 42 (replicateNative @Int k 42))))"
        , "activationConstant :: Nat -> Nat"
        , "activationConstant n = Z"
        ]

-- | Check eight independent finite values without changing the stdout oracle.
runConstructedCallActivationSourceChecks :: GHC.Ghc ()
runConstructedCallActivationSourceChecks = do
    let moduleName = "NativeSourceConstructedCallActivation"
        observation =
            unlines
                [ "do one <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (activateConstructed f n))"
                , "        oneConstant <- driveNat (\\n -> let { f :: Nat -> Nat; f = activationConstant } in observeInt (activateConstructed f n)) (S (S Z))"
                , "        two <- driveNats (\\n -> let { f :: Nat -> Nat; f = opaqueNat } in observeInt (activateNested f n))"
                , "        twoConstant <- driveNat (\\n -> let { f :: Nat -> Nat; f = activationConstant } in observeInt (activateNested f n)) (S (S Z))"
                , "        pure (one ++ unlines [oneConstant] ++ two ++ unlines [twoConstant])"
                ]
        expected = ["1", "3", "4", "0", "2", "4", "5", "0"]
    expression <- sourceEntry moduleName (retainedConstructedActivationSource moduleName observation)
    environment <- GHC.getSession
    flags <- GHC.getSessionDynFlags
    actual <- liftIO $ execute environment flags "constructed call activation" expression
    unless (lines actual == expected) $
        fail ("FAIL: constructed call activation: " ++ show actual)

{- Note [Multi-constructor package source checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check two constructor declaration orders with source-written telescopes.
Each selected row passes its own retained field Id to PackVec. These quiet
checks use the existing object-module runner. Constructor workers must be
linked from their compiled module. Extracting entry Core alone cannot supply
the generated DataCon workers. Keep the registered stdout oracle and all
inherited groups unchanged.
-}

-- | Add source package declarations and local conversion workers.
retainedMultiPackageSource :: String -> String -> String
retainedMultiPackageSource moduleName observation =
    unlines $ concatMap insert (lines (retainedNatSource moduleName observation))
  where
    insert line@"      run :: Nat -> IO String" = map ("      " ++) workers ++ [line]
    insert line = [line]
    workers =
        [ "makeMulti :: Nat -> Availability Int"
        , "makeMulti n = Available n (replicateNative @Int n 42)"
        , "makeMultiReverse :: Nat -> ReverseAvailability Int"
        , "makeMultiReverse n = PresentFirst n (replicateNative @Int n 42)"
        , "multiToPacked :: forall a. Availability a -> Maybe (PackedVec a)"
        , "multiToPacked Unavailable = Nothing"
        , "multiToPacked (Available n xs) = Just (PackVec n xs)"
        , "multiReverseToPacked :: forall a. ReverseAvailability a -> Maybe (PackedVec a)"
        , "multiReverseToPacked (PresentFirst n xs) = Just (PackVec n xs)"
        , "multiReverseToPacked AbsentLast = Nothing"
        , "observeMulti :: Maybe (PackedVec Int) -> IO String"
        , "observeMulti Nothing = pure \"absent\""
        , "observeMulti (Just (PackVec n xs)) = pure (show (vecList xs))"
        ]

-- | Compile source constructors with the ordinary runtime helpers.
multiPackageHelperSource :: String
multiPackageHelperSource =
    unlines (["{-# LANGUAGE ExperimentalDependentTypes #-}"] ++ lines helperSource ++ packageDeclarations)
  where
    packageDeclarations =
        [ "data PackedVec a where"
        , "  PackVec :: foreach (n :: Nat) -> Vec a n -> PackedVec a"
        , "data Availability a where"
        , "  Unavailable :: Availability a"
        , "  Available :: foreach (n :: Nat) -> Vec a n -> Availability a"
        , "data ReverseAvailability a where"
        , "  PresentFirst :: foreach (n :: Nat) -> Vec a n -> ReverseAvailability a"
        , "  AbsentLast :: ReverseAvailability a"
        ]

-- | Check six IORef-fed finite packages and both absent alternatives quietly.
runMultiPackageSourceChecks :: FilePath -> IO ()
runMultiPackageSourceChecks libdir = do
    let moduleName = "Main"
        observation =
            unlines
                [ "do forward <- driveNats (\\n -> observeMulti (multiToPacked (makeMulti n)))"
                , "        reverse <- driveNats (\\n -> observeMulti (multiReverseToPacked (makeMultiReverse n)))"
                , "        absent <- observeMulti (multiToPacked (Unavailable :: Availability Int))"
                , "        reverseAbsent <- observeMulti (multiReverseToPacked (AbsentLast :: ReverseAvailability Int))"
                , "        pure (forward ++ reverse ++ unlines [absent, reverseAbsent])"
                ]
        expected = ["[]", "[42,42]", "[42,42,42]", "[]", "[42,42]", "[42,42,42]", "absent", "absent"]
    let consumer = retainedMultiPackageSource moduleName observation ++ "\nmain :: IO ()\nmain = entry >>= putStr\n"
    runModuleCase libdir "multi-constructor-quiet" "NativeRuntimeHelpers" multiPackageHelperSource consumer expected

{- Note [Function package module source checks]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Compile source constructors into an object before executing their consumers.
The function field, scalar field and dependent payload share actual runtime
Ids. Keep this source group separate from the Core-only and earlier groups.
-}

-- | Check a generic stored function telescope through a disk interface.
runFunctionPackageModuleChecks :: FilePath -> IO ()
runFunctionPackageModuleChecks libdir = do
    packed <- readFile "PackedVecLibrary.hs"
    let (header, declaration) = break (isInfixOf "module PackedVecLibrary ") (lines packed)
        producer = unlines (header ++ ["module NativeFunctionPackageAPI where"] ++ drop 1 (dropWhile (/= ") where") declaration) ++ functionFields)
        functionFields =
            [ ""
            , "-- | Bind the function and input used by the following vector field."
            , "data FunctionPack a where"
            , "    FunctionPack :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> FunctionPack a"
            , ""
            , "-- | Give a lifted error an explicit native result type."
            , "raisePayload :: forall a. String -> a"
            , "raisePayload = error"
            , ""
            , "-- | Build the vector under the checked equality for the exact whole call."
            , "buildPack :: forall a. (Nat -> Nat) -> Nat -> a -> FunctionPack a"
            , "buildPack f n value = case f n of"
            , "    Z -> let ys :: Vec a (f n); ys = VNil in FunctionPack f n ys"
            , "    S k -> let ys :: Vec a (f n); ys = VCons value (replicateNative @a k value) in FunctionPack f n ys"
            , ""
            , "-- | Store all fields without executing the function or inspecting the input."
            , "lazyPack :: forall a. (Nat -> Nat) -> Nat -> FunctionPack a"
            , "lazyPack f n ="
            , "    let ys :: Vec a (f n)"
            , "        ys = raisePayload @(Vec a (f n)) \"function-pack-payload\""
            , "     in FunctionPack f n ys"
            , ""
            , "-- | Reconstruct the package with the actual selected field identities."
            , "repackFunction :: FunctionPack a -> FunctionPack a"
            , "repackFunction (FunctionPack f n xs) = FunctionPack f n xs"
            , ""
            , "-- | Inspect only the outer package constructor."
            , "ignoreFunction :: FunctionPack a -> Int"
            , "ignoreFunction (FunctionPack f n xs) = 42"
            , ""
            , "-- | Demand the original input independently from the function and payload."
            , "inputLength :: FunctionPack a -> Int"
            , "inputLength (FunctionPack f n xs) = natToInt n"
            , ""
            , "-- | Demand the computed result and payload through the selected function Id."
            , "inspectFunction :: FunctionPack a -> (Int, [a])"
            , "inspectFunction (FunctionPack f n xs) = case f n of"
            , "    Z -> (0, toList xs)"
            , "    S k -> (natToInt (S k), toList xs)"
            , ""
            , "-- | Observe the payload without executing the stored function."
            , "payloadOnly :: FunctionPack a -> [a]"
            , "payloadOnly (FunctionPack f n xs) = toList xs"
            , ""
            , "-- | Demand only one payload constructor."
            , "headOnly :: FunctionPack Int -> Int"
            , "headOnly (FunctionPack f n VNil) = 0"
            , "headOnly (FunctionPack f n (VCons x rest)) = x"
            , ""
            , "-- | Keep both selected function and index identities in separate scopes."
            , "pairPayloads :: FunctionPack a -> FunctionPack b -> ([a], [b])"
            , "pairPayloads (FunctionPack f n xs) (FunctionPack g m ys) = (toList xs, toList ys)"
            , ""
            , "-- | Resume the second row outside the first row's field scope."
            , "guardedIgnore :: Bool -> FunctionPack a -> Int"
            , "guardedIgnore flag (FunctionPack f n xs) | flag = ignoreFunction (FunctionPack f n xs)"
            , "guardedIgnore _ (FunctionPack f n xs) = ignoreFunction (FunctionPack f n xs)"
            ]
        consumer = unlines
            [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications #-}"
            , ""
            , "-- | Prospective finite observations through a separate producer interface."
            , "module Main (main) where"
            , ""
            , "import Control.Exception (ErrorCall, displayException, evaluate, try)"
            , "import Control.Monad (forM_, unless)"
            , "import Data.IORef (newIORef, readIORef)"
            , "import Data.List (isInfixOf)"
            , "import NativeFunctionPackageAPI"
            , ""
            , "-- | Preserve an opaque identity head at runtime."
            , "{-# NOINLINE identity #-}"
            , "identity :: Nat -> Nat"
            , "identity n = n"
            , ""
            , "-- | Preserve a head that adds one lazy successor."
            , "{-# NOINLINE successor #-}"
            , "successor :: Nat -> Nat"
            , "successor n = S n"
            , ""
            , "-- | Preserve a noninjective head that ignores its input."
            , "{-# NOINLINE constantZero #-}"
            , "constantZero :: Nat -> Nat"
            , "constantZero n = Z"
            , ""
            , "-- | Fail when the stored function is executed."
            , "bottomHead :: Nat -> Nat"
            , "bottomHead = error \"function-pack-head\""
            , ""
            , "-- | Fail when the original stored input is demanded."
            , "bottomIndex :: Nat"
            , "bottomIndex = error \"function-pack-index\""
            , ""
            , "-- | Supply a bounded observation of an infinite successor spine."
            , "cyclicIndex :: Nat"
            , "cyclicIndex = S cyclicIndex"
            , ""
            , "-- | Check the independent expected result before printing it."
            , "check :: (Eq a, Show a) => a -> a -> IO ()"
            , "check expected actual = do"
            , "    unless (expected == actual) (error (\"function package mismatch: \" ++ show actual))"
            , "    print actual"
            , ""
            , "-- | Require a named error instead of any arbitrary failure."
            , "expectMarker :: String -> IO a -> IO ()"
            , "expectMarker marker action = do"
            , "    result <- try action"
            , "    case result of"
            , "        Left (exception :: ErrorCall)"
            , "            | marker `isInfixOf` displayException exception -> putStrLn marker"
            , "        Left exception -> error (displayException exception)"
            , "        Right _ -> error (\"expected \" ++ marker)"
            , ""
            , "-- | Observe each function at independently loaded runtime inputs."
            , "main :: IO ()"
            , "main = do"
            , "    forM_ [(Z, (0, []), (1, [42])), (S (S Z), (2, [42,42]), (3, [42,42,42])), (S (S (S Z)), (3, [42,42,42]), (4, [42,42,42,42]))] $ \\(seed, same, next) -> do"
            , "        a <- newIORef seed >>= readIORef"
            , "        b <- newIORef seed >>= readIORef"
            , "        c <- newIORef seed >>= readIORef"
            , "        check same (inspectFunction (repackFunction (buildPack @Int identity a 42)))"
            , "        check next (inspectFunction (repackFunction (buildPack @Int successor b 42)))"
            , "        check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero c 42))"
            , "    _ <- evaluate (lazyPack @Int bottomHead bottomIndex)"
            , "    _ <- evaluate (repackFunction (lazyPack @Int bottomHead bottomIndex))"
            , "    putStrLn \"package and repack: whnf\""
            , "    check 42 (ignoreFunction (lazyPack @Int bottomHead bottomIndex))"
            , "    check 42 (guardedIgnore False (lazyPack @Int bottomHead cyclicIndex))"
            , "    check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero bottomIndex 42))"
            , "    check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero cyclicIndex 42))"
            , "    check 42 (headOnly (buildPack @Int successor bottomIndex 42))"
            , "    check [42,42,42] (take 3 (payloadOnly (buildPack @Int identity cyclicIndex 42)))"
            , "    check ([42,42], [42,42,42]) (pairPayloads (buildPack @Int identity (S (S Z)) 42) (buildPack @Int successor (S (S Z)) 42))"
            , "    expectMarker \"function-pack-payload\" $ evaluate (length (payloadOnly (lazyPack @Int bottomHead bottomIndex)))"
            , "    expectMarker \"function-pack-head\" $ evaluate (fst (inspectFunction (lazyPack @Int bottomHead bottomIndex)))"
            , "    expectMarker \"function-pack-index\" $ evaluate (inputLength (lazyPack @Int bottomHead bottomIndex))"
            , "    expectMarker \"function-pack-index\" $ evaluate (fst (inspectFunction (lazyPack @Int identity bottomIndex)))"
            , "    expectMarker \"function-pack-index\" $ evaluate (length (payloadOnly (buildPack @Int successor bottomIndex 42)))"
            , "    expectMarker \"function-pack-outer\" $ evaluate (ignoreFunction (error \"function-pack-outer\" :: FunctionPack Int))"
            , "    check (2, \"qq\") (inspectFunction (buildPack @Char identity (S (S Z)) 'q'))"
            ]
        expected =
            [ "(0,[])"
            , "(1,[42])"
            , "(0,[])"
            , "(2,[42,42])"
            , "(3,[42,42,42])"
            , "(0,[])"
            , "(3,[42,42,42])"
            , "(4,[42,42,42,42])"
            , "(0,[])"
            , "package and repack: whnf"
            , "42"
            , "42"
            , "(0,[])"
            , "(0,[])"
            , "42"
            , "[42,42,42]"
            , "([42,42],[42,42,42])"
            , "function-pack-payload"
            , "function-pack-head"
            , "function-pack-index"
            , "function-pack-index"
            , "function-pack-index"
            , "function-pack-outer"
            , "(2,\"qq\")"
            ]
    unless (not (null declaration) && ") where" `elem` declaration) $
        fail "FAIL: PackedVecLibrary module header changed"
    runModuleCase libdir "function-package-source" "NativeFunctionPackageAPI" producer consumer expected

-- | Check function fields, family transport and separate lazy demand boundaries.
runFunctionFieldModuleChecks :: FilePath -> IO ()
runFunctionFieldModuleChecks libdir = do
    library <- readFile "FunctionFieldLibrary.hs"
    client <- readFile "FunctionFieldClient.hs"
    let warnings = ["-Werror=inaccessible-code", "-Werror=overlapping-patterns"]
        consumer = unlines
            [ "{-# LANGUAGE ScopedTypeVariables #-}"
            , ""
            , "module Main (main) where"
            , ""
            , "import Control.Exception (ErrorCall, displayException, evaluate, try)"
            , "import Control.Monad (unless)"
            , "import Data.IORef (newIORef, readIORef)"
            , "import Data.List (isInfixOf)"
            , "import FunctionFieldClient"
            , "import FunctionFieldLibrary"
            , ""
            , "-- | Check the independent expected value before printing it."
            , "check :: (Eq a, Show a) => a -> a -> IO ()"
            , "check expected actual = do"
            , "    unless (expected == actual) (error (\"function field mismatch: \" ++ show actual))"
            , "    print actual"
            , ""
            , "-- | Require the selected demand failure instead of an arbitrary exception."
            , "expectMarker :: String -> Int -> IO ()"
            , "expectMarker marker value = do"
            , "    result <- try (evaluate value)"
            , "    case result of"
            , "        Left (exception :: ErrorCall)"
            , "            | marker `isInfixOf` displayException exception -> putStrLn marker"
            , "        Left exception -> error (displayException exception)"
            , "        Right _ -> error (\"expected \" ++ marker)"
            , ""
            , "-- | Check finite results and the separate lazy demand boundaries."
            , "main :: IO ()"
            , "main = do"
            , "    bFalse <- newIORef False >>= readIORef"
            , "    bTrue <- newIORef True >>= readIORef"
            , "    check 2 (inspect (build not bFalse))"
            , "    check 1 (inspect (build not bTrue))"
            , "    check 2 (inspectViaCall (repack (build id bTrue)))"
            , "    check True (proofOnly (build not bFalse))"
            , "    _ <- evaluate (loose (error \"function-field-container\") (error \"function-field-argument\"))"
            , "    _ <- evaluate (repack (loose (error \"function-field-container\") (error \"function-field-argument\")))"
            , "    putStrLn \"package and repack: whnf\""
            , "    check 42 (outer (loose (error \"function-field-container\") (error \"function-field-argument\")))"
            , "    check 1 (which (loose (Just (error \"function-field-function\")) (error \"function-field-argument\")))"
            , "    check 0 (which (loose Nothing (error \"function-field-argument\")))"
            , "    expectMarker \"function-field-container\" (which (loose (error \"function-field-container\") True))"
            , "    expectMarker \"function-field-function\" (inspect (loose (Just (error \"function-field-function\")) True))"
            , "    expectMarker \"function-field-argument\" (inspect (loose (Just id) (error \"function-field-argument\")))"
            , "    expectMarker \"function-field-proof\" (inspect (loose (Just (const False)) True))"
            ]
        expected =
            [ "2", "1", "2", "True", "package and repack: whnf", "42", "1", "0"
            , "function-field-container", "function-field-function"
            , "function-field-argument", "function-field-proof"
            ]
    runModuleCases libdir "function-field-source"
        [("FunctionFieldLibrary", library), ("FunctionFieldClient", client)]
        warnings consumer expected
    artifacts <- makeAbsolute ("NativeDependentCoreRun.modules" </> "function-field-source" </> "artifacts")
    forM_
            [ ("RejectReductionDepth", 40404, "Reduction stack overflow", unlines
                [ "{-# LANGUAGE DataKinds, ExperimentalDependentTypes, KindSignatures #-}"
                , "{-# LANGUAGE TypeApplications, TypeFamilies, UndecidableInstances #-}"
                , "{-# OPTIONS_GHC -freduction-depth=12 #-}"
                , "module RejectReductionDepth where"
                , "import Data.Kind (Type)"
                , "import GHC.Exts (FUN, U, Multiplicity (Many))"
                , "type BoolEndo = FUN @U 'Many Bool Bool"
                , "type family Spin (b :: Bool) :: Type where"
                , "    Spin b = Spin b"
                , "bad :: foreach (f :: BoolEndo) -> foreach (b :: Bool) -> Spin (f b)"
                , "bad f b = case f b of"
                , "    False -> error \"unreachable\""
                , "    True -> error \"unreachable\""
                ]
            )
            , ("RejectCrossPayload", 5617, "consumeFlag", unlines
                [ "{-# LANGUAGE ExperimentalDependentTypes, GADTs #-}"
                , "module RejectCrossPayload where"
                , "import FunctionFieldLibrary"
                , "wrong :: Box -> Box -> Int"
                , "wrong (Box (Just f) b proof) (Box (Just g) c other) = consumeFlag f b other"
                ]
            )
            , ("RejectMatchableShape", 40564, "Inaccessible code", unlines
                [ "{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeApplications #-}"
                , "{-# LANGUAGE ExperimentalDependentTypes, ExplicitForAll #-}"
                , "module RejectMatchableShape where"
                , "import GHC.Exts (FUN, M, Multiplicity (Many))"
                , "data Flag (b :: Bool) where"
                , "    IsFalse :: Flag 'False"
                , "knownM :: forall (f :: FUN @M 'Many Bool Bool) (b :: Bool). Flag (f b) -> Int"
                , "knownM IsFalse = 42"
                ]
            )
            ]
        $ \(moduleName, code, fragment, source) -> do
            let path = artifacts </> (moduleName ++ ".hs")
            writeFile path source
            GHC.runGhc (Just libdir) $ do
                environment <- nativeModuleEnvironment artifacts warnings NoLink Nothing
                result <- GHC.handleSourceError (pure . Left) $ do
                    liftIO $ void $ compileFile environment NoStop (path, Nothing)
                    pure (Right ())
                checkSourceRejection moduleName code fragment result

-- | Check generic list indices through separately compiled interfaces.
runGenericListModuleChecks :: FilePath -> IO ()
runGenericListModuleChecks libdir = do
    library <- readFile "PackedVecLibrary.hs"
    let warnings = ["-Werror=inaccessible-code", "-Werror=overlapping-patterns"]
        consumer = unlines
            [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE EmptyCase #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE TypeOperators #-}"
            , ""
            , "{-# LANGUAGE PolyKinds #-}"
            , ""
            , "-- | Exercise actual runtime elements and dependent element domains."
            , "module Main where"
            , ""
            , "import Control.Exception (ErrorCall, SomeException, displayException, evaluate, try)"
            , "import Data.IORef"
            , "import Data.List (isInfixOf)"
            , "import PackedVecLibrary"
            , ""
            , "-- | Keep a boxed type outside the selected structural field policy."
            , "data Opaque = Opaque Int deriving (Eq, Show)"
            , ""
            , "-- | Keep a newtype as an opaque list element."
            , "newtype Hidden = Hidden Int deriving (Eq, Show)"
            , ""
            , "-- | Normalize the body without changing a closed binder domain."
            , "type family Element where Element = Int"
            , "closedVector :: foreach (xs :: [Element]) -> Vec Element (GenericCount xs)"
            , "closedVector xs = genericVector xs"
            , ""
            , "-- | Run finite observations without assuming total inputs."
            , "main :: IO ()"
            , "main = do"
            , "    flagRef <- newIORef True"
            , "    flag <- readIORef flagRef"
            , "    let bools = [flag, False]"
            , "    print (toList (genericVector @Bool bools))"
            , "    let integers = [11, 22] :: [Int]"
            , "    print (toList (genericVector integers))"
            , "    print (toList (genericVector @Int integers))"
            , "    let functions = [not]"
            , "    print (map ($ True) (toList (genericVector functions)))"
            , "    let actions = [pure 7, pure 9] :: [IO Int]"
            , "    sequence (toList (genericVector actions)) >>= print"
            , "    let opaque = [Opaque 11]"
            , "    print (toList (genericVector opaque))"
            , "    let hidden = [Hidden 13]"
            , "    print (toList (genericVector hidden))"
            , "    let partial = (error \"opaque head forced\" :: Int) : error \"opaque tail forced\""
            , "    print (genericTag partial)"
            , "    print (length (take 1 (toList (genericVector partial))))"
            , "    let cyclic = 17 : cyclic :: [Int]"
            , "    print (take 3 (toList (genericVector cyclic)))"
            , "    sizeRef <- newIORef (S (S Z))"
            , "    n <- readIORef sizeRef"
            , "    let vector = replicateNative @Int n 23"
            , "    let vectors = [vector]"
            , "    print (map toList (toList (genericNested n vectors)))"
            , "    print (map toList (toList (genericRefinedNested n vectors)))"
            , "    let emptyVector = replicateNative @Int Z 29"
            , "    let emptyVectors = [emptyVector]"
            , "    print (map toList (toList (genericRefinedNested Z emptyVectors)))"
            , ""
            , "    let constructed = replicateNative @Int (S n) 31"
            , "    let constructedVectors = [constructed]"
            , "    print (map toList (toList (genericNested (S n) constructedVectors)))"
            , "    demanded <- try @ErrorCall (evaluate (case genericFirst partial of Nothing -> 0; Just x -> x))"
            , "    case demanded of"
            , "        Left err | \"opaque head forced\" `isInfixOf` displayException err -> print True"
            , "        _ -> error \"genericFirst did not expose the expected opaque head\""
            , "    let lazySize = error \"partial worker forced its index\" :: Nat"
            , "    let suspended :: foreach (ys :: [Vec Int lazySize]) -> Vec (Vec Int lazySize) (GenericCount ys)"
            , "        suspended = genericRefinedNested lazySize"
            , "    suspendedResult <- try @SomeException"
            , "        @(foreach (ys :: [Vec Int lazySize]) -> Vec (Vec Int lazySize) (GenericCount ys))"
            , "        (evaluate @(foreach (ys :: [Vec Int lazySize]) -> Vec (Vec Int lazySize) (GenericCount ys)) suspended)"
            , "    print (case suspendedResult of Left _ -> False; Right _ -> True)"
            , "    print (map (toList) (toList (genericNested n vectors)))"
            , "    print (map @(Vec Int n) @[Int] toList (toList (genericNested n vectors)))"
            , "    let lateVectors = [error \"generic late head forced\", vector]"
            , "    print (length (take 1 (toList (genericNested n lateVectors))))"
            , "    let closed = [3] :: [Element]"
            , "    print (toList (closedVector closed))"
            ]
        expected =
            [ "[True,False]"
            , "[11,22]"
            , "[11,22]"
            , "[False]"
            , "[7,9]"
            , "[Opaque 11]"
            , "[Hidden 13]"
            , "True"
            , "1"
            , "[17,17,17]"
            , "[[23,23]]"
            , "[[23,23]]"
            , "[[]]"
            , "[[31,31,31]]"
            , "True"
            , "True"
            , "[[23,23]]"
            , "[[23,23]]"
            , "1"
            , "[3]"
            ]
        rejection = unlines
            [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE EmptyCase #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE TypeOperators #-}"
            , ""
            , "{-# LANGUAGE PolyKinds #-}"
            , ""
            , "-- | Require evidence for the actual input list."
            , "module RejectGenericList where"
            , ""
            , "import Data.Kind (Type)"
            , "import PackedVecLibrary"
            , ""
            , "wrongList :: forall (a :: Type). foreach (xs :: [a]) -> foreach (ys :: [a]) -> Vec a (GenericCount xs)"
            , "wrongList xs ys = genericVector ys"
            ]
        linearRejection = unlines
            [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE LinearTypes #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , ""
            , "-- | Reject a linear runtime value at an unrestricted retained argument."
            , "module LinearRetainedArg where"
            , ""
            , "import PackedVecLibrary (genericVector)"
            , "import PackedVecLibrary (Vec (..))"
            , ""
            , "bad :: [Bool] %1 -> Bool"
            , "bad xs = case genericVector @Bool xs of"
            , "    VNil -> False"
            , "    VCons _ _ -> True"
            ]
    runModuleCases libdir "generic-list-source" [("PackedVecLibrary", library)] warnings consumer expected
    artifacts <- makeAbsolute ("NativeDependentCoreRun.modules" </> "generic-list-source" </> "artifacts")
    forM_
        [ ("RejectGenericList", 83865, ["GenericCount"], rejection)
        , ("LinearRetainedArg", 83605, ["Many", "One"], linearRejection)
        ]
        $ \(moduleName, code, fragments, source) -> do
            let path = artifacts </> (moduleName ++ ".hs")
            writeFile path source
            GHC.runGhc (Just libdir) $ do
                environment <- nativeModuleEnvironment artifacts warnings NoLink Nothing
                result <- GHC.handleSourceError (pure . Left) $ do
                    liftIO $ void $ compileFile environment NoStop (path, Nothing)
                    pure (Right ())
                forM_ fragments $ \fragment ->
                    checkSourceRejection moduleName code fragment result

{- Note [Generic safe index module controls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Compile the existing providers before each interface-only consumer. Check
ordinary imported use with and without the dependent source extension.
The first six results reuse the authored Showcase observations. The function
list has an unused bottom head. Selecting it separately must expose bottom at
function WHNF. Do not replace that function with an eta wrapper.

The PackedVec callback keeps its literal [vector] argument. Its element type
contains the callback's actual runtime n. Declaration advice must preserve
that type before checking the list. An explicit element type or a let-bound
list would bypass this source inference control. The other vector observation
uses the known retained-list contracts of mkFinGeneric and indexGeneric.
Convert the selected vector with toList before constructing Maybe [Int].
This control does not require standalone native container inference or nested
fmap toList inference. All typed values and evidence must remain under their
existing lexical scope.
-}

-- | Check generic finite-position selection through producer interfaces.
runGenericSafeIndexModuleChecks :: FilePath -> IO ()
runGenericSafeIndexModuleChecks libdir = do
    packed <- readFile "PackedVecLibrary.hs"
    parameterized <- readFile "ParameterizedIndexLibrary.hs"
    let warnings = ["-Werror=inaccessible-code", "-Werror=overlapping-patterns"]
        body =
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE RankNTypes #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , "module Main (main) where"
            , ""
            , "import Control.Exception (SomeException, displayException, evaluate, try)"
            , "import Control.Monad (unless)"
            , "import Data.IORef"
            , "import Data.List (isInfixOf)"
            , "import PackedVecLibrary"
            , "import ParameterizedIndexLibrary (Fin (FZ), indexGeneric, mkFinGeneric, lookupGeneric)"
            , ""
            , "-- | Check an independent result before printing its observation."
            , "observe :: (Eq a, Show a) => String -> a -> a -> IO ()"
            , "observe label expected actual = do"
            , "    unless (actual == expected) (error (label ++ \": wrong value\"))"
            , "    putStrLn (label ++ \": \" ++ show actual)"
            , ""
            , "-- | Keep the literal list inside the original retained callback scope."
            , "observePacked :: PackedVec Int -> [Int]"
            , "observePacked package = withPacked @Int package (\\n vector -> toList (indexGeneric [vector] (FZ @'Z)))"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    valuesRef <- newIORef ([11, 22] :: [Int])"
            , "    positionRef <- newIORef (1 :: Int)"
            , "    values <- readIORef valuesRef"
            , "    position <- readIORef positionRef"
            , "    let selected = case mkFinGeneric @Int values position of"
            , "            Nothing -> Nothing"
            , "            Just checked -> Just (indexGeneric @Int values checked)"
            , "    observe \"generic checked Int position\" (Just 22) selected"
            , "    observe \"generic negative position ignores list\" Nothing"
            , "        (lookupGeneric (error \"m69-unused-negative-list\" :: [Int]) (-1))"
            , "    let functions = [error \"m69-selected-function\", (* 2)] :: [Int -> Int]"
            , "    observe \"generic checked function position\" (Just 42)"
            , "        (fmap ($ 21) (lookupGeneric functions position))"
            , "    sizeRef <- newIORef (S (S Z))"
            , "    n <- readIORef sizeRef"
            , "    let vector = replicateNative @Int n 7"
            , "    let selectedVector = case mkFinGeneric [vector] 0 of"
            , "            Nothing -> Nothing"
            , "            Just checked -> Just (toList (indexGeneric [vector] checked))"
            , "    observe \"generic checked runtime-indexed payload\" (Just [7, 7]) selectedVector"
            , "    observe \"generic lookup through fmap ignores head\" (Just [7, 7])"
            , "        (fmap toList (lookupGeneric [error \"m70-unused-vector\", (vector)] 1))"
            , "    let partial :: [Int]"
            , "        partial = error \"m69-unused-head\" : [29]"
            , "    observe \"generic checked position ignores head\" (Just 29)"
            , "        (lookupGeneric partial position)"
            , "    let cyclic :: [Int]"
            , "        cyclic = 31 : cyclic"
            , "    observe \"generic checked position in cyclic list\" (Just 31)"
            , "        (lookupGeneric cyclic 5)"
            , "    observe \"generic literal list in retained callback\" [7, 7]"
            , "        (observePacked (PackVec @Int n vector))"
            , "    selectedFunction <- try (evaluate (case lookupGeneric functions 0 of"
            , "        Nothing -> error \"m69-missing-function\""
            , "        Just function -> function)) :: IO (Either SomeException (Int -> Int))"
            , "    case selectedFunction of"
            , "        Left exception"
            , "            | \"m69-selected-function\" `isInfixOf` displayException exception ->"
            , "                putStrLn \"generic selected function WHNF: forced\""
            , "            | otherwise -> error \"m69-wrong-function-exception\""
            , "        Right _ -> error \"m69-hidden-bottom-function\""
            ]
        expected =
            [ "generic checked Int position: Just 22"
            , "generic negative position ignores list: Nothing"
            , "generic checked function position: Just 42"
            , "generic checked runtime-indexed payload: Just [7,7]"
            , "generic lookup through fmap ignores head: Just [7,7]"
            , "generic checked position ignores head: Just 29"
            , "generic checked position in cyclic list: Just 31"
            , "generic literal list in retained callback: [7,7]"
            , "generic selected function WHNF: forced"
            ]
    forM_ [("edt", "ExperimentalDependentTypes"), ("ordinary", "NoExperimentalDependentTypes")] $
        \(label, extension) -> do
            let consumer = unlines (("{-# LANGUAGE " ++ extension ++ " #-}") : body)
            runModuleCases libdir ("generic-safe-index-" ++ label)
                [("PackedVecLibrary", packed), ("ParameterizedIndexLibrary", parameterized)]
                warnings consumer expected

{- Note [Ordinary clients of retained contracts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Each paired client imports the same retained signatures. Only its EDT pragma
changes. Compile both providers once, then check each client in a fresh
interface-only session. The driver labels each client's independent oracle.
Dictionary and callback results must keep the actual runtime index scope.
Run rejection pairs only after all positive oracles pass. Check the located
semantic diagnostic and its argument context. These checks add no output to
the fixture's existing golden.
-}

-- | Check retained imports and native inference in ordinary source modules.
runOrdinaryRetainedModuleChecks :: FilePath -> IO ()
runOrdinaryRetainedModuleChecks libdir = do
    library <- readFile "PackedVecLibrary.hs"
    ordinary <- readFile "OrdinaryVecClient.hs"
    let warnings = ["-Werror=inaccessible-code", "-Werror=overlapping-patterns"]
        modes = [("NoEDT", "NoExperimentalDependentTypes"), ("EDT", "ExperimentalDependentTypes")]
        cases :: [(String, [String], String, [String])]
        cases =
            [
                ( "ConstructorPrefixList"
                , ["GADTs"]
                , unlines
                    [ "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> Int -> [Int] -> Maybe [Int]"
                    , "probe n x xs = fmap toList (VCons x <$> fromListExact n xs)"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    sizeRef <- newIORef (1 :: Nat)"
                    , "    size <- readIORef sizeRef"
                    , "    print (probe size 1 [2])"
                    , "    print (probe size 1 [])"
                    ]
                , ["Just [1,2]", "Nothing"]
                )
            ,
                ( "ExplicitCase"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> Int"
                    , "probe n = case replicateNative @Int n (7 :: Int) of"
                    , "    VNil -> 0"
                    , "    VCons x _ -> x"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["0", "7"]
                )
            ,
                ( "InferredCase"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> Int"
                    , "probe n = case replicateNative n (7 :: Int) of"
                    , "    VNil -> 0"
                    , "    VCons x _ -> x"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["0", "7"]
                )
            ,
                ( "ExplicitList"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> [Int]"
                    , "probe n = toList @Int (replicateNative @Int n (7 :: Int))"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["[]", "[7,7]"]
                )
            ,
                ( "InferredList"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> [Int]"
                    , "probe n = toList (replicateNative n (7 :: Int))"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["[]", "[7,7]"]
                )
            ,
                ( "NativeConsumer"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> Nat -> Int"
                    , "probe n m = case appendNative @Int n m (replicateNative @Int n (7 :: Int)) (replicateNative @Int m (9 :: Int)) of"
                    , "    VNil -> 0"
                    , "    VCons x _ -> x"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    oneRef <- newIORef (S Z)"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    one <- readIORef oneRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero zero)"
                    , "    print (probe zero one)"
                    , "    print (probe two one)"
                    ]
                , ["0", "9", "7"]
                )
            ,
                ( "GenericList"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Observe imported retained applications without dependent source syntax."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: [Int] -> [Int]"
                    , "probe xs = toList @Int (genericVector @Int xs)"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    emptyRef <- newIORef ([] :: [Int])"
                    , "    valuesRef <- newIORef ([11, 22] :: [Int])"
                    , "    empty <- readIORef emptyRef"
                    , "    values <- readIORef valuesRef"
                    , "    print (probe empty)"
                    , "    print (probe values)"
                    ]
                , ["[]", "[11,22]"]
                )
            ,
                ( "DictionaryScope"
                , ["DataKinds", "GADTs", "TypeApplications", "FlexibleInstances"]
                , unlines
                    [ "-- | Resolve a dictionary for the exact native argument type."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "class Measure a where"
                    , "    measure :: a -> Int"
                    , ""
                    , "instance Measure (Vec Int n) where"
                    , "    measure VNil = 0"
                    , "    measure (VCons _ rest) = 1 + measure rest"
                    , ""
                    , "probe :: Nat -> Int"
                    , "probe n = measure (replicateNative @Int n (7 :: Int))"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["0", "2"]
                )
            ,
                ( "PackedCallback"
                , ["DataKinds", "GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Consume a real package with a retained callback and an ordinary list result."
                    , ""
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "probe :: Nat -> [Int]"
                    , "probe n ="
                    , "    let package :: PackedVec Int"
                    , "        package = PackVec @Int n (replicateNative @Int n (7 :: Int))"
                    , "     in withPacked @Int package (\\n xs -> toList xs)"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["[]", "[7,7]"]
                )
            ,
                ( "CallbackDictionaries"
                , ["DataKinds", "GADTs", "TypeApplications", "FlexibleInstances", "ExplicitForAll"]
                , unlines
                    [ "-- | Keep dictionary evidence inside each retained callback scope."
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary"
                    , ""
                    , "class Measure a where"
                    , "    measure :: a -> Int"
                    , ""
                    , "instance Measure (Vec Int n) where"
                    , "    measure VNil = 0"
                    , "    measure (VCons _ rest) = 1 + measure rest"
                    , ""
                    , "measureVia :: forall b a. Measure a => b -> a -> Int"
                    , "measureVia _ = measure"
                    , ""
                    , "probe :: Nat -> (Int, Int)"
                    , "probe n ="
                    , "    let package :: PackedVec Int"
                    , "        package = PackVec @Int n (replicateNative @Int n (7 :: Int))"
                    , "     in ( withPacked @Int package (\\n xs -> measure xs)"
                    , "        , withPacked @Int package (\\n xs -> measureVia @Int (0 :: Int) xs)"
                    , "        )"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (probe zero)"
                    , "    print (probe two)"
                    ]
                , ["(0,0)", "(2,2)"]
                )
            ,
                ( "OrdinaryExample"
                , []
                , unlines
                    [ "-- | Call the ordinary exports of the separately compiled example client."
                    , "import Data.IORef (newIORef, readIORef)"
                    , "import PackedVecLibrary (Nat (..))"
                    , "import OrdinaryVecClient (ordinaryReplicate, ordinaryRoundTrip)"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    zeroRef <- newIORef Z"
                    , "    twoRef <- newIORef (S (S Z))"
                    , "    zero <- readIORef zeroRef"
                    , "    two <- readIORef twoRef"
                    , "    print (ordinaryReplicate zero True)"
                    , "    print (ordinaryReplicate two True)"
                    , "    print (ordinaryRoundTrip ([] :: [Int]))"
                    , "    print (ordinaryRoundTrip ([11, 22] :: [Int]))"
                    ]
                , ["[]", "[True,True]", "[]", "[11,22]"]
                )
            ,
                ( "QuickLookCompleteNative"
                , ["GADTs", "ImpredicativeTypes"]
                , unlines
                    [ "-- | Complete the ordinary const application from declared argument types."
                    , ""
                    , "import PackedVecLibrary (rightZero)"
                    , ""
                    , "probe :: Bool -> Int"
                    , "probe flag = case const rightZero flag of"
                    , "    _ -> 42"
                    , ""
                    , "main :: IO ()"
                    , "main = do"
                    , "    print (probe False)"
                    , "    print (probe True)"
                    ]
                , ["42", "42"]
                )
            ,
                ( "QuickLookOrdinaryPartial"
                , ["GADTs", "ImpredicativeTypes"]
                , unlines
                    [ "-- | Keep ordinary partial application inference unchanged."
                    , ""
                    , "import PackedVecLibrary (natToInt)"
                    , ""
                    , "probe :: Int"
                    , "probe = case const natToInt of"
                    , "    _ -> 42"
                    , ""
                    , "main :: IO ()"
                    , "main = print probe"
                    ]
                , ["42"]
                )
            ]
        rejections :: [(String, [String], String, Int, [String])]
        rejections =
            [
                ( "ConstructorPrefixIndependentIndex"
                , ["DataKinds", "GADTs"]
                , unlines
                    [ "import PackedVecLibrary"
                    , ""
                    , "bad :: Nat -> Nat -> [Int] -> [Int] -> Maybe [(Int, Int)]"
                    , "bad n m xs ys = do"
                    , "    left <- fmap (VCons 1) (fromListExact n xs)"
                    , "    right <- fromListExact m ys"
                    , "    pure (toList (zipVec left right))"
                    ]
                , 83865
                , ["Couldn't match type", "zipVec", "Vec Int ('S n)", "Vec Int m"]
                )
            ,
                ( "ConstructorPrefixLinearCaptureTwice"
                , ["DataKinds", "GADTs", "LinearTypes"]
                , unlines
                    [ "import PackedVecLibrary"
                    , ""
                    , "applyOnce :: (a %1 -> b) %1 -> a %1 -> b"
                    , "applyOnce f x = f x"
                    , "bad :: Int %1 -> Vec Int n -> (Vec Int (S n), Vec Int (S n))"
                    , "bad x xs = (applyOnce (VCons x) xs, applyOnce (VCons x) xs)"
                    ]
                , 18872
                , ["multiplicity", "x"]
                )
            ,
                ( "WrongSourceLengthElement"
                , ["TypeApplications"]
                , unlines
                    [ "import PackedVecLibrary"
                    , ""
                    , "wrong :: [Int] -> PackedVec Int"
                    , "wrong xs = PackVec (sourceLength @Bool xs) (sourceVector xs)"
                    ]
                , 83605
                , ["Expected: [Bool]", "Actual: [Int]"]
                )
            ,
                ( "WrongPackageLength"
                , []
                , unlines
                    [ "import PackedVecLibrary"
                    , ""
                    , "wrong :: [a] -> [a] -> PackedVec a"
                    , "wrong xs ys = PackVec (sourceLength xs) (sourceVector ys)"
                    ]
                , 83865
                , ["sourceVector ys"]
                )
            ,
                ( "IndependentListLengths"
                , ["GADTs"]
                , unlines
                    [ "-- | Keep independently checked runtime lengths distinct."
                    , "import PackedVecLibrary"
                    , ""
                    , "bad :: Nat -> Nat -> [Int] -> [Bool] -> Maybe [(Int, Bool)]"
                    , "bad n m xs ys = do"
                    , "    vx <- fromListExact n xs"
                    , "    vy <- fromListExact m ys"
                    , "    pure (toList (zipVec vx vy))"
                    ]
                , 83865
                , ["Couldn't match type", "zipVec vx vy"]
                )
            ,
                ( "IndexMismatch"
                , ["GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Require each vector to use the actual retained length passed to appendNative."
                    , "import PackedVecLibrary"
                    , ""
                    , "bad :: Nat -> Nat -> Int"
                    , "bad n m = case appendNative @Int n n (replicateNative @Int n (7 :: Int)) (replicateNative @Int m (9 :: Int)) of"
                    , "    VNil -> 0"
                    , "    VCons x _ -> x"
                    ]
                , 83865
                , ["Expected: Vec Int", "Actual: Vec Int m", "In the fifth argument of", "appendNative"]
                )
            ,
                ( "EscapingCallback"
                , ["DataKinds", "GADTs", "TypeApplications"]
                , unlines
                    [ "-- | Reject an inferred result that would expose the callback's runtime index."
                    , ""
                    , "import PackedVecLibrary"
                    , ""
                    , "escape package = withPacked @Int package (\\n xs -> xs)"
                    ]
                , 25897
                , ["Vec Int", "rigid type variable", "In the expression: xs", "In the third argument of", "withPacked"]
                )
            ,
                ( "LinearRetainedArg"
                , ["DataKinds", "GADTs", "TypeApplications", "LinearTypes"]
                , unlines
                    [ "-- | Reject a local linear Nat argument at an imported unrestricted retained call."
                    , ""
                    , "import PackedVecLibrary"
                    , ""
                    , "bad :: Nat %1 -> Int"
                    , "bad n = case replicateNative @Int n (7 :: Int) of"
                    , "    VNil -> 0"
                    , "    VCons x _ -> x"
                    ]
                , 83605
                , ["unrestricted multiplicity Many", "This argument has multiplicity:", "One", "replicateNative"]
                )
            ,
                ( "QuickLookPartialNative"
                , ["GADTs", "ImpredicativeTypes"]
                , unlines
                    [ "-- | Keep an incomplete ordinary application with a native value anchor."
                    , ""
                    , "import PackedVecLibrary (rightZero)"
                    , ""
                    , "probe :: Int"
                    , "probe = case const rightZero of"
                    , "    _ -> 42"
                    , ""
                    , "main :: IO ()"
                    , "main = print probe"
                    ]
                , 83605
                , ["Quick Look inference does not support this retained type yet", "In the first argument of", "const", "rightZero"]
                )
            ]
        clients =
            [ (moduleName, unlines $
                map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") (mode : extensions)
                ++ ["", "module " ++ moduleName ++ " (main) where", ""] ++ lines body)
            | (name, extensions, body, _) <- cases
            , (suffix, mode) <- modes
            , let moduleName = "M68" ++ name ++ suffix
            ]
        driver = unlines $
            ["{-# LANGUAGE NoExperimentalDependentTypes #-}", "module Main (main) where"]
            ++ ["import qualified " ++ name | (name, _) <- clients]
            ++ ["main :: IO ()", "main = do"]
            ++ concat [["    putStrLn " ++ show name, "    " ++ name ++ ".main"] | (name, _) <- clients]
        expected = concat
            [ ("M68" ++ name ++ suffix) : oracle
            | (name, _, _, oracle) <- cases
            , (suffix, _) <- modes
            ]
    runModuleCases libdir "ordinary-retained-source"
        ([("PackedVecLibrary", library), ("OrdinaryVecClient", ordinary)] ++ clients)
        warnings driver expected
    artifacts <- makeAbsolute ("NativeDependentCoreRun.modules" </> "ordinary-retained-source" </> "artifacts")
    rejectionDirectory <- makeAbsolute ("NativeDependentCoreRun.modules" </> "ordinary-retained-source" </> "rejections")
    createDirectoryIfMissing True rejectionDirectory
    forM_ rejections $ \(name, extensions, body, code, fragments) ->
        forM_ modes $ \(suffix, mode) -> do
            let moduleName = "M68Reject" ++ name ++ suffix
                source = unlines $
                    map (\extension -> "{-# LANGUAGE " ++ extension ++ " #-}") (mode : extensions)
                    ++ ["", "module " ++ moduleName ++ " where", ""] ++ lines body
                path = rejectionDirectory </> (moduleName ++ ".hs")
            writeFile path source
            GHC.runGhc (Just libdir) $ do
                environment <- nativeModuleEnvironment artifacts warnings NoLink Nothing
                result <- GHC.handleSourceError (pure . Left) $ do
                    liftIO $ void $ compileFile environment NoStop (path, Nothing)
                    pure (Right ())
                forM_ fragments $ \fragment ->
                    checkSourceRejection moduleName code fragment result

{- Note [Bounded family diagnostic argument positions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Native argument caches count value arguments. Diagnostics count user-written
type arguments as well. In the VTA control, xs is the third value argument
and the fourth written argument, after @Bool. Check its own source span and
argument context in the same depth diagnostic. Do not check the reported
depth size. That size can change when the rewrite implementation changes.

The list domains in the cached negatives require a family view before the
current matcher can select their element types. That path normalizes the
complete actual type and must report its bounded depth failure. This tests
the current matching strategy. It does not state that these programs are
intrinsically untypable. A plain a -> () helper can select the unreduced type.
Keep that case as a positive control with a recursive family and bottom inputs.
-}

-- | Check finite native family views and located reduction-depth failures.
runBoundedFamilyModuleChecks :: FilePath -> IO ()
runBoundedFamilyModuleChecks libdir = do
    let finite = unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module FiniteNative where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "-- | A runtime length."
            , "data Nat = Z | S Nat"
            , ""
            , "-- | A vector with an erased length parameter."
            , "data Vec a (n :: Nat) where"
            , "    VNil :: Vec a 'Z"
            , "    VCons :: a -> Vec a n -> Vec a ('S n)"
            , ""
            , "type family Shape (n :: Nat) :: Type where"
            , "    Shape n = [Vec Int n]"
            , ""
            , "-- | Inspect only the outer list constructor."
            , "shape :: foreach (n :: Nat) -> Shape n -> Int"
            , "shape n xs = case xs of"
            , "    [] -> 0"
            , "    _ : _ -> 1"
            , ""
            , "-- | Preserve the actual runtime length in the selected payload."
            , "payload :: foreach (n :: Nat) -> Shape n -> Maybe (Vec Int n)"
            , "payload n xs = case xs of"
            , "    [] -> Nothing"
            , "    value : _ -> Just value"
            , ""
            , "-- | Ignore an argument with an ordinary polymorphic function."
            , "ignore :: forall a. a -> ()"
            , "ignore _ = ()"
            , ""
            , "-- | Select the cached argument type without evaluating either argument."
            , "cached :: foreach (n :: Nat) -> Shape n -> ()"
            , "cached n xs = ignore xs"
            , ""
            , "-- | Keep an unused recursive family type unreduced."
            , "type family Spin (b :: Bool) :: Type where Spin b = Spin b"
            , "type family LoopShape (b :: Bool) :: Type where LoopShape b = [Spin b]"
            , "cachedLoop :: foreach (b :: Bool) -> LoopShape b -> ()"
            , "cachedLoop b xs = ignore xs"
            ]
        consumer = unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module Main where"
            , ""
            , "import FiniteNative"
            , ""
            , "main :: IO ()"
            , "main = do"
            , "    print (shape Z [])"
            , "    print (shape (S Z) [VCons 7 VNil])"
            , "    print (case payload Z [VNil] of"
            , "        Nothing -> False"
            , "        Just VNil -> True)"
            , "    let n :: Nat"
            , "        n = error \"index-demand\""
            , "    let xs :: Shape n"
            , "        xs = error \"argument-demand\""
            , "    print (case cached n xs of () -> (42 :: Int))"
            , "    print (case payload (S Z) [error \"vector-demand\"] of"
            , "        Nothing -> (0 :: Int)"
            , "        Just _ -> 1)"
            , "    let b :: Bool"
            , "        b = error \"recursive-index-demand\""
            , "    let ys :: LoopShape b"
            , "        ys = error \"recursive-argument-demand\""
            , "    case cachedLoop b ys of () -> pure ()"
            ]
        depthFlags = ["-freduction-depth=12"]
    runModuleCases libdir "bounded-family-source" [("FiniteNative", finite)]
        depthFlags consumer ["0", "1", "True", "42", "1"]
    directory <- makeAbsolute ("NativeDependentCoreRun.modules" </> "bounded-family-source" </> "diagnostics")
    createDirectoryIfMissing True directory
    forM_
        [ ("LoopShape", Nothing, unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module LoopShape where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "type family Spin (b :: Bool) :: Type where"
            , "    Spin b = Spin b"
            , ""
            , "type family Shape (b :: Bool) :: Type where"
            , "    Shape b = [Spin b]"
            , ""
            , "shapeOnly :: foreach (b :: Bool) -> Shape b -> ()"
            , "shapeOnly b xs = case xs of"
            , "    [] -> ()"
            , "    _ : _ -> ()"
            ]
          )
        , ("LoopCached", Nothing, unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module LoopCached where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "type family Spin (b :: Bool) :: Type where"
            , "    Spin b = Spin b"
            , ""
            , "type family Shape (b :: Bool) :: Type where"
            , "    Shape b = [Spin b]"
            , ""
            , "ignore :: forall a. [a] -> ()"
            , "ignore _ = ()"
            , ""
            , "cached :: foreach (b :: Bool) -> Shape b -> ()"
            , "cached b xs = ignore xs"
            ]
          )
        , ("LoopCachedSecond", Just (22, 28, "second argument"), unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module LoopCachedSecond where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "type family Spin (b :: Bool) :: Type where"
            , "    Spin b = Spin b"
            , ""
            , "type family Shape (b :: Bool) :: Type where"
            , "    Shape b = [Spin b]"
            , ""
            , "ignore :: forall a c. a -> [c] -> ()"
            , "ignore _ _ = ()"
            , ""
            , "cached :: foreach (b :: Bool) -> Shape b -> ()"
            , "cached b xs = ignore False xs"
            ]
          )
        , ("LoopCachedVta", Just (23, 37, "fourth argument"), unlines
            [ "{-# LANGUAGE TypeApplications #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module LoopCachedVta where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "type family Spin (b :: Bool) :: Type where"
            , "    Spin b = Spin b"
            , ""
            , "type family Shape (b :: Bool) :: Type where"
            , "    Shape b = [Spin b]"
            , ""
            , "ignore :: forall tag a c. tag -> a -> [c] -> ()"
            , "ignore _ _ _ = ()"
            , ""
            , "cached :: foreach (b :: Bool) -> Shape b -> ()"
            , "cached b xs = ignore @Bool False () xs"
            ]
          )
        , ("LoopOrdinary", Nothing, unlines
            [ "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE ExplicitForAll #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE KindSignatures #-}"
            , "{-# LANGUAGE TypeFamilies #-}"
            , "{-# LANGUAGE UndecidableInstances #-}"
            , "module LoopOrdinary where"
            , ""
            , "import Data.Kind (Type)"
            , ""
            , "type family Spin (b :: Bool) :: Type where"
            , "    Spin b = Spin b"
            , ""
            , "type family Shape (b :: Bool) :: Type where"
            , "    Shape b = [Spin b]"
            , ""
            , "plain :: Spin 'False -> ()"
            , "plain value = value"
            ]
          )
        ]
        $ \(moduleName, expectedArgument, source) -> do
            let path = directory </> (moduleName ++ ".hs")
            writeFile path source
            GHC.runGhc (Just libdir) $ do
                environment <- nativeModuleEnvironment directory depthFlags NoLink Nothing
                result <- GHC.handleSourceError (pure . Left) $ do
                    liftIO $ void $ compileFile environment NoStop (path, Nothing)
                    pure (Right ())
                checkSourceRejection moduleName 40404 "Reduction stack overflow" result
                case (expectedArgument, result) of
                    (Just (line, column, context), Left exception) -> do
                        flags <- GHC.getSessionDynFlags
                        let messages = bagToList (getMessages (srcErrorMessages exception))
                            matches envelope =
                                let rendered = showSDoc flags (pprDiagnostic (errMsgDiagnostic envelope))
                                in diagnosticCode (errMsgDiagnostic envelope) == Just (DiagnosticCode "GHC" 40404)
                                    && all (`isInfixOf` rendered) [context, "ignore", "namely", "xs"]
                                    && case errMsgSpan envelope of
                                        RealSrcSpan span _ ->
                                            srcSpanFile span == fsLit path
                                                && srcSpanStartLine span == line
                                                && srcSpanEndLine span == line
                                                && srcSpanStartCol span == column
                                                && srcSpanEndCol span == column + 2
                                        _ -> False
                        unless (any matches messages) $
                            fail ("FAIL: family reduction lost its argument location or context: " ++ moduleName
                                ++ ":\n" ++ showSDoc flags (vcat (concatMap (\envelope ->
                                    [ppr (errMsgSpan envelope), pprDiagnostic (errMsgDiagnostic envelope)]) messages)))
                    _ -> pure ()

-- | Check an exported function telescope and package fields through an object interface.
runExportedFunctionSignatureModuleChecks :: FilePath -> IO ()
runExportedFunctionSignatureModuleChecks libdir = do
    packed <- readFile "PackedVecLibrary.hs"
    let (header, declaration) = break (isInfixOf "module PackedVecLibrary ") (lines packed)
        producer = unlines (header ++ ["module NativeFunctionSignatureAPI where"] ++ drop 1 (dropWhile (/= ") where") declaration) ++ exportedFields)
        exportedFields =
            [ "-- | Bind the function and input used by the following vector field."
            , "data FunctionPack a where"
            , "    FunctionPack :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> FunctionPack a"
            , ""
            , "-- | Give a lifted error an explicit native result type."
            , "raisePayload :: forall a. String -> a"
            , "raisePayload = error"
            , ""
            , "-- | Build the vector under the checked equality for the exact whole call."
            , "buildPack :: forall a. (Nat -> Nat) -> Nat -> a -> FunctionPack a"
            , "buildPack f n value = case f n of"
            , "    Z -> let ys :: Vec a (f n); ys = VNil in FunctionPack f n ys"
            , "    S k -> let ys :: Vec a (f n); ys = VCons value (replicateNative @a k value) in FunctionPack f n ys"
            , ""
            , "-- | Store all fields without executing the function or inspecting the input."
            , "lazyPack :: forall a. (Nat -> Nat) -> Nat -> FunctionPack a"
            , "lazyPack f n ="
            , "    let ys :: Vec a (f n)"
            , "        ys = raisePayload @(Vec a (f n)) \"function-pack-payload\""
            , "     in FunctionPack f n ys"
            , ""
            , "-- | Reconstruct the package with the actual selected field identities."
            , "repackFunction :: FunctionPack a -> FunctionPack a"
            , "repackFunction (FunctionPack f n xs) = FunctionPack f n xs"
            , ""
            , "-- | Inspect only the outer package constructor."
            , "ignoreFunction :: FunctionPack a -> Int"
            , "ignoreFunction (FunctionPack f n xs) = 42"
            , ""
            , "-- | Demand the original input independently from the function and payload."
            , "inputLength :: FunctionPack a -> Int"
            , "inputLength (FunctionPack f n xs) = natToInt n"
            , ""
            , "-- | Demand the computed result and payload through the selected function Id."
            , "inspectFunction :: FunctionPack a -> (Int, [a])"
            , "inspectFunction (FunctionPack f n xs) = case f n of"
            , "    Z -> (0, toList xs)"
            , "    S k -> (natToInt (S k), toList xs)"
            , ""
            , "-- | Observe the payload without executing the stored function."
            , "payloadOnly :: FunctionPack a -> [a]"
            , "payloadOnly (FunctionPack f n xs) = toList xs"
            , ""
            , "-- | Demand only one payload constructor."
            , "headOnly :: FunctionPack Int -> Int"
            , "headOnly (FunctionPack f n VNil) = 0"
            , "headOnly (FunctionPack f n (VCons x rest)) = x"
            , ""
            , "-- | Keep both selected function and index identities in separate scopes."
            , "pairPayloads :: FunctionPack a -> FunctionPack b -> ([a], [b])"
            , "pairPayloads (FunctionPack f n xs) (FunctionPack g m ys) = (toList xs, toList ys)"
            , ""
            , "-- | Resume the second row outside the first row's field scope."
            , "guardedIgnore :: Bool -> FunctionPack a -> Int"
            , "guardedIgnore flag (FunctionPack f n xs) | flag = ignoreFunction (FunctionPack f n xs)"
            , "guardedIgnore _ (FunctionPack f n xs) = ignoreFunction (FunctionPack f n xs)"
            , ""
            , "-- | Return the original vector without evaluating its function or input index."
            , "idCall :: forall a. foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec a (f n) -> Vec a (f n)"
            , "idCall f n xs = xs"
            ]
        consumer = unlines
            [ "{-# LANGUAGE ExperimentalDependentTypes #-}"
            , "{-# LANGUAGE DataKinds #-}"
            , "{-# LANGUAGE GADTs #-}"
            , "{-# LANGUAGE ScopedTypeVariables #-}"
            , "{-# LANGUAGE TypeApplications #-}"
            , ""
            , "-- | Use an imported retained worker with actual function-package fields."
            , "module Main (main) where"
            , ""
            , "import Control.Exception (ErrorCall, displayException, evaluate, try)"
            , "import Control.Monad (forM_, unless)"
            , "import Data.IORef (newIORef, readIORef)"
            , "import Data.List (isInfixOf)"
            , "import NativeFunctionSignatureAPI"
            , "import System.Mem.StableName (makeStableName)"
            , ""
            , "-- | Preserve a runtime identity head."
            , "{-# NOINLINE identity #-}"
            , "identity :: Nat -> Nat"
            , "identity n = n"
            , ""
            , "-- | Preserve a runtime successor head."
            , "{-# NOINLINE successor #-}"
            , "successor :: Nat -> Nat"
            , "successor n = S n"
            , ""
            , "-- | Ignore the input with a noninjective runtime head."
            , "{-# NOINLINE constantZero #-}"
            , "constantZero :: Nat -> Nat"
            , "constantZero n = Z"
            , ""
            , "-- | Select the original vector through the imported worker."
            , "payloadThrough :: forall a. FunctionPack a -> [a]"
            , "payloadThrough (FunctionPack f n xs) = toList (idCall @a f n xs)"
            , ""
            , "-- | Keep the actual package fields without demanding the selected payload."
            , "ignoreThrough :: FunctionPack Int -> Int"
            , "ignoreThrough (FunctionPack f n xs) = const 42 (idCall @Int f n xs)"
            , ""
            , "-- | Compare the same evaluated vector constructor before and after the worker."
            , "sharesPayload :: forall a. FunctionPack a -> IO Bool"
            , "sharesPayload (FunctionPack f n xs) = do"
            , "    original <- evaluate xs"
            , "    selected <- evaluate (idCall @a f n xs)"
            , "    before <- makeStableName original"
            , "    after <- makeStableName selected"
            , "    pure (before == after)"
            , ""
            , "-- | Check the independent expected result and print its label."
            , "check :: (Eq a, Show a) => String -> a -> a -> IO ()"
            , "check label expected actual = do"
            , "    unless (expected == actual) (error (label ++ \": \" ++ show actual))"
            , "    putStrLn (label ++ \": \" ++ show actual)"
            , ""
            , "-- | Require the original payload or input error."
            , "expectMarker :: String -> IO a -> IO ()"
            , "expectMarker marker action = do"
            , "    result <- try action"
            , "    case result of"
            , "        Left (exception :: ErrorCall)"
            , "            | marker `isInfixOf` displayException exception -> putStrLn marker"
            , "        Left exception -> error (displayException exception)"
            , "        Right _ -> error (\"expected \" ++ marker)"
            , ""
            , "-- | Load finite inputs independently and check value, identity and demand."
            , "main :: IO ()"
            , "main = do"
            , "    forM_ [(Z, 0, [], [9]), (S (S Z), 2, [7, 7], [9, 9, 9]), (S (S (S Z)), 3, [7, 7, 7], [9, 9, 9, 9])] $ \\(seed, size, same, next) -> do"
            , "        n <- newIORef seed >>= readIORef"
            , "        m <- newIORef seed >>= readIORef"
            , "        k <- newIORef seed >>= readIORef"
            , "        check (\"identity \" ++ show (size :: Int)) same (payloadThrough (buildPack @Int identity n 7))"
            , "        check (\"successor \" ++ show size) next (payloadThrough (buildPack @Int successor m 9))"
            , "        check (\"constant \" ++ show size) ([] :: [Int]) (payloadThrough (buildPack @Int constantZero k 13))"
            , "    check \"generic character payload\" \"qq\" (payloadThrough (buildPack @Char identity (S (S Z)) 'q'))"
            , "    shared <- sharesPayload (buildPack @Int identity (S (S Z)) 7)"
            , "    check \"vector constructor identity\" True shared"
            , "    let f :: Nat -> Nat"
            , "        f = error \"exported-function-head\""
            , "        n :: Nat"
            , "        n = error \"exported-input\""
            , "        zero :: Nat -> Nat"
            , "        zero = constantZero"
            , "        cyclic :: Nat"
            , "        cyclic = S cyclic"
            , "    _ <- evaluate (idCall @Int f)"
            , "    putStrLn \"partial function argument: whnf\""
            , "    _ <- evaluate (idCall @Int f n)"
            , "    putStrLn \"partial function and input: whnf\""
            , "    check \"ignored bottom payload\" 42 (ignoreThrough (lazyPack @Int f n))"
            , "    expectMarker \"function-pack-payload\" (evaluate (length (payloadThrough (lazyPack @Int f n))))"
            , "    check \"constant ignores bottom input\" ([] :: [Int]) (payloadThrough (buildPack @Int zero n 13))"
            , "    check \"constant ignores cyclic input\" ([] :: [Int]) (payloadThrough (buildPack @Int zero cyclic 13))"
            , "    case payloadThrough (buildPack @Int successor n 7) of"
            , "        first : _ -> check \"partial input head\" 7 first"
            , "        [] -> error \"expected a successor prefix\""
            , "    expectMarker \"exported-input\" (evaluate (length (payloadThrough (buildPack @Int successor n 7))))"
            ]
        expected =
            [ "identity 0: []"
            , "successor 0: [9]"
            , "constant 0: []"
            , "identity 2: [7,7]"
            , "successor 2: [9,9,9]"
            , "constant 2: []"
            , "identity 3: [7,7,7]"
            , "successor 3: [9,9,9,9]"
            , "constant 3: []"
            , "generic character payload: \"qq\""
            , "vector constructor identity: True"
            , "partial function argument: whnf"
            , "partial function and input: whnf"
            , "ignored bottom payload: 42"
            , "function-pack-payload"
            , "constant ignores bottom input: []"
            , "constant ignores cyclic input: []"
            , "partial input head: 7"
            , "exported-input"
            ]
    unless (not (null declaration) && ") where" `elem` declaration) $
        fail "FAIL: PackedVecLibrary module header changed"
    runModuleCase libdir "exported-function-signature-source" "NativeFunctionSignatureAPI" producer consumer expected

{- Note [Opaque retained function casts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Keep the two payload types distinct while checking the abstract kernel. The
nominal proof is a scoped CoVar. Its value cast must survive direct CorePrep
without becoming a structured congruence proof. Check the full expression type
and the actual lexical binders after preparation. The cast-only kernel has no
new runtime lambda. It must preserve a bottom worker's WHNF behavior.

Only the runtime caller supplies Int at both payloads and a reflexivity proof.
That execution cannot by itself establish opacity during typed preparation.
The object control checks a separate abstract NOINLINE binding in the actual
object pipeline. These checks add no function eta equality or proof projection.
-}

-- | Check an opaque whole-function cast before closing its abstract payload types.
runOpaquePiWholeCastChecks :: HscEnv -> DynFlags -> [Id] -> [TyCon] -> IO ()
runOpaquePiWholeCastChecks environment flags bindings tycons = do
    vec <- named "Vec"
    nat <- named "Nat"
    config <- initCorePrepConfig environment
    let nativeConfig =
            config
                { cp_allowNativeDependent = True
                , cp_arityOpts = Nothing
                , cp_specEval = False
                , cp_specEvalDFun = False
                }
        natTy = mkTyConTy nat
        fnTy = mkFunctionType unmatchableDataConTy ManyTy natTy natTy
        a = mkTyVar (mkSystemName (unique 16000) (mkTyVarOcc "opaque_a")) liftedTypeKind
        b = mkTyVar (mkSystemName (unique 16001) (mkTyVarOcc "opaque_b")) liftedTypeKind
        -- Endpoint templates and actual operands have DIFFERENT Uniques.
        templateF = local 16002 "phi_f" fnTy
        templateN = local 16003 "phi_n" natTy
        vector element function index =
            mkTyConApp vec [element, TermAppTy function (TermVarTy index)]
        phi element =
            mkDepFunTy templateF
                $ mkDepFunTy templateN
                $ mkFunctionType unmatchableDataConTy ManyTy (vector element templateF templateN)
                $ mkFunctionType unmatchableDataConTy ManyTy element element
        q = proof 16004 (phi (mkTyVarTy a)) (phi (mkTyVarTy b))
        h = local 16005 "opaque_h" (phi (mkTyVarTy a))
        f = local 16006 "actual_f" fnTy
        n = local 16007 "actual_n" natTy
        prefix = [a, b, q, h]
        casted = Cast (Var h) (SubCo (mkCoVarCo q))
        raws =
            [ (0, mkLams prefix casted)
            , (1, mkLams (prefix ++ [f]) (App casted (Var f)))
            , (2, mkLams (prefix ++ [f, n]) (mkApps casted [Var f, Var n]))
            ]
        wf = local 16008 "worker_f" fnTy
        wn = local 16009 "worker_n" natTy
        vx = local 16010 "worker_vec" (vector intTy wf wn)
        answer = local 16011 "worker_answer" intTy
        vecCase = local 16012 "worker_vec_case" (varType vx)
        nCase = local 16013 "worker_n_case" natTy
        keep = mkLams [wf, wn, vx, answer] (Var answer)
        demandVec =
            mkLams [wf, wn, vx, answer] $
                Case (Var vx) vecCase intTy [Alt DEFAULT [] (Var answer)]
        demandN =
            mkLams [wf, wn, vx, answer] $
                Case (Var wn) nCase intTy [Alt DEFAULT [] (Var answer)]
        runtimeH = local 16014 "closed_h" (phi intTy)
        bottom ty = App (Var (helper bindings "bottomFunction")) (Type ty)
        bottomVector =
            App
                (Var (helper bindings "bottomVec"))
                (Type (TermAppTy f (TermVarTy n)))
        instantiate count prepared =
            mkApps prepared $
                [Type intTy, Type intTy, Coercion (mkNomReflCo (phi intTy)), Var runtimeH]
                    ++ take count [Var f, Var n]
        force expression =
            mkApps
                (Var (helper bindings "force"))
                [Type (exprType expression), expression]
        observe expression = App (Var (helper bindings "observeInt")) expression
        close worker actualHead seed action =
            Let (NonRec runtimeH worker)
                $ Let (NonRec f actualHead)
                $ mkApps (Var (helper bindings "driveNat")) [Lam n action, seed]
        expectBottom :: String -> String -> CoreExpr -> IO ()
        expectBottom label marker expression = do
            outcome <- try (execute environment flags label expression) :: IO (Either SomeException String)
            case outcome of
                Left exception | marker `isInfixOf` displayException exception -> pure ()
                Left exception -> fail ("FAIL: " ++ label ++ "; wrong exception: " ++ displayException exception)
                Right actual -> fail ("FAIL: " ++ label ++ "; returned " ++ show actual)
        -- Actual runtime operands are local variables; globals occur only in RHSs/seeds.
        bottomN = Var (helper bindings "bottomNat")
        bottomF = bottom fnTy
        identityF = Var (helper bindings "opaqueNat")
        payload = mkIntExpr (targetPlatform flags) 42
    forM_ raws $ \(count, raw) -> do
        lintNative flags ("opaque whole-Pi raw " ++ show count) raw
        prepared <- corePrepExpr (hsc_logger environment) nativeConfig raw
        lintNative flags ("opaque whole-Pi prepared " ++ show count) prepared
        unless (eqType (exprType raw) (exprType prepared)) $
            fail "FAIL: opaque whole-Pi preparation changed its type"
        unless (opaquePiCastIsScoped raw && opaquePiCastIsScoped prepared) $
            fail "FAIL: opaque whole-Pi cast lost its scoped abstract evidence"
        let application = instantiate count prepared
        actual <-
            execute environment flags "opaque whole-Pi keep WHNF" $
                close keep bottomF bottomN (force application)
        unless (actual == "forced") $ fail "FAIL: opaque whole-Pi keep WHNF"
        expectBottom ("opaque whole-Pi bottom worker WHNF " ++ show count) "native-bottom-function" $
            close (bottom (phi intTy)) identityF bottomN (force application)
        if count /= 2
            then pure ()
            else do
                let observed = observe (mkApps application [bottomVector, payload])
                actualValue <-
                    execute environment flags "opaque whole-Pi ignored operands" $
                        close keep bottomF bottomN observed
                unless (actualValue == "42") $ fail "FAIL: opaque whole-Pi ignored operands"
                expectBottom "opaque whole-Pi demanded vector" "native-bottom-vector" $
                    close demandVec identityF bottomN observed
                expectBottom "opaque whole-Pi demanded index" "native-bottom-nat" $
                    close demandN identityF bottomN observed
  where
    named :: String -> IO TyCon
    named label = case find ((== label) . occNameString . nameOccName . tyConName) tycons of
        Just tycon -> pure tycon
        Nothing -> fail ("FAIL: opaque whole-Pi missing type constructor " ++ label)

{- | Find nominal CoVar evidence between two distinct scoped payload types.
This accepts only the expected SubCo/CoVarCo value cast. Input Lint is mandatory.
-}
opaquePiCastIsScoped :: CoreExpr -> Bool
opaquePiCastIsScoped = go emptyVarSet emptyVarSet
  where
    -- \| Replace a shadowed Unique before extending the lexical binder sets.
    extend :: (TyVarSet, CoVarSet) -> Var -> (TyVarSet, CoVarSet)
    extend (typeScope, coScope) binder
        | isTyVar binder = (extendVarSet types binder, cos)
        | isCoVar binder = (types, extendVarSet cos binder)
        | otherwise = (types, cos)
      where
        types = delVarSet typeScope binder
        cos = delVarSet coScope binder

    -- \| Inspect a cast only under the binders that scope over that expression.
    candidate :: TyVarSet -> CoVarSet -> Coercion -> Bool
    candidate types cos (SubCo nominal@(CoVarCo actualQ)) =
        case lookupVarSet cos actualQ of
            Just scopedQ ->
                varType scopedQ `eqType` varType actualQ
                    && coercionRole nominal == Nominal
                    && retainedEndpoint left
                    && retainedEndpoint right
                    && not (left `eqType` right)
                    && sizeVarSet leftPayloads == 1
                    && sizeVarSet rightPayloads == 1
                    && disjointVarSet leftPayloads rightPayloads
                    && allVarSet scopedPayload (leftPayloads `unionVarSet` rightPayloads)
            Nothing -> False
      where
        left = coercionLKind nominal
        right = coercionRKind nominal
        leftPayloads = filterVarSet isTyVar (tyCoVarsOfType left)
        rightPayloads = filterVarSet isTyVar (tyCoVarsOfType right)
        scopedPayload variable = case lookupVarSet types variable of
            Just bound ->
                tyVarKind bound `eqType` tyVarKind variable
                    && tyVarKind bound `eqType` liftedTypeKind
            Nothing -> False
    candidate _ _ _ = False

    -- \| Require the two retained binder layers used by this fixture.
    retainedEndpoint :: Type -> Bool
    retainedEndpoint (DepFunTy DepFunVisible function (DepFunTy DepFunVisible index _)) =
        isNativeIndexFunctionId function && isNativeIndexId index
    retainedEndpoint _ = False

    -- \| Traverse term children with the lexical scope of each child.
    go :: TyVarSet -> CoVarSet -> CoreExpr -> Bool
    go types cos expression = case expression of
        Lam binder body ->
            let (types', cos') = extend (types, cos) binder
             in go types' cos' body
        App function argument -> go types cos function || go types cos argument
        Let (NonRec binder rhs) body ->
            let (types', cos') = extend (types, cos) binder
             in go types cos rhs || go types' cos' body
        Let (Rec pairs) body ->
            let (types', cos') = foldl extend (types, cos) (map fst pairs)
             in any (go types' cos' . snd) pairs || go types' cos' body
        Case scrut binder _ alternatives ->
            go types cos scrut || any (alternative (extend (types, cos) binder)) alternatives
        Cast body co -> candidate types cos co || go types cos body
        Tick _ body -> go types cos body
        _ -> False

    -- \| Keep alternative fields local to their own branch.
    alternative :: (TyVarSet, CoVarSet) -> CoreAlt -> Bool
    alternative scope (Alt _ binders body) =
        let (types, cos) = foldl extend scope binders
         in go types cos body
