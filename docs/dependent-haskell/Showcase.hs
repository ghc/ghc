{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Runnable examples of the completed Dependent Haskell source features.
-- See BOUNDARY-EVIDENCE.md for the optional plugin, splice, and observation examples.
-- The companion UseMapWitness.hs runs an ordinary optimized client. It checks
-- @firstFromProvider = checkedFirst checked@ with the producer's interface
-- and prints the head inferred through both a Maybe and a list.
-- In FileOffer.hs, an external search supplies a position for
-- @Member 8 (OrdinaryOffer.offered 7)@. GHC checks the generated path.
-- In GHCi, load PackedVecLibrary with the options in the adjacent README.
-- Enter these as separate commands to keep a retained function at the prompt.
--
-- > let fill :: foreach (n :: Nat) -> Vec Bool (1 + n); fill n = replicateNative (1 + n) True
-- > toList (fill 2)
module Main (main) where

import qualified BetaCallClient as Beta
import qualified CharacterExport
import qualified ClosedLengthProducer as Length
import qualified ClosedTableLibrary as Table
import qualified ComputedPair as Computed
import qualified ConstructorCallClient as Constructor
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import qualified Data.Char
import qualified Data.Either
import qualified ErasedPackage
import qualified ExistentialVec as E
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.List (isInfixOf)
import qualified Data.List
import qualified Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:) (Refl))
import qualified FiniteTree
import qualified FunctionFieldClient
import qualified FunctionFieldLibrary as FunctionFields
import qualified FunctionPackageLibrary as FunctionPackages
import qualified FunctionSignatureLibrary as FunctionSignatures
import qualified GroundApplicationClient as Ground
import GHC.Exts (FUN, M, U, Matchability (..), Multiplicity (Many))
import qualified GHC.TypeNats as TypeNats
import GHC.TypeLits (charVal)
import qualified IndexBetaClient as IndexBeta
import qualified IndexedMark
import qualified IndexedVec
import qualified KnownArgumentClient as Known
import qualified Language.Haskell.TH as TH
import qualified LocalOpen
import qualified LocalInstance
import qualified LocalAssociated
import qualified IndependentFamily
import qualified FamilySignature
import qualified ImportedFamily
import qualified MixedFamily
import qualified InterpretedKind
import qualified InvisibleFamily
import qualified TypeLambda
import qualified TypeLet
import qualified NativeCase
import qualified LocalDataFamily
import qualified LazyParcel
import qualified LegacyKindArgument
import qualified OpenBuildSource
import qualified OpenMapProof
import qualified InstalledNotProof
import qualified PreludeMapProof
import qualified OpenBuildWorker as Build
import qualified SpineWorker as Spine
import qualified OpenConstructorSource
import qualified OpenConstructorWorker as W
import qualified NativeMultiPackage as Packages
import Numeric.Natural (Natural)
import qualified OpenSource
import qualified Signal
import qualified PreludePairExport
import qualified OpaqueCallClient as Opaque
import qualified PairProjection as ComputedProjection
import qualified OrdinaryVecClient as Ordinary
import qualified ParameterizedIndexLibrary as Parameterized
import qualified PackedProjection
import PackedVecLibrary
import RetainedNat (Retained(..), constantPackage, withRetained)
import qualified DependentPair as Pair
import qualified RetainedProjection
import qualified QualifiedPiLibrary as Qualified
import qualified ReadNatural
import qualified Type.Reflection as Reflection
import qualified UnicodeExport

{- Note [Maintaining the Dependent Haskell showcase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Add a small, labeled example when a source feature is completed. Keep the
examples finite. Reuse the checked example libraries and show their types here
when the signature explains the feature. Compiler-only invariants remain in
CORE.md and the existing tests. A passing example does not prove totality.
The API fixture checks native dead-local removal and detected recursive
interface failures. This program checks evaluation and sharing through the
same native compiler path.

This module uses ordinary application checking. Imported providers keep their
own language extensions. GroundApplicationClient checks the native function
types used as equality operands with ImpredicativeTypes.

Run from the repository root with this fork's matching compiler and libraries:

  mkdir -p _build/showcase
  _build/stage1/bin/ghc --make -O0 -fomit-interface-pragmas \
    -fno-write-if-simplified-core -dcore-lint -dstg-lint \
    -idocs/dependent-haskell/examples/packed-vector \
    -idocs/dependent-haskell/examples/qualified-pi \
    -idocs/dependent-haskell/examples/closed-evaluation \
    -idocs/dependent-haskell/examples/open-evaluation \
    -outputdir _build/showcase docs/dependent-haskell/Showcase.hs \
    -o _build/showcase/showcase
  _build/showcase/showcase

General optimization and arbitrary dependent evaluation remain outside the
completed source subset. Function-value indices use the fixed closed-domain
policy in CORE.md.
-}

-- | Select different ordinary result types with a runtime Boolean.
type family Select (b :: Bool) where
    Select True = Int
    Select False = Bool

-- | Use a closed three-constructor domain as a runtime index.
data Light = Red | Amber | Green

-- | Select an ordinary payload type at each finite-domain constructor.
type family LightValue (light :: Light) where
    LightValue Red = Int
    LightValue Amber = Bool
    LightValue Green = Char

-- | Define an empty domain for checked empty-case elimination.
data NoIndex

-- | Leave the result at an impossible index abstract.
type family Unreachable (index :: NoIndex)

-- | Eliminate an empty retained domain with a checked constructor lambda.
absurdIndex :: foreach (index :: NoIndex) -> Unreachable index
absurdIndex = \case {}

-- | Fix a lifted result type before using visible type application.
bottomValue :: forall a. String -> a
bottomValue = error

-- | Print one finite observation with its feature label.
observe :: (Show a) => String -> a -> IO ()
observe label value = putStrLn (label ++ ": " ++ show value)

-- | Check that the demanded value raises its original error.
expectError :: String -> String -> IO a -> IO ()
expectError label marker action = do
    result <- try action
    case result of
        Left (err :: ErrorCall) -> do
            unless (marker `isInfixOf` displayException err) (fail (displayException err))
            putStrLn (label ++ ": " ++ marker)
        Right _ -> fail (label ++ ": expected an error")

-- | Render the actual stored length and vector through a retained callback.
renderPacked :: PackedVec Int -> (Int, [Int])
renderPacked package = withPacked package (\n xs -> (natToInt n, toList xs))

-- | Read the length, values and proof depth from a bounded package.
renderBounded :: BoundedVec Int limit -> (Int, [Int], Int)
renderBounded package = withBounded package (\n xs proof -> (natToInt n, toList xs, proofDepth proof))
  where
    -- Count the successor constructors in a length bound proof.
    proofDepth :: LE n m -> Int
    proofDepth LEZ = 0
    proofDepth (LES proof) = 1 + proofDepth proof

-- | Show constructor refinement and an ignored Boolean index and payload.
booleans :: IO ()
booleans = do
    cell <- newIORef True
    b <- readIORef cell
    observe "runtime Bool choice" (renderChoice b (choose b))
    observe "choose True has result Int" (choose True)
    observe "choose False has result Bool" (choose False)
    let bottomB :: Bool
        bottomB = error "showcase-bool"
    let payload :: Select bottomB
        payload = bottomValue @(Select bottomB) "showcase-payload"
    observe "keep ignores bottom index and payload" (keep bottomB payload)
  where
    -- Return a result at the selected Boolean type.
    choose :: foreach (b :: Bool) -> Select b
    choose True = 42
    choose False = False

    -- Ignore the retained index and ordinary payload.
    keep :: foreach (b :: Bool) -> Select b -> Int
    keep _ _ = 42

    -- Obtain the matching Show dictionary after refinement.
    renderChoice :: foreach (b :: Bool) -> Select b -> String
    renderChoice b value = case b of
        True -> show value
        False -> show value

-- | Show runtime indices, packages, algorithms, proofs and ordinary inference.
vectors :: IO ()
vectors = do
    indexCell <- newIORef (3 :: Nat)
    n <- readIORef indexCell
    otherCell <- newIORef (3 :: Nat)
    m <- readIORef otherCell
    let xs :: Vec Int n
        xs = replicateNative @Int n 7
        ys :: Vec Int m
        ys = replicateNative @Int m 8
        package = PackVec n xs
    observe "IORef length and dependent vector" (renderPacked (repack package))
    observe "nested package/vector patterns" (headPacked package)
    case unconsPacked package of
        Nothing -> fail "expected a nonempty package"
        Just (x, rest) -> observe "uncons retains the runtime predecessor" (x, renderPacked rest)
    case sameNat n m of
        Nothing -> fail "expected equal runtime lengths"
        Just Refl -> do
            let pairs :: Vec (Int, Int) n
                pairs = zipVec xs ys
            observe "Refl aligns independent runtime indices" (toList pairs)
    case zipPacked package (PackVec m ys) of
        Nothing -> fail "expected equal packages"
        Just paired -> observe "zip packages" (withPacked paired (\size values -> toList values))
    let sample = PackVec 3 (VCons (1 :: Int) (VCons 2 (VCons 3 VNil)))
    observe "reverse preserves length" (renderPacked (reversePacked sample))
    observe "append computes its result length" (renderPacked (appendPacked sample package))
    cutoffCell <- newIORef (1 :: Int)
    cutoff <- readIORef cutoffCell
    observe "filter computes a hidden result length" (renderPacked (filterPacked (> cutoff) sample))
    withPacked
        sample
        ( \size values -> do
            let bounded = filterBounded (> cutoff) values
            observe "filter returns a length bound proof" (renderBounded bounded)
            observe "consume a package without its proof" (renderPacked (repackWithoutProof bounded))
        )
    case sumNat n m of
        Sum total Refl -> observe "runtime addition with equality evidence" (natToInt total)
    case rightZero n of
        Refl -> observe "arithmetic induction proof" (toList xs)
    effects <- newIORef (0 :: Int)
    let action :: IO (Vec Int n)
        action = modifyIORef' effects (+ 1) >> pure xs
        computation = do
            values <- action
            pure (toList values)
        section = (xs,)
    first <- computation
    second <- computation
    count <- readIORef effects
    observe "saved IO action runs twice" (first, second, count)
    observe "native values in tuple sections" (toList (fst (section True)))
    let
        -- Check a lambda against a retained function type.
        identity :: foreach (size :: Nat) -> Vec Int size -> Vec Int size
        identity = \size values -> values
        -- Construct a vector at the supplied runtime index.
        worker :: foreach (size :: Nat) -> Vec Int size
        worker size = replicateNative @Int size 7
        built :: foreach (size :: Nat) -> Vec Int size
        built = \case
            Z -> VNil
            S k -> VCons 42 (replicateNative @Int k 42)
        linearProof :: foreach (size :: Nat) -> (Add size Z :~: size) %1 -> Int
        linearProof = \size proof -> case proof of Refl -> 42
        transported :: foreach (size :: Nat) -> Vec Int size
        transported = transportElementWith @Int @Int Refl worker
    observe "checked retained lambda" (toList (identity n xs))
    observe "checked constructor lambda" (toList (built n))
    observe "linear equality witness beneath a retained binder" (linearProof n (rightZero n))
    observe "equality evidence beneath a retained Pi" (toList (transported n))
    observe "immediate index beta reduction" (toList (IndexBeta.betaSuccessor n 5))
    observe "explicit erased choices and qualified Pi" (toList (Ground.groundDirect n Qualified.fixedQ))
    observe "known arguments select erased choices" (toList (Known.knownDirect n Qualified.fixedQ))
    observe "generic consumer of a native function" (Known.knownGenericIgnore Qualified.fixedQ)
    observe "qualified native functions in tuples" (toList (Qualified.headTuple (Qualified.fixedQ, Qualified.fixedQ) n))
    observe "ignored equality witness stays lazy" (toList (Ground.groundIgnoreProof Qualified.fixedQ n))
    expectError "demanded equality witness" "native-ground-bottom-proof" $
        evaluate (length (toList (Ground.groundDemandProof Qualified.fixedQ n)))
    observe "opaque call refines its whole result" (Opaque.probeCall Opaque.identityNat n xs)
    observe "constructor argument call index" (Constructor.constructorLength Opaque.identityNat n xs)
    observe "signed beta inside an opaque call" (Beta.betaCallSuccessor Opaque.identityNat n xs)
    observe "guarded package rows" (renderPacked (guardedRepack False sample))

-- | Pass runtime function values as retained binders and use exact call evidence.
functions :: IO ()
functions = do
    cell <- newIORef (2 :: Nat)
    n <- readIORef cell
    let f :: Nat -> Nat
        f = Opaque.identityNat
        constant :: Nat -> Nat
        constant = Opaque.constantZero
        bottomN :: Nat
        bottomN = error "showcase-function-index"
        bottomF :: Nat -> Nat
        bottomF = error "showcase-function-head"
    let payload :: Vec Int (bottomF bottomN)
        payload = bottomValue @(Vec Int (bottomF bottomN)) "showcase-function-payload"
    observe "fresh worker under call evidence" (direct f n)
    observe "fresh worker at a constructed index" (constructed f n)
    observe "constant function ignores a bottom predecessor" (constructed constant bottomN)
    observe "retained function and payload stay lazy" (keepCall bottomF bottomN payload)
  where
    -- Ignore the retained head, index and payload.
    keepCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Int
    keepCall _ _ _ = 42

    -- Preserve the original payload at its exact call index.
    idCall :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> Vec Int (f n) -> Vec Int (f n)
    idCall f n payload = payload

    -- Apply the worker under the result constructor's equality.
    direct :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]
    direct f n = case f n of
        Z -> toList (idCall f n VNil)
        S k -> toList (idCall f n (VCons 42 (replicateNative @Int k 42)))

    -- Retain constructor arguments during worker activation.
    constructed :: foreach (f :: Nat -> Nat) -> foreach (n :: Nat) -> [Int]
    constructed f n = case f (S n) of
        Z -> toList (idCall f (S n) VNil)
        S k -> toList (idCall f (S n) (VCons 42 (replicateNative @Int k 42)))

-- | Store the actual function and input used by a later payload type.
functionFields :: IO ()
functionFields = do
    cell <- newIORef (3 :: Nat)
    n <- readIORef cell
    let successor :: Nat -> Nat
        successor size = S size
        package = FunctionPackages.buildPack @Int successor n 42
    case FunctionPackages.repackFunction package of
        FunctionPackages.FunctionPack storedFunction storedInput values -> do
            observe "stored function, input and dependent payload" (natToInt (storedFunction storedInput), toList values)
            observe "imported function signature preserves payload" (toList (FunctionSignatures.idCall @Int storedFunction storedInput values))
    observe "generic payload in a function package" (FunctionPackages.inspectFunction (FunctionPackages.buildPack @Char successor n 'q'))
    observe "function package fields stay lazy" (FunctionPackages.ignoreFunction (FunctionPackages.lazyPack @Int (error "showcase-stored-function") (error "showcase-stored-input")))
    observe "imported function keeps ignored fields lazy" (ignoreImported (FunctionPackages.lazyPack @Int (error "showcase-imported-function") (error "showcase-imported-input")))
  where
    -- | Apply the imported worker without demanding its result.
    ignoreImported :: FunctionPackages.FunctionPack Int -> Int
    ignoreImported (FunctionPackages.FunctionPack f n xs) =
        const 42 (FunctionSignatures.idCall @Int f n xs)

-- | Expose a stored function through a saturated family and preserve laziness.
algebraicFunctionFields :: IO ()
algebraicFunctionFields = do
    cell <- newIORef False
    b <- readIORef cell
    observe "function value in an algebraic index" $
        FunctionFieldClient.inspect (FunctionFields.build not b)
    observe "family-exposed call uses imported proof" $
        FunctionFieldClient.inspectViaCall (FunctionFields.repack (FunctionFields.build id b))
    observe "proof retains the stored function identity" $
        FunctionFieldClient.proofOnly (FunctionFields.build not b)
    observe "algebraic function container stays lazy" $
        FunctionFieldClient.outer (FunctionFields.loose (error "showcase-function-container") (error "showcase-function-input"))
    observe "algebraic function leaf stays lazy" $
        FunctionFieldClient.which (FunctionFields.loose (Just (error "showcase-function-leaf")) (error "showcase-function-input"))

-- | Open either constructor without sharing their retained field binders.
alternatives :: IO ()
alternatives = do
    cell <- newIORef (2 :: Nat)
    n <- readIORef cell
    observe "absent package constructor" (fmap renderPacked (Packages.toPacked (Packages.Unavailable :: Packages.Availability Int)))
    observe "present package constructor" (fmap renderPacked (Packages.toPacked (Packages.makeAvailable n)))
    observe "opposite constructor declaration order" (fmap renderPacked (Packages.toPackedReverse (Packages.makeReverse n)))
    observe "nested vector pattern in a retained alternative" (Packages.firstAvailable (Packages.makeAvailable n))
    let bottomN :: Nat
        bottomN = error "showcase-package-index"
    observe "failed guard preserves lazy fields" (Packages.guardedRows False (Packages.lazyAvailable bottomN))

-- | Show a finite source domain and demand in an empty domain.
finiteDomains :: IO ()
finiteDomains = do
    cell <- newIORef Green
    light <- readIORef cell
    observe "three-constructor runtime domain" (renderLight light (chooseLight light))
    let impossible :: NoIndex
        impossible = error "showcase-empty-domain"
    expectError "empty-domain elimination demands its scrutinee" "showcase-empty-domain" (evaluate (absurdIndex impossible))
  where
    -- \| Return the payload selected by the runtime constructor.
    chooseLight :: foreach (light :: Light) -> LightValue light
    chooseLight Red = 42
    chooseLight Amber = False
    chooseLight Green = 'G'

    -- \| Obtain the matching dictionary after finite-domain refinement.
    renderLight :: foreach (light :: Light) -> LightValue light -> String
    renderLight light value = case light of
        Red -> show value
        Amber -> show value
        Green -> show value

-- | Keep later runtime indices in separate scopes after an ordinary argument.
forwardLater :: foreach (n :: Nat) -> Vec Int n -> foreach (m :: Nat) -> foreach (k :: Nat) -> Vec Int m -> Vec Int k -> (PackedVec Int, PackedVec Int, Int)
forwardLater Z firstPayload m k middlePayload lastPayload =
    let right () = PackVec k lastPayload
        shadow m = m + 1
     in (left (), right (), shadow 41)
  where
    left () = PackVec m middlePayload
forwardLater (S predecessor) firstPayload middle final middlePayload lastPayload =
    let left () = PackVec middle middlePayload
        shadow middle = middle + 1
     in (left (), right (), shadow 41)
  where
    right () = PackVec final lastPayload

-- | Keep a leading class dictionary across later retained arguments.
leadingContext :: forall a. (Show a) => foreach (n :: Nat) -> Vec a n -> foreach (m :: Nat) -> Vec a m -> String
leadingContext Z xs right ys = show (toList xs, toList ys)
leadingContext (S predecessor) xs later ys = show (toList xs, toList ys)

-- | Use equality evidence between retained argument positions.
keepWithLaterEvidence :: foreach (n :: Nat) (m :: Nat) -> (n ~ m) => foreach (k :: Nat) -> Vec Int m -> Vec Int k -> (Vec Int n, Vec Int k)
keepWithLaterEvidence Z _ _ xs ys = (xs, ys)
keepWithLaterEvidence (S _) _ _ xs ys = (xs, ys)

-- | Read distinct indices and observe their separate argument positions.
laterArguments :: IO ()
laterArguments = do
    firstCell <- newIORef (0 :: Nat)
    middleCell <- newIORef (2 :: Nat)
    lastCell <- newIORef (3 :: Nat)
    n <- readIORef firstCell
    m <- readIORef middleCell
    k <- readIORef lastCell
    let first :: Vec Int n
        first = replicateNative @Int n 7
        middle :: Vec Int m
        middle = replicateNative @Int m 8
        final :: Vec Int k
        final = replicateNative @Int k 9
        (left, right, shadow) = forwardLater n first m k middle final
        equalPair :: (Vec Int n, Vec Int k)
        equalPair = keepWithLaterEvidence n n k first final
    observe "later retained indices and local shadowing" (renderPacked left, renderPacked right, shadow)
    observe "leading class dictionary" (leadingContext @Int n first m middle)
    case equalPair of
        (equalFirst, separateLast) -> observe "equality context before a later retained index" (toList equalFirst, toList separateLast)

-- | Retain ordinary parameterized values and check package and call demand.
parameterizedDomains :: IO ()
parameterizedDomains = do
    listCell <- newIORef [True, False, True]
    bs <- readIORef listCell
    observe "runtime list as retained index" (toList (Parameterized.listVector bs))
    observe "list-indexed package" (Parameterized.openListVector (Parameterized.packListVector bs))
    natCell <- newIORef (2 :: Nat)
    n <- readIORef natCell
    observe "optional retained Nat" (Parameterized.optionalRoundtrip n)
    treeCell <- newIORef (Parameterized.Node (Parameterized.Leaf True) (Parameterized.Leaf False))
    tree <- readIORef treeCell
    observe "runtime tree selects nested payload" (Parameterized.renderTree tree (Parameterized.treePayload tree))
    _ <- evaluate (Parameterized.packListVector (error "unused retained list"))
    observe "list package does not force its index" ()
    observe "opaque call ignores retained list"
        (Parameterized.observeListCall (\ignored -> 2) (error "unused opaque call list"))

-- | Convert nominally equal scalar domains and preserve function demand.
domainConversion :: IO ()
domainConversion = do
    lengthCell <- newIORef (2 :: Nat)
    n <- readIORef lengthCell
    tokenCell <- newIORef (Parameterized.DomainToken :: Parameterized.TargetToken)
    token <- readIORef tokenCell
    let f :: Parameterized.SourceToken -> Nat
        f ignored = n
    let source :: Parameterized.SourceToken
        source = Parameterized.DomainToken
        ignoredToken :: Parameterized.TargetToken
        ignoredToken = bottomValue "showcase-domain-index"
        demanding :: Parameterized.SourceToken -> Nat
        demanding Parameterized.DomainToken = Z
        bottomHead :: Parameterized.SourceToken -> Nat
        bottomHead = bottomValue "showcase-domain-head"
        bottomWorker :: foreach (x :: Parameterized.SourceToken) -> Vec Int (f x)
        bottomWorker = bottomValue "showcase-domain-function"
        roundtrip :: foreach (x :: Parameterized.SourceToken) -> Vec Int (f x)
        roundtrip = Parameterized.adaptTokenVectorBack f
            (Parameterized.adaptTokenVector f (Parameterized.tokenVector f 9))
    observe "nominal domain conversion from source"
        (toList (Parameterized.adaptTokenVector f (Parameterized.tokenVector f 42) token))
    observe "converted argument application" (toList (Parameterized.applyTokenVector f token))
    observe "payload inference below retained binder" (toList (Parameterized.specializedTokenVector f token))
    observe "nominal domain conversion in both directions" (toList (roundtrip source))
    observe "domain cast ignores bottom index"
        (toList (Parameterized.adaptTokenVector f (Parameterized.tokenVector f 5) ignoredToken))
    Parameterized.forceTokenFunction f (Parameterized.tokenVector f 42)
    observe "converted partial function WHNF" ()
    expectError "converted bottom function WHNF" "showcase-domain-function"
        (Parameterized.forceTokenFunction f bottomWorker)
    expectError "domain cast preserves demanded index" "showcase-domain-index"
        (evaluate (length (toList (Parameterized.applyTokenVector demanding ignoredToken))))
    expectError "domain conversion preserves function demand order" "showcase-domain-head"
        (evaluate (length (toList (Parameterized.applyTokenVector bottomHead ignoredToken))))

-- | Compare complete workers and retain their runtime function demand.
fixedFunctionDomains :: IO ()
fixedFunctionDomains = do
    lengthCell <- newIORef (2 :: Nat)
    n <- readIORef lengthCell
    identityCell <- newIORef (id :: Nat -> Nat)
    selectedIdentity <- readIORef identityCell
    successorCell <- newIORef (S :: Nat -> Nat)
    selectedSuccessor <- readIORef successorCell
    constantCell <- newIORef (const 2 :: Nat -> Nat)
    selectedConstant <- readIORef constantCell
    let f :: Nat -> Nat
        f = selectedIdentity
        successor :: Nat -> Nat
        successor = selectedSuccessor
        constant :: Nat -> Nat
        constant = selectedConstant
        bottomIndex :: Nat
        bottomIndex = bottomValue "showcase-function-body-index"
        bottomHead :: Nat -> Nat
        bottomHead = bottomValue "showcase-function-body-head"
        bottomWorker :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m)
        bottomWorker = bottomValue "showcase-function-body-worker"
    observe "whole function worker selects Int"
        (toList (FunctionSignatures.specializedCall f n))
    observe "preallocated payload below function binder"
        (toList (FunctionSignatures.allocatedCall f n))
    observe "runtime successor determines vector length"
        (toList (FunctionSignatures.specializedCall successor n))
    observe "runtime constant ignores bottom index"
        (toList (FunctionSignatures.specializedCall constant bottomIndex))
    observe "ordinary equality below function binder"
        (toList (FunctionSignatures.transportCall @Int @Int (FunctionSignatures.numericCall @Int) f n))
    observe "shadowed retained function binders"
        (toList (FunctionSignatures.shadowedCall constant n))
    FunctionSignatures.forceCall FunctionSignatures.specializedCall
    observe "whole function worker WHNF" ()
    expectError "bottom whole function worker WHNF" "showcase-function-body-worker"
        (FunctionSignatures.forceCall bottomWorker)
    FunctionSignatures.forcePartialCall bottomHead (FunctionSignatures.specializedCall bottomHead)
    observe "partial worker ignores bottom function" ()
    expectError "function body preserves head demand order" "showcase-function-body-head"
        (evaluate (length (toList (FunctionSignatures.specializedCall bottomHead bottomIndex))))
    expectError "function body preserves index demand" "showcase-function-body-index"
        (evaluate (length (toList (FunctionSignatures.specializedCall f bottomIndex))))

-- | Check runtime positions and preserve finite access to lazy lists.
closedListIndexing :: IO ()
closedListIndexing = do
    listCell <- newIORef [True, False, True]
    positionCell <- newIORef 1
    xs <- readIORef listCell
    position <- readIORef positionCell
    observe "runtime checked list position" (Parameterized.lookupRuntime xs position)
    observe "negative position ignores list"
        (Parameterized.lookupRuntime (error "unused negative-position list") (-1))
    observe "out-of-range list position" (Parameterized.lookupRuntime xs 3)
    observe "checked first position ignores tail"
        (Parameterized.lookupRuntime (True : error "unused checked-position tail") 0)
    let cyclic :: [Bool]
        cyclic = True : False : cyclic
    observe "checked position in cyclic list" (Parameterized.lookupRuntime cyclic 5)

-- | Select retained workers with explicit types and preserve their demand.
completeFunctionArguments :: IO ()
completeFunctionArguments = do
    lengthCell <- newIORef (2 :: Nat)
    functionCell <- newIORef (id :: Nat -> Nat)
    n <- readIORef lengthCell
    selected <- readIORef functionCell
    let f :: Nat -> Nat
        f = selected
        bottomWorker :: foreach (g :: Nat -> Nat) -> foreach (m :: Nat) -> Vec Int (g m)
        bottomWorker = bottomValue "showcase-explicit-worker"
    observe "explicit complete worker runtime application"
        (toList (FunctionSignatures.selectCall FunctionSignatures.specializedCall f n))
    FunctionSignatures.forceExplicitCall FunctionSignatures.specializedCall
    observe "explicit complete worker WHNF" ()
    expectError "explicit bottom worker WHNF" "showcase-explicit-worker"
        (FunctionSignatures.forceExplicitCall bottomWorker)
    FunctionSignatures.forceNestedCall bottomWorker
    observe "explicit nested type ignores bottom worker" ()

-- | Observe constructor and runtime arrow matchability through public reflection.
arrowMatchability :: IO ()
arrowMatchability = do
    let ordinary = Reflection.typeRep @(Int -> Bool)
    observe "constructor kind arrow is matchable" $
        case Reflection.typeRepKind (Reflection.typeRep @Maybe) of
            Reflection.Fun' metadata _ _ ->
                Reflection.SomeTypeRep metadata == Reflection.SomeTypeRep (Reflection.typeRep @M)
            _ -> False
    observe "runtime function arrow is unmatchable" $
        case ordinary of
            Reflection.Fun' metadata _ _ ->
                Reflection.SomeTypeRep metadata == Reflection.SomeTypeRep (Reflection.typeRep @U)
            _ -> False
    observe "matchability distinguishes function types" $
        Reflection.SomeTypeRep (Reflection.typeRep @(FUN @M 'Many Int Bool))
            /= Reflection.SomeTypeRep ordinary

-- | Preserve an existing matchable forall without a new type lambda.
reuseMatchable :: (forall a. @M a -> a) -> (forall a. @M a -> a)
reuseMatchable f = f

-- | Use explicit U for an ordinary runtime type abstraction.
identityForallU :: forall a. @U a -> a
identityForallU x = x

-- | Keep the result kind matchable until the family consumes each argument.
type ForallF0 :: forall k -> k -> Type
type family ForallF0

-- | Consume only the visible kind argument.
type ForallF1 :: forall k -> k -> Type
type family ForallF1 k

-- | Consume both arguments and classify both as U.
type ForallF2 :: forall k -> k -> Type
type family ForallF2 k a

-- | Compute the annotation without replacing it in the stored kind.
type family StoredMatchability :: Matchability where
    StoredMatchability = M

-- | Use computed M on the erased constructor-kind binder.
type ForallData :: forall k. @StoredMatchability k -> Type
data ForallData (a :: k)

-- End the declaration group before reification.
$(pure [])

-- | Observe source forall classification and public reflection.
forallMatchability :: IO ()
forallMatchability = do
    observe "explicit runtime forall" (identityForallU (42 :: Int))
    observe "family prefix matchability (M is True)" $(do
        let tag ma
                | ma `elem` [TH.ConT ''M, TH.PromotedT 'Matchable] = True
                | ma `elem` [TH.ConT ''U, TH.PromotedT 'Unmatchable] = False
                | otherwise = error ("Unexpected matchability: " ++ show ma)
            metadata (TH.ForallMatchT ma (TH.ForallVisT [_] body)) = tag ma : metadata body
            metadata (TH.AppT (TH.AppT (TH.AppT (TH.AppKindT (TH.ConT fun) ma) _) _) rest)
                | fun == ''FUN = tag ma : metadata rest
            metadata _ = []
        kinds <- mapM TH.reifyType [''ForallF0, ''ForallF1, ''ForallF2]
        pure $ TH.ListE
            [TH.ListE [TH.ConE (if ma then 'True else 'False) | ma <- metadata kind] | kind <- kinds])
    observe "computed forall metadata is retained" $(do
        kind <- TH.reifyType ''ForallData
        let retained = case kind of
                TH.ForallMatchT (TH.ConT name) (TH.ForallT [_] [] _) -> name == ''StoredMatchability
                _ -> False
        pure (TH.ConE (if retained then 'True else 'False)))
    observe "computed forall kind supports reflection" $
        case Reflection.typeRep @(ForallData @Type Int) of
            Reflection.App _ argument ->
                Reflection.SomeTypeRep argument == Reflection.SomeTypeRep (Reflection.typeRep @Int)
            _ -> False

-- | Keep a boxed value opaque inside a structural list.
data GenericOpaque = GenericOpaque Int deriving (Eq, Show)

-- | Keep a newtype value opaque inside a structural list.
newtype GenericHidden = GenericHidden Int deriving (Eq, Show)

-- | Observe generic list indices without adding demand to their elements.
genericListIndexing :: IO ()
genericListIndexing = do
    flagRef <- newIORef True
    flag <- readIORef flagRef
    let bools = [flag, False]
    observe "generic Boolean list" (toList (genericVector @Bool bools))
    let integers = [11, 22] :: [Int]
    observe "generic inferred Int list" (toList (genericVector integers))
    observe "generic explicit Int list" (toList (genericVector @Int integers))
    let functions = [not]
    observe "generic function elements" (map ($ True) (toList (genericVector functions)))
    let actions = [pure 7, pure 9] :: [IO Int]
    sequence (toList (genericVector actions)) >>= observe "generic IO elements"
    let opaque = [GenericOpaque 11]
    observe "generic boxed elements" (toList (genericVector opaque))
    let hidden = [GenericHidden 13]
    observe "generic newtype elements" (toList (genericVector hidden))
    let partial = (error "opaque head forced" :: Int) : error "opaque tail forced"
    observe "generic outer list constructor" (genericTag partial)
    observe "generic partial vector prefix" (length (take 1 (toList (genericVector partial))))
    let cyclic = 17 : cyclic :: [Int]
    observe "generic cyclic list prefix" (take 3 (toList (genericVector cyclic)))
    sizeRef <- newIORef (2 :: Nat)
    n <- readIORef sizeRef
    let vector = replicateNative @Int n 23
    let vectors = [vector]
    observe "generic runtime-indexed elements" (map toList (toList (genericNested n vectors)))
    observe "generic refined runtime domain" (map toList (toList (genericRefinedNested n vectors)))
    let emptyVector = replicateNative @Int Z 29
    let emptyVectors = [emptyVector]
    observe "generic zero runtime domain" (map toList (toList (genericRefinedNested Z emptyVectors)))
    let constructed = replicateNative @Int (S n) 31
    let constructedVectors = [constructed]
    observe "generic constructed runtime domain" (map toList (toList (genericNested (S n) constructedVectors)))
    expectError "generic opaque head demand" "opaque head forced" $
        evaluate (case genericFirst partial of Nothing -> 0; Just x -> x)
    let lazySize = error "partial worker forced its index" :: Nat
    let suspended :: foreach (ys :: [Vec Int lazySize]) -> Vec (Vec Int lazySize) (GenericCount ys)
        suspended = genericRefinedNested lazySize
    _ <- evaluate @(foreach (ys :: [Vec Int lazySize]) -> Vec (Vec Int lazySize) (GenericCount ys)) suspended
    observe "generic partial worker WHNF" True

-- | Use imported retained APIs from a module without dependent source syntax.
ordinaryModuleCalls :: IO ()
ordinaryModuleCalls = do
    sizeRef <- newIORef (2 :: Nat)
    n <- readIORef sizeRef
    observe "ordinary module runtime replicate" (Ordinary.ordinaryReplicate n (7 :: Int))
    valuesRef <- newIORef ([11, 22] :: [Int])
    values <- readIORef valuesRef
    observe "ordinary module generic round-trip" (Ordinary.ordinaryRoundTrip values)

-- | Check finite positions in lists with opaque elements.
genericSafeIndexing :: IO ()
genericSafeIndexing = do
    valuesRef <- newIORef ([11, 22] :: [Int])
    positionRef <- newIORef (1 :: Int)
    values <- readIORef valuesRef
    position <- readIORef positionRef
    let selected = case Parameterized.mkFinGeneric @Int values position of
            Nothing -> Nothing
            Just checked -> Just (Parameterized.indexGeneric @Int values checked)
    observe "generic checked Int position" selected
    observe "generic negative position ignores list"
        (Parameterized.lookupGeneric (error "unused generic negative-position list" :: [Int]) (-1))
    let functions = [id, (* 2)] :: [Int -> Int]
    observe "generic checked function position"
        (fmap ($ 21) (Parameterized.lookupGeneric functions position))
    sizeRef <- newIORef (2 :: Nat)
    n <- readIORef sizeRef
    let vector = replicateNative @Int n 7
    let vectors = [vector]
    let selectedVector = case Parameterized.lookupGeneric vectors 0 of
            Nothing -> Nothing
            Just chosen -> Just (toList chosen)
    observe "generic checked runtime-indexed payload" selectedVector
    let partial :: [Int]
        partial = error "unused generic safe-index head" : [29]
    observe "generic checked position ignores head"
        (Parameterized.lookupGeneric partial 1)
    let cyclic :: [Int]
        cyclic = 31 : cyclic
    observe "generic checked position in cyclic list"
        (Parameterized.lookupGeneric cyclic 5)
    observe "generic vector lookup through fmap"
        (fmap toList (Parameterized.lookupGeneric [vector] 0))

-- | Return the runtime length inferred from the vector's declared index.
readInvisibleLength :: forall a. foreach (n :: Nat). Vec a n -> Int
readInvisibleLength _ = natToInt n

-- | Ignore both the vector and its invisible runtime argument.
ignoreInvisibleLength :: forall a. foreach (n :: Nat). Vec a n -> Int
ignoreInvisibleLength _ = 42

-- | Construct a vector indexed by the explicit runtime length.
makeInvisibleVector :: foreach (n :: Nat). Vec Int n
makeInvisibleVector = replicateNative @Int n 7

-- | Compare omitted and explicit calls with an IO-sourced runtime index.
invisibleRetainedArguments :: IO ()
invisibleRetainedArguments = do
    n <- newIORef (2 :: Nat) >>= readIORef
    let vector :: Vec Int n
        vector = makeInvisibleVector @{n}
    observe "invisible retained length" (readInvisibleLength vector)
    observe "invisible retained length with @Int" (readInvisibleLength @Int vector)
    observe "explicit invisible retained length" (readInvisibleLength @{n} vector)
    observe "explicit invisible retained length with @Int" (readInvisibleLength @Int @{n} vector)
    bottom <- newIORef (bottomValue @Nat "ignored invisible length") >>= readIORef
    let unused :: Vec Int bottom
        unused = replicateNative @Int bottom 7
    observe "invisible retained ignores bottom" (ignoreInvisibleLength unused)
    observe "explicit invisible retained ignores bottom" (ignoreInvisibleLength @Int @{bottom} unused)

-- | Recover both runtime lengths from the vector declarations.
inferredAppend :: IO ()
inferredAppend = do
    n <- newIORef (2 :: Nat) >>= readIORef
    m <- newIORef (1 :: Nat) >>= readIORef
    let xs :: Vec Int n
        xs = replicateNative @Int n 7
        ys :: Vec Int m
        ys = replicateNative @Int m 9
        joined :: Vec Int (n + m)
        joined = appendHidden xs ys
    observe "append with inferred lengths" (toList joined)
    observe "append with first length written" (toList (appendHidden @{n} xs ys))

-- | Run each completed source feature group with finite runtime observations.
-- | Both strict cases select the first input field even when the pair is unknown.
duplicateProjection :: forall (p :: (Bool, Char)). Proxy p
                    -> Prelude.snd (PreludePairExport.duplicateFirst p) :~: Prelude.fst p
duplicateProjection _ = Refl

main :: IO ()
main = do
    putStrLn "Dependent Haskell: completed source features"
    booleans
    finiteDomains
    vectors
    functions
    functionFields
    laterArguments
    alternatives
    parameterizedDomains
    domainConversion
    fixedFunctionDomains
    closedListIndexing
    completeFunctionArguments
    arrowMatchability
    forallMatchability
    algebraicFunctionFields
    genericListIndexing
    ordinaryModuleCalls
    genericSafeIndexing
    invisibleRetainedArguments
    inferredAppend
    closedSourceFunctions
    constructorFields
    recursiveLengths
    sourceAddition
    arithmeticNotation
    listBoundaries
    observe "ordinary length evaluated while checking the type" (TypeNats.natVal closedLength)
    observe "installed Prelude.length converted to a Nat index" (TypeNats.natVal installedLength)
    observe "ordinary identity applied to a type variable" (openIdentity @Int)
    observe "uppercase character and digit proof from Data.Char" checkedCharacters
    observe "Prelude.const selects the first type variable" (openLeft 'x' (undefined :: Bool))
    observe "Prelude.sum in a type" (TypeNats.natVal (Proxy @(Prelude.sum '[2, 3, 5])))
    observe "title case after the pattern determines the character"
        (charVal (titleFromPattern (Proxy @'a')))
    observe "Prelude.reverse '[2, 3, 5] equals '[5, 3, 2]"
        (Refl :: Prelude.reverse '[2, 3, 5] :~: '[5, 3, 2])
    observe "sum of a list reversed during type checking"
        (TypeNats.natVal (Proxy @(Prelude.sum (Prelude.reverse '[2, 3, 5]))))
    observe "an imported character case selects between two unknown types"
        (OpenSource.routedFirst False (42 :: Int), OpenSource.routedSecond False (42 :: Int))
    observe "odd accepts the Natural computed by sum"
        (Refl :: Prelude.odd (Prelude.sum '[2, 3, 5]) :~: 'False)
    observe "odd applied directly to the literal 5" (Refl :: odd 5 :~: 'True)
    observe "an exported character equality at a mapped and a default input"
        [CharacterExport.p3 (Proxy @'x'), CharacterExport.p3 (Proxy @'!')]
    observe "Unicode code points from an exported Data.Char.toUpper equality"
        (map fromEnum [UnicodeExport.p3 (Proxy @'é'), UnicodeExport.p3 (Proxy @'\x1e943')])
    observe "greatest common divisor checked in a type"
        (TypeNats.natVal (Proxy @(Prelude.gcd 18 24)))
    observe "first and second fields proved through an imported pair equality"
        ( PreludePairExport.first (Proxy @'( 'True, 'x')) (Proxy @'True)
        , PreludePairExport.second (Proxy @'( 'True, 'x')) (Proxy @'x') )
    observe "first and second projections from a pair of unknown types"
        (OpenSource.tupleFirst False (42 :: Int), OpenSource.tupleSecond False (42 :: Int))
    observe "quotient and remainder returned during type checking"
        (reflectPair (Proxy @(Prelude.quotRem 17 5)))
    observe "listToMaybe at a nonempty list and at an empty list"
        ( TypeNats.natVal (Proxy @(Data.Maybe.fromMaybe 99 (Data.Maybe.listToMaybe '[2,3])))
        , TypeNats.natVal (Proxy @(Data.Maybe.fromMaybe 99 (Data.Maybe.listToMaybe '[]))) )
    observe "readEither at valid and invalid input with a default of 99"
        ( TypeNats.natVal (Proxy @(Data.Either.fromRight 99 (ReadNatural.readNatural '[ '2','3'])))
        , TypeNats.natVal (Proxy @(Data.Either.fromRight 99 (ReadNatural.readNatural '[ 'x']))) )
    observe "concatenate all suffixes of [1,2,3] during type checking"
        (Refl :: Prelude.concat (Data.List.tails '[1,2,3]) :~: '[1,2,3,2,3,3])
    observe "collect the present values from an optional list literal"
        (Refl :: Data.Maybe.catMaybes '[ 'Nothing, 'Just 3] :~: '[3])
    observe "project rigid fields after swapping a known pair"
        ( PreludePairExport.nestedFirst (Proxy @'True)
        , PreludePairExport.nestedSecond (Proxy @'x') )
    observe "project through a swap proved for an unknown pair"
        ( PreludePairExport.abstractFirst (Proxy @'( 'True, 'x'))
        , PreludePairExport.abstractSecond (Proxy @'( 'True, 'x')) )
    observe "check a user-defined signal and pass it to another ordinary call"
        ( Refl :: Signal.at 2 :~: 'Signal.Stop
        , Refl :: Signal.shouldStop (Signal.at 3) :~: 'False )
    observe "project the second copy of an unknown pair's first field"
        (duplicateProjection (Proxy @'( 'True, 'x')))
    observe "read the leaves of a tree returned by an ordinary function"
        (Refl :: FiniteTree.leaves (FiniteTree.sample 4) :~: '[4,5,6])
    observe "select a type with a function defined later in the same module"
        (LocalOpen.keepFirst 'x' False)
    observe "use a chain of local functions on types and on values"
        (Refl :: LocalOpen.LastOf Char Bool :~: Bool, LocalOpen.lastOf 'x' 'y')
    observe "read the fields of a result indexed by True"
        (Refl :: IndexedMark.readMark (IndexedMark.sample 6) :~: 67)
    observe "read the head of an ordinary length-indexed vector"
        (Refl :: IndexedVec.safeHead (IndexedVec.two 3) :~: 3)
    observe "ignore a recursive helper in a type and at runtime"
        (Refl :: LocalOpen.LazyFirst Char Bool :~: Char, LocalOpen.lazyFirst 'x' 'y')
    observe "open erased packages and leave an unused payload lazy"
        ErasedPackage.observation
    observe "read the cells of a vector packaged with its length"
        (Refl :: E.readSomeVec ('E.SomeVec ('E.SS ('E.SS 'E.SZ))
                    ('IndexedVec.VCons 4 ('IndexedVec.VCons 5 'IndexedVec.VNil))) :~: '[4, 5])
    observe "select a type with a locally declared route"
        (Refl :: LocalOpen.Routed Char Bool :~: Bool, LocalOpen.route 'x' 'y')
    observe "select unknown fields from known constructors"
        (OpenConstructorSource.left True 'x', OpenConstructorSource.nested (7 :: Int) 'z')
    observe "read a length witness returned by an ordinary function"
        ( Refl :: E.someVec 4 :~: 'E.SomeVec ('E.SS ('E.SS 'E.SZ))
                      ('IndexedVec.VCons 4 ('IndexedVec.VCons 5 'IndexedVec.VNil))
        , E.readSomeVec (E.someVec 4) )
    observe "read one field of a private parcel without forcing the other"
        (Refl :: LazyParcel.first (LazyParcel.make 7) :~: 7, LazyParcel.first (LazyParcel.make 7))
    observe "construct a result from two symbolic fields"
        (OpenBuildSource.exact (Proxy @Char) (Proxy @Bool))
    observe "use a derived instance beside a dependent type"
        LocalOpen.routeLabel
    observe "a literal sibling fixes a nested numeric conversion"
        (Refl :: Prelude.max (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 2 :~: 3)
    observe "an ordinary datatype infers the arrow required by Maybe"
        (LegacyKindArgument.maybeField (Just True))
    observe "supply the pair to an exported swap after typechecking its definition"
        (Refl :: OpenBuildSource.Swap '( 'True, 'x') :~: '( 'x', 'True))
    observe "a computed sibling fixes a nested numeric conversion"
        (Refl :: Prelude.max (Prelude.sum '[1, 2]) (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) :~: 3)
    observe "supply either constructor to a projection checked at an unknown input"
        (Refl :: Pick ('W.LeftFields 'True 'x') :~: 'x', Refl :: Pick ('W.RightFields 'y' 'False) :~: 'y')
    observe "infer a conversion from the selected field of a computed pair"
        (Refl :: (Prelude.fst (Build.buildPair (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 'x') :: TypeNats.Nat) :~: 3)
    observe "infer the element type of a nested empty list from its sibling"
        (Refl :: Prelude.max (Prelude.reverse '[]) (Prelude.reverse '[ 'Just 'x']) :~: '[ 'Just 'x'])
    observe "supply the inner constructor to a deferred nested match"
        (Refl :: PickNested ('W.Nested ('Just 'x')) 'y' :~: 'x', Refl :: PickNested ('W.Nested 'Nothing) 'y' :~: 'y')
    observe "use a local instance whose method checks a dependent input type"
        (LocalInstance.passValue 'x', LocalInstance.passValue True)
    observe "check an associated type equation before a dependent producer"
        LocalAssociated.result
    observe "build an optional head when a client supplies the spine constructor"
        (Refl :: FirstMaybe ('Spine.Empty :: Spine.Spine Bool) :~: 'Nothing,
         Refl :: FirstMaybe ('Spine.More 'x' 'Spine.Empty) :~: 'Just 'x')
    observe "test the selected field of a pair whose other field throws"
        (Refl :: Prelude.odd (Prelude.fst (Build.partialPair 7)) :~: 'True)
    observe "run associated and standalone data instances from a dependent module"
        LocalDataFamily.result
    observe "reduce an optional head through a private helper"
        (Refl :: ViaHelper ('Spine.More 'x' 'Spine.Empty) :~: 'Just 'x')
    observe "use a local identity in a family equation needed by a later producer"
        IndependentFamily.result
    -- NativeClosedPartialPair checks the same proof with only the two interfaces.
    observe "prove a direct projection of a pair with an exceptional field"
        (Refl :: Prelude.fst (Build.partialPair 7) :~: 7)
    observe "prove each computed field without evaluating its exceptional sibling"
        ( Refl :: ComputedProjection.pick (Computed.bump 7) :~: 8
        , Refl :: ComputedProjection.pickRight (Computed.bumpRight 7) :~: 8 )
    observe "build a public constructor through three private helpers"
        (Refl :: MarkedHead ('Spine.More 'x' 'Spine.Empty) :~: 'Just ('Spine.Marked 'x'))
    observe "infer a local function used in an imported family equation"
        (ImportedFamily.importedFamilyResults :: (Bool, Ordering, Char, Maybe Bool, Ordering))
    observe "check a local datatype before an imported family equation"
        (case MixedFamily.mixedResult of MixedFamily.Stored flag -> flag)
    observe "use a closed family in a helper and in the called function's signature"
        (FamilySignature.observed, FamilySignature.directObserved)
    observe "select the second element through a private case"
        (Refl :: Spine.secondOrFirst ('Spine.More 'x' ('Spine.More 'y' 'Spine.Empty)) :~: 'Just 'y')
    observe "rebuild one spine cell while its tail remains unknown"
        (erasedCell (Proxy :: Proxy ('Spine.Empty :: Spine.Spine Bool)))
    observe "carry the same tag through a recursive spine"
        (filledCell (Proxy :: Proxy ('Spine.Empty :: Spine.Spine Bool)) (Proxy :: Proxy ('Just 'True)))
    observe "apply Prelude.map to Prelude.not at a known list"
        (Refl :: Prelude.map Prelude.not '[ 'True, 'False] :~: '[ 'False, 'True])
    observe "pass an unapplied type family to another family"
        (Refl :: MapFlags FlipFlag '[ 'True, 'False] :~: '[ 'False, 'True])
    observe "pass a type family with an unapplied invisible binder"
        (Refl :: InvisibleFamily.Observed :~: 'True)
    observe "interpret a kind code as an ordinary type constructor" InterpretedKind.optionalFlag
    observe "apply a type lambda to Maybe, Either String and IO" TypeLambda.roadmap
    observe "partially apply a two-argument type lambda" TypeLambda.partial
    observe "map a type lambda with a constructor case" TypeLambda.mixed
    observe "read fields from two indexed type case branches" TypeLambda.indexedFields
    observe "retain the kind of a matched Nothing"
        (Refl :: TypeLambda.Rebuild ('Nothing :: Maybe Ordering) :~: Proxy ('Nothing :: Maybe Ordering))
    observe "name and reuse a type-level intermediate result" TypeLet.roadmap
    observe "bind the result of a constructor case"
        (Right True :: TypeLet.CaseResult 'True, Left 'c' :: TypeLet.CaseResult 'False)
    observe "select the result type by a constructor" NativeCase.roadmap
    observe "match Bool and Ordering directly in a type"
        (7 :: NativeCase.ByType Bool, 'x' :: NativeCase.ByType Ordering)
    observe "use a local type in a constructor case"
        (Just True :: NativeCase.Captured 'False 'False)
    observe "infer one kind for a local type from both case branches"
        (True :: NativeCase.ContextKind 'True)
    observe "pass a constructor to uncurry with two unknown fields"
        (OpenBuildSource.uncurried (Proxy @Int) (Proxy @Char))
    observe "prove one map step for an arbitrary callback and tail"
        (OpenMapProof.eachCons (Proxy @FlipFlag) (Proxy @'True) (Proxy @'[ 'False]))
    observe "map Boolean negation and traffic signals over an unknown tail"
        (OpenMapProof.negateHead (Proxy @'[]), OpenMapProof.redStops (Proxy @'[]))
    observe "map an ordinary enumeration callback over an unknown tail"
        (OpenMapProof.ordinaryRedStops (Proxy @'[]))
    observe "use Prelude.not on a known head with an unknown tail"
        (InstalledNotProof.negateHead (Proxy @'[]))
    putStrLn ("use Prelude.map and Prelude.not on an empty list and an unknown tail: "
              ++ PreludeMapProof.showPreludeMapProofs)
    observe "collect row heads with an ordinary polymorphic callback"
        (OpenMapProof.firstRowHead (Proxy @'True) (Proxy @'Spine.Empty) (Proxy @'[]))

-- | Complement one flag at the type level.
type family FlipFlag (b :: Bool) :: Bool where
    FlipFlag 'False = 'True
    FlipFlag 'True = 'False

-- | Apply an unmatchable Boolean function to each list element.
type MapFlags :: (Bool %U -> Bool) -> [Bool] -> [Bool]
type family MapFlags f xs where
    MapFlags f '[] = '[]
    MapFlags f (x ': xs) = f x ': MapFlags f xs

-- | Prove the first output cell without supplying the input tail.
erasedCell :: forall (xs :: Spine.Spine Bool). Proxy xs
           -> Spine.erase ('Spine.More 'False xs) :~: 'Spine.More 'True (Spine.erase xs)
erasedCell _ = Refl

-- | Keep the original tag and tail in the recursive result.
filledCell :: forall (xs :: Spine.Spine Bool) (tag :: Maybe Bool). Proxy xs -> Proxy tag
           -> Spine.fill ('Spine.More 'False xs) tag :~: 'Spine.More tag (Spine.fill xs tag)
filledCell _ _ = Refl

-- | Keep the helper equation in the ordinary worker's case.
type ViaHelper :: Spine.Spine a -> Maybe a
type ViaHelper xs = Spine.firstViaHelper xs

-- | Preserve the helper that wraps a field in the worker's public constructor.
type MarkedHead :: Spine.Spine a -> Maybe (Spine.Marked a)
type MarkedHead xs = Spine.firstViaMark xs

-- | Keep the ordinary case until the client supplies the outer constructor.
type FirstMaybe :: Spine.Spine a -> Maybe a
type FirstMaybe xs = Spine.firstMaybe xs

-- | Keep the ordinary projection in the type until a client supplies a constructor.
type Pick :: W.Choice Bool Char -> Char
type Pick p = W.pick p

-- | Retain the nested match and its fallback until a constructor selects a branch.
type PickNested :: W.Nested k -> k -> k
type PickNested p fallback = W.pickNested p fallback

-- | Read the two natural-number indices inferred from a computed pair.
reflectPair :: forall q r. (TypeNats.KnownNat q, TypeNats.KnownNat r)
            => Proxy '(q, r) -> (Natural, Natural)
reflectPair _ = (TypeNats.natVal (Proxy @q), TypeNats.natVal (Proxy @r))

-- | The producer exports only an ordinary Haskell function.
closedLength :: Proxy (Length.length [1, 2, 3])
closedLength = Proxy

-- | Use Prelude's Int result and request a Nat from its ordinary conversion.
installedLength :: Proxy (Prelude.fromIntegral (Prelude.length '[1, 2, 3]) :: TypeNats.Nat)
installedLength = Proxy

-- | The ordinary polymorphic identity returns the caller's type variable.
openIdentity :: forall a. Num a => id a
openIdentity = 42

-- | Reflect an upper-case character and prove that seven is a digit.
checkedCharacters :: (Char, Data.Char.isDigit '7' :~: 'True)
checkedCharacters = (charVal (Proxy @(Data.Char.toUpper 'x')), Refl)

-- | The result type selects the first operand even when the argument types differ.
openLeft :: forall a b. a -> b -> const a b
openLeft x _ = x

-- | The argument pattern fixes the wildcard before GHC checks the result.
titleFromPattern :: Proxy _c -> Proxy (Data.Char.toTitle _c)
titleFromPattern (_ :: Proxy 'a') = Proxy @'A'

-- | Use the same imported function at runtime and in a result type.
closedSourceFunctions :: IO ()
closedSourceFunctions = do
    observe "flipBit at runtime" (map Table.flipBit [False, True])
    observe "payload selected by flipBit" (chooseFlipped False, chooseFlipped True)
    case signalProof of
        Refl -> observe "signal at Green" (Table.signal Table.Green)
  where
    chooseFlipped :: foreach (b :: Bool) -> Select (Table.flipBit b)
    chooseFlipped False = 42
    chooseFlipped True = False

    signalProof :: Table.signal 'Table.Green :~: 'Table.Go
    signalProof = Refl

-- | The term function that computes a predecessor also indexes the shorter vector.
constructorFields :: IO ()
constructorFields = do
    let xs = VCons (7 :: Int) (VCons 9 VNil)
        shorter :: Vec Int (previous 2)
        shorter = dropFirst xs
    observe "dropFirst with a computed result length" (toList shorter)
    observe "empty vector stays empty" (toList (dropFirst (VNil :: Vec Int Z)))
    observe "successor test ignores the field" (isSuccessor (S (error "unused predecessor")))

-- | Use a recursive source function as the result index for a runtime list.
recursiveLengths :: IO ()
recursiveLengths = do
    values <- newIORef [True, False, True] >>= readIORef
    let vector :: Vec Bool (listCount values)
        vector = boolVector values
    observe "vector indexed by a recursive source length" (toList vector)
    observe "list length ignores bottom elements" (natToInt (listCount [undefined, undefined]))
    observe "first length constructor ignores the tail" (isSuccessor (listCount (False : undefined)))
    ints <- newIORef ([11, 22] :: [Int]) >>= readIORef
    let numbers :: Vec Int (sourceLength @Int ints)
        numbers = sourceVector ints
    observe "Int vector indexed by the same polymorphic length" (toList numbers)
    observe "source length of a list of functions" (natToInt (sourceLength [not, id]))
    empty <- newIORef ([] :: [Int]) >>= readIORef
    observe "heads of empty and nonempty runtime lists" (first (sourceVector empty), first numbers)
    observe "safeHead of a known list cell" (firstCell False values)
    n <- newIORef (bottomValue @Nat "unused tail length") >>= readIORef
    observe "safeHead at length 1 + n" (safeHead (replicateNative (1 + n) True))
  where
    firstCell :: a -> [a] -> a
    firstCell x xs = safeHead (sourceVector (x : xs))

    first :: Vec a n -> Maybe a
    first VNil = Nothing
    first v@(VCons _ _) = Just (safeHead v)

-- | Read the first cell of a vector whose length reduces to a successor.
safeHead :: Vec a (1 + n) -> a
safeHead (VCons x _) = x

-- | Name a shared product inside a vector whose result type uses that product.
namedPrefix :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Char (1 + rows * cols)
namedPrefix rows cols =
    let cells = rows * cols
    in replicateNative (1 + cells) 'x'

-- | Give both stages of a runtime length calculation a type signature.
signedPrefix :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Vec Char (1 + rows * cols)
signedPrefix rows cols =
    let total, cells :: Nat
        total = 1 + cells
        cells = rows * cols
    in replicateNative total 'x'

-- | Take the head without specifying the tail length of the local vector.
localHead :: foreach (rows :: Nat) -> foreach (cols :: Nat) -> Char
localHead rows cols = safeHead (replicateNative total 'x')
  where
    total, cells :: Nat
    total = 1 + cells
    cells = rows * cols

-- | Accept a length computed by the caller if a runtime test finds one cell.
-- Match S Z because the caller supplies the meaning of a term literal here.
oneCellHead :: Num Nat => foreach (extent :: Nat) -> Maybe Char
oneCellHead extent =
    let count :: Nat
        count = 1 + extent
    in case sameNat count (S Z) of
         Just Refl -> Just (safeHead (replicateNative count 'x'))
         Nothing -> Nothing

-- | Use ordinary addition on two unknown lengths in a vector result type.
sourceAddition :: IO ()
sourceAddition = do
    n <- newIORef (1 :: Nat) >>= readIORef
    m <- newIORef (2 :: Nat) >>= readIORef
    let xs :: Vec Bool n
        xs = replicateNative n True
        ys :: Vec Bool m
        ys = replicateNative m False
        joined :: Vec Bool (n + m)
        joined = appendHidden xs ys
    observe "runtime sum and appended vector" (natToInt (n + m), toList joined)
    observe "outer successor ignores the second addend" (isSuccessor (1 + undefined))

    let filled :: Vec Bool (n + m)
        filled = replicateNative (n + m) True
    observe "replicate at a computed runtime length" (toList filled)

    bottom <- newIORef (bottomValue @Nat "showcase-sum-tail") >>= readIORef
    observe "grow at a locally named runtime index"
        (toList (grow m 'x'), safeHead (grow bottom 'x'))
    let tag :: Proxy (bottom + m)
        tag = tagLength (bottom + m)
        partial :: Vec Bool (n + bottom)
        partial = replicateNative @Bool (n + bottom) True
    case tag of Proxy -> observe "ignore a computed runtime length" True
    case partial of
        VCons x _ -> observe "first cell at a partial runtime length" x
        VNil -> fail "Expected one cell before the unknown tail"
  where
    grow :: foreach (n :: Nat) -> a -> (let k = 1 + n in Vec a k)
    grow n x = replicateNative (1 + n) x

    tagLength :: foreach (k :: Nat) -> Proxy k
    tagLength _k = Proxy

-- | Use decimal numerals in the original vector GADT and add runtime indices.
arithmeticNotation :: IO ()
arithmeticNotation = do
    let three :: Vec Bool 3
        three = sourceVector @Bool [True, False, True]
    n <- newIORef (2 :: Nat) >>= readIORef
    let prefix :: Vec Bool n
        prefix = replicateNative n False
        joined :: Vec Bool (n + 3)
        joined = appendVec prefix three
    observe "vector with length 3" (toList three)
    observe "replicateAt 3" (toList (replicateAt 3 True))
    let filled :: Vec Bool 3
        filled = replicateNative 3 True
        oneMore :: Vec Bool (n + 1)
        oneMore = replicateNative (n + 1) False
    observe "replicateNative 3" (toList filled)
    observe "replicateNative (n + 1)" (toList oneMore)
    observe "vector with length n + 3" (toList joined)
    observe "2 + 3 at runtime" ((2 + 3) :: Nat)
    rows <- newIORef (2 :: Nat) >>= readIORef
    cols <- newIORef (3 :: Nat) >>= readIORef
    let row :: Vec Char cols
        row = replicateNative cols 'x'
        rectangle = replicateNative rows row
        flattened :: Vec Char (rows * cols)
        flattened = flattenRows rectangle
        filled :: Vec Char (rows * cols)
        filled = replicateNative (rows * cols) 'x'
    observe "flatten a runtime 2-by-3 rectangle" (toList flattened)
    observe "replicate at the product of two runtime lengths" (toList filled)
    unknownCols <- newIORef (bottomValue @Nat "unused column count") >>= readIORef
    let noCells :: Vec Char 0
        noCells = replicateNative (0 * unknownCols) 'x'
    observe "zero rows leave the column count unevaluated" (toList noCells)
    let withPrefix :: Vec Char (1 + rows * cols)
        withPrefix = replicateNative (1 + rows * cols) 'x'
    observe "one cell plus a runtime rectangle" (toList withPrefix)
    observe "safeHead before an unknown product"
        (safeHead (replicateNative (1 + rows * unknownCols) 'x'))
    observe "one plus a let-bound product" (length (toList (namedPrefix rows cols)))
    observe "safeHead with a let-bound product" (safeHead (namedPrefix rows unknownCols))
    observe "signed length bindings" (length (toList (signedPrefix rows cols)),
                                      safeHead (signedPrefix rows unknownCols))
    observe "safeHead inferred from a where-bound length" (localHead rows unknownCols)
    let threeLocal :: Vec Char 3
        threeLocal = let count :: Nat; count = 3 in replicateNative count 'x'
    observe "vector at a let-bound numeral" (toList threeLocal)
    observe "head after testing the actual length" (oneCellHead Z, oneCellHead (S Z))

-- | Prefix a checked row without changing its runtime tail index.
prefixRowExact :: forall a. foreach (n :: Nat) -> a -> [a] -> Maybe (Vec a (1 + n))
prefixRowExact n x xs = VCons x <$> fromListExact n xs

-- | Check a size from IO or let the input list determine its own size.
listBoundaries :: IO ()
listBoundaries = do
    n <- newIORef (2 :: Nat) >>= readIORef
    xs <- newIORef "ab" >>= readIORef
    observe "ordinary zip at a checked runtime length" (Ordinary.ordinaryZipExact n xs [1, 2 :: Int])
    observe "ordinary zip with a short input" (Ordinary.ordinaryZipExact n xs [1 :: Int])
    observe "safeHead after checking for exactly one cell" (first "x", first "", first "xy")
    observe "first cell of a packaged list" (headPacked (packList ('p' : undefined)))
    let p = packList xs
    observe "project the vector at the length stored in its own package"
        (toList (PackedProjection.payload p))
    observe "project the length of a known dependent package in a type"
        (Refl :: PackedProjection.Length PackedProjection.One :~: 'S 'Z)
    let vector = Retain n (replicateNative n 'x') :: Retained (Vec Char)
    let seven = constantPackage (error "unused projected witness") (7 :: Int)
    observe "project from two payload families with the same dependent function"
        (toList (RetainedProjection.project vector), getConst (RetainedProjection.project seven))
    observe "use the stored length and vector in a generic dependent eliminator"
        (withRetained vector (\k v -> (natToInt k, toList v)))
    let chars = Pair.Pair n (replicateNative n 'x') :: Pair.DPair Nat (Vec Char)
        action = error "unused IO witness" :: IO Int
        tag = Pair.Pair action (Const 'i') :: Pair.DPair (IO Int) (Const Char)
    observe "use one dependent pair type at two witness types"
        (Pair.withPair chars (\k v -> (natToInt k, toList v)),
         Pair.withPair tag (\_ c -> getConst c))
    observe "read either package field without demanding its sibling"
        ( withRetained (constantPackage (error "unused witness") (7 :: Int))
              (\_ box -> getConst box)
        , withRetained (constantPackage n (error "unused payload" :: Int))
              (\k _ -> natToInt k) )
    observe "checked row prefix with a runtime length"
        (toList <$> prefixRowExact n 'x' xs, toList <$> prefixRowExact n 'x' "a")
  where
    first :: [a] -> Maybe a
    first xs = case fromListExact 1 xs of
        Just one -> Just (safeHead one)
        Nothing -> Nothing
