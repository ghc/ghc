{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Runnable examples of the completed Dependent Haskell source features.
module Main (main) where

import qualified BetaCallClient as Beta
import qualified ConstructorCallClient as Constructor
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import qualified FunctionPackageLibrary as FunctionPackages
import qualified FunctionSignatureLibrary as FunctionSignatures
import qualified GroundApplicationClient as Ground
import qualified IndexBetaClient as IndexBeta
import qualified KnownArgumentClient as Known
import qualified NativeMultiPackage as Packages
import qualified OpaqueCallClient as Opaque
import qualified ParameterizedIndexLibrary as Parameterized
import PackedVecLibrary
import qualified QualifiedPiLibrary as Qualified

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
    -outputdir _build/showcase docs/dependent-haskell/Showcase.hs \
    -o _build/showcase/showcase
  _build/showcase/showcase

General optimization, arbitrary dependent evaluation and bare function values
as indices remain outside the completed source subset. See README.md and CORE.md.
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
    keep b payload = 42

    -- Obtain the matching Show dictionary after refinement.
    renderChoice :: foreach (b :: Bool) -> Select b -> String
    renderChoice b value = case b of
        True -> show value
        False -> show value

-- | Show runtime indices, packages, algorithms, proofs and ordinary inference.
vectors :: IO ()
vectors = do
    indexCell <- newIORef (S (S (S Z)))
    n <- readIORef indexCell
    otherCell <- newIORef (S (S (S Z)))
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
    let sample = PackVec (S (S (S Z))) (VCons (1 :: Int) (VCons 2 (VCons 3 VNil)))
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
    cell <- newIORef (S (S Z))
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
    keepCall f n payload = 42

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
    cell <- newIORef (S (S (S Z)))
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

-- | Open either constructor without sharing their retained field binders.
alternatives :: IO ()
alternatives = do
    cell <- newIORef (S (S Z))
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
keepWithLaterEvidence :: foreach (n :: Nat) -> foreach (m :: Nat) -> (n ~ m) => foreach (k :: Nat) -> Vec Int m -> Vec Int k -> (Vec Int n, Vec Int k)
keepWithLaterEvidence Z m k xs ys = (xs, ys)
keepWithLaterEvidence (S predecessor) middle final xs ys = (xs, ys)

-- | Read distinct indices and observe their separate argument positions.
laterArguments :: IO ()
laterArguments = do
    firstCell <- newIORef Z
    middleCell <- newIORef (S (S Z))
    lastCell <- newIORef (S (S (S Z)))
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
    natCell <- newIORef (S (S Z))
    n <- readIORef natCell
    observe "optional retained Nat" (Parameterized.optionalRoundtrip n)
    treeCell <- newIORef (Parameterized.Node (Parameterized.Leaf True) (Parameterized.Leaf False))
    tree <- readIORef treeCell
    observe "runtime tree selects nested payload" (Parameterized.renderTree tree (Parameterized.treePayload tree))
    _ <- evaluate (Parameterized.packListVector (error "unused retained list"))
    observe "list package does not force its index" ()
    observe "opaque call ignores retained list"
        (Parameterized.observeListCall (\ignored -> S (S Z)) (error "unused opaque call list"))

-- | Run each completed source feature group with finite runtime observations.
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
