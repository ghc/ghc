{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
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
module Main (main) where

import qualified BetaCallClient as Beta
import qualified ConstructorCallClient as Constructor
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Kind (Type)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import qualified FunctionFieldClient
import qualified FunctionFieldLibrary as FunctionFields
import qualified FunctionPackageLibrary as FunctionPackages
import qualified FunctionSignatureLibrary as FunctionSignatures
import qualified GroundApplicationClient as Ground
import GHC.Exts (FUN, M, U, Matchability (..), Multiplicity (Many))
import qualified IndexBetaClient as IndexBeta
import qualified KnownArgumentClient as Known
import qualified Language.Haskell.TH as TH
import qualified NativeMultiPackage as Packages
import qualified OpaqueCallClient as Opaque
import qualified OrdinaryVecClient as Ordinary
import qualified ParameterizedIndexLibrary as Parameterized
import PackedVecLibrary
import qualified QualifiedPiLibrary as Qualified
import qualified Type.Reflection as Reflection

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

-- | Convert nominally equal scalar domains and preserve function demand.
domainConversion :: IO ()
domainConversion = do
    lengthCell <- newIORef (S (S Z))
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
    lengthCell <- newIORef (S (S Z))
    n <- readIORef lengthCell
    identityCell <- newIORef (id :: Nat -> Nat)
    selectedIdentity <- readIORef identityCell
    successorCell <- newIORef (S :: Nat -> Nat)
    selectedSuccessor <- readIORef successorCell
    constantCell <- newIORef (const (S (S Z)) :: Nat -> Nat)
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
    lengthCell <- newIORef (S (S Z))
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
    sizeRef <- newIORef (S (S Z))
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
    sizeRef <- newIORef (S (S Z))
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
    sizeRef <- newIORef (S (S Z))
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
    n <- newIORef (S (S Z)) >>= readIORef
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
