{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE Modifiers #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Runnable examples of the completed Dependent Haskell source features.
-- In GHCi, load PackedVecLibrary with the options in the adjacent README.
-- Enter these as separate commands to keep a retained function at the prompt.
--
-- > let fill :: foreach (n :: Nat) -> Vec Bool (1 + n); fill n = replicateNative (1 + n) True
-- > toList (fill 2)
module Main (main) where

import qualified BetaCallClient as Beta
import qualified CallbackRulesClient
import qualified CharacterDecision
import qualified RetryCharacters
import qualified CharacterExport
import qualified CharacterCase
import qualified ClosedLengthProducer as Length
import qualified ClosedTableLibrary as Table
import qualified CompareAt
import qualified CompareOptional
import qualified ComputedPair as Computed
import qualified ConstructorCallClient as Constructor
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Either
import DefaultVector (DefaultElement(..))
import qualified ErasedPackage
import FamilyTuple (Tuple(Tup2))
import qualified FamilyRefinementClient
import qualified LabelledValueClient
import qualified QuotedColumn
import qualified RetainedFamilyTuple
import qualified ForgetWitness
import qualified ImplicitVector
import qualified PartitionChoices
import qualified PrimitiveWordFieldsClient
import qualified StandaloneCallbackClient
import qualified ErasedStream
import qualified StreamFold
import qualified StreamPreview
import qualified PackageRules
import OrphanPackageRules ()
import qualified LexicalPackageRules
import qualified RankedPackage
import qualified RankedPackageClient
import qualified RetainedPackageFormat
import qualified RetainedPackageFormatClient
import qualified StoredVector
import qualified ExistentialVec as E
import qualified WitnessedBatch as Batch
import qualified RectangularTable as Table
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import Data.List (isInfixOf)
import qualified Data.List
import qualified Data.Maybe
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:) (Refl), (:~~:) (HRefl))
import qualified FiniteTree
import qualified FunctionFieldClient
import qualified FunctionFieldLibrary as FunctionFields
import qualified RetainedRulesClient
import qualified VisibleRuleClient
import qualified RetainedRules as Rule
import qualified ImportedRetainedRules
import qualified FunctionPackageLibrary as FunctionPackages
import qualified FunctionSignatureLibrary as FunctionSignatures
import qualified GroundApplicationClient as Ground
import GHC.Exts (FUN, Int (I#), Multiplicity (Many, One))
import GHC.Matchability (M, U, Matchability(..))
import qualified GHC.TypeNats as TypeNats
import GHC.TypeLits (charVal)
import qualified IndexBetaClient as IndexBeta
import qualified RecursiveJobs
import qualified IndexedMark
import qualified IndexedVec
import qualified CertifiedSplitClient
import qualified CursorClient
import qualified AssociatedPayloadSumClient
import qualified ComposedPayloadSumClient
import qualified RuntimeMessageClient
import qualified RuntimeTypeColumn as Column
import qualified FinitePositionClient
import qualified OrdinaryOpenVectorAppend
import qualified VectorSplitObservation
import qualified KnownArgumentClient as Known
import qualified Language.Haskell.TH as TH
import qualified LocalOpen
import qualified LocalAliasField
import qualified LocalInstance
import qualified LocalAssociated
import qualified LocalDerivedAssociated
import qualified IndependentFamily
import qualified FamilySignature
import qualified ImportedFamily
import qualified MixedFamily
import qualified InterpretedKind
import qualified InterpretedKindUseSite
import qualified NestedFunctionClient
import qualified InvisibleFamily
import qualified LiberalCallback
import ConstrainedElementsClient (advanceCursor, showAdvance, showRemainder, splitCollection, Buffered(..), firstBuffered, partialElement, aliasedReflection, annotatedElement, constrainedElements, explicitElements, formatField, renderField, renderKnownFields, retainedMaybeDomain)
import ConstrainedElementsQuickLook (showReflection)
import ConstrainedFormatClient (renderedRawView)
import OpenArrowKindClient (openKindRows)
import StoredReflectionClient (readStoredReflection)
import IndexedMapClient (mappedCells)
import qualified MatchableView as View
import LaterMatchabilityClient (annotatedMap, appliedCallback, appliedSynonym, barePromotedEndo, boxedCallbacks, computedDictionary, flippedCallbacks, forwardedMap,
                                inferredEndo, laterMatchability, nestedCallback, rankedMap)
import qualified SchemaColumns
import qualified TypeLambda
import qualified TypeLambdaArgument
import qualified TypeLambdaClient
import qualified TypeLambdaQuote
import qualified QuotedSensorRow
import qualified TypeLet
import qualified FamilyCasePayload as FamilyCase
import qualified NativeCase
import qualified NativeNestedCase
import qualified NativeRetainedTH
import qualified NativeRetainedConTH
import qualified LocalDataFamily
import qualified LazyParcel
import qualified LegacyKindArgument
import qualified OpenBuildSource
import qualified OpenMapProof
import qualified OpenMapWorker
import qualified InstalledNotProof
import qualified OrderingPayload
import qualified ConcreteGuardedListProof
import qualified PreludeDropWhileRuntime
import qualified PreludeFilterAppendProof
import qualified PreludeFilterCutoff
import qualified PreludeFilterDecider
import qualified PreludeAppendLengthProof
import qualified ListFoldrProof
import qualified MappingProof
import qualified PreludeMappingProof
import qualified OrdinaryEitherMapping
import qualified OrdinaryTheseMapping
import qualified Row
import qualified RowProof
import qualified OrdinaryFamilyBranches
import qualified PreludeFoldrProof
import qualified PreludeZipProof
import qualified HaskLib
import qualified Client as HaskLibClient
import qualified PreludeZipWithLengthProof
import qualified PreludeZipWithProof
import qualified PreludeFilterLengthProof
import qualified PreludeFilterProof
import qualified IndexedPrefix
import qualified IndexedPrefixClient
import qualified DirectIndexedListClient
import qualified DirectIndexedVectorClient
import qualified IndexedMatrixClient
import qualified PreludeFilterRuntime
import qualified PreludeMapAppendProof
import qualified PreludeMapCompositionProof
import qualified PreludeSpanRuntime
import qualified PreludeTakeWhile
import qualified PreludeMapLengthProof
import qualified PreludeMapLengthSize as Size
import qualified PreludeMapProof
import qualified ConsRuleMapProof
import qualified OpenBuildWorker as Build
import qualified SpineWorker as Spine
import qualified OpenConstructorSource
import qualified OpenConstructorWorker as W
import qualified NativeMultiPackage as Packages
import Numeric.Natural (Natural)
import qualified OpenSource
import qualified OrdinaryScopedComparison
import qualified OrdinaryScopedChoice
import qualified ScopedClassMethod as Choice
import qualified ScopedClassMethodClient
import qualified StoredDictionaryClient
import qualified Signal
import qualified PreludePairExport
import qualified OpaqueCallClient as Opaque
import qualified PairProjection as ComputedProjection
import qualified OrdinaryVecClient as Ordinary
import qualified ConservedPartition
import qualified GHC.Sigma as Sigma
import qualified ListVectorClient
import qualified NamedSplitClient
import qualified ParameterizedIndexLibrary as Parameterized
import qualified PackedProjection
import qualified PackageEquality
import AppendLengthClient (appendTwoAndOne)
import PackedVecLibrary
import OrdinaryZipRows (headerCountReport, zipReport)
import qualified AppendByMethodClient
import qualified RetainedSumOperator
import RetainedNat (Retained(..), constantPackage, withRetained)
import qualified DependentPair as Pair
import qualified DependentProjection as Pair
import qualified RetainedProjection
import qualified RetainedCharClient
import qualified SiblingColumnRow
import qualified SiblingColumns
import qualified SiblingColumnsClient
import qualified NamedSiblingColumns as Named
import qualified NamedSiblingColumnsClient
import qualified QualifiedPiLibrary as Qualified
import qualified QuantifiedGadtIndex as QuantifiedIndex
import qualified ReadNatural
import qualified Type.Reflection as Reflection
import qualified UnicodeExport
import qualified RetainedCharacterClient
import qualified RetainedNaturalClient

{- Note [Maintaining the Dependent Haskell showcase]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Add a small, labeled example when a source feature is completed. Keep the
examples finite. Reuse the checked example libraries and show their types here
when the signature explains the feature. Compiler-only invariants remain in
CORE.md and the existing tests. DeferredHeterogeneous checks the kinds of
coercion binders for deferred type errors. A passing example does not prove totality.
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
    -idocs/dependent-haskell/examples/erased-existentials \
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

-- | Return the measurement and certify that reversal preserves it.
measureReverseLength :: forall a. foreach (measure :: [a] -> HaskLib.Size)
                     -> foreach (xs :: [a])
                     -> measure xs :~: HaskLib.size xs
                     -> (HaskLib.Size, HaskLib.size (reverse xs) :~: measure xs)
measureReverseLength measure xs agrees =
    case agrees of Refl -> (measure xs, HaskLib.lengthReverse xs)

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
    observe "apply retained function rules without forcing an unused index"
        (RetainedRulesClient.fromInput b, RetainedRulesClient.atFalse 11, RetainedRulesClient.ignoredIndex)
    observe "infer both rule binders from readKept" (RetainedRulesClient.boundPayload b)
    readingIndex <- newIORef (Just b) >>= readIORef
    lazyIndex <- newIORef (error "forced reading index" :: Maybe Bool) >>= readIORef
    observe "use a type-applied rule with a runtime Maybe index"
        (VisibleRuleClient.readAt readingIndex 37, VisibleRuleClient.readAt lazyIndex 19)
    ignoredFirst <- newIORef (error "forced first rule argument") >>= readIORef
    observe "match a type argument after a lazy value argument"
        (VisibleRuleClient.readAfter ignoredFirst readingIndex 43)
    observe "select an erased type between two lazy runtime indices"
        (VisibleRuleClient.readTagged ignoredFirst lazyIndex 47)
    observe "pass a required type between lazy runtime indices"
        (VisibleRuleClient.readRequired ignoredFirst lazyIndex 53)
    lazyCharIndex <- newIORef (error "forced second reading index" :: Maybe Char) >>= readIORef
    observe "infer a rule binder between two required types"
        (VisibleRuleClient.readTwoRequired ignoredFirst lazyIndex lazyCharIndex 61)
    observe "apply a rule declared outside the retained function's module"
        (Rule.readChoice (Rule.keep True (ImportedRetainedRules.tagged 23)))
    observe "choose True has result Int" (choose True)
    observe "choose False has result Bool" (choose False)
    let bottomB :: Bool
        bottomB = error "showcase-bool"
    let payload :: Select bottomB
        payload = bottomValue @(Select bottomB) "showcase-payload"
    observe "keep ignores bottom index and payload" (keep bottomB payload)
    writeIORef cell False
    other <- readIORef cell
    ignored <- newIORef bottomB >>= readIORef
    observe "inline retained type case"
        ( NativeCase.renderRetainedPick b
        , NativeCase.renderRetainedPick other
        , NativeCase.retainedKeep ignored (error "showcase-inline-payload")
        )
    observe "compose retained type cases from separate modules"
        (renderIndependent b, renderIndependent other)
    maybeCell <- newIORef Nothing
    absent <- renderIndependentMaybe <$> readIORef maybeCell
    writeIORef maybeCell (Just bottomB)
    present <- renderIndependentMaybe <$> readIORef maybeCell
    observe "compose retained type cases with an ignored field" (absent, present)
    writeIORef maybeCell Nothing
    fieldAbsent <- renderIndependentField <$> readIORef maybeCell
    writeIORef maybeCell (Just bottomB)
    fieldPresent <- renderIndependentField <$> readIORef maybeCell
    observe "compose retained type cases with a field-dependent result" (fieldAbsent, fieldPresent)
    nested <- mapM (\m -> writeIORef maybeCell m >> readIORef maybeCell)
                   [Nothing, Just True, Just False]
    observe "compose nested type cases from separate modules" (map renderNested nested)
  where
    renderIndependent :: Bool -> String
    renderIndependent flag = NativeRetainedTH.renderCase flag (NativeCase.retainedPick flag)

    renderIndependentMaybe :: Maybe Bool -> String
    renderIndependentMaybe m =
        NativeRetainedTH.renderMaybeCase m (NativeCase.retainedMaybePick m)

    renderNested :: Maybe Bool -> String
    renderNested m = NativeRetainedTH.renderNestedMaybe m (NativeCase.retainedNestedMaybe m)

    renderIndependentField :: Maybe Bool -> String
    renderIndependentField m =
        NativeRetainedTH.renderMaybeField m (NativeCase.retainedMaybeField m)

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
    modeCell <- newIORef True
    mode <- readIORef modeCell
    observe "package a runtime mode with its associated payload" (RuntimeMessageClient.observeMessage mode)
    observe "read runtime tags with undefined associated payloads" (AssociatedPayloadSumClient.observePayload mode)
    missingTag <- readIORef =<< newIORef (error "forced tag of Nothing")
    observe "read a composed sum payload with an undefined tag" (ComposedPayloadSumClient.observePayloads missingTag)
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
    observe "append two and one at their written sum" appendTwoAndOne
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
    observe "inline an ordinary function that calls a retained helper" (IndexBeta.inlineIncrement 41)
    observe "sum separately typed job costs through a recursive package"
        (RecursiveJobs.totalJobs length ["ghc", "core"] :: Integer,
         RecursiveJobs.totalJobs fromEnum ['A','B'] :: Double,
         RecursiveJobs.totalJobs (const 3) [error "unused payload"] :: Int)
    observe "replicate a captured element at two runtime lengths"
        (IndexBeta.replicateTwice 7 (S Z) (S (S Z)))
    observe "inline a local package helper with captured values"
        (inlinePacked 10 True 7,
         inlinePacked (error "unused offset") False (error "unused payload"))
    observe "apply an exported package helper to a captured function"
        (PackageRules.applyHidden (+ 5) 12,
         PackageRules.applyHidden (const 3) (error "unused package field"))
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

-- | Inline a local package constructor that captures an offset and a flag.
inlinePacked :: Int -> Bool -> Int -> Int
inlinePacked offset keep value =
    let {-# INLINE readValue #-}
        readValue :: Int -> Int
        readValue value = PackageRules.open ((value, render) :: PackageRules.Hidden)
        render stored = if keep then offset + stored else 0
    in readValue value + 1

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
-- Enable -Wpun-bindings to report the shared type and constructor name.
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
    observe "pair a list's computed length with its vector"
        (ListVectorClient.contents (ListVectorClient.fromList values) :: (Int, [Int]))
    observe "read a closed family's retained length without its payload"
        (ListVectorClient.familyResultLength n)

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
    observe "index a list by its ordinary source length" =<< FinitePositionClient.observeListIndices
    observe "empty-list index forces its position" =<< FinitePositionClient.observeEmptyIndex
    observe "read checked list positions through ordinary and linear callers" =<< CursorClient.observeCursors
    observe "read a checked position with a linear callback" =<< CursorClient.observeIndexedCallback
    observe "use one bounded position on equal-length lists" =<< CursorClient.observeAligned
    observe "consume two independently bounded list positions" =<< CursorClient.observeBoth
    observe "zip ordinary lists at their proven common length" =<< CursorClient.observeZip
    observe "zip two list literals at their statically known length" $
        CursorClient.zipTwoPairs 'a' 'b' (10 :: Int) 20
    observe "zip integer literals at a proven common length" CursorClient.zipIntLiterals
    observe "zip with the first list at Int8" CursorClient.zipInt8Literals
    let chars = Column.Column (type Char) "abc"
        unused = Column.Column (type Bool) (error "forced column")
    observe "read a column at a runtime type and offset"
        ( Column.lookupColumn (type Char) chars position
        , Column.lookupColumn (type Char) unused (error "forced offset")
        , Column.lookupColumn (type Bool) unused (-1) )
    observe "infer the column element type from the result"
        ( Column.lookupColumnInferred chars position :: Maybe Char
        , Column.lookupColumnInferred unused (error "forced offset") :: Maybe Char )
    let ignoredType = error "forced skipped type witness" :: Reflection.TypeRep Char
    observe "skip a typed column request without forcing its witness"
        ( Column.lookupColumnWhen True (Column.ReadColumn position) chars :: Maybe Char
        , Column.lookupColumnWhen False @{ignoredType} Column.SkipColumn
            (error "forced skipped column") :: Maybe Char )
    observe "check a signed position in a packaged vector" $
        case ListVectorClient.fromList values of
            Sigma.Pair _ cells -> FinitePositionClient.lookupVecViaView cells position
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

instance DefaultElement Char where
    defaultElement = 'x'

data RankedField (d :: Maybe Bool) = RankedField
  (forall (d :: Maybe Bool). NativeNestedCase.CaseFlag d ->
    (case d of { Just True -> Int; Just _ -> Int; Nothing -> Char }))

constructorFieldCases :: (Int, Char)
constructorFieldCases =
  case (RankedField (\case
          NativeNestedCase.CaseOn -> 7
          NativeNestedCase.CaseOff -> 0
          NativeNestedCase.CaseMissing -> 'x') :: RankedField ('Just 'True)) of
    RankedField select ->
      ( case (NativeNestedCase.CaseField (select NativeNestedCase.CaseOn)
                  :: NativeNestedCase.CaseField ('Just 'True)) of
          NativeNestedCase.CaseField n -> n
      , select NativeNestedCase.CaseMissing )

-- | Select the input type of an arrow with a quantified multiplicity.
type PairCaseArrow (b :: Bool) = forall (p :: Multiplicity).
    (case b of { True -> (Int, Char); False -> Bool }) %p -> (Int, Char)

-- | Select the second field of a promoted data family constructor.
type family SecondFlag (t :: Tuple Bool Bool) :: Bool where
    SecondFlag ('Tup2 left right) = right

-- | Check the second field of a particular pair at compile time.
secondFlag :: SecondFlag ('Tup2 'True 'False) :~: 'False
secondFlag = Refl

-- | Check that the imported call returns a proof for the written pair.
retainedPair :: ('Tup2 'True 'False :~: 'Tup2 'True 'False, (Bool, Bool))
retainedPair = RetainedFamilyTuple.readPair (Tup2 True False)

-- | Store a column value before the required type argument.
data ColumnSample a b where
    ColumnSample :: forall a. (a :~: Int) -> a -> forall b -> ColumnSample a b
    GivenCount :: forall a. (a ~ Int) => a -> forall b -> ColumnSample a b

-- | Matching Refl establishes that the last type pattern is Int.
columnCount :: ColumnSample a a -> Int
columnCount (ColumnSample Refl n (type Int)) = n
columnCount (GivenCount n (type Int)) = n

-- | Keep the dictionary supplied after the field.
data Labelled a where
    Labelled :: Int -> forall a. Eq a => Labelled a

-- | Compare two values with the dictionary that the constructor stored.
compareLabel :: Labelled a -> a -> a -> (Int, Bool)
compareLabel (Labelled n) x y = (n, x == y)

-- | Hide the field type and supply its dictionary after the field.
data EqSample where
    EqSample :: a -> Eq a => EqSample

-- | Compare the hidden value with itself using its stored instance.
equalToSelf :: EqSample -> Bool
equalToSelf (EqSample x) = x == x

-- | Put the equality between two fields of the same indexed type.
data CountField a where
    CountField :: forall a -> a -> CountField a

data InteriorCount a where
    InteriorCount :: forall a. CountField a -> (a ~ Int) => CountField a -> InteriorCount a

-- | The stored equality is available at the second field pattern.
readInteriorCount :: InteriorCount a -> Int
readInteriorCount (InteriorCount _ (CountField (type Int) n)) = n

-- | A type-only key with a required type argument after its Boolean field.
type data PayloadKey a where
    ChoosePayload :: Bool -> forall a -> PayloadKey a

-- | Read a value of the type selected by the key.
keyedPayload :: Proxy (key :: PayloadKey a) -> a -> a
keyedPayload _ value = value

-- | Hide the field type and retain both dictionaries for its consumer.
data PrintablePair where
    PrintablePair :: forall a. Eq a => (Show a => a -> a -> PrintablePair)

-- | Compare the pair and display its second field at the hidden type.
describePair :: PrintablePair -> (Bool, String)
describePair (PrintablePair a b) = (a == b, show b)

$([d|
    data QuotedComparison where
        QuotedComparison :: forall a -> Eq a => forall {b} c. Show c
                         => a -> !(b, c) -> QuotedComparison
    |])

-- | Use each dictionary in the field pattern after its context.
describeQuotedComparison :: QuotedComparison -> (Bool, String)
describeQuotedComparison
    (QuotedComparison (type a) @c ((\x -> x == x) -> equal) ((\(_, x) -> show x) -> shown)) =
    (equal, shown)

-- | A two-element vector used in the table proofs.
type Vec2 x y = 'IndexedVec.VCons x ('IndexedVec.VCons y 'IndexedVec.VNil)

-- | A three-element vector used in the table proofs.
type Vec3 x y z = 'IndexedVec.VCons x (Vec2 y z)

main :: IO ()
main = do
    observe "select the second field of a promoted data family pair" secondFlag
    observe "retain a data family pair and read its fields" retainedPair
    observe "pass a refined data family constructor from an ordinary client"
        FamilyRefinementClient.refinedResult
    observe "use earlier field evidence in a later constructor type pattern"
        (columnCount (ColumnSample @Int Refl 37 (type Int)))
    observe "use a stored equality in a later constructor type pattern"
        (columnCount (GivenCount @Int 41 (type Int)))
    observe "use the dictionary supplied after a constructor field"
        (compareLabel (Labelled 29 @String) "yes" "yes",
         compareLabel (Labelled 31 @Int) 1 3)
    observe "compare a hidden field with the dictionary supplied after it"
        (equalToSelf (EqSample True), equalToSelf (EqSample (0 / 0 :: Double)))
    observe "use an equality between two constructor fields"
        (readInteriorCount (InteriorCount @Int (error "unused first field") (CountField (type Int) 47)))
    observe "choose a type between two constructor fields" LabelledValueClient.labelledValues
    observe "choose the types of two successive fields" LabelledValueClient.labelledPair
    observe "compare a payload using a dictionary supplied between fields" LabelledValueClient.storedChecks
    observe "use a leading context in a field pattern and a later equality in the branch" LabelledValueClient.rankedPairs
    observe "supply a required type between constructor fields" LabelledValueClient.typedNotes
    observe "read the dictionary supplied after a constructor field and its required type" LabelledValueClient.typeTags
    observe "duplicate the unrestricted field of a quoted constructor" (QuotedColumn.readQuoted 17)
    observe "recover consecutive GADT constraints" (describePair (PrintablePair False True))
    observe "choose a type after a field in a type-only constructor"
        (keyedPayload (Proxy @(ChoosePayload 'True Int)) 47)
    observe "use two ordered dictionaries in a quoted constructor"
        (describeQuotedComparison (QuotedComparison (type Int) @Bool 41 ('x', True)))
    observe "form an associated family type from its class evidence" constrainedElements
    observe "form an expression annotation with the matched dictionary" annotatedElement
    observe "pass an associated type as a specified and a required argument" explicitElements
    fieldFlag <- newIORef True
    enabled <- readIORef fieldFlag
    writeIORef fieldFlag False
    disabled <- readIORef fieldFlag
    observe "use two caller dictionaries in imported dependent results"
        (ScopedClassMethodClient.twoDictionaries enabled, ScopedClassMethodClient.twoDictionaries disabled)
    cellDemand <- StoredDictionaryClient.bindingDemand
    observe "read stored dictionaries and check strict and lazy cell binding"
        (StoredDictionaryClient.storedDecisions, cellDemand)
    observe "read a scoped method through a Haskell 2010 client"
        (ScopedClassMethodClient.ordinaryChoices enabled, ScopedClassMethodClient.ordinaryChoices disabled)
    observe "infer a payload constraint beside a scoped method"
        (OrdinaryScopedChoice.readDescription enabled 'q')
    observe "pass a literal length beside a scoped method receiver"
        (OrdinaryScopedChoice.readAtFive enabled)
    observe "project the length of a method-indexed pair with snd and id"
        (OrdinaryScopedChoice.projectFive enabled)
    observe "discard the payload of a locally indexed choice" $
        let localFlag = False
        in case Choice.describeChoice localFlag (error "unused payload" :: Char) of
            (Choice.Tagged flag, _) -> flag
    observe "compare zero under two caller dictionaries"
        (map OrdinaryScopedComparison.observations [-2, 0, 2])
    observe "render a field with evidence for its runtime flag" (renderField enabled, renderField disabled)
    observe "format a field with a polymorphic consumer" (formatField enabled, formatField disabled)
    observe "choose a field domain with a required type argument" renderKnownFields
    retainedMaybeDomain >>= observe "reflect a runtime Maybe index using its kind"
    observe "use a qualified proof and a partial family through imported type synonyms" aliasedReflection
    observe "render a retained callback with ImpredicativeTypes enabled" (map showReflection [Just True, Nothing])
    observe "forward a runtime element with a quantified partial signature" (partialElement enabled, partialElement disabled)
    observe "render a raw view through an ordinary wrapper around a partial family" renderedRawView
    observe "split collections whose types determine their elements"
        (splitCollection [enabled, disabled], splitCollection (Just 'x'), splitCollection (Nothing :: Maybe Char))
    observe "read the derived element type of a list-backed buffer"
        (firstBuffered (Buffered [enabled, disabled]), firstBuffered (Buffered [] :: Buffered Bool))
    observe "show the rest of a collection"
        (showRemainder [enabled, disabled], showRemainder (Just 'x'), showRemainder ([] :: [Bool]))
    observe "print a cursor through its constraint synonym"
        (showAdvance id [enabled, disabled], showAdvance id [disabled], showAdvance (== 'x') (Just 'x'))
    observe "advance a cursor using its superclass family equation"
        (advanceCursor id [enabled, disabled], advanceCursor id [disabled, enabled],
         advanceCursor (== 'x') (Just 'x'))
    boxedFlag <- newIORef (Just True) >>= readIORef
    boxedLetter <- newIORef (Just 'x') >>= readIORef
    observe "use both equations of an open arrow kind" (openKindRows disabled)
    observe "use a stored retained callback at Bool and Char" (readStoredReflection boxedFlag boxedLetter)
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
    observe "certified Unicode title, upper, and lower case" certifiedCaseMappings
    observe "certified character predicate" certifiedCharacterPredicate
    observe "match the Int# field in a closed type family" machineIntProof
    observe "match zero and the target maximum in a Word# family" PrimitiveWordFieldsClient.wordFieldResults
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
    text <- newIORef " x\t" >>= readIORef
    observe "classify whitespace in a runtime string"
        [characterDecisionValue (CharacterDecision.spaceDecision c) | c <- text]
    observe "Unicode code points from an exported Data.Char.toUpper equality"
        (map fromEnum [UnicodeExport.p3 (Proxy @'é'), UnicodeExport.p3 (Proxy @'\x1e943')])
    observe "written character indices and a lazy later element"
        (RetainedCharacterClient.observeCharacters (error "forced the later character"))
    observe "quoted retained string with an embedded NUL"
        (RetainedCharacterClient.observeQuoted (error "forced the later string"))
    observe "small and large Natural literals at their exact indices" RetainedNaturalClient.observeNaturals
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
    observe "pass named, computed and forwarded erased packages to a polymorphic reader"
        ( RankedPackageClient.runMixedPackage 17
        , RankedPackageClient.runProducedPackage 23
        , RankedPackageClient.runForwardedPackage 29
        )
    observe "read a forwarded erased package and keep its label"
        (RankedPackageClient.runLabelledPackage 31)
    packageChoice <- newIORef True
    chooseLeft <- readIORef packageChoice
    writeIORef packageChoice False
    chooseRight <- readIORef packageChoice
    observe "read the selected erased package and leave the other lazy"
        (RankedPackageClient.runSelectedPackage chooseLeft,
         RankedPackageClient.runSelectedPackage chooseRight)
    let evens = ImplicitVector.filterVec even (VCons (4 :: Int) VNil)
        odds = ImplicitVector.filterVec odd (VCons (5 :: Int) VNil)
    observe "open the vector chosen by a runtime conditional"
        (RankedPackageClient.firstSelected chooseLeft evens (error "unused right vector"),
         RankedPackageClient.firstSelected chooseRight (error "unused left vector") odds)
    observe "open the result of the selected polymorphic filter"
        ( RankedPackageClient.firstSelectedFilter chooseLeft even
            (VCons (6 :: Int) (error "unused left tail")) (error "unused right input")
        , RankedPackageClient.firstSelectedFilter chooseRight odd
            (error "unused left input") (VCons (7 :: Int) (error "unused right tail")) )
    let unusedVector = error "unused case alternative"
        order = compare chooseRight chooseLeft
    observe "open one of three vectors selected by a case"
        ( RankedPackageClient.firstByOrdering order evens unusedVector unusedVector
        , RankedPackageClient.firstByOrdering EQ unusedVector odds unusedVector
        , RankedPackageClient.firstByOrdering GT unusedVector unusedVector evens )
    observe "read a vector forwarded through id"
        (RankedPackageClient.firstForwarded odds)
    observe "consume a hidden Boolean or integer once"
        (RankedPackageClient.readCount (ErasedPackage.encodeCount (Left True)),
         RankedPackageClient.readCount (ErasedPackage.encodeCount (Right 17)))
    observe "read dependent fields from a local package pattern"
        (RankedPackageClient.runLocalPattern chooseLeft,
         RankedPackageClient.runLocalPattern chooseRight)
    let formatterTag = error "unused formatter tag"
    observe "use a Show superclass to format a reopened package"
        ( RetainedPackageFormatClient.formatPackage formatterTag
            (RetainedPackageFormat.chooseValue chooseLeft True False)
        , RetainedPackageFormatClient.formatPackage formatterTag
            (RetainedPackageFormat.chooseValue chooseRight 'x' 'q') )
    observe "filter a vector without forcing the tail to learn the result length"
        ErasedPackage.filterObservation
    observe "infer the hidden lengths of a lazy filter and a retained vector"
        ImplicitVector.observations
    observe "refilter a vector through a rank-2 callback at its hidden length"
        ( take 1 (ImplicitVector.cells (RankedPackageClient.refilter odd (> 3)
            (VCons (3 :: Int) (VCons 7 (error "unused refilter tail")))))
        , ImplicitVector.cells (RankedPackageClient.refilter odd (> 3)
            (VCons (2 :: Int) (VCons 7 (VCons 9 VNil)))) )
    observe "refilter two vectors and infer both hidden result lengths" $
        RankedPackageClient.refilterRows odd even (< 10)
            (VCons (7 :: Int) (VCons 11 VNil))
            (VCons 2 (VCons 4 (VCons 10 VNil)))
    observe "repack two hidden lengths through a local let and case"
        ( ImplicitVector.rowLists (RankedPackageClient.swapRowsLet
            (ImplicitVector.partitionRows odd (VCons (7 :: Int) (VCons 2 (VCons 9 VNil)))))
        , take 1 (fst (ImplicitVector.rowLists (RankedPackageClient.swapRowsCase
            (ImplicitVector.packRows (error "unused row")
                (VCons (4 :: Int) (error "unused row tail")))))) )
    cutoff <- newIORef (7 :: Int) >>= readIORef
    observe "filter a numeric zip through an Int callback at its hidden length"
        ( take 1 (RankedPackageClient.refilterNumericZip (> cutoff)
            1 (error "unused left suffix") 6 (error "unused right suffix"))
        , RankedPackageClient.refilterNumericZip (> cutoff) 1 2 6 2
        , RankedPackageClient.refilterNumericZip (> cutoff) 0 2 5 2 )
    observe "attach a reader at the hidden length of a filtered vector"
        ( RankedPackageClient.readFiltered (RankedPackageClient.packageFiltered odd
            (Data.Maybe.listToMaybe . toList) (VCons (7 :: Int) (error "unused packaged tail")))
        , RankedPackageClient.readFiltered (RankedPackageClient.packageFiltered odd
            (const True) (error "unused packaged input" :: Vec Int 'Z)) )
    observe "project a stored vector from a strict record field"
        (StoredVector.cells (StoredVector.singleton 'h'))
    observe "read a retained length through a newtype record selector" $
        StoredVector.wrappedContents (StoredVector.wrapVector 2
            (VCons 'h' (VCons 's' VNil)))
    observe "read a GADT newtype length without forcing its vector" $
        StoredVector.gadtLength 1 (error "unused vector" :: Vec Char 1)
    observe "read nested lengths and the first aligned cells"
        StoredVector.nestedObservation
    observe "apply a stored selector at a different vector length" StoredVector.viewSelection
    let (passing, failing) = ImplicitVector.partitionVec odd
            (VCons (7 :: Int) (VCons 2 (VCons 9 VNil)))
        (firstPart, _) = ImplicitVector.partitionVec odd (VCons (7 :: Int) (error "unused partition tail"))
    observe "partition a vector at two independent result lengths"
        (ImplicitVector.cells passing, ImplicitVector.cells failing,
         take 1 (ImplicitVector.cells firstPart))
    observe "match a hidden vector inside a pair without opening its other field"
        (ImplicitVector.firstKept (firstPart, error "unused rejected partition"),
         ImplicitVector.firstKept (ImplicitVector.partitionVec odd (VCons (2 :: Int) VNil)))
    let rows = ImplicitVector.partitionRows odd
            (VCons (7 :: Int) (VCons 2 (VCons 9 VNil)))
    observe "match both hidden lengths in one partition result" $
        ImplicitVector.rowLists rows
    observe "choose a partition without forcing the unused branch" $
        ImplicitVector.rowLists (Data.Bool.bool (error "unused partition") rows True)
    observe "match hidden partition lengths in function equations"
        (ImplicitVector.firstPassing (ImplicitVector.partitionRows odd
            (VCons (7 :: Int) (error "unused equation tail"))),
         ImplicitVector.firstPassing (ImplicitVector.partitionRows odd
            (VCons (2 :: Int) VNil)))
    observe "map an ordinary observer over differently sized partitions" $
        PartitionChoices.partitionViews (VCons 7 (VCons 2 (VCons 9 VNil)))
    observe "use a hidden polymorphic identity at Int and Bool" RankedPackage.runCallback
    observe "infer a hidden function type from its paired callback" RankedPackage.runPair
    observe "apply the second projection to the first in an ordinary module" RankedPackageClient.runProjections
    observe "project twice from the same package producer call" (map RankedPackageClient.projectAt [True,False])
    observe "infer a polymorphic lambda witness and call its builder at Int" (RankedPackage.runBuilder 11)
    observe "infer a polymorphic witness through a forwarding lambda"
        (map RankedPackageClient.runRankedInput [chooseLeft, chooseRight])
    observe "infer a guarded callback and skip an unused input" RankedPackageClient.runGuarded
    observe "infer a hidden callback type from an application argument"
        (map RankedPackageClient.runArgumentWitness [False, True])
    observe "infer one callback type for both arms of a conditional producer"
        ( RankedPackageClient.runConditional chooseLeft (error "unused conditional input")
        , RankedPackageClient.runConditional chooseRight True )
    observe "infer callback witnesses from a type argument and separate equations"
        RankedPackageClient.runDeclaredWitnesses
    observe "choose each branch's callback witness by type application"
        RankedPackageClient.runExplicitBranches
    observe "read an integer through an erased package" (PackageRules.roundTrip 7)
    observe "apply an imported integer package rule and keep the witness lazy"
        (PackageRules.open (41 :: Int, (+ 1)),
         PackageRules.open (undefined :: Int, const 7),
         PackageRules.open (True, fromEnum))
    observe "match and open existential packages in optimizer rules"
        (LexicalPackageRules.local 3 (LexicalPackageRules.hideInt 5))
    observe "select ordinary instances for streams with hidden state"
        (StreamPreview.preview (ErasedStream.fromList ('G' : 'H' : error "unused character tail")),
         StreamPreview.preview (ErasedStream.fromList (True : error "unused Boolean tail")),
         StreamPreview.preview @(Either Char (ErasedStream.Stream Char)) (Left 'x'))
    observe "resolve the stream instance from its requested element type"
        (StreamPreview.prefix 2 (ErasedStream.fromList [4, 5 :: Int]) :: [Int])
    observe "read streams with different hidden state types"
        (ErasedStream.toList (ErasedStream.oddThrough 6),
         take 1 (ErasedStream.toList (ErasedStream.fromList (True : error "unused stream tail"))))
    observe "fold a stream and stop before an undefined tail"
        (StreamFold.digits (ErasedStream.oddThrough 6),
         StreamFold.first (ErasedStream.fromList (True : error "unused fold tail")),
         StreamFold.first (ErasedStream.fromList ([] :: [Bool])))
    observe "fold a mapped stream without forcing its head or tail"
        (StreamFold.firstMapped not (ErasedStream.fromList [False]),
         StreamFold.firstMapped (const 'x') (ErasedStream.fromList (undefined : undefined)))
    observe "transport element equality under a hidden vector length"
        (ImplicitVector.cells (ImplicitVector.transportFiltered Refl passing))
    observe "zip a filtered vector with itself at its hidden length" $
        take 1 (ImplicitVector.pairWithSelf
            (ImplicitVector.filterVec odd (VCons (7 :: Int) (error "unused vector tail"))))
    observe "pass a lambda with a hidden vector length and filter its result"
        (ImplicitVector.runBuilder ImplicitVector.singletonBuilder odd (7 :: Int))
    observe "forget the retained witness and inspect the indexed payload"
        ( map (ForgetWitness.observe . ForgetWitness.forget . ForgetWitness.retained) [True, False]
        , ForgetWitness.observe (ForgetWitness.forget (ForgetWitness.unknown (error "forced witness"))) )
    observe "match independent retained sums in a repeated type-family argument"
        PackageEquality.sameRetained
    observe "inherit a class method that fills a length-indexed vector"
        (toList (filled @Char 3))
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
    observe "read independent length witnesses in a recursive batch"
        ( Refl :: Batch.build 4 :~: 'Batch.Chunk ('E.SS 'E.SZ) ('IndexedVec.VCons 4 'IndexedVec.VNil)
                    ('Batch.Chunk ('E.SS ('E.SS 'E.SZ))
                        ('IndexedVec.VCons 5 ('IndexedVec.VCons 6 'IndexedVec.VNil)) 'Batch.End)
        , case Batch.build 4 of
            batch@(Batch.Chunk _ _ rest) -> [Batch.firstWidth batch, Batch.firstWidth rest]
            Batch.End -> []
        , Batch.firstWidth (Batch.partial 9) )
    observe "select values with a mask at the same stored length"
        ( Refl :: Batch.maskedRow 4 :~:
            'Batch.MaskedRow ('E.SS ('E.SS 'E.SZ))
                ('IndexedVec.VCons 4 ('IndexedVec.VCons 5 'IndexedVec.VNil))
                'x' ('IndexedVec.VCons 'False ('IndexedVec.VCons 'True 'IndexedVec.VNil))
        , Batch.selectedValues (Batch.maskedRow 4)
        , Batch.rowWidth (Batch.partialRow 9) )
    observe "look up a column at a shared length carried by the vectors"
        ( Refl :: Batch.keyedRow 4 :~: 'Batch.KeyedRow 'r'
            ('IndexedVec.VCons 'x' ('IndexedVec.VCons 'y' 'IndexedVec.VNil))
            ('IndexedVec.VCons 4 ('IndexedVec.VCons 5 'IndexedVec.VNil))
        , Batch.lookupColumn 'y' (Batch.keyedRow 4)
        , Batch.lookupColumn 'y' (Batch.keyedRow 0) )
    observe "read row and column totals at independent hidden lengths"
        ( Refl :: Batch.tableMargins 4 :~: 'Batch.TableMargins
            ('IndexedVec.VCons 'r' 'IndexedVec.VNil)
            ('IndexedVec.VCons 'x' ('IndexedVec.VCons 'y' 'IndexedVec.VNil))
            ('IndexedVec.VCons 8 'IndexedVec.VNil)
            ('IndexedVec.VCons 4 ('IndexedVec.VCons 4 'IndexedVec.VNil))
        , Batch.lookupTotals 'r' 'y' (Batch.tableMargins 4)
        , Batch.lookupTotals 'r' 'x' (Batch.tableMargins 0) )
    observe "sum a rectangular table by row and by column"
        ( Refl :: Table.totals (Table.sample 4) :~: 'Batch.TableMargins
            ('IndexedVec.VCons 'r' ('IndexedVec.VCons 's' 'IndexedVec.VNil))
            ('IndexedVec.VCons 'x' ('IndexedVec.VCons 'y' ('IndexedVec.VCons 'z' 'IndexedVec.VNil)))
            ('IndexedVec.VCons 6 ('IndexedVec.VCons 15 'IndexedVec.VNil))
            ('IndexedVec.VCons 5 ('IndexedVec.VCons 7 ('IndexedVec.VCons 9 'IndexedVec.VNil)))
        , Table.selectedTotals (Table.sample 4) )
    observe "read back the stored cells at both table dimensions"
        (Refl :: Table.sample 4 :~: 'Table.Table (Vec2 'r' 's') (Vec3 'x' 'y' 'z')
            (Vec2 (Vec3 1 2 3) (Vec3 4 5 6)))
    observe "recover a table's row count from its cells"
        (Refl :: Table.withoutRowLabels (Table.sample 4) :~: 'Table.ColumnTable
            (Vec3 'x' 'y' 'z') (Vec2 (Vec3 1 2 3) (Vec3 4 5 6)))
    observe "recover one side length for every row of a square"
        (Refl :: Table.square 1 :~: 'Table.Square (Vec2 (Vec2 1 2) (Vec2 3 4)))
    observe "check each row against column labels stored after the cells"
        (Refl :: Table.matrix (Table.sample 4) :~: 'Table.Matrix
            (Vec2 (Vec3 1 2 3) (Vec3 4 5 6)) (Vec3 'x' 'y' 'z'))
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
    observe "use one class default for a constructor kind and a callback type"
        LegacyKindArgument.defaultCallback
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
    observe "show a local result checked by an associated equation"
        (Refl :: LocalDerivedAssociated.Flipped 'LocalDerivedAssociated.Stop :~: 'LocalDerivedAssociated.Go,
         LocalDerivedAssociated.sample ())
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
        (Refl :: Map FlipFlag '[ 'True, 'False] :~: '[ 'False, 'True])
    observe "map partially applied type-level addition over a list"
        (Refl :: Map ((TypeNats.+) 1) '[2, 3, 4] :~: '[3, 4, 5])
    observe "replace an equal callback before supplying the input list"
        (sameMapping (Refl :: FlipFlag :~: FlipFlag))
    observe "pass a type family with an unapplied invisible binder"
        (Refl :: InvisibleFamily.Observed :~: 'True)
    observe "infer the result kind after passing a visible family argument"
        (Refl :: InvisibleFamily.ObservedVisible :~: Int)
    observe "infer the first invisible family argument and leave the second open"
        (Refl :: InvisibleFamily.ReadFlag :~: 'True, Refl :: InvisibleFamily.ReadType :~: Int)
    observe "apply Maybe and a type family through a computed arrow kind" InvisibleFamily.wrappedValues
    observe "pass a family before giving its required kind argument" InvisibleFamily.requiredArguments
    observe "map cells with a class method over a family prefix and Maybe" mappedCells
    observe "infer the matchability of a family prefix from a later argument" laterMatchability
    observe "infer the arrows in a promoted Endo callback" inferredEndo
    observe "infer the arrows of a bare promoted Endo passed to Pure1" barePromotedEndo
    observe "apply one inferred synonym to a constructor and a family" appliedSynonym
    observe "use a standalone family callback in an ordinary module"
        (StandaloneCallbackClient.callbackResults (Just 'q') [False, True])
    observe "forward an inferred partial-family callback and an ordinary constructor" forwardedMap
    observe "infer both bare kind arrows in a list family proof" annotatedMap
    observe "infer the inner arrow of a higher-order family callback" nestedCallback
    observe "infer an unsigned synonym kind from a bare callback use" appliedCallback
    observe "pass a partial synonym and a list constructor to one callback type" LiberalCallback.collectValues
    observe "store the result of Maybe and an imported open family" boxedCallbacks
    observe "use one polymorphic map with Maybe and a type family" rankedMap
    observe "apply a callback with matchability computed in its declared kind" flippedCallbacks
    observe "select a dictionary with computed matchability" computedDictionary
    let applied = View.Applied @M @Maybe Proxy View.KnownM 'q' (fmap odd)
        (element, run) = View.viewResult applied
    observe "recover the element and its map from a matchable application"
        (element, run (Just 3), View.betaResult applied (Just 4))
    observe "interpret a kind code as an ordinary type constructor" InterpretedKind.optionalFlag
    observe "interpret two applications of the optional type constructor" InterpretedKind.nestedOptional
    observe "choose an interpreted arrow from its expected kind"
        (InterpretedKindUseSite.atM, InterpretedKindUseSite.atU)
    observe "use a nested arrow for Either and a short-circuiting callback"
        (NestedFunctionClient.binaryChoice, NestedFunctionClient.conjunction False (error "unused operand"))
    observe "apply a type lambda to Maybe, Either String and IO" TypeLambda.roadmap
    let checkedRow :: TypeLambda.CheckedRow String '[Bool, Int]
        checkedRow = TypeLambda.CheckedRow
            (TypeLambda.Field (Right True) (TypeLambda.Field (Left "missing count") TypeLambda.End))
    case TypeLambda.checkedCells checkedRow of
        TypeLambda.Field flag (TypeLambda.Field count TypeLambda.End) ->
            observe "store a checked value or an error in each record cell" (flag, count)
    observe "map a partially applied column type synonym" TypeLambda.optionalColumns
    observe "compare two column readers before choosing the cell type" TypeLambda.sameColumnReader
    observe "partially apply a two-argument type lambda" TypeLambda.partial
    observe "map a callback that captures a closed family pattern" TypeLambda.prefixes
    observe "capture a type before and after a local let"
        (Refl :: TypeLambda.ScopedRow Char '[Bool] :~: '[(Maybe Char, Either Char Bool)])
    let fields = TypeLambda.Field 'x' (TypeLambda.Field True TypeLambda.End)
    case (TypeLambda.withErrors (Proxy @String) fields
          :: TypeLambda.Fields (TypeLambda.Map (\cell -> Either String cell) '[Char, Bool])) of
        TypeLambda.Field x (TypeLambda.Field y TypeLambda.End) ->
            observe "give every field in a row the same error type" (x, y)
    case TypeLambda.withScopedErrors (Proxy @Ordering) fields of
        TypeLambda.Field x (TypeLambda.Field y TypeLambda.End) ->
            observe "map row cells with a lambda in a local signature" (x, y)
    case TypeLambda.taggedCell (Proxy @'True) True of
        TypeLambda.Field (Proxy, value) TypeLambda.End ->
            observe "tag a row cell with a local type lambda that infers the capture kind" value
    case (TypeLambdaQuote.checkedColumns, TypeLambdaQuote.labelledColumn "column") of
        (TypeLambda.Field count (TypeLambda.Field flag TypeLambda.End),
         TypeLambda.Field label TypeLambda.End) ->
            observe "splice a column schema and attach a caller-supplied label" (count, flag, label)
    observe "read two named sensors with a missing slot" QuotedSensorRow.readings
    let reported :: TypeLambda.Fields '[Either Ordering Int, Either Ordering Bool]
        reported = TypeLambdaArgument.rowFor @(\cell -> Either Ordering cell) Proxy (Right 42) (Left LT)
    case reported of
        TypeLambda.Field count (TypeLambda.Field flag TypeLambda.End) ->
            observe "pass a row schema as a visible type argument" (count, flag)
    let lazyFields = TypeLambda.Field 'x' (TypeLambda.Field (error "unused field" :: Bool) TypeLambda.End)
    case TypeLambdaClient.forwardErrors (Proxy @String) lazyFields of
        TypeLambda.Field first _ ->
            observe "forward a row with the same callback across modules" first
    let optional :: TypeLambda.Fields (TypeLambda.Columns TypeLambda.Optional '[Int])
        optional = TypeLambda.Field (Just 42) TypeLambda.End
        checked :: TypeLambda.Fields (TypeLambda.Columns (SchemaColumns.Checked String) '[Bool])
        checked = TypeLambda.Field (Left "missing") TypeLambda.End
    case (optional, checked) of
        (TypeLambda.Field x TypeLambda.End, TypeLambda.Field y TypeLambda.End) ->
            observe "use column schemas defined in separate modules" (x, y)
    observe "map a type lambda with a constructor case" TypeLambda.mixed
    observe "read fields from two indexed type case branches" TypeLambda.indexedFields
    observe "retain the kind of a matched Nothing"
        (Refl :: TypeLambda.Rebuild ('Nothing :: Maybe Ordering) :~: Proxy ('Nothing :: Maybe Ordering))
    observe "name and reuse a type-level intermediate result" TypeLet.roadmap
    observe "sum two matrix dimensions with a type let"
        (TypeNats.natVal (Proxy @(TypeLet.DimensionSum '(2, 3))))
    observe "sum two named dimensions while leaving depth unknown"
        (TypeNats.natVal (Proxy @(TypeLet.ProductDimensionSum
            ('TypeLet.Dimensions 2 3 TypeLet.UnknownDepth))))
    observe "splice a type let that shadows its outer binder"
        (Right (Just True) :: $([t| let a = Bool in let a = Maybe a in Either a a |]))
    observe "capture an outer type in a let group before shadowing its name"
        (Refl :: TypeLet.CaptureBeforeShadow 'False :~: '( 'False, 'True))
    let row = VCons 'x' (VCons 'y' VNil)
    observe "select a vector header with a runtime flag and a computed width"
        (TypeLet.frameValues enabled 2 ':' row,
         TypeLet.frameValues disabled 2 (error "unused header") row)
    observe "refer to an outer type let before shadowing it in a case"
        (Refl :: NativeCase.NestedLetShadow 'True ('Just 'False) :~: (Proxy 'False, Proxy ('Just 'True)))
    observe "bind the result of a constructor case"
        (Right True :: TypeLet.CaseResult 'True, Left 'c' :: TypeLet.CaseResult 'False)
    observe "use a default width from a preceding type case"
        (TypeNats.natVal TypeLet.defaultWidth)
    observe "choose a payload type inside a closed family equation"
        ('q' :: FamilyCase.ChoosePayload Char 'Nothing,
         True :: FamilyCase.ChoosePayload Char ('Just Bool))
    observe "recurse on a smaller input in a type case"
        (Refl :: FamilyCase.Down ('FamilyCase.S ('FamilyCase.S 'FamilyCase.Z)) :~: 'True)
    observe "select the result type by a constructor" NativeCase.roadmap
    observe "repeat a type case in a function argument and result"
        (NativeCase.caseIdentity (Proxy @'True) 42, NativeCase.caseIdentity (Proxy @'False) False)
    observe "select through nested type case patterns"
        ( Proxy :: NativeNestedCase.Pick ('Just ('Just 'True))
        , 7 :: NativeNestedCase.Pick ('Just 'Nothing) )
    observe "select a constructor at an open family index"
        ( 7 :: NativeNestedCase.OpenIndexPick (NativeNestedCase.OpenCaseIndex 'False)
                  ('Just ('NativeNestedCase.OpenIndexed ('Proxy :: Proxy 'False)))
        , False :: NativeNestedCase.OpenIndexAtOrdering ('Just ('NativeNestedCase.OpenPlain 'False)) )
    observe "distinguish an outer index from a shadowing pattern variable"
        ( Refl :: NativeNestedCase.ScopedOpenIndex 'True ('Just ('NativeNestedCase.OpenPlain 'True))
                  :~: Proxy 'True
        , Refl :: NativeNestedCase.ScopedOpenIndex 'True ('Just ('NativeNestedCase.OpenPlain 'False))
                  :~: Proxy 'False )
    observe "refine a constructor field and transport a later dependent index"
        ( Refl :: NativeNestedCase.IndexedFieldPick
            ('Just ('NativeNestedCase.RefinedFieldBox ('Proxy :: Proxy 'False)
                     ('NativeNestedCase.FieldOn 'True))) :~: Proxy 'True
        , Refl :: NativeNestedCase.DependentFieldPick
            ('Just ('NativeNestedCase.IndexedField 'Proxy)) :~: Char )
    observe "refine the outer and inner GADT indices in one type case"
        ( 7 :: NativeNestedCase.RefinedBranchPick 'False
            ('Just ('NativeNestedCase.RefinedBranch 'Proxy ('NativeNestedCase.RefinedBit 'True)))
        , True :: NativeNestedCase.RefinedBranchPick 'False
            ('Just ('NativeNestedCase.RefinedBranch 'Proxy 'NativeNestedCase.InnerDisjoint))
        , 'x' :: NativeNestedCase.RefinedBranchPick 'False ('Just 'NativeNestedCase.OuterDisjoint) )
    observe "choose types in ordinary and rank-n constructor fields" constructorFieldCases
    let fieldResult :: Bool -> String
        fieldResult b = case NativeNestedCase.retainedCaseField of
            NativeNestedCase.RetainedCaseField choose -> case b of
                True -> show (choose b :: Int)
                False -> [choose b]
        negatedResult :: Bool -> String
        negatedResult b = case NativeNestedCase.negatedCaseField of
            NativeNestedCase.NegatedCaseField choose -> case b of
                True -> [choose b]
                False -> show (choose b :: Int)
        quotedResult :: Bool -> String
        quotedResult b = NativeRetainedTH.render b (NativeRetainedTH.quoted b)
        splicedResult :: Bool -> String
        splicedResult b = NativeRetainedTH.renderCase b (NativeRetainedTH.quotedCase b ())
        storedResult :: Bool -> String
        storedResult b =
            NativeRetainedConTH.renderStored
                (NativeRetainedConTH.Stored b (NativeRetainedTH.quoted b))
        inlineStoredResult :: Bool -> String
        inlineStoredResult b = case NativeRetainedConTH.storeInline b of
            NativeRetainedConTH.InlineStored True n -> show n
            NativeRetainedConTH.InlineStored False c -> [c]
    fieldFlag <- newIORef True
    firstField <- fieldResult <$> readIORef fieldFlag
    firstNegated <- negatedResult <$> readIORef fieldFlag
    firstQuoted <- quotedResult <$> readIORef fieldFlag
    firstSpliced <- splicedResult <$> readIORef fieldFlag
    firstStored <- storedResult <$> readIORef fieldFlag
    firstInlineStored <- inlineStoredResult <$> readIORef fieldFlag
    writeIORef fieldFlag False
    secondField <- fieldResult <$> readIORef fieldFlag
    secondNegated <- negatedResult <$> readIORef fieldFlag
    secondQuoted <- quotedResult <$> readIORef fieldFlag
    secondSpliced <- splicedResult <$> readIORef fieldFlag
    secondStored <- storedResult <$> readIORef fieldFlag
    secondInlineStored <- inlineStoredResult <$> readIORef fieldFlag
    observe "pass a refined runtime Boolean to a retained field" (firstField, secondField)
    observe "choose a field result by negating its runtime index" (firstNegated, secondNegated)
    observe "choose a result with a quoted retained signature" (firstQuoted, secondQuoted)
    observe "splice a result type selected by a retained Boolean"
        (NativeRetainedTH.quotedCase True (), NativeRetainedTH.quotedCase False ())
    observe "compose two signature splices at a runtime Boolean" (firstSpliced, secondSpliced)
    observe "store a runtime index in a quoted constructor" (firstStored, secondStored)
    observe "read a case-selected field from a declaration quotation"
        (firstInlineStored, secondInlineStored)
    let aliasResult :: Bool -> String
        aliasResult b = case LocalAliasField.aliasField of
            LocalAliasField.AliasField choose -> case b of
                True -> show (choose b :: Int)
                False -> [choose b]
    observe "choose a retained field result through a saturated type synonym"
        (aliasResult False, aliasResult True)
    observe "select through an ordinary type constructor"
        ( 7 :: NativeNestedCase.TypeHead ('Just Bool)
        , 'x' :: NativeNestedCase.TypeHead ('Just Ordering)
        , () :: NativeNestedCase.TypeHead ('Just [Bool]) )
    observe "select an arrow result inside forall"
        ( NativeNestedCase.forallResult (Proxy @'LT) NativeNestedCase.CaseOn
        , NativeNestedCase.forallResult (Proxy @'False) NativeNestedCase.CaseMissing )
    let arrowInput :: NativeNestedCase.ArrowDomain 'LT ('Just ('Just 'True))
        arrowInput (_, value) = value
        arrowMissing :: NativeNestedCase.ArrowDomain 'EQ 'Nothing
        arrowMissing c = fromEnum c
    observe "select the input type of an ordinary arrow"
        (arrowInput (Proxy, 7), arrowMissing 'x')
    let linearInput :: NativeNestedCase.LinearArrowDomain 'True
        linearInput x = x
        pairIdentity :: PairCaseArrow 'True
        pairIdentity x = x
        pairAtOne :: (Int, Char) %1 -> (Int, Char)
        pairAtOne x = pairIdentity @'One x
    observe "select linear and multiplicity-polymorphic arrow inputs"
        (linearInput 7, pairAtOne (19, 'q'), fst (pairIdentity @'Many (29, error "unused pair field")))
    observe "match Bool and Ordering directly in a type"
        (7 :: NativeCase.ByType Bool, 'x' :: NativeCase.ByType Ordering)
    observe "remove a Maybe layer and keep any other type"
        (7 :: NativeCase.ElementOrSelf (Maybe Int), 'x' :: NativeCase.ElementOrSelf Char)
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
    observe "leave the permission argument open in an ordinary traffic-light function"
        ( OpenMapProof.greenPermission (Proxy @'True)
        , OpenMapWorker.mayProceed OpenMapWorker.Red (error "unused permission") )
    observe "use Prelude.not on a known head with an unknown tail"
        (InstalledNotProof.negateHead (Proxy @'[]))
    putStrLn ("map Prelude.not and Prelude.id over a known head and an unknown tail: "
              ++ PreludeMapProof.showPreludeMapProofs)
    putStrLn ("prove map preserves length and leaves the callback lazy: "
              ++ PreludeMapLengthProof.showPreludeMapLength)
    putStrLn ("prove that append adds the list lengths: "
              ++ PreludeAppendLengthProof.showPreludeAppendLength)
    putStrLn ("prove the list fold law for append: " ++ ListFoldrProof.showListFoldAppend)
    observe "prove the append law through the Prelude.foldr list instance"
        PreludeFoldrProof.example
    putStrLn ("prove the two equations of a Maybe mapping method: " ++ MappingProof.example)
    -- These proofs use the installed Functor Maybe equations. An importing
    -- module checks their rule dependencies when it uses the exported proof.
    putStrLn ("prove that Prelude.fmap leaves Nothing unchanged: " ++ PreludeMappingProof.example)
    putStrLn ("map an open Just field once and twice with Prelude.fmap: " ++ PreludeMappingProof.presentExample)
    putStrLn ("prove the two Either equations of Prelude.fmap: " ++ OrdinaryEitherMapping.example)
    cutoff <- newIORef (4 :: Int) >>= readIORef
    putStrLn ("map a runtime predicate and prove the label is preserved: "
              ++ OrdinaryTheseMapping.report (> cutoff) 'w' 5)
    putStrLn ("map a runtime-sized string and prove the label is preserved: "
              ++ OrdinaryTheseMapping.report (replicate cutoff) 'w' 'a')
    putStrLn ("map a partially applied comparator and prove its argument order: "
              ++ OrdinaryTheseMapping.reportAt (flip compare) False 'w' True)
    let reportInstances :: (Int -> Bool) -> Bool -> Int -> String
        reportInstances p flag x =
            show ( fmap p (Row.Flagged flag x :: Row.Row Int Bool Int)
                 , fmap p (Row.Flagged flag x :: Row.Row Bool Bool Int)
                 , RowProof.mapped p flag x, RowProof.discarded p flag x )
    putStrLn ("map the same constructor with two instance dictionaries: "
              ++ reportInstances (> cutoff) True 5)
    putStrLn ("prove the checked and literal Either family branches: " ++ OrdinaryFamilyBranches.example)
    putStrLn ("prove that folding a mapped list composes the step: " ++ PreludeFoldrProof.showFoldMap)
    reverseInput <- newIORef [error "forced reverse element", False] >>= readIORef
    observe "prove that reverse preserves list length"
        (HaskLib.lengthReverse reverseInput, HaskLib.size (reverse reverseInput))
    observe "pass size as a callback and check the reversed list length"
        (case measureReverseLength HaskLib.size reverseInput Refl of
             (n, Refl) -> n)
    prefixBound <- newIORef (HaskLib.Succ HaskLib.Zero) >>= readIORef
    prefixInput <- newIORef (True : error "forced the list beyond the prefix") >>= readIORef
    observe "take a runtime-bounded prefix and prove its length"
        (HaskLib.takeSize prefixBound prefixInput, HaskLib.lengthTakeSize prefixBound prefixInput)
    putStrLn ("prove that map distributes over append: "
              ++ PreludeMapAppendProof.showPreludeMapAppend)
    putStrLn ("prove that two maps compose on a finite list: "
              ++ PreludeMapCompositionProof.showPreludeMapComposition)
    putStrLn ("filter with Prelude.not and Prelude.id without forcing the kept tail: "
              ++ PreludeFilterProof.showPreludeFilterProofs)
    putStrLn ("prove filter never increases length: "
              ++ PreludeFilterLengthProof.showPreludeFilterLength)
    putStrLn ("prove that filter distributes over append: "
              ++ PreludeFilterAppendProof.showPreludeFilterAppend)
    flag <- newIORef True >>= readIORef
    let prefix bit rest = IndexedPrefix.elements (IndexedPrefix.negatedCons bit rest)
        unusedTail = error "forced indexed list tail"
    observe "read the head of a list indexed by its contents"
        (take 1 (prefix flag unusedTail), take 1 (prefix (not flag) unusedTail),
         not (null (prefix (error "forced indexed list head") unusedTail)))
    IndexedPrefixClient.matchIndex >>= observe "match the stored list equality with an unevaluated head and tail"
    DirectIndexedListClient.firstCell >>= observe "read the first cell of a directly indexed list"
    DirectIndexedVectorClient.firstCells >>= observe "read the first cells of polymorphic directly indexed vectors"
    observe "match the stored length of directly indexed vectors" DirectIndexedVectorClient.firstCellsByLength
    observe "read a payload at the predecessor of its public index" DirectIndexedVectorClient.successorPayload
    IndexedMatrixClient.observeDimensions >>= observe "read a rectangle's rows and compare a square's dimensions"
    IndexedMatrixClient.observeTile >>= observe "pack and open tiles with runtime dimensions"
    observe "read the top layer of a two-layer volume" IndexedMatrixClient.observeTopLayer
    observe "adjust a reading while preserving its calibration index"
        (DirectIndexedListClient.readingFields enabled 36)
    observe "read calibrated samples from erased packages"
        (DirectIndexedListClient.acceptedSamples flag)
    runtimeList <- newIORef [False, True, False]
    xs <- readIORef runtimeList
    putStrLn ("prove the filter bound from a runtime list: "
              ++ PreludeFilterRuntime.showRuntimeBound (PreludeFilterRuntime.packRuntimeBound xs))
    ref <- newIORef (PreludeFilterDecider.SomeFilterDecider PreludeFilterDecider.alwaysDecider)
    decider <- readIORef ref
    putStrLn ("prove the filter bound with a runtime decider: "
              ++ PreludeFilterDecider.showDecidedBound decider xs)
    cutoff <- newIORef (Size.Succ Size.Zero) >>= readIORef
    rows <- newIORef [[], [False], [True, False]] >>= readIORef
    putStrLn ("prove the filter bound for rows at a runtime cutoff: "
              ++ PreludeFilterCutoff.showCutoffBound (PreludeFilterCutoff.packCutoff cutoff rows))
    scores <- newIORef [(1,"low"), (3,"high"), (2,"equal"), (5,"higher")] >>= readIORef
    let above :: (Int, String) -> Bool
        above (n, _) = n > length rows - 1
        unchecked :: Int -> Bool
        unchecked _ = error "empty filter forced its predicate"
        (selected, bound) = PreludeFilterRuntime.filterReport above scores
        (_, emptyBound) = PreludeFilterRuntime.filterReport unchecked []
    observe "bound a filter using its runtime predicate"
        (map snd selected, bound, emptyBound)
    gateOff <- newIORef False >>= readIORef
    gateOn <- newIORef True >>= readIORef
    let rest = error "takeWhile forced the tail"
        prefix gate x = PreludeTakeWhile.prefixObservation (PreludeTakeWhile.observePrefix gate x rest)
    observe "stop or keep a lazy list prefix with a runtime gate"
        (prefix gateOff (error "disabled gate forced the head"), prefix gateOn True)
    prefixInput <- newIORef (False : True : error "prefix and suffix forced the rejected tail") >>= readIORef
    putStrLn ("recover a list from its checked prefix and suffix: "
              ++ PreludeDropWhileRuntime.showPrefixAndSuffix prefixInput)
    putStrLn ("prove both projections and rejoin an ordinary span: "
              ++ PreludeSpanRuntime.showSpan prefixInput)
    putStrLn ("prove guarded list equations at two concrete element types: "
              ++ ConcreteGuardedListProof.showConcreteGuardedLists)
    pivot <- newIORef True >>= readIORef
    let unused = error "comparison forced an unused payload"
    observe "select a payload with a checked runtime comparison"
        [ OrderingPayload.showComparison (not pivot) pivot (17, unused, unused)
        , OrderingPayload.showComparison pivot pivot (unused, True, unused)
        , OrderingPayload.showComparison pivot (not pivot) (unused, unused, 'G') ]
    let observeComparison :: (Bool -> Bool -> Ordering) -> Bool -> Bool -> Ordering
        observeComparison cmp x y = case CompareAt.decideCompare cmp x y of
            CompareAt.Less -> LT
            CompareAt.Equal -> EQ
            CompareAt.Greater -> GT
    observe "witness a runtime comparator at both of its arguments"
        [ observeComparison (flip compare) (not pivot) pivot
        , observeComparison (flip compare) pivot (not pivot)
        , observeComparison (flip compare) pivot pivot ]
    let familyComparison :: (CompareAt.Result 'False -> Bool -> Ordering) -> Bool -> Bool -> Ordering
        familyComparison cmp x y = case CompareAt.decideFamilyCompare cmp x y of
            CompareAt.Less -> LT
            CompareAt.Equal -> EQ
            CompareAt.Greater -> GT
    observe "apply the comparison selected by a type family"
        [ familyComparison (flip compare) (not pivot) pivot
        , familyComparison (flip compare) pivot (not pivot) ]
    let compareOptional x y = compare (maybe False id y) x
    observe "compare a constructed Just without forcing an unused field"
        [ CompareOptional.readJust compareOptional (not pivot) pivot
        , CompareOptional.readJust (\_ _ -> EQ) pivot (error "forced Just field") ]
    let keepComparison :: (CompareAt.Result 'False -> Bool -> Ordering) -> Bool -> Bool -> String
        keepComparison cmp x y = show (CompareAt.keepFamilyCompare cmp x y)
    putStrLn ("keep a comparison proof without forcing the converted callback: " ++
              keepComparison (error "forced comparator") (error "forced first argument")
                             (error "forced second argument"))
    observe "read an indexed payload through a rule for a converted callback"
        CallbackRulesClient.observation
    observe "read a payload with an absent polymorphic callback" CallbackRulesClient.absentObservation
    observe "use a checked cons rule without forcing the callback, head or tail"
        (ConsRuleMapProof.negateHead (Proxy @'[]), ConsRuleMapProof.lazySpine)
    zipLeft <- newIORef [True, False] >>= readIORef
    zipRight <- newIORef [False] >>= readIORef
    observe "zip two lists and prove their result length with an ordinary minimum"
        ( zip zipLeft zipRight
        , PreludeZipProof.firstPair
        , HaskLib.lengthZip zipLeft zipRight
        , PreludeZipWithLengthProof.shorterRight
        )
    zipCallback <- newIORef (&&) >>= readIORef
    let unusedCallback :: Bool -> Bool -> Bool
        unusedCallback = error "lengthZipWith forced its callback"
    observe "zipWith a runtime callback and prove the result length"
        ( zipWith zipCallback zipLeft zipRight
        , HaskLib.lengthZipWith zipCallback zipLeft zipRight
        , HaskLib.lengthZipWith unusedCallback zipLeft zipRight
        )
    let mapPair :: (Bool, Bool) -> Bool
        mapPair = Prelude.uncurry unusedCallback
    observe "prove the length after mapping over zipped pairs"
        (HaskLib.lengthMapZip mapPair zipLeft zipRight)
    beforeZip <- newIORef not >>= readIORef
    observe "prove map and zipWith length directly and with trans and congFirst"
        ( zipWith zipCallback (map beforeZip zipLeft) zipRight
        , HaskLib.lengthZipMap beforeZip zipCallback zipLeft zipRight
        , HaskLibClient.zipMapLaw beforeZip zipCallback zipLeft zipRight
        )
    observe "use the Prelude.zipWith cons equation with both tails unevaluated"
        PreludeZipWithProof.firstPair
    observe "compose two checked traversals without forcing the inner callback"
        (ConsRuleMapProof.twiceHead (Proxy @'[]), ConsRuleMapProof.composedHead (undefined : undefined))
    observe "collect row heads with an ordinary polymorphic callback"
        (OpenMapProof.firstRowHead (Proxy @'True) (Proxy @'Spine.Empty) (Proxy @'[]))
    cutoff <- newIORef (2 :: Int) >>= readIORef
    partitionInput <- newIORef [3, 1, 4, 2] >>= readIORef
    observe "regroup both partitions at the original length"
        (ListVectorClient.regroupObservation (> cutoff) partitionInput)
    case ListVectorClient.fromList partitionInput of
        Sigma.Pair total values ->
            let split = ConservedPartition.partitionVec (> cutoff) values
                select kept = ListVectorClient.choosePartition @Int (type total) kept split
                    (\n cells -> (natToInt n, toList cells))
            in observe "choose either partition at a runtime input length" (select True, select False)

-- | Complement one flag at the type level.
type family FlipFlag (b :: Bool) :: Bool where
    FlipFlag 'False = 'True
    FlipFlag 'True = 'False

-- | Apply an unmatchable type function to each list element.
type Map :: (a %U -> b) -> [a] -> [b]
type family Map f xs where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

-- | Equal callbacks give equal traversals before either traversal receives a list.
sameMapping :: (f :: Bool %U -> Bool) :~: g -> Map f :~: Map g
sameMapping Refl = Refl

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

-- | Check the three case equations and reflect their result code points.
certifiedCaseMappings :: [Int]
certifiedCaseMappings =
    case ( Refl :: UnicodeExport.Title '\x1c6' :~: '\x1c5'
         , Refl :: Data.Char.toUpper '\x1c6' :~: '\x1c4'
         , Refl :: UnicodeExport.Lower '\x1c4' :~: '\x1c6'
         ) of
        (Refl, Refl, Refl) ->
            map fromEnum
                [ charVal (Proxy @(UnicodeExport.Title '\x1c6'))
                , charVal (Proxy @(Data.Char.toUpper '\x1c6'))
                , charVal (Proxy @(UnicodeExport.Lower '\x1c4'))
                ]

-- | Check the specialized predicate equations and show the runtime results.
certifiedCharacterPredicate :: [Bool]
certifiedCharacterPredicate =
    case ( Refl :: CharacterExport.Flag 'x' :~: 'True
         , Refl :: CharacterExport.Flag 'é' :~: 'True
         , Refl :: CharacterExport.Flag '!' :~: 'False
         ) of
        (Refl, Refl, Refl) -> map CharacterCase.flag ['x', 'é', '!']

-- | Read the outcome from the decision constructor.
characterDecisionValue :: CharacterDecision.Decision predicate result -> Bool
characterDecisionValue CharacterDecision.Yes = True
characterDecisionValue CharacterDecision.No = False

type family MatchInt (i :: Int) :: () where
    MatchInt ('I# x) = '()

-- | Match a concrete machine integer in a closed family.
machineIntProof :: MatchInt ('I# 3#) :~: '()
machineIntProof = Refl

-- | The result type selects the first operand even when the argument types differ.
openLeft :: forall a b. a -> b -> const a b
openLeft x _ = x

-- | The argument pattern fixes the wildcard before GHC checks the result.
titleFromPattern :: Proxy _c -> Proxy (RetryCharacters.title _c)
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
-- Compare with S Z because the caller supplies the meaning of a term literal.
oneCellHead :: Num Nat => foreach (extent :: Nat) -> Maybe Char
oneCellHead extent =
    let count :: Nat
        count = 1 + extent
    in case sameNat count (S Z) of
         Just proof ->
             HaskLib.withEquality proof (Just (safeHead (replicateNative count 'x')))
         Nothing -> Nothing

-- | Return equality with the empty constructor after inspecting the vector.
emptyVectorProof :: forall a n. foreach (xs :: Vec a n) -> Maybe (xs :~~: ('VNil :: Vec a 'Z))
emptyVectorProof xs = case xs of
    VNil -> Just HRefl
    VCons _ _ -> Nothing

-- | Reverse the proof while the vector length is still an open index.
reverseEmptyProof :: forall a n. foreach (xs :: Vec a n) ->
    xs :~~: ('VNil :: Vec a 'Z) -> ('VNil :: Vec a 'Z) :~~: xs
reverseEmptyProof _ HRefl = HRefl

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
        split :: (Vec Bool n, Vec Bool m)
        split = splitAtVec n joined
        prefix :: Vec Bool n
        prefix = takeVec @m n joined
        suffix :: Vec Bool m
        suffix = dropVec n joined
    observe "runtime sum and appended vector" (natToInt (n + m), toList joined)
    observe "use ordinary vector append in a type"
        (Refl :: PackedVecLibrary.appendVec ('VCons 'True 'VNil) ('VCons 'False 'VNil)
              :~: 'VCons 'True ('VCons 'False 'VNil))
    observe "prove empty append preserves a runtime vector"
        (OrdinaryOpenVectorAppend.appendContents joined)
    observe "prove the first cell of append at runtime"
        (OrdinaryOpenVectorAppend.consContents False prefix suffix)
    zeroCut <- newIORef (0 :: Nat) >>= readIORef
    observe "prove empty append for two vectors at a shared runtime cut"
        (OrdinaryOpenVectorAppend.observeAppendPair zeroCut, OrdinaryOpenVectorAppend.observeAppendPair n)
    labels <- newIORef "abc" >>= readIORef
    flags <- newIORef [True, False] >>= readIORef
    putStrLn ("prove zip returns the smaller of two runtime lengths: " ++ zipReport labels flags)
    putStrLn ("count records with a header, skip records without one: " ++ headerCountReport flags)
    observe "split the appended vector at its first length"
        (toList (fst split), toList (snd split))
    rebuilt <- CertifiedSplitClient.splitObservation
    observe "split a vector with a proof of its original contents" rebuilt
    observe "split proof demand (zero ignores input, one ignores fields, one demands the cell)"
        =<< CertifiedSplitClient.checkDemand
    checkedContents <- CertifiedSplitClient.checkedObservation
    observe "check a runtime cut and prove the original contents" checkedContents
    observe "split two independent vectors at one checked cut" =<< CertifiedSplitClient.pairedObservation
    observe "split flags and characters at the same checked cut" =<< CertifiedSplitClient.mixedObservation
    finitePosition <- FinitePositionClient.observeFinitePosition
    observe "recover the checked predecessor of a finite position" finitePosition
    observe "pass zero and a written successor to a retained function"
        =<< FinitePositionClient.observeConstructedViews
    observe "open finite zero views with INLINE and INLINABLE"
        FinitePositionClient.observeZeroViews
    observe "take and drop at the first runtime length" (toList prefix, toList suffix)
    checkedInput <- newIORef [True, False] >>= readIORef
    let matchesEmpty input = case ListVectorClient.fromList input of
            Sigma.Pair _ xs -> case emptyVectorProof xs of
                Just proof -> case reverseEmptyProof xs proof of HRefl -> True
                Nothing -> False
    observe "reverse the proof that a runtime vector is empty"
        (matchesEmpty (drop 2 checkedInput), matchesEmpty checkedInput)
    pick <- newIORef QuantifiedIndex.PickBool >>= readIORef
    case QuantifiedIndex.transport pick (QuantifiedIndex.Carrier 41) of
        QuantifiedIndex.Carrier value ->
            observe "transport a GADT index with a quantified kind" value
    observe "check a runtime cut against a vector length"
        (ListVectorClient.checkedSplitObservation n checkedInput,
         ListVectorClient.checkedSplitObservation (S m) checkedInput)
    observe "pass an unevaluated vector at its branch-refined split width"
        ( checkCutInput Z (error "forced one-cell input" :: Vec Bool ('S 'Z))
        , checkCutInput (S Z) (error "forced two-cell input" :: Vec Bool ('S ('S 'Z))) )
    oneCell <- newIORef (VCons True VNil)
    proofFromIO <- ListVectorClient.checkedSplitIO
        (splitAtVecChecked (S Z) (S Z) <$> readIORef oneCell) (VCons False VNil)
    observe "use a split proof from IO in the next argument" proofFromIO
    case (Refl :: VectorSplitObservation.reassembleTwo ('VCons 'True ('VCons 'False 'VNil))
               :~: 'VCons 'True ('VCons 'False 'VNil)) of
        Refl -> observe "reassemble the same two vector elements"
            (toList (VectorSplitObservation.reassembleTwo (VCons True (VCons False VNil))))
    twoCells <- newIORef (VCons True (VCons False VNil))
    namedSplit <- NamedSplitClient.observeNamedSplitIO
        (splitAtVecChecked (S Z) (S (S Z)) <$> readIORef twoCells)
        (VCons False (VCons True VNil))
    observe "name an arbitrary retained suffix in a split pattern" namedSplit
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
    let annotated :: Vec Bool 2
        annotated = replicateNative (2 :: Nat) True
    observe "replicateNative (2 :: Nat)" (toList annotated)
    observe "explicit numeric conversion in an ordinary client" Ordinary.ordinaryConvert
    observe "use the selected (+) in a retained result" (toList (RetainedSumOperator.sumIndex n (S Z)))
    observe "append two and one using the selected (+)" AppendByMethodClient.appendTwoAndOneByMethod
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

-- | Keep the identity of the input when the cut refines its declared width.
checkCutInput :: foreach (cut :: Nat) -> foreach (xs :: Vec Bool (Add cut ('S 'Z))) -> Bool
checkCutInput cut xs = case atCut cut xs of Proxy -> True
  where
    atCut :: foreach (n :: Nat) -> foreach (v :: Vec Bool (Add n ('S 'Z))) -> Proxy v
    atCut Z v = atWidth @('S 'Z) v
    atCut (S k) v = atWidth @('S (Add k ('S 'Z))) v

    atWidth :: forall n. foreach (v :: Vec Bool n) -> Proxy v
    atWidth _ = Proxy

-- | Read a vector and its label through a named retained telescope.
describeVector :: Pair.Consumer (Vec Char n) (Const String) (String, String)
describeVector xs (Const label) = (label, toList xs)

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
    RetainedCharClient.observeProjection >>= observe "project each field using a local retained witness"
    RetainedCharClient.observeReplacement >>= observe "replace a vector after checking the stored lengths"
    let columns = SiblingColumns.Columns (S (S Z))
            (VCons 'r' (VCons 's' VNil))
            (VCons True (VCons False VNil))
    observe "zip two vectors at their shared stored length" (SiblingColumnsClient.readPairs columns)
    let named = Named.NamedColumns
            { Named.namedFlags = VCons True VNil
            , Named.storedLength = S Z
            , Named.namedLetters = VCons 'x' VNil
            }
    observe "zip two named fields at their stored length" (NamedSiblingColumnsClient.readPairs named)
    let flagsOnly = Named.NamedColumns
            { Named.storedLength = S Z
            , Named.namedFlags = VCons True VNil
            }
    observe "read a supplied field when its sibling is omitted"
        (toList (Named.namedFlags flagsOnly))
    let updated = named
            { Named.namedFlags = case Named.namedFlags named of
                VNil -> VNil
                VCons _ rest -> VCons False rest
            }
    observe "replace a named field at the original stored length"
        (natToInt (Named.storedLength updated), NamedSiblingColumnsClient.readPairs updated)
    let emptied = updated
            { Named.storedLength = Z
            , Named.namedLetters = VNil
            , Named.namedFlags = VNil
            }
    observe "replace the stored length and both named payloads"
        (natToInt (Named.storedLength emptied), NamedSiblingColumnsClient.readPairs emptied)
    let unpack :: forall len. TypeLambda.Fields '[Vec Char len, Vec Bool len] -> (String, [Bool])
        unpack (TypeLambda.Field xs (TypeLambda.Field ys TypeLambda.End)) = (toList xs, toList ys)
    observe "put two columns in a row indexed by the record length"
        (unpack (SiblingColumnRow.packRow columns))
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
    case (Refl :: Pair.Consumer (Vec Char 'Z) (Const String) String
               :~: (foreach (v :: Vec Char 'Z) -> Const String v -> String)) of
        Refl -> observe "expand a named consumer over an indexed vector" True
    let row = replicateNative n 'x'
    observe "name a dependent consumer at a runtime vector length"
        (describeVector row (Const "row"))
    observe "project the payload at the first field of a dependent pair"
        (natToInt (Pair.first chars), toList (Pair.second chars), getConst (Pair.second tag))
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
