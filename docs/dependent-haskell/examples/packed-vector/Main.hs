{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoPostfixOperators #-}

-- | Check source packages, filtering, and evaluation demand.
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary

-- | Construct a package from an ordinary runtime input.
makePacked :: Nat -> PackedVec Int
makePacked n = PackVec n (replicateNative @Int n 42)

-- | Keep the payload undefined at the exact runtime index.
lazyPacked :: Nat -> PackedVec Int
lazyPacked n = PackVec n bottomPayload

-- | Keep a cyclic payload undefined at the exact runtime index.
cyclicPacked :: Nat -> PackedVec Int
cyclicPacked n = PackVec n cyclicPayload

-- | Render both fields of a finite package.
renderPacked :: PackedVec Int -> String
renderPacked package = withPacked package (\n xs -> show (natToInt n, toList xs))

-- | Observe a finite length, vector and proof independently.
renderBounded :: BoundedVec Int limit -> String
renderBounded package = withBounded package (\n xs proof -> show (natToInt n, toList xs, proofDepth proof))

-- | Remove the bound proof without evaluating it.
forgetBounded :: BoundedVec a limit -> PackedVec a
forgetBounded (PackBounded n xs proof) = PackVec n xs

-- | Count successor steps in a finite bound proof.
proofDepth :: LE n m -> Int
proofDepth LEZ = 0
proofDepth (LES proof) = 1 + proofDepth proof

-- | Read one payload constructor without evaluating the predecessor length.
firstPacked :: PackedVec Int -> Int
firstPacked package =
    withPacked
        package
        ( \n xs -> case xs of
            VNil -> 0
            VCons x rest -> x
        )

-- | Send all finite inputs through the same runtime worker.
{-# NOINLINE drive #-}
drive :: Nat -> IO String
drive input = do
    cell <- newIORef input
    n <- readIORef cell
    pure (renderPacked (repack (makePacked n)))

-- | Filter with a cutoff read at runtime.
{-# NOINLINE driveFilter #-}
driveFilter :: Int -> IO String
driveFilter input = do
    cell <- newIORef input
    cutoff <- readIORef cell
    pure (renderPacked (filterPacked (> cutoff) samplePacked))

-- | Filter at a runtime cutoff and observe the constructed bound proof.
{-# NOINLINE driveBoundedFilter #-}
driveBoundedFilter :: Int -> IO String
driveBoundedFilter input = do
    cell <- newIORef input
    cutoff <- readIORef cell
    pure (withPacked samplePacked (\n xs -> renderBounded (filterBounded (> cutoff) xs)))

-- | Select nested vector and proof alternatives with one runtime cutoff.
{-# NOINLINE drivePatterns #-}
drivePatterns :: Int -> IO String
drivePatterns input = do
    cell <- newIORef input
    cutoff <- readIORef cell
    let filtered = filterPacked (> cutoff) samplePacked
        bounded = withPacked samplePacked (\n xs -> proofDepthBounded (filterBounded (> cutoff) xs))
    pure (show (headPacked filtered, bounded))

-- | Render the returned head and the stored length and vector of its tail.
renderUncons :: PackedVec Int -> String
renderUncons package = case unconsPacked package of
    Nothing -> "Nothing"
    Just (x, rest) -> show x ++ ":" ++ renderPacked rest

-- | Check that the returned tail stores the actual runtime predecessor.
{-# NOINLINE driveUncons #-}
driveUncons :: Nat -> IO String
driveUncons input = do
    cell <- newIORef input
    n <- readIORef cell
    pure (renderUncons (makePacked n))

-- | Drain a filtered package with distinct elements and a runtime cutoff.
{-# NOINLINE driveDrainFilter #-}
driveDrainFilter :: Int -> IO String
driveDrainFilter input = do
    cell <- newIORef input
    cutoff <- readIORef cell
    pure (show (toListPacked (filterPacked (> cutoff) samplePacked)))

-- | Render a successful alignment's stored length and paired elements.
renderZip :: Maybe (PackedVec (Int, Int)) -> String
renderZip Nothing = "Nothing"
renderZip (Just package) = "Just " ++ withPacked package (\n xs -> show (natToInt n, toList xs))

-- | Keep the nested-case equality algorithm as an independent source reference.
sameNatByCase :: foreach (n :: Nat) -> foreach (m :: Nat) -> Maybe (n :~: m)
sameNatByCase n m = case n of
    Z -> case m of
        Z -> Just Refl
        S right -> Nothing
    S left -> case m of
        Z -> Nothing
        S right -> case sameNatByCase left right of
            Nothing -> Nothing
            Just Refl -> Just Refl

-- | Preserve payloads on both sides of the second retained index column.
matrixPayload :: foreach (n :: Nat) -> Vec Int n -> foreach (m :: Nat) -> Vec Int m -> (Vec Int n, Vec Int m)
matrixPayload Z leftZeroZero Z rightZeroZero = (leftZeroZero, rightZeroZero)
matrixPayload Z leftZeroSuccessor (S rightPredecessor) rightZeroSuccessor = (leftZeroSuccessor, rightZeroSuccessor)
matrixPayload (S leftPredecessor) leftSuccessorZero Z rightSuccessorZero = (leftSuccessorZero, rightSuccessorZero)
matrixPayload (S firstPredecessor) leftSuccessorSuccessor (S secondPredecessor) rightSuccessorSuccessor = (leftSuccessorSuccessor, rightSuccessorSuccessor)

-- | Read independent lengths and force each successful equality proof.
{-# NOINLINE driveIndexMatrix #-}
driveIndexMatrix :: Nat -> Nat -> IO (String, String)
driveIndexMatrix left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    n <- readIORef leftCell
    m <- readIORef rightCell
    let actual = case sameNat n m of
            Nothing -> "different"
            Just Refl -> "same"
        reference = case sameNatByCase n m of
            Nothing -> "different"
            Just Refl -> "same"
    pure (actual, reference)

-- | Compare finite results and evaluation demand against nested source cases.
runIndexMatrixChecks :: IO ()
runIndexMatrixChecks = do
    let inputs = [Z, S Z, S (S Z), S (S (S Z))]
        expected =
            [ "same"
            , "different"
            , "different"
            , "different"
            , "different"
            , "same"
            , "different"
            , "different"
            , "different"
            , "different"
            , "same"
            , "different"
            , "different"
            , "different"
            , "different"
            , "same"
            ]
    forM_ (zip [(n, m) | n <- inputs, m <- inputs] expected) $ \((n, m), wanted) -> do
        (actual, reference) <- driveIndexMatrix n m
        let label = "runtime equality matrix " ++ show (natToInt n, natToInt m)
        assertEqual (label ++ " reference") wanted reference
        assertEqual label wanted actual
        putStrLn (label ++ ": " ++ actual)
    checkWhnf (sameNatByCase bottomLength)
    assertWhnf "matrix partial ignores bottom index" (sameNat bottomLength)
    checkWhnf (sameNatByCase cyclicLength)
    assertWhnf "matrix partial ignores cyclic index" (sameNat cyclicLength)
    check_marker
        "matrix first index before second"
        "packed-bottom-length"
        (evaluate (sameNatByCase bottomLength bottomRightLength) >> pure ())
        (evaluate (sameNat bottomLength bottomRightLength) >> pure ())
    check_marker
        "matrix zero demands second index"
        "equality-right-length"
        (evaluate (sameNatByCase Z bottomRightLength) >> pure ())
        (evaluate (sameNat Z bottomRightLength) >> pure ())
    let zeroSuccessor = case sameNat Z (S bottomRightLength) of
            Nothing -> True
            Just _ -> False
        zeroSuccessorReference = case sameNatByCase Z (S bottomRightLength) of
            Nothing -> True
            Just _ -> False
    assertEqual "matrix zero successor reference" True zeroSuccessorReference
    assertEqual "matrix zero successor" True zeroSuccessor
    putStrLn "matrix zero successor ignores predecessor: different"
    let successorZero = case sameNat (S bottomLength) Z of
            Nothing -> True
            Just _ -> False
        successorZeroReference = case sameNatByCase (S bottomLength) Z of
            Nothing -> True
            Just _ -> False
    assertEqual "matrix successor zero reference" True successorZeroReference
    assertEqual "matrix successor zero" True successorZero
    putStrLn "matrix successor zero ignores predecessor: different"
    check_marker
        "matrix recursive left before right"
        "packed-bottom-length"
        (evaluate (sameNatByCase (S bottomLength) (S bottomRightLength)) >> pure ())
        (evaluate (sameNat (S bottomLength) (S bottomRightLength)) >> pure ())
    check_marker
        "matrix recursive right demand"
        "equality-right-length"
        (evaluate (sameNatByCase (S Z) (S bottomRightLength)) >> pure ())
        (evaluate (sameNat (S Z) (S bottomRightLength)) >> pure ())
    check_marker
        "matrix shared bottom still demanded"
        "packed-bottom-length"
        (evaluate (sameNatByCase bottomLength bottomLength) >> pure ())
        (evaluate (sameNat bottomLength bottomLength) >> pure ())
    let cyclicMismatch = case sameNat cyclicLength Z of
            Nothing -> True
            Just _ -> False
        cyclicMismatchReference = case sameNatByCase cyclicLength Z of
            Nothing -> True
            Just _ -> False
    assertEqual "matrix cyclic mismatch reference" True cyclicMismatchReference
    assertEqual "matrix cyclic mismatch" True cyclicMismatch
    putStrLn "matrix cyclic successor zero mismatch: different"
    forM_
        [ (Z, Z, "([],[])")
        , (Z, S Z, "([],[9])")
        , (S Z, Z, "([7],[])")
        , (S Z, S Z, "([7],[9])")
        ]
        $ \(leftInput, rightInput, expected) -> do
            leftCell <- newIORef leftInput
            rightCell <- newIORef rightInput
            n <- readIORef leftCell
            m <- readIORef rightCell
            case matrixPayload n (replicateNative @Int n 7) m (replicateNative @Int m 9) of
                (xs, ys) -> do
                    let actual = show (toList xs, toList ys)
                        label = "runtime payload matrix " ++ show (natToInt n, natToInt m)
                    assertEqual label expected actual
                    putStrLn (label ++ ": " ++ actual)
    assertWhnf "payload matrix partial ignores fields" (matrixPayload bottomLength bottomPayload bottomRightLength)
    expectMarker "payload matrix first index before payloads" "packed-bottom-length" $
        evaluate (matrixPayload bottomLength bottomPayload bottomRightLength bottomPayload)
  where
    check_marker :: String -> String -> IO () -> IO () -> IO ()
    check_marker label marker reference actual = do
        result <- try @ErrorCall reference
        case result of
            Left exception
                | marker `isInfixOf` displayException exception ->
                    expectMarker label marker actual
            Left exception -> fail (label ++ ": unexpected reference error: " ++ displayException exception)
            Right () -> fail (label ++ ": reference did not raise " ++ marker)

-- | Read two independent lengths before comparing and pairing their vectors.
{-# NOINLINE driveEqualityZip #-}
driveEqualityZip :: Nat -> Nat -> IO String
driveEqualityZip left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    n <- readIORef leftCell
    m <- readIORef rightCell
    let equality = case sameNat n m of
            Nothing -> "different"
            Just Refl -> "same"
        leftPackage = PackVec n (replicateNative @Int n 42)
        rightPackage = PackVec m (replicateNative @Int m 7)
    pure (equality ++ " " ++ renderZip (zipPacked leftPackage rightPackage))

-- | Compare two independently filtered lengths and preserve element order.
{-# NOINLINE driveFilteredZip #-}
driveFilteredZip :: Int -> Int -> IO String
driveFilteredZip left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    leftCutoff <- readIORef leftCell
    rightCutoff <- readIORef rightCell
    pure (renderZip (zipPacked (filterPacked (> leftCutoff) samplePacked) (filterPacked (> rightCutoff) rightSamplePacked)))

-- | Read independent runtime lengths and append their packaged vectors.
{-# NOINLINE driveAppend #-}
driveAppend :: Nat -> Nat -> IO String
driveAppend left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    n <- readIORef leftCell
    m <- readIORef rightCell
    pure (renderPacked (appendPacked (PackVec n (replicateNative @Int n 42)) (PackVec m (replicateNative @Int m 7))))

-- | Append vectors selected by independent runtime predicates.
{-# NOINLINE driveFilteredAppend #-}
driveFilteredAppend :: Int -> Int -> IO String
driveFilteredAppend left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    leftCutoff <- readIORef leftCell
    rightCutoff <- readIORef rightCell
    pure (renderPacked (appendPacked (filterPacked (> leftCutoff) samplePacked) (filterPacked (> rightCutoff) rightSamplePacked)))

-- | Store undefined equality evidence without evaluating it.
lazySum :: foreach (n :: Nat) -> Sum n Z
lazySum n = Sum n bottomEquality

-- | Reverse a package constructed from a length read at runtime.
{-# NOINLINE driveReverse #-}
driveReverse :: Nat -> IO String
driveReverse input = do
    cell <- newIORef input
    n <- readIORef cell
    pure (renderPacked (reversePacked (makePacked n)))

-- | Reverse distinct elements selected by a runtime predicate.
{-# NOINLINE driveFilteredReverse #-}
driveFilteredReverse :: Int -> IO String
driveFilteredReverse input = do
    cell <- newIORef input
    cutoff <- readIORef cell
    pure (renderPacked (reversePacked (filterPacked (> cutoff) samplePacked)))

-- | Reverse onto an accumulator with an independently read length.
{-# NOINLINE driveReverseAppend #-}
driveReverseAppend :: Int -> Nat -> IO String
driveReverseAppend cutoffInput lengthInput = do
    cutoffCell <- newIORef cutoffInput
    lengthCell <- newIORef lengthInput
    cutoff <- readIORef cutoffCell
    m <- readIORef lengthCell
    pure
        ( withPacked
            (filterPacked (> cutoff) samplePacked)
            ( \n xs ->
                case sumNat n m of
                    Sum p Refl -> renderPacked (PackVec p (reverseAppendNative @Int n m xs (replicateNative @Int m 7)))
            )
        )

-- | Demand both arithmetic proofs at independently read runtime indices.
{-# NOINLINE driveReverseProofs #-}
driveReverseProofs :: Nat -> Nat -> IO String
driveReverseProofs left right = do
    leftCell <- newIORef left
    rightCell <- newIORef right
    n <- readIORef leftCell
    m <- readIORef rightCell
    case rightZero n of
        Refl -> case addSucc n m of
            Refl -> pure "Refl"

-- | A named error for demand of the ordinary equality field.
bottomEquality :: forall n m. n :~: m
bottomEquality = error "append-bottom-proof"

-- | Demand an explicit empty vector pattern after matching its stored index.
zeroVector :: PackedVec a -> Bool
zeroVector (PackVec Z VNil) = True
zeroVector (PackVec (S k) xs) = False

-- | Match a stored index and vector before a later Boolean argument.
indexThenFlag :: PackedVec Int -> Bool -> Int
indexThenFlag (PackVec Z _) False = 0
indexThenFlag (PackVec (S k) (VCons x xs)) True = x
indexThenFlag (PackVec n xs) flag = 42

-- | A view whose demand has a distinct error marker.
viewFlag :: Bool -> Bool
viewFlag flag = error "pattern-view"

-- | Keep index discrimination before a view in the next argument.
indexBeforeView :: PackedVec Int -> Bool -> Int
indexBeforeView (PackVec Z _) (viewFlag -> True) = 0
indexBeforeView (PackVec (S k) xs) flag = 1

-- | Preserve guarded-row failure with separate retained field identities.
guardedByIndex :: PackedVec Int -> Bool -> Int
guardedByIndex (PackVec (S k) xs) takeFirst
    | takeFirst = 42
guardedByIndex (PackVec Z _) takeFirst = 0
guardedByIndex (PackVec n xs) takeFirst = 99

-- | Store two runtime lengths and equality evidence between them.
data EqualLengths where
    EqualLengths :: foreach (n :: Nat) -> foreach (m :: Nat) -> (n :~: m) -> EqualLengths

-- | Store one input in both retained fields with reflexivity evidence.
makeEqualLengths :: Nat -> EqualLengths
makeEqualLengths n = EqualLengths n n Refl

-- | Leave the equality field undefined at two independently stored indices.
lazyEqualLengths :: Nat -> Nat -> EqualLengths
lazyEqualLengths n m = EqualLengths n m bottomEquality

-- | Use two constructor refinements and their shared proof to align tails.
equalTails :: EqualLengths -> PackedVec Int
equalTails (EqualLengths Z Z Refl) = PackVec Z VNil
equalTails (EqualLengths (S k) (S j) Refl) = PackVec k (replicateNative @Int j 42)

-- | Send both equality patterns through one runtime input worker.
{-# NOINLINE driveEqualTails #-}
driveEqualTails :: Nat -> IO String
driveEqualTails input = do
    cell <- newIORef input
    n <- readIORef cell
    pure (renderPacked (equalTails (makeEqualLengths n)))

-- | Match a stored sum constructor without inspecting its proof or predecessor.
sumTag :: Sum n m -> Int
sumTag (Sum Z proof) = 0
sumTag (Sum (S k) proof) = 1

-- | A second ordered vector with different element values.
rightSamplePacked :: PackedVec Int
rightSamplePacked = PackVec (S (S (S Z))) (VCons 10 (VCons 20 (VCons 30 VNil)))

-- | Select one pair without evaluating either of its components.
firstPair :: PackedVec (Int, Int) -> (Int, Int)
firstPair package = case headPacked package of
    Nothing -> error "expected a nonempty aligned vector"
    Just pair -> pair

-- | Keep two vector errors distinct during alignment.
leftVector :: forall (n :: Nat). Vec Int n
leftVector = error "zip-left-vector"

-- | Keep the right vector error distinct from the left vector error.
rightVector :: forall (n :: Nat). Vec Int n
rightVector = error "zip-right-vector"

-- | A distinct error for the right runtime length.
bottomRightLength :: Nat
bottomRightLength = error "equality-right-length"

-- | Exercise both guarded rows after reading the guard at runtime.
{-# NOINLINE driveGuardedRepack #-}
driveGuardedRepack :: Bool -> IO String
driveGuardedRepack input = do
    cell <- newIORef input
    takeFirst <- readIORef cell
    pure (renderPacked (guardedRepack takeFirst (makePacked (S (S Z)))))

-- | Let an earlier guarded row ignore an undefined payload.
earlyCatchall :: Bool -> PackedVec Int -> Int
earlyCatchall takeFirst (PackVec n xs)
    | takeFirst = 42
earlyCatchall takeFirst (PackVec m (VCons x xs)) = x
earlyCatchall takeFirst (PackVec k VNil) = 0

-- | Demand the payload before considering the guarded catchall row.
payloadFirst :: Bool -> PackedVec Int -> Int
payloadFirst takeFirst (PackVec n (VCons x xs)) = x
payloadFirst takeFirst (PackVec m ys)
    | takeFirst = 42
payloadFirst takeFirst (PackVec k VNil) = 0

-- | Store three independently undefined fields at one runtime index.
undefinedBoundedFields :: Nat -> BoundedVec Int (S Z)
undefinedBoundedFields n = PackBounded n bottomPayload bottomProof

{-# NOINLINE bottomProof #-}

-- | A named error in a bound proof.
bottomProof :: forall n m. LE n m
bottomProof = error "bounded-bottom-proof"

-- | Preserve a cyclic proof without evaluating it.
cyclicProof :: forall n m. LE n m
cyclicProof = cyclicProof

-- | Keep a valid vector independent of its undefined proof.
lazyProofBounded :: BoundedVec Int (S Z)
lazyProofBounded = PackBounded (S Z) (VCons 42 VNil) bottomProof

-- | Keep the proof constructor defined while its recursive proof is bottom.
partialProofBounded :: BoundedVec Int (S (S Z))
partialProofBounded = PackBounded (S (S Z)) (VCons 1 (VCons 2 VNil)) (LES bottomProof)

-- | Delay the proof transformation used by the rejected filter branch.
weakenedProofBounded :: BoundedVec Int (S (S Z))
weakenedProofBounded = PackBounded (S Z) (VCons 42 VNil) (weakenRight bottomProof)

-- | A finite input with three distinct elements.
samplePacked :: PackedVec Int
samplePacked = PackVec (S (S (S Z))) (VCons 1 (VCons 2 (VCons 3 VNil)))

-- | Test payload demand independently of predicate and length demand.
singletonBottom :: PackedVec Int
singletonBottom = PackVec (S Z) (VCons bottomElement VNil)

{-# NOINLINE bottomElement #-}

-- | A named error in a stored element.
bottomElement :: Int
bottomElement = error "packed-filter-bottom-element"

{-# NOINLINE bottomPredicate #-}

-- | A bottom function that an empty input must not evaluate.
bottomPredicate :: Int -> Bool
bottomPredicate = error "packed-filter-bottom-predicate"

{-# NOINLINE bottomOperator #-}

-- | Check section closure demand independently of operator demand.
bottomOperator :: Int -> Int -> Bool
bottomOperator = error "packed-section-bottom-operator"

-- | Check a deterministic result.
assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
    unless (expected == actual) $
        fail (label ++ ": expected " ++ show expected ++ ", got " ++ show actual)

-- | Evaluate a value without adding a printed observation.
checkWhnf :: a -> IO ()
checkWhnf value = do
    _ <- evaluate value
    pure ()

-- | Check constructor or function weak-head normal form.
assertWhnf :: String -> a -> IO ()
assertWhnf label value = do
    _ <- evaluate value
    putStrLn (label ++ ": ok")

-- | Accept only the named error from a demanded value.
expectMarker :: String -> String -> IO a -> IO ()
expectMarker label marker action = do
    result <- try @ErrorCall (action >> pure ())
    case result of
        Left exception
            | marker `isInfixOf` displayException exception ->
                putStrLn (label ++ ": " ++ marker)
        Left exception -> fail (label ++ ": unexpected error: " ++ displayException exception)
        Right () -> fail (label ++ ": expected " ++ marker)

{-# NOINLINE bottomLength #-}

-- | A named error for length strictness checks.
bottomLength :: Nat
bottomLength = error "packed-bottom-length"

{-# NOINLINE cyclicLength #-}

-- | A runtime length with an infinite successor chain.
cyclicLength :: Nat
cyclicLength = S cyclicLength

-- | A payload error available at any erased length.
bottomPayload :: forall (n :: Nat). Vec Int n
bottomPayload = error "packed-bottom-payload"

-- | A cyclic bottom available at any erased length. Never demand this value.
cyclicPayload :: forall (n :: Nat). Vec Int n
cyclicPayload = cyclicPayload

-- | A package error for outer constructor strictness checks.
bottomPackage :: PackedVec Int
bottomPackage = error "packed-bottom-package"

-- | Keep two later runtime indices in separate lazy packages.
forwardLater :: foreach (n :: Nat) -> Vec Int n -> foreach (m :: Nat) -> foreach (k :: Nat) -> Vec Int m -> Vec Int k -> (PackedVec Int, PackedVec Int, Int)
forwardLater Z firstPayload m k middlePayload lastPayload =
    let right () = PackVec k lastPayload
        shadow :: Int -> Int
        shadow m = m + 1
     in (left (), right (), shadow 41)
  where
    left () = PackVec m middlePayload
forwardLater (S predecessor) firstPayload middle final middlePayload lastPayload =
    let left () = PackVec middle middlePayload
        shadow :: Int -> Int
        shadow middle = middle + 1
     in (left (), right (), shadow 41)
  where
    right () = PackVec final lastPayload

-- | Use the equality context between two retained argument positions.
keepWithLaterEvidence :: foreach (n :: Nat) -> foreach (m :: Nat) -> (n ~ m) => foreach (k :: Nat) -> Vec Int m -> Vec Int k -> (Vec Int n, Vec Int k)
keepWithLaterEvidence Z m k xs ys = (xs, ys)
keepWithLaterEvidence (S predecessor) middle final xs ys = (xs, ys)

-- | Keep a leading class context before a retained argument telescope.
leadingContext :: forall a. (Show a) => foreach (n :: Nat) -> Vec a n -> foreach (m :: Nat) -> Vec a m -> String
leadingContext Z xs right ys = show (toList xs, toList ys)
leadingContext (S predecessor) xs later ys = show (toList xs, toList ys)

-- | Read independent indices before checking both forwarded packages.
{-# NOINLINE driveLaterIndices #-}
driveLaterIndices :: Nat -> Nat -> Nat -> IO String
driveLaterIndices first middle final = do
    firstCell <- newIORef first
    middleCell <- newIORef middle
    finalCell <- newIORef final
    n <- readIORef firstCell
    m <- readIORef middleCell
    k <- readIORef finalCell
    case forwardLater n (replicateNative @Int n 42) m k (replicateNative @Int m 7) (replicateNative @Int k 9) of
        (left, right, marker) -> pure (renderPacked left ++ "|" ++ renderPacked right ++ "|" ++ show marker)

-- | Supply equality evidence for independently read equal indices.
{-# NOINLINE driveLaterEvidence #-}
driveLaterEvidence :: Nat -> Nat -> Nat -> IO String
driveLaterEvidence first middle final = do
    firstCell <- newIORef first
    middleCell <- newIORef middle
    finalCell <- newIORef final
    n <- readIORef firstCell
    m <- readIORef middleCell
    k <- readIORef finalCell
    case sameNat n m of
        Nothing -> fail "later evidence inputs differ"
        Just Refl -> case keepWithLaterEvidence n m k (replicateNative @Int m 7) (replicateNative @Int k 9) of
            (left, right) -> pure (show (toList left, toList right))

-- | Check shared later indices, dictionary order and evaluation demand.
runLaterIndexChecks :: IO ()
runLaterIndexChecks = do
    forM_
        [ (Z, S (S Z), S (S (S Z)), "(2,[7,7])|(3,[9,9,9])|42")
        , (S Z, S (S (S Z)), Z, "(3,[7,7,7])|(0,[])|42")
        ]
        $ \(n, m, k, expected) -> do
            actual <- driveLaterIndices n m k
            assertEqual "runtime later indices" expected actual
            putStrLn ("runtime later indices: " ++ actual)
    forM_
        [ (Z, Z, S (S Z), "([],[9,9])")
        , (S (S Z), S (S Z), S (S (S Z)), "([7,7],[9,9,9])")
        ]
        $ \(n, m, k, expected) -> do
            actual <- driveLaterEvidence n m k
            assertEqual "runtime later evidence" expected actual
            putStrLn ("runtime later evidence: " ++ actual)
    assertWhnf "later partial ignores first index" (forwardLater bottomLength)
    assertWhnf "later partial ignores cyclic first index" (forwardLater cyclicLength)
    assertWhnf "later partial ignores all supplied fields" (forwardLater bottomLength leftVector bottomRightLength cyclicLength)
    assertWhnf "later tuple ignores later fields" (forwardLater Z leftVector bottomLength bottomRightLength leftVector rightVector)
    assertWhnf
        "later left package ignores partial index"
        (case forwardLater (S bottomLength) leftVector bottomLength bottomRightLength leftVector rightVector of (left, right, marker) -> left)
    assertWhnf
        "later right package ignores cyclic first index"
        (case forwardLater cyclicLength leftVector bottomLength bottomRightLength leftVector rightVector of (left, right, marker) -> right)
    expectMarker "later first index before payloads" "packed-bottom-length" $
        evaluate (forwardLater bottomLength leftVector bottomRightLength cyclicLength leftVector rightVector)
    expectMarker "later left stored index demand" "packed-bottom-length" $
        evaluate (case forwardLater Z leftVector bottomLength bottomRightLength leftVector rightVector of (left, right, marker) -> withPacked left (\n xs -> natToInt n))
    expectMarker "later right stored index demand" "equality-right-length" $
        evaluate (case forwardLater (S Z) leftVector bottomLength bottomRightLength leftVector rightVector of (left, right, marker) -> withPacked right (\n xs -> natToInt n))
    expectMarker "later left payload demand" "zip-left-vector" $
        evaluate (case forwardLater Z leftVector (S (S Z)) bottomRightLength leftVector rightVector of (left, right, marker) -> firstPacked left)
    expectMarker "later right payload demand" "zip-right-vector" $
        evaluate (case forwardLater (S Z) leftVector bottomLength (S (S Z)) leftVector rightVector of (left, right, marker) -> firstPacked right)
    assertEqual
        "later ordinary shadowing"
        42
        (case forwardLater Z leftVector bottomLength bottomRightLength leftVector rightVector of (left, right, marker) -> marker)
    putStrLn "later ordinary shadowing: 42"
    assertWhnf "later evidence partial ignores indices" (keepWithLaterEvidence bottomLength bottomLength)
    expectMarker "later evidence first index before payloads" "packed-bottom-length" $
        evaluate (keepWithLaterEvidence bottomLength bottomLength bottomRightLength leftVector rightVector)

    forM_
        [(Z, "([],[7])"), (S (S Z), "([42,42],[7])")]
        $ \(input, expected) -> do
            cell <- newIORef input
            n <- readIORef cell
            let m = S Z
                actual = leadingContext @Int n (replicateNative @Int n 42) m (replicateNative @Int m 7)
            assertEqual "runtime leading context" expected actual
            putStrLn ("runtime leading context: " ++ actual)

-- | Run finite observations and bounded strictness checks.
main :: IO ()
main = do
    forM_
        [ (Z, "(0,[])")
        , (S (S Z), "(2,[42,42])")
        , (S (S (S Z)), "(3,[42,42,42])")
        ]
        $ \(input, expected) -> do
            actual <- drive input
            assertEqual "runtime package" expected actual
            putStrLn ("runtime package: " ++ actual)
    assertWhnf "constructor with bottom fields" (lazyPacked bottomLength)
    assertWhnf "repack with bottom fields" (repack (lazyPacked bottomLength))
    assertWhnf "repack with cyclic fields" (repack (cyclicPacked cyclicLength))
    assertEqual "ignore bottom fields" 42 (ignorePacked (lazyPacked bottomLength))
    putStrLn "ignore bottom fields: 42"
    assertEqual "ignore cyclic fields" 42 (withPacked (cyclicPacked cyclicLength) (\n xs -> 42 :: Int))
    putStrLn "ignore cyclic fields: 42"
    assertEqual "partial length payload head" 42 (firstPacked (makePacked (S bottomLength)))
    putStrLn "partial length payload head: 42"
    assertEqual "cyclic length payload head" 42 (firstPacked (makePacked cyclicLength))
    putStrLn "cyclic length payload head: 42"
    expectMarker "demand payload before length" "packed-bottom-payload" $
        evaluate (withPacked (lazyPacked bottomLength) (\n xs -> sum (toList xs)))
    expectMarker "demand stored length" "packed-bottom-length" $
        evaluate (withPacked (lazyPacked bottomLength) (\n xs -> natToInt n))
    expectMarker "ignore bottom package" "packed-bottom-package" $
        evaluate (ignorePacked bottomPackage)
    expectMarker "repack bottom package" "packed-bottom-package" $
        evaluate (repack bottomPackage)
    assertEqual "unused whole package" 42 (const (42 :: Int) bottomPackage)
    putStrLn "unused whole package: 42"
    forM_
        [ (0, "(3,[1,2,3])")
        , (1, "(2,[2,3])")
        , (3, "(0,[])")
        ]
        $ \(cutoff, expected) -> do
            actual <- driveFilter cutoff
            assertEqual "runtime filter" expected actual
            putStrLn ("runtime filter: " ++ actual)
    let empty = filterPacked bottomPredicate (makePacked Z)
        rejected = filterPacked (const False) singletonBottom
        retained = filterPacked (const True) singletonBottom
    assertEqual "filter empty ignores predicate" "(0,[])" (renderPacked empty)
    putStrLn "filter empty ignores predicate: (0,[])"
    assertEqual "filter rejects bottom element" "(0,[])" (renderPacked rejected)
    putStrLn "filter rejects bottom element: (0,[])"
    assertWhnf "filter retains bottom element" retained
    assertEqual "filter retained length" 1 (withPacked retained (\n xs -> natToInt n))
    putStrLn "filter retained length: 1"
    expectMarker "filter demands retained element" "packed-filter-bottom-element" $
        evaluate (firstPacked retained)
    expectMarker "filter payload before length" "packed-bottom-payload" $
        evaluate (filterPacked (const True) (lazyPacked bottomLength))
    expectMarker "filter predicate before tail" "packed-filter-bottom-predicate" $
        evaluate (filterPacked bottomPredicate (makePacked (S bottomLength)))
    expectMarker "filter scans partial tail" "packed-bottom-length" $
        evaluate (filterPacked (const False) (makePacked (S bottomLength)))
    expectMarker "filter demands package" "packed-bottom-package" $
        evaluate (filterPacked (const True) bottomPackage)
    assertEqual "unused filtered package" 42 (const (42 :: Int) (filterPacked bottomPredicate bottomPackage))
    putStrLn "unused filtered package: 42"
    assertWhnf "left section with bottom operator" (1 `bottomOperator`)
    assertWhnf "right section with bottom operator" (`bottomOperator` 1)
    assertWhnf "left section with bottom operand" (bottomElement +)
    assertWhnf "right section with bottom operand" (+ bottomElement)
    expectMarker "applied left section demands operator" "packed-section-bottom-operator" $
        evaluate ((1 `bottomOperator`) 2)
    expectMarker "applied right section demands operator" "packed-section-bottom-operator" $
        evaluate ((`bottomOperator` 1) 2)
    expectMarker "applied left section demands operand" "packed-filter-bottom-element" $
        evaluate ((bottomElement +) 0)
    expectMarker "applied right section demands operand" "packed-filter-bottom-element" $
        evaluate ((+ bottomElement) 0)
    forM_
        [ (0, "(3,[1,2,3],3)")
        , (1, "(2,[2,3],2)")
        , (3, "(0,[],0)")
        ]
        $ \(cutoff, expected) -> do
            actual <- driveBoundedFilter cutoff
            assertEqual "runtime bounded filter" expected actual
            putStrLn ("runtime bounded filter: " ++ actual)
    let boundedEmpty = filterBounded bottomPredicate VNil
        boundedRejected = filterBounded (const False) (VCons bottomElement VNil)
        boundedRetained = filterBounded (const True) (VCons bottomElement VNil)
        cyclicBounded = PackBounded (S Z) (VCons 42 VNil) cyclicProof :: BoundedVec Int (S Z)
    assertEqual "bounded empty ignores predicate" "(0,[],0)" (renderBounded boundedEmpty)
    putStrLn "bounded empty ignores predicate: (0,[],0)"
    assertEqual "bounded rejects bottom element" "(0,[],0)" (renderBounded boundedRejected)
    putStrLn "bounded rejects bottom element: (0,[],0)"
    assertWhnf "bounded retains bottom element" boundedRetained
    assertEqual "bounded retained proof depth" 1 (withBounded boundedRetained (\n xs proof -> proofDepth proof))
    putStrLn "bounded retained proof depth: 1"
    assertEqual "bounded retained length" 1 (withBounded boundedRetained (\n xs proof -> natToInt n))
    putStrLn "bounded retained length: 1"
    expectMarker "bounded demands retained element" "packed-filter-bottom-element" $
        evaluate (firstPacked (forgetBounded boundedRetained))
    assertWhnf "bounded constructor with bottom proof" lazyProofBounded
    assertEqual "bounded length ignores proof" 1 (withBounded lazyProofBounded (\n xs proof -> natToInt n))
    putStrLn "bounded length ignores proof: 1"
    assertEqual "bounded vector ignores proof" "(1,[42])" (renderPacked (forgetBounded lazyProofBounded))
    putStrLn "bounded vector ignores proof: (1,[42])"
    expectMarker "bounded demands proof" "bounded-bottom-proof" $
        withBounded lazyProofBounded (\n xs proof -> evaluate proof >> pure ())
    withBounded partialProofBounded (\n xs proof -> assertWhnf "bounded partial proof constructor" proof)
    expectMarker "bounded demands recursive proof" "bounded-bottom-proof" $
        evaluate (withBounded partialProofBounded (\n xs proof -> proofDepth proof))
    assertWhnf "bounded constructor with weakened proof" weakenedProofBounded
    assertEqual "bounded vector ignores weakened proof" 42 (firstPacked (forgetBounded weakenedProofBounded))
    putStrLn "bounded vector ignores weakened proof: 42"
    expectMarker "bounded demands weakened proof" "bounded-bottom-proof" $
        withBounded weakenedProofBounded (\n xs proof -> evaluate proof >> pure ())
    assertEqual "bounded vector ignores cyclic proof" 42 (firstPacked (forgetBounded cyclicBounded))
    putStrLn "bounded vector ignores cyclic proof: 42"
    expectMarker "bounded predicate before tail" "packed-filter-bottom-predicate" $
        evaluate (withPacked (makePacked (S bottomLength)) (\n xs -> forgetBounded (filterBounded bottomPredicate xs)))
    expectMarker "bounded scans partial tail" "packed-bottom-length" $
        evaluate (withPacked (makePacked (S bottomLength)) (\n xs -> forgetBounded (filterBounded (const False) xs)))
    assertEqual "unused bounded filter" 42 (const (42 :: Int) (withPacked bottomPackage (\n xs -> forgetBounded (filterBounded bottomPredicate xs))))
    putStrLn "unused bounded filter: 42"
    forM_
        [ (0, "(Just 1,3)")
        , (1, "(Just 2,2)")
        , (3, "(Nothing,0)")
        ]
        $ \(cutoff, expected) -> do
            actual <- drivePatterns cutoff
            assertEqual "runtime nested patterns" expected actual
            putStrLn ("runtime nested patterns: " ++ actual)
    assertEqual "nested empty head" Nothing (headPacked (makePacked Z))
    putStrLn "nested empty head: Nothing"
    assertEqual "nested partial tail head" (Just 42) (headPacked (makePacked (S bottomLength)))
    putStrLn "nested partial tail head: Just 42"
    assertWhnf "nested bottom element constructor" (headPacked singletonBottom)
    expectMarker "nested demands element" "packed-filter-bottom-element" $
        evaluate (maybe 0 id (headPacked singletonBottom))
    expectMarker "nested payload before index" "packed-bottom-payload" $
        evaluate (headPacked (lazyPacked bottomLength))
    expectMarker "nested proof before fields" "bounded-bottom-proof" $
        evaluate (proofDepthBounded (undefinedBoundedFields bottomLength))
    expectMarker "nested partial proof" "bounded-bottom-proof" $
        evaluate (proofDepthBounded partialProofBounded)
    assertWhnf "wildcard proof repack" (repackWithoutProof (undefinedBoundedFields bottomLength))
    assertEqual "wildcard proof ignores bottom" (Just 42) (headPacked (repackWithoutProof lazyProofBounded))
    putStrLn "wildcard proof ignores bottom: Just 42"
    assertEqual "wildcard proof ignores cyclic" (Just 42) (headPacked (repackWithoutProof cyclicBounded))
    putStrLn "wildcard proof ignores cyclic: Just 42"
    assertEqual "early catchall ignores payload" 42 (earlyCatchall True (lazyPacked bottomLength))
    putStrLn "early catchall ignores payload: 42"
    expectMarker "reversed catchall demands payload" "packed-bottom-payload" $
        evaluate (payloadFirst True (lazyPacked bottomLength))
    forM_ [False, True] $ \takeFirst -> do
        actual <- driveGuardedRepack takeFirst
        assertEqual "runtime guarded repack" "(2,[42,42])" actual
        putStrLn ("runtime guarded repack: " ++ actual)
    assertWhnf "guard fallback bottom fields" (guardedRepack False (lazyPacked bottomLength))
    assertWhnf "guard fallback cyclic fields" (guardedRepack False (cyclicPacked cyclicLength))
    expectMarker "nested head bottom package" "packed-bottom-package" $
        evaluate (headPacked bottomPackage)
    expectMarker "nested proof bottom package" "bounded-bottom-package" $
        evaluate (proofDepthBounded (error "bounded-bottom-package" :: BoundedVec Int (S Z)))
    expectMarker "wildcard repack bottom package" "bounded-bottom-package" $
        evaluate (repackWithoutProof (error "bounded-bottom-package" :: BoundedVec Int (S Z)))
    expectMarker "guard fallback bottom package" "packed-bottom-package" $
        evaluate (guardedRepack False bottomPackage)
    forM_
        [ (Z, "Nothing")
        , (S (S Z), "42:(1,[42])")
        , (S (S (S Z)), "42:(2,[42,42])")
        ]
        $ \(input, expected) -> do
            actual <- driveUncons input
            assertEqual "runtime uncons" expected actual
            putStrLn ("runtime uncons: " ++ actual)
    forM_ [(0, "[1,2,3]"), (1, "[2,3]"), (3, "[]")] $ \(cutoff, expected) -> do
        actual <- driveDrainFilter cutoff
        assertEqual "runtime drain filtered" expected actual
        putStrLn ("runtime drain filtered: " ++ actual)
    case unconsPacked (lazyPacked Z) of
        Nothing -> putStrLn "uncons zero ignores payload: Nothing"
        Just _ -> fail "uncons zero ignores payload: expected Nothing"
    assertEqual "drain zero ignores payload" [] (toListPacked (lazyPacked Z))
    putStrLn "drain zero ignores payload: []"
    expectMarker "uncons index before payload" "packed-bottom-length" $
        evaluate (unconsPacked (lazyPacked bottomLength))
    expectMarker "uncons successor demands vector" "packed-bottom-payload" $
        evaluate (unconsPacked (lazyPacked (S bottomLength)))
    let partial = unconsPacked (PackVec (S bottomLength) (VCons bottomElement bottomPayload))
    assertWhnf "uncons partial result" partial
    case partial of
        Nothing -> fail "uncons partial result: expected Just"
        Just pair -> do
            assertWhnf "uncons partial pair" pair
            let (x, rest) = pair
            assertWhnf "uncons partial tail package" rest
            expectMarker "uncons demands head" "packed-filter-bottom-element" (evaluate x)
            expectMarker "uncons demands predecessor" "packed-bottom-length" $
                evaluate (withPacked rest (\n xs -> natToInt n))
            expectMarker "uncons demands tail payload" "packed-bottom-payload" $
                evaluate (headPacked rest)
    let cyclic = unconsPacked (makePacked (S cyclicLength))
    assertWhnf "uncons cyclic predecessor result" cyclic
    case cyclic of
        Nothing -> fail "uncons cyclic predecessor: expected Just"
        Just (x, rest) -> do
            assertWhnf "uncons cyclic predecessor tail package" rest
            assertEqual "uncons cyclic predecessor head" 42 x
            putStrLn "uncons cyclic predecessor head: 42"
            assertEqual "uncons cyclic bounded prefix" [42, 42, 42] (take 3 (toListPacked rest))
            putStrLn "uncons cyclic bounded prefix: [42,42,42]"
    expectMarker "uncons bottom package" "packed-bottom-package" $
        evaluate (unconsPacked bottomPackage)
    forM_
        [ (Z, Z, "same Just (0,[])")
        , (Z, S (S Z), "different Nothing")
        , (S (S Z), Z, "different Nothing")
        , (S (S Z), S (S Z), "same Just (2,[(42,7),(42,7)])")
        , (S (S Z), S (S (S Z)), "different Nothing")
        , (S (S (S Z)), S (S Z), "different Nothing")
        , (S (S (S Z)), S (S (S Z)), "same Just (3,[(42,7),(42,7),(42,7)])")
        ]
        $ \(left, right, expected) -> do
            actual <- driveEqualityZip left right
            assertEqual "runtime equality zip" expected actual
            putStrLn ("runtime equality zip: " ++ actual)
    forM_
        [ (0, 0, "Just (3,[(1,10),(2,20),(3,30)])")
        , (1, 10, "Just (2,[(2,20),(3,30)])")
        , (1, 0, "Nothing")
        , (3, 30, "Just (0,[])")
        ]
        $ \(left, right, expected) -> do
            actual <- driveFilteredZip left right
            assertEqual "runtime filtered zip" expected actual
            putStrLn ("runtime filtered zip: " ++ actual)
    case sameNat Z (S bottomRightLength) of
        Nothing -> putStrLn "equality zero successor ignores predecessor: Nothing"
        Just _ -> fail "equality zero successor: expected Nothing"
    case sameNat (S bottomLength) Z of
        Nothing -> putStrLn "equality successor zero ignores predecessor: Nothing"
        Just _ -> fail "equality successor zero: expected Nothing"
    case sameNat cyclicLength Z of
        Nothing -> putStrLn "equality cyclic successor zero: Nothing"
        Just _ -> fail "equality cyclic successor zero: expected Nothing"
    expectMarker "equality demands left length" "packed-bottom-length" $
        evaluate (sameNat bottomLength Z)
    expectMarker "equality demands right length" "equality-right-length" $
        evaluate (sameNat Z bottomRightLength)
    expectMarker "equality same index still demands length" "packed-bottom-length" $
        evaluate (sameNat bottomLength bottomLength)
    expectMarker "equality demands left predecessor" "packed-bottom-length" $
        evaluate (sameNat (S bottomLength) (S Z))
    expectMarker "equality demands right predecessor" "equality-right-length" $
        evaluate (sameNat (S Z) (S bottomRightLength))
    case zipPacked (PackVec Z leftVector) (PackVec (S Z) rightVector) of
        Nothing -> putStrLn "zip mismatch ignores vectors: Nothing"
        Just _ -> fail "zip mismatch ignores vectors: expected Nothing"
    case zipPacked (PackVec (S Z) leftVector) (PackVec Z rightVector) of
        Nothing -> putStrLn "zip reversed mismatch ignores vectors: Nothing"
        Just _ -> fail "zip reversed mismatch ignores vectors: expected Nothing"
    expectMarker "zip demands left package first" "packed-bottom-package" $
        evaluate (zipPacked bottomPackage (error "zip-right-package" :: PackedVec Int))
    expectMarker "zip demands right package" "zip-right-package" $
        evaluate (zipPacked (PackVec bottomLength leftVector) (error "zip-right-package" :: PackedVec Int))
    let undefinedVectors = zipPacked (PackVec (S Z) leftVector) (PackVec (S Z) rightVector)
    assertWhnf "zip equal result ignores vectors" undefinedVectors
    case undefinedVectors of
        Nothing -> fail "zip equal result: expected Just"
        Just package -> do
            assertWhnf "zip equal package ignores vectors" package
            assertEqual "zip length ignores vectors" 1 (withPacked package (\n xs -> natToInt n))
            putStrLn "zip length ignores vectors: 1"
            expectMarker "zip vector demands left tag" "zip-left-vector" $
                evaluate (firstPair package)
    case zipPacked (PackVec (S Z) (VCons 42 VNil)) (PackVec (S Z) rightVector) of
        Nothing -> fail "zip right tag: expected Just"
        Just package ->
            expectMarker "zip vector demands right tag" "zip-right-vector" $
                evaluate (firstPair package)
    let partialLeft = PackVec (S (S Z)) (VCons bottomElement leftVector)
        partialRight = PackVec (S (S Z)) (VCons (error "zip-right-head") rightVector)
    case zipPacked partialLeft partialRight of
        Nothing -> fail "zip partial fields: expected Just"
        Just package -> do
            let pair = firstPair package
            assertWhnf "zip tuple ignores components" pair
            expectMarker "zip demands left head" "packed-filter-bottom-element" (evaluate (fst pair))
            expectMarker "zip demands right head" "zip-right-head" (evaluate (snd pair))
            case unconsPacked package of
                Nothing -> fail "zip partial tail: expected Just"
                Just (_, rest) -> do
                    assertWhnf "zip tail package ignores tails" rest
                    expectMarker "zip demands left tail" "zip-left-vector" $
                        evaluate (firstPair rest)
    case zipPacked (PackVec (S (S Z)) (VCons 42 (VCons 43 VNil))) partialRight of
        Nothing -> fail "zip right tail: expected Just"
        Just package -> case unconsPacked package of
            Nothing -> fail "zip right tail: expected a nonempty vector"
            Just (_, rest) ->
                expectMarker "zip demands right tail" "zip-right-vector" $
                    evaluate (firstPair rest)
    forM_
        [ (Z, Z, "(0,[])")
        , (Z, S (S Z), "(2,[7,7])")
        , (S (S Z), Z, "(2,[42,42])")
        , (S (S Z), S (S Z), "(4,[42,42,7,7])")
        , (S (S Z), S (S (S Z)), "(5,[42,42,7,7,7])")
        , (S (S (S Z)), S (S Z), "(5,[42,42,42,7,7])")
        , (S (S (S Z)), S (S (S Z)), "(6,[42,42,42,7,7,7])")
        ]
        $ \(left, right, expected) -> do
            actual <- driveAppend left right
            assertEqual "runtime append" expected actual
            putStrLn ("runtime append: " ++ actual)
    forM_
        [ (0, 0, "(6,[1,2,3,10,20,30])")
        , (1, 10, "(4,[2,3,20,30])")
        , (1, 0, "(5,[2,3,10,20,30])")
        , (3, 30, "(0,[])")
        ]
        $ \(left, right, expected) -> do
            actual <- driveFilteredAppend left right
            assertEqual "runtime filtered append" expected actual
            putStrLn ("runtime filtered append: " ++ actual)
    assertWhnf "partial sum keeps bottom left" (sumNat bottomLength)
    assertWhnf "partial sum keeps cyclic left" (sumNat cyclicLength)
    assertWhnf "sum zero ignores right bottom" (sumNat Z bottomRightLength)
    assertWhnf "sum successors ignore right bottom" (sumNat (S (S Z)) bottomRightLength)
    assertWhnf "sum ignores cyclic right" (sumNat (S Z) cyclicLength)
    expectMarker "sum demands left length" "packed-bottom-length" $
        evaluate (sumNat bottomLength Z)
    expectMarker "sum demands left predecessor" "packed-bottom-length" $
        evaluate (sumNat (S bottomLength) Z)
    case sumNat (S Z) bottomRightLength of
        Sum p proof -> do
            assertWhnf "sum proof ignores right bottom" proof
            case p of
                S q -> putStrLn "sum length head ignores right bottom: S"
                Z -> fail "sum length head: expected S"
            expectMarker "sum length demands right bottom" "equality-right-length" $
                evaluate (natToInt p)
    assertWhnf "sum constructor ignores bottom fields" (lazySum bottomLength)
    case lazySum bottomLength of
        Sum p proof -> do
            expectMarker "sum stored length demand" "packed-bottom-length" $
                evaluate (natToInt p)
            expectMarker "sum stored proof demand" "append-bottom-proof" $
                evaluate proof
    case lazySum Z of
        Sum p proof -> do
            assertEqual "sum length ignores proof" 0 (natToInt p)
            putStrLn "sum length ignores proof: 0"
    assertEqual "unused append" 42 (const (42 :: Int) (appendPacked bottomPackage bottomPackage))
    putStrLn "unused append: 42"
    expectMarker "append demands left package first" "packed-bottom-package" $
        evaluate (appendPacked bottomPackage (error "append-right-package" :: PackedVec Int))
    expectMarker "append demands right package before lengths" "append-right-package" $
        evaluate (appendPacked (lazyPacked bottomLength) (error "append-right-package" :: PackedVec Int))
    expectMarker "append demands left length before vectors" "packed-bottom-length" $
        evaluate (appendPacked (lazyPacked bottomLength) (lazyPacked Z))
    expectMarker "append scans first length" "packed-bottom-length" $
        evaluate (appendPacked (lazyPacked (S bottomLength)) (lazyPacked Z))
    let zeroAppend = appendPacked (PackVec Z VNil) (PackVec bottomRightLength rightVector)
    assertWhnf "append zero ignores right length and vector" zeroAppend
    expectMarker "append zero demands right vector" "zip-right-vector" $
        evaluate (firstPacked zeroAppend)
    expectMarker "append zero length demands right length" "equality-right-length" $
        evaluate (withPacked zeroAppend (\n xs -> natToInt n))
    let undefinedAppend = appendPacked (PackVec (S Z) leftVector) (PackVec (S Z) rightVector)
    assertWhnf "append package ignores vectors" undefinedAppend
    assertEqual "append length ignores vectors" 2 (withPacked undefinedAppend (\n xs -> natToInt n))
    putStrLn "append length ignores vectors: 2"
    expectMarker "append vector demands left tag" "zip-left-vector" $
        evaluate (firstPacked undefinedAppend)
    let deferredRight = appendPacked (makePacked (S Z)) (PackVec (S Z) rightVector)
    assertEqual "append first head ignores right vector" 42 (firstPacked deferredRight)
    putStrLn "append first head ignores right vector: 42"
    expectMarker "append tail demands right vector" "zip-right-vector" $
        evaluate (sum (toListPacked deferredRight))
    let undefinedHead = appendPacked singletonBottom (makePacked Z)
    assertWhnf "append package ignores left element" undefinedHead
    expectMarker "append demands left element" "packed-filter-bottom-element" $
        evaluate (firstPacked undefinedHead)
    let deferredLeftTail = appendPacked (PackVec (S (S Z)) (VCons 42 leftVector)) (makePacked Z)
    assertEqual "append first head ignores left tail" 42 (firstPacked deferredLeftTail)
    putStrLn "append first head ignores left tail: 42"
    expectMarker "append demands left tail" "zip-left-vector" $
        evaluate (sum (take 2 (toListPacked deferredLeftTail)))
    assertEqual "append finite left ignores cyclic right" 42 (firstPacked (appendPacked (makePacked (S Z)) (cyclicPacked cyclicLength)))
    putStrLn "append finite left ignores cyclic right: 42"
    assertEqual "append cyclic right bounded prefix" [42, 42, 42] (take 3 (toListPacked (appendPacked (makePacked Z) (makePacked cyclicLength))))
    putStrLn "append cyclic right bounded prefix: [42,42,42]"
    forM_ [(Z, "(0,[])"), (S (S Z), "(1,[42])")] $ \(input, expected) -> do
        actual <- driveEqualTails input
        assertEqual "runtime equal tail patterns" expected actual
        putStrLn ("runtime equal tail patterns: " ++ actual)
    expectMarker "equal patterns demand proof" "append-bottom-proof" $
        evaluate (equalTails (lazyEqualLengths (S Z) (S Z)))
    expectMarker "equal patterns ignore predecessors before proof" "append-bottom-proof" $
        evaluate (equalTails (lazyEqualLengths (S bottomLength) (S bottomRightLength)))
    let partialEqualTail = equalTails (makeEqualLengths (S bottomLength))
    assertWhnf "equal patterns keep partial tail package" partialEqualTail
    expectMarker "equal patterns demand tail vector" "packed-bottom-length" $
        evaluate (firstPacked partialEqualTail)
    expectMarker "equal patterns demand tail length" "packed-bottom-length" $
        evaluate (withPacked partialEqualTail (\n xs -> natToInt n))
    expectMarker "equal patterns demand first index" "packed-bottom-length" $
        evaluate (equalTails (lazyEqualLengths bottomLength bottomRightLength))
    expectMarker "equal patterns demand second index" "equality-right-length" $
        evaluate (equalTails (lazyEqualLengths Z bottomRightLength))
    assertEqual "sum zero pattern ignores proof" 0 (sumTag (lazySum Z))
    putStrLn "sum zero pattern ignores proof: 0"
    assertEqual "sum successor pattern ignores predecessor and proof" 1 (sumTag (lazySum (S bottomLength)))
    putStrLn "sum successor pattern ignores predecessor and proof: 1"
    expectMarker "sum pattern demands stored index" "packed-bottom-length" $
        evaluate (sumTag (lazySum bottomLength))
    assertEqual "index zero vector pattern" True (zeroVector (makePacked Z))
    putStrLn "index zero vector pattern: True"
    expectMarker "explicit zero vector pattern demands payload" "packed-bottom-payload" $
        evaluate (zeroVector (lazyPacked Z))
    expectMarker "index pattern before payload" "packed-bottom-length" $
        evaluate (zeroVector (lazyPacked bottomLength))
    assertEqual "successor pattern ignores predecessor and vector" False (zeroVector (lazyPacked (S bottomLength)))
    putStrLn "successor pattern ignores predecessor and vector: False"
    expectMarker "index pattern before later argument" "packed-bottom-length" $
        evaluate (indexThenFlag (lazyPacked bottomLength) (error "pattern-flag"))
    expectMarker "zero pattern then later argument" "pattern-flag" $
        evaluate (indexThenFlag (lazyPacked Z) (error "pattern-flag"))
    expectMarker "vector pattern before later argument" "packed-bottom-payload" $
        evaluate (indexThenFlag (lazyPacked (S Z)) (error "pattern-flag"))
    assertEqual "zero pattern selects first row" 0 (indexThenFlag (makePacked Z) False)
    putStrLn "zero pattern selects first row: 0"
    assertEqual "successor pattern selects element" 1 (indexThenFlag samplePacked True)
    putStrLn "successor pattern selects element: 1"
    assertEqual "later argument mismatch reaches catchall" 42 (indexThenFlag samplePacked False)
    putStrLn "later argument mismatch reaches catchall: 42"
    expectMarker "index pattern before later view" "packed-bottom-length" $
        evaluate (indexBeforeView (lazyPacked bottomLength) True)
    expectMarker "zero pattern then later view" "pattern-view" $
        evaluate (indexBeforeView (lazyPacked Z) True)
    assertEqual "index mismatch skips later view" 1 (indexBeforeView (lazyPacked (S bottomLength)) True)
    putStrLn "index mismatch skips later view: 1"
    assertEqual "index guard ignores cyclic fields" 42 (guardedByIndex (cyclicPacked cyclicLength) True)
    putStrLn "index guard ignores cyclic fields: 42"
    assertEqual "index guard failure reaches catchall" 99 (guardedByIndex (lazyPacked (S Z)) False)
    putStrLn "index guard failure reaches catchall: 99"
    assertEqual "index mismatch skips guard argument" 0 (guardedByIndex (lazyPacked Z) (error "pattern-flag"))
    putStrLn "index mismatch skips guard argument: 0"
    expectMarker "index pattern before guard" "packed-bottom-length" $
        evaluate (guardedByIndex (lazyPacked bottomLength) True)
    forM_
        [ (Z, "(0,[])")
        , (S (S Z), "(2,[42,42])")
        , (S (S (S Z)), "(3,[42,42,42])")
        ]
        $ \(input, expected) -> do
            actual <- driveReverse input
            assertEqual "runtime reverse" expected actual
            putStrLn ("runtime reverse: " ++ actual)
    forM_
        [ (0, "(3,[3,2,1])")
        , (1, "(2,[3,2])")
        , (3, "(0,[])")
        ]
        $ \(cutoff, expected) -> do
            actual <- driveFilteredReverse cutoff
            assertEqual "runtime filtered reverse" expected actual
            putStrLn ("runtime filtered reverse: " ++ actual)
    forM_
        [ (0, S (S Z), "(5,[3,2,1,7,7])")
        , (1, S Z, "(3,[3,2,7])")
        , (3, S (S Z), "(2,[7,7])")
        ]
        $ \(cutoff, size, expected) -> do
            actual <- driveReverseAppend cutoff size
            assertEqual "runtime reverse append" expected actual
            putStrLn ("runtime reverse append: " ++ actual)
    proofs <- driveReverseProofs (S (S Z)) (S Z)
    assertEqual "runtime reverse arithmetic proofs" "Refl" proofs
    putStrLn ("runtime reverse arithmetic proofs: " ++ proofs)
    assertWhnf "reverse constructor with bottom fields" (reversePacked (lazyPacked bottomLength))
    assertWhnf "reverse constructor with cyclic fields" (reversePacked (cyclicPacked cyclicLength))
    assertEqual "reverse keeps stored length" 2 (withPacked (reversePacked (lazyPacked (S (S Z)))) (\n xs -> natToInt n))
    putStrLn "reverse keeps stored length: 2"
    assertEqual "reverse keeps partial stored constructor" 42 (withPacked (reversePacked (lazyPacked (S bottomLength))) (\n xs -> case n of Z -> 0; S k -> 42 :: Int))
    putStrLn "reverse keeps partial stored constructor: 42"
    expectMarker "reverse stored length demands index" "packed-bottom-length" $
        evaluate (withPacked (reversePacked (lazyPacked bottomLength)) (\n xs -> natToInt n))
    expectMarker "reverse index before payload" "packed-bottom-length" $
        evaluate (firstPacked (reversePacked (lazyPacked bottomLength)))
    expectMarker "reverse partial index before payload" "packed-bottom-length" $
        evaluate (firstPacked (reversePacked (lazyPacked (S bottomLength))))
    expectMarker "reverse zero inspects payload" "packed-bottom-payload" $
        evaluate (firstPacked (reversePacked (lazyPacked Z)))
    expectMarker "reverse successor inspects payload" "packed-bottom-payload" $
        evaluate (firstPacked (reversePacked (lazyPacked (S Z))))
    expectMarker "reverse complete spine before head" "packed-bottom-payload" $
        evaluate (firstPacked (reversePacked (PackVec (S (S Z)) (VCons 42 bottomPayload))))
    assertWhnf "reverse lazy element result" (headPacked (reversePacked singletonBottom))
    expectMarker "reverse demands element" "packed-filter-bottom-element" $
        evaluate (firstPacked (reversePacked singletonBottom))
    let reversedBottomHead = reversePacked (PackVec (S (S Z)) (VCons bottomElement (VCons 7 VNil)))
    assertEqual "reverse preserves last element" 7 (firstPacked reversedBottomHead)
    putStrLn "reverse preserves last element: 7"
    expectMarker "reverse later element demand" "packed-filter-bottom-element" $
        evaluate (withPacked reversedBottomHead (\n xs -> sum (toList xs)))
    expectMarker "reverse bottom package" "packed-bottom-package" $
        evaluate (reversePacked bottomPackage)
    assertEqual "unused reverse package" 42 (const (42 :: Int) (reversePacked bottomPackage))
    putStrLn "unused reverse package: 42"
    assertWhnf "partial reverse ignores bottom index" (reverseNative @Int bottomLength)
    assertWhnf "partial reverse ignores cyclic index" (reverseNative @Int cyclicLength)
    assertWhnf "partial reverse append ignores indices" (reverseAppendNative @Int bottomLength bottomRightLength)
    assertWhnf "partial reverse append ignores payload" (reverseAppendNative @Int bottomLength bottomRightLength bottomPayload)
    expectMarker "empty reverse append inspects first vector" "zip-left-vector" $
        evaluate (reverseAppendNative @Int Z bottomLength leftVector rightVector)
    expectMarker "empty reverse append ignores right index" "zip-right-vector" $
        evaluate (reverseAppendNative @Int Z bottomLength VNil rightVector)
    case addSucc Z bottomLength of
        Refl -> putStrLn "successor proof zero ignores right index: Refl"
    expectMarker "successor proof demands first index" "packed-bottom-length" $
        evaluate (addSucc bottomLength bottomRightLength)
    expectMarker "successor proof demands remaining index" "packed-bottom-length" $
        evaluate (addSucc (S bottomLength) bottomRightLength)
    expectMarker "right zero proof demands index" "packed-bottom-length" $
        evaluate (rightZero bottomLength)
    assertEqual "ignored reverse proof stays lazy" 42 (const (42 :: Int) (rightZero cyclicLength))
    putStrLn "ignored reverse proof stays lazy: 42"
    expectMarker "reverse append payload before predecessor" "packed-bottom-payload" $
        evaluate (reverseAppendNative @Int (S bottomLength) bottomRightLength bottomPayload bottomPayload)
    expectMarker "reverse append proof before tail" "packed-bottom-length" $
        evaluate (reverseAppendNative @Int (S bottomLength) Z (VCons 42 bottomPayload) VNil)
    assertEqual
        "reverse append ignores right fields"
        42
        (case reverseAppendNative @Int (S Z) bottomRightLength (VCons 42 VNil) bottomPayload of VCons x xs -> x)
    putStrLn "reverse append ignores right fields: 42"
    runLaterIndexChecks
    runIndexMatrixChecks
