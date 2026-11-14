{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoPostfixOperators #-}

-- | Check source packages, filtering, and evaluation demand.
module Main (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
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
