{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Check strict opaque calls with scalar constructor arguments.
module ConstructorCallClient where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Type.Equality ((:~:) (Refl))
import PackedVecLibrary

-- | Observe an opaque call with a nullary constructor argument.
constructorZero :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorZero f n original = case f Z of
    Z -> let ys :: Vec Int (f Z); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f Z); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Observe a call whose argument contains the retained runtime index.
constructorLength :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorLength f n original = case f (S n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check a second constructor layer without evaluating its predecessor early.
constructorTwice :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorTwice f n original = case f (S (S n)) of
    Z -> let ys :: Vec Int (f (S (S n))); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S (S n))); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Transport a consumer captured before the outer call case.
constructorCaptured :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorCaptured f n original =
    let consume :: Vec Int (f (S n)) -> Int
        consume cached = case f (S n) of
            Z -> length (toList cached)
            S k -> length (toList cached)
     in case f (S n) of
            Z -> let ys :: Vec Int (f (S n)); ys = VNil in consume ys
            S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in consume ys

-- | Read the new head without demanding the constructor argument's tail.
constructorHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorHead f n original = case f (S n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in head (toList ys)

-- | Read a finite prefix from a cyclic constructor argument.
constructorPrefix :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> [Int]
constructorPrefix f n original = case f (S n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in take 4 (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in take 4 (toList ys)

-- | Demand the actual result predecessor explicitly.
constructorSeq :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorSeq f n original = case f (S n) of
    Z -> 0
    S k -> k `seq` 42

-- | Keep the call case outside a returned ordinary function.
constructorFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int
constructorFunction f n original = case f (S n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in \x -> length (toList ys) + x
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in \x -> head (toList ys) + x

-- | Check a Bool constructor argument with a different result domain.
constructorBool :: (Bool -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
constructorBool f n original = case f True of
    Z -> let ys :: Vec Int (f True); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f True); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check a constructor argument when the call returns Bool.
constructorMixed :: (Nat -> Bool) -> foreach (n :: Nat) -> Vec Int n -> Bool
constructorMixed f n original = case f (S n) of
    False -> let witness :: f (S n) :~: 'False; witness = Refl in case witness of Refl -> False
    True -> let witness :: f (S n) :~: 'True; witness = Refl in case witness of Refl -> True

-- | Preserve an opaque identity head.
{-# NOINLINE identityConstructor #-}
identityConstructor :: Nat -> Nat
identityConstructor x = x

-- | Return one result for distinct constructor arguments.
{-# NOINLINE zeroConstructor #-}
zeroConstructor :: Nat -> Nat
zeroConstructor x = Z

-- | Return a Nat for a Bool constructor.
{-# NOINLINE boolConstructor #-}
boolConstructor :: Bool -> Nat
boolConstructor True = S Z
boolConstructor False = Z

-- | Ignore the argument of a Bool-returning call.
{-# NOINLINE trueConstructor #-}
trueConstructor :: Nat -> Bool
trueConstructor x = True

-- | Require the original runtime error before printing a demand observation.
expectConstructorError :: String -> IO a -> IO ()
expectConstructorError fragment action = do
    outcome <- try (action >> pure ())
    case outcome of
        Left (err :: SomeException) -> unless (fragment `isInfixOf` displayException err) (fail (displayException err))
        Right () -> fail ("Expected runtime error: " ++ fragment)

-- | Read independent runtime indices and observe constructor call demand.
main :: IO ()
main = do
    forM_ [Z, S (S Z), S (S (S Z))] $ \seed -> do
        ref <- newIORef seed
        n <- readIORef ref
        let xs :: Vec Int n
            xs = replicateNative @Int n 7
        print (constructorLength identityConstructor n xs, constructorTwice identityConstructor n xs, constructorCaptured identityConstructor n xs)
    let bottomN :: Nat
        bottomN = error "constructor-bottom-index"
        bottomF :: Nat -> Nat
        bottomF = error "constructor-bottom-head"
        cyclic :: Nat
        cyclic = S cyclic
    print (constructorZero identityConstructor bottomN (error "unused-vector"))
    print (constructorLength zeroConstructor bottomN (error "unused-vector"))
    print (constructorHead identityConstructor bottomN (error "unused-vector"))
    print (constructorHead identityConstructor cyclic (error "unused-vector"))
    print (constructorPrefix identityConstructor cyclic (error "unused-vector"))
    print (constructorBool boolConstructor bottomN (error "unused-vector"))
    print (constructorMixed trueConstructor bottomN (error "unused-vector"))
    evaluate (constructorFunction identityConstructor bottomN (error "unused-vector")) >> putStrLn "constructor-function: forced"
    print (constructorFunction identityConstructor Z VNil 7)
    print (constructorLength zeroConstructor Z VNil, constructorLength zeroConstructor (S Z) (VCons 7 VNil))
    expectConstructorError "constructor-bottom-index" (evaluate (constructorSeq identityConstructor bottomN (error "unused-vector")))
    putStrLn "constructor-predecessor: bottom-index"
    expectConstructorError "constructor-bottom-head" (evaluate (constructorHead bottomF bottomN (error "unused-vector")))
    putStrLn "constructor-head: bottom-head"
    expectConstructorError "constructor-bottom-index" (evaluate (constructorLength identityConstructor bottomN (error "unused-vector")))
    putStrLn "constructor-length: bottom-index"
    expectConstructorError "constructor-bottom-head" (evaluate (constructorFunction bottomF bottomN (error "unused-vector")))
    putStrLn "constructor-function: bottom-head"
