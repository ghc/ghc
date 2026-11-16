{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Check signed beta arguments in strict opaque-call cases.
module BetaCallClient where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import PackedVecLibrary

-- | Check the canonical index of betaCallIdentity without unfolding the opaque head.
betaCallIdentity :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallIdentity f n original = case f (((\x -> x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f n); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f n); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check the canonical index of betaCallSuccessor without unfolding the opaque head.
betaCallSuccessor :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallSuccessor f n original = case f (((\x -> S x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check the canonical index of betaCallNested without unfolding the opaque head.
betaCallNested :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallNested f n original = case f (((\x -> S (((\y -> S y) :: Nat -> Nat) x)) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S (S n))); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S (S n))); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check the canonical index of betaCallConstant without unfolding the opaque head.
betaCallConstant :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallConstant f n original = case f (((\x -> Z) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f Z); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f Z); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

-- | Check the canonical index of betaCallHead without unfolding the opaque head.
betaCallHead :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallHead f n original = case f (((\x -> S x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in head (toList ys)

-- | Check the canonical index of betaCallPrefix without unfolding the opaque head.
betaCallPrefix :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> [Int]
betaCallPrefix f n original = case f (((\x -> S x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in take 4 (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in take 4 (toList ys)

-- | Check the canonical index of betaCallSeq without unfolding the opaque head.
betaCallSeq :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallSeq f n original = case f (((\x -> S x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in k `seq` head (toList ys)

-- | Check the canonical index of betaCallFunction without unfolding the opaque head.
betaCallFunction :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int -> Int
betaCallFunction f n original = case f (((\x -> S x) :: Nat -> Nat) n) of
    Z -> let ys :: Vec Int (f (S n)); ys = VNil in \x -> length (toList ys) + x
    S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in \x -> head (toList ys) + x

-- | Transport a consumer that captures the canonical successor call.
betaCallCaptured :: (Nat -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallCaptured f n original =
    let consume :: Vec Int (f (S n)) -> Int
        consume cached = case f (S n) of
            Z -> length (toList cached)
            S k -> length (toList cached)
     in case f (((\x -> S x) :: Nat -> Nat) n) of
            Z -> let ys :: Vec Int (f (S n)); ys = VNil in consume ys
            S k -> let ys :: Vec Int (f (S n)); ys = VCons 42 (replicateNative @Int k 42) in consume ys

-- | Check a signed Bool endofunction in a mixed-domain call.
betaCallBool :: (Bool -> Nat) -> foreach (n :: Nat) -> Vec Int n -> Int
betaCallBool f n original = case f (((\b -> b) :: Bool -> Bool) True) of
    Z -> let ys :: Vec Int (f True); ys = VNil in length (toList ys)
    S k -> let ys :: Vec Int (f True); ys = VCons 42 (replicateNative @Int k 42) in length (toList ys)

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

-- | Require the original runtime error before printing a demand observation.
expectConstructorError :: String -> IO a -> IO ()
expectConstructorError fragment action = do
    outcome <- try (action >> pure ())
    case outcome of
        Left (err :: SomeException) -> unless (fragment `isInfixOf` displayException err) (fail (displayException err))
        Right () -> fail ("Expected runtime error: " ++ fragment)

-- | Observe signed beta arguments with independent runtime inputs.
main :: IO ()
main = do
    forM_ [Z, S (S Z), S (S (S Z))] $ \seed -> do
        ref <- newIORef seed
        n <- readIORef ref
        let xs :: Vec Int n
            xs = replicateNative @Int n 7
        print (betaCallIdentity identityConstructor n xs, betaCallSuccessor identityConstructor n xs, betaCallNested identityConstructor n xs, betaCallCaptured identityConstructor n xs)
    let bottomN :: Nat
        bottomN = error "constructor-bottom-index"
        bottomF :: Nat -> Nat
        bottomF = error "constructor-bottom-head"
        cyclic :: Nat
        cyclic = S cyclic
    print (betaCallConstant identityConstructor bottomN (error "unused-vector"))
    print (betaCallIdentity zeroConstructor bottomN (error "unused-vector"))
    print (betaCallHead identityConstructor bottomN (error "unused-vector"))
    print (betaCallHead identityConstructor cyclic (error "unused-vector"))
    print (betaCallPrefix identityConstructor cyclic (error "unused-vector"))
    evaluate (betaCallFunction identityConstructor bottomN (error "unused-vector")) >> putStrLn "beta-function: forced"
    print (betaCallFunction identityConstructor Z VNil 7)
    print (betaCallBool boolConstructor bottomN (error "unused-vector"))
    expectConstructorError "constructor-bottom-index" (evaluate (betaCallSeq identityConstructor bottomN (error "unused-vector")))
    putStrLn "beta-predecessor: bottom-index"
    expectConstructorError "constructor-bottom-head" (evaluate (betaCallHead bottomF bottomN (error "unused-vector")))
    putStrLn "beta-head: bottom-head"
    expectConstructorError "constructor-bottom-index" (evaluate (betaCallIdentity identityConstructor bottomN (error "unused-vector")))
    putStrLn "beta-identity: bottom-index"
    expectConstructorError "constructor-bottom-head" (evaluate (betaCallFunction bottomF bottomN (error "unused-vector")))
    putStrLn "beta-function: bottom-head"
