{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

-- | Check source construction, import, and lazy elimination.
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
