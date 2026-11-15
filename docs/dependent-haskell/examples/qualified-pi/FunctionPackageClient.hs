{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Finite observations through a separate producer interface.
module FunctionPackageClient (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import FunctionPackageLibrary
import PackedVecLibrary

-- | Preserve an opaque identity head at runtime.
{-# NOINLINE identity #-}
identity :: Nat -> Nat
identity n = n

-- | Preserve a head that adds one lazy successor.
{-# NOINLINE successor #-}
successor :: Nat -> Nat
successor n = S n

-- | Preserve a noninjective head that ignores its input.
{-# NOINLINE constantZero #-}
constantZero :: Nat -> Nat
constantZero n = Z

-- | Fail when the stored function is executed.
bottomHead :: Nat -> Nat
bottomHead = error "function-pack-head"

-- | Fail when the original stored input is demanded.
bottomIndex :: Nat
bottomIndex = error "function-pack-index"

-- | Supply a bounded observation of an infinite successor spine.
cyclicIndex :: Nat
cyclicIndex = S cyclicIndex

-- | Check the independent expected result before printing it.
check :: (Eq a, Show a) => a -> a -> IO ()
check expected actual = do
    unless (expected == actual) (error ("function package mismatch: " ++ show actual))
    print actual

-- | Require a named error instead of any arbitrary failure.
expectMarker :: String -> IO a -> IO ()
expectMarker marker action = do
    result <- try action
    case result of
        Left (exception :: ErrorCall)
            | marker `isInfixOf` displayException exception -> putStrLn marker
        Left exception -> error (displayException exception)
        Right _ -> error ("expected " ++ marker)

-- | Observe each function at independently loaded runtime inputs.
main :: IO ()
main = do
    forM_ [(Z, (0, []), (1, [42])), (S (S Z), (2, [42, 42]), (3, [42, 42, 42])), (S (S (S Z)), (3, [42, 42, 42]), (4, [42, 42, 42, 42]))] $ \(seed, same, next) -> do
        a <- newIORef seed >>= readIORef
        b <- newIORef seed >>= readIORef
        c <- newIORef seed >>= readIORef
        check same (inspectFunction (repackFunction (buildPack @Int identity a 42)))
        check next (inspectFunction (repackFunction (buildPack @Int successor b 42)))
        check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero c 42))
    _ <- evaluate (lazyPack @Int bottomHead bottomIndex)
    _ <- evaluate (repackFunction (lazyPack @Int bottomHead bottomIndex))
    putStrLn "package and repack: whnf"
    check 42 (ignoreFunction (lazyPack @Int bottomHead bottomIndex))
    check 42 (guardedIgnore False (lazyPack @Int bottomHead cyclicIndex))
    check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero bottomIndex 42))
    check (0, [] :: [Int]) (inspectFunction (buildPack @Int constantZero cyclicIndex 42))
    check 42 (headOnly (buildPack @Int successor bottomIndex 42))
    check [42, 42, 42] (take 3 (payloadOnly (buildPack @Int identity cyclicIndex 42)))
    check ([42, 42], [42, 42, 42]) (pairPayloads (buildPack @Int identity (S (S Z)) 42) (buildPack @Int successor (S (S Z)) 42))
    expectMarker "function-pack-payload" $ evaluate (length (payloadOnly (lazyPack @Int bottomHead bottomIndex)))
    expectMarker "function-pack-head" $ evaluate (fst (inspectFunction (lazyPack @Int bottomHead bottomIndex)))
    expectMarker "function-pack-index" $ evaluate (inputLength (lazyPack @Int bottomHead bottomIndex))
    expectMarker "function-pack-index" $ evaluate (fst (inspectFunction (lazyPack @Int identity bottomIndex)))
    expectMarker "function-pack-index" $ evaluate (length (payloadOnly (buildPack @Int successor bottomIndex 42)))
    expectMarker "function-pack-outer" $ evaluate (ignoreFunction (error "function-pack-outer" :: FunctionPack Int))
    check (2, "qq") (inspectFunction (buildPack @Char identity (S (S Z)) 'q'))
