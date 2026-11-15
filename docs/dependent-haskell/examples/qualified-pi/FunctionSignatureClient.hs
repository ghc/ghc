{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Use an imported retained worker with actual function-package fields.
module FunctionSignatureClient (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import FunctionPackageLibrary
import FunctionSignatureLibrary
import PackedVecLibrary
import System.Mem.StableName (makeStableName)

-- | Preserve a runtime identity head.
{-# NOINLINE identity #-}
identity :: Nat -> Nat
identity n = n

-- | Preserve a runtime successor head.
{-# NOINLINE successor #-}
successor :: Nat -> Nat
successor n = S n

-- | Ignore the input with a noninjective runtime head.
{-# NOINLINE constantZero #-}
constantZero :: Nat -> Nat
constantZero n = Z

-- | Select the original vector through the imported worker.
payloadThrough :: forall a. FunctionPack a -> [a]
payloadThrough (FunctionPack f n xs) = toList (idCall @a f n xs)

-- | Keep the actual package fields without demanding the selected payload.
ignoreThrough :: FunctionPack Int -> Int
ignoreThrough (FunctionPack f n xs) = const 42 (idCall @Int f n xs)

-- | Compare the same evaluated vector constructor before and after the worker.
sharesPayload :: forall a. FunctionPack a -> IO Bool
sharesPayload (FunctionPack f n xs) = do
    original <- evaluate xs
    selected <- evaluate (idCall @a f n xs)
    before <- makeStableName original
    after <- makeStableName selected
    pure (before == after)

-- | Check the independent expected result and print its label.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual = do
    unless (expected == actual) (error (label ++ ": " ++ show actual))
    putStrLn (label ++ ": " ++ show actual)

-- | Require the original payload or input error.
expectMarker :: String -> IO a -> IO ()
expectMarker marker action = do
    result <- try action
    case result of
        Left (exception :: ErrorCall)
            | marker `isInfixOf` displayException exception -> putStrLn marker
        Left exception -> error (displayException exception)
        Right _ -> error ("expected " ++ marker)

-- | Load finite inputs independently and check value, identity and demand.
main :: IO ()
main = do
    forM_ [(Z, 0, [], [9]), (S (S Z), 2, [7, 7], [9, 9, 9]), (S (S (S Z)), 3, [7, 7, 7], [9, 9, 9, 9])] $ \(seed, size, same, next) -> do
        n <- newIORef seed >>= readIORef
        m <- newIORef seed >>= readIORef
        k <- newIORef seed >>= readIORef
        check ("identity " ++ show (size :: Int)) same (payloadThrough (buildPack @Int identity n 7))
        check ("successor " ++ show size) next (payloadThrough (buildPack @Int successor m 9))
        check ("constant " ++ show size) ([] :: [Int]) (payloadThrough (buildPack @Int constantZero k 13))
    check "generic character payload" "qq" (payloadThrough (buildPack @Char identity (S (S Z)) 'q'))
    shared <- sharesPayload (buildPack @Int identity (S (S Z)) 7)
    check "vector constructor identity" True shared
    let f :: Nat -> Nat
        f = error "exported-function-head"
        n :: Nat
        n = error "exported-input"
        zero :: Nat -> Nat
        zero = constantZero
        cyclic :: Nat
        cyclic = S cyclic
    _ <- evaluate (idCall @Int f)
    putStrLn "partial function argument: whnf"
    _ <- evaluate (idCall @Int f n)
    putStrLn "partial function and input: whnf"
    check "ignored bottom payload" 42 (ignoreThrough (lazyPack @Int f n))
    expectMarker "function-pack-payload" (evaluate (length (payloadThrough (lazyPack @Int f n))))
    check "constant ignores bottom input" ([] :: [Int]) (payloadThrough (buildPack @Int zero n 13))
    check "constant ignores cyclic input" ([] :: [Int]) (payloadThrough (buildPack @Int zero cyclic 13))
    check "partial input head" 7 (head (payloadThrough (buildPack @Int successor n 7)))
    expectMarker "exported-input" (evaluate (length (payloadThrough (buildPack @Int successor n 7))))
