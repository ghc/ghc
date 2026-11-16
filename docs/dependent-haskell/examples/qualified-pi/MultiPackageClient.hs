{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Observe multi-constructor packages across a module interface.
module MultiPackageClient (main) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (forM_, unless)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import NativeMultiPackage
import PackedVecLibrary

-- | Render a finite stored length and vector.
render :: Maybe (PackedVec Int) -> String
render Nothing = "unavailable"
render (Just package) = withPacked package (\n xs -> show (natToInt n, toList xs))

-- | Check a constructor imported from the separate producer interface.
consumerIgnore :: Availability Int -> Int
consumerIgnore Unavailable = 0
consumerIgnore (Available n xs) = ignorePacked (PackVec n xs)

-- | Read each finite length through a mutable cell before using it.
drive :: (Nat -> String) -> Nat -> IO String
drive worker input = do
    cell <- newIORef input
    n <- readIORef cell
    pure (worker n)

-- | Check each expected observation before printing it.
observe :: (Eq a, Show a) => String -> a -> a -> IO ()
observe label expected actual = do
    unless (expected == actual) $ error (label ++ ": " ++ show actual)
    putStrLn (label ++ ": " ++ show actual)

-- | Require the original named exception and reject other exception types.
expectMarker :: String -> String -> IO a -> IO ()
expectMarker label marker action = do
    result <- try action
    case result of
        Left (exception :: ErrorCall)
            | marker `isInfixOf` displayException exception -> putStrLn (label ++ ": " ++ marker)
        Left exception -> error (label ++ ": unexpected marker: " ++ displayException exception)
        Right _ -> error (label ++ ": expected ErrorCall")

-- | Fail only when the retained length is demanded.
bottomLength :: Nat
bottomLength = error "multi-bottom-index"

-- | Supply an infinite successor spine without forcing it fully.
cyclicLength :: Nat
cyclicLength = S cyclicLength

-- | Fail when the package constructor itself is inspected.
bottomPackage :: Availability Int
bottomPackage = error "multi-bottom-package"

-- | Exercise finite branches, source row failure and field demand.
main :: IO ()
main = do
    let inputs = [(Z, "(0,[])"), (S (S Z), "(2,[42,42])"), (S (S (S Z)), "(3,[42,42,42])")]
    forM_ inputs $ \(n, expected) -> do
        actual <- drive (render . toPacked . makeAvailable) n
        observe "available" expected actual
    forM_ inputs $ \(n, expected) -> do
        actual <- drive (render . toPackedReverse . makeReverse) n
        observe "reverse constructor order" expected actual
    observe "unavailable" "unavailable" (render (toPacked (Unavailable :: Availability Int)))
    observe "reverse unavailable" "unavailable" (render (toPackedReverse (AbsentLast :: ReverseAvailability Int)))
    observe "ignored bottom fields" 42 (consumerIgnore (lazyAvailable bottomLength))
    observe "ignored cyclic fields" 42 (consumerIgnore (cyclicAvailable cyclicLength))
    case toPacked (lazyAvailable bottomLength) of
        Nothing -> error "expected Available"
        Just package -> do
            _ <- evaluate package
            putStrLn "converted bottom fields: whnf"
            expectMarker "demand vector" "multi-bottom-vector" $
                evaluate (withPacked package (\n xs -> sum (toList xs)))
            expectMarker "demand index" "multi-bottom-index" $
                evaluate (withPacked package (\n xs -> natToInt n))
    observe "row failure" 42 (guardedRows False (lazyAvailable bottomLength))
    observe "nested vector empty" (Nothing :: Maybe Int) (firstAvailable (makeAvailable Z))
    observe "nested vector nonempty" (Just (42 :: Int)) (firstAvailable (makeAvailable (S (S Z))))
    expectMarker "bottom package" "multi-bottom-package" $ evaluate (toPacked bottomPackage)
