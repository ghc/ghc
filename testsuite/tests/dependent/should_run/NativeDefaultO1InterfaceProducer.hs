{-# LANGUAGE BangPatterns, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module NativeDefaultO1InterfaceProducer (exposure, ordinaryWorker, indexedWorker, Flag(..), classify) where

import Data.Type.Equality ((:~:)(Refl))

-- | Record the Boolean that the callback returns.
data Flag (b :: Bool) where
    Off :: Flag False
    On  :: Flag True

-- | Keep the callback result as the index of the selected constructor.
classify :: foreach (f :: Bool -> Bool) -> foreach (b :: Bool) -> Flag (f b)
classify f b = case f b of
    False -> Off
    True  -> On

indexedWorker :: foreach (index :: Bool) -> Bool -> Int -> (Int, index :~: index)
indexedWorker _ _ !n
    | n > 1000 = (n * 31 + 1, Refl)
    | n > 100 = (n * 23 + 2, Refl)
    | n > 10 = (n * 13 + 3, Refl)
    | n > 0 = (n + 4, Refl)
    | n > (-100) = (n - 5, Refl)
    | otherwise = (n * 7 - 6, Refl)

exposure :: Bool -> Bool -> Int -> Int
exposure index unrelated n = case indexedWorker index unrelated n of
    (result, Refl) -> result

ordinaryWorker :: Int -> Bool -> Int
ordinaryWorker !n _
    | n > 1000 = n * 19 + 1
    | n > 100 = n * 17 + 2
    | n > 10 = n * 11 + 3
    | n > 0 = n + 6
    | n > (-100) = n - 7
    | otherwise = n * 5 - 8
