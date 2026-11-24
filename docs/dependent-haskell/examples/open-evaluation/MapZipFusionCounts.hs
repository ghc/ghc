-- | Compare two evaluations of the length law for map after zip.
module MapZipFusionCounts (producer, mapper, report) where

import Control.Exception (ErrorCall, evaluate, try)
import GHC.Exts (build)
import HaskLib (Nat(..), minSize, size)

-- | Inspect the first application of the supplied list constructor.
{-# NOINLINE producer #-}
producer :: (Bool -> r -> r) -> r -> r
producer cons nil = cons undefined undefined `seq` cons False nil

-- | Retain the source list in the mapper's closure.
{-# NOINLINE mapper #-}
mapper :: [Bool] -> (Bool, ()) -> Char
mapper source _ = source `seq` errorWithoutStackTrace "mapper forced"

-- | Count the mapped pairs at one optimized call site.
{-# NOINLINE mappedCount #-}
mappedCount :: (Bool -> Bool) -> [()] -> Nat
mappedCount p ys =
    let source = build producer
    in size (map (mapper source) (zip (filter p source) ys))

-- | Compute the right side of the law at a separate call site.
{-# NOINLINE plainCount #-}
plainCount :: (Bool -> Bool) -> [()] -> Nat
plainCount p ys = minSize (size (filter p (build producer))) (size ys)

-- | Give both sides the same filtered list binding.
{-# NOINLINE sharedCounts #-}
sharedCounts :: (Bool -> Bool) -> [()] -> (Nat, Nat)
sharedCounts p ys =
    let source = build producer
        xs = filter p source
    in (size (map (mapper source) (zip xs ys)), minSize (size xs) (size ys))

-- | Report the outer constructor or the exception raised when demanding it.
observe :: Nat -> IO String
observe n = do
    result <- try (evaluate n) :: IO (Either ErrorCall Nat)
    pure $ case result of
        Left _ -> "bottom"
        Right Zero -> "Zero"
        Right (Succ _) -> "Succ"

-- | Compare independent calls with the shared-input control.
report :: (Bool -> Bool) -> [()] -> IO ()
report p ys = do
    counts "independent" (mappedCount p ys, plainCount p ys)
    counts "shared" (sharedCounts p ys)
  where
    counts label (m, n) = do
        left <- observe m
        right <- observe n
        putStrLn (unwords [label, left, right])
