{-# LANGUAGE ScopedTypeVariables #-}
module Bug where

f1 :: forall a. [a] -> [a]
f1 (x:xs) = xs ++ [ x :: a ]   -- OK

f2 :: forall a. [a] -> [a]
f2 = \(x:xs) -> xs ++ [ x :: a ]   -- OK

-- This pair is a cut-down version of Trac #2030
isSafe alts = isSafeAlts alts

isSafeAlts :: forall m . Int -> m Int
isSafeAlts x = error "urk"
  where
    isSafeAlt :: Int -> m Int
    isSafeAlt alt = isSafe `seq` error "urk"

