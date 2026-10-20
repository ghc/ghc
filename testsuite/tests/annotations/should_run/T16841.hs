-- | Measures the overhead of serializing annotations of a few types.
module T16841 where

foo :: Int
foo = 42

{-# ANN foo (42 :: Int) #-}
{-# ANN foo (2^100 :: Integer) #-}
{-# ANN foo (3.25 :: Double) #-}
{-# ANN foo 'x' #-}
{-# ANN foo "helloworld" #-}
{-# ANN foo (Just True) #-}
{-# ANN foo ([1..100] :: [Int]) #-}
{-# ANN module (Left 7 :: Either Int String) #-}

