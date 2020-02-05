{-# language ExistentialQuantification #-}

-- | This code is extracted from containers' sequence-benchmarks and the gauge
-- package.
module T17724 where

import Control.Exception (evaluate)

data Benchmarkable = forall a .
    Benchmarkable
      { allocEnv :: Int -> IO a
      , runRepeatedly :: a -> Int -> IO ()
      }

a, b :: Benchmarkable
a = nf (\(s,t) -> (,) <$> replicate s () <*> replicate t ()) (100,2500)
b = nf (\(s,t) -> (,) <$> replicate s () <*> replicate t ()) (2500,100)

nf :: (a -> b) -> a -> Benchmarkable
nf f0 x0 = Benchmarkable (const (return ())) (const (go f0 x0))
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (f x) >> go f x (n-1)
