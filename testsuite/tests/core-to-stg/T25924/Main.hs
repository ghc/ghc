module Main where
import B

getCounterRef' :: forall b r.
  ( YulO1 b
  , YulO1 r
  -- , YulO1 (REF b)
  ) =>
  P r ()  -> P r (REF b)
getCounterRef' a = extendType'l (keccak256'l a)
{-# NOINLINE getCounterRef' #-}

main :: IO ()
main = putStrLn $ lfn' @() getCounterRef'
