module Main where

import Control.Exception (evaluate)
import GHC.Exts (lazy, noinline)

data StrictPair a b = !a :*: !b

tailEval1 :: a -> IO a
{-# OPAQUE tailEval1 #-}
tailEval1 = lazy $ \x -> do
  pure ()
  pure $! x

tailEval2 :: a -> IO a
{-# OPAQUE tailEval2 #-}
tailEval2 x = evaluate x

go :: [a] -> IO ()
go = noinline mapM_ $ \x -> do
  y1 <- tailEval1 x
  y2 <- tailEval2 x
  evaluate (y1 :*: y2)

main :: IO ()
main = do
  let ints :: [Int]
      ints = take 1000 $ noinline iterate (\x -> x * 35) 1
  go ints
  go [LT, EQ, GT]
  go $ noinline map (toEnum @Ordering . flip mod 3) ints
  go $ noinline map Left ints
  go $ noinline map (+)  ints
