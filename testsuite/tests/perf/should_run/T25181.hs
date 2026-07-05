-- T25181
import Control.Exception (evaluate)

import Prelude hiding (repeat)

repeat :: a -> [a]
repeat a = res
  where res = a : res
{-# NOINLINE repeat #-}

silly :: [(Int, Int)] -> IO ()
silly = foldr go (pure ())
  where
    go p r = do
      (x, y) <- evaluate p
      (x', y') <- evaluate (x + 3, y)
      evaluate y'
      r

main :: IO ()
-- 10,000,000 repetitions take only a twentieth of a second,
-- but allocations go up dramatically if the result is not
-- known evaluated.
main = silly $ take 10000000 $ repeat (1,1)
