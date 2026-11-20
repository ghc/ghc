{-# LANGUAGE ExperimentalDependentTypes #-}
module ErasedStream (Stream, fromList, oddThrough, toList) where

import Data.Kind (Type)

-- | One transition of a stream with state s.
data Step a s = Done | Yield a s | Skip s

-- | The step function and its private initial state.
type Stream a = exists (s :: Type). (s -> Step a s, s)

-- | Yield each list element in order.
fromList :: [a] -> Stream a
fromList xs = (step, xs)
  where
    step [] = Done
    step (x : rest) = Yield x rest

-- | Yield the positive odd integers up to the given limit.
oddThrough :: Integer -> Stream Integer
oddThrough limit = (step, 1)
  where
    step n
      | n > limit = Done
      | even n = Skip (n + 1)
      | otherwise = Yield n (n + 1)

-- | Produce the next list cell as soon as the step function yields.
toList :: Stream a -> [a]
toList (step, start) = go start
  where
    go s = case step s of
      Done -> []
      Skip next -> go next
      Yield x next -> x : go next
