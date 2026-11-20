{-# LANGUAGE ExperimentalDependentTypes, ScopedTypeVariables #-}
module ErasedStream (Stream, fromList, oddThrough, toList) where

import Data.Kind (Type)
import GHC.Exts (build)

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
{-# INLINE [1] toList #-}
toList (step, start) = foldSteps (:) [] step start

-- | Fold yielded elements in order. The combining function can ignore the tail.
foldSteps :: (a -> b -> b) -> b -> (s -> Step a s) -> s -> b
foldSteps cons done step = go
  where
    go s = case step s of
      Done -> done
      Skip next -> go next
      Yield x next -> cons x (go next)

{-# RULES "toList/build" [~1] forall (stream :: Stream a).
    toList stream = build (\cons nil -> case stream of
      (step, start) -> foldSteps cons nil step start) #-}
