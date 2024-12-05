{-# OPTIONS_GHC -O -fno-cse -fmax-simplifier-iterations=0 #-}

-- Not running the simplifier and CSE leads to type-lets persisting longer

module T13708 where

indexOr :: a -> Int -> [a] -> a
indexOr fallback idx xs =
  if (idx < length xs)
  then xs !! idx
  else fallback
