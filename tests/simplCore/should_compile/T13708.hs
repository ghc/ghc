{-# OPTIONS_GHC -O -fmax-simplifier-iterations=0 #-}

-- Not running the simplifier leads to type-lets persisting longer

module T13708 where

indexOr :: a -> Int -> [a] -> a
indexOr fallback idx xs =
  if (idx < length xs)
  then xs !! idx
  else fallback
