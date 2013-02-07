{-# LANGUAGE ExistentialQuantification, DeriveDataTypeable #-}

-- The second version (count2) took ages with GHC 6.12
-- because the typeOf function was not properly memoised

import Data.Typeable
import System.CPUTime

size :: Int
size = 40000	-- This was big enough to take 5 seconds in 
       		-- the bad case on my machine.

data Any = forall a. (Typeable a) => Any a

int_type, int_list_type :: TypeRep
int_type = typeOf (undefined :: Int)
int_list_type = typeOf (undefined :: [Int])

count1 :: [Any] -> Int
count1 [] = 0
count1 (Any x:xs) = count1 xs + (if typeOf x == int_type then 1 else 0)

doTime x = do
  start <- getCPUTime
  putStr "Result: "
  print x
  stop <- getCPUTime
  putStr "Time(sec): "
  print (round $ fromIntegral (stop - start) / 1e12)
    -- The 'round' rounds to an integral number of seconds
    -- Should be zero if things are working right!

main = do
  let list = [MkT | i <- [1..size  :: Int]]
  putStrLn "count1"
  let x = map Any list
  doTime $ count1 x
  doTime $ count1 x
  doTime $ count1 x
  putStrLn ""
  putStrLn "count2"
  let x = map (Any . (:[])) list
  doTime $ count1 x
  doTime $ count1 x
  doTime $ count1 x

data T = MkT deriving Typeable
