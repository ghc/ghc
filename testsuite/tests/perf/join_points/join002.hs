module Main where

import Data.List (find)

-- These four functions should all wind up the same; they represent successive
-- simplifications that should happen. (Actual details may vary, since find
-- isn't quite defined this way, but the differences disappear by the end.)

firstEvenIsPositive1 :: [Int] -> Bool
firstEvenIsPositive1 = maybe False (> 0) . find even

-- After inlining:

firstEvenIsPositive2 :: [Int] -> Bool
firstEvenIsPositive2 xs =
  let go xs = case xs of x:xs' -> if even x then Just x else go xs'
                         []    -> Nothing
  in case go xs of Just n  -> n > 0
                   Nothing -> False

-- Note that go *could* be a join point if it were declared inside the scrutinee
-- instead of outside. So it's now Float In's job to move the binding inward a
-- smidge. *But* if it goes too far inward (as it would until recently), it will
-- wrap only "go" instead of "go xs", which won't let us mark go as a join point
-- since join points can't be partially invoked.
--
-- After Float In:

firstEvenIsPositive3 :: [Int] -> Bool
firstEvenIsPositive3 xs =
  case let {-join-} go xs = case xs of x:xs' -> if even x then Just x
                                                          else go xs'
                                       []    -> Nothing
       in go xs of
    Just n  -> n > 0
    Nothing -> False

-- After the simplifier:

firstEvenIsPositive4 :: [Int] -> Bool
firstEvenIsPositive4 xs =
  let {-join-} go xs = case xs of x:xs' -> if even x then x > 0 else go xs'
                                  []    -> False
  in go xs

-- This only worked because go was a join point so that the case gets moved
-- inside.

{-# NOINLINE firstEvenIsPositive1 #-}

main = print $ or $ [firstEvenIsPositive1 [1,3..n] | n <- [1..10000]]
