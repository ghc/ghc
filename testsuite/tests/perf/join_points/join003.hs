{-
 - A variation on join002.hs that avoids Float Out issues. The join points in
 - join002.hs may get floated to top level, which is necessary to allow in
 - general, but which makes them into functions rather than join points, thus
 - messing up the test.
 -}

module Main (
  firstMultIsPositive1, firstMultIsPositive2, firstMultIsPositive3,
  firstMultIsPositive4,

  main
) where

import Data.List (find)

divides :: Int -> Int -> Bool
p `divides` n = n `mod` p == 0

infix 4 `divides`

-- These four functions should all wind up the same; they represent successive
-- simplifications that should happen. (Actual details may vary, since find
-- isn't quite defined this way, but the differences disappear by the end.)

firstMultIsPositive1 :: Int -> [Int] -> Bool
firstMultIsPositive1 p = maybe False (> 0) . find (p `divides`)

-- After inlining:

firstMultIsPositive2 :: Int -> [Int] -> Bool
firstMultIsPositive2 p xs =
  let go xs = case xs of x:xs' -> if p `divides` x then Just x else go xs'
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

firstMultIsPositive3 :: Int -> [Int] -> Bool
firstMultIsPositive3 p xs =
  case let {-join-} go xs = case xs of x:xs' -> if p `divides` x then Just x
                                                                 else go xs'
                                       []    -> Nothing
       in go xs of
    Just n  -> n > 0
    Nothing -> False

-- After the simplifier:

firstMultIsPositive4 :: Int -> [Int] -> Bool
firstMultIsPositive4 p xs =
  let {-join-} go xs = case xs of x:xs' -> if p `divides` x then x > 0
                                                            else go xs'
                                  []    -> False
  in go xs

-- This only worked because go was a join point so that the case gets moved
-- inside.

{-# NOINLINE firstMultIsPositive1 #-}

main = print $ or $ [firstMultIsPositive1 2 [1,3..n] | n <- [1..10000]]
