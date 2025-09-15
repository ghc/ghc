{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TransformListComp #-}

module T25081 where

dup_last :: a %1 -> [a]
dup_last x = [ x | _ <- [0,1]]

dup_bind :: a %1 -> [()]
dup_bind x = [ () | _ <- [0,1], _ <- [x]]

dup_guard :: a %1 -> (a %1 -> Bool) -> [()]
dup_guard x g = [ () | _ <- [0,1], g x ]

guard_last :: a %1 -> [a]
guard_last x = [ x | False]

guard_bind :: a %1 -> [()]
guard_bind x = [ () | False, _ <- [x]]

guard_guard :: a %1 -> (a %1 -> Bool) %1 -> [()]
guard_guard x g = [ () | False, g x ]

-- This could, in principle, be linear. But see Note [Binding in list
-- comprehension isn't linear] in GHC.Tc.Gen.Match.
first_bind :: [()] %1 -> [Int]
first_bind xs = [ y | () <- xs, y <- [0,1]]

parallel :: a %1 -> [(a, Bool)]
parallel x = [(y,z) | y <- [x] | z <- [True]]

parallel_guard :: a %1 -> (a %1 -> Bool) -> [(Int, Bool)]
parallel_guard x g = [(y, z) | g x, y <- [0,1] | z <- [True, False]]

transform :: a %1 -> (a %1 -> Bool) -> [a]
transform x g = [y | g x, y <- [0, 1], then take 2]
