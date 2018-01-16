module Test.QuickCheck.Utils where

import Test.QuickCheck.Gen


-- | Adjust the size of the generated value.
--
-- In general the size gets bigger and bigger linearly. For some types
-- it is not appropriate to generate ever bigger values but instead
-- to generate lots of intermediate sized values. You could do that using:
--
-- > adjustSize (\n -> min n 5)
--
-- Similarly, for some types the linear size growth may mean getting too big
-- too quickly relative to other values. So you may want to adjust how
-- quickly the size grows. For example dividing by a constant, or even
-- something like the integer square root or log.
--
-- > adjustSize (\n -> n `div` 2)
--
-- Putting this together we can make for example a relatively short list:
--
-- > adjustSize (\n -> min 5 (n `div` 3)) (listOf1 arbitrary)
--
-- Not only do we put a limit on the length but we also scale the growth to
-- prevent it from hitting the maximum size quite so early.
--
adjustSize :: (Int -> Int) -> Gen a -> Gen a
adjustSize adjust gen = sized (\n -> resize (adjust n) gen)
