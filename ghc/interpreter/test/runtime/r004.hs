--!!! Testing Read (assuming that Eq, Show and Enum work!)

module TestRead where

import Ratio(Ratio,(%),Rational)
import List(zip4,zip5,zip6,zip7)

-- test that expected equality holds
tst :: (Read a, Show a, Eq a) => a -> Bool
tst x = read (show x) == x

-- measure degree of error
diff :: (Read a, Show a, Num a) => a -> a
diff x = read (show x) - x

----------------------------------------------------------------
-- test for hand-written instances
----------------------------------------------------------------

test1 = tst ()
test2 = all tst [False,True]
test3 = all tst [minBound::Char ..]
test4 = all tst [Nothing, Just (Just True)]
test5 = all tst [Left True, Right (Just True)]
test6 = all tst [LT .. GT]
test7 = all tst [[],['a'..'z'],['A'..'Z']]
test8 = all tst $ [minBound,maxBound] 
                  ++ [-100..100 :: Int]
test9 = all tst $ [(fromInt minBound)-1, (fromInt maxBound)+1]
                  ++ [-100..100 :: Integer]

-- we don't test fractional Floats/Doubles because they don't work
test10 = all tst $ [-100..100 :: Float]
test11 = all tst $ [-100..100 :: Double]

test12 = all tst $ [-2%2,-1%2,0%2,1%2,2%2]
                   ++ [-10.0,-9.9..10.0 :: Ratio Int]
test13 = all tst $ [-2%2,-1%2,0%2,1%2,2%2]
                   ++ [-10.0,-9.9..10.0 :: Rational]

----------------------------------------------------------------
-- test for derived instances
----------------------------------------------------------------

-- Tuples

test21 = all tst $      [-1..1]
test22 = all tst $ zip  [-1..1] [-1..1]
test23 = all tst $ zip3 [-1..1] [-1..1] [-1..1]
test24 = all tst $ zip4 [-1..1] [-1..1] [-1..1] [-1..1]
test25 = all tst $ zip5 [-1..1] [-1..1] [-1..1] [-1..1] [-1..1]
{- Not derived automatically
test26 = all tst $ zip6 [-1..1] [-1..1] [-1..1] [-1..1] [-1..1] [-1..1]
test27 = all tst $ zip7 [-1..1] [-1..1] [-1..1] [-1..1] [-1..1] [-1..1] [-1..1]
-}

-- Enumeration

data T1 = C1 | C2 | C3 | C4 | C5 | C6 | C7 
  deriving (Eq, Ord, Enum, Read, Show)

test30 = all tst [C1 .. C7]

-- Records

data T2 = A Int | B {x,y::Int, z::Bool} | C Bool
  deriving (Eq, Read, Show)

test31 = all tst [A 1, B 1 2 True, C True]

-- newtype

newtype T3 = T3 Int
  deriving (Eq, Read, Show)

test32 = all tst [ T3 i | i <- [-10..10] ]

----------------------------------------------------------------
-- Random tests for things which have failed in the past
----------------------------------------------------------------

test100 = read "(True)" :: Bool

test101 = tst  (pi :: Float)
test102 = diff (pi :: Float)

test103 = tst  (pi :: Double)
test104 = diff (pi :: Double)



