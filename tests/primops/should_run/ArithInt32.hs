{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Int
import Data.List (findIndex)
import GHC.Exts

main :: IO ()
main = do

    --
    -- Check if passing Int32# on the stack works (32 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allInt32
            ]
        expected =
            [ toInt32
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Int32# on the stack" input expected actual

    --
    -- negateInt32#
    --
    let input = allInt32
        expected = [ toInt32 (negate a) | a <- input ]
        actual = [ apply1 negateInt32# a | a <- input ]
    checkResults "negateInt32#" input expected actual

    --
    -- plusInt32#
    --
    let input = [ (a, b) | a <- allInt32, b <- allInt32 ]
        expected = [ toInt32 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusInt32# a b | (a, b) <- input ]
    checkResults "plusInt32#" input expected actual

    -- --
    -- -- subInt32#
    -- --
    let input = [ (a, b) | a <- allInt32, b <- allInt32 ]
        expected = [ toInt32 (a - b) | (a, b) <- input ]
        actual = [ apply2 subInt32# a b | (a, b) <- input ]
    checkResults "subInt32#" input expected actual

    --
    -- timesInt32#
    --
    let input = [ (a, b) | a <- allInt32, b <- allInt32 ]
        expected = [ toInt32 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesInt32# a b | (a, b) <- input ]
    checkResults "timesInt32#" input expected actual

    --
    -- remInt32#
    --
    let input =
            [ (a, b) | a <- allInt32, b <- allInt32
            -- Don't divide by 0 or cause overflow
            , b /= 0, not (a == -2147483648 && b == -1)
            ]
        expected = [ toInt32 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remInt32# a b | (a, b) <- input ]
    checkResults "remInt32#" input expected actual

    --
    -- quotInt32#
    --
    let input =
            [ (a, b) | a <- allInt32, b <- allInt32
            , b /= 0, not (a == -2147483648 && b == -1)
            ]
        expected = [ toInt32 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotInt32# a b | (a, b) <- input ]
    checkResults "quotInt32#" input expected actual

    --
    -- quotRemInt32#
    --
    let input =
            [ (a, b) | a <- allInt32, b <- allInt32
            , b /= 0, not (a == -2147483648 && b == -1)
            ]
        expected =
            [ (toInt32 q, toInt32 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemInt32# a b | (a, b) <- input ]
    checkResults "quotRemInt32#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

-- testing across the entire Int32 range blows the memory,
-- hence choosing a smaller range
allInt32 :: [Int]
allInt32 = [ -50 .. 50 ]

toInt32 :: Int -> Int
toInt32 a = fromIntegral (fromIntegral a :: Int32)

addMany#
    :: Int32# -> Int32# -> Int32# -> Int32#
    -> Int32# -> Int32# -> Int32# -> Int32#
    -> Int32# -> Int32# -> Int32# -> Int32#
    -> Int32# -> Int32# -> Int32# -> Int32#
    -> Int32#
addMany# a b c d e f g h i j k l m n o p =
    a `plusInt32#` b `plusInt32#` c `plusInt32#` d `plusInt32#`
    e `plusInt32#` f `plusInt32#` g `plusInt32#` h `plusInt32#`
    i `plusInt32#` j `plusInt32#` k `plusInt32#` l `plusInt32#`
    m `plusInt32#` n `plusInt32#` o `plusInt32#` p
{-# NOINLINE addMany# #-}

addMany
    :: Int -> Int -> Int -> Int
    -> Int -> Int -> Int -> Int
    -> Int -> Int -> Int -> Int
    -> Int -> Int -> Int -> Int
    -> Int
addMany (I# a) (I# b) (I# c) (I# d)
        (I# e) (I# f) (I# g) (I# h)
        (I# i) (I# j) (I# k) (I# l)
        (I# m) (I# n) (I# o) (I# p)
            = I# (int32ToInt# int32)
  where
    !int32 = addMany#
                (intToInt32# a) (intToInt32# b) (intToInt32# c) (intToInt32# d)
                (intToInt32# e) (intToInt32# f) (intToInt32# g) (intToInt32# h)
                (intToInt32# i) (intToInt32# j) (intToInt32# k) (intToInt32# l)
                (intToInt32# m) (intToInt32# n) (intToInt32# o) (intToInt32# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Int32#
apply1 :: (Int32# -> Int32#) -> Int -> Int
apply1 opToTest (I# a) = I# (int32ToInt# (opToTest (intToInt32# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Int32# -> Int32# -> Int32#) -> Int -> Int -> Int
apply2 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# intToInt32# a, intToInt32# b #)
        r = opToTest sa sb
    in I# (int32ToInt# r)
{-# NOINLINE apply2 #-}

apply3 :: (Int32# -> Int32# -> (# Int32#, Int32# #)) -> Int -> Int -> (Int, Int)
apply3 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# intToInt32# a, intToInt32# b #)
        (# ra, rb #) = opToTest sa sb
    in (I# (int32ToInt# ra), I# (int32ToInt# rb))
{-# NOINLINE apply3 #-}

instance
        (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h,
         Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o, Eq p)
      => Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
    (a1, b1, c1, d1, e1, f1, g1, h1, i1, j1, k1, l1, m1, n1, o1, p1) ==
        (a2, b2, c2, d2, e2, f2, g2, h2, i2, j2, k2, l2, m2, n2, o2, p2) =
            a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2 &&
            e1 == e2 && f1 == f2 && g1 == g2 && h1 == h2 &&
            i1 == i2 && j1 == j2 && k1 == k2 && l1 == l2 &&
            m1 == m2 && n1 == n2 && o1 == o2 && p1 == p2

instance
        (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h,
         Show i, Show j, Show k, Show l, Show m, Show n, Show o, Show p)
      => Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) where
    show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) =
        "(" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "," ++ show d ++
        "," ++ show e ++ "," ++ show f ++ "," ++ show g ++ "," ++ show h ++
        "," ++ show i ++ "," ++ show j ++ "," ++ show k ++ "," ++ show l ++
        "," ++ show m ++ "," ++ show n ++ "," ++ show o ++ "," ++ show p ++
        ")"
