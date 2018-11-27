{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Int
import Data.List
import GHC.Prim
import GHC.Exts

main :: IO ()
main = do

    --
    -- Check if passing Int16# on the stack works (16 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allInt16
            ]
        expected =
            [ toInt16
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Int16# on the stack" input expected actual

    --
    -- negateInt16#
    --
    let input = allInt16
        expected = [ toInt16 (negate a) | a <- input ]
        actual = [ apply1 negateInt16# a | a <- input ]
    checkResults "negateInt16#" input expected actual

    --
    -- plusInt16#
    --
    let input = [ (a, b) | a <- allInt16, b <- allInt16 ]
        expected = [ toInt16 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusInt16# a b | (a, b) <- input ]
    checkResults "plusInt16#" input expected actual

    -- --
    -- -- subInt16#
    -- --
    let input = [ (a, b) | a <- allInt16, b <- allInt16 ]
        expected = [ toInt16 (a - b) | (a, b) <- input ]
        actual = [ apply2 subInt16# a b | (a, b) <- input ]
    checkResults "subInt16#" input expected actual

    --
    -- timesInt16#
    --
    let input = [ (a, b) | a <- allInt16, b <- allInt16 ]
        expected = [ toInt16 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesInt16# a b | (a, b) <- input ]
    checkResults "timesInt16#" input expected actual

    --
    -- remInt16#
    --
    let input =
            [ (a, b) | a <- allInt16, b <- allInt16
            -- Don't divide by 0 or cause overflow
            , b /= 0, not (a == -32768 && b == -1)
            ]
        expected = [ toInt16 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remInt16# a b | (a, b) <- input ]
    checkResults "remInt16#" input expected actual

    --
    -- quotInt16#
    --
    let input =
            [ (a, b) | a <- allInt16, b <- allInt16
            , b /= 0, not (a == -32768 && b == -1)
            ]
        expected = [ toInt16 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotInt16# a b | (a, b) <- input ]
    checkResults "quotInt16#" input expected actual

    --
    -- quotRemInt16#
    --
    let input =
            [ (a, b) | a <- allInt16, b <- allInt16
            , b /= 0, not (a == -32768 && b == -1)
            ]
        expected =
            [ (toInt16 q, toInt16 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemInt16# a b | (a, b) <- input ]
    checkResults "quotRemInt16#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

-- testing across the entire Int16 range blows the memory,
-- hence choosing a smaller range
allInt16 :: [Int]
allInt16 = [ -50 .. 50 ]

toInt16 :: Int -> Int
toInt16 a = fromIntegral (fromIntegral a :: Int16)

addMany#
    :: Int16# -> Int16# -> Int16# -> Int16#
    -> Int16# -> Int16# -> Int16# -> Int16#
    -> Int16# -> Int16# -> Int16# -> Int16#
    -> Int16# -> Int16# -> Int16# -> Int16#
    -> Int16#
addMany# a b c d e f g h i j k l m n o p =
    a `plusInt16#` b `plusInt16#` c `plusInt16#` d `plusInt16#`
    e `plusInt16#` f `plusInt16#` g `plusInt16#` h `plusInt16#`
    i `plusInt16#` j `plusInt16#` k `plusInt16#` l `plusInt16#`
    m `plusInt16#` n `plusInt16#` o `plusInt16#` p
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
            = I# (extendInt16# int16)
  where
    !int16 = addMany#
                (narrowInt16# a) (narrowInt16# b) (narrowInt16# c) (narrowInt16# d)
                (narrowInt16# e) (narrowInt16# f) (narrowInt16# g) (narrowInt16# h)
                (narrowInt16# i) (narrowInt16# j) (narrowInt16# k) (narrowInt16# l)
                (narrowInt16# m) (narrowInt16# n) (narrowInt16# o) (narrowInt16# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Int16#
apply1 :: (Int16# -> Int16#) -> Int -> Int
apply1 opToTest (I# a) = I# (extendInt16# (opToTest (narrowInt16# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Int16# -> Int16# -> Int16#) -> Int -> Int -> Int
apply2 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# narrowInt16# a, narrowInt16# b #)
        r = opToTest sa sb
    in I# (extendInt16# r)
{-# NOINLINE apply2 #-}

apply3 :: (Int16# -> Int16# -> (# Int16#, Int16# #)) -> Int -> Int -> (Int, Int)
apply3 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# narrowInt16# a, narrowInt16# b #)
        (# ra, rb #) = opToTest sa sb
    in (I# (extendInt16# ra), I# (extendInt16# rb))
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
