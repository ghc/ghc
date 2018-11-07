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
    -- Check if passing Int8# on the stack works (16 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allInt8
            ]
        expected =
            [ toInt8
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Int8# on the stack" input expected actual

    --
    -- negateInt8#
    --
    let input = allInt8
        expected = [ toInt8 (negate a) | a <- input ]
        actual = [ apply1 negateInt8# a | a <- input ]
    checkResults "negateInt8#" input expected actual

    --
    -- plusInt8#
    --
    let input = [ (a, b) | a <- allInt8, b <- allInt8 ]
        expected = [ toInt8 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusInt8# a b | (a, b) <- input ]
    checkResults "plusInt8#" input expected actual

    --
    -- subInt8#
    --
    let input = [ (a, b) | a <- allInt8, b <- allInt8 ]
        expected = [ toInt8 (a - b) | (a, b) <- input ]
        actual = [ apply2 subInt8# a b | (a, b) <- input ]
    checkResults "subInt8#" input expected actual

    --
    -- timesInt8#
    --
    let input = [ (a, b) | a <- allInt8, b <- allInt8 ]
        expected = [ toInt8 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesInt8# a b | (a, b) <- input ]
    checkResults "timesInt8#" input expected actual

    --
    -- remInt8#
    --
    let input =
            [ (a, b) | a <- allInt8, b <- allInt8
            -- Don't divide by 0 or cause overflow
            , b /= 0, not (a == -128 && b == -1)
            ]
        expected = [ toInt8 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remInt8# a b | (a, b) <- input ]
    checkResults "remInt8#" input expected actual

    --
    -- quotInt8#
    --
    let input =
            [ (a, b) | a <- allInt8, b <- allInt8
            , b /= 0, not (a == -128 && b == -1)
            ]
        expected = [ toInt8 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotInt8# a b | (a, b) <- input ]
    checkResults "quotInt8#" input expected actual

    --
    -- quotRemInt8#
    --
    let input =
            [ (a, b) | a <- allInt8, b <- allInt8
            , b /= 0, not (a == -128 && b == -1)
            ]
        expected =
            [ (toInt8 q, toInt8 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemInt8# a b | (a, b) <- input ]
    checkResults "quotRemInt8#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

allInt8 :: [Int]
allInt8 = [ minInt8 .. maxInt8 ]

minInt8 :: Int
minInt8 = fromIntegral (minBound :: Int8)

maxInt8 :: Int
maxInt8 = fromIntegral (maxBound :: Int8)

toInt8 :: Int -> Int
toInt8 a = fromIntegral (fromIntegral a :: Int8)

addMany#
    :: Int8# -> Int8# -> Int8# -> Int8#
    -> Int8# -> Int8# -> Int8# -> Int8#
    -> Int8# -> Int8# -> Int8# -> Int8#
    -> Int8# -> Int8# -> Int8# -> Int8#
    -> Int8#
addMany# a b c d e f g h i j k l m n o p =
    a `plusInt8#` b `plusInt8#` c `plusInt8#` d `plusInt8#`
    e `plusInt8#` f `plusInt8#` g `plusInt8#` h `plusInt8#`
    i `plusInt8#` j `plusInt8#` k `plusInt8#` l `plusInt8#`
    m `plusInt8#` n `plusInt8#` o `plusInt8#` p
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
            = I# (extendInt8# int8)
  where
    !int8 = addMany#
                (narrowInt8# a) (narrowInt8# b) (narrowInt8# c) (narrowInt8# d)
                (narrowInt8# e) (narrowInt8# f) (narrowInt8# g) (narrowInt8# h)
                (narrowInt8# i) (narrowInt8# j) (narrowInt8# k) (narrowInt8# l)
                (narrowInt8# m) (narrowInt8# n) (narrowInt8# o) (narrowInt8# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Int8#
apply1 :: (Int8# -> Int8#) -> Int -> Int
apply1 opToTest (I# a) = I# (extendInt8# (opToTest (narrowInt8# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Int8# -> Int8# -> Int8#) -> Int -> Int -> Int
apply2 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# narrowInt8# a, narrowInt8# b #)
        r = opToTest sa sb
    in I# (extendInt8# r)
{-# NOINLINE apply2 #-}

apply3 :: (Int8# -> Int8# -> (# Int8#, Int8# #)) -> Int -> Int -> (Int, Int)
apply3 opToTest (I# a) (I# b) =
    let (# sa, sb #) = (# narrowInt8# a, narrowInt8# b #)
        (# ra, rb #) = opToTest sa sb
    in (I# (extendInt8# ra), I# (extendInt8# rb))
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
