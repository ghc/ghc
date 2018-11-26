{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Word
import Data.Bits
import Data.List
import GHC.Prim
import GHC.Exts

main :: IO ()
main = do

    --
    -- Check if passing Word16# on the stack works (16 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allWord16
            ]
        expected =
            [ toWord16
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Word16# on the stack" input expected actual

    --
    -- notWord16#
    --
    let input = allWord16
        expected = [ toWord16 (complement a) | a <- input ]
        actual = [ apply1 notWord16# a | a <- input ]
    checkResults "notWord16#" input expected actual

    --
    -- plusWord16#
    --
    let input = [ (a, b) | a <- allWord16, b <- allWord16 ]
        expected = [ toWord16 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusWord16# a b | (a, b) <- input ]
    checkResults "plusWord16#" input expected actual

    --
    -- subWord16#
    --
    let input = [ (a, b) | a <- allWord16, b <- allWord16 ]
        expected = [ toWord16 (a - b) | (a, b) <- input ]
        actual = [ apply2 subWord16# a b | (a, b) <- input ]
    checkResults "subWord16#" input expected actual

    --
    -- timesWord16#
    --
    let input = [ (a, b) | a <- allWord16, b <- allWord16 ]
        expected = [ toWord16 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesWord16# a b | (a, b) <- input ]
    checkResults "timesWord16#" input expected actual

    --
    -- remWord16#
    --
    let input =
            -- Don't divide by 0.
            [ (a, b) | a <- allWord16, b <- allWord16 , b /= 0 ]
        expected = [ toWord16 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remWord16# a b | (a, b) <- input ]
    checkResults "remWord16#" input expected actual

    --
    -- quotWord16#
    --
    let input =
            [ (a, b) | a <- allWord16, b <- allWord16, b /= 0 ]
        expected = [ toWord16 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotWord16# a b | (a, b) <- input ]
    checkResults "quotWord16#" input expected actual

    --
    -- quotRemWord16#
    --
    let input =
            [ (a, b) | a <- allWord16, b <- allWord16, b /= 0 ]
        expected =
            [ (toWord16 q, toWord16 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemWord16# a b | (a, b) <- input ]
    checkResults "quotRemWord16#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

-- testing across the entire Word16 range blows the memory,
-- hence choosing a smaller range
allWord16 :: [Word]
allWord16 = [ 0 .. 100 ]

toWord16 :: Word -> Word
toWord16 a = fromIntegral (fromIntegral a :: Word16)

addMany#
    :: Word16# -> Word16# -> Word16# -> Word16#
    -> Word16# -> Word16# -> Word16# -> Word16#
    -> Word16# -> Word16# -> Word16# -> Word16#
    -> Word16# -> Word16# -> Word16# -> Word16#
    -> Word16#
addMany# a b c d e f g h i j k l m n o p =
    a `plusWord16#` b `plusWord16#` c `plusWord16#` d `plusWord16#`
    e `plusWord16#` f `plusWord16#` g `plusWord16#` h `plusWord16#`
    i `plusWord16#` j `plusWord16#` k `plusWord16#` l `plusWord16#`
    m `plusWord16#` n `plusWord16#` o `plusWord16#` p
{-# NOINLINE addMany# #-}

addMany
    :: Word -> Word -> Word -> Word
    -> Word -> Word -> Word -> Word
    -> Word -> Word -> Word -> Word
    -> Word -> Word -> Word -> Word
    -> Word
addMany (W# a) (W# b) (W# c) (W# d)
        (W# e) (W# f) (W# g) (W# h)
        (W# i) (W# j) (W# k) (W# l)
        (W# m) (W# n) (W# o) (W# p)
            = W# (extendWord16# word16)
  where
    !word16 =
        addMany#
            (narrowWord16# a) (narrowWord16# b) (narrowWord16# c) (narrowWord16# d)
            (narrowWord16# e) (narrowWord16# f) (narrowWord16# g) (narrowWord16# h)
            (narrowWord16# i) (narrowWord16# j) (narrowWord16# k) (narrowWord16# l)
            (narrowWord16# m) (narrowWord16# n) (narrowWord16# o) (narrowWord16# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Word16#
apply1 :: (Word16# -> Word16#) -> Word -> Word
apply1 opToTest (W# a) = W# (extendWord16# (opToTest (narrowWord16# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Word16# -> Word16# -> Word16#) -> Word -> Word -> Word
apply2 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# narrowWord16# a, narrowWord16# b #)
        r = opToTest sa sb
    in W# (extendWord16# r)
{-# NOINLINE apply2 #-}

apply3
  :: (Word16# -> Word16# -> (# Word16#, Word16# #)) -> Word -> Word -> (Word, Word)
apply3 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# narrowWord16# a, narrowWord16# b #)
        (# ra, rb #) = opToTest sa sb
    in (W# (extendWord16# ra), W# (extendWord16# rb))
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
