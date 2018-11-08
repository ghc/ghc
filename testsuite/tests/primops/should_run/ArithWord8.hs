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
    -- Check if passing Word8# on the stack works (16 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allWord8
            ]
        expected =
            [ toWord8
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Word8# on the stack" input expected actual

    --
    -- notWord8#
    --
    let input = allWord8
        expected = [ toWord8 (complement a) | a <- input ]
        actual = [ apply1 notWord8# a | a <- input ]
    checkResults "notWord8#" input expected actual

    --
    -- plusWord8#
    --
    let input = [ (a, b) | a <- allWord8, b <- allWord8 ]
        expected = [ toWord8 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusWord8# a b | (a, b) <- input ]
    checkResults "plusWord8#" input expected actual

    --
    -- subWord8#
    --
    let input = [ (a, b) | a <- allWord8, b <- allWord8 ]
        expected = [ toWord8 (a - b) | (a, b) <- input ]
        actual = [ apply2 subWord8# a b | (a, b) <- input ]
    checkResults "subWord8#" input expected actual

    --
    -- timesWord8#
    --
    let input = [ (a, b) | a <- allWord8, b <- allWord8 ]
        expected = [ toWord8 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesWord8# a b | (a, b) <- input ]
    checkResults "timesWord8#" input expected actual

    --
    -- remWord8#
    --
    let input =
            -- Don't divide by 0.
            [ (a, b) | a <- allWord8, b <- allWord8 , b /= 0 ]
        expected = [ toWord8 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remWord8# a b | (a, b) <- input ]
    checkResults "remWord8#" input expected actual

    --
    -- quotWord8#
    --
    let input =
            [ (a, b) | a <- allWord8, b <- allWord8, b /= 0 ]
        expected = [ toWord8 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotWord8# a b | (a, b) <- input ]
    checkResults "quotWord8#" input expected actual

    --
    -- quotRemWord8#
    --
    let input =
            [ (a, b) | a <- allWord8, b <- allWord8, b /= 0 ]
        expected =
            [ (toWord8 q, toWord8 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemWord8# a b | (a, b) <- input ]
    checkResults "quotRemWord8#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

allWord8 :: [Word]
allWord8 = [ minWord8 .. maxWord8 ]

minWord8 :: Word
minWord8 = fromIntegral (minBound :: Word8)

maxWord8 :: Word
maxWord8 = fromIntegral (maxBound :: Word8)

toWord8 :: Word -> Word
toWord8 a = fromIntegral (fromIntegral a :: Word8)

addMany#
    :: Word8# -> Word8# -> Word8# -> Word8#
    -> Word8# -> Word8# -> Word8# -> Word8#
    -> Word8# -> Word8# -> Word8# -> Word8#
    -> Word8# -> Word8# -> Word8# -> Word8#
    -> Word8#
addMany# a b c d e f g h i j k l m n o p =
    a `plusWord8#` b `plusWord8#` c `plusWord8#` d `plusWord8#`
    e `plusWord8#` f `plusWord8#` g `plusWord8#` h `plusWord8#`
    i `plusWord8#` j `plusWord8#` k `plusWord8#` l `plusWord8#`
    m `plusWord8#` n `plusWord8#` o `plusWord8#` p
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
            = W# (extendWord8# word8)
  where
    !word8 =
        addMany#
            (narrowWord8# a) (narrowWord8# b) (narrowWord8# c) (narrowWord8# d)
            (narrowWord8# e) (narrowWord8# f) (narrowWord8# g) (narrowWord8# h)
            (narrowWord8# i) (narrowWord8# j) (narrowWord8# k) (narrowWord8# l)
            (narrowWord8# m) (narrowWord8# n) (narrowWord8# o) (narrowWord8# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Word8#
apply1 :: (Word8# -> Word8#) -> Word -> Word
apply1 opToTest (W# a) = W# (extendWord8# (opToTest (narrowWord8# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Word8# -> Word8# -> Word8#) -> Word -> Word -> Word
apply2 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# narrowWord8# a, narrowWord8# b #)
        r = opToTest sa sb
    in W# (extendWord8# r)
{-# NOINLINE apply2 #-}

apply3
  :: (Word8# -> Word8# -> (# Word8#, Word8# #)) -> Word -> Word -> (Word, Word)
apply3 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# narrowWord8# a, narrowWord8# b #)
        (# ra, rb #) = opToTest sa sb
    in (W# (extendWord8# ra), W# (extendWord8# rb))
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
