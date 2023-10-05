{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Main where

import Data.Word
import Data.Bits
import Data.List (findIndex)
import GHC.Exts

main :: IO ()
main = do

    --
    -- Check if passing Word32# on the stack works (32 parameter function will
    -- need to use stack for some of the them)
    --
    let input =
            [ ( (a + 0), (a + 1), (a + 2), (a + 3),
                (a + 4), (a + 5), (a + 6), (a + 7),
                (a + 8), (a + 9), (a + 10), (a + 11),
                (a + 12), (a + 13), (a + 14), (a + 15) )
            | a <- allWord32
            ]
        expected =
            [ toWord32
                  (a + b + c + d + e + f + g + h +
                   i + j + k + l + m + n + o + p)
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
        actual =
            [ addMany a b c d e f g h i j k l m n o p
            | (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) <- input
            ]
    checkResults "passing Word32# on the stack" input expected actual

    --
    -- notWord32#
    --
    let input = allWord32
        expected = [ toWord32 (complement a) | a <- input ]
        actual = [ apply1 notWord32# a | a <- input ]
    checkResults "notWord32#" input expected actual

    --
    -- plusWord32#
    --
    let input = [ (a, b) | a <- allWord32, b <- allWord32 ]
        expected = [ toWord32 (a + b) | (a, b) <- input ]
        actual = [ apply2 plusWord32# a b | (a, b) <- input ]
    checkResults "plusWord32#" input expected actual

    --
    -- subWord32#
    --
    let input = [ (a, b) | a <- allWord32, b <- allWord32 ]
        expected = [ toWord32 (a - b) | (a, b) <- input ]
        actual = [ apply2 subWord32# a b | (a, b) <- input ]
    checkResults "subWord32#" input expected actual

    --
    -- timesWord32#
    --
    let input = [ (a, b) | a <- allWord32, b <- allWord32 ]
        expected = [ toWord32 (a * b) | (a, b) <- input ]
        actual = [ apply2 timesWord32# a b | (a, b) <- input ]
    checkResults "timesWord32#" input expected actual

    --
    -- remWord32#
    --
    let input =
            -- Don't divide by 0.
            [ (a, b) | a <- allWord32, b <- allWord32 , b /= 0 ]
        expected = [ toWord32 (a `rem` b) | (a, b) <- input ]
        actual = [ apply2 remWord32# a b | (a, b) <- input ]
    checkResults "remWord32#" input expected actual

    --
    -- quotWord32#
    --
    let input =
            [ (a, b) | a <- allWord32, b <- allWord32, b /= 0 ]
        expected = [ toWord32 (a `quot` b) | (a, b) <- input ]
        actual = [ apply2 quotWord32# a b | (a, b) <- input ]
    checkResults "quotWord32#" input expected actual

    --
    -- quotRemWord32#
    --
    let input =
            [ (a, b) | a <- allWord32, b <- allWord32, b /= 0 ]
        expected =
            [ (toWord32 q, toWord32 r)  | (a, b) <- input
            , let (q, r) = a `quotRem` b
            ]
        actual = [ apply3 quotRemWord32# a b | (a, b) <- input ]
    checkResults "quotRemWord32#" input expected actual


checkResults
    :: (Eq a, Eq b, Show a, Show b) => String -> [a] -> [b] -> [b] -> IO ()
checkResults test inputs expected actual =
    case findIndex (\(e, a) -> e /= a) (zip expected actual) of
        Nothing -> putStrLn $ "Pass: " ++ test
        Just i -> error $
            "FAILED: " ++ test ++ " for input: " ++ show (inputs !! i)
              ++ " expected: " ++ show (expected !! i)
              ++ " but got: " ++ show (actual !! i)

-- testing across the entire Word32 range blows the memory,
-- hence choosing a smaller range
allWord32 :: [Word]
allWord32 = [ 0 .. 100 ]

toWord32 :: Word -> Word
toWord32 a = fromIntegral (fromIntegral a :: Word32)

addMany#
    :: Word32# -> Word32# -> Word32# -> Word32#
    -> Word32# -> Word32# -> Word32# -> Word32#
    -> Word32# -> Word32# -> Word32# -> Word32#
    -> Word32# -> Word32# -> Word32# -> Word32#
    -> Word32#
addMany# a b c d e f g h i j k l m n o p =
    a `plusWord32#` b `plusWord32#` c `plusWord32#` d `plusWord32#`
    e `plusWord32#` f `plusWord32#` g `plusWord32#` h `plusWord32#`
    i `plusWord32#` j `plusWord32#` k `plusWord32#` l `plusWord32#`
    m `plusWord32#` n `plusWord32#` o `plusWord32#` p
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
            = W# (word32ToWord# word32)
  where
    !word32 =
        addMany#
            (wordToWord32# a) (wordToWord32# b) (wordToWord32# c) (wordToWord32# d)
            (wordToWord32# e) (wordToWord32# f) (wordToWord32# g) (wordToWord32# h)
            (wordToWord32# i) (wordToWord32# j) (wordToWord32# k) (wordToWord32# l)
            (wordToWord32# m) (wordToWord32# n) (wordToWord32# o) (wordToWord32# p)
{-# NOINLINE addMany #-}

-- Convenient and also tests higher order functions on Word32#
apply1 :: (Word32# -> Word32#) -> Word -> Word
apply1 opToTest (W# a) = W# (word32ToWord# (opToTest (wordToWord32# a)))
{-# NOINLINE apply1 #-}

apply2 :: (Word32# -> Word32# -> Word32#) -> Word -> Word -> Word
apply2 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# wordToWord32# a, wordToWord32# b #)
        r = opToTest sa sb
    in W# (word32ToWord# r)
{-# NOINLINE apply2 #-}

apply3
  :: (Word32# -> Word32# -> (# Word32#, Word32# #)) -> Word -> Word -> (Word, Word)
apply3 opToTest (W# a) (W# b) =
    let (# sa, sb #) = (# wordToWord32# a, wordToWord32# b #)
        (# ra, rb #) = opToTest sa sb
    in (W# (word32ToWord# ra), W# (word32ToWord# rb))
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
