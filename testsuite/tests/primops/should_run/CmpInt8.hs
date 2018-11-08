{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int
import Data.List
import GHC.Prim
import GHC.Exts


-- Having a wrapper gives us two things:
-- * it's easier to test everything (no need for code using raw primops)
-- * we test the deriving mechanism for Int8#
data TestInt8 = T8 Int8#
    deriving (Eq, Ord)

mkT8 :: Int -> TestInt8
mkT8 (I# a) = T8 (narrowInt8# a)

main :: IO ()
main = do
    let input = [ (a, b) | a <- allInt8, b <- allInt8 ]

    --
    -- (==)
    --
    let expected = [ a == b | (a, b) <- input ]
        actual = [ mkT8 a == mkT8 b | (a, b) <- input ]
    checkResults "(==)" input expected actual

    --
    -- (/=)
    --
    let expected = [ a /= b | (a, b) <- input ]
        actual = [ mkT8 a /= mkT8 b | (a, b) <- input ]
    checkResults "(/=)" input expected actual

    --
    -- (<)
    --
    let expected = [ a < b | (a, b) <- input ]
        actual = [ mkT8 a < mkT8 b | (a, b) <- input ]
    checkResults "(<)" input expected actual

    --
    -- (>)
    --
    let expected = [ a > b | (a, b) <- input ]
        actual = [ mkT8 a > mkT8 b | (a, b) <- input ]
    checkResults "(>)" input expected actual

    --
    -- (<=)
    --
    let expected = [ a <= b | (a, b) <- input ]
        actual = [ mkT8 a <= mkT8 b | (a, b) <- input ]
    checkResults "(<=)" input expected actual

    --
    -- (>=)
    --
    let expected = [ a >= b | (a, b) <- input ]
        actual = [ mkT8 a >= mkT8 b | (a, b) <- input ]
    checkResults "(>=)" input expected actual

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
