{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Int
import Data.List
import GHC.Prim
import GHC.Exts


-- Having a wrapper gives us two things:
-- * it's easier to test everything (no need for code using raw primops)
-- * we test the deriving mechanism for Int16#
data TestInt16 = T16 Int16#
    deriving (Eq, Ord)

mkT16 :: Int -> TestInt16
mkT16 (I# a) = T16 (narrowInt16# a)

main :: IO ()
main = do
    let input = [ (a, b) | a <- allInt16, b <- allInt16 ]

    --
    -- (==)
    --
    let expected = [ a == b | (a, b) <- input ]
        actual = [ mkT16 a == mkT16 b | (a, b) <- input ]
    checkResults "(==)" input expected actual

    --
    -- (/=)
    --
    let expected = [ a /= b | (a, b) <- input ]
        actual = [ mkT16 a /= mkT16 b | (a, b) <- input ]
    checkResults "(/=)" input expected actual

    --
    -- (<)
    --
    let expected = [ a < b | (a, b) <- input ]
        actual = [ mkT16 a < mkT16 b | (a, b) <- input ]
    checkResults "(<)" input expected actual

    --
    -- (>)
    --
    let expected = [ a > b | (a, b) <- input ]
        actual = [ mkT16 a > mkT16 b | (a, b) <- input ]
    checkResults "(>)" input expected actual

    --
    -- (<=)
    --
    let expected = [ a <= b | (a, b) <- input ]
        actual = [ mkT16 a <= mkT16 b | (a, b) <- input ]
    checkResults "(<=)" input expected actual

    --
    -- (>=)
    --
    let expected = [ a >= b | (a, b) <- input ]
        actual = [ mkT16 a >= mkT16 b | (a, b) <- input ]
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

-- testing across the entire Int16 range blows the memory,
-- hence choosing a smaller range
allInt16 :: [Int]
allInt16 = [ -50 .. 50 ]
