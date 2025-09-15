{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

module Main where

import Data.Word
import Data.List (findIndex)
import GHC.Exts


-- Having a wrapper gives us two things:
-- * it's easier to test everything (no need for code using raw primops)
-- * we test the deriving mechanism for Word32#
data TestWord32 = T32 Word32#
    deriving (Eq, Ord)

mkT32 :: Word -> TestWord32
mkT32 (W# a) = T32 (wordToWord32# a)

main :: IO ()
main = do
    let input = [ (a, b) | a <- allWord32, b <- allWord32 ]

    --
    -- (==)
    --
    let expected = [ a == b | (a, b) <- input ]
        actual = [ mkT32 a == mkT32 b | (a, b) <- input ]
    checkResults "(==)" input expected actual

    --
    -- (/=)
    --
    let expected = [ a /= b | (a, b) <- input ]
        actual = [ mkT32 a /= mkT32 b | (a, b) <- input ]
    checkResults "(/=)" input expected actual

    --
    -- (<)
    --
    let expected = [ a < b | (a, b) <- input ]
        actual = [ mkT32 a < mkT32 b | (a, b) <- input ]
    checkResults "(<)" input expected actual

    --
    -- (>)
    --
    let expected = [ a > b | (a, b) <- input ]
        actual = [ mkT32 a > mkT32 b | (a, b) <- input ]
    checkResults "(>)" input expected actual

    --
    -- (<=)
    --
    let expected = [ a <= b | (a, b) <- input ]
        actual = [ mkT32 a <= mkT32 b | (a, b) <- input ]
    checkResults "(<=)" input expected actual

    --
    -- (>=)
    --
    let expected = [ a >= b | (a, b) <- input ]
        actual = [ mkT32 a >= mkT32 b | (a, b) <- input ]
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

-- testing across the entire Word32 range blows the memory,
-- hence choosing a smaller range
allWord32 :: [Word]
allWord32 = [ 0 .. 100 ]
