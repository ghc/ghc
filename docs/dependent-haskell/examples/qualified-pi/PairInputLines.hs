{-# LANGUAGE Haskell2010, NoExperimentalDependentTypes #-}

-- | Pair the corresponding characters of two input lines.
module Main (main) where

import AlignedList (checkedZipWith)
import PackedVecLibrary (toList)
import System.Exit (die)
import System.IO (isEOF)

main :: IO ()
main = do
    left <- readInputLine "first"
    right <- readInputLine "second"
    case checkedZipWith left right (,) of
        Just pairs -> print (toList pairs)
        Nothing -> die ("input lengths differ (first line: " ++ show (length left)
                       ++ ", second line: " ++ show (length right) ++ ")")

-- | Read one line. An empty line is valid input.
readInputLine :: String -> IO String
readInputLine label = do
    eof <- isEOF
    if eof then die ("missing " ++ label ++ " input line") else getLine
