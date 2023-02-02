module Main where

import Data.Char
import Data.Foldable
import Numeric

header :: String
header = "Code C P S U L A D"

predicates :: [Char -> Bool]
predicates = [
  isControl,
  isPrint,
  isSpace,
  isUpper,
  isLower,
  isAlpha,
  isDigit]

showBool :: Bool -> String
showBool True  = " T"
showBool False = " F"

-- [NOTE] The original justification for this value is unknown
-- (commit 98f34fe900bc9fe1b86e2c01345c00bf579fd0eb from 2005).
-- It is probably a typo and is meant to be “65533” (0xfffd).
maxChar :: Char
maxChar = '\6553'

padding :: Int
padding = length (show (ord maxChar))

showCode :: Char -> String
showCode c = take padding (shows (ord c) (repeat ' '))

charEntry :: Char -> String
charEntry c = showCode c <> foldMap (showBool . ($ c)) predicates

main :: IO ()
main = do
    putStrLn header
    traverse_ (putStrLn . charEntry) [ minBound .. maxChar ]
