module Main where


import Data.Char
import Numeric

header = "Code C P S U L A D"

preds = [
  isControl,
  isPrint,
  isSpace,
  isUpper,
  isLower,
  isAlpha,
  isDigit]

prtBool :: Bool -> String

prtBool True  = "T "
prtBool False = "F "

showCode :: Char -> Int -> String

showCode c w = code ++ pad
  where
    code = show (ord c)
    l = length code
    spaces = map anytospace [1..]
    anytospace _ = ' '
    pad  | l >= w = ""
         | otherwise = take (w - l) spaces

charCode :: Char -> String

rapply a b = b a

charCode c = (showCode c 5) ++ (foldr1 (++) $ map prtBool $ map (rapply c) preds)

main = do
    putStrLn header
    mapM (putStrLn . charCode) [ (chr 0) .. (chr 6553) ]


