-- !!! Testing NumExts
module Main(main) where

import Numeric
import Data.Char

main :: IO ()
main = tst

tst :: IO ()
tst = do
  test_doubleToFloat
  test_floatToDouble
  test_showHex
  test_showOct
  test_showBin

----
-- Test data:
doubles :: [Double]
doubles = [ -1.2 , 0, 0.1, 0.5, 1.0, 1234.45454, 
           1.6053e4, 1.64022e12, 6.894e-4, 6.34543455634582173,
	   5342413403.40540423255]
ints :: [Int]
ints = [ 0, 1, 255, 65513, 6029, 1024, 256, 201357245]

integers :: [Integer]
integers = [ 0, 1, 255, 65513, 6029, 1024, 256,
	    2343243543500233, 656194962055457832]
---

test_doubleToFloat :: IO ()
test_doubleToFloat = do
  test_banner "doubleToFloat"
  putStrLn (show doubles)
  putStrLn (show $ map doubleToFloat doubles)

doubleToFloat :: Double -> Float
doubleToFloat = realToFrac

floatToDouble :: Float -> Double
floatToDouble = realToFrac

test_floatToDouble :: IO ()
test_floatToDouble = do
  test_banner "doubleToFloat"
  putStrLn (show doubles)
  putStrLn (show $ map doubleToFloat doubles)
  putStrLn (show $ map (floatToDouble.doubleToFloat) doubles)

test_showHex :: IO ()
test_showHex = do
  test_banner "showHex"
  putStrLn (show ints)
  putStrLn (showList' (map showHex ints))
  putStrLn (show integers)
  putStrLn (showList' (map showHex integers))

test_showBin :: IO ()
test_showBin = do
  test_banner "showBin"
  putStrLn (show ints)
  putStrLn (showList' (map showBin ints))
  putStrLn (show integers)
  putStrLn (showList' (map showBin integers))

showBin i = showIntAtBase 2 intToDigit i

showList' :: [ShowS] -> String
showList' [] = "[]"
showList' (x:xs) = showChar '[' . x $ showl xs ""
      where 
       showl []     = showChar ']'
       showl (x:xs) = showChar ',' . x . showl xs


test_showOct :: IO ()
test_showOct = do
  test_banner "showOct"
  putStrLn (show ints)
  putStrLn (showList' (map showOct ints))
  putStrLn (show integers)
  putStrLn (showList' (map showOct integers))

----
test_banner :: String -> IO ()
test_banner tst = do
  putStrLn $ "--------------------------------"
  putStrLn $ "--Testing " ++ tst
  putStrLn $ "--------------------------------"
