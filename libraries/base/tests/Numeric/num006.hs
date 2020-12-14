-- Exercising the showing of positive numbers at various bases.
--
module Main(main) where

import Numeric
import Data.Char

--showDec :: Integral a => a -> ShowS
showDec = showInt
main =
 do
  print (map (\ x -> showOct x []) [1..32])
  print (map (\ x -> showDec x []) [1..32])
  print (map (\ x -> showHex x []) [1..32])
  print (map (\ x -> showBin x []) [1..32])
  putStrLn (showOct (241324784::Int) [])
  putStrLn (showDec (241324784::Int) [])
  putStrLn (showHex (241324784::Int) [])
  putStrLn (showBin (241324784::Int) [])
