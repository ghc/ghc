-- Exercising the showing of positive numbers at various bases.
--
module Main(main) where

import Numeric
import Data.Char

--showDec :: Integral a => a -> ShowS
showDec = showInt

{-
--showBinary :: Integral a => a -> ShowS
showBinary n r =
 showString "0b" $
 showIntAtBase 2 (toChr) n r
 where toChr d = chr (ord '0' + fromIntegral d)
-}

main =
 do
  print (map (\ x -> showOct x []) [1..32])
  print (map (\ x -> showDec x []) [1..32])
  print (map (\ x -> showHex x []) [1..32])
--  print (map (\ x -> showBinary x []) [1..32])
  putStrLn (showOct (241324784::Int) [])
  putStrLn (showDec (241324784::Int) [])
  putStrLn (showHex (241324784::Int) [])
---  putStrLn (showBinary (241324784::Int) [])
