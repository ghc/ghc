-- showing floats
--
module Main(main) where

import Numeric

main = 
 do
  putStrLn (showEFloat (Just 7) (1.82173691287639817263897126389712638972163e-300::Double) [])
  putStrLn (showFFloat (Just 7) (1.82173691287639817263897126389712638972163::Double) [])
  putStrLn (showGFloat (Just 7) (1.82173691287639817263897126389712638972163e-300::Double) [])
  putStrLn (showEFloat (Just 7) (1.82173691287639817263897126389712638972163e-300::Float) [])
  putStrLn (showEFloat (Just 10) (0.0::Double) [])
  putStrLn (showEFloat (Just 0) (2.3::Double) [])
  putStrLn (showEFloat Nothing (0.0::Double) [])
  putStrLn (showEFloat Nothing (3::Float) [])
  putStrLn (showEFloat Nothing (0.5::Float) [])
  putStrLn (show (floatToDigits 10 (0.0::Double)))
  putStrLn (showFFloat (Just 7) (1.82173691287639817263897126389712638972163e-300::Float) [])
  putStrLn (showGFloat (Just 7) (1.82173691287639817263897126389712638972163e-300::Float) [])

