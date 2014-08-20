-- Exercising the reading of positive numbers at various bases.
--
module Main(main) where

import Numeric

main =
 do
  putStrLn (show (readOct "00000111"))
  putStrLn (show (readDec "00000111"))
  putStrLn (show (readHex "00000111"))
  putStrLn (show (readOct "-24"))
  putStrLn (show (readDec "-24"))
  putStrLn (show (readHex "-24"))
  putStrLn (show ((readOct ::ReadS Integer) "3248784372843778438743"))
  putStrLn (show ((readDec ::ReadS Integer) "3248784372843778438743"))
  putStrLn (show ((readHex ::ReadS Integer) "3248784372843778438743"))
