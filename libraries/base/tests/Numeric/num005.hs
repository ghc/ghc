-- Exercising Numeric.readSigned a bit
--
module Main(main) where

import Numeric

main =
  let
   ls = ["3489348394032498320438240938403","0","-1","1","34323","2L","012","0x23","3243ab"]
   present str f ls =
    sequence (map (\ v -> putStr ('\n':str ++
                                  ' ': v   ++
                                  " = "    ++
                                  (show (f v)))) ls)
  in
  do
   present "(readDec::ReadS Integer)" (readDec::ReadS Integer) ls
   present "(readDec::ReadS Int)"     (readDec::ReadS Int) ls
   present "(readOct::ReadS Integer)" (readOct::ReadS Integer) ls
   present "(readOct::ReadS Int)"     (readOct::ReadS Int) ls
   present "(readHex::ReadS Integer)" (readHex::ReadS Integer) ls
   present "(readHex::ReadS Int)"     (readHex::ReadS Int) ls
   putStrLn ""
