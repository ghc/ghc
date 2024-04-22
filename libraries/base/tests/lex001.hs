module Main where

import Text.ParserCombinators.ReadP
import qualified Text.Read.Lex

testStrings
 = [    "0x3y",
        "0X3abx",
        "0o39y",
        "0O334z",

        "NaN",
        "NaNx",
        "Infinity",
        "Infinityx",

        "Wibble Foo",
        "Wibble8_+",

        "34yy",
        "34.4x",
        "034.4x",
        "31.45e-6y",
        "49.2v",
        "049.2v",
        "35e-3x",
        "035e-3x",
        "35e+3y",
        "83.3e-22",
        "083.3e-22",

        "0b001",
        "0b100",
        "0b110",
        "0B001",
        "0B100",
        "0B110",

        "78_91",
        "678_346",
        "0x23d_fa4",
        "0X23d_fa4",
        "0o01_253",
        "0O304_367",
        "0b0101_0110",
        "0B11_010_0110"
   ]

main = mapM test testStrings

test s = do print s
            print (lex s)
            print (readP_to_S Text.Read.Lex.lex s)
            putStrLn ""

