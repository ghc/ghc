module Main where

import Text.ParserCombinators.ReadP
import qualified Text.Read.Lex

testStrings
 = [    "0x3y",
        "0X3abx",
        "0x1p+1x",
        "0x1p+8x",
        "0x1.b7p-1x",
        "0x1.fffffffffffffp+1023x",
        "0X1.921FB4D12D84AP-1x",
        "0x.Ap4x",
        "0xABx",
        "0x1a.3x",
        "0x1p-4x",
        "0x1.2p3x",
        "0x1p3x",
        "0x1p5000x",
        "0x1p-5000x",

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
        "083.3e-22"
   ]

main = mapM test testStrings

test s = do print s
            print (lex s)
            print (readP_to_S Text.Read.Lex.lex s)
            putStrLn ""

