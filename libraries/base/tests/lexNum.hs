module Main where

testStrings
 = [    "0x3y",
        "0X3abx",
        "0o39y",
        "0O334z",

        "NaN",
        "Infinity",

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

test s = print (lex s)

