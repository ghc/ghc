module Main where

import B

data MyDataType = MyDataType String Int deriving Show

instance TypeClass MyDataType where
    printA = putStrLn . show

main :: IO ()
main = do
    let myValue = MyDataType "haha" 99
    sz <- getSize myValue
    putStrLn $ show sz
    printA myValue
