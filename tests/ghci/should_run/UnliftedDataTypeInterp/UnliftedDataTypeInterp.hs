{-# OPTIONS_GHC -fbyte-code #-}

module Main where

{-
  Test pattern matching on unlifted data types in ghci
 -}

import Data.Foldable (forM_)

import qualified Obj      as O
import qualified ByteCode as B
import Types

main :: IO ()
main = do
    testO O.showT0_1
    testB B.showT0_1
    testO O.showT1I
    testB B.showT1I
    testO O.showT0_2
    testB B.showT0_2
    testO O.showT2BI
    testB B.showT2BI
    testO O.showT0_3
    testB B.showT0_3
    testO O.showT3CIB
    testB B.showT3CIB
    testO O.showT0_4
    testB B.showT0_4

    -- testing calls between BCO and object code (object code function with an unlifted
    -- value allocated from bytecode and vice-versa)
    let a = testX [1..7] O.t B.show_inc
    let b = testX [1..7] B.t O.show_inc
    putStrLn "____"
    print $ a == b
    putStrLn "____"
    putStrLn "Obj data Bytecode function"
    forM_ a putStrLn
    putStrLn "Bytecode data Object function"
    forM_ b putStrLn


testO v = putStrLn $ "Obj: " ++ v
testB v = putStrLn $ "Bc: " ++  v

testX :: [Int] -> (Int -> T) -> (Int -> (Int -> T) -> String) -> [String]
testX is get_T show_inc_T = map (\i -> show_inc_T i get_T) is
