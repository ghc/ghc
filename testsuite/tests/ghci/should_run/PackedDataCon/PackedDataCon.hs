{-# OPTIONS_GHC -fbyte-code #-}

{-
  Test constructor packing in GHCi with unboxed fields of various sizes
 -}

module Main where

import qualified Obj      as O
import qualified ByteCode as B

import GHC.Exts
import GHC.Word

main :: IO ()
main = do

    -- pack a single field
    testX "D1w8"  B.showD1 O.showD1 B.d1w8  O.d1w8  (\f -> f 33)
    testX "D1i8"  B.showD1 O.showD1 B.d1i8  O.d1i8  (\f -> f 33)
    testX "D1w16" B.showD1 O.showD1 B.d1w16 O.d1w16 (\f -> f 3333)
    testX "D1i16" B.showD1 O.showD1 B.d1i16 O.d1i16 (\f -> f 3333)
    testX "D1w32" B.showD1 O.showD1 B.d1w32 O.d1w32 (\f -> f 33333333)
    testX "D1i32" B.showD1 O.showD1 B.d1i32 O.d1i32 (\f -> f 33333333)

    -- pack multiple fields
    testX "D2a" B.showD2 O.showD2 B.d2a O.d2a (\f -> f 44 55)
    testX "D2b" B.showD2 O.showD2 B.d2b O.d2b (\f -> f 44 55)
    testX "D2c" B.showD2 O.showD2 B.d2c O.d2c (\f -> f 44 5555 66)
    testX "D2d" B.showD2 O.showD2 B.d2d O.d2d (\f -> f 5555 66 7777)
    testX "D2e" B.showD2 O.showD2 B.d2e O.d2e (\f -> f 7777 66 55.55 44.44 33)

    -- pack multiple fields from literals
    testX "D2l 0" B.showD2 O.showD2 B.d2l O.d2l (\f -> f 0)
    testX "D2l 1" B.showD2 O.showD2 B.d2l O.d2l (\f -> f 1)
    testX "D2l 2" B.showD2 O.showD2 B.d2l O.d2l (\f -> f 2)
    testX "D2l 3" B.showD2 O.showD2 B.d2l O.d2l (\f -> f 3)
    testX "D2l 4" B.showD2 O.showD2 B.d2l O.d2l (\f -> f 4)

testX :: (Eq a, Show a)
      => String -> (p -> a) -> (p -> a) -> f -> f -> (f -> p) -> IO ()
testX msg a1 a2 b1 b2 ap =
    let (r:rs) = [f (ap g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ (show $ all (==r) rs) ++ " " ++ show r)
