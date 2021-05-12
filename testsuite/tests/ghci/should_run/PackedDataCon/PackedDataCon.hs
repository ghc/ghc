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

    -- function arguments, not packed
    testX' "f1 "  B.f1_show O.f1_show B.f1 O.f1
    testX' "f1a"  B.f1_show O.f1_show (B.f1a_app B.f1a) (B.f1a_app B.f1a)
    testX' "f1b"  B.f1_show O.f1_show (B.f1a_app O.f1a) (B.f1a_app B.f1a)
    testX' "f1c"  B.f1_show O.f1_show (O.f1a_app B.f1a) (B.f1a_app B.f1a)
    testX' "f1d"  B.f1_show O.f1_show (O.f1a_app O.f1a) (B.f1a_app B.f1a)

    -- unboxed return values and tuples
    testX'' "unboxed1 0" B.unboxed1_a O.unboxed1_a B.unboxed1 O.unboxed1 (\f -> f 0)
    testX'' "unboxed1 1" B.unboxed1_a O.unboxed1_a B.unboxed1 O.unboxed1 (\f -> f 1)

    testX'' "unboxed2 0" B.unboxed2_a O.unboxed2_a B.unboxed2 O.unboxed2 (\f -> f 0)
    testX'' "unboxed2 1" B.unboxed2_a O.unboxed2_a B.unboxed2 O.unboxed2 (\f -> f 1)

    testX'' "unboxed3 0" B.unboxed3_a O.unboxed3_a B.unboxed3 O.unboxed3 (\f -> f 0)
    testX'' "unboxed3 1" B.unboxed3_a O.unboxed3_a B.unboxed3 O.unboxed3 (\f -> f 1)

    testX'' "tuple1"   B.tuple1_a O.tuple1_a B.tuple1 O.tuple1 (\f -> f 3)


testX :: (Eq a, Show a)
      => String -> (p -> a) -> (p -> a) -> f -> f -> (f -> p) -> IO ()
testX msg a1 a2 b1 b2 ap =
    let (r:rs) = [f (ap g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ (show $ all (==r) rs) ++ " " ++ show r)

testX' :: String -> (f -> String) -> (f -> String) -> f -> f -> IO ()
testX' msg a1 a2 b1 b2 =
    let (r:rs) = [f g | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ (show $ all (==r) rs) ++ " " ++ show r)

testX'' :: (Eq a, Show a)
        => String -> (p -> t) -> (p -> t) -> p -> p -> (t -> a) -> IO ()
testX'' msg a1 a2 b1 b2 ap =
    let (r:rs) = [ap (f g) | f <- [a1,a2], g <- [b1,b2]]
    in putStrLn (msg ++ " " ++ (show $ all (==r) rs) ++ " " ++ show r)
