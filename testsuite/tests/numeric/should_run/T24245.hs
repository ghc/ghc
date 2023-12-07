{-# LANGUAGE DataKinds #-}
module Main where

import GHC.TypeLits
import GHC.TypeNats.Experimental
import GHC.TypeLits.Experimental

main :: IO ()
main = do
    testBinary plusSNat  (SNat @2) (SNat @3) SNat
    testBinary timesSNat (SNat @2) (SNat @3) SNat
    testBinary powerSNat (SNat @2) (SNat @3) SNat
    testBinary minusSNat (SNat @7) (SNat @3) SNat
    testBinary divSNat   (SNat @7) (SNat @3) SNat
    testBinary modSNat   (SNat @7) (SNat @3) SNat
    testUnary  log2SNat  (SNat @7)           SNat

    testBinaryS appendSSymbol (SSymbol @"foo") (SSymbol @"bar") SSymbol
    testBinaryCSS consSSymbol (SChar @'x') (SSymbol @"yz") SSymbol
    testUnaryCN sCharToSNat (SChar @'x') SNat
    testUnaryNC sNatToSChar (SNat @62) SChar

testBinary
    :: (SNat a -> SNat b -> SNat c)
    -> SNat a
    -> SNat b
    -> SNat c
    -> IO ()
testBinary f n m p = do
    print (f n m, p)
    assertEqualOnShow (f n m) p

testUnary
    :: (SNat a -> SNat b)
    -> SNat a
    -> SNat b
    -> IO ()
testUnary f n m = do
    print (f n, m)
    assertEqualOnShow (f n) m

testBinaryS
    :: (SSymbol a -> SSymbol b -> SSymbol c)
    -> SSymbol a
    -> SSymbol b
    -> SSymbol c
    -> IO ()
testBinaryS f n m p = do
    print (f n m, p)
    assertEqualOnShow (f n m) p

testBinaryCSS
    :: (SChar a -> SSymbol b -> SSymbol c)
    -> SChar a
    -> SSymbol b
    -> SSymbol c
    -> IO ()
testBinaryCSS f n m p = do
    print (f n m, p)
    assertEqualOnShow (f n m) p

testUnaryCN
    :: (SChar a -> SNat b)
    -> SChar a
    -> SNat b
    -> IO ()
testUnaryCN f n m = do
    print (f n, m)
    assertEqualOnShow (f n) m

testUnaryNC
    :: (SNat a -> SChar b)
    -> SNat a
    -> SChar b
    -> IO ()
testUnaryNC f n m = do
    print (f n, m)
    assertEqualOnShow (f n) m

assertEqualOnShow :: Show a => a -> a -> IO ()
assertEqualOnShow x y
    | show x == show y = return ()
    | otherwise        = fail "inequality"
