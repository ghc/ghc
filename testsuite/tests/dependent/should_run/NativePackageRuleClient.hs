{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import PackageRules (hide, open, roundTrip)
import qualified LexicalPackageRules as L

readBack :: Int -> Int
readBack x = open (hide x)

main :: IO ()
main = do
    forced <- try (evaluate (readBack (error "demanded"))) :: IO (Either ErrorCall Int)
    print (roundTrip 7, readBack 5, const 9 (readBack (error "unused")),
           either (const True) (const False) forced)
    forcedPackage <- try (evaluate (L.project (L.hideInt (error "demanded")))) :: IO (Either ErrorCall Int)
    let packed = L.hideInt 5
    print (L.local 3 packed, L.readHidden (3 :: Int, (+ 2)), L.readHidden (True, fromEnum),
           L.readHidden (undefined :: Int, const 7), L.project packed,
           L.increment (case packed of (v, f) -> f v), either (const True) (const False) forcedPackage)
