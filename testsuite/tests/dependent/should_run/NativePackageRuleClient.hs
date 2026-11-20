{-# LANGUAGE NoExperimentalDependentTypes #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import PackageRules (hide, open, roundTrip)

readBack :: Int -> Int
readBack x = open (hide x)

main :: IO ()
main = do
    forced <- try (evaluate (readBack (error "demanded"))) :: IO (Either ErrorCall Int)
    print (roundTrip 7, readBack 5, const 9 (readBack (error "unused")),
           either (const True) (const False) forced)
