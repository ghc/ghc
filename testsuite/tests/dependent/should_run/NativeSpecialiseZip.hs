{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Proxy (Proxy(..))
import NativeSpecialiseZipProvider (emptySpine, keepIndex)

-- | Pass zip to both retained binders in the same optimized client.
{-# NOINLINE check #-}
check :: [Bool] -> [()] -> Bool
check xs ys = case keepIndex (zip @Bool @() xs ys) of
    Proxy -> emptySpine (zip xs ys)

main :: IO ()
main = print (check [] undefined, check (undefined:undefined) [()])
