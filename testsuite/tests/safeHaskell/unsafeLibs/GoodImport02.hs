{-# LANGUAGE Safe #-}
-- | Import safe versions of unsafe modules from prelude
module Main where

import Control.Monad.ST.Safe
import Control.Monad.ST.Lazy.Safe
import Foreign.ForeignPtr.Safe
import Foreign.Safe

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

