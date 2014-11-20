{-# LANGUAGE Safe #-}
-- | Import (now safe by default) modules.
module Main where

import Control.Monad.ST
import Control.Monad.ST.Lazy
import Foreign.ForeignPtr
import Foreign

f :: Int
f = 2

main :: IO ()
main = putStrLn $ "X is: " ++ show f

