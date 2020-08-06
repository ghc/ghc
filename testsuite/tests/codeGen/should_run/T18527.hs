{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Data.Bits (setBit)
import Data.Word (Word32)
import Data.Int (Int64)

main :: IO ()
main = offending 100 0 1

offending :: Int64 -> Int64 -> Word32 -> IO ()
offending h i id = do
    oldMask <- sendMessage h (2245) i 0
    let newMask = setBit oldMask (fromIntegral id)
    sendMessage h (2244) i newMask
    return ()

foreign import ccall "func"
    sendMessage :: Int64 -> Word32 -> Int64 -> Int64 -> IO Int64
