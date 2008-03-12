{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

-- Test for #1648

import Foreign
import Data.Int
import Data.Word

f :: Int64 -> IO Int64
f x = return $ x + 1

g :: Word64 -> IO Word64
g x = return $ x + 2

type WCall = Word64 -> IO Word64
foreign import ccall "wrapper" mkWCall :: WCall -> IO (FunPtr WCall)
foreign import ccall "dynamic" call_w :: FunPtr WCall -> WCall

type ICall = Int64 -> IO Int64
foreign import ccall "wrapper" mkICall :: ICall -> IO (FunPtr ICall)
foreign import ccall "dynamic" call_i :: FunPtr ICall -> ICall

main = do
  fp <- mkICall f
  call_i fp 3 >>= print
  fp <- mkWCall g
  call_w fp 4 >>= print
