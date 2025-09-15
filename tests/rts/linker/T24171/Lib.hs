module Lib (globalInt, chooseInt) where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

foreign export ccall chooseInt :: Bool -> Int

globalInt :: IORef Int
globalInt = unsafePerformIO $ newIORef 5
{-# NOINLINE globalInt #-}

add1 :: Int -> Int
add1 x = x + 1
{-# OPAQUE add1 #-}

chooseInt :: Bool -> Int
chooseInt = unsafePerformIO $ do
  x <- readIORef globalInt
  let y = add1 x -- intentionally build a thunk
  pure (\b -> if b then y else x)
