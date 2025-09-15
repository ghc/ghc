
module Ccfail004 where

import Ccfail004A

newtype NT = NT [NT]

-- All these should be rejected as the newtype constructors aren't in scope
foreign import ccall f1 :: NInt -> IO Int
foreign import ccall f2 :: Int -> IO NInt
foreign import ccall f3 :: Int -> NIO Int

-- Both these should be rejected as NT is recursive
foreign import ccall f4 :: NT -> IO ()
foreign import ccall f5 :: IO NT

