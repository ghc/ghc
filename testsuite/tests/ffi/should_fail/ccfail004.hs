
module Ccfail004 where

import Ccfail004A

-- Both these should be rejected as the NInt constructor isn't in scope
foreign import ccall f1 :: NInt -> IO Int
foreign import ccall f2 :: Int -> IO NInt
foreign import ccall f3 :: Int -> NIO Int

