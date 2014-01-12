
module Ccfail004A (NInt, NIO) where

newtype NInt = NInt Int

newtype NIO a = NIO (IO a)

