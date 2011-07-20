module Tc239_Help ( WrapIO, WrapIO2 ) where

newtype WrapIO e a = MkWrapIO { unwrap :: IO a }

type WrapIO2 a = WrapIO String a

instance Monad (WrapIO e) where
  return x = MkWrapIO (return x)

  m >>= f  = MkWrapIO (do x <- unwrap m
                          unwrap (f x) )

  fail str = error str