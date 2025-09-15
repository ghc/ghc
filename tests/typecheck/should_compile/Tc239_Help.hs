module Tc239_Help ( WrapIO, WrapIO2 ) where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

newtype WrapIO e a = MkWrapIO { unwrap :: IO a }

type WrapIO2 a = WrapIO String a

instance Functor (WrapIO e) where
    fmap = liftM

instance Applicative (WrapIO e) where
    pure x = MkWrapIO (return x)
    (<*>) = ap

instance Monad (WrapIO e) where

  m >>= f  = MkWrapIO (do x <- unwrap m
                          unwrap (f x) )

