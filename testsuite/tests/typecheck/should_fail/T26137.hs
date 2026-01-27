{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
module T26137 where

import Control.Monad.Trans.Reader(Reader)
import GHC.Generics (Generically(Generically))

class MyClass a where
  foo :: Reader () a

newtype T = MkT { unT :: () }
  deriving MyClass via (Generically T)
instance MyClass (Generically a)
