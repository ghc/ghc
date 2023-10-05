{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T14934 where

import T14934a
import GHC.TypeLits

a :: Foo (1 - 0)
a = f MkFoo1

b :: Foo (CharToNat '\1')
b = g MkFoo1
