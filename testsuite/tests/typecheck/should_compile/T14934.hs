{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module T14934 where

import T14934a
import GHC.TypeLits

g :: Foo (1 - 0)
g = f MkFoo1
