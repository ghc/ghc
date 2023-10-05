-- !! Rexporting
module Test ( module Test , module RnAux017 ) where

import {-# SOURCE #-} RnAux017

f x = x

data Foo = MkFoo

class FOO a where
    op :: a -> Int

instance FOO Foo where
    op x = 42
