{-# OPTIONS_GHC -fglasgow-exts #-}

-- Deriving Typeable has various special cases
module Foo where

import Data.Typeable

data Foo1 = Foo1 deriving( Typeable )
data Foo2 a = Foo2 a deriving( Typeable )
data Foo3 a b = Foo3 a b deriving( Typeable )

