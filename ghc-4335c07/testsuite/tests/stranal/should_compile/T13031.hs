{-# LANGUAGE MagicHash #-}

module Foo( f ) where
import GHC.Prim

f True  = raise# True
f False = \p q -> raise# False




