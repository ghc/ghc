{-# LANGUAGE MagicHash #-}

module Foo( f ) where
import GHC.Exts

f True  = raise# True
f False = \p q -> raise# False




