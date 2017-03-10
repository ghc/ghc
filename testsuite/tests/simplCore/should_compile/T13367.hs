{-# LANGUAGE MagicHash #-}

module T13367( z ) where
import GHC.Exts
data T = MkT Addr#

x = MkT "foo"#
y = MkT "foo"#

z = (x,y)
