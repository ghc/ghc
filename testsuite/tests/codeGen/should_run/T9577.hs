{-# LANGUAGE MagicHash #-}

import T9577_A

import GHC.Exts (Ptr(..), Addr#)

main = print (foo == Ptr "foo"#)
