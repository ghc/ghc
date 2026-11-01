{-# LANGUAGE MagicHash, ExtendedLiterals, TypeFamilies #-}
module T27862d where

import GHC.Exts

type family F a where F () = Int8#

f :: F () -> Int
f 300#Int8 = 1   -- overflow warning expected
f _ = 0
