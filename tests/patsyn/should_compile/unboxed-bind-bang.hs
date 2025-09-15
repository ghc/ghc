{-# LANGUAGE PatternSynonyms, MagicHash, BangPatterns #-}
module ShouldCompile where

import GHC.Base

data Foo = MkFoo Int# Int#

pattern P x = MkFoo 0# x

f x = let !(P arg) = x in arg
