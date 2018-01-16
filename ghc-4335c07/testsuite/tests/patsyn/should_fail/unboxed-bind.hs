{-# LANGUAGE PatternSynonyms, MagicHash #-}
{-# OPTIONS_GHC -Wunbanged-strict-patterns -Werror=unbanged-strict-patterns #-}
module ShouldFail where

import GHC.Base

data Foo = MkFoo Int# Int#

pattern P x = MkFoo 0# x

f x = let P arg = x in arg
