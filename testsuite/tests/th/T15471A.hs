{-# LANGUAGE TemplateHaskell #-}
module T15471A where

import Language.Haskell.TH

foo1 x = x


test_foo :: Code Q (a -> a)
test_foo = [|| foo1 ||]


list_foo :: Code Q a -> Code Q [a]
list_foo x = [|| [ $$x, $$x ] ||]
