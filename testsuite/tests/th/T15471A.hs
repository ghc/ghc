{-# LANGUAGE TemplateHaskell #-}
module T15471A where

import Language.Haskell.TH

foo1 x = x


test_foo :: Q (TExp (a -> a))
test_foo = [|| foo1 ||]


list_foo :: Q (TExp a) -> Q (TExp [a])
list_foo x = [|| [ $$x, $$x ] ||]
