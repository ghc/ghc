{-# LANGUAGE NoFieldSelectors, GHC2021 #-}
module NoFieldSelectorsFail (foo, bar) where

import NoFieldSelectorsFailA

foo1 :: Foo
foo1 = Foo 3 "bar"

bar0 = Bar { foo = 0, bar' = "bar'" }

foo3 :: Foo
foo3 = foo1 { foo = 4 } -- update

bar1 = bar0 { foo = 1 }

foo4 = foo1 { bar = "" } -- currently rejected, see #18999
