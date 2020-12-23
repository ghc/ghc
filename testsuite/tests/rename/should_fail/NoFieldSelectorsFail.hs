{-# LANGUAGE NoFieldSelectors #-}

module NoFieldSelectorsFail (foo)
where

import Prelude


data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

foo1 :: Foo
foo1 = Foo 3 "bar"

bar0 = Bar { foo = 0, bar' = "bar'" }

foo3 :: Foo
foo3 = foo1 { foo = 4 } -- update

bar1 = bar0 { foo = 1 }

bar = undefined

foo4 = foo1 { bar = "" } -- currently rejected, see #18999
