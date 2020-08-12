{-# LANGUAGE NoFieldSelectors #-}

module NoFieldSelectors
where

import Prelude

data Foo = Foo { foo :: Int, bar :: String }

foo = 3 -- should not conflict
-- bar = 42

foo1 = Foo 3 "bar"
foo2 = Foo { foo = 3, bar = "bar" }
-- bar = foo1 { foo = 4 } -- ‘foo’ is not a record selector
