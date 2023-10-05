{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module NFSDRF where

import Prelude


data Foo = Foo { foo :: Int, bar :: String }
data Bar = Bar { foo :: Int, bar' :: String }

foo = 3 -- should not conflict

fooX = foo + 1

rwcPatFoo Foo{..} = show (foo, bar)
rwcConFoo = Foo{..} where
  foo = 42
  bar = "hello"

foo1 :: Foo
foo1 = Foo 3 "bar"

foo2 = Foo { foo = 3, bar = "bar" } -- disambiguate foo


-- foo3 :: Foo
-- foo3 = foo1 { foo = 4 } -- currently rejected, see #18999

foo4 = foo1 { bar = "baz" } -- unambiguous

bar0 = Bar { foo = 0, bar' = "bar'" }

-- bar1 :: Bar
-- bar1 = bar0 { foo = 1 } -- currently rejected, see #18999
