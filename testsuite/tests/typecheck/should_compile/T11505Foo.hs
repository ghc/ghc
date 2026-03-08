module T11505Foo where

import T11505Bar

-- #11505: this used to fail with:
--
-- T11505Foo.hs:12:1:
--     Type constructor `Foo' has conflicting definitions in the module
--     and its hs-boot file
--     Main module: data Foo = Foo {x :: {-# UNPACK #-} !Int}
--     Boot file:   data Foo = Foo {x :: !Int}
data Foo = Foo { x :: {-# UNPACK #-} !Int }
