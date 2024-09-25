{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module T25115 where

import T25115a ( ABC )

-- Check that we don't suggest to use the 'Foo' pattern synonym from
-- T25115a, as it is not imported (even though the import of T25115a
-- has brought into scope all COMPLETE pragmas from that module).

foo :: Bool -> Int
foo = \case {}

bar :: Bool -> Int
bar = \case
  True -> 3

baz :: Ordering -> Int
baz = \case
  EQ -> 5

-- Check that we do still suggest something for ABC, even though
-- all constructors are out of scope.

quux :: ABC -> Int
quux = \case {}
