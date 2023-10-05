module Foo
(
  -- The reference to chunk2 should show up in the -ddump-parsed output.
  -- $chunk1
  -- $chunk2
  foo,
  -- $chunk3
  bar
)
where

{- $chunk1
This is chunk 1.
-}

{- $chunk2
This is chunk 2.
-}

{- $chunk3
This is chunk 3.
-}

foo = 3
bar = 7
