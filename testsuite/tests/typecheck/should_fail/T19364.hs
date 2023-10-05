module T19364 where

type Foo = Bool
type Bar = String

data Pair a b = Pair a b

baz :: Pair Foo Bar
baz = Pair "yes" "no"

-- The whole point about this test is the quality
-- of the error message in baz: see #19364
