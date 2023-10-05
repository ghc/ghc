{-# OPTIONS_GHC -Wno-missing-methods #-}

-- Pattern match checking is broken for overloaded rationals currently.

module T15646a where


foo_small :: ()
foo_small = case 2e2 :: Rational of
  2e1 -> () -- redundant
  2e2 -> ()

-- Large exponents are handled differently so we have an extra check.
foo :: ()
foo = case 2e102 :: Rational of
  2e101 -> () -- redundant
  2e102 -> ()

-- Any literal of type T will desugar to the same value MkT.
-- Eg. (1.0 :: T) == MkT
data T = MkT deriving Eq
instance Num T where
instance Fractional T where
  fromRational _ = MkT

bar :: ()
bar = case 2e102 :: T of
  2e101 -> () -- not redundant, pattern evaluates to MkT
  2e102 -> () -- redundant, pattern also evaluates to MkT

baz :: ()
baz = case 2e1 :: T of
  2e1 -> () -- not redundant, pattern evaluates to MkT
  2e2 -> () -- redundant, pattern also evaluates to MkT
