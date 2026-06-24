{-# LANGUAGE LazyFieldAnnotations #-}
{-# LANGUAGE GADTs #-}

-- | LazyFieldAnnotations accepts the prefix ~ annotation in every
-- constructor-field position.
module LazyFieldAnnotations where

-- Haskell-98 prefix and record fields
data A = A ~Int Bool
data B = B { b1 :: ~Int, b2 :: !Bool }

-- GADT argument types and record fields
data C where
  C1 :: ~Int -> C
  C2 :: { c1 :: ~Int, c2 :: !Bool } -> C
