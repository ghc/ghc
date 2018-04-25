{-# LANGUAGE TypeOperators, TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
    -- The type of 'empty' is indeed ambiguous

module T2544 where

data (:|:) a b = Inl a | Inr b

class Ix i where
   type IxMap i :: * -> *
   empty  :: IxMap i [Int]

data BiApp a b c = BiApp (a c) (b c)

instance (Ix l, Ix r) => Ix (l :|: r) where
   type IxMap (l :|: r) = BiApp (IxMap l) (IxMap r)
   empty = BiApp empty empty

-- [W] w1: a c ~ IxMap ii1 [Int]                  (from first 'empty')
-- [W] w2: b c ~ IxMap ii2 [Int]                  (from second 'empty')
-- [W] w3: BiApp a b c ~ IxMap (l :|: r) [Int]    (from call of BiApp
--           ~ BiApp (IxMap l) (IxMap r) [Int]

-- If we process w3 first, we'll rewrite it with w1, w2
-- yielding two constraints (Ix io ~ IxMap l, Ix i1 ~ IxMap r)
-- both with location of w3.  Then we report just one of them,
-- because we suppress multiple errors from the same location
--
-- But if we process w1,w2 first, we'll get the same constraints
-- but this time with different locations.
