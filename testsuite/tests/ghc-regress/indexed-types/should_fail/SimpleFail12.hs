{-# LANGUAGE TypeFamilies, Rank2Types #-}


module ShouldFail where

type family C a :: *
-- must fail: rhs is not a tau type
type instance C Int = forall a. [a]

