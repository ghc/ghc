{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fglasgow-exts #-}

module ShouldFail where

type family C a :: *
-- must fail: rhs is not a tau type
type instance C Int = forall a. [a]

