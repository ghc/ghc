{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification #-}

-- !!! scoped type variables w/ existential types
-- this test failed in GHC 5.00

module ShouldCompile where

data T = forall a. MkT [a]

f :: T -> T
f (MkT [t::a]) = MkT t3
    where t3::[a] = [t,t,t]
