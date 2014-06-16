{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

module Numerals
where

data Z          -- empty data type
data S a        -- empty data type

data SNat n where  -- natural numbers as singleton type
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)

zero  = Zero
one   = Succ zero
two   = Succ one
three = Succ two
-- etc...we really would like some nicer syntax here

type family (:+:) n m :: *
type instance Z     :+: m = m
type instance (S n) :+: m = S (n :+: m)

add :: SNat n -> SNat m -> SNat (n :+: m)
add Zero     m = m
add (Succ n) m = Succ (add n m)

