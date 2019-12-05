{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

module T17202 where

type family F a

class C1 a
class (forall c. C1 c) => C2 a
class (forall b. (b ~ F a) => C2 a) => C3 a

data Dict c = c => Dict

foo :: forall a. C3 a => Dict (C1 a)
foo = Dict

{- is rejected due to #17719
bar :: forall a. C3 a => Dict (C1 a)
bar = Dict :: C2 a => Dict (C1 a)
-}
