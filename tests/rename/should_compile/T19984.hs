{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module T19984 where

data D a = (:-) a a

-- promoted datacons missing promotion tick
-- (should give warnings with -fwarn-unticked-promoted-constructors)
type A1 = Int : '[]
type B1 = [Int, Bool]
type C1 = (:) Int '[]
type E1 = Int :- Bool
type F1 = (:-) Int Bool

-- promoted datacons with promotion ticks
-- (no warnings)
type A2 = Int ': '[]
type B2 = '[Int, Bool]
type C2 = '(:) Int '[]
type E2 = Int ':- Bool
type F2 = '(:-) Int Bool

-- non-promoted datacons
-- (no warnings)
data G = GA | GB
a3, b3, c3 :: [G]
a3 = GA : []
b3 = [GA, GB]
c3 = (:) GA []

e3, f3 :: D G
e3 = GA :- GB
f3 = (:-) GA GB
