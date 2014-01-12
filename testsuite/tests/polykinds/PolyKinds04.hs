
module PolyKinds04 where

data A f
data B = B1 (A Maybe)

-- Should fail. `A` is kind checked alone, and its kind is defaulted to * -> *.
-- The constructor `B1` then uses `A` with the wrong kind.
