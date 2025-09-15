module T20609a where

-- Declarations in this module used to be accepted by GHC
-- before `forall` became a keyword (#23719).

data MyRecord a = R { forall :: a }

x = forall (R { forall = () })
f (R { forall = r }) = r
