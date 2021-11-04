module T20609a where

-- Triggers the warning (definition/binding sites):
-- ------------------------------------------------

data MyRecord a = R { forall :: a }

-- Does not trigger the warning (use sites):
-- -----------------------------------------

x = forall (R { forall = () })
f (R { forall = r }) = r
