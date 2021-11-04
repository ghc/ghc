module T20609 where

-- Triggers the warning (definition/binding sites):
-- ------------------------------------------------

forall x = ()

(∀) x = ()

fparam forall = ()

asPattern forall@Nothing = ()

localLet = let forall = () in forall

{-# RULES "rule" forall forall. id forall = forall #-}

{-# RULES "rule_sig" forall a. forall (forall :: a). id forall = forall #-}

-- Does not trigger the warning (use sites):
-- -----------------------------------------

other = forall
