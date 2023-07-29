module T20609 where

-- Declarations in this module used to be accepted by GHC
-- before `forall` became a keyword (#23719).

forall x = ()

(∀) x = ()

fparam forall = ()

asPattern forall@Nothing = ()

localLet = let forall = () in forall

{-# RULES "rule" forall forall. id forall = forall #-}

{-# RULES "rule_sig" forall a. forall (forall :: a). id forall = forall #-}

other = forall