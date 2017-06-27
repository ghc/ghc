{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnicodeSyntax #-}
module T12803a where

type Constrd a = Num a ⇒ a

data ADT a = ADT (Constrd a) ExistentiallyLost

data ExistentiallyLost = ∀ u. TC u ⇒ ExistentiallyLost u

class u ~ (ATF1 u, ATF2 u) ⇒ TC u where
    type ATF1    u ∷ *
    type ATF2    u ∷ *
    uie_handlers   ∷ ADT Int

-- Loop:
--  - ADT depends on ExistentiallyLost (also the Constrd appendage)
--  - ExistentiallyLost depends on TC
--  - TC depends on ADT
