{-# LANGUAGE TypeFamilies, ConstraintKinds, UndecidableInstances #-}
module Ctx where

import GHC.Prim( Constraint )

type family Indirect :: * -> Constraint
type instance Indirect = Show

class Cls a where
    f :: a -> String

instance Indirect a => Cls [a] where
    f = show
