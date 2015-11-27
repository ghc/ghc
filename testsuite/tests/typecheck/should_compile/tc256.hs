{-# LANGUAGE TypeFamilies, ConstraintKinds, UndecidableInstances, UndecidableSuperClasses #-}
module Ctx where

import GHC.Prim( Constraint )

type family Indirect :: * -> Constraint
type instance Indirect = Show

class Indirect a => Cls a where

foo :: Cls a => a -> String
foo = show
