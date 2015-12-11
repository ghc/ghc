{-# LANGUAGE TypeFamilies, ConstraintKinds, UndecidableInstances #-}
module Ctx where

import Data.Kind ( Constraint )

type family Indirect :: * -> Constraint
type instance Indirect = Show

class Indirect a => Cls a where

foo :: Cls a => a -> String
foo = show
