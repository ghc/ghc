{-# LANGUAGE TypeFamilies, ConstraintKinds, UndecidableInstances, UndecidableSuperClasses #-}
module Ctx where

import Data.Kind ( Type, Constraint )

type family Indirect :: Type -> Constraint
type instance Indirect = Show

class Indirect a => Cls a where

foo :: Cls a => a -> String
foo = show
