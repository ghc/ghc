{-# LANGUAGE TypeFamilies, ConstraintKind, UndecidableInstances #-}
module Ctx where

type family Indirect :: * -> Constraint
type instance Indirect = Show

class Indirect a => Cls a where

foo :: Cls a => a -> String
foo = show
