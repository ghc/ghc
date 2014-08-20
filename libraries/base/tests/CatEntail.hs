{-# LANGUAGE ConstraintKinds, GADTs, RankNTypes #-}
{-# LANGUAGE TypeOperators, KindSignatures #-}
module CatEntail where
import Prelude hiding (id, (.))
import GHC.Prim (Constraint)
import Control.Category

-- One dictionary to rule them all.
data Dict :: Constraint -> * where
  Dict :: ctx => Dict ctx

-- Entailment.
-- Note the kind 'Constraint -> Constraint -> *'
newtype (|-) a b = Sub (a => Dict b)

(\\) :: a => (b => r) -> (a |- b) -> r
r \\ Sub Dict = r

reflexive :: a |- a
reflexive = Sub Dict

transitive :: (b |- c) -> (a |- b) -> a |- c
transitive f g = Sub $ Dict \\ f \\ g

instance Category (|-) where
  id  = reflexive
  (.) = transitive
