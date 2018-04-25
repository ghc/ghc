module Unbound.Generics.LocallyNameless where

import Data.Typeable (Typeable)

data Name a = Name

class Alpha a where
  isTerm :: a -> Bool

instance Typeable a => Alpha (Name a) where
  isTerm _ = False
