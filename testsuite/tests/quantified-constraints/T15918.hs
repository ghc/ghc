{-# Language PolyKinds             #-}
{-# Language TypeFamilies          #-}
{-# Language ConstraintKinds       #-}
{-# Language FlexibleContexts      #-}
{-# Language QuantifiedConstraints #-}
{-# Language UndecidableInstances  #-}
module T15918 where

import Data.Kind

class Rev f where
  rev :: f a

instance (forall xx. cls xx => Rev xx) => Rev (Build cls) where
  rev = undefined

data Build :: ((k -> Type) -> Constraint) -> (k -> Type)

uu = rev :: Build [] a
