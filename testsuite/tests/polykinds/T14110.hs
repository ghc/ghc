{-# Language TypeFamilies, ScopedTypeVariables, PolyKinds, DataKinds #-}

import Data.Kind

class R (c :: k -> Constraint) where
  type R_ (c :: k -> Constraint) :: k -> Type

instance R Eq where
  type R_ Eq a = a -> a -> Bool
