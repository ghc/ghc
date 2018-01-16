{-# LANGUAGE MultiParamTypeClasses #-}

module ShouldFail where

data T a = MkT

class C a b where
  op :: T a -> T b -> Bool

-- Repeated type variable in an instance constraint
-- should require UndecidableInstances
instance C a a => Eq (T a) where
  (==) = op
