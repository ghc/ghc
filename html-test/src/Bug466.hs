{-# LANGUAGE DataKinds, TypeFamilies, StarIsType #-}
module Bug466 where

class Cl a where
  type Fam a :: [*]

data X = X
instance Cl X where
  type Fam X = '[Char]
