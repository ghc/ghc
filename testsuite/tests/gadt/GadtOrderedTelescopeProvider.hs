{-# LANGUAGE GADTs, RequiredTypeArguments #-}
module GadtOrderedTelescopeProvider where

data Mixed a where
  Mixed :: !Int -> forall a. Eq a => a -> Mixed a

-- | Supply equality between two fields of the same type.
data FieldCheck a where
  FieldCheck :: forall a. a -> Eq a => a -> FieldCheck a

data Reversed a b where
  Reversed :: forall a. Int -> Eq a => forall b. (a,b) -> Reversed a b

data Several a b where
  Several :: forall a. Show a => Int -> Eq a => a -> forall b. Show b => b -> Several a b

data Tag a where
  Tag :: forall a -> a -> Tag a

data Layered a where
  Layered :: forall a. Maybe () -> forall b. (a ~ b) => Tag b -> (b ~ Int) => Tag a -> Layered a

data StoredOrder a where
  StoredOrder :: Ord a => StoredOrder a

data Refined a where
  Refined :: forall a. StoredOrder a -> forall b. (a ~ b) => b -> Show b => Bool -> Refined a

-- Previously rejected cases from T12087.
data F3 a where
  MkF3 :: forall a. Eq a => a -> forall b. Eq b => b -> F3 a

data F4 a where
  MkF4 :: forall a b. Eq a => a -> Eq b => b -> F4 a

data F5 a where
  MkF5 :: Int -> Int -> forall a. a -> Int -> Int -> forall b. b -> F5 a
