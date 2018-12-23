{-# LANGUAGE KindSignatures, FlexibleInstances, GADTs, DataKinds #-}
module Bug923 where

-- | A promoted tuple type
data T :: (* -> (*,*)) -> * where
  T :: a -> T ('(,) a)

-- | A promoted tuple type in an instance
instance Eq a => Eq (T ('(,) a)) where
  T x == T y = x == y

