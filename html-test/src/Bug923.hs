{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE KindSignatures, FlexibleInstances, GADTs, DataKinds #-}
module Bug923 where

import Data.Kind (Type)

-- | A promoted tuple type
data T :: (Type -> (Type,Type)) -> Type where
  T :: a -> T ('(,) a)

-- | A promoted tuple type in an instance
instance Eq a => Eq (T ('(,) a)) where
  T x == T y = x == y

