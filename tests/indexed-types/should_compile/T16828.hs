{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Bug where

import Data.Kind

data family Foo :: Type -> Type

-- This declaration is fine
newtype instance Foo :: Type -> Type where
  MkFoo :: a -> Foo a
