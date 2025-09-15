{-# LANGUAGE GADTs, KindSignatures, PatternSynonyms #-}
module ShouldCompile where

import Data.Kind (Type)

data X :: (Type -> Type) -> Type -> Type where
  Y :: f a -> X f (Maybe a)

pattern C :: a -> X Maybe (Maybe a)
pattern C x = Y (Just x)

