{-# LANGUAGE GADTs, KindSignatures, PatternSynonyms, FlexibleContexts #-}
module ShouldCompile where

import Data.Kind (Type)

data X :: (Type -> Type) -> Type -> Type where
  Y :: (Show a) => f a -> X f (Maybe a)

pattern C :: (Show (a, Bool)) => a -> X Maybe (Maybe (a, Bool))
pattern C x = Y (Just (x, True))
