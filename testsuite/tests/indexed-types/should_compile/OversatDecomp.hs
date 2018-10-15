{-# LANGUAGE TypeFamilies, MultiParamTypeClasses #-}

module OversatDecomp where

import Data.Kind (Type)

class Blah f a where
  blah :: a -> T f f a

class A (f :: Type -> Type) where
  type T f :: (Type -> Type) -> Type -> Type

wrapper :: Blah f a => a -> T f f a
wrapper x = blah x
