{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module T11833 where

import Data.Kind (Type)

class Category (cat :: k -> k -> Type) where
  catId   :: cat a a
  catComp :: cat b c -> cat a b -> cat a c

newtype T (c :: Type -> Type -> Type) a b = MkT (c a b) deriving Category
