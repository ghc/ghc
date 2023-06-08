{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}


module PromotedTypes where

import Data.Kind (Type)


data RevList a = RNil | RevList a :> a


data Pattern :: [Type] -> Type where
  Nil :: Pattern '[]
  Cons :: Maybe h -> Pattern t -> Pattern (h ': t)


-- Unlike (:), (:>) does not have to be quoted on type level.
data RevPattern :: RevList Type -> Type where
    RevNil :: RevPattern RNil
    RevCons :: Maybe h -> RevPattern t -> RevPattern (t :> h)


data Tuple :: (Type, Type) -> Type where
    Tuple :: a -> b -> Tuple '(a, b)
