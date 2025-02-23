{-# language AllowAmbiguousTypes #-}
{-# language TypeData #-}
{-# language DataKinds, TypeFamilyDependencies #-}

{- This test checks that
     MyEq (Var A) (Var B) --> False
     MyEq (Var A) (Var A) --> True

remembering that Var is injective.

To achieve this we need
   MyEq (Var A) (Var B)
to be apart from
   MyEq a a
o
-}
module T25657 where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))


type Tag :: Type
type data Tag = A | B


type Var :: forall {k}. Tag -> k
type family Var tag = a | a -> tag


type MyEq :: k -> k -> Bool
type family MyEq a b where
   MyEq a a = 'True
   MyEq _ _ = 'False


true :: MyEq (Var A) (Var A) :~: 'True
true = Refl


false :: MyEq (Var A) (Var B) :~: 'False
false = Refl
