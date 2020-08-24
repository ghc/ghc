{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language PolyKinds #-}
{-# Language StandaloneKindSignatures #-}
{-# Language TypeFamilyDependencies #-}
{-# Language UndecidableInstances #-}
{-# Language UndecidableSuperClasses #-}
module T18585 (Functor(..)) where

import Data.Kind (Type)
import Prelude hiding (Functor(..))

type Cat i = i -> i -> Type

class
  ( Op (Op k) ~ k
  , Category (Op k)
  ) => Category (k :: Cat i) where
  type Op k :: i -> i -> Type
  type Op k = Y k

newtype Y k a b = Y (k b a)

instance (Category k, Op k ~ Y k) => Category (Y k) where
  type Op (Y k) = k

instance Category (->)

type SelfDom :: (i -> j) -> Cat i -> Cat i
type family SelfDom (f :: i -> j) (k :: Cat i) :: Cat i where
  SelfDom p p = Op p
  SelfDom f p = p

type family DefaultCat (i :: Type) = (res :: Cat i) | res -> i
type instance DefaultCat Type = (->)

class
  ( Category (Dom f)
  , Category (Cod f)
  ) => Functor (f :: i -> j) where

  type Dom f :: Cat i
  type Dom (f :: i -> j) = SelfDom f (DefaultCat i)

  type Cod f :: Cat j
  type Cod (f :: i -> j) = DefaultCat j

instance Functor IO
