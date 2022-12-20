{-# LANGUAGE TypeApplications, KindSignatures, DataKinds, TypeFamilies, FlexibleInstances #-}

module T22647 where

import Data.Kind

data D = D
type family F :: D -> Type

class C f where
  meth :: f

instance C (f (p :: D)) where   -- f :: D -> Type
  meth = error "urk1"

instance C (g (q :: Type)) where -- g :: Type -> Type
  meth = error "urk2"

x = meth :: F 'D

y = meth :: [Type]
