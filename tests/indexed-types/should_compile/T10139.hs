{-# LANGUAGE TypeFamilies, FlexibleInstances, FlexibleContexts, UndecidableInstances,
             MultiParamTypeClasses, FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10139 where

import Data.Monoid
import Data.Kind
import Data.Coerce

class Monoid v => Measured v a | a -> v where
  _measure :: v -> a
data FingerTree v a = Dummy v a
singleton :: Measured v a => a -> FingerTree v a
singleton = undefined

class DOps a where
  plus :: a -> D a -> a

type family D a :: Type
type instance D (FingerTree (Size Int, v) (Sized a)) = [Diff (Normal a)]

type family Normal a :: Type

data Diff a = Add Int a

newtype Sized a = Sized a
newtype Size a = Size a

-- This works:
{-
instance (Measured (Size Int, v) (Sized a), Coercible (Normal a) (Sized a)) => DOps (FingerTree (Size Int, v) (Sized a)) where
  plus = foldr (\(Add index val) seq -> singleton ((coerce) val))
-}

-- This hangs:
instance (Measured (Size Int, v) (Sized a), Coercible (Normal a) (Sized a)) => DOps (FingerTree (Size Int, v) (Sized a)) where
  plus = foldr (flip f)
    where f _seq x = case x of
            Add _index val -> singleton ((coerce) val)
