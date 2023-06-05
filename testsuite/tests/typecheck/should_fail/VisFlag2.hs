{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module VisFlag2 where

import Data.Kind (Type)

-- the (Type ->) parameter is to prevent instantiation of invisible variables

type family Invis :: Type -> forall a.   a
type family Vis   :: Type -> forall a -> a

type instance Vis = Invis  -- Bad
type instance Invis = Vis  -- Bad
