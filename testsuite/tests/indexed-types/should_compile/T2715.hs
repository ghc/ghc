{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
    -- The type of 'empty' is indeed ambiguous

module T2715 where

import Data.Kind (Type)

data Interval v where
   Intv :: (Ord v, Enum v) => (v,v) -> Interval v

type family Domain (d :: Type -> Type) :: Type -> Type
type instance Domain Interval = Interval 

type family Value (d :: Type -> Type) :: Type

class IDomain d where
   empty   :: (Ord (Value d), Enum (Value d)) => Domain d (Value d)

class (IDomain d1) -- (IDomain d1, IDomain d2, Value d1 ~ Value d2) 
   => IIDomain (d1 :: Type -> Type) (d2 :: Type -> Type ) where
   equals  :: Domain d1 (Value d1) -> Domain d2 (Value d2) -> Bool


instance Ord (Value Interval) 
      => IDomain Interval where
   empty = Intv (toEnum 1, toEnum 0)

instance Ord (Value Interval) 
      => IIDomain Interval Interval where
   equals  (Intv ix) (Intv iy) = ix == iy
