{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module T2715 where

data Interval v where
   Intv :: (Ord v, Enum v) => (v,v) -> Interval v

type family Domain (d :: * -> *) :: * -> *
type instance Domain Interval = Interval 

type family Value (d :: * -> *) :: *


class IDomain d where
   empty   :: (Ord (Value d), Enum (Value d)) => (Domain d) (Value d)

class (IDomain d1) -- (IDomain d1, IDomain d2, Value d1 ~ Value d2) 
   => IIDomain (d1 :: * -> *) (d2 :: * -> * ) where
   equals  :: Domain d1 (Value d1) -> Domain d2 (Value d2) -> Bool


instance Ord (Value Interval) 
      => IDomain Interval where
   empty                   = Intv (toEnum 1, toEnum 0)

instance Ord (Value Interval) 
      => IIDomain Interval Interval where
   equals  (Intv ix) (Intv iy) = ix == iy
