{-# LANGUAGE TypeFamilies, InstanceSigs #-}
{-# LANGUAGE BangPatterns #-}  -- should parse even with BangPatterns enabled

module T12862 where

import Data.Kind (Type)

class Key key where
  data TotalMap key :: Type -> Type
  (!) :: TotalMap key val -> (key -> val)

instance Key Bool where
  data TotalMap Bool val = BoolMap val val
  (!) :: TotalMap Bool val -> (Bool -> val)
  (BoolMap f _) ! False = f   -- with parentheses
  BoolMap f _ ! True = f      -- without parentheses
