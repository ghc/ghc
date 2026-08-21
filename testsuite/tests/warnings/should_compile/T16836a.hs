{-# OPTIONS_GHC -Wimplicit-field-strictness #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module T16836a where

-- plain multi-constructor data
-- warns for both constructors
data T a = MkT a Bool
         | MkT2 !Int a

-- record with a shared field group
-- warns for x, y and z; not for b
data R = MkR { x, y :: Int, z :: Char, b :: !Bool }

-- infix constructor
-- warns for the first argument
data I = Int :+: !Bool

-- GADT syntax
-- warns for the first argument
data G a where
  MkG :: Int -> !Bool -> G a

-- GADT record syntax
-- warns for gx
data GR a where
  MkGR :: { gx :: Int, gy :: !Bool } -> GR a

-- data family instance
-- warns
data family F a
data instance F Int = MkF Char

-- fully annotated
-- doesn't warn
data S = MkS !Int !Bool
