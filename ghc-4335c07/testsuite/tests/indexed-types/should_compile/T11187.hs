{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T11187 where
import Data.Type.Coercion

type family X

coercionXX :: Coercion X X
coercionXX = Coercion

coercionXX1 :: Coercion X X
coercionXX1 = c where
  c :: x ~ X => Coercion x x
  c = Coercion

coercionXX2 :: Coercion X X
coercionXX2 = c where c = Coercion
