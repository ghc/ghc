{-# LANGUAGE PolyKinds, MultiParamTypeClasses, FlexibleContexts, DataKinds,
             TypeFamilies #-}

module T9200 where

import Data.Kind (Type, Constraint)
import Data.Proxy

------
-- test SAKS on classes

type C :: k -> k2 -> Constraint
class C f a where
  c_meth :: D a => Proxy f -> Proxy a -> ()

class C () a => D a


---------
--- test SAKS on type synonyms

data T1 a b c = MkT1 (S True b c)
data T2 p q r = MkT2 (S p 5 r)
data T3 x y q = MkT3 (S x y '())

type S :: k1 -> k2 -> k3 -> Type
type S f g h = (T1 f g h, T2 f g h, T3 f g h)


----------
-- test SAKS on closed type families

type F :: k -> k
type family F a where
  F True = False
  F False = True
  F x = x

