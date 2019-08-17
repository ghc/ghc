{-# LANGUAGE PolyKinds, MultiParamTypeClasses, FlexibleContexts, DataKinds,
             TypeFamilies #-}

module T9200 where

import Data.Kind (Type)
import Data.Proxy

------
-- test CUSK on classes

class C (f :: k) (a :: k2) where
  c_meth :: D a => Proxy f -> Proxy a -> ()
  
class C () a => D a


---------
--- test CUSK on type synonyms
data T1 a b c = MkT1 (S True b c)
data T2 p q r = MkT2 (S p 5 r)
data T3 x y q = MkT3 (S x y '())
type S (f :: k1) (g :: k2) (h :: k3) = ((T1 f g h, T2 f g h, T3 f g h) :: Type)


----------
-- test CUSK on closed type families
type family F (a :: k) :: k where
  F True = False
  F False = True
  F x = x

