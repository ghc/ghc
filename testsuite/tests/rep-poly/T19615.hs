{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE TypeFamilies #-}
module T19615 where

import GHC.Exts

class Call (r :: RuntimeRep) where
  type Lev (a :: TYPE r)
  lift' :: (a :: TYPE r) -> (Lev a -> b) -> b
  mapF  :: forall r' (a :: TYPE r) (b :: TYPE r'). Call r' => (a -> b) -> Lev a -> Lev b

instance Call LiftedRep where
  type Lev a = a
  lift' a k = k a
  mapF f x = lift' (f x) id
