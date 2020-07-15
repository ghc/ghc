{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T17566b where

class C f where
  type T1 (f :: k1)
  type T2 (f :: k2)
