{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T13971 where

class C a where
  type T a :: k
  type T a = Int
