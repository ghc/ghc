{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
module T13398b where

import GHC.TypeLits

class C a where
  type T a (b :: Bool) :: a

instance C Nat where
  type T Nat 'True  = 1
  type T Nat 'False = 0
