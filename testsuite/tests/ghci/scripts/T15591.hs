{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Foo where

import Data.Kind

type family T1 (x :: f (a :: Type))

class C (a :: Type) where
  type T2 (x :: f a)
