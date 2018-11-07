{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T15592b where

import Data.Kind

class C a where
  type T (x :: (f :: k -> Type) a)
