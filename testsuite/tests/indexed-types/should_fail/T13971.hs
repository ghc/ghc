{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
module T13971 where

import Data.Kind (Type)

class C a where
  type T a :: k
  type T @Type a = Int
