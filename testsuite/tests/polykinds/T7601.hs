{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}

module T7601 where

import Data.Kind

class C (a :: k) where
   type F (a :: k)

class Category (c :: k -> k -> Type) where
   type Ob c :: k -> Constraint
