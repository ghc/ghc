{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE TypeFamilies           #-}

module T7601 where

import GHC.Exts

class C (a :: k) where
   type F (a :: k)

class Category (c :: k -> k -> *) where
   type Ob c :: k -> Constraint