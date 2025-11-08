{-# LANGUAGE TypeFamilies #-}
module T21092 where

import Data.Kind

type family F a

type instance F Type = Int
type instance F Constraint = Bool

-- Nov 2025: Type and Constraint are now Apart (#24279)
