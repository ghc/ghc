{-# LANGUAGE DataKinds #-}
module SI36_A ( C1, C2, C3 ) where

import Data.Kind
import GHC.TypeLits

type C1 :: Symbol -> Constraint
class C1 s

type C2 :: Symbol -> Constraint
class C2 s

type C3 :: Symbol -> Constraint
class C3 s
