{-# LANGUAGE TypeFamilies #-}

module T22560a where

import Data.Kind
import Data.Proxy

-- Invisible binders in type declarations with standalone kind signatures:
--
--   1. data
--   2. newtype
--   3. type
--   4. class
--   5. type family
--   6. data family
--
-- Properties that we test:
--
--   a) the @-binder syntax is accepted in type declaration headers
--   b) the bound variable is in scope in subsequent binders and on the RHS
--   c) the name of the variable in the standalone kind signature and in the
--      type declaration header need not coincide (we use k/j)

type D :: forall k. k -> Type
data D @j (a :: j) = MkD (Proxy j) (Proxy a)

type N :: forall k. k -> Type
newtype N @j (a :: j) = MkN (Proxy a -> Proxy j)

type S :: forall k. k -> Type
type S @j (a :: j) = Proxy a -> Proxy j

type C :: forall k. k -> Constraint
class C @j (a :: j) where
  f :: Proxy a -> Proxy j

type F :: forall k. k -> k
type family F @j (a :: j) where
  F a = a