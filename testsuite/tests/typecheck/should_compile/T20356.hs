{-# LANGUAGE TypeFamilies, PolyKinds, ConstraintKinds #-}

module T20356 where

import GHC.Types

type family Id (a :: k -> Constraint) :: l -> Constraint
type instance Id @k @k f = f

type T :: Constraint -> Constraint
type T = Id Eq

data Proxy p = MkProxy

id' :: f a -> f a
id' x = x

z = id' (MkProxy @T)
