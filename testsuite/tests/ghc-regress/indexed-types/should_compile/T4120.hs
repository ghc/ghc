{-# LANGUAGE Rank2Types, TypeFamilies #-}

-- Unification yielding a coercion under a forall

module Data.Vector.Unboxed where

import Control.Monad.ST ( ST )


data MVector s a = MV
data Vector a    = V

type family Mutable (v :: * -> *) :: * -> * -> *
type instance Mutable Vector = MVector

create :: (forall s. MVector s a) -> Int
create = create1

create1 :: (forall s. Mutable Vector s a) -> Int
create1 = error "urk"


