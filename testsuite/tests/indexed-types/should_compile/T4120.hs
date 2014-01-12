{-# LANGUAGE RankNTypes, TypeFamilies #-}

-- Unification yielding a coercion under a forall

module Data.Vector.Unboxed where

import Control.Monad.ST ( ST )


data MVector s a = MV
data Vector a    = V

type family Mutable (v :: * -> *) :: * -> * -> *
type instance Mutable Vector = MVector

create :: (forall s. MVector s a) -> Int
create = create1
-- Here we get  Couldn't match expected type `forall s. MVector s a'
--                          with actual type `forall s. Mutable Vector s a1'
-- Reason: when unifying under a for-all we don't solve type 
--         equalities.  Think more about this.

create1 :: (forall s. Mutable Vector s a) -> Int
create1 = error "urk"


