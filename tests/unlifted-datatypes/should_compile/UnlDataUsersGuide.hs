{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ConstraintKinds #-}

module UnlDataUsersGuide where

import GHC.Exts
import GHC.Types

data UList a :: UnliftedType where
  UCons :: a -> UList a -> UList a
  UNil :: UList a

type UList2 :: Type -> UnliftedType
data UList2 a = UCons2 a (UList2 a) | UNil2

type PEither :: Type -> Type -> TYPE (BoxedRep l)
data PEither l r = PLeft l | PRight r

f :: PEither @Unlifted Int Bool -> Bool
f (PRight b) = b
f _          = False

type T :: TYPE (BoxedRep l)
data T where
  MkT :: forall l. (() :: Constraint) => T @l
  MkT2 :: Proxy# () -> T @l

t1 :: T @Lifted
t1 = MkT

t2 :: T @Lifted
t2 = MkT2 proxy#
