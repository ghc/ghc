{-# LANGUAGE UnliftedDatatypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneKindSignatures #-}

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
