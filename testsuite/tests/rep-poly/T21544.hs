
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T21544 where

import Data.Kind
import GHC.Exts


type N1 :: forall (r :: RuntimeRep) -> TYPE r -> TYPE r
data family N1 r a
newtype instance N1 r a = MkN1 a

foo1 :: Int# -> N1 IntRep Int#
foo1 = MkN1


type N2 :: forall (r :: RuntimeRep) -> TYPE r -> Type -> Type -> Type -> TYPE r
data family N2 r a i
newtype instance Ord b => N2 r a Int b c = MkN2 a

foo2 :: Ord b => Int# -> N2 IntRep Int# Int b c
foo2 = MkN2
