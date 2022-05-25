
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnliftedNewtypes #-}

module EtaExpandDataFamily where

import Data.Kind
import GHC.Exts


type N :: forall (r :: RuntimeRep) -> TYPE r -> TYPE r
data family N r a
newtype instance N r a = MkN a

foo :: Int# -> N IntRep Int#
foo = MkN


type N :: forall (r :: RuntimeRep) -> TYPE r -> Type -> Type -> Type -> TYPE r
data family N r a i
newtype instance Ord b => N r a Int b c = MkN a

foo :: Ord b => Int# -> N IntRep Int# Int b c
foo = MkN
