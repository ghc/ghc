{-# LANGUAGE TopLevelKindSignatures #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, MagicHash #-}

module TLKS_031 where

import Data.Kind
import GHC.Exts

type T1 :: Type -> TYPE 'IntRep
data family T1

newtype instance T1 a = MkT1 Int#

type T2 :: TYPE IntRep
newtype T2 = MkT2 Int#
