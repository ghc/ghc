{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module UnliftedNewtypesUnassociatedFamily where

import GHC.Int (Int(I#))
import GHC.Exts (Int#,Word#,RuntimeRep(IntRep))
import GHC.Exts (TYPE)

type KindOf (a :: TYPE k) = k
data family D (a :: TYPE r) :: TYPE r
newtype instance D a = MkWordD Word#
newtype instance D a :: TYPE (KindOf a) where
  MkIntD :: forall (a :: TYPE 'IntRep). Int# -> D a
