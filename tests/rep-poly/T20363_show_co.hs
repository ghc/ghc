{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T20363_show_co where

import GHC.Exts

type NilRep :: RuntimeRep
type family NilRep where
  NilRep = TupleRep '[]

type UnitTupleNT :: TYPE NilRep
newtype UnitTupleNT = MkNT (# #)

test1b :: UnitTupleNT -> Int
test1b ( MkNT (# #) ) = 0
