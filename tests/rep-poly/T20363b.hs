{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T20363b where

import GHC.Exts

data Nat = Zero | Suc Nat

type NestedTupleRep :: Nat -> RuntimeRep -> RuntimeRep
type family NestedTupleRep n r where
  NestedTupleRep Zero    r = TupleRep '[]
  NestedTupleRep (Suc n) r = TupleRep '[ r, NestedTupleRep n r ]

type NestedTuple
        :: forall ( n :: Nat )
        -> forall ( r :: RuntimeRep )
        .  forall ( a :: TYPE r )
        -> TYPE ( NestedTupleRep n r )
type family NestedTuple n a where
  NestedTuple Zero    @r a = (# #)
  NestedTuple (Suc n) @r a = (# a, NestedTuple n @r a #)

type NestedTupleNT
        :: forall ( n :: Nat )
        -> forall ( r :: RuntimeRep )
        .  forall ( a :: TYPE r )
        -> TYPE ( NestedTupleRep n r )
newtype NestedTupleNT n (a :: TYPE r) = MkNT ( NestedTuple n a )

test1a :: NestedTuple Zero Addr# -> Int
test1a (# #) = 0

test2a :: NestedTuple (Suc Zero) Addr# -> Addr#
test2a (# i, (# #) #) = i

test1b :: NestedTupleNT Zero Addr# -> Int
test1b ( MkNT (# #) ) = 0

test2b :: NestedTupleNT (Suc Zero) Addr# -> Addr#
test2b ( MkNT (# i, (# #) #) ) = i

test1c :: Int
test1c = test1b ( MkNT (# #) )

test2c :: () -> Addr#
test2c _ = test2b ( MkNT (# nullAddr#, (# #) #) )
