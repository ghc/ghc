{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module T20363c where

import GHC.Exts

type NilRep :: RuntimeRep
type family NilRep where
  NilRep = TupleRep '[]

type UnitTupleNT :: TYPE NilRep
newtype UnitTupleNT = MkNT (# #)

test1 :: UnitTupleNT -> Int
test1 (MkNT (# #)) = 1

test2 :: UnitTupleNT -> Int
test2 (MkNT _) = 2

test3 :: UnitTupleNT -> Int
test3 (MkNT {}) = 3

test4 :: UnitTupleNT -> Int
test4 !(MkNT {}) = 4

test5 :: UnitTupleNT -> Int
test5 !(MkNT (# #)) = 5

test6 :: UnitTupleNT -> Int
test6 x = case x of MkNT y -> case y of (# #) -> 6

type IntRepF :: RuntimeRep
type family IntRepF where
  IntRepF = IntRep

type IntNT :: TYPE IntRepF
newtype IntNT = MkIntNT Int#

test7 :: IntNT -> Int
test7 (MkIntNT x) = I# x

test8 :: Int# -> IntNT
test8 x = MkIntNT x

test9 :: IntNT -> Int
test9 nt = let !(MkIntNT x) = nt in I# x

data Color = Red | Blue

type Interpret :: Color -> RuntimeRep
type family Interpret c where
  Interpret Red  = IntRep
  Interpret Blue = WordRep

data family DF (c :: Color) :: TYPE (Interpret c)
newtype instance DF Red = MkDFRed Int#

test10 :: DF Red -> Int
test10 (MkDFRed x) = I# x
