{-# LANGUAGE TypeOperators, DeriveGeneric, TypeFamilies,
             FlexibleInstances, MagicHash #-}

module Main where

import Data.Kind
import GHC.Exts
import GHC.Generics hiding (C, D)
import GEq1A

-- We should be able to generate a generic representation for these types

data C = C0 | C1
  deriving Generic

data D a = D0 | D1 { d11 :: a, d12 :: (D a) }
  deriving Generic

data (:**:) a b = a :**: b
  deriving Generic

data family F a b :: Type -> Type
data instance F Int b c = F b Int c
  deriving Generic

data U a = U a Addr# Char# Double# Float# Int# Word#
  deriving Generic

data family UF a b :: Type -> Type
data instance UF Int b c = UF b c Addr# Char# Double# Float# Int# Word#
  deriving Generic

-- Example values
c0 = C0
c1 = C1

d0 :: D Char
d0 = D0
d1 = D1 'p' D0

p1 :: Int :**: Char
p1 = 3 :**: 'p'

f1 :: F Int Float Char
f1 = F 0.0 3 'h'
f2 = F 0.0 4 'h'

u0 :: U Int
u0 = U 1 "1"# '1'# 1.0## 1.0# 1# 1##
{-# NOINLINE u0 #-}

uf0 :: UF Int Int Int
uf0 = UF 2 2 "1"# '2'# 2.0## 2.0# 2# 2##
{-# NOINLINE uf0 #-}

-- Generic instances
instance                   GEq C
instance (GEq a)        => GEq (D a)
instance (GEq a, GEq b) => GEq (a :**: b)
instance (GEq b, GEq c) => GEq (F Int b c)
instance (GEq a)        => GEq (U a)
instance (GEq b, GEq c) => GEq (UF Int b c)

-- Tests
teq0 = geq c0  c1
teq1 = geq d0  d1
teq2 = geq d0  d0
teq3 = geq p1  p1
teq4 = geq f1  f2
teq5 = geq u0  u0
teq6 = geq uf0 uf0

main = mapM_ print [teq0, teq1, teq2, teq3, teq4, teq5, teq6]
