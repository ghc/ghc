{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Unboxed (Vec, MVec, Unbox) where

import Data.Data (Data(..), Constr, DataType, Fixity(..), mkConstr, mkDataType)
import Data.Typeable (Typeable)
import GHC.TypeLits (Nat)

import T21689a

data family Vec  (n :: Nat) a
data family MVec (n :: Nat) s a

class (Arity n, IVector (Vec n) a, MVector (MVec n) a) => Unbox n a

type instance Mutable (Vec n) = MVec n

type instance Dim  (Vec  n) = n
type instance DimM (MVec n) = n

instance (Unbox n a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}

instance (Typeable n, Unbox n a, Data a) => Data (Vec n a) where
  gfoldl       = gfoldl'
  gunfold      = gunfold'
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Unboxed.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix
