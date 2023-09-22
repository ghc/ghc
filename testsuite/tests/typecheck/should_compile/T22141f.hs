{-# LANGUAGE NoDataKinds #-}
{-# LANGUAGE LinearTypes #-}
module T22141f where

import Data.Kind (Constraint, Type)
import GHC.Exts ( CONSTRAINT, Levity(..), LiftedRep, Multiplicity(..)
                , RuntimeRep(..), TYPE, VecCount(..), VecElem(..) )

-- All of the following should be accepted without DataKinds.

type A :: Type
data A

type B :: TYPE LiftedRep
data B

type C :: Type -> Constraint
class C a

type D :: TYPE LiftedRep -> CONSTRAINT LiftedRep
class D a

type E :: Multiplicity -> Type
data E a

type E1 = Int %'One  -> Int
type E2 = Int %'Many -> Int

type F1      = TYPE 'IntRep
type F2      = TYPE 'Int8Rep
type F3      = TYPE 'Int16Rep
type F4      = TYPE 'Int32Rep
type F5      = TYPE 'Int64Rep
type F6      = TYPE 'WordRep
type F7      = TYPE 'Word8Rep
type F8      = TYPE 'Word16Rep
type F9      = TYPE 'Word32Rep
type F10     = TYPE 'Word64Rep
type F11     = TYPE 'AddrRep
type F12     = TYPE 'FloatRep
type F13     = TYPE 'DoubleRep
type F14 x   = TYPE ('TupleRep x)
type F15 x   = TYPE ('SumRep x)
type F16 x y = TYPE ('VecRep x y)
type F17 x   = TYPE ('BoxedRep x)

type G :: Levity -> Type
data G a

type G1 = 'BoxedRep 'Lifted
type G2 = 'BoxedRep 'Unlifted

type H :: VecCount -> Type
data H a

type H1 x = 'VecRep 'Vec2 x
type H2 x = 'VecRep 'Vec4 x
type H3 x = 'VecRep 'Vec8 x
type H4 x = 'VecRep 'Vec16 x
type H5 x = 'VecRep 'Vec32 x
type H6 x = 'VecRep 'Vec64 x

type I :: VecElem -> Type
data I a

type I1  x = 'VecRep x 'Int8ElemRep
type I2  x = 'VecRep x 'Int16ElemRep
type I3  x = 'VecRep x 'Int32ElemRep
type I4  x = 'VecRep x 'Int64ElemRep
type I5  x = 'VecRep x 'Word8ElemRep
type I6  x = 'VecRep x 'Word16ElemRep
type I7  x = 'VecRep x 'Word32ElemRep
type I8  x = 'VecRep x 'Word64ElemRep
type I9  x = 'VecRep x 'FloatElemRep
type I10 x = 'VecRep x 'DoubleElemRep
