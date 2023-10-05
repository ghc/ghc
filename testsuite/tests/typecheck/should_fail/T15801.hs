{-# Language CPP                   #-}
{-# Language QuantifiedConstraints #-}
{-# Language TypeApplications      #-}
{-# Language PolyKinds             #-}
{-# Language TypeOperators         #-}
{-# Language DataKinds             #-}
{-# Language TypeFamilies          #-}
{-# Language TypeSynonymInstances  #-}
{-# Language FlexibleInstances     #-}
{-# Language GADTs                 #-}
{-# Language UndecidableInstances  #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleContexts      #-}

module Bug where
import Data.Coerce
import Data.Kind

type Cat ob = ob -> ob -> Type

type Obj = Type

class    Coercible (op_a --> b) (b <-- op_a) => (op_a -#- b)
instance Coercible (op_a --> b) (b <-- op_a) => (op_a -#- b)

class    (forall (op_a :: obj) (b :: obj). op_a -#- b) => OpOpNoOp obj
instance (forall (op_a :: obj) (b :: obj). op_a -#- b) => OpOpNoOp obj

class
  Ríki (obj :: Obj) where
  type (-->) :: obj -> obj -> Type

  ið :: a --> (a::obj)

class
  OpOpNoOp obj
  =>
  OpRíki (obj :: Obj) where
  type (<--) :: obj -> obj -> Type

data Op a = Op a

type family UnOp op where UnOp ('Op obj) = obj

newtype Y :: Cat (Op a) where
  Y :: (UnOp b --> UnOp a) -> Y a b

instance Ríki Type where
 type (-->) = (->)
 ið x = x

instance OpRíki (Op Type) where
 type (<--) @(Op Type) = Y @Type
