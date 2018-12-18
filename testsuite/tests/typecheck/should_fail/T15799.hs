{-# Language CPP               #-}
{-# Language DataKinds         #-}
{-# Language RankNTypes        #-}
{-# Language PatternSynonyms   #-}
{-# Language TypeOperators     #-}
{-# Language PolyKinds         #-}
{-# Language GADTs             #-}
{-# Language TypeFamilies      #-}
{-# Language TypeApplications  #-}
{-# Language FlexibleContexts  #-}
{-# Language FlexibleInstances #-}
{-# Language InstanceSigs      #-}

module T15799 where
import qualified GHC.TypeLits as TypeLits
import GHC.TypeLits (Nat, KnownNat)
import Data.Kind

data Op obj = Op obj

type family
 UnOp (op_a :: Op obj) :: obj where
 UnOp ('Op obj) = obj

class
 Ríki (obj :: Type) where
 type (-->) :: Op obj -> obj -> Type
 type (<--) :: obj -> Op obj -> Type

 unop :: forall (a :: obj) (b :: obj). (a <-- 'Op b) -> ('Op b --> a)

data (<=) :: Op Nat -> Nat -> Type where
  LessThan :: (KnownNat (UnOp op_a), KnownNat b, UnOp op_a TypeLits.<= b)
           => (op_a <= b)

newtype (>=) :: Nat -> Op Nat -> Type where
  Y :: (a <= b) -> (b >= a)

instance Ríki Nat where
 type (-->) = (<=)
 type (<--) = (>=)

 unop :: (a >= b) -> (b <= a)
 unop GreaterThan = LessThan

pattern GreaterThan :: () => (KnownNat (UnOp b), KnownNat a, UnOp b <= a) => a >= b
pattern GreaterThan = Y LessThan
