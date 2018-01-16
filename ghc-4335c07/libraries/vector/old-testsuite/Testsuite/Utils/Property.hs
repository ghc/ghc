{-# LANGUAGE TypeFamilies, FlexibleContexts, GeneralizedNewtypeDeriving #-}
module Testsuite.Utils.Property (
  Modelled(..), EqTestable(..), (==?), A, B, C,

  Ty_A(..), Ty_B(..), Ty_C(..), Ty_Bool(..), Ty_Int(..), Ty_Fn(..),
  arg_ty, arg2_ty, arg3_ty,

  arg, arg2, arg3, args2, args3
) where

import Test.QuickCheck

newtype A = A_ Int deriving (Eq, Ord, Show, Arbitrary)
newtype B = B_ Int deriving (Eq, Ord, Show, Arbitrary)
newtype C = C_ Int deriving (Eq, Ord, Show, Arbitrary)

class Modelled a where
  type Model a

  model :: a -> Model a
  unmodel :: Model a -> a

instance (Modelled a, Modelled b) => Modelled (a -> b) where
  type Model (a -> b) = Model a -> Model b

  model   f = model   . f . unmodel
  unmodel f = unmodel . f . model

instance Modelled Int where
  type Model Int = Int
  model = id
  unmodel = id

instance Modelled Bool where
  type Model Bool = Bool
  model = id
  unmodel = id

instance Modelled A where
  type Model A = A

  model = id
  unmodel = id

instance Modelled B where
  type Model B = B
  model = id
  unmodel = id

instance Modelled C where
  type Model C = C
  model = id
  unmodel = id

instance Modelled a => Modelled (Maybe a) where
  type Model (Maybe a) = Maybe (Model a)

  model = fmap model
  unmodel = fmap unmodel

instance Modelled Ordering where
  type Model Ordering = Ordering

  model = id
  unmodel = id

class Testable (EqTest a) => EqTestable a where
  type EqTest a

  (===) :: a -> a -> EqTest a

instance (Arbitrary a, Show a, EqTestable b) => EqTestable (a -> b) where
  type EqTest (a -> b) = a -> EqTest b

  (f === g) x = f x === g x

instance EqTestable Int where
  type EqTest Int = Bool
  (===) = (==)

instance EqTestable Bool where
  type EqTest Bool = Bool
  (===) = (==)

instance EqTestable A where
  type EqTest A = Bool
  (===) = (==)

instance EqTestable B where
  type EqTest B = Bool
  (===) = (==)

instance EqTestable C where
  type EqTest C = Bool
  (===) = (==)

instance Eq a => EqTestable [a] where
  type EqTest [a] = Bool
  (===) = (==)

instance Eq a => EqTestable (Maybe a) where
  type EqTest (Maybe a) = Bool
  (===) = (==)

instance EqTestable Ordering where
  type EqTest Ordering = Bool
  (===) = (==)

(==?) :: (Modelled a, EqTestable (Model a)) => a -> Model a -> EqTest (Model a)
x ==? y = model x === y

data Ty_A = A
data Ty_B = B
data Ty_C = C
data Ty_Bool = Bool
data Ty_Int = Int
data Ty_Fn a b = a :-> b
infixr 0 :->

type family Ty a
type instance Ty Ty_A = A
type instance Ty Ty_B = B
type instance Ty Ty_C = C
type instance Ty Ty_Bool = Bool
type instance Ty Ty_Int = Int
type instance Ty (a,b) = (Ty a, Ty b)
type instance Ty [a]  = [Ty a]
type instance Ty (Ty_Fn a b) = Ty a -> Ty b

arg_ty :: a -> (Ty a -> b) -> (Ty a -> b)
arg_ty _ = id

arg2_ty :: b -> (a -> Ty b -> c) -> (a -> Ty b -> c)
arg2_ty _ = id

arg3_ty :: c -> (a -> b -> Ty c -> d) -> (a -> b -> Ty c -> d)
arg3_ty _ = id

arg :: Testable b => (a -> Bool) -> (a -> b) -> a -> Property
arg p f x = p x ==> f x

arg2 :: Testable c => (b -> Bool) -> (a -> b -> c) -> a -> b -> Property
arg2 p f x y = p y ==> f x y

arg3 :: Testable d => (c -> Bool) -> (a -> b -> c -> d)
                   -> a -> b -> c -> Property
arg3 p f x y z = p z ==> f x y z

args2 :: Testable c => (a -> b -> Bool) -> (a -> b -> c) -> a -> b -> Property
args2 p f x y = p x y ==> f x y

args3 :: Testable d => (a -> b -> c -> Bool) -> (a -> b -> c -> d)
                    -> a -> b -> c -> Property
args3 p  f x y z = p x y z ==> f x y z

