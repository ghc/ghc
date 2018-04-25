{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- # OPTIONS_GHC -fno-defer-type-errors #-}
module T13446 where

import Data.Coerce (Coercible)
import GHC.Exts (Constraint)
import GHC.TypeLits (Symbol)

data Dict :: Constraint -> * where
  Dict :: a => Dict a

infixr 9 :-
newtype a :- b = Sub (a => Dict b)
instance a => Show (a :- b) where
  showsPrec d (Sub Dict) = showParen (d > 10) $ showString "Sub Dict"

class Lifting p f where
  lifting :: p a :- p (f a)

data Blah a = Blah

newtype J (a :: JType) = J (Blah (J a))
newtype JComparable a = JComparable (J (T (JTy a)))

instance Lifting JReference JComparable where
  lifting = Sub 'a'

class (Coercible a (J (JTy a))) => JReference a where
  type JTy a :: JType

type T a
  = 'Generic ('Iface "java.lang.Comparable") '[a]
data JType = Class Symbol
           | Generic JType [JType]
           | Iface Symbol
type JObject = J (Class "java.lang.Object")
instance JReference JObject where
  type JTy JObject = 'Class "java.lang.Object"
