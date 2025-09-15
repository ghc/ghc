{-# LANGUAGE QuantifiedConstraints, MultiParamTypeClasses,
             KindSignatures, FlexibleInstances, TypeFamilies #-}

module T17564 where

import Data.Kind

class (forall (a :: Type -> Type). a b ~ a c) => C b c
instance C a a

class (b ~ c) => D b c
instance D a a

foo :: C a b => a -> b
foo = undefined

bar = foo

food :: D a b => a -> b
food = undefined

bard = food
