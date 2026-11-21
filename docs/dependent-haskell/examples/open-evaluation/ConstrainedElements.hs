{-# LANGUAGE ExperimentalDependentTypes, FlexibleInstances, TypeFamilies #-}
module ConstrainedElements (Allowed, Collects(..)) where

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

instance Allowed a => Collects [a] where
    type Elem [a] = a
