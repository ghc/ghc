{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
module NativeConstrainedFamilyProvider (Collects(..), Allowed, Defaulted(..), Pair(..)) where

class Allowed a
instance Allowed Bool

class Collects c where
    type Elem c

instance Allowed a => Collects [a] where
    type Elem [a] = a

class Defaulted a where
    type Def a
    type Def a = Char

instance Defaulted Int

class Pair a b where
    type Swapped b a

instance Pair Bool Int where
    type Swapped Int Bool = Char
