{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, KindSignatures #-}

-- #1470

module Foo where

import Data.Kind (Type)

class Sat a
class Data (ctx :: Type -> Type) a
instance  Sat (ctx Char)             => Data ctx Char
instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]

class Data FooD a => Foo a

data FooD a = FooD

instance Foo t => Sat (FooD t)

instance {-# OVERLAPPABLE #-} Data FooD a => Foo a
instance {-# OVERLAPS #-}     Foo a       => Foo [a]
instance {-# OVERLAPPING #-}                 Foo [Char]
{-
 Given:                Foo a,
 and its superclasses: Data FooD a

 Want superclass: Data FooD [a]

 by instance Data FooD [a]
 want:   Sat (FooD [a])
         Data FooD a      -- We have this

 by instance Sat (FooD t)
 want:   Foo [a]

BUT THIS INSTANCE OVERLAPS
-}

