{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, OverlappingInstances, UndecidableInstances, KindSignatures #-}

-- Trac #1470

module Foo where

class Sat a
class Data (ctx :: * -> *) a
instance  Sat (ctx Char)             => Data ctx Char
instance (Sat (ctx [a]), Data ctx a) => Data ctx [a]

class Data FooD a => Foo a

data FooD a = FooD

instance Foo t => Sat (FooD t)

instance Data FooD a => Foo a


instance Foo a       => Foo [a]
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

instance                Foo [Char]
