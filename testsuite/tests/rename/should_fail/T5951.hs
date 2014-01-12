module T5951 where

class A a 
class B b
class C c

instance
       A =>
       B =>
       C where
         foo = undefined
