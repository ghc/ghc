-- Test Trac #1930: display of infix constructors
module Test where

data Foo = Foo1 Int 
         | Int `InfixCon` Bool