{-# OPTIONS -fglasgow-exts #-}

{-

The following datatypes comprise the abstract syntax of a simple
imperative language. Some provisions are such that the discussion
of test-set generation is simplified. In particular, we do not 
consider anything but monomorphic *data*types --- no primitive
types, no tuples, ...

-}
 
module LittleLanguage where
 
import Data.Generics

data Prog = Prog Dec Stat 
            deriving (Show, Typeable, Data)

data Dec  = Nodec
          | Ondec Id Type 
          | Manydecs Dec Dec
            deriving (Show, Typeable, Data)

data Id = A | B
          deriving (Show, Typeable, Data)

data Type = Int | Bool
            deriving (Show, Typeable, Data)

data Stat = Noop
          | Assign Id Exp
          | Seq Stat Stat
            deriving (Show, Typeable, Data)

data Exp = Zero 
         | Succ Exp
           deriving (Show, Typeable, Data)
