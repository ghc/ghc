{-# OPTIONS -fglasgow-exts #-}

-- Abstract syntax of a simple functional language

module FunDatatypes where

import Data.Generics

data System     = S [Function]                     deriving (Typeable, Data)

data Function   = F Name [Equation]                deriving (Typeable, Data)

data Equation   = E [Pattern] Expression System    deriving (Typeable, Data)

data Pattern    = PVar Name
                | PTerm Name [Pattern]             deriving (Typeable, Data)

data Expression = Var Name
                | App Expression Expression
                | Lambda Name Expression           deriving (Typeable, Data)

type Name       = String

-- A little sample program

sys1   = S [f1,f2]
f1     = F "f1" [e11]
f2     = F "f2" [e21,e22]
e11    = E [] (Var "id") (S [])
e21    = E [ PTerm "C" [ PVar "x" ] ] (Var "x") (S [])
e22    = E [] (Var "id") (S [])
