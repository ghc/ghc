{-# OPTIONS -fglasgow-exts #-}

{-

The following examples illustrate the reification facilities for type
structure. Most notably, we generate shallow terms using the depth of
types and constructors as means to steer the generation.

-}

module Main where
import Data.Maybe
import Data.Generics
import Control.Monad.State
import CompanyDatatypes



-- Build a shallow term 
shallowTerm :: GenericR Maybe -> GenericB
shallowTerm cust
  =
    maybe gdefault id cust

 where

  -- The worker, also used for type disambiguation
  gdefault = case con of
              Just (con, Just _) -> gunfoldB con (shallowTerm cust)
              _ -> error "no shallow term!"

  -- The type to be constructed
  typeVal = val2type gdefault

  -- The most shallow constructor if any 
  con = depthOfType (const True) typeVal



-- For testing shallowTerm
shallowTermBase :: GenericR Maybe
shallowTermBase =        Nothing 
                  `extR` Just (1.23::Float)
                  `extR` Just ("foo"::String)



-- Sample datatypes
data T1 = T1a               deriving (Typeable, Data) -- just a constant
data T2 = T2 T1             deriving (Typeable, Data) -- little detour
data T3 = T3a T3 | T3b T2   deriving (Typeable, Data) -- recursive case
data T4 = T4 T3 T3          deriving (Typeable, Data) -- sum matters



-- Sample type arguments
t0 = typeVal :: TypeVal Int
t1 = typeVal :: TypeVal T1
t2 = typeVal :: TypeVal T2
t3 = typeVal :: TypeVal T3
t4 = typeVal :: TypeVal T4
tCompany = typeVal :: TypeVal Company
tPerson = typeVal :: TypeVal Person
tEmployee = typeVal :: TypeVal Employee
tDept = typeVal :: TypeVal Dept



-- Test cases
test0   = t1 `reachableType` t1
test1   = t1 `reachableType` t2
test2   = t2 `reachableType` t1
test3   = t1 `reachableType` t3
test4   = tPerson `reachableType` tCompany
test5   = gcountSubtermTypes tPerson
test6   = gcountSubtermTypes tEmployee
test7   = gcountSubtermTypes tDept
test8   = shallowTerm shallowTermBase :: Person
test9   = shallowTerm shallowTermBase :: Employee
test10  = shallowTerm shallowTermBase :: Dept



main = print $ (   test0
               , ( test1
               , ( test2
               , ( test3
               , ( test4
               , ( test5
               , ( test6
               , ( test7
               , ( test8
               , ( test9
               , ( test10
               )))))))))))
