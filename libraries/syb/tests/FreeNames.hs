{-# OPTIONS -fglasgow-exts #-}

module FreeNames (tests) where

{-

This example illustrates the kind of traversals that naturally show up
in language processing. That is, the free names (say, variables) are
derived for a given program fragment. To this end, we need several
worker functions that extract declaring and referencing occurrences
from given program fragments; see "decsExpr", "decsEqua",
etc. below. Then, we need a traversal "freeNames" that traverses over
the program fragment in a bottom-up manner so that free names from
subterms do not escape to the top when corresponding declarations are
provided. The "freeNames" algorithm uses set operations "union" and
"//" to compute sets of free names from the declared and referenced
names of the root term and free names of the immediate subterms.

Contributed by Ralf Laemmel, ralf@cwi.nl

-}

import Test.Tasty.HUnit

import Data.Generics
import Data.List

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


-- Names declared in an expression
decsExpr :: Expression -> [Name]
decsExpr (Lambda n _) = [n]
decsExpr _            = []

-- Names declared in an equation
decsEqua :: Equation -> [Name]
decsEqua (E ps _ _) = everything union ([] `mkQ` pvar) ps
  where
    pvar (PVar n) = [n]
    pvar _        = []

-- Names declared in a system
decsSyst :: System -> [Name]
decsSyst (S l) = nub $ map (\(F n _) -> n) l

-- Names referenced in an expression
refsExpr :: Expression -> [Name]
refsExpr (Var n) = [n]

-- Names referenced in an equation
refsEqua :: Equation -> [Name]
refsEqua (E ps _ _) = everything union ([] `mkQ` pterm) ps
  where
    pterm (PTerm n _) = [n]
    pterm _           = []

-- Combine the above type-specific cases to obtain
-- generic functions that find declared and referenced names
--
decsFun :: Data a => a -> [Name]
decsFun =  const [] `extQ` decsExpr `extQ` decsEqua `extQ` decsSyst

refsFun :: Data a => a -> [Name]
refsFun =  const [] `extQ` refsExpr `extQ` refsEqua



{-

Free name analysis: Take the union of free names obtained from the
immediate subterms (via gmapQ) and the names being referred to at the
root of the present term, but subtract all the names that are declared
at the root.

-}

freeNames :: Data a => a -> [Name]
freeNames x = ( (refsFun x)
                `union`
                (nub . concat . gmapQ freeNames) x
              ) \\ decsFun x

{-

Print the free names for the sample program sys1; see module
FunDatatypes.hs. This should print the list ["id","C"] because the
"Prelude" function "id" is used in the sample program, and also the
term constructor "C" occurs in a pattern; we assume a language without
explicit datatype declarations ;-)

-}

tests = freeNames sys1 @=? output

output = ["id","C"]
