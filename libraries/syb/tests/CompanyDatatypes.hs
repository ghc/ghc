{-# OPTIONS -fglasgow-exts #-}

module CompanyDatatypes where

import Data.Generics (Data, Typeable)

-- The organisational structure of a company

data Company  = C [Dept]               deriving (Eq, Show, Typeable, Data)
data Dept     = D Name Manager [Unit]  deriving (Eq, Show, Typeable, Data)
data Unit     = PU Employee | DU Dept  deriving (Eq, Show, Typeable, Data)
data Employee = E Person Salary        deriving (Eq, Show, Typeable, Data)
data Person   = P Name Address         deriving (Eq, Show, Typeable, Data)
data Salary   = S Double               deriving (Eq, Show, Typeable, Data)
type Manager  = Employee
type Name     = String
type Address  = String

-- An illustrative company
genCom :: Company
genCom = C [D "Research" laemmel [PU joost, PU marlow],
            D "Strategy" blair   []]

-- A typo for the sake of testing equality;
-- (cf. lammel vs. laemmel)
genCom' :: Company
genCom' = C [D "Research" lammel [PU joost, PU marlow],
             D "Strategy" blair   []]

lammel, laemmel, joost, marlow, blair :: Employee
lammel  = E (P "Lammel" "Amsterdam") (S 8000)
laemmel = E (P "Laemmel" "Amsterdam") (S 8000)
joost   = E (P "Joost"   "Amsterdam") (S 1000)
marlow  = E (P "Marlow"  "Cambridge") (S 2000)
blair   = E (P "Blair"   "London")    (S 100000)

-- Some more test data
person1 = P "Lazy" "Home"
dept1   = D "Useless" (E person1 undefined) []
