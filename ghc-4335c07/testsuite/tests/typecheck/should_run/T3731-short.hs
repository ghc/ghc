{-# LANGUAGE  DeriveDataTypeable,
              FlexibleContexts, FlexibleInstances,
              MultiParamTypeClasses,
              OverlappingInstances, UndecidableInstances,
              Rank2Types, KindSignatures, EmptyDataDecls #-}

{-# OPTIONS_GHC -Wall #-}

module Main (main) where

class Sat a where
    dict :: a -- Holds a default value

class Sat a => Data a where
    gunfold :: (forall b r. Data b => (b -> r) -> r) -> a

instance (Sat [a], Data a) => Data [a] where
   gunfold _  = []

class Data a => Default a where
    defaultValue :: a
    defaultValue = gunfold (\c -> c dict)

instance Default t => Sat t where
    dict = defaultValue

instance Default a => Default [a] where
    defaultValue = []

data Proposition = Prop Expression   
data Expression  = Conj [Expression]

instance Data Expression => Data Proposition where 
  gunfold k = k Prop

instance (Data [Expression],Sat Expression) => Data Expression where 
-- DV: Notice what happens when we remove the Sat Expression above!
--     Everything starts working!
  gunfold k = k Conj

instance Default Expression
instance Default Proposition

main :: IO () 

main = case (defaultValue :: Proposition) of 
         Prop exp -> case exp of 
                         Conj _ -> putStrLn "Hurray2!"

{-  Need Default Proposition
    for which we have an instance

Instance
   Default Proposition
needs superclass
   Data Proposition
via instance dfun, needs
   Data Expression
via instance dfun, needs
   Sat Expression
via instance dfun, needs
   Default Expression
for which we have an instance

Instance
   d1: Default Expression
needs superclass  [d1 = MkD d2 ..]
   d2: Data Expression {superclass Sat Expression}
via instance dfun, [d2 = dfun d3 d4]  needs
   d3 : Sat Expression (and d4 : Data [Expression])
via instance dfun, [d3 = dfun d5] needs
   d5 Default Expression
for which we have an instance [d5 = d1]

    d1 = MkD d2 ..
    d2 = dfun d3 d4
    d3 = dfun d1

Instance
   d1: Default Expression
needs superclass  [d1 = MkD d2 ..]
   d2: Data Expression {superclass Sat Expression  d2' = sc d2 }
via instance dfun, [d2 = dfun d3 d4]  needs
   d3 : Sat Expression (and d4 : Data [Expression])
and we can solve: d3 = d2'... no: recursion checker will reject

-}

