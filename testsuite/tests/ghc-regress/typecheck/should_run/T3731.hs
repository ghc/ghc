{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, RankNTypes, ScopedTypeVariables, KindSignatures, EmptyDataDecls, NoMonomorphismRestriction #-}

module Main where

data Proxy (a :: * -> *)
data DefaultD a  = DefaultD { defaultValueD :: a }
data Proposition = Proposition Expression 
data Expression  = Conjunction Expression deriving ( Show )

----------- Sat -----------------
class Sat a where
   dict :: a

instance Default t => Sat (DefaultD t) where
   dict = error "Sat (DefaultD t) not implemented"

----------- Data -----------------
class (Sat (ctx a)) => Data ctx a where
    gunfold :: Proxy ctx
            -> (forall b r. Data ctx b => c (b -> r) -> c r)
            -> (forall r. r -> c r)
            -> c a

-- Change Data ctx [Expression] to Data ctx Expression and main works.
instance ( Data ctx [Expression]
         , Sat (ctx Expression)
         ) => Data ctx Expression

instance (Sat (ctx [a]),Data ctx a) => Data ctx [a]

instance Data DefaultD Proposition  where
   gunfold _ k z = k (z Proposition)
--    gunfold _ k z c = error "gunfold"

------------- Default ----------------

class (Data DefaultD a) => Default a where
   defaultValue :: a

instance Default a => Default [a] where
   defaultValue = error "Default [a] not implemented"

instance Default Proposition
instance Default Expression

----------------- Code to run that will show up the bug ------------
mydict :: DefaultD Expression
mydict = dict 

e :: Expression
e = defaultValueD mydict

main = print e

---------------------------------------------------------------


{-      What is going on (mainly notes to Simon)
    [Uniques refer to ~simonpj/tmp/T3731/T3731-612.stg]

defn of gunfold in instance DeafultD Proposition 
   needs (Data DefaultD Expression) {aHe}
        has superclasses   (Typeable Expression, Sat (DefaultD Expression))

   Data DefaultD Expression {aHe} needs
        (Data DefaultD [Expression] {aIe}, Sat (DefaultD Expression) {aIf})

      Data DefaultD [Expression] {aIe}        superclasses   Sat (DefaultD [Expression])
        needs       
        (Sat (DefaultD [Expression]) {aIg}, Data DefaultD Expression {aIh} [rec])

        Sat (DefaultD [Expression]) needs   
            Default [Expression]              superclass Data DefaultD [Expression]

            Default [Expression] needs
               Default Expression             superclass Data DefaultD Expression {aIk}
                                              and hence  Sat (DefaultD Expression) {aIm}

               Default Expression holds  $fDefaultExpression


So the problem is that when we add {aIk}, $p2 $fDefaultExpression
this good definition later gets overwritten with $p2 {aHe}, when we
add the superclasses {aHe}
-}

