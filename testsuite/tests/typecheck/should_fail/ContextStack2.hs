{-# LANGUAGE AllowAmbiguousTypes, TypeFamilies #-}

module ContextStack2 where

import Data.Kind (Type)

type family TF a :: Type
type instance TF (a,b) = (TF a, TF b)

-- Succeeds with new approach to fuvs
-- Aug 2016
t :: (a ~ TF (a,Int)) => Int
t = undefined

{- a ~ TF (a,Int)  
     ~ (TF a, TF Int)
     ~ (TF (TF (a,Int)), TF Int)
     ~ (TF (TF a, TF Int), TF Int)
     ~ ((TF (TF a), TF (TF Int)), TF Int) 


      fsk ~ a
      TF (a, Int) ~ fsk
-->
      fsk ~ a
*     fsk ~ (TF a, TF Int)
        (flatten lhs)
         a ~ (TF a, TF Int)
        (flatten rhs)
        a ~ (fsk1, TF Int)
(wk)  TF a ~ fsk1   

--> (rewrite inert)

   fsk ~ (fsk1, TF Int)
   a ~ (fsk1, TF Int)
(wk) TF a ~ fsk1

-->
      fsk ~ (fsk1, TF Int)
      a   ~ (fsk1, TF Int)

*     TF (fsk1, fsk2) ~ fsk1
(wk)  TF Tnt ~ fsk2

-->   
      fsk ~ (fsk1, TF Int)
      a   ~ (fsk1, TF Int)

*     fsk1 ~ (TF fsk1, TF fsk2)
        (flatten rhs)
        fsk1 ~ (fsk3, TF fsk2)

   
(wk)  TF Int ~ fsk2
      TF fsk1 ~ fsk3
-}
