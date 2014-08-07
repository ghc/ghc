{-# LANGUAGE TypeFamilies #-}

module T3826 where

class C a where
  type E a
  c :: E a -> a -> a

data T a = MkT a
-- MkT :: a -> T a

instance C (T b) where
  type E (T b) = b
  c x (MkT _) = MkT x


f t@(MkT x) = c x t

{- c :: E alpha -> alpha -> alpha
   t :: T beta
   x :: beta
   f :: T beta -> gamma


   [W] C alpha
   [W] E alpha ~ beta
   [W] alpha ~ T beta
   [W] gamma ~ alpha   

--->                       beta = t_aqf  alpha = t_aqg
   alpha := T beta
   gamma := alpha

   [W] E (T beta) ~ beta

-->
   [W] fsk ~ beta
   [W] E (T beta) ~ fsk

--> 
   [W] x : E (T fsk) ~ fsk

take a step   ax: E (T fsk) ~ fsk
DO NOT unify fsk.  Instead x = ax

--------------------------
 But what about this?
--------------------------

axiom F [a] = F [a]

   x : F [a] ~ fsk
step
   ax : F [a] ~ F [a] 
flatten
   ax ; x : F [a] ~ fsk
   x = ax ; x       Oh dear!
-}

