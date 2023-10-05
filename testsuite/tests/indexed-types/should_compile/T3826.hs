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
   [W] ufsk ~ beta
   [W] E (T beta) ~ ufsk

--> (swap and subst)
   beta := ufsk
   [W] x : E (T ufsk) ~ ufsk   (do not rewrite RHS)

take a step   ax: E (T beta) ~ beta

-->
   [W] ufsk
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

