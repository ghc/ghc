{-# LANGUAGE TypeFamilies #-}

-- This should fail, I think, because of the loopy equality,
-- but the error message is hopeless

module Simple13 where

type family F a

same :: a -> a -> a
same = undefined

mkf :: a -> [F a]
mkf p = undefined

foo :: a ~ [F a] => a -> a
foo p = same p (mkf p)

{- p :: a

  [G] g : a ~ [F a]
  [W] w : a ~ [F a]

---> 
  g' = g;[x]                g'=aq4
  [G] g'  : a ~ [fsk]       g=aqW
  [W] x : F a ~ fsk         x=aq3

  [W] w : a ~ [F a]

  --> subst a
       x = F g' ; x2
   [W] x2 : F fsk ~ fsk     x2=aq5

  -->  (subst a)
      w = g' ; w2
  [W] w2 : [fsk] ~ [F a]

  --> decompose 
       w2 = [w3]
   [W] w3 : fsk ~ F a



cycle is
   aq3 = Sym (F aq4) ; aq5    x = Sym (F g') ; x2
   aq4 = apw ; aq3            g' = 
-}
