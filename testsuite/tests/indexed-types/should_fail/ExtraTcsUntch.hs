{-# LANGUAGE TypeFamilies, FunctionalDependencies, FlexibleContexts, GADTs, ScopedTypeVariables #-}

module ExtraTcsUntch where 


class C x y | x -> y where 
 op :: x -> y -> ()

instance C [a] [a]

type family F a :: *

h :: F Int -> ()
h = undefined

data TEx where 
  TEx :: a -> TEx 



f x = 
    let g1 :: forall b. b -> ()
        g1 _ = h [x]
        g2 z = case z of TEx y -> (h [[undefined]], op x [y])
    in (g1 '3', g2 undefined)


{- This example comes from Note [Extra TcS Untouchables] in TcSimplify. It demonstrates 
   why when floating equalities out of an implication constraint we must record the free
   variables of the equalities as untouchables. With GHC 7.4.1 this program gives a Core
   Lint error because of an existential escaping. 

    assuming x:beta

    forall b. F Int ~ [beta]                          (from g1)
    forall a. F Int ~ [[alpha]], C beta [a]           (from g2)

-}



   
{- Assume x:beta
   From g1 we get    (forall b.  F Int ~ [beta])
   From g2 we get    (forall c. 0 => F Int ~ [[alpha]] /\ C beta [c])

Floating we get
   F Int ~ [beta], F Int ~ [[alpha]], alpha ~ alpha', forall c. C beta [c]
=  { alpha := alpha' }
=  beta ~ [alpha'], F Int ~ [[alpha']], forall c. C beta [c]
=  { beta := [alpha']
   F Int ~ [[alpha']], forall c. C [alpha'] [c]
=  F Int ~ [[alpha']], forall c. (C [alpha'] [c], alpha' ~ c)
-}