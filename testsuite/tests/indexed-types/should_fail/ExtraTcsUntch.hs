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


f (x::beta) = 
    let g1 :: forall b. b -> ()
        g1 _ = h [x]
        g2 z = case z of TEx y -> (h [[undefined]], op x [y])
    in (g1 '3', g2 undefined)


{- This example comes from Note [Extra TcS Untouchables] in TcSimplify. It demonstrates 
   why when floating equalities out of an implication constraint we must record the free
   variables of the equalities as untouchables. With GHC 7.4.1 this program gives a Core
   Lint error because of an existential escaping. -}


   
