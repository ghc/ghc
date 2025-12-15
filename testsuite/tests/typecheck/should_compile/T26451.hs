{-# LANGUAGE ImplicitParams, TypeFamilies, FunctionalDependencies #-}

module T26451 where

type family F a
type instance F Bool = [Char]

class C a b | b -> a
instance C Bool Bool
instance C Char Char

eq :: forall a b. C a b => a -> b -> ()
eq p q = ()

g :: a -> F a
g = g

f (x::tx) (y::ty)   -- x :: alpha y :: beta
  = let ?v = g x   -- ?ip :: F alpha
      in (?v::[ty], eq x True)


{- tx, and ty are unification variables

Inert: [G] dg :: IP "v" (F tx)
       [W] dw :: IP "v" [ty]
Work-list: [W] dc1 :: C tx Bool
           [W] dc2 :: C ty Char

* Solve dc1, we get tx := Bool from fundep
* Kick out dg
* Solve dg to get [G] dc : IP "v" [Char]
* Add that new dg to the inert set: that simply deletes dw!!!
-}
