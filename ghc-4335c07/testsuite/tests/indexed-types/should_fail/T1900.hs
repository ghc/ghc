{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Class4 where

class  (Eq (Depend s)) => Bug s where
  type Depend s 
  trans :: Depend s -> Depend s
  
instance Bug Int where
  type Depend Int = ()
  trans = (+1)

check :: (Bug s) => Depend s -> Bool
check d = d == trans d

{-
 Given: (Bug s, Eq (Depend s))
      = (Bug s, Eq fsk, Depend s ~ fsk)

 Wanted: (Eq alpha,                 (invocation of == at alpha)
          Depend s ~ alpha          (first arg of ==)
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          Depend sigma ~ Depend s   (first arg of trans)

          {der}Eq (Depend sigma)    (superclass of Bug sigma)

==>
 Wanted: (Eq alpha,                 (invocation of == at alpha)
          Depend s ~ alpha          (first arg of ==)
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          {der}Eq (Depend sigma)    (superclass of Bug sigma)

==>
 Wanted: (Eq alpha,                 (invocation of == at alpha)
          Depend s ~ alpha          (first arg of ==)
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)

          {der}Eq uf_ahj
          Depend sigma ~ uf_ahj

==> uf := alpha
 Wanted: (Eq alpha,                 (invocation of == at alpha)
          Depend s ~ alpha          (first arg of ==)
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          {der}Eq alpha)
==> discharge Eq alpha from {der}
 Wanted: (Depend s ~ alpha          (first arg of ==)
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          {der}Eq alpha)

==> use given Depend s ~ fsk
 Wanted: (alpha ~ fsk
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          {der}Eq alpha)

==> alpha := fsk
 Wanted: ({given}alpha ~ fsk
          Depend sigma ~ alpha      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)
          {der}Eq fsk)

==> discharge {der} Eq fsk
 Wanted: ({given}uf ~ fsk
          Depend sigma ~ uf      (second arg of ==)
          Bug sigma,                (invocation of trans at sigma)

-}
