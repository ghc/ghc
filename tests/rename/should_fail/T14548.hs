{-# LANGUAGE ScopedTypeVariables, TypeApplications, PolyKinds #-}

module T14548 where

data Prox (a :: k) = MkProx

-- fail
f :: forall a. Prox (a :: k)
f = MkProx @k @a

-- fail
g :: forall (a :: k). Prox (a :: k)
g = MkProx @k @a

-- ok
h :: forall k (a :: k). Prox (a :: k)
h = MkProx @k @a
