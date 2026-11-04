
{-# LANGUAGE TypeFamilyDependencies #-}

module T28082 where

type family F g = r | r -> g

data P a = P a a

type instance F ( P a ) = P a

idF :: F g -> F g
idF x = x

f :: F a ~ a => P a -> P a
f p = idF p 
