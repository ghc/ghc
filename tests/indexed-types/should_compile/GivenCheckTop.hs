{-# LANGUAGE TypeFamilies #-}

module GivenCheckTop where 

type family S x

type instance S [e] = e 

f :: a -> S a
f = undefined

g :: S [a] ~ Char => a -> Char
g y = y
