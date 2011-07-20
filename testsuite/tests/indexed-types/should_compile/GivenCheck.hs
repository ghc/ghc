{-# LANGUAGE TypeFamilies #-}

module GivenCheck where 

type family S x

f :: a -> S a
f = undefined

g :: S a ~ Char => a -> Char
g y   | False  		= f y
      |	otherwise	= 'a'
