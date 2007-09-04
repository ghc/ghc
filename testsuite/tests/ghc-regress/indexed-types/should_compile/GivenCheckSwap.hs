{-# LANGUAGE TypeFamilies #-}

module GivenCheckSwapMain where 

type family S x

f :: a -> S a
f = undefined

g :: Char ~ S a => a -> Char
g y   | False  		= f y
      |	otherwise	= 'a'
