{-# LANGUAGE TypeFamilies #-}

module GivenCheckDecomp where 

type family S x

f :: a -> S a
f = undefined

g :: [S a] ~ [Char] => a -> Char
g y   | 'a' == 'b'	= f y
