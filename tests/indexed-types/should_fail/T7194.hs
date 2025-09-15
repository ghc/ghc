{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-} 

-- This one produced a Lint error in GHC 7.4 and 7.6

module Foo where

type family F a

class C b where {}

foo :: a -> F a
foo x = error "urk"

h :: (b -> ()) -> Int
h = error "urk"

f = h (\x -> let g :: C (F a) => a -> Int
                 g y = length [x, foo y]
             in ())

