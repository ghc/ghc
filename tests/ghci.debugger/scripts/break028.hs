{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

type family Id x
type instance Id Int = Int
type instance Id Bool = Bool

class Convert x y where convert :: x -> y
instance Convert x x where convert = id

f :: Convert a (Id a) => a -> Id a
f x = convert x

g :: Convert a (Id a) => a -> Id a
g x = let x' = f x in x'