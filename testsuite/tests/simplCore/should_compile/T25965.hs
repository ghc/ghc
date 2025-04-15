{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -O -fpolymorphic-specialisation #-}

module Foo where

type family F a

data T a = T1

instance Eq (T a) where { (==) x y = False }

foo :: Eq a => a -> Bool
foo x | x==x = True
      | otherwise = foo x

bar :: forall b. b -> T (F b) -> Bool
bar y x = foo x

