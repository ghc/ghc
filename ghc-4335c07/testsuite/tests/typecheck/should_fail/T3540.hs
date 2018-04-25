{-# LANGUAGE ImplicitParams, TypeFamilies #-}
module T3540 where

thing ::  (a~Int)
thing = undefined

thing1 ::  Int -> (a~Int)
thing1 = undefined

thing2 ::  (a~Int) -> Int
thing2 = undefined

thing3 :: (?dude :: Int) -> Int
thing3 = undefined

thing4:: (Eq a) -> Int
thing4 = undefined