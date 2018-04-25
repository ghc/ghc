{-# LANGUAGE TupleSections, RankNTypes, ImpredicativeTypes #-}

-- Sept 16: This test involves wholesale use of impredicative polymorphism
--          and I'm amazed it has worked for so long. Anyway it is now
--          failing, which is OK.  We don't really support impredicative
--          polymorphism!
--
-- The test was added by Max in 5e8ff849, apparently to test tuple sections

module Main where

e :: a -> (forall b. b -> b -> b) -> (a, String, forall c. c -> c -> c)
e = (,"Hello" ++ "World",)

dropFunction :: (a, String, forall c. c -> c -> c) -> (a, String, Int)
dropFunction (x, y, z) = (x, y, z 10 20)

main = print (dropFunction $ e "Meh" (flip const), dropFunction $ e 10 const)
