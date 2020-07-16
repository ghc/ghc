{-# LANGUAGE TupleSections, RankNTypes, ImpredicativeTypes #-}

-- Sept 16: This test involves wholesale use of impredicative polymorphism
--          and I'm amazed it has worked for so long. Anyway it is now
--          failing, which is OK.  We don't really support impredicative
--          polymorphism!
--
-- Apr 20: Works again.  NB: the ImpredicativeTypes flag
--
-- July 20: Fails again.  Under Quick Look this now fails, because
--          we don't have a special typing rule for ExplicitTuples
--          We could, and it would improve the typing for tuple sections.
--          But for now I'm just letting it fail, until someone yells.
--
-- The test was added by Max in 5e8ff849, apparently to test tuple sections

module Main where

e :: a -> (forall b. b -> b -> b) -> (a, String, forall c. c -> c -> c)
e = (,"Hello" ++ "World",)

dropFunction :: (a, String, forall c. c -> c -> c) -> (a, String, Int)
dropFunction (x, y, z) = (x, y, z 10 20)

main = print ( dropFunction (e "Meh" (flip const))
             , dropFunction (e 10 const))
