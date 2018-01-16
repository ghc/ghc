{-# OPTIONS_GHC -dcore-lint #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, UndecidableInstances #-}

-- This is a rather exotic functional-dependency test.
-- It crashed GHC 5.04.3 with a core-lint error, because
-- of a bug in tcSimplifyRestricted (fixed Apr 03)

module Main where


class ComposePS a b c | a b -> c where
    (.>) :: PSOp a -> PSOp b -> PSOp c
    (V a) .> (V b) = V (a ++ b)

instance (ConcatPS a b c, CheckPS c Id Id d) => ComposePS a b d

------------------------------------------------------------------------------

data PSOp a = V [String] deriving Show

data Id
data Push t rest
data Pop t rest


class Reverse a b c | a b -> c
instance                           Reverse Id b b
instance Reverse a (Pop t b)  c => Reverse (Pop t a) b c
instance Reverse a (Push t b) c => Reverse (Push t a) b c

------------------------------------------------------------------------------

class ConcatPS a b c | a b -> c where
    ccat :: a -> b -> c
instance ConcatPS Id a a
instance ConcatPS a b c => ConcatPS (Pop t a) b (Pop t c)
instance ConcatPS a b c => ConcatPS (Push t a) b (Push t c)

------------------------------------------------------------------------------

class CheckPS a b c d | a b c -> d where
    check :: a -> b -> c -> d
    check _ _ _ = error "oki"

instance Reverse a b c => CheckPS Id a b c

instance CheckPS a b (Push t c) d => CheckPS (Push t a) b c d

instance CheckPS a (Pop t b) Id d => CheckPS (Pop t a) b Id d

instance CheckPS a b c d => CheckPS (Pop t a) b (Push t c) d


v1 :: PSOp (Pop a Id)
v1 = V []

v2 :: PSOp Id
v2 = V []

t = v1 .> v2

main = print t
