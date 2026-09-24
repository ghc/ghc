-- Repro.hs
-- The call to f forces c ~ (b, F c) inside the scope of the skolem b.
-- Correct behaviour: a type error at the first argument of const
-- (as GHC 9.12.4 reports).

{-# LANGUAGE RankNTypes, TypeFamilies #-}
module T27844 where

type family F a

f :: (forall b. b -> (b, F c)) -> c -> ()
f _ _ = ()

g x = f (\_ -> const x (show x)) x
