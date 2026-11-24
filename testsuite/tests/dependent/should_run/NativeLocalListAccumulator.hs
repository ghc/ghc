{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeLocalListAccumulator (shunt, step) where

import Data.Type.Equality ((:~:)(Refl))

shunt :: [a] -> [a] -> [a]
#ifndef PARTIAL
shunt [] ys = ys
#endif
#ifdef NONDECREASING
shunt (x : xs) ys = shunt (x : xs) (x : ys)
#elif defined(FORCED)
shunt (x : xs) ys = seq ys (shunt xs (x : ys))
#else
shunt (x : xs) ys = shunt xs (x : ys)
#endif

#ifdef RULE
{-# RULES "skip shunt" forall xs ys. shunt xs ys = ys #-}
#endif

step :: foreach (x :: a) -> foreach (xs :: [a]) -> foreach (ys :: [a])
#ifdef FALSE_EQ
     -> shunt (x ': xs) ys :~: shunt xs ys
#else
     -> shunt (x ': xs) ys :~: shunt xs (x ': ys)
#endif
step _ _ _ = Refl
