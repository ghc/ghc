{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeLocalListAccumulator
    ( shunt, step
#ifdef LOCAL_REVERSE
    , rev, reverseStep
#endif
    ) where

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

#ifdef LOCAL_REVERSE
rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

reverseStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
#ifdef FALSE_REVERSE
            -> rev (x ': xs) :~: (rev xs ++ '[ 'False])
#else
            -> rev (x ': xs) :~: (rev xs ++ '[x])
#endif
reverseStep _ _ = Refl
#endif
