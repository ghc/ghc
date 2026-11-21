{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables, Strict #-}
module NativeLinearIndexColumns (choose) where

import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))

-- The selected element does not need the fallback in the first equation.
choose :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> Bool -> a -> a
choose (x : _) FZ True ~_ = x
choose (_ : _) FZ False other = other
choose (_ : rest) (FS i) bit ~other = choose rest i bit other

#if defined(DISCARD) || defined(DUPLICATE)
bad :: forall a. foreach (xs :: [a]) -> Fin (sourceLength xs) %1 -> (a -> a) %1 -> a
#ifdef DISCARD
bad (x : _) FZ k = x
#else
bad (x : _) FZ k = k (k x)
#endif
bad (_ : rest) (FS i) k = bad rest i k
#endif
