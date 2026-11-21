{-# LANGUAGE CPP, EmptyCase, ExperimentalDependentTypes, GADTs, LinearTypes, ScopedTypeVariables #-}
module NativeRetainedAsPattern (lookupBoth) where

import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin, indexGeneric, mkFinGeneric)

data TwoPositions a b where
    Positions :: foreach (xs :: [a]) -> Fin (sourceLength xs) %1
              -> foreach (ys :: [b]) -> Fin (sourceLength ys) %1 -> TwoPositions a b

readBoth :: TwoPositions a b %1 -> (a, b)
readBoth (Positions [] i _ unused) = case i of {}
readBoth (Positions (_ : _) unused [] j) = case j of {}
readBoth (Positions xs@(_ : _) i (ys@(_ : _)) j) = (indexGeneric xs i, indexGeneric ys j)

lookupBoth :: forall a b. [a] -> Int -> [b] -> Int -> Maybe (a, b)
lookupBoth xs i ys j = case mkFinGeneric xs i of
    Nothing -> Nothing
    Just p -> case mkFinGeneric ys j of
        Nothing -> Nothing
        Just q -> Just (readBoth (Positions xs p ys q))

#ifdef WRONG_BOUND
wrongBound :: TwoPositions a a %1 -> (a, a)
wrongBound (Positions xs@(_ : _) i ys@(_ : _) j) = (indexGeneric xs j, indexGeneric ys i)
#endif
