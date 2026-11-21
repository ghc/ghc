{-# LANGUAGE ExperimentalDependentTypes #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns -Werror=overlapping-patterns -Wno-inaccessible-code #-}
module NativeListIndexDiagnostics where

import Data.Kind (Type)
import PackedVecLibrary (sourceLength)
import ParameterizedIndexLibrary (Fin(..))

missing :: forall (a :: Type). foreach (items :: [a]) -> Fin (sourceLength items) -> a
missing (value : _) FZ = value

impossible :: forall (a :: Type). foreach (items :: [a]) -> Fin (sourceLength items) -> a
impossible [] FZ = error "impossible position"
impossible (value : _) FZ = value
impossible (_ : rest) (FS smaller) = impossible rest smaller

ordinary :: [Int] -> Int
ordinary (value : _) = value
ordinary whole@(_ : _) = length whole
ordinary [] = 0
