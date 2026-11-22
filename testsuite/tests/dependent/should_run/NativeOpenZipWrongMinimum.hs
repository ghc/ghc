{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenZipWrongMinimum where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipDemandControls (twiceMinimum)
import PreludeMapLengthSize (size)

wrong :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
      -> size (zip xs ys) :~: twiceMinimum (size xs) (size ys)
wrong [] _ = Refl
wrong (_ : _) [] = Refl
wrong (_ : xs) (_ : ys) = case wrong xs ys of
    Refl -> Refl
