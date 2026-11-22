{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeZipRuntime (lengthZip) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude (zip)
import PreludeMapLengthSize (size)
import PreludeZipWithLengthProof (Minimum)

-- | Prove the length law by matching the two input lists.
lengthZip :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
          -> size (zip xs ys) :~: Minimum (size xs) (size ys)
lengthZip [] _ = Refl
lengthZip (_ : _) [] = Refl
lengthZip (_ : xs) (_ : ys) = case lengthZip xs ys of
    Refl -> Refl
