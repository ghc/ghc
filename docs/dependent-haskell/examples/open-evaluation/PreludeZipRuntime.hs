{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
module PreludeZipRuntime (lengthZip, lengthZipWith, lengthZipMap) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude (zip, zipWith)
import PreludeMapLengthSize (minSize, size)
import PreludeZipMapRuntime (lengthZipMap)

-- | Prove the length law by matching the two input lists.
lengthZip :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
          -> size (zip xs ys) :~: minSize (size xs) (size ys)
lengthZip [] _ = Refl
lengthZip (_ : _) [] = Refl
lengthZip (_ : xs) (_ : ys) = case lengthZip xs ys of
    Refl -> Refl

-- | Prove the same length law for any runtime callback.
lengthZipWith :: forall a b c. foreach (f :: a -> b -> c)
              -> foreach (xs :: [a]) -> foreach (ys :: [b])
              -> size (zipWith f xs ys) :~: minSize (size xs) (size ys)
lengthZipWith _ [] _ = Refl
lengthZipWith _ (_ : _) [] = Refl
lengthZipWith f (_ : xs) (_ : ys) = case lengthZipWith f xs ys of Refl -> Refl
