{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeZipRuntime (lengthZip, lengthZipWith, lengthZipMap) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude (map, zip, zipWith)
import PreludeMapLengthSize (minSize, size)

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

-- | Mapping the left list before zipping preserves the shorter length.
lengthZipMap :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
             -> foreach (xs :: [a]) -> foreach (ys :: [c])
             -> size (zipWith combine (map pre xs) ys) :~: minSize (size xs) (size ys)
lengthZipMap _ _ [] _ = Refl
lengthZipMap _ _ (_ : _) [] = Refl
lengthZipMap pre combine (_ : xs) (_ : ys) =
    case lengthZipMap pre combine xs ys of Refl -> Refl
