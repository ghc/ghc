{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeZipMapRuntime (lengthZipMap) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude (map, zipWith)
import PreludeMapLengthSize (minSize, size)

-- | Mapping the left list before zipping preserves the shorter length.
lengthZipMap :: forall a b c d. foreach (pre :: a -> b) -> foreach (combine :: b -> c -> d)
             -> foreach (xs :: [a]) -> foreach (ys :: [c])
             -> size (zipWith combine (map pre xs) ys) :~: minSize (size xs) (size ys)
lengthZipMap _ _ [] _ = Refl
lengthZipMap _ _ (_ : _) [] = Refl
lengthZipMap pre combine (_ : xs) (_ : ys) =
    case lengthZipMap pre combine xs ys of Refl -> Refl
