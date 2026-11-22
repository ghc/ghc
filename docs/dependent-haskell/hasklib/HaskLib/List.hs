{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

-- | Length laws for the installed Prelude functions on ordinary lists.
module HaskLib.List
    ( Size(..), size, plus, minSize
    , lengthMap, lengthAppend, lengthZip, lengthZipWith
    ) where

import Data.Type.Equality ((:~:)(Refl))
import Prelude ((++), map)
import PreludeAppendLengthSource (plus)
import PreludeMapLengthSize (Size(..), minSize, size)
import PreludeZipRuntime (lengthZip, lengthZipWith)

-- | Mapping a function preserves the number of list cells.
lengthMap :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a])
          -> size (map f xs) :~: size xs
lengthMap f = \case
    []     -> Refl
    _ : xs -> case lengthMap f xs of Refl -> Refl

-- | The length of an append is the sum of the input lengths.
lengthAppend :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
             -> size (xs ++ ys) :~: plus (size xs) (size ys)
lengthAppend [] _ = Refl
lengthAppend (_ : xs) ys = case lengthAppend xs ys of Refl -> Refl
