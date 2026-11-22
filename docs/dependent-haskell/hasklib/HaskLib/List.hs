{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

-- | Length laws for the installed Prelude functions on ordinary lists.
module HaskLib.List
    ( Size(..), size, plus, minSize
    , lengthMap, lengthAppend, lengthZip, lengthZipWith, lengthZipMap, lengthReverse
    ) where

import Data.Type.Equality ((:~:)(Refl), trans)
import qualified GHC.Internal.List as List
import HaskLib.Size (congSucc, plusSucc, plusZero)
import Prelude ((++), map, reverse)
import PreludeAppendLengthSource (plus)
import PreludeMapLengthSize (Size(..), minSize, size)
import PreludeZipRuntime (lengthZip, lengthZipMap, lengthZipWith)

-- | Mapping a function preserves the number of list cells.
lengthMap :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a])
          -> size (map f xs) :~: size xs
lengthMap f = \case
    []     -> Refl
    _ : xs -> congSucc (lengthMap f xs)

-- | The length of an append is the sum of the input lengths.
lengthAppend :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
             -> size (xs ++ ys) :~: plus (size xs) (size ys)
lengthAppend [] _ = Refl
lengthAppend (_ : xs) ys = congSucc (lengthAppend xs ys)

-- | Count the result of the reverse worker with any accumulator.
lengthReverseOnto :: forall a. foreach (xs :: [a]) -> foreach (acc :: [a])
                  -> size (List.reverseOnto xs acc) :~: plus (size xs) (size acc)
lengthReverseOnto [] _ = Refl
lengthReverseOnto (x : xs) acc =
    trans (lengthReverseOnto xs (x : acc))
          (plusSucc (size xs) (size acc))

-- | Reversing a list preserves its finite length.
lengthReverse :: forall a. foreach (xs :: [a]) -> size (reverse xs) :~: size xs
lengthReverse xs = trans (lengthReverseOnto xs []) (plusZero (size xs))
