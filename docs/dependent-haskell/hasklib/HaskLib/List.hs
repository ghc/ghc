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
-- See Note [Append fusion and length witnesses].
lengthAppend :: forall a. foreach (xs :: [a]) -> foreach (ys :: [a])
             -> size (xs ++ ys) :~: plus (size xs) (size ys)
lengthAppend [] _ = Refl
lengthAppend (_ : xs) ys = congSucc (lengthAppend xs ys)

{- Note [Append fusion and length witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The proof uses the source equations for (++). A client can observe
different demands after foldr/build fusion. Suppose a producer forces
its seed before it returns its first cell. Building the list gives it
[] as the seed. Fusing append can give it the right operand instead.
If that operand is bottom, size can lose its first Succ. The proof
still returns Refl for the one-cell left list. See AppendFusionDemand.hs
in docs/dependent-haskell/examples/open-evaluation.
-}

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
