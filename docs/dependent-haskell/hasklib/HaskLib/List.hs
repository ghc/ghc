{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, LambdaCase, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}

-- | Laws for the installed Prelude functions on ordinary lists.
module HaskLib.List
    ( Nat(..), Size, size, plus, minSize, takeSize
    , appendAssoc, lengthMap, lengthAppend, lengthZip, lengthZipWith, lengthZipMap, lengthMapZip, lengthReverse, lengthTakeSize
    ) where

import Data.Type.Equality ((:~:)(Refl), trans)
import qualified GHC.Internal.List as List
import HaskLib.List.Prefix (takeSize)
import HaskLib.Nat (Nat(..), congSucc, plus, plusSucc, plusZero)
import Prelude ((++), map, reverse, zip)
import PreludeMapLengthSize (Size, minSize, size)
import PreludeZipRuntime (lengthZip, lengthZipMap, lengthZipWith)

-- | Appending three lists gives the same result with either grouping.
appendAssoc :: foreach (xs :: [a]) -> foreach (ys :: [a]) -> foreach (zs :: [a])
            -> ((xs ++ ys) ++ zs) :~: (xs ++ (ys ++ zs))
appendAssoc [] _ _ = Refl
appendAssoc (_ : xs) ys zs = case appendAssoc xs ys zs of
    Refl -> Refl

-- | Mapping a function preserves the number of list cells.
-- See Note [Map fusion and length witnesses].
lengthMap :: forall a b. foreach (f :: a -> b) -> foreach (xs :: [a])
          -> size (map f xs) :~: size xs
lengthMap f = \case
    []     -> Refl
    _ : xs -> congSucc (lengthMap f xs)

{- Note [Map fusion and length witnesses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The proof uses the source equations for size and map. Under default -O2
an optimized client can observe Zero from the mapped count and bottom
from the plain count. A producer passed to build can use seq to inspect
its step. Fusion can supply a different step at each call site when the
mapper captures the input. The published witness can still return Refl.
See the MapLengthFusionWitness example and the R16 roadmap card.
-}

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

-- | The prefix length is the smaller of the bound and input length.
lengthTakeSize :: forall a. foreach (n :: Nat) -> foreach (xs :: [a])
               -> size (takeSize n xs) :~: minSize n (size xs)
lengthTakeSize Zero _ = Refl
lengthTakeSize (Succ _) [] = Refl
lengthTakeSize (Succ n) (_ : xs) = congSucc (lengthTakeSize n xs)

-- | Mapping the pairs preserves the length of the shorter list.
-- See Note [Inlining the composed zip proof].
{-# NOINLINE lengthMapZip #-}
lengthMapZip :: forall a b c. foreach (f :: (a, b) -> c)
             -> foreach (xs :: [a]) -> foreach (ys :: [b])
             -> size (map f (zip xs ys)) :~: minSize (size xs) (size ys)
lengthMapZip f xs ys = case lengthMap f (zip xs ys) of
    Refl -> lengthZip xs ys

{- Note [Inlining the composed zip proof]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This module checks the two proof steps with list rewrite rules disabled.
Inlining the proof in an optimized caller can expose the indexed argument
of lengthMap to the zip rule. Specialise can then replace that argument
with build. Core Lint rejects the changed index. Keep the proof call intact.
See Note [Map fusion and length witnesses] for the separate demand issue.
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
