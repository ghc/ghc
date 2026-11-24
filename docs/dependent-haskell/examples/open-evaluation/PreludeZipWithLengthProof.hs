{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeZipWithLengthProof (Minimum, lengthZipWith, shorterLeft, shorterRight) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthProof (Count, Spine(..))
import PreludeMapLengthSize (Nat(..))
import PreludeZipWithSource (Minimum, PairHeads, Zipped)

{- Note [Minimum length for ordinary zippers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An ordinary client uses Spine for induction and Minimum as a synonym for
the term function minSize. PreludeZipRuntime matches the input lists
directly. Each proof uses the checked equations of its own installed zipper. An empty left list returns an
empty result for any right list. An empty right list needs a left cons
because both zippers inspect the left list first.
-}

-- | The result has the length of the shorter list, for any callback.
lengthZipWith :: Proxy f -> Spine xs -> Spine ys
              -> Count (Zipped f xs ys) :~: Minimum (Count xs) (Count ys)
lengthZipWith _ Empty _ = Refl
lengthZipWith _ (Cons _) Empty = Refl
lengthZipWith f (Cons xs) (Cons ys) = case lengthZipWith f xs ys of
    Refl -> Refl

-- | The left list can supply the shorter length.
shorterLeft :: 'Succ 'Zero :~: 'Succ 'Zero
shorterLeft = lengthZipWith (Proxy @PairHeads)
    (Cons Empty :: Spine '[ 'False])
    (Cons (Cons Empty) :: Spine '[ 'True, 'False])

-- | The right list can supply the shorter length.
shorterRight :: 'Succ 'Zero :~: 'Succ 'Zero
shorterRight = lengthZipWith (Proxy @PairHeads)
    (Cons (Cons Empty) :: Spine '[ 'True, 'False])
    (Cons Empty :: Spine '[ 'False])

#if defined(UNKNOWN_LEFT_LENGTH)
unknownLeft :: Proxy f -> Proxy xs -> Count (Zipped f xs '[]) :~: 'Zero
unknownLeft _ _ = Refl
#elif defined(DROP_SUCCESSOR)
dropSuccessor :: Proxy f -> Proxy x -> Proxy y -> Proxy xs -> Proxy ys
              -> Count (Zipped f (x ': xs) (y ': ys)) :~: Count (Zipped f xs ys)
dropSuccessor _ _ _ _ _ = Refl
#endif
