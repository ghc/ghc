{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeZipWithLengthProof (Minimum, lengthZipWith, shorterLeft, shorterRight) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthProof (Count, Spine(..))
import PreludeMapLengthSize (Size(..))
import PreludeZipWithSource (PairHeads, Zipped)

-- | Compare two sizes, starting with the left size.
type family Minimum (left :: Size) (right :: Size) :: Size where
    Minimum 'Zero _ = 'Zero
    Minimum ('Succ _) 'Zero = 'Zero
    Minimum ('Succ left) ('Succ right) = 'Succ (Minimum left right)

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
