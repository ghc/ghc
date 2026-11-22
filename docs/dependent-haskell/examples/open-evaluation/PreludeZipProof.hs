{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
#ifndef RULES_ON
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
#endif
module PreludeZipProof where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeMapLengthProof (Count, Spine(..))
import PreludeZipSource (ZipPairs)
import PreludeZipWithLengthProof (Minimum)

-- | Pair the known heads and keep the two tails abstract.
pairCons :: Proxy x -> Proxy xs -> Proxy y -> Proxy ys
         -> ZipPairs (x ': xs) (y ': ys) :~: ('(x, y) ': ZipPairs xs ys)
pairCons _ _ _ _ = Refl

leftNil :: Proxy ys -> ZipPairs '[] ys :~: '[]
leftNil _ = Refl

rightNil :: Proxy x -> Proxy xs -> ZipPairs (x ': xs) '[] :~: '[]
rightNil _ _ = Refl

-- | Prove that zip returns as many pairs as there are elements in the shorter list.
-- See Note [Minimum length for ordinary zippers] in PreludeZipWithLengthProof.
lengthZip :: Spine xs -> Spine ys
          -> Count (ZipPairs xs ys) :~: Minimum (Count xs) (Count ys)
lengthZip Empty _ = Refl
lengthZip (Cons _) Empty = Refl
lengthZip (Cons xs) (Cons ys) = case lengthZip xs ys of
    Refl -> Refl

#if defined(UNKNOWN_LEFT)
unknownLeft :: Proxy xs -> ZipPairs xs '[] :~: '[]
unknownLeft _ = Refl
#elif defined(DROP_CONS)
dropCons :: Proxy x -> Proxy xs -> Proxy y -> Proxy ys
         -> ZipPairs (x ': xs) (y ': ys) :~: ZipPairs xs ys
dropCons _ _ _ _ = Refl
#elif defined(UNKNOWN_LEFT_LENGTH)
unknownLeftLength :: Proxy xs -> Count (ZipPairs xs '[]) :~: Count '[]
unknownLeftLength _ = Refl
#elif defined(DROP_SUCCESSOR)
dropSuccessor :: Proxy x -> Proxy y -> Proxy xs -> Proxy ys
              -> Count (ZipPairs (x ': xs) (y ': ys)) :~: Count (ZipPairs xs ys)
dropSuccessor _ _ _ _ = Refl
#endif
