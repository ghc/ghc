{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeApplications, TypeOperators #-}
#ifndef RULES_ON
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
#endif
module PreludeZipWithProof where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import PreludeZipWithSource (PairHeads, Zipped)

-- | Prove the first equation with an abstract callback and tails.
{-# OPAQUE cons #-}
cons :: Proxy f -> Proxy x -> Proxy y -> Proxy xs -> Proxy ys
     -> Zipped f (x ': xs) (y ': ys) :~: (f x y ': Zipped f xs ys)
cons _ _ _ _ _ = Refl

leftNil :: Proxy f -> Proxy ys -> Zipped f '[] ys :~: '[]
leftNil _ _ = Refl

rightNil :: Proxy f -> Proxy x -> Proxy xs -> Zipped f (x ': xs) '[] :~: '[]
rightNil _ _ _ = Refl

-- | Apply the proof and read the first pair without evaluating either tail.
firstPair :: [(Bool, Bool)]
firstPair =
    case cons (Proxy @PairHeads) (Proxy @'True) (Proxy @'False)
              (Proxy @'[]) (Proxy @'[]) of
        Refl -> take 1 (zipWith (,) (True : error "forced left tail") (False : error "forced right tail"))

#if defined(UNKNOWN_LEFT)
unknownLeft :: Proxy f -> Proxy xs -> Zipped f xs '[] :~: '[]
unknownLeft _ _ = Refl
#elif defined(DROP_CONS)
dropCons :: Proxy f -> Proxy x -> Proxy y -> Proxy xs -> Proxy ys
         -> Zipped f (x ': xs) (y ': ys) :~: Zipped f xs ys
dropCons _ _ _ _ _ = Refl
#endif
