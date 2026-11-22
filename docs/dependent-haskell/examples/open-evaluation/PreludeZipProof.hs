{-# LANGUAGE CPP, DataKinds, GADTs, NoExperimentalDependentTypes, PolyKinds, TypeOperators #-}
module PreludeZipProof where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import PreludeZipSource (ZipPairs)

-- | Pair the known heads and keep the two tails abstract.
pairCons :: Proxy x -> Proxy xs -> Proxy y -> Proxy ys
         -> ZipPairs (x ': xs) (y ': ys) :~: ('(x, y) ': ZipPairs xs ys)
pairCons _ _ _ _ = Refl

leftNil :: Proxy ys -> ZipPairs '[] ys :~: '[]
leftNil _ = Refl

rightNil :: Proxy x -> Proxy xs -> ZipPairs (x ': xs) '[] :~: '[]
rightNil _ _ = Refl

#if defined(UNKNOWN_LEFT)
unknownLeft :: Proxy xs -> ZipPairs xs '[] :~: '[]
unknownLeft _ = Refl
#elif defined(DROP_CONS)
dropCons :: Proxy x -> Proxy xs -> Proxy y -> Proxy ys
         -> ZipPairs (x ': xs) (y ': ys) :~: ZipPairs xs ys
dropCons _ _ _ _ = Refl
#endif
