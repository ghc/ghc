{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleContexts, KindSignatures #-}
{-# LANGUAGE PolyKinds, TypeFamilies, TypeOperators #-}
module PreludePairExport
  (first, second, swapped, nestedFirst, nestedSecond,
   abstractFirst, abstractSecond, get, swapIndex, secondIndex, duplicateFirst) where

import Data.Proxy (Proxy(..))
import qualified Data.Tuple
import Data.Type.Equality ((:~:)(Refl))

-- | Return the evidence for the first field of an unknown pair.
first :: forall k l (p :: (k, l)) (x :: k).
         Prelude.fst p ~ x => Proxy p -> Proxy x -> x :~: Prelude.fst p
first _ _ = Refl

-- | Return the evidence for the second field of an unknown pair.
second :: forall k l (p :: (k, l)) (x :: l).
          Prelude.snd p ~ x => Proxy p -> Proxy x -> x :~: Prelude.snd p
second _ _ = Refl

-- | Return the given equality for the swapped pair.
swapped :: forall k l (p :: (k, l)) (q :: (l, k)).
           Data.Tuple.swap p ~ q => Proxy p -> Proxy q -> q :~: Data.Tuple.swap p
swapped _ _ = Refl

-- | Select the second input field without learning the first.
nestedFirst :: forall (b :: Bool). Proxy b
            -> Prelude.fst (Data.Tuple.swap '(b, 'x')) :~: 'x'
nestedFirst _ = Refl

-- | Select the first input field without learning the second.
nestedSecond :: forall (c :: Char). Proxy c
             -> Prelude.snd (Data.Tuple.swap '( 'True, c)) :~: 'True
nestedSecond _ = Refl

-- | Project the second input field of a rigid pair.
{-# INLINE abstractFirst #-}
abstractFirst :: forall (p :: (Bool, Char)). Proxy p
              -> Prelude.fst (Data.Tuple.swap p) :~: Prelude.snd p
abstractFirst _ = Refl

-- | Project the first input field of a rigid pair.
abstractSecond :: forall (p :: (Bool, Char)). Proxy p
               -> Prelude.snd (Data.Tuple.swap p) :~: Prelude.fst p
abstractSecond _ = Refl

-- | Expose an application of the first projection for later substitution.
get :: forall k l (p :: (k, l)). Proxy p -> Proxy (Prelude.fst p)
get _ = Proxy

-- | Carry the swapped index through an ordinary proxy.
swapIndex :: forall k l (p :: (k, l)). Proxy p -> Proxy (Data.Tuple.swap p)
swapIndex _ = Proxy

-- | Carry the second field through an ordinary proxy.
secondIndex :: forall k l (p :: (k, l)). Proxy p -> Proxy (Prelude.snd p)
secondIndex _ = Proxy

-- | Build a pair from the first field of its input.
duplicateFirst :: (a, b) -> (a, a)
duplicateFirst (x, _) = (x, x)
