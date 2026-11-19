{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, TypeApplications, TypeOperators #-}
module NativeClosedFamilyExpected where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.TypeNats (Nat)
#if defined(UNUSED_PROJECTION)
import Numeric.Natural (Natural)
#endif
import qualified NativeClosedFamilyProjection as F
import qualified NativeClosedReturnedPairLib as P

#if defined(AMBIGUOUS)
bad :: Proxy
  (F.pick (P.makePair (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 'x'))
bad = Proxy
#elif defined(DICTIONARY)
bad :: (F.pick
          (P.makePair (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 'x') :: Bool)
       :~: 'True
bad = Refl
#elif defined(DEMANDED_FALLBACK)
bad :: (F.choose ('F.Optional 'Nothing) (P.firstOf '[]) :: Nat) :~: 7
bad = Refl
#elif defined(UNUSED_PROJECTION)
-- | Form a nominal application without requiring its equation.
type Alias = F.pick (P.makePair 7 'x')
{-# RULES "pick/unused" forall a b. F.pick @Natural @Char (a, b) = a + 1 #-}
#elif defined(DEMANDED_PAIR_FIELD)
bad :: Prelude.odd (F.pick (P.bottomFirst 7)) :~: 'True
bad = Refl
#else
-- | The projection forces the pair but only renders its selected field.
lazyPairNested :: Prelude.odd (F.pick (P.bottomField 7)) :~: 'True
lazyPairNested = Refl

-- | The equation identifies the second variable even at equal field types.
lazyRight :: Prelude.odd (F.pickRight (P.bottomFirst 7)) :~: 'True
lazyRight = Refl

-- | An ordinary type synonym remains a nominal operand.
nominal :: F.choosePacket ('F.Packet 'Nothing Bool) P.Ordinary :~: Int
nominal = Refl

#if defined(EXECUTABLE_PACKET)
-- | Keep execution fused when a closed argument is a nullary synonym.
packet :: F.choosePacket (F.makePacket 7) P.Zero :~: 7
packet = Refl
#endif

-- | Exercise the same judgment as a nested operand and at the root.
ignored :: Prelude.odd (F.choose ('F.Optional ('Just 7)) (P.firstOf '[])) :~: 'True
ignored = Refl

selected :: F.choose ('F.Optional 'Nothing) (Prelude.sum '[2, 3]) :~: 5
selected = Refl

proof :: (F.pick
            (P.makePair (Prelude.fromIntegral (Prelude.length '[1, 2, 3])) 'x') :: Nat)
         :~: 3
proof = Refl
#endif
