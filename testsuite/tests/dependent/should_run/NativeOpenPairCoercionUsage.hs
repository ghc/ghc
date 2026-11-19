{-# LANGUAGE DataKinds, ExperimentalDependentTypes, TypeOperators #-}
module NativeOpenPairCoercionUsage where

import Data.Proxy (Proxy)
import qualified Data.Tuple
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeOpenPairCoercionWorker as W

-- | Follow both positions across three strict constructor cases.
chain :: forall (p :: (Bool, Char)). Proxy p
      -> Prelude.snd (W.duplicateRight (Data.Tuple.swap (W.same p))) :~: Prelude.fst p
chain _ = Refl

-- | Both observations copy the original first field.
both :: forall (p :: (Bool, Char)). Proxy p
     -> Prelude.fst (W.duplicateRight (Data.Tuple.swap p))
         :~: Prelude.snd (W.duplicateRight (Data.Tuple.swap (W.same p)))
both _ = Refl

-- | Both fields agree whatever value the common input pair has.
sameInput :: forall (p :: (Bool, Char)). Proxy p
          -> Prelude.fst (W.duplicateRight p) :~: Prelude.snd (W.duplicateRight p)
sameInput _ = Refl
