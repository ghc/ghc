{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExperimentalDependentTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module NativeClosedReturnedPairRejects where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedReturnedPairLib as P

#if defined(FALSE_PROOF)
import Numeric.Natural (Natural)

instance {-# OVERLAPPING #-} Show (Natural, Natural) where
  show _ = "(2,3)"

bad :: Prelude.quotRem 17 5 :~: '(2, 3)
bad = Refl
#elif defined(BOTTOM_FIELD)
bad :: P.ignorePair (P.bottomField 1) :~: 7
bad = Refl
#elif defined(WRONG_KIND)
bad :: P.ignorePair (P.wrongKind 1) :~: 7
bad = Refl
#elif defined(UNSUPPORTED_FIELD)
bad :: P.unsupported 1 :~: '(1, 2)
bad = Refl
#endif
