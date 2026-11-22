{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, TemplateHaskell, TypeOperators #-}
module NativeLocalMinSizeRule where

import Data.Type.Equality ((:~:)(Refl))

data Size = Zero | Succ Size

minSize :: Size -> Size -> Size
minSize Zero _ = Zero
minSize (Succ _) Zero = Zero
minSize (Succ m) (Succ n) = Succ (minSize m n)

zeroLaw :: foreach (n :: Size) -> minSize 'Zero n :~: 'Zero
zeroLaw _ = Refl

#ifdef LATER_RULE
$(pure [])
#endif

#ifdef NEVER_ACTIVE
{-# RULES "minSize/zero" [~] forall n. minSize Zero n = Succ Zero #-}
#else
{-# RULES "minSize/zero" forall n. minSize Zero n = Succ Zero #-}
#endif
