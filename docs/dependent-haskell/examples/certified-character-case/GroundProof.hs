{-# LANGUAGE DataKinds, NoExperimentalDependentTypes, TypeFamilies, TypeOperators #-}
module GroundProof (lateProof) where

import Data.Type.Equality ((:~:)(Refl))
import Upper (Upper)

{-# INLINE lateProof #-}
lateProof :: Upper '\x1e943' :~: '\x1e921'
lateProof = Refl
