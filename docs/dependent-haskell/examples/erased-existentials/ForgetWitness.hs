{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
{-# OPTIONS_GHC -O0 #-}

-- | Keep the indexed payload and discard the runtime witness.
module ForgetWitness (Retained, Hidden, forget, retained, observe) where

import GHC.Exists
import GHC.Sigma (Sigma(Pair))

data Flag (b :: Bool) where
  On  :: Flag 'True
  Off :: Flag 'False

-- | Store the Boolean together with its indexed payload.
type Retained = some (b :: Bool) | Flag b

-- | Store the same payload with an erased witness.
type Hidden = exists (b :: Bool) | Flag b

-- | The payload determines the witness supplied to Pack.
forget :: Retained -> Hidden
forget (Pair _ flag) = Pack _ flag

-- | Construct the pair after checking the runtime Boolean.
retained :: Bool -> Retained
retained True  = Pair True On
retained False = Pair False Off

-- | A payload pattern can recover which constructor was packed.
observe :: Hidden -> Bool
observe package = case package of
  Pack _ flag -> case flag of
    On  -> True
    Off -> False
