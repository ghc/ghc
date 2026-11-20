{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -O0 #-}

-- | Keep the indexed payload and discard the runtime witness.
module ForgetWitness (Retained, Hidden, forget, retained, unknown, observe) where

import GHC.Exists
import GHC.Sigma (Sigma(Pair))

data Flag (b :: Bool) where
  On  :: Flag 'True
  Off :: Flag 'False
  Unknown :: Flag b

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

-- | Store a payload that can accompany either Boolean.
unknown :: Bool -> Retained
unknown b = Pair b (Unknown @b)

-- | Inspect the payload without reading the stored Boolean.
observe :: Hidden -> Maybe Bool
observe package = case package of
  Pack _ flag -> case flag of
    On      -> Just True
    Off     -> Just False
    Unknown -> Nothing
