{-# LANGUAGE DataKinds, NoExperimentalDependentTypes #-}
module ImportedRetainedRules (tagged) where

import RetainedRules (Choice(Unused), keep)

-- | The local function gives the rule an anchor in this module.
{-# NOINLINE tagged #-}
tagged :: Int -> Choice 'True
tagged = Unused

{-# RULES "keep/tagged" forall n. keep True (tagged n) = tagged n #-}
