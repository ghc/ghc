{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module RetainedRules (Choice(..), choose, keep, readChoice, readKept) where

-- | The first two constructors record the Boolean that selected them.
data Choice (b :: Bool) where
    Off :: Choice 'False
    On :: Int -> Choice 'True
    Unused :: Int -> Choice b

-- | Select the constructor for the supplied Boolean.
{-# NOINLINE choose #-}
choose :: foreach (b :: Bool) -> Choice b
choose False = Off
choose True = On 7

-- | Return the payload without inspecting its index.
{-# NOINLINE keep #-}
keep :: foreach (b :: Bool) -> Choice b -> Choice b
keep _ value = value

{-# RULES
"keep/off" forall value. keep False value = value
"keep/choose" forall (b :: Bool). keep b (choose b) = choose b
"read/kept" forall b x. readKept b x = readChoice x
  #-}

-- | Observe the selected value without inspecting the index.
readChoice :: Choice b -> Int
readChoice Off = 0
readChoice (On n) = n
readChoice (Unused n) = n

-- | Read a retained payload through its index-preserving function.
{-# NOINLINE readKept #-}
readKept :: foreach (b :: Bool) -> Choice b -> Int
readKept b x = readChoice (keep b x)
