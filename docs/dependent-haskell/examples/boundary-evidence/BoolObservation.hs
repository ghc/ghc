{-# LANGUAGE NoExperimentalDependentTypes #-}

module BoolObservation (lazyTrue, strictTrue, sameResult) where

-- | Return True without evaluating the argument.
lazyTrue :: Bool -> Bool
lazyTrue _ = True

-- | Evaluate the argument before returning True.
strictTrue :: Bool -> Bool
strictTrue x = x `seq` True

-- | Compare the results of the two functions at one input.
sameResult :: Bool -> Bool
sameResult x = lazyTrue x == strictTrue x
