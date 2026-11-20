{-# LANGUAGE NoExperimentalDependentTypes #-}
module WitnessPairEscape where
import WitnessSchema

escape :: CallbackPair -> a
escape (f, _) = f
