{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE LinearTypes #-}
module WitnessPairEscape where
import WitnessSchema

escape :: CallbackPair -> a
escape (f, _) = f

crossed :: ProjectionPair -> ProjectionPair -> Int
crossed p q = snd p (fst q)

projectedEscape :: ProjectionPair -> a
projectedEscape p = fst p

duplicated :: ProjectionPair %1 -> Int
duplicated p = snd p (fst p)
