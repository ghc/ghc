{-# LANGUAGE NoExperimentalDependentTypes #-}
module WitnessPairClient where
import WitnessSchema

inferredPair :: CallbackPair
inferredPair = (id, atTwoTypes)

main :: IO ()
main = print (observePair inferredPair, observePair (id, atTwoTypes),
              ignorePair (error "unused function", atTwoTypes))
