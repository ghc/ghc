{-# LANGUAGE NoExperimentalDependentTypes #-}
module WitnessClient where
import WitnessSchema
hidden :: Hidden
hidden = callback
both :: Multi
both = pair
atKind :: KindBox
atKind = kindBox

inferredPair :: CallbackPair
inferredPair = (id, atTwoTypes)

main :: IO ()
main = do
    print (unpack hidden)
    print (observePair inferredPair, observePair (id, atTwoTypes),
           ignorePair (error "unused function", atTwoTypes))
