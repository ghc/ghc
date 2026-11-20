{-# LANGUAGE NoExperimentalDependentTypes #-}
module WitnessClient where
import WitnessSchema
hidden :: Hidden
hidden = callback
both :: Multi
both = pair
atKind :: KindBox
atKind = kindBox

main :: IO ()
main = print (unpack hidden)
