{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE LinearTypes #-}
module WitnessClient where
import WitnessSchema
hidden :: Hidden
hidden = callback
both :: Multi
both = pair
atKind :: KindBox
atKind = kindBox

viaCall :: RankedBuilder
viaCall = \b -> buildAt b

viaValue :: RankedBuilder
viaValue = \_ -> callback

viaLinear :: LinearBuilder
viaLinear = \b -> buildLinear b

main :: IO ()
main = print (unpack hidden, observeBuilder viaCall False,
              observeBuilder viaValue (error "unused builder operand"))
