{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoExperimentalDependentTypes #-}
{-# LANGUAGE DataKinds, LinearTypes, TypeApplications #-}
module WitnessClient where
import Control.Monad (unless)
import Data.Proxy
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

viaTypeArg :: RankedBuilder
viaTypeArg = \b -> configured @True @Bool Proxy id b

viaLinear :: LinearBuilder
viaLinear = \b -> buildLinear b

viaGadt :: FlaggedBuilder a
viaGadt = \Flag b -> buildAt b

main :: IO ()
main = do
    unless (observeFlagged viaGadt False == (9, False)
         && observeFlagged viaGadt True == (7, True)) $
        fail "GADT pattern did not refine the builder operand"
    print (unpack hidden, observeBuilder viaCall False,
           observeBuilder viaValue (error "unused builder operand"),
           observeBuilder viaTypeArg False)
