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

viaPackageTypeArg :: Bool -> (Int, Bool)
viaPackageTypeArg flag = case ranked @Bool flag of
    Payload value run -> run value

viaObserverTypeArg :: Bool -> (Int, Bool)
viaObserverTypeArg flag = observe (ranked @Bool flag)
  where
    observe :: Payload t -> (Int, Bool)
    observe (Payload value run) = run value

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
           observeBuilder viaTypeArg False, viaPackageTypeArg False,
           viaObserverTypeArg True)
