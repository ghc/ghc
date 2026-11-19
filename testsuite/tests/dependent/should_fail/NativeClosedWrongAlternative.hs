{-# LANGUAGE DataKinds, ExperimentalDependentTypes, FlexibleInstances, TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module NativeClosedWrongAlternative where
import qualified Data.Maybe
import Data.Type.Equality ((:~:)(Refl))
import Numeric.Natural (Natural)

instance {-# OVERLAPPING #-} Show (Maybe Natural) where
    show _ = "Nothing"

bad :: Data.Maybe.listToMaybe '[5] :~: ('Nothing :: Maybe Natural)
bad = Refl
