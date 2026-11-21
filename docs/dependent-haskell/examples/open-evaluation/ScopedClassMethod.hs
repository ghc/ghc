{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, ScopedTypeVariables #-}
module ScopedClassMethod (Choose(..), Tagged(..), chooseTagged, describeChoice, tagAt) where

import Numeric.Natural (Natural)
import RetainedNaturals (Reading, rememberNatural)

class Choose a where
    choose :: a -> Bool

data Tagged b where
    Tagged :: Bool -> Tagged b

tagBool :: foreach (b :: Bool) -> Tagged b
tagBool b = Tagged @b b

-- | Keep the selected method and its caller-supplied dictionary in the result type.
chooseTagged :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
chooseTagged x = tagBool (choose x)

-- | Use the receiver's scoped method and the ordinary payload's Show instance.
describeChoice :: forall r a. (Choose r, Show a)
               => foreach (x :: r) -> a -> (Tagged (choose x), String)
describeChoice x payload = (tagBool (choose x), show payload)


-- | Keep the method's receiver and the independent length in separate indices.
tagAt :: forall a. Choose a => foreach (x :: a) -> foreach (n :: Natural)
      -> (Tagged (choose x), Reading n)
tagAt x n = (chooseTagged x, rememberNatural n)
