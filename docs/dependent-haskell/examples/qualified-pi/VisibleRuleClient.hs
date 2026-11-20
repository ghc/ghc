{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module VisibleRuleClient (readAt) where

import VisibleRuleArguments (Reading(..), keepReading)

-- | Use the element type of the runtime index to fill the wildcard.
readAt :: Maybe Bool -> Int -> Int
readAt m value = case keepReading @_ m (Reading value) of Reading n -> n
