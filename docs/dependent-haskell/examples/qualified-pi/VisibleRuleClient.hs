{-# LANGUAGE NoExperimentalDependentTypes, TypeApplications #-}
module VisibleRuleClient (readAt, readAfter) where

import VisibleRuleArguments (Reading(..), keepReading, keepAfter)

-- | Use the element type of the runtime index to fill the wildcard.
readAt :: Maybe Bool -> Int -> Int
readAt m value = case keepReading @_ m (Reading value) of Reading n -> n

-- | Supply a visible type after the first value argument.
readAfter :: Bool -> Maybe Bool -> Int -> Int
readAfter ignored m value = case keepAfter ignored @Bool m (Reading value) of Reading n -> n
