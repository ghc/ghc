{-# LANGUAGE ImpredicativeTypes, NoDeepSubsumption, NoExperimentalDependentTypes #-}
module ConstrainedElementsQuickLook (showReflection) where

import ConstrainedElements (reflectMaybe)
import ConstrainedElementsClient (runReflection)

-- | Render an ordinary nested call with a retained callback.
showReflection :: Maybe Bool -> String
showReflection input = show (runReflection reflectMaybe input)
