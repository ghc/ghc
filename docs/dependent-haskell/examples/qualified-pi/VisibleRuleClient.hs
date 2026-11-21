{-# LANGUAGE ExplicitNamespaces, NoExperimentalDependentTypes, RequiredTypeArguments, TypeApplications #-}
module VisibleRuleClient (readAt, readAfter, readTagged, readRequired, readTwoRequired) where

import VisibleRuleArguments (Reading(..), keepReading, keepAfter, TaggedReading(..), keepTagged, keepRequired, keepTwoRequired)

-- | Use the element type of the runtime index to fill the wildcard.
readAt :: Maybe Bool -> Int -> Int
readAt m value = case keepReading @_ m (Reading value) of Reading n -> n

-- | Supply a visible type after the first value argument.
readAfter :: Bool -> Maybe Bool -> Int -> Int
readAfter ignored m value = case keepAfter ignored @Bool m (Reading value) of Reading n -> n

-- | Read a payload whose two indices come from the caller.
readTagged :: Bool -> Maybe Bool -> Int -> Int
readTagged tag m value =
    case keepTagged tag @Bool m (TaggedReading value) of TaggedReading n -> n

-- | Supply the erased type between the two runtime indices.
readRequired :: Bool -> Maybe Bool -> Int -> Int
readRequired tag m value =
    case keepRequired tag (type Bool) m (TaggedReading value) of TaggedReading n -> n

-- | Supply the middle index at the domain given by the first type argument.
readTwoRequired :: Bool -> Maybe Bool -> Maybe Char -> Int -> Int
readTwoRequired tag m n value =
    case keepTwoRequired tag (type Bool) m (type Char) n (TaggedReading value) of TaggedReading x -> x
