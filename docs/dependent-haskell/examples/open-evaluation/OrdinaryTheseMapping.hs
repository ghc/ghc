{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinaryTheseMapping (report) where

import These (These(..))
import TheseMapping (mapBoth)

-- | Display a mapped value and its proof for the same predicate and inputs.
report :: (a -> Bool) -> Char -> a -> String
report p label x = show (fmap p (These label x), mapBoth p label x)
