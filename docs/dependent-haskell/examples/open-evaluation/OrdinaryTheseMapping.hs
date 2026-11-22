{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinaryTheseMapping (report) where

import These (These(..))
import TheseMapping (mapBoth)

-- | Display a mapped value and its proof for the same function and inputs.
report :: Show b => (a -> b) -> Char -> a -> String
report p label x = show (fmap p (These label x), mapBoth p label x)
