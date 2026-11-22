{-# LANGUAGE NoExperimentalDependentTypes #-}
module OrdinaryTheseMapping (report, reportAt) where

import These (These(..))
import TheseMapping (mapAt, mapBoth)

-- | Display a mapped value and its proof for the same function and inputs.
report :: Show b => (a -> b) -> Char -> a -> String
report p label x = show (fmap p (These label x), mapBoth p label x)

-- | Display the mapping result and proof after fixing the first argument.
reportAt :: (Show label, Show c) => (a -> b -> c) -> a -> label -> b -> String
reportAt f pivot tag x = show (fmap (f pivot) (These tag x), mapAt f pivot tag x)
