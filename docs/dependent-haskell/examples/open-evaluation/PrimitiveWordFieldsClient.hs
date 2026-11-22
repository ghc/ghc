{-# LANGUAGE NoExperimentalDependentTypes #-}
module PrimitiveWordFieldsClient (wordFieldResults) where

import PrimitiveWordFields (MaximumWord, WordFieldResult, ZeroWord)

-- | Observe the two result types selected by the imported family.
wordFieldResults :: (WordFieldResult ZeroWord, WordFieldResult MaximumWord)
wordFieldResults = (False, 'M')
