{-# LANGUAGE Haskell2010 #-}

module StandaloneCallbackClient (callbackResults) where

import StandaloneCallback (Apply, FamilyResult)

-- | Use the exported callback kind and the provider's family result.
callbackResults :: Apply Maybe Char -> FamilyResult -> (Maybe Char, [Bool])
callbackResults constructorResult familyResult = (constructorResult, familyResult)
