{-# LANGUAGE NoExperimentalDependentTypes #-}
module RetainedCharClient (observeProjection, observeSeparateProjection) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import PackedVecLibrary (Nat, natToInt, toList)
import RetainedCharLength (storedLength)
import RetainedCharLocal (localLength, localCells)
import RetainedCharProjection (storedCells)
import RetainedCharVector (CharVector, bottomFields, bottomPayload, sample)

readCells :: CharVector -> String
readCells vector = toList (storedCells vector)

readLocalCells :: CharVector -> String
readLocalCells vector = toList (localCells vector)

-- | Read each field through the projections defined together.
observeProjection :: IO (Int, String, Bool)
observeProjection = observeFields localLength readLocalCells

-- | Read each field through the projections defined in separate modules.
observeSeparateProjection :: IO (Int, String, Bool)
observeSeparateProjection = observeFields storedLength readCells

observeFields :: (CharVector -> Nat) -> (CharVector -> String) -> IO (Int, String, Bool)
observeFields readLength readVector = do
    result <- try (evaluate (readVector bottomFields)) :: IO (Either ErrorCall String)
    let foundPayload = case result of
            Left ex -> "forced payload" `isInfixOf` displayException ex
            Right _ -> False
    pure (natToInt (readLength bottomPayload), readVector sample, foundPayload)
