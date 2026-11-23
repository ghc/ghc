{-# LANGUAGE NoExperimentalDependentTypes #-}
module RetainedCharClient (observeProjection) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.List (isInfixOf)
import PackedVecLibrary (natToInt, toList)
import RetainedCharLength (storedLength)
import RetainedCharProjection (storedCells)
import RetainedCharVector (CharVector, bottomFields, bottomPayload, sample)

readCells :: CharVector -> String
readCells vector = toList (storedCells vector)

-- | Read each field while the other field can raise an exception.
observeProjection :: IO (Int, String, Bool)
observeProjection = do
    result <- try (evaluate (readCells bottomFields)) :: IO (Either ErrorCall String)
    let foundPayload = case result of
            Left ex -> "forced payload" `isInfixOf` displayException ex
            Right _ -> False
    pure (natToInt (storedLength bottomPayload), readCells sample, foundPayload)
