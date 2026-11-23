{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module RetainedCharClient
    (observeProjection, observeSeparateProjection, observeReplacement) where

import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import Data.Maybe (isNothing)
import Data.Type.Equality ((:~:)(Refl))
import PackedVecLibrary (Nat(..), Vec(..), natToInt, sameNat, toList)
import RetainedCharLength (storedLength)
import RetainedCharLocal (localLength, localCells)
import RetainedCharProjection (storedCells)
import RetainedCharVector (CharVector(..), bottomFields, bottomPayload, cellsOf, lengthOf, sample)

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

-- | Replace the payload when the source and target have equal lengths.
replaceCells :: CharVector -> CharVector -> Maybe CharVector
replaceCells target source = case sameNat (lengthOf target) (lengthOf source) of
    Nothing -> Nothing
    Just Refl -> Just (CharVector (lengthOf target) (cellsOf source))

-- | Observe a replacement and a length mismatch with two undefined payloads.
observeReplacement :: IO (Maybe String, Bool)
observeReplacement = do
    source <- newIORef (CharVector (S Z) (VCons 'b' VNil)) >>= readIORef
    let emptyTarget = CharVector Z (error "forced target payload")
    pure (fmap render (replaceCells bottomPayload source),
          isNothing (replaceCells emptyTarget bottomPayload))
  where
    render :: CharVector -> String
    render vector = toList (cellsOf vector)
