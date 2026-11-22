{-# LANGUAGE GADTs, NoExperimentalDependentTypes #-}
module DirectIndexedListClient (firstCell, readingFields, acceptedSamples) where

import Data.IORef (newIORef, readIORef)
import DirectIndexedList (IndexedList(IndexedList), Reading(Reading), Sample(..), SomeSample)

-- | Read the first cell after rebuilding the list at its original index.
firstCell :: IO [Bool]
firstCell = do
    xs <- newIORef (True : error "forced direct indexed list tail") >>= readIORef
    pure $ case rebuild (IndexedList xs) of
        IndexedList ys -> take 1 ys
  where
    rebuild :: IndexedList xs -> IndexedList xs
    rebuild (IndexedList ys) = IndexedList ys

-- | Adjust a reading and preserve the calibration index.
adjust :: (Int -> Int) -> Reading calibrated -> Reading calibrated
adjust f (Reading calibrated value) = Reading calibrated (f value)

-- | Each observation can leave the other field unevaluated.
readingFields :: Bool -> Int -> (Bool, Int)
readingFields calibrated value =
    (fst (fields calibrated (error "forced an unused reading")),
     snd (fields (error "forced an unused calibration flag") value))
  where
    fields flag n = case adjust (+ 1) (Reading flag n) of
        Reading stored result -> (stored, result)

-- | Read only samples recorded with calibration.
acceptedSamples :: Bool -> [Maybe Int]
acceptedSamples calibrated =
    [ accept (record calibrated 37)
    , accept (record False (error "forced uncalibrated sample"))
    , accept (MissingSample :: SomeSample Int)
    ]
  where
    record :: Bool -> a -> SomeSample a
    record flag value = Sample flag value

    accept :: Sample a status -> Maybe a
    accept MissingSample = Nothing
    accept (Sample False _) = Nothing
    accept (Sample True value) = Just value
