{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs #-}
module DirectIndexedList (IndexedList(IndexedList), Reading(Reading), Sample(..), SomeSample) where

-- | Store a runtime list at its own index.
data IndexedList (xs :: [Bool]) where
    IndexedList :: foreach (ys :: [Bool]) -> IndexedList ys

-- | Record whether the instrument was calibrated when it took the reading.
data Reading (calibrated :: Bool) where
    Reading :: foreach (b :: Bool) -> Int -> Reading b

-- | Store a sample with its calibration status, or record its absence.
data Sample a (status :: Maybe Bool) where
    MissingSample :: Sample a 'Nothing
    Sample :: foreach (calibrated :: Bool) -> a -> Sample a ('Just calibrated)

-- | Hide the calibration status at the point of recording.
type SomeSample a = exists (status :: Maybe Bool). Sample a status
