{-# LANGUAGE ExperimentalDependentTypes #-}

module RetainedPrint where

keep :: foreach -- Retain this Boolean argument.
    (b :: Bool)
    -> Select b -> Select b
keep b x = x

nested :: (foreach (b :: Bool) -> Select b) -> Bool
nested x = True
