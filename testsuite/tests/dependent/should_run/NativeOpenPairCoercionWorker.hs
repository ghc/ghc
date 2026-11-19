{-# LANGUAGE ExperimentalDependentTypes #-}
module NativeOpenPairCoercionWorker (first, second, exchange, duplicate, same, duplicateRight) where

first :: (a, a) -> a
first (x, _) = x

second :: (a, a) -> a
second (_, y) = y

exchange :: (a, a) -> (a, a)
exchange (x, y) = (y, x)

duplicate :: (a, a) -> (a, a)
duplicate (x, _) = (x, x)

same :: (a, b) -> (a, b)
same (x, y) = (x, y)

duplicateRight :: (a, b) -> (b, b)
duplicateRight (_, y) = (y, y)
