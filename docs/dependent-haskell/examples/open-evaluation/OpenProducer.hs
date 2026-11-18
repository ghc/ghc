{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
module OpenProducer (select, second, strictSecond) where

import qualified OpenAlias

-- | Reuse the independently defined worker.
{-# INLINE select #-}
select :: u -> u
select = OpenAlias.worker

-- | Return the second argument without inspecting the first.
{-# INLINE second #-}
second :: u -> v -> v
second _ y = y

-- | Demand the first argument before returning the second.
{-# INLINE strictSecond #-}
strictSecond :: u -> v -> v
strictSecond !_ y = y
