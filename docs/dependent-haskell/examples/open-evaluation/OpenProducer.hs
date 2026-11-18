{-# LANGUAGE NoExperimentalDependentTypes #-}
module OpenProducer (select) where

import qualified OpenAlias

-- | Reuse the independently defined worker.
{-# INLINE select #-}
select :: u -> u
select = OpenAlias.worker
