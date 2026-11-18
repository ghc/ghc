{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas -fignore-interface-pragmas #-}
module OpenProducer (select, second, strictSecond, routeLeft, routeRight) where

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

-- | Select the first operand through the imported case.
{-# INLINE routeLeft #-}
routeLeft :: t -> t -> t
routeLeft = OpenAlias.route 'x'

-- | Select the second operand through the default.
{-# INLINE routeRight #-}
routeRight :: t -> t -> t
routeRight = OpenAlias.route '!'
