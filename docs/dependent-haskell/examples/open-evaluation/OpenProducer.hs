{-# LANGUAGE BangPatterns, NoExperimentalDependentTypes #-}
{-# OPTIONS_GHC -fno-unoptimized-core-for-interpreter -fno-omit-interface-pragmas -fignore-interface-pragmas #-}
module OpenProducer (select, second, strictSecond, routeLeft, routeRight, pairFirst, strictPairFirst, ignorePair) where

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

-- | Return the first field of a boxed pair.
{-# INLINE pairFirst #-}
pairFirst :: (t, t) -> t
pairFirst (x, _) = x

-- | Demand the second field before returning the first.
{-# INLINE strictPairFirst #-}
strictPairFirst :: (u, v) -> u
strictPairFirst (x, !_) = x

-- | Return the next argument without inspecting the pair.
{-# INLINE ignorePair #-}
ignorePair :: (u, v) -> w -> w
ignorePair _ x = x
