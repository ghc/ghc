{-# LANGUAGE CPP #-}
#if !defined(TESTING) && defined(__GLASGOW_HASKELL__)
{-# LANGUAGE Safe #-}
#endif

-- | A strict pair

module GHC.Utils.Containers.Internal.StrictPair (StrictPair(..), toPair) where

import Prelude () -- for build ordering; see #23942 and
                  -- Note [Depend on GHC.Num.Integer] in base:GHC.Base

-- | The same as a regular Haskell pair, but
--
-- @
-- (x :*: _|_) = (_|_ :*: y) = _|_
-- @
data StrictPair a b = !a :*: !b

infixr 1 :*:

-- | Convert a strict pair to a standard pair.
toPair :: StrictPair a b -> (a, b)
toPair (x :*: y) = (x, y)
{-# INLINE toPair #-}
