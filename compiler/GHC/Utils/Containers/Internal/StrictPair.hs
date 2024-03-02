{-# LANGUAGE CPP #-}

-- | A strict pair

module GHC.Utils.Containers.Internal.StrictPair (StrictPair(..), toPair) where

-- stupid build-order workaround until #23942 is properly fixed
import GHC.Base ()


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
