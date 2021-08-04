-- |
-- Module         : Data.ByteString.Search.Substitution
-- Copyright      : Daniel Fischer
-- Licence        : BSD3
-- Maintainer     : Daniel Fischer <daniel.is.fischer@googlemail.com>
-- Stability      : Provisional
-- Portability    : portable
--
-- Class for values to be substituted into strict and lazy 'S.ByteString's
-- by the @replace@ functions defined in this package.
--
module Data.ByteString.Search.Substitution ( Substitution(..)) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as LI

-- | Type class of meaningful substitutions for replace functions
--   on ByteStrings. Instances for strict and lazy ByteStrings are
--   provided here.
class Substitution a where
    -- | @'substitution'@ transforms a value to a substitution function.
    substitution :: a -> ([S.ByteString] -> [S.ByteString])
    -- | @'prependCycle' sub lazyBS@ shall prepend infinitely many copies
    --   of @sub@ to @lazyBS@ without entering an infinite loop in case
    --   of an empty @sub@, so e.g.
    --
    -- @
    --   'prependCycle' \"\" \"ab\" == \"ab\"
    -- @
    --
    -- shall (quickly) evaluate to 'True'.
    -- For non-empty @sub@, the cycle shall be constructed efficiently.
    prependCycle :: a -> (L.ByteString -> L.ByteString)

instance Substitution S.ByteString where
    {-# INLINE substitution #-}
    substitution sub = if S.null sub then id else (sub :)
    {-# INLINE prependCycle #-}
    prependCycle sub
        | S.null sub    = id
        | otherwise     = let c = LI.Chunk sub c in const c

instance Substitution L.ByteString where
    {-# INLINE substitution #-}
    substitution LI.Empty = id
    substitution (LI.Chunk c t) = (c :) . flip (LI.foldrChunks (:)) t
    {-# INLINE prependCycle #-}
    prependCycle sub
        | L.null sub    = id
    prependCycle sub = let cyc = LI.foldrChunks LI.Chunk cyc sub in const cyc
