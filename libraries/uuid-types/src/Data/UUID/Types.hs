{-# LANGUAGE CPP         #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif

{- |
Module      : Data.UUID.Types
Copyright   : (c) 2017-2018 Herbert Valerio Riedel
              (c) 2008,2012 Antoine Latter

License     : BSD-style

Maintainer  : hvr@gnu.org
Portability : portable

This library is useful for comparing, parsing and
printing <http://en.wikipedia.org/wiki/UUID Universally Unique Identifiers (UUID)>.
See <http://tools.ietf.org/html/rfc4122 RFC 4122> for the specification.

-}
module Data.UUID.Types
    ( -- * The 'UUID' Type
      UUID
      -- * Nil UUID
    , nil
    , null
      -- * Textual Representation
    , toString
    , fromString
    , toText
    , fromText
    , toASCIIBytes
    , fromASCIIBytes
    , toLazyASCIIBytes
    , fromLazyASCIIBytes
      -- * Binary Representation
    , toByteString
    , fromByteString
      -- * Integer Representation
    , toWords
    , fromWords
    , toWords64
    , fromWords64
    ) where

import           Data.UUID.Types.Internal
import           Prelude                  ()

-- Everything is really implemented in Data.UUID.Types.Internal, but I
-- don't want to export the constructors out of the package.
