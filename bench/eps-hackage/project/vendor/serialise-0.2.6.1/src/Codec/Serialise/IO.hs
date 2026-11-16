-- |
-- Module      : Codec.Serialise.IO
-- Copyright   : (c) Duncan Coutts 2015-2017
-- License     : BSD3-style (see LICENSE.txt)
--
-- Maintainer  : duncan@community.haskell.org
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- High-level file-based API for serialising and deserialising values.
--
module Codec.Serialise.IO
  ( -- * @'FilePath'@ API
    writeFileSerialise
  , readFileDeserialise
    -- * @'System.IO.Handle'@ API
  , hPutSerialise
  ) where

import Codec.Serialise
