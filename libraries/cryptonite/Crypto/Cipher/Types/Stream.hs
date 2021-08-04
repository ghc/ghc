-- |
-- Module      : Crypto.Cipher.Types.Stream
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- Stream cipher basic types
--
module Crypto.Cipher.Types.Stream
    ( StreamCipher(..)
    ) where

import Crypto.Cipher.Types.Base
import Crypto.Internal.ByteArray (ByteArray)

-- | Symmetric stream cipher class
class Cipher cipher => StreamCipher cipher where
    -- | Combine using the stream cipher
    streamCombine :: ByteArray ba => cipher -> ba -> (ba, cipher)
