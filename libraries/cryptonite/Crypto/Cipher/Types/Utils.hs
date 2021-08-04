-- |
-- Module      : Crypto.Cipher.Types.Utils
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- Basic utility for cipher related stuff
--
module Crypto.Cipher.Types.Utils where

import           Crypto.Internal.ByteArray (ByteArray)
import qualified Crypto.Internal.ByteArray as B

-- | Chunk some input byte array into @sz byte list of byte array.
chunk :: ByteArray b => Int -> b -> [b]
chunk sz bs = split bs
  where split b | B.length b <= sz = [b]
                | otherwise        =
                        let (b1, b2) = B.splitAt sz b
                         in b1 : split b2
