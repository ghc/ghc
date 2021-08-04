-- |
-- Module      : Crypto.ConstructHash.MiyaguchiPreneel
-- License     : BSD-style
-- Maintainer  : Kei Hibino <ex8k.hibino@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Provide the hash function construction method from block cipher
-- <https://en.wikipedia.org/wiki/One-way_compression_function>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.ConstructHash.MiyaguchiPreneel
       ( compute, compute'
       , MiyaguchiPreneel
       ) where

import           Data.List (foldl')

import           Crypto.Data.Padding (pad, Format (ZERO))
import           Crypto.Cipher.Types
import           Crypto.Error (throwCryptoError)
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, Bytes)
import qualified Crypto.Internal.ByteArray as B


newtype MiyaguchiPreneel a = MP Bytes
    deriving (ByteArrayAccess)

instance Eq (MiyaguchiPreneel a) where
    MP b1 == MP b2  =  B.constEq b1 b2


-- | Compute Miyaguchi-Preneel one way compress using the supplied block cipher.
compute' :: (ByteArrayAccess bin, BlockCipher cipher)
         => (Bytes -> cipher)       -- ^ key build function to compute Miyaguchi-Preneel. care about block-size and key-size
         -> bin                     -- ^ input message
         -> MiyaguchiPreneel cipher -- ^ output tag
compute' g = MP . foldl' (step $ g) (B.replicate bsz 0) . chunks . pad (ZERO bsz) . B.convert
  where
    bsz = blockSize ( g B.empty {- dummy to get block size -} )
    chunks msg
      | B.null msg  =  []
      | otherwise  =   (hd :: Bytes) : chunks tl
      where
        (hd, tl) = B.splitAt bsz msg

-- | Compute Miyaguchi-Preneel one way compress using the inferred block cipher.
--   Only safe when KEY-SIZE equals to BLOCK-SIZE.
--
--   Simple usage /mp' msg :: MiyaguchiPreneel AES128/
compute :: (ByteArrayAccess bin, BlockCipher cipher)
        => bin                     -- ^ input message
        -> MiyaguchiPreneel cipher -- ^ output tag
compute = compute' $ throwCryptoError . cipherInit

-- | computation step of Miyaguchi-Preneel
step :: (ByteArray ba, BlockCipher k)
     => (ba -> k)
     -> ba
     -> ba
     -> ba
step g iv msg =
    ecbEncrypt k msg `bxor` iv `bxor` msg
  where
    k = g iv

bxor :: ByteArray ba => ba -> ba -> ba
bxor = B.xor
