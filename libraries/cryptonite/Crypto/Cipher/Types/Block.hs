-- |
-- Module      : Crypto.Cipher.Types.Block
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- Block cipher basic types
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE Rank2Types #-}
module Crypto.Cipher.Types.Block
    (
    -- * BlockCipher
      BlockCipher(..)
    , BlockCipher128(..)
    -- * Initialization vector (IV)
    , IV(..)
    , makeIV
    , nullIV
    , ivAdd
    -- * XTS
    , XTS
    -- * AEAD
    , AEAD(..)
    -- , AEADState(..)
    , AEADModeImpl(..)
    , aeadAppendHeader
    , aeadEncrypt
    , aeadDecrypt
    , aeadFinalize
    -- * CFB 8 bits
    --, cfb8Encrypt
    --, cfb8Decrypt
    ) where

import           Data.Word
import           Crypto.Error
import           Crypto.Cipher.Types.Base
import           Crypto.Cipher.Types.GF
import           Crypto.Cipher.Types.AEAD
import           Crypto.Cipher.Types.Utils

import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, withByteArray, Bytes)
import qualified Crypto.Internal.ByteArray as B

import           Foreign.Ptr
import           Foreign.Storable

-- | an IV parametrized by the cipher
data IV c = forall byteArray . ByteArray byteArray => IV byteArray

instance BlockCipher c => ByteArrayAccess (IV c) where
    withByteArray (IV z) f = withByteArray z f
    length (IV z) = B.length z
instance Eq (IV c) where
    (IV a) == (IV b) = B.eq a b

-- | XTS callback
type XTS ba cipher = (cipher, cipher)
                  -> IV cipher        -- ^ Usually represent the Data Unit (e.g. disk sector)
                  -> DataUnitOffset   -- ^ Offset in the data unit in number of blocks
                  -> ba               -- ^ Data
                  -> ba               -- ^ Processed Data

-- | Symmetric block cipher class
class Cipher cipher => BlockCipher cipher where
    -- | Return the size of block required for this block cipher
    blockSize    :: cipher -> Int

    -- | Encrypt blocks
    --
    -- the input string need to be multiple of the block size
    ecbEncrypt :: ByteArray ba => cipher -> ba -> ba

    -- | Decrypt blocks
    --
    -- the input string need to be multiple of the block size
    ecbDecrypt :: ByteArray ba => cipher -> ba -> ba

    -- | encrypt using the CBC mode.
    --
    -- input need to be a multiple of the blocksize
    cbcEncrypt :: ByteArray ba => cipher -> IV cipher -> ba -> ba
    cbcEncrypt = cbcEncryptGeneric
    -- | decrypt using the CBC mode.
    --
    -- input need to be a multiple of the blocksize
    cbcDecrypt :: ByteArray ba => cipher -> IV cipher -> ba -> ba
    cbcDecrypt = cbcDecryptGeneric

    -- | encrypt using the CFB mode.
    --
    -- input need to be a multiple of the blocksize
    cfbEncrypt :: ByteArray ba => cipher -> IV cipher -> ba -> ba
    cfbEncrypt = cfbEncryptGeneric
    -- | decrypt using the CFB mode.
    --
    -- input need to be a multiple of the blocksize
    cfbDecrypt :: ByteArray ba => cipher -> IV cipher -> ba -> ba
    cfbDecrypt = cfbDecryptGeneric

    -- | combine using the CTR mode.
    --
    -- CTR mode produce a stream of randomized data that is combined
    -- (by XOR operation) with the input stream.
    --
    -- encryption and decryption are the same operation.
    --
    -- input can be of any size
    ctrCombine :: ByteArray ba => cipher -> IV cipher -> ba -> ba
    ctrCombine = ctrCombineGeneric

    -- | Initialize a new AEAD State
    --
    -- When Nothing is returns, it means the mode is not handled.
    aeadInit :: ByteArrayAccess iv => AEADMode -> cipher -> iv -> CryptoFailable (AEAD cipher)
    aeadInit _ _ _ = CryptoFailed CryptoError_AEADModeNotSupported

-- | class of block cipher with a 128 bits block size
class BlockCipher cipher => BlockCipher128 cipher where
    -- | encrypt using the XTS mode.
    --
    -- input need to be a multiple of the blocksize, and the cipher
    -- need to process 128 bits block only
    xtsEncrypt :: ByteArray ba
               => (cipher, cipher)
               -> IV cipher        -- ^ Usually represent the Data Unit (e.g. disk sector)
               -> DataUnitOffset   -- ^ Offset in the data unit in number of blocks
               -> ba               -- ^ Plaintext
               -> ba               -- ^ Ciphertext
    xtsEncrypt = xtsEncryptGeneric

    -- | decrypt using the XTS mode.
    --
    -- input need to be a multiple of the blocksize, and the cipher
    -- need to process 128 bits block only
    xtsDecrypt :: ByteArray ba
               => (cipher, cipher)
               -> IV cipher        -- ^ Usually represent the Data Unit (e.g. disk sector)
               -> DataUnitOffset   -- ^ Offset in the data unit in number of blocks
               -> ba               -- ^ Ciphertext
               -> ba               -- ^ Plaintext
    xtsDecrypt = xtsDecryptGeneric

-- | Create an IV for a specified block cipher
makeIV :: (ByteArrayAccess b, BlockCipher c) => b -> Maybe (IV c)
makeIV b = toIV undefined
  where toIV :: BlockCipher c => c -> Maybe (IV c)
        toIV cipher
          | B.length b == sz = Just $ IV (B.convert b :: Bytes)
          | otherwise        = Nothing
          where sz = blockSize cipher

-- | Create an IV that is effectively representing the number 0
nullIV :: BlockCipher c => IV c
nullIV = toIV undefined
  where toIV :: BlockCipher c => c -> IV c
        toIV cipher = IV (B.zero (blockSize cipher) :: Bytes)

-- | Increment an IV by a number.
--
-- Assume the IV is in Big Endian format.
ivAdd :: IV c -> Int -> IV c
ivAdd (IV b) i = IV $ copy b
  where copy :: ByteArray bs => bs -> bs
        copy bs = B.copyAndFreeze bs $ loop i (B.length bs - 1)

        loop :: Int -> Int -> Ptr Word8 -> IO ()
        loop acc ofs p
            | ofs < 0   = return ()
            | otherwise = do
                v <- peek (p `plusPtr` ofs) :: IO Word8
                let accv    = acc + fromIntegral v
                    (hi,lo) = accv `divMod` 256
                poke (p `plusPtr` ofs) (fromIntegral lo :: Word8)
                loop hi (ofs - 1) p

cbcEncryptGeneric :: (ByteArray ba, BlockCipher cipher) => cipher -> IV cipher -> ba -> ba
cbcEncryptGeneric cipher ivini input = mconcat $ doEnc ivini $ chunk (blockSize cipher) input
  where doEnc _  []     = []
        doEnc iv (i:is) =
            let o = ecbEncrypt cipher $ B.xor iv i
             in o : doEnc (IV o) is

cbcDecryptGeneric :: (ByteArray ba, BlockCipher cipher) => cipher -> IV cipher -> ba -> ba
cbcDecryptGeneric cipher ivini input = mconcat $ doDec ivini $ chunk (blockSize cipher) input
  where
        doDec _  []     = []
        doDec iv (i:is) =
            let o = B.xor iv $ ecbDecrypt cipher i
             in o : doDec (IV i) is

cfbEncryptGeneric :: (ByteArray ba, BlockCipher cipher) => cipher -> IV cipher -> ba -> ba
cfbEncryptGeneric cipher ivini input = mconcat $ doEnc ivini $ chunk (blockSize cipher) input
  where
        doEnc _  []     = []
        doEnc (IV iv) (i:is) =
            let o = B.xor i $ ecbEncrypt cipher iv
             in o : doEnc (IV o) is

cfbDecryptGeneric :: (ByteArray ba, BlockCipher cipher) => cipher -> IV cipher -> ba -> ba
cfbDecryptGeneric cipher ivini input = mconcat $ doDec ivini $ chunk (blockSize cipher) input
  where
        doDec _  []     = []
        doDec (IV iv) (i:is) =
            let o = B.xor i $ ecbEncrypt cipher iv
             in o : doDec (IV i) is

ctrCombineGeneric :: (ByteArray ba, BlockCipher cipher) => cipher -> IV cipher -> ba -> ba
ctrCombineGeneric cipher ivini input = mconcat $ doCnt ivini $ chunk (blockSize cipher) input
  where doCnt _  [] = []
        doCnt iv@(IV ivd) (i:is) =
            let ivEnc = ecbEncrypt cipher ivd
             in B.xor i ivEnc : doCnt (ivAdd iv 1) is

xtsEncryptGeneric :: (ByteArray ba, BlockCipher128 cipher) => XTS ba cipher
xtsEncryptGeneric = xtsGeneric ecbEncrypt

xtsDecryptGeneric :: (ByteArray ba, BlockCipher128 cipher) => XTS ba cipher
xtsDecryptGeneric = xtsGeneric ecbDecrypt

xtsGeneric :: (ByteArray ba, BlockCipher128 cipher)
           => (cipher -> ba -> ba)
           -> (cipher, cipher)
           -> IV cipher
           -> DataUnitOffset
           -> ba
           -> ba
xtsGeneric f (cipher, tweakCipher) (IV iv) sPoint input =
    mconcat $ doXts iniTweak $ chunk (blockSize cipher) input
  where encTweak = ecbEncrypt tweakCipher iv
        iniTweak = iterate xtsGFMul encTweak !! fromIntegral sPoint
        doXts _     []     = []
        doXts tweak (i:is) =
            let o = B.xor (f cipher $ B.xor i tweak) tweak
             in o : doXts (xtsGFMul tweak) is

{-
-- | Encrypt using CFB mode in 8 bit output
--
-- Effectively turn a Block cipher in CFB mode into a Stream cipher
cfb8Encrypt :: BlockCipher a => a -> IV a -> B.byteString -> B.byteString
cfb8Encrypt ctx origIv msg = B.unsafeCreate (B.length msg) $ \dst -> loop dst origIv msg
  where loop d iv@(IV i) m
            | B.null m  = return ()
            | otherwise = poke d out >> loop (d `plusPtr` 1) ni (B.drop 1 m)
          where m'  = if B.length m < blockSize ctx
                            then m `B.append` B.replicate (blockSize ctx - B.length m) 0
                            else B.take (blockSize ctx) m
                r   = cfbEncrypt ctx iv m'
                out = B.head r
                ni  = IV (B.drop 1 i `B.snoc` out)

-- | Decrypt using CFB mode in 8 bit output
--
-- Effectively turn a Block cipher in CFB mode into a Stream cipher
cfb8Decrypt :: BlockCipher a => a -> IV a -> B.byteString -> B.byteString
cfb8Decrypt ctx origIv msg = B.unsafeCreate (B.length msg) $ \dst -> loop dst origIv msg
  where loop d iv@(IV i) m
            | B.null m  = return ()
            | otherwise = poke d out >> loop (d `plusPtr` 1) ni (B.drop 1 m)
          where m'  = if B.length m < blockSize ctx
                            then m `B.append` B.replicate (blockSize ctx - B.length m) 0
                            else B.take (blockSize ctx) m
                r   = cfbDecrypt ctx iv m'
                out = B.head r
                ni  = IV (B.drop 1 i `B.snoc` B.head m')
-}
