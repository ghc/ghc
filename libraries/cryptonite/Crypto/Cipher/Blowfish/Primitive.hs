-- |
-- Module      : Crypto.Cipher.Blowfish.Primitive
-- License     : BSD-style
-- Stability   : experimental
-- Portability : Good

-- Rewritten by Vincent Hanquez (c) 2015
--              Lars Petersen (c) 2018
--
-- Original code:
--      Crypto.Cipher.Blowfish.Primitive, copyright (c) 2012 Stijn van Drongelen
--      based on: BlowfishAux.hs (C) 2002 HardCore SoftWare, Doug Hoyte
--           (as found in Crypto-4.2.4)
{-# LANGUAGE BangPatterns #-}
module Crypto.Cipher.Blowfish.Primitive
    ( Context
    , initBlowfish
    , encrypt
    , decrypt
    , KeySchedule
    , createKeySchedule
    , freezeKeySchedule
    , expandKey
    , expandKeyWithSalt
    , cipherBlockMutable
    ) where

import           Control.Monad              (when)
import           Data.Bits
import           Data.Memory.Endian
import           Data.Word

import           Crypto.Cipher.Blowfish.Box
import           Crypto.Error
import           Crypto.Internal.ByteArray  (ByteArray, ByteArrayAccess)
import qualified Crypto.Internal.ByteArray  as B
import           Crypto.Internal.Compat
import           Crypto.Internal.Imports
import           Crypto.Internal.WordArray

newtype Context = Context Array32

instance NFData Context where
    rnf a = a `seq` ()

-- | Initialize a new Blowfish context from a key.
--
-- key needs to be between 0 and 448 bits.
initBlowfish :: ByteArrayAccess key => key -> CryptoFailable Context
initBlowfish key
    | B.length key > (448 `div` 8) = CryptoFailed CryptoError_KeySizeInvalid
    | otherwise                    = CryptoPassed $ unsafeDoIO $ do
        ks <- createKeySchedule
        expandKey ks key
        freezeKeySchedule ks

-- | Get an immutable Blowfish context by freezing a mutable key schedule.
freezeKeySchedule :: KeySchedule -> IO Context
freezeKeySchedule (KeySchedule ma) = Context `fmap` mutableArray32Freeze ma

expandKey :: (ByteArrayAccess key) => KeySchedule -> key -> IO ()
expandKey ks@(KeySchedule ma) key = do
    when (B.length key > 0) $ iterKeyStream key 0 0 $ \i l r a0 a1 cont-> do
        mutableArrayWriteXor32 ma i l
        mutableArrayWriteXor32 ma (i + 1) r
        when (i + 2 < 18) (cont a0 a1)
    loop 0 0 0
    where
        loop i l r = do
            n <- cipherBlockMutable ks (fromIntegral l `shiftL` 32 .|. fromIntegral r)
            let nl = fromIntegral (n `shiftR` 32)
                nr = fromIntegral (n .&. 0xffffffff)
            mutableArrayWrite32 ma i nl
            mutableArrayWrite32 ma (i + 1) nr
            when (i < 18 + 1024) (loop (i + 2) nl nr)

expandKeyWithSalt :: (ByteArrayAccess key, ByteArrayAccess salt)
    => KeySchedule
    -> key
    -> salt
    -> IO ()
expandKeyWithSalt ks key salt
    | B.length salt == 16 = expandKeyWithSalt128 ks key (fromBE $ B.toW64BE salt 0) (fromBE $ B.toW64BE salt 8)
    | otherwise           = expandKeyWithSaltAny ks key salt

expandKeyWithSaltAny :: (ByteArrayAccess key, ByteArrayAccess salt)
    => KeySchedule         -- ^ The key schedule
    -> key                 -- ^ The key
    -> salt                -- ^ The salt
    -> IO ()
expandKeyWithSaltAny ks@(KeySchedule ma) key salt = do
    when (B.length key > 0) $ iterKeyStream key 0 0 $ \i l r a0 a1 cont-> do
        mutableArrayWriteXor32 ma i l
        mutableArrayWriteXor32 ma (i + 1) r
        when (i + 2 < 18) (cont a0 a1)
    -- Go through the entire key schedule overwriting the P-Array and S-Boxes
    when (B.length salt > 0) $ iterKeyStream salt 0 0 $ \i l r a0 a1 cont-> do
        let l' = xor l a0
        let r' = xor r a1
        n <- cipherBlockMutable ks (fromIntegral l' `shiftL` 32 .|. fromIntegral r')
        let nl = fromIntegral (n `shiftR` 32)
            nr = fromIntegral (n .&. 0xffffffff)
        mutableArrayWrite32 ma i nl
        mutableArrayWrite32 ma (i + 1) nr
        when (i + 2 < 18 + 1024) (cont nl nr)

expandKeyWithSalt128 :: ByteArrayAccess ba
    => KeySchedule         -- ^ The key schedule
    -> ba                  -- ^ The key
    -> Word64              -- ^ First word of the salt
    -> Word64              -- ^ Second word of the salt
    -> IO ()
expandKeyWithSalt128 ks@(KeySchedule ma) key salt1 salt2 = do
    when (B.length key > 0) $ iterKeyStream key 0 0 $ \i l r a0 a1 cont-> do
        mutableArrayWriteXor32 ma i l
        mutableArrayWriteXor32 ma (i + 1) r
        when (i + 2 < 18) (cont a0 a1)
    -- Go through the entire key schedule overwriting the P-Array and S-Boxes
    loop 0 salt1 salt1 salt2
    where
        loop i input slt1 slt2
            | i == 1042   = return ()
            | otherwise = do
                n <- cipherBlockMutable ks input
                let nl = fromIntegral (n `shiftR` 32)
                    nr = fromIntegral (n .&. 0xffffffff)
                mutableArrayWrite32 ma i     nl
                mutableArrayWrite32 ma (i+1) nr
                loop (i+2) (n `xor` slt2) slt2 slt1

-- | Encrypt blocks
--
-- Input need to be a multiple of 8 bytes
encrypt :: ByteArray ba => Context -> ba -> ba
encrypt ctx ba
    | B.length ba == 0         = B.empty
    | B.length ba `mod` 8 /= 0 = error "invalid data length"
    | otherwise                = B.mapAsWord64 (cipherBlock ctx False) ba

-- | Decrypt blocks
--
-- Input need to be a multiple of 8 bytes
decrypt :: ByteArray ba => Context -> ba -> ba
decrypt ctx ba
    | B.length ba == 0         = B.empty
    | B.length ba `mod` 8 /= 0 = error "invalid data length"
    | otherwise                = B.mapAsWord64 (cipherBlock ctx True) ba

-- | Encrypt or decrypt a single block of 64 bits.
--
-- The inverse argument decides whether to encrypt or decrypt.
cipherBlock :: Context -> Bool -> Word64 -> Word64
cipherBlock (Context ar) inverse input = doRound input 0
    where
    -- | Transform the input over 16 rounds
    doRound :: Word64 -> Int -> Word64
    doRound !i roundIndex
        | roundIndex == 16 =
            let final = (fromIntegral (p 16) `shiftL` 32) .|. fromIntegral (p 17)
             in rotateL (i `xor` final) 32
        | otherwise     =
            let newr = fromIntegral (i `shiftR` 32) `xor` p roundIndex
                newi = ((i `shiftL` 32) `xor` f newr) .|. fromIntegral newr
             in doRound newi (roundIndex+1)

    -- | The Blowfish Feistel function F
    f   :: Word32 -> Word64
    f t = let a = s0 (0xff .&. (t `shiftR` 24))
              b = s1 (0xff .&. (t `shiftR` 16))
              c = s2 (0xff .&. (t `shiftR` 8))
              d = s3 (0xff .&.  t)
           in fromIntegral (((a + b) `xor` c) + d) `shiftL` 32

    -- | S-Box arrays, each containing 256 32-bit words
    --   The first 18 words contain the P-Array of subkeys
    s0, s1, s2, s3 :: Word32 -> Word32
    s0 i            = arrayRead32 ar (fromIntegral i + 18)
    s1 i            = arrayRead32 ar (fromIntegral i + 274)
    s2 i            = arrayRead32 ar (fromIntegral i + 530)
    s3 i            = arrayRead32 ar (fromIntegral i + 786)
    p              :: Int -> Word32
    p i | inverse   = arrayRead32 ar (17 - i)
        | otherwise = arrayRead32 ar i

-- | Blowfish encrypt a Word using the current state of the key schedule
cipherBlockMutable :: KeySchedule -> Word64 -> IO Word64
cipherBlockMutable (KeySchedule ma) input = doRound input 0
    where
    -- | Transform the input over 16 rounds
    doRound !i roundIndex
        | roundIndex == 16 = do
            pVal1 <- mutableArrayRead32 ma 16
            pVal2 <- mutableArrayRead32 ma 17
            let final = (fromIntegral pVal1 `shiftL` 32) .|. fromIntegral pVal2
            return $ rotateL (i `xor` final) 32
        | otherwise     = do
            pVal <- mutableArrayRead32 ma roundIndex
            let newr = fromIntegral (i `shiftR` 32) `xor` pVal
            newr' <- f newr
            let newi = ((i `shiftL` 32) `xor` newr') .|. fromIntegral newr
            doRound newi (roundIndex+1)

    -- | The Blowfish Feistel function F
    f   :: Word32 -> IO Word64
    f t = do
        a <- s0 (0xff .&. (t `shiftR` 24))
        b <- s1 (0xff .&. (t `shiftR` 16))
        c <- s2 (0xff .&. (t `shiftR` 8))
        d <- s3 (0xff .&.  t)
        return (fromIntegral (((a + b) `xor` c) + d) `shiftL` 32)

    -- | S-Box arrays, each containing 256 32-bit words
    --   The first 18 words contain the P-Array of subkeys
    s0, s1, s2, s3 :: Word32 -> IO Word32
    s0 i = mutableArrayRead32 ma (fromIntegral i + 18)
    s1 i = mutableArrayRead32 ma (fromIntegral i + 274)
    s2 i = mutableArrayRead32 ma (fromIntegral i + 530)
    s3 i = mutableArrayRead32 ma (fromIntegral i + 786)

iterKeyStream :: (ByteArrayAccess x)
    => x
    -> Word32
    -> Word32
    -> (Int -> Word32 -> Word32 -> Word32 -> Word32 -> (Word32 -> Word32 -> IO ()) -> IO ())
    -> IO ()
iterKeyStream x a0 a1 g = f 0 0 a0 a1
    where
        len          = B.length x
        -- Avoiding the modulo operation when interating over the ring
        -- buffer is assumed to be more efficient here. All other
        -- implementations do this, too. The branch prediction shall prefer
        -- the branch with the increment.
        n j          = if j + 1 >= len then 0 else j + 1
        f i j0 b0 b1 = g i l r b0 b1 (f (i + 2) j8)
            where
                j1 = n j0
                j2 = n j1
                j3 = n j2
                j4 = n j3
                j5 = n j4
                j6 = n j5
                j7 = n j6
                j8 = n j7
                x0 = fromIntegral (B.index x j0)
                x1 = fromIntegral (B.index x j1)
                x2 = fromIntegral (B.index x j2)
                x3 = fromIntegral (B.index x j3)
                x4 = fromIntegral (B.index x j4)
                x5 = fromIntegral (B.index x j5)
                x6 = fromIntegral (B.index x j6)
                x7 = fromIntegral (B.index x j7)
                l  = shiftL x0 24 .|. shiftL x1 16 .|. shiftL x2 8 .|. x3
                r  = shiftL x4 24 .|. shiftL x5 16 .|. shiftL x6 8 .|. x7
{-# INLINE iterKeyStream #-}
-- Benchmarking shows that GHC considers this function too big to inline
-- although forcing inlining causes an actual improvement.
-- It is assumed that all function calls (especially the continuation)
-- collapse into a tight loop after inlining.
