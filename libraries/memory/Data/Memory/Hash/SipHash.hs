-- |
-- Module      : Data.Memory.Hash.SipHash
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
-- provide the SipHash algorithm.
-- reference: <http://131002.net/siphash/siphash.pdf>
--
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Memory.Hash.SipHash
    ( SipKey(..)
    , SipHash(..)
    , hash
    , hashWith
    ) where

import           Data.Memory.Endian
import           Data.Memory.Internal.Compat
import           Data.Word
import           Data.Bits
import           Data.Typeable (Typeable)
import           Control.Monad
import           Foreign.Ptr
import           Foreign.Storable

-- | SigHash Key
data SipKey = SipKey {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | Siphash tag value
newtype SipHash = SipHash Word64
    deriving (Show,Eq,Ord,Typeable)

data InternalState = InternalState {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64

-- | produce a siphash with a key and a memory pointer + length.
hash :: SipKey -> Ptr Word8 -> Int -> IO SipHash
hash = hashWith 2 4

-- | same as 'hash', except also specifies the number of sipround iterations for compression and digest.
hashWith :: Int       -- ^ siphash C
         -> Int       -- ^ siphash D
         -> SipKey    -- ^ key for the hash
         -> Ptr Word8 -- ^ memory pointer
         -> Int       -- ^ length of the data
         -> IO SipHash
hashWith c d key startPtr totalLen = runHash (initSip key) startPtr totalLen
  where runHash !st !ptr l
            | l > 7     = peek (castPtr ptr) >>= \v -> runHash (process st (fromLE v)) (ptr `plusPtr` 8) (l-8)
            | otherwise = do
                let !lengthBlock = (fromIntegral totalLen `mod` 256) `unsafeShiftL` 56
                (finish . process st) `fmap` case l of
                    0 -> do return lengthBlock
                    1 -> do v0 <- peekByteOff ptr 0
                            return (lengthBlock .|. to64 v0)
                    2 -> do (v0,v1) <- liftM2 (,) (peekByteOff ptr 0) (peekByteOff ptr 1)
                            return (lengthBlock
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    3 -> do (v0,v1,v2) <- liftM3 (,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                            return (    lengthBlock
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    4 -> do (v0,v1,v2,v3) <- liftM4 (,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                                                          (peekByteOff ptr 3)
                            return (    lengthBlock
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    5 -> do (v0,v1,v2,v3,v4) <- liftM5 (,,,,) (peekByteOff ptr 0) (peekByteOff ptr 1) (peekByteOff ptr 2)
                                                              (peekByteOff ptr 3) (peekByteOff ptr 4)
                            return (    lengthBlock
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    6 -> do v0 <- peekByteOff ptr 0
                            v1 <- peekByteOff ptr 1
                            v2 <- peekByteOff ptr 2
                            v3 <- peekByteOff ptr 3
                            v4 <- peekByteOff ptr 4
                            v5 <- peekByteOff ptr 5
                            return (    lengthBlock
                                    .|. (to64 v5 `unsafeShiftL` 40)
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    7 -> do v0 <- peekByteOff ptr 0
                            v1 <- peekByteOff ptr 1
                            v2 <- peekByteOff ptr 2
                            v3 <- peekByteOff ptr 3
                            v4 <- peekByteOff ptr 4
                            v5 <- peekByteOff ptr 5
                            v6 <- peekByteOff ptr 6
                            return (    lengthBlock
                                    .|. (to64 v6 `unsafeShiftL` 48)
                                    .|. (to64 v5 `unsafeShiftL` 40)
                                    .|. (to64 v4 `unsafeShiftL` 32)
                                    .|. (to64 v3 `unsafeShiftL` 24)
                                    .|. (to64 v2 `unsafeShiftL` 16)
                                    .|. (to64 v1 `unsafeShiftL` 8)
                                    .|. to64 v0)
                    _ -> error "siphash: internal error: cannot happens"

        {-# INLINE to64 #-}
        to64 :: Word8 -> Word64
        to64 = fromIntegral

        {-# INLINE process #-}
        process istate m = newState
            where newState = postInject $! runRoundsCompression $! preInject istate
                  preInject  (InternalState v0 v1 v2 v3) = InternalState v0 v1 v2 (v3 `xor` m)
                  postInject (InternalState v0 v1 v2 v3) = InternalState (v0 `xor` m) v1 v2 v3

        {-# INLINE finish #-}
        finish istate = getDigest $! runRoundsDigest $! preInject istate
            where getDigest (InternalState v0 v1 v2 v3) = SipHash (v0 `xor` v1 `xor` v2 `xor` v3)
                  preInject (InternalState v0 v1 v2 v3) = InternalState v0 v1 (v2 `xor` 0xff) v3

        {-# INLINE doRound #-}
        doRound (InternalState v0 v1 v2 v3) =
              let !v0'    = v0 + v1
                  !v2'    = v2 + v3
                  !v1'    = v1 `rotateL` 13
                  !v3'    = v3 `rotateL` 16
                  !v1''   = v1' `xor` v0'
                  !v3''   = v3' `xor` v2'
                  !v0''   = v0' `rotateL` 32
                  !v2''   = v2' + v1''
                  !v0'''  = v0'' + v3''
                  !v1'''  = v1'' `rotateL` 17
                  !v3'''  = v3'' `rotateL` 21
                  !v1'''' = v1''' `xor` v2''
                  !v3'''' = v3''' `xor` v0'''
                  !v2'''  = v2'' `rotateL` 32
               in InternalState v0''' v1'''' v2''' v3''''

        {-# INLINE runRoundsCompression #-}
        runRoundsCompression st
            | c == 2    = doRound $! doRound st
            | otherwise = loopRounds c st

        {-# INLINE runRoundsDigest #-}
        runRoundsDigest st
            | d == 4    = doRound $! doRound $! doRound $! doRound st
            | otherwise = loopRounds d st

        {-# INLINE loopRounds #-}
        loopRounds 1 !v = doRound v
        loopRounds n !v = loopRounds (n-1) (doRound v)

        {-# INLINE initSip #-}
        initSip (SipKey k0 k1) = InternalState (k0 `xor` 0x736f6d6570736575)
                                               (k1 `xor` 0x646f72616e646f6d)
                                               (k0 `xor` 0x6c7967656e657261)
                                               (k1 `xor` 0x7465646279746573)
