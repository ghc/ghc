-- |
-- Module      : Crypto.MAC.CMAC
-- License     : BSD-style
-- Maintainer  : Kei Hibino <ex8k.hibino@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Provide the CMAC (Cipher based Message Authentification Code) base algorithm.
-- <http://en.wikipedia.org/wiki/CMAC>
-- <http://csrc.nist.gov/publications/nistpubs/800-38B/SP_800-38B.pdf>
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.MAC.CMAC
    ( cmac
    , CMAC
    , subKeys
    ) where

import           Data.Word
import           Data.Bits (setBit, testBit, shiftL)
import           Data.List (foldl')

import           Crypto.Cipher.Types
import           Crypto.Internal.ByteArray (ByteArrayAccess, ByteArray, Bytes)
import qualified Crypto.Internal.ByteArray as B

-- | Authentication code
newtype CMAC a = CMAC Bytes
    deriving (ByteArrayAccess)

instance Eq (CMAC a) where
  CMAC b1 == CMAC b2  =  B.constEq b1 b2

-- | compute a MAC using the supplied cipher
cmac :: (ByteArrayAccess bin, BlockCipher cipher)
     => cipher      -- ^ key to compute CMAC with
     -> bin         -- ^ input message
     -> CMAC cipher -- ^ output tag
cmac k msg =
    CMAC $ foldl' (\c m -> ecbEncrypt k $ bxor c m) zeroV ms
  where
    bytes = blockSize k
    zeroV = B.replicate bytes 0 :: Bytes
    (k1, k2) = subKeys k
    ms = cmacChunks k k1 k2 $ B.convert msg

cmacChunks :: (BlockCipher k, ByteArray ba) => k -> ba -> ba -> ba -> [ba]
cmacChunks k k1 k2  =  rec'  where
    rec' msg
      | B.null tl  =  if lack == 0
                      then  [bxor k1 hd]
                      else  [bxor k2 $ hd `B.append` B.pack (0x80 : replicate (lack - 1) 0)]
      | otherwise  =        hd : rec' tl
      where
          bytes = blockSize k
          (hd, tl) = B.splitAt bytes msg
          lack = bytes - B.length hd

-- | make sub-keys used in CMAC
subKeys :: (BlockCipher k, ByteArray ba)
        => k         -- ^ key to compute CMAC with
        -> (ba, ba)  -- ^ sub-keys to compute CMAC
subKeys k = (k1, k2)   where
    ipt = cipherIPT k
    k0 = ecbEncrypt k $ B.replicate (blockSize k) 0
    k1 = subKey ipt k0
    k2 = subKey ipt k1

-- polynomial multiply operation to culculate subkey
subKey :: (ByteArray ba) => [Word8] -> ba -> ba
subKey ipt ws  =  case B.unpack ws of
    []                  ->  B.empty
    w:_  | testBit w 7  ->  B.pack ipt `bxor` shiftL1 ws
         | otherwise    ->  shiftL1 ws

shiftL1 :: (ByteArray ba) => ba -> ba
shiftL1 = B.pack . shiftL1W . B.unpack

shiftL1W :: [Word8] -> [Word8]
shiftL1W []         =  []
shiftL1W ws@(_:ns)  =  rec' $ zip ws (ns ++ [0])   where
    rec'  []         =  []
    rec' ((x,y):ps)  =  w : rec' ps
      where
          w | testBit y 7  =  setBit sl1 0
            | otherwise    =  sl1
            where     sl1 = shiftL x 1

bxor :: ByteArray ba => ba -> ba -> ba
bxor = B.xor


-----


cipherIPT :: BlockCipher k => k -> [Word8]
cipherIPT = expandIPT . blockSize

-- Data type which represents the smallest irreducibule binary polynomial
-- against specified degree.
--
-- Maximum degree bit and degree 0 bit are omitted.
-- For example, The value /Q 7 2 1/ corresponds to the degree /128/.
-- It represents that the smallest irreducible binary polynomial of degree 128
-- is x^128 + x^7 + x^2 + x^1 + 1.
data IPolynomial
  = Q Int Int Int
---  | T Int

iPolynomial :: Int -> Maybe IPolynomial
iPolynomial = d  where
    d   64  =  Just $ Q 4 3 1
    d  128  =  Just $ Q 7 2 1
    d    _  =  Nothing

-- Expand a tail bit pattern of irreducible binary polynomial
expandIPT :: Int -> [Word8]
expandIPT bytes = expandIPT' bytes ipt  where
    ipt = maybe (error $ "Irreducible binary polynomial not defined against " ++ show nb ++ " bit") id
          $ iPolynomial nb
    nb = bytes * 8

-- Expand a tail bit pattern of irreducible binary polynomial
expandIPT' :: Int         -- ^ width in byte
           -> IPolynomial -- ^ irreducible binary polynomial definition
           -> [Word8]     -- ^ result bit pattern
expandIPT' bytes (Q x y z) =
    reverse . setB x . setB y . setB z . setB 0 $ replicate bytes 0
  where
    setB i ws =  hd ++ setBit (head tl) r : tail tl  where
        (q, r) = i `quotRem` 8
        (hd, tl) = splitAt q ws
