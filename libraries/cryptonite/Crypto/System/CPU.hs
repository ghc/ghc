-- |
-- Module      : Crypto.System.CPU
-- License     : BSD-style
-- Maintainer  : Olivier Chéron <olivier.cheron@gmail.com>
-- Stability   : experimental
-- Portability : unknown
--
-- Gives information about cryptonite runtime environment.
--
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Crypto.System.CPU
    ( ProcessorOption (..)
    , processorOptions
    ) where

import Data.Data
import Data.List (findIndices)
#ifdef SUPPORT_RDRAND
import Data.Maybe (isJust)
#endif
import Data.Word (Word8)
import Foreign.Ptr
import Foreign.Storable

import Crypto.Internal.Compat

#ifdef SUPPORT_RDRAND
import Crypto.Random.Entropy.RDRand
import Crypto.Random.Entropy.Source
#endif

-- | CPU options impacting cryptography implementation and library performance.
data ProcessorOption
    = AESNI   -- ^ Support for AES instructions, with flag @support_aesni@
    | PCLMUL  -- ^ Support for CLMUL instructions, with flag @support_pclmuldq@
    | RDRAND  -- ^ Support for RDRAND instruction, with flag @support_rdrand@
    deriving (Show,Eq,Enum,Data)

-- | Options which have been enabled at compile time and are supported by the
-- current CPU.
processorOptions :: [ProcessorOption]
processorOptions = unsafeDoIO $ do
    p <- cryptonite_aes_cpu_init
    options <- traverse (getOption p) aesOptions
    rdrand  <- hasRDRand
    return (decodeOptions options ++ [ RDRAND | rdrand ])
  where
    aesOptions    = [ AESNI .. PCLMUL ]
    getOption p   = peekElemOff p . fromEnum
    decodeOptions = map toEnum . findIndices (> 0)
{-# NOINLINE processorOptions #-}

hasRDRand :: IO Bool
#ifdef SUPPORT_RDRAND
hasRDRand = fmap isJust getRDRand
  where getRDRand = entropyOpen :: IO (Maybe RDRand)
#else
hasRDRand = return False
#endif

foreign import ccall unsafe "cryptonite_aes_cpu_init"
    cryptonite_aes_cpu_init :: IO (Ptr Word8)
