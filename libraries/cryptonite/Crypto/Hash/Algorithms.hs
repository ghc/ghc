-- |
-- Module      : Crypto.Hash.Algorithms
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Definitions of known hash algorithms
--
module Crypto.Hash.Algorithms
    ( HashAlgorithm
    , HashAlgorithmPrefix
    -- * Hash algorithms
    , Blake2s_160(..)
    , Blake2s_224(..)
    , Blake2s_256(..)
    , Blake2sp_224(..)
    , Blake2sp_256(..)
    , Blake2b_160(..)
    , Blake2b_224(..)
    , Blake2b_256(..)
    , Blake2b_384(..)
    , Blake2b_512(..)
    , Blake2bp_512(..)
    , MD2(..)
    , MD4(..)
    , MD5(..)
    , SHA1(..)
    , SHA224(..)
    , SHA256(..)
    , SHA384(..)
    , SHA512(..)
    , SHA512t_224(..)
    , SHA512t_256(..)
    , RIPEMD160(..)
    , Tiger(..)
    , Keccak_224(..)
    , Keccak_256(..)
    , Keccak_384(..)
    , Keccak_512(..)
    , SHA3_224(..)
    , SHA3_256(..)
    , SHA3_384(..)
    , SHA3_512(..)
    , SHAKE128(..)
    , SHAKE256(..)
    , Blake2b(..), Blake2bp(..)
    , Blake2s(..), Blake2sp(..)
    , Skein256_224(..)
    , Skein256_256(..)
    , Skein512_224(..)
    , Skein512_256(..)
    , Skein512_384(..)
    , Skein512_512(..)
    , Whirlpool(..)
    ) where

import           Crypto.Hash.Types (HashAlgorithm, HashAlgorithmPrefix)
import           Crypto.Hash.Blake2s
import           Crypto.Hash.Blake2sp
import           Crypto.Hash.Blake2b
import           Crypto.Hash.Blake2bp
import           Crypto.Hash.MD2
import           Crypto.Hash.MD4
import           Crypto.Hash.MD5
import           Crypto.Hash.SHA1
import           Crypto.Hash.SHA224
import           Crypto.Hash.SHA256
import           Crypto.Hash.SHA384
import           Crypto.Hash.SHA512
import           Crypto.Hash.SHA512t
import           Crypto.Hash.SHA3
import           Crypto.Hash.Keccak
import           Crypto.Hash.RIPEMD160
import           Crypto.Hash.Tiger
import           Crypto.Hash.Skein256
import           Crypto.Hash.Skein512
import           Crypto.Hash.Whirlpool
import           Crypto.Hash.SHAKE
import           Crypto.Hash.Blake2
