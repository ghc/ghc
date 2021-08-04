-- |
-- Module      : Crypto.Cipher.Types.Base
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : Excellent
--
-- Symmetric cipher basic types
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Crypto.Cipher.Types.Base
    ( KeySizeSpecifier(..)
    , Cipher(..)
    , AuthTag(..)
    , AEADMode(..)
    , CCM_M(..)
    , CCM_L(..)
    , DataUnitOffset
    ) where

import           Data.Word
import           Crypto.Internal.ByteArray (Bytes, ByteArrayAccess, ByteArray)
import qualified Crypto.Internal.ByteArray as B
import           Crypto.Internal.DeepSeq
import           Crypto.Error

-- | Different specifier for key size in bytes
data KeySizeSpecifier =
      KeySizeRange Int Int -- ^ in the range [min,max]
    | KeySizeEnum  [Int]   -- ^ one of the specified values
    | KeySizeFixed Int     -- ^ a specific size
    deriving (Show,Eq)

-- | Offset inside an XTS data unit, measured in block size.
type DataUnitOffset = Word32

-- | Authentication Tag for AE cipher mode
newtype AuthTag = AuthTag { unAuthTag :: Bytes }
    deriving (Show, ByteArrayAccess, NFData)

instance Eq AuthTag where
    (AuthTag a) == (AuthTag b) = B.constEq a b

data CCM_M = CCM_M4 | CCM_M6 | CCM_M8 | CCM_M10 | CCM_M12 | CCM_M14 | CCM_M16 deriving (Show, Eq)
data CCM_L = CCM_L2 | CCM_L3 | CCM_L4 deriving (Show, Eq)

-- | AEAD Mode
data AEADMode =
      AEAD_OCB -- OCB3
    | AEAD_CCM Int CCM_M CCM_L
    | AEAD_EAX
    | AEAD_CWC
    | AEAD_GCM
    deriving (Show,Eq)

-- | Symmetric cipher class.
class Cipher cipher where
    -- | Initialize a cipher context from a key
    cipherInit    :: ByteArray key => key -> CryptoFailable cipher
    -- | Cipher name
    cipherName    :: cipher -> String
    -- | return the size of the key required for this cipher.
    -- Some cipher accept any size for key
    cipherKeySize :: cipher -> KeySizeSpecifier
