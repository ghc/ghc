module Crypto.Cipher.Utils
    ( validateKeySize
    ) where

import Crypto.Error
import Crypto.Cipher.Types

import Data.ByteArray as BA

validateKeySize :: (ByteArrayAccess key, Cipher cipher) => cipher -> key -> CryptoFailable key
validateKeySize c k = if validKeyLength
                      then CryptoPassed k
                      else CryptoFailed CryptoError_KeySizeInvalid
  where keyLength = BA.length k
        validKeyLength = case cipherKeySize c of
          KeySizeRange low high -> keyLength >= low && keyLength <= high
          KeySizeEnum lengths -> keyLength `elem` lengths
          KeySizeFixed s -> keyLength == s