{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module KAT_TripleDES (tests) where

import Imports
import BlockCipher
import qualified Crypto.Cipher.TripleDES as TripleDES

kats = defaultKATs

tests = localOption (QuickCheckTests 5)
      $ testBlockCipher kats (undefined :: TripleDES.DES_EEE3)
