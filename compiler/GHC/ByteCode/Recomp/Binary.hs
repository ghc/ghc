module GHC.ByteCode.Recomp.Binary (
  -- * Fingerprinting ByteCode objects
  computeFingerprint,
) where

import GHC.Prelude

import GHC.ByteCode.Binary (addBinNameWriter)
import GHC.Iface.Binary
import GHC.Iface.Recomp.Binary (putNameLiterally, fingerprintBinMem)
import GHC.Types.Name
import GHC.Utils.Fingerprint
import GHC.Utils.Binary

import System.IO.Unsafe

-- | Create a 'Fingerprint' using the appropriate serializers
-- for 'ModuleByteCode'.
--
computeFingerprint :: (Binary a)
                   => (WriteBinHandle -> Name -> IO ())
                   -> a
                   -> Fingerprint
computeFingerprint put_nonbinding_name a = unsafePerformIO $ do
    bh <- fmap set_user_data $ openBinMem (3*1024) -- just less than a block
    bh' <- addBinNameWriter bh
    putWithUserData QuietBinIFace NormalCompression bh' a
    fingerprintBinMem bh'
  where
    set_user_data bh = setWriterUserData bh $ mkWriterUserData
      [ mkSomeBinaryWriter $ mkWriter put_nonbinding_name
      , mkSomeBinaryWriter $ simpleBindingNameWriter $ mkWriter putNameLiterally
      , mkSomeBinaryWriter $ mkWriter putFS
      ]
