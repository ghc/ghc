

-- | Computing fingerprints of values serializeable with GHC's \"Binary\" module.
module GHC.Iface.Recomp.Binary
  ( -- * Computing fingerprints
    fingerprintBinMem
  , computeFingerprint
  , putNameLiterally
  ) where

import GHC.Prelude

import GHC.Utils.Fingerprint
import GHC.Utils.Binary
import GHC.Types.Name
import GHC.Utils.Panic.Plain

fingerprintBinMem :: BinHandle -> IO Fingerprint
fingerprintBinMem bh = withBinBuffer bh f
  where
    f bs =
        -- we need to take care that we force the result here
        -- lest a reference to the ByteString may leak out of
        -- withBinBuffer.
        let fp = fingerprintByteString bs
        in fp `seq` return fp

computeFingerprint :: (Binary a)
                   => (BinHandle -> Name -> IO ())
                   -> a
                   -> IO Fingerprint
computeFingerprint put_nonbinding_name a = do
    bh <- fmap set_user_data $ openBinMem (3*1024) -- just less than a block
    put_ bh a
    fingerprintBinMem bh
  where
    set_user_data bh =
      setUserData bh $ newWriteState put_nonbinding_name putNameLiterally putFS

-- | Used when we want to fingerprint a structure without depending on the
-- fingerprints of external Names that it refers to.
putNameLiterally :: BinHandle -> Name -> IO ()
putNameLiterally bh name = assert (isExternalName name) $ do
    put_ bh $! nameModule name
    put_ bh $! nameOccName name
