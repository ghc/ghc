import Data.Bifunctor
import Foreign.Ptr
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified GHC.Compact as Compact
import qualified GHC.Compact.Serialized as CompactSerialize

-- | Minimal test case for reproducing compactFixupPointers# bug for large compact regions.
-- See Issue #16992.
main :: IO ()
main = do
  let
    large = 1024 * 1024 * 128
    largeString = replicate large 'A'

  region <- Compact.compact largeString

  Just deserialized <- CompactSerialize.withSerializedCompact region $ \s -> do
    blks <- mapM (BS.unsafePackCStringLen . bimap castPtr fromIntegral) (CompactSerialize.serializedCompactBlockList s)
    CompactSerialize.importCompactByteStrings s blks

  print (Compact.getCompact deserialized == largeString)
