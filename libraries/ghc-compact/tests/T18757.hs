{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import Control.Monad
import GHC.Compact
import GHC.Compact.Serialized
import GHC.IO
import GHC.Prim

-- | Test case for #18757, ensuring that the compact region allocator doesn't produce blocks
-- smaller than the chosen default block size.
main :: IO ()
main = do
  let
    -- Valid for the x86_64 target of GHC
    blocksPerMBlock, blockSize, dataBytesInMegablock :: Integral a => a
    blocksPerMBlock = 252
    blockSize = 4096
    dataBytesInMegablock = blocksPerMBlock * blockSize

  region <- compactSized dataBytesInMegablock False ()
  largeObject <- newLargeObject

  -- Add the large object a few times to our compact region:
  replicateM 510 $ void $ compactAdd region largeObject

  -- Now check how many blocks were allocated,
  -- and how much data they each contain
  blockSizes <- withSerializedCompact region $ \serialized ->
    pure $ map snd $ serializedCompactBlockList serialized

  -- This should print a list with only two entries, as the allocated objects
  -- should all fit within one megablock.
  print blockSizes

-- | Create an object larger than the large object threshold
-- (valid for the x86_64 target of GHC)
newLargeObject :: IO LargeObject
newLargeObject = IO $ \s ->
  case newByteArray# 4000# s of
    (# s', arr #) -> case unsafeFreezeByteArray# arr s of
      (# s'', frozenArr #) -> (# s'', LargeObject frozenArr #)

data LargeObject = LargeObject ByteArray#
