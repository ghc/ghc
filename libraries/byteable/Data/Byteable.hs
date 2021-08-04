-- |
-- Module      : Data.Byteable
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : good
--
module Data.Byteable
    ( Byteable(..)
    , constEqBytes
    ) where

import Foreign.Ptr (Ptr, plusPtr)
import Foreign.ForeignPtr (withForeignPtr)
import Data.ByteString (ByteString)
import Data.List (foldl')
import Data.Word (Word8)
import qualified Data.ByteString as B (length, zipWith)
import qualified Data.ByteString.Internal as B (toForeignPtr)

-- | Class of things that can generate sequence of bytes
class Byteable a where
    -- | Convert a byteable type to a bytestring
    toBytes        :: a -> ByteString

    -- | Return the size of the byteable .
    byteableLength :: a -> Int
    byteableLength = B.length . toBytes

    -- | Provide a way to look at the data of a byteable type with a ptr.
    withBytePtr :: a -> (Ptr Word8 -> IO b) -> IO b
    withBytePtr a f = withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
      where (fptr, off, _) = B.toForeignPtr $ toBytes a

instance Byteable ByteString where
    toBytes bs = bs

-- | A constant time equality test for 2 byteable objects.
--
-- If objects are of 2 different sizes, the function will abort early
-- without comparing any bytes.
--
-- compared to == , this function will go over all the bytes
-- present before yielding a result even when knowing the
-- overall result early in the processing.
constEqBytes :: Byteable a => a -> a -> Bool
constEqBytes a b = constEqByteString (toBytes a) (toBytes b)
{-# RULES "constEqBytes/ByteString" constEqBytes = constEqByteString #-}

{-# INLINE constEqByteString #-}
constEqByteString :: ByteString -> ByteString -> Bool
constEqByteString a b
    | len /= B.length b = False
    | otherwise         = foldl' (&&!) True $ B.zipWith (==) a b
  where len = B.length a

        (&&!) :: Bool -> Bool -> Bool
        True  &&! True  = True
        True  &&! False = False
        False &&! True  = False
        False &&! False = False
