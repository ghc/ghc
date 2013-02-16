module Main (main) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (inlinePerformIO)
import qualified Data.ByteString.Internal as S
import Data.Monoid
import Foreign
import System.IO.Unsafe

newtype Builder = Builder {
        runBuilder :: (Buffer -> [S.ByteString]) -> Buffer -> [S.ByteString]
    }

instance Monoid Builder where
    mempty  = empty
    {-# INLINE mempty #-}
    mappend = append
    {-# INLINE mappend #-}
    mconcat = foldr mappend mempty
    {-# INLINE mconcat #-}

empty :: Builder
empty = Builder (\ k b -> b `seq` k b)
{-# INLINE empty #-}

singleton :: Word8 -> Builder
singleton = writeN 1 . flip poke
{-# INLINE singleton #-}

append :: Builder -> Builder -> Builder
append (Builder f) (Builder g) = Builder (f . g)
{-# INLINE [0] append #-}

-- Our internal buffer type
data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- used bytes
                     {-# UNPACK #-} !Int                -- length left

-- | /O(1)./ Pop the 'S.ByteString' we have constructed so far, if any,
-- yielding a new chunk in the result lazy 'L.ByteString'.
flush :: Builder
flush = Builder $ \ k buf@(Buffer p o u l) ->
    if u == 0
      then k buf
      else S.PS p o u : k (Buffer p (o+u) 0 l)

-- | /O(n)./ Extract a lazy 'L.ByteString' from a 'Builder'.
-- The construction work takes place if and when the relevant part of
-- the lazy 'L.ByteString' is demanded.
--
toLazyByteString :: Builder -> L.ByteString
toLazyByteString m = L.fromChunks $ unsafePerformIO $ do
    buf <- newBuffer defaultSize
    return (runBuilder (m `append` flush) (const []) buf)
{-# INLINE toLazyByteString #-}

defaultSize :: Int
defaultSize = 32 * k - overhead
    where k = 1024
          overhead = 2 * sizeOf (undefined :: Int)

-- | Sequence an IO operation on the buffer
unsafeLiftIO :: (Buffer -> IO Buffer) -> Builder
unsafeLiftIO f =  Builder $ \ k buf -> inlinePerformIO $ do
    buf' <- f buf
    return (k buf')
{-# INLINE unsafeLiftIO #-}

-- | Get the size of the buffer
withSize :: (Int -> Builder) -> Builder
withSize f = Builder $ \ k buf@(Buffer _ _ _ l) -> runBuilder (f l) k buf

-- | Ensure that there are at least @n@ many bytes available.
ensureFree :: Int -> Builder
ensureFree n = n `seq` withSize $ \ l ->
    if n <= l then empty else
        flush `append` unsafeLiftIO (const (newBuffer (max n defaultSize)))
{-# INLINE [0] ensureFree #-}

-- | Ensure that @n@ many bytes are available, and then use @f@ to write some
-- bytes into the memory.
writeN :: Int -> (Ptr Word8 -> IO ()) -> Builder
writeN n f = ensureFree n `append` unsafeLiftIO (writeNBuffer n f)
{-# INLINE [0] writeN #-}

writeNBuffer :: Int -> (Ptr Word8 -> IO ()) -> Buffer -> IO Buffer
writeNBuffer n f (Buffer fp o u l) = do
    withForeignPtr fp (\p -> f (p `plusPtr` (o+u)))
    return (Buffer fp o (u+n) (l-n))
{-# INLINE writeNBuffer #-}

newBuffer :: Int -> IO Buffer
newBuffer size = do
    fp <- S.mallocByteString size
    return $! Buffer fp 0 0 size
{-# INLINE newBuffer #-}

-- Merge buffer bounds checks.
{-# RULES
"append/writeN" forall a b (f::Ptr Word8 -> IO ())
                           (g::Ptr Word8 -> IO ()) ws.
    append (writeN a f) (append (writeN b g) ws) =
        append (writeN (a+b) (\p -> f p >> g (p `plusPtr` a))) ws

"writeN/writeN" forall a b (f::Ptr Word8 -> IO ())
                           (g::Ptr Word8 -> IO ()).
    append (writeN a f) (writeN b g) =
        writeN (a+b) (\p -> f p >> g (p `plusPtr` a))

"ensureFree/ensureFree" forall a b .
    append (ensureFree a) (ensureFree b) = ensureFree (max a b)
 #-}

-- Test case

-- Argument must be a multiple of 4.
test :: Int -> Builder
test 0 = mempty
test n = singleton 1 `mappend`
         (singleton 2 `mappend`
          (singleton 3 `mappend`
           (singleton 4 `mappend` test (n-4))))

main = print $ L.length $ toLazyByteString $ test 10000000
