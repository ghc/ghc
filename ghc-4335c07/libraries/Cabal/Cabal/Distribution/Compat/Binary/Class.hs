{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DefaultSignatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Distribution.Compat.Binary.Class
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires the FFI and some flexible instances
--
-- Typeclass and instances for binary serialization.
--
-----------------------------------------------------------------------------

module Distribution.Compat.Binary.Class (

    -- * The Binary class
      Binary(..)

    -- * Support for generics
    , GBinary(..)

    ) where

import Data.Word

import Data.Binary.Put
import Data.Binary.Get

import Control.Applicative ((<$>), (<*>), (*>))
import Foreign

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L

import Data.Char     (chr,ord)
import Data.List     (unfoldr)
import Data.Foldable (traverse_)

-- And needed for the instances:
import qualified Data.ByteString as B
import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.IntMap     as IntMap
import qualified Data.IntSet     as IntSet
import qualified Data.Ratio      as R

import qualified Data.Tree as T

import Data.Array.Unboxed

import GHC.Generics

import qualified Data.Sequence as Seq
import qualified Data.Foldable as Fold

------------------------------------------------------------------------

class GBinary f where
    gput :: f t -> Put
    gget :: Get (f t)

-- | The 'Binary' class provides 'put' and 'get', methods to encode and
-- decode a Haskell value to a lazy 'ByteString'. It mirrors the 'Read' and
-- 'Show' classes for textual representation of Haskell types, and is
-- suitable for serialising Haskell values to disk, over the network.
--
-- For decoding and generating simple external binary formats (e.g. C
-- structures), Binary may be used, but in general is not suitable
-- for complex protocols. Instead use the 'Put' and 'Get' primitives
-- directly.
--
-- Instances of Binary should satisfy the following property:
--
-- > decode . encode == id
--
-- That is, the 'get' and 'put' methods should be the inverse of each
-- other. A range of instances are provided for basic Haskell types.
--
class Binary t where
    -- | Encode a value in the Put monad.
    put :: t -> Put
    -- | Decode a value in the Get monad
    get :: Get t

    default put :: (Generic t, GBinary (Rep t)) => t -> Put
    put = gput . from

    default get :: (Generic t, GBinary (Rep t)) => Get t
    get = to `fmap` gget

------------------------------------------------------------------------
-- Simple instances

-- The () type need never be written to disk: values of singleton type
-- can be reconstructed from the type alone
instance Binary () where
    put ()  = return ()
    get     = return ()

-- Bools are encoded as a byte in the range 0 .. 1
instance Binary Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = fmap (toEnum . fromIntegral) getWord8

-- Values of type 'Ordering' are encoded as a byte in the range 0 .. 2
instance Binary Ordering where
    put     = putWord8 . fromIntegral . fromEnum
    get     = fmap (toEnum . fromIntegral) getWord8

------------------------------------------------------------------------
-- Words and Ints

-- Words8s are written as bytes
instance Binary Word8 where
    put     = putWord8
    get     = getWord8

-- Words16s are written as 2 bytes in big-endian (network) order
instance Binary Word16 where
    put     = putWord16be
    get     = getWord16be

-- Words32s are written as 4 bytes in big-endian (network) order
instance Binary Word32 where
    put     = putWord32be
    get     = getWord32be

-- Words64s are written as 8 bytes in big-endian (network) order
instance Binary Word64 where
    put     = putWord64be
    get     = getWord64be

-- Int8s are written as a single byte.
instance Binary Int8 where
    put i   = put (fromIntegral i :: Word8)
    get     = fmap fromIntegral (get :: Get Word8)

-- Int16s are written as a 2 bytes in big endian format
instance Binary Int16 where
    put i   = put (fromIntegral i :: Word16)
    get     = fmap fromIntegral (get :: Get Word16)

-- Int32s are written as a 4 bytes in big endian format
instance Binary Int32 where
    put i   = put (fromIntegral i :: Word32)
    get     = fmap fromIntegral (get :: Get Word32)

-- Int64s are written as a 4 bytes in big endian format
instance Binary Int64 where
    put i   = put (fromIntegral i :: Word64)
    get     = fmap fromIntegral (get :: Get Word64)

------------------------------------------------------------------------

-- Words are are written as Word64s, that is, 8 bytes in big endian format
instance Binary Word where
    put i   = put (fromIntegral i :: Word64)
    get     = fmap fromIntegral (get :: Get Word64)

-- Ints are are written as Int64s, that is, 8 bytes in big endian format
instance Binary Int where
    put i   = put (fromIntegral i :: Int64)
    get     = fmap fromIntegral (get :: Get Int64)

------------------------------------------------------------------------
--
-- Portable, and pretty efficient, serialisation of Integer
--

-- Fixed-size type for a subset of Integer
type SmallInt = Int32

-- Integers are encoded in two ways: if they fit inside a SmallInt,
-- they're written as a byte tag, and that value.  If the Integer value
-- is too large to fit in a SmallInt, it is written as a byte array,
-- along with a sign and length field.

instance Binary Integer where

    {-# INLINE put #-}
    put n | n >= lo && n <= hi = do
        putWord8 0
        put (fromIntegral n :: SmallInt)  -- fast path
     where
        lo = fromIntegral (minBound :: SmallInt) :: Integer
        hi = fromIntegral (maxBound :: SmallInt) :: Integer

    put n = do
        putWord8 1
        put sign
        put (unroll (abs n))         -- unroll the bytes
     where
        sign = fromIntegral (signum n) :: Word8

    {-# INLINE get #-}
    get = do
        tag <- get :: Get Word8
        case tag of
            0 -> fmap fromIntegral (get :: Get SmallInt)
            _ -> do sign  <- get
                    bytes <- get
                    let v = roll bytes
                    return $! if sign == (1 :: Word8) then v else - v

--
-- Fold and unfold an Integer to and from a list of its bytes
--
unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldr unstep 0
  where
    unstep b a = a `shiftL` 8 .|. fromIntegral b

{-

--
-- An efficient, raw serialisation for Integer (GHC only)
--

-- TODO  This instance is not architecture portable.  GMP stores numbers as
-- arrays of machine sized words, so the byte format is not portable across
-- architectures with different endianness and word size.

import Data.ByteString.Base (toForeignPtr,unsafePackAddress, memcpy)
import GHC.Base     hiding (ord, chr)
import GHC.Prim
import GHC.Ptr (Ptr(..))
import GHC.IOBase (IO(..))

instance Binary Integer where
    put (S# i)    = putWord8 0 *> put (I# i)
    put (J# s ba) = do
        putWord8 1
        put (I# s)
        put (BA ba)

    get = do
        b <- getWord8
        case b of
            0 -> do (I# i#) <- get
                    return (S# i#)
            _ -> do (I# s#) <- get
                    (BA a#) <- get
                    return (J# s# a#)

instance Binary ByteArray where

    -- Pretty safe.
    put (BA ba) =
        let sz   = sizeofByteArray# ba   -- (primitive) in *bytes*
            addr = byteArrayContents# ba
            bs   = unsafePackAddress (I# sz) addr
        in put bs   -- write as a ByteString. easy, yay!

    -- Pretty scary. Should be quick though
    get = do
        (fp, off, n@(I# sz)) <- fmap toForeignPtr get      -- so decode a ByteString
        assert (off == 0) $ return $ unsafePerformIO $ do
            (MBA arr) <- newByteArray sz                    -- and copy it into a ByteArray#
            let to = byteArrayContents# (unsafeCoerce# arr) -- urk, is this safe?
            withForeignPtr fp $ \from -> memcpy (Ptr to) from (fromIntegral n)
            freezeByteArray arr

-- wrapper for ByteArray#
data ByteArray = BA  {-# UNPACK #-} !ByteArray#
data MBA       = MBA {-# UNPACK #-} !(MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newPinnedByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

-}

instance (Binary a,Integral a) => Binary (R.Ratio a) where
    put r = put (R.numerator r) *> put (R.denominator r)
    get = (R.%) <$> get <*> get

------------------------------------------------------------------------

-- Char is serialised as UTF-8
instance Binary Char where
    put a | c <= 0x7f     = put (fromIntegral c :: Word8)
          | c <= 0x7ff    = do put (0xc0 .|. y)
                               put (0x80 .|. z)
          | c <= 0xffff   = do put (0xe0 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | c <= 0x10ffff = do put (0xf0 .|. w)
                               put (0x80 .|. x)
                               put (0x80 .|. y)
                               put (0x80 .|. z)
          | otherwise     = error "Not a valid Unicode code point"
     where
        c = ord a
        z, y, x, w :: Word8
        z = fromIntegral (c           .&. 0x3f)
        y = fromIntegral (shiftR c 6  .&. 0x3f)
        x = fromIntegral (shiftR c 12 .&. 0x3f)
        w = fromIntegral (shiftR c 18 .&. 0x7)

    get = do
        let getByte = fmap (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- fmap (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- fmap (xor 0x80) getByte
                                    y <- fmap (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- fmap (xor 0x80) getByte
                                y <- fmap (xor 0x80) getByte
                                z <- fmap (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        return $! chr r

------------------------------------------------------------------------
-- Instances for the first few tuples

instance (Binary a, Binary b) => Binary (a,b) where
    put (a,b)           = put a *> put b
    get                 = (,) <$> get <*> get

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put (a,b,c)         = put a *> put b *> put c
    get                 = (,,) <$> get <*> get <*> get

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put (a,b,c,d)       = put a *> put b *> put c *> put d
    get                 = (,,,) <$> get <*> get <*> get <*> get

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put (a,b,c,d,e)     = put a *> put b *> put c *> put d *> put e
    get                 = (,,,,) <$> get <*> get <*> get <*> get <*> get

--
-- and now just recurse:
--

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f)
        => Binary (a,b,c,d,e,f) where
    put (a,b,c,d,e,f)   = put (a,(b,c,d,e,f))
    get                 = do (a,(b,c,d,e,f)) <- get ; return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g)
        => Binary (a,b,c,d,e,f,g) where
    put (a,b,c,d,e,f,g) = put (a,(b,c,d,e,f,g))
    get                 = do (a,(b,c,d,e,f,g)) <- get ; return (a,b,c,d,e,f,g)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h)
        => Binary (a,b,c,d,e,f,g,h) where
    put (a,b,c,d,e,f,g,h) = put (a,(b,c,d,e,f,g,h))
    get                   = do (a,(b,c,d,e,f,g,h)) <- get ; return (a,b,c,d,e,f,g,h)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i)
        => Binary (a,b,c,d,e,f,g,h,i) where
    put (a,b,c,d,e,f,g,h,i) = put (a,(b,c,d,e,f,g,h,i))
    get                     = do (a,(b,c,d,e,f,g,h,i)) <- get ; return (a,b,c,d,e,f,g,h,i)

instance (Binary a, Binary b, Binary c, Binary d, Binary e,
          Binary f, Binary g, Binary h, Binary i, Binary j)
        => Binary (a,b,c,d,e,f,g,h,i,j) where
    put (a,b,c,d,e,f,g,h,i,j) = put (a,(b,c,d,e,f,g,h,i,j))
    get                       = do (a,(b,c,d,e,f,g,h,i,j)) <- get ; return (a,b,c,d,e,f,g,h,i,j)

------------------------------------------------------------------------
-- Container types

instance Binary a => Binary [a] where
    put l  = put (length l) *> traverse_ put l
    get    = do n <- get :: Get Int
                getMany n

-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Binary a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}

instance (Binary a) => Binary (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 *> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> fmap Just get

instance (Binary a, Binary b) => Binary (Either a b) where
    put (Left  a) = putWord8 0 *> put a
    put (Right b) = putWord8 1 *> put b
    get = do
        w <- getWord8
        case w of
            0 -> fmap Left  get
            _ -> fmap Right get

------------------------------------------------------------------------
-- ByteStrings (have specially efficient instances)

instance Binary B.ByteString where
    put bs = do put (B.length bs)
                putByteString bs
    get    = get >>= getByteString

--
-- Using old versions of fps, this is a type synonym, and non portable
--
-- Requires 'flexible instances'
--
instance Binary ByteString where
    put bs = do put (fromIntegral (L.length bs) :: Int)
                putLazyByteString bs
    get    = get >>= getLazyByteString

------------------------------------------------------------------------
-- Maps and Sets

instance (Binary a) => Binary (Set.Set a) where
    put s = put (Set.size s) *> traverse_ put (Set.toAscList s)
    get   = fmap Set.fromDistinctAscList get

instance (Binary k, Binary e) => Binary (Map.Map k e) where
    put m = put (Map.size m) *> traverse_ put (Map.toAscList m)
    get   = fmap Map.fromDistinctAscList get

instance Binary IntSet.IntSet where
    put s = put (IntSet.size s) *> traverse_ put (IntSet.toAscList s)
    get   = fmap IntSet.fromDistinctAscList get

instance (Binary e) => Binary (IntMap.IntMap e) where
    put m = put (IntMap.size m) *> traverse_ put (IntMap.toAscList m)
    get   = fmap IntMap.fromDistinctAscList get

------------------------------------------------------------------------
-- Queues and Sequences

instance (Binary e) => Binary (Seq.Seq e) where
    put s = put (Seq.length s) *> Fold.traverse_ put s
    get = do n <- get :: Get Int
             rep Seq.empty n get
      where rep xs 0 _ = return $! xs
            rep xs n g = xs `seq` n `seq` do
                           x <- g
                           rep (xs Seq.|> x) (n-1) g

------------------------------------------------------------------------
-- Floating point

instance Binary Double where
    put d = put (decodeFloat d)
    get   = encodeFloat <$> get <*> get

instance Binary Float where
    put f = put (decodeFloat f)
    get   = encodeFloat <$> get <*> get

------------------------------------------------------------------------
-- Trees

instance (Binary e) => Binary (T.Tree e) where
    put (T.Node r s) = put r *> put s
    get = T.Node <$> get <*> get

------------------------------------------------------------------------
-- Arrays

instance (Binary i, Ix i, Binary e) => Binary (Array i e) where
    put a = do
        put (bounds a)
        put (rangeSize $ bounds a) -- write the length
        traverse_ put (elems a)        -- now the elems.
    get = do
        bs <- get
        n  <- get                  -- read the length
        xs <- getMany n            -- now the elems.
        return (listArray bs xs)

--
-- The IArray UArray e constraint is non portable. Requires flexible instances
--
instance (Binary i, Ix i, Binary e, IArray UArray e) => Binary (UArray i e) where
    put a = do
        put (bounds a)
        put (rangeSize $ bounds a) -- now write the length
        traverse_ put (elems a)
    get = do
        bs <- get
        n  <- get
        xs <- getMany n
        return (listArray bs xs)
