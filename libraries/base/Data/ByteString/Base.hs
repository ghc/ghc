{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
-- |
-- Module      : Data.ByteString.Base
-- License     : BSD-style
-- Maintainer  : dons@cse.unsw.edu.au
-- Stability   : experimental
-- Portability : portable
-- 
-- A module containing semi-public 'ByteString' internals. This exposes
-- the 'ByteString' representation and low level construction functions.
-- Modules which extend the 'ByteString' system will need to use this module
-- while ideally most users will be able to make do with the public interface
-- modules.
--
module Data.ByteString.Base (

        -- * The @ByteString@ type and representation
        ByteString(..),         -- instances: Eq, Ord, Show, Read, Data, Typeable
        LazyByteString(..),     -- instances: Eq, Ord, Show, Read, Data, Typeable      

        -- * Unchecked access
        unsafeHead,             -- :: ByteString -> Word8
        unsafeTail,             -- :: ByteString -> ByteString
        unsafeIndex,            -- :: ByteString -> Int -> Word8
        unsafeTake,             -- :: Int -> ByteString -> ByteString
        unsafeDrop,             -- :: Int -> ByteString -> ByteString

        -- * Low level introduction and elimination
        empty,                  -- :: ByteString
        create,                 -- :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
        createAndTrim,          -- :: Int -> (Ptr Word8 -> IO Int) -> IO  ByteString
        createAndTrim',         -- :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
        mallocByteString,       -- :: Int -> IO (ForeignPtr a)

        unsafeCreate,           -- :: Int -> (Ptr Word8 -> IO ()) ->  ByteString
        unsafeUseAsCString,     -- :: ByteString -> (CString -> IO a) -> IO a
        unsafeUseAsCStringLen,  -- :: ByteString -> (CStringLen -> IO a) -> IO a

        fromForeignPtr,         -- :: ForeignPtr Word8 -> Int -> ByteString
        toForeignPtr,           -- :: ByteString -> (ForeignPtr Word8, Int, Int)

#if defined(__GLASGOW_HASKELL__)
        packCStringFinalizer,   -- :: Ptr Word8 -> Int -> IO () -> IO ByteString
        packAddress,            -- :: Addr# -> ByteString
        unsafePackAddress,      -- :: Int -> Addr# -> ByteString
        unsafeFinalize,         -- :: ByteString -> IO ()
#endif

        -- * Utilities
        inlinePerformIO,            -- :: IO a -> a
        nullForeignPtr,             -- :: ForeignPtr Word8

        countOccurrences,           -- :: (Storable a, Num a) => Ptr a -> Ptr Word8 -> Int -> IO ()

        -- * Standard C Functions
        c_strlen,                   -- :: CString -> IO CInt
        c_malloc,                   -- :: CInt -> IO (Ptr Word8)
        c_free,                     -- :: Ptr Word8 -> IO ()
        c_free_finalizer,           -- :: FunPtr (Ptr Word8 -> IO ())

        memchr,                     -- :: Ptr Word8 -> Word8 -> CSize -> IO Ptr Word8
        memcmp,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt
        memcpy,                     -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memmove,                    -- :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()
        memset,                     -- :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

        -- * cbits functions
        c_reverse,                  -- :: Ptr Word8 -> Ptr Word8 -> CInt -> IO ()
        c_intersperse,              -- :: Ptr Word8 -> Ptr Word8 -> CInt -> Word8 -> IO ()
        c_maximum,                  -- :: Ptr Word8 -> CInt -> IO Word8
        c_minimum,                  -- :: Ptr Word8 -> CInt -> IO Word8
        c_count,                    -- :: Ptr Word8 -> CInt -> Word8 -> IO CInt

        -- * Internal GHC magic
#if defined(__GLASGOW_HASKELL__)
        memcpy_ptr_baoff,           -- :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif

        -- * Chars
        w2c, c2w, isSpaceWord8

  ) where

import Foreign.ForeignPtr       (ForeignPtr, newForeignPtr_, withForeignPtr)
import Foreign.Ptr              (Ptr, FunPtr, plusPtr, castPtr)
import Foreign.Storable         (Storable(..))
import Foreign.C.Types          (CInt, CSize, CULong)
import Foreign.C.String         (CString, CStringLen)

import Control.Exception        (assert)

import Data.Char                (ord)
import Data.Word                (Word8)

#if defined(__GLASGOW_HASKELL__)
import qualified Foreign.ForeignPtr as FC (finalizeForeignPtr)
import qualified Foreign.Concurrent as FC (newForeignPtr)

import Data.Generics            (Data(..), Typeable(..))
import GHC.Prim                 (Addr#)
import GHC.Ptr                  (Ptr(..))
import GHC.Base                 (realWorld#,unsafeChr)
import GHC.IOBase               (IO(IO), unsafePerformIO, RawBuffer)
#else
import Data.Char                (chr)
import System.IO.Unsafe         (unsafePerformIO)
#endif

#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)
#else
import Foreign.ForeignPtr       (mallocForeignPtrBytes)
#endif

#if __GLASGOW_HASKELL__>=605
import GHC.ForeignPtr           (ForeignPtr(ForeignPtr))
import GHC.Base                 (nullAddr#)
#else
import Foreign.Ptr              (nullPtr)
#endif

-- CFILES stuff is Hugs only
{-# CFILES cbits/fpstring.c #-}

-- -----------------------------------------------------------------------------
--
-- Useful macros, until we have bang patterns
--

#define STRICT1(f) f a | a `seq` False = undefined
#define STRICT2(f) f a b | a `seq` b `seq` False = undefined
#define STRICT3(f) f a b c | a `seq` b `seq` c `seq` False = undefined
#define STRICT4(f) f a b c d | a `seq` b `seq` c `seq` d `seq` False = undefined
#define STRICT5(f) f a b c d e | a `seq` b `seq` c `seq` d `seq` e `seq` False = undefined

-- -----------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8)
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length

#if defined(__GLASGOW_HASKELL__)
    deriving (Data, Typeable)
#endif

instance Show ByteString where
    showsPrec p ps r = showsPrec p (unpackWith w2c ps) r

instance Read ByteString where
    readsPrec p str = [ (packWith c2w x, y) | (x, y) <- readsPrec p str ]

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith _ (PS _  _ 0) = []
unpackWith k (PS ps s l) = inlinePerformIO $ withForeignPtr ps $ \p ->
        go (p `plusPtr` s) (l - 1) []
    where
        STRICT3(go)
        go p 0 acc = peek p          >>= \e -> return (k e : acc)
        go p n acc = peekByteOff p n >>= \e -> go p (n-1) (k e : acc)
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}

-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = unsafeCreate (length str) $ \p -> go p str
    where
        STRICT2(go)
        go _ []     = return ()
        go p (x:xs) = poke p (k x) >> go (p `plusPtr` 1) xs -- less space than pokeElemOff
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

------------------------------------------------------------------------

-- | A space-efficient representation of a Word8 vector, supporting many
-- efficient operations.  A 'ByteString' contains 8-bit characters only.
--
-- Instances of Eq, Ord, Read, Show, Data, Typeable
--
newtype LazyByteString = LPS [ByteString] -- LPS for lazy packed string
    deriving (Show,Read
#if defined(__GLASGOW_HASKELL__)
                        ,Data, Typeable
#endif
             )

------------------------------------------------------------------------

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = PS nullForeignPtr 0 0

nullForeignPtr :: ForeignPtr Word8
#if __GLASGOW_HASKELL__>=605
nullForeignPtr = ForeignPtr nullAddr# undefined --TODO: should ForeignPtrContents be strict?
#else
nullForeignPtr = unsafePerformIO $ newForeignPtr_ nullPtr
{-# NOINLINE nullForeignPtr #-}
#endif

-- ---------------------------------------------------------------------
--
-- Extensions to the basic interface
--

-- | A variety of 'head' for non-empty ByteStrings. 'unsafeHead' omits the
-- check for the empty case, so there is an obligation on the programmer
-- to provide a proof that the ByteString is non-empty.
unsafeHead :: ByteString -> Word8
unsafeHead (PS x s l) = assert (l > 0) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p s
{-# INLINE unsafeHead #-}

-- | A variety of 'tail' for non-empty ByteStrings. 'unsafeTail' omits the
-- check for the empty case. As with 'unsafeHead', the programmer must
-- provide a separate proof that the ByteString is non-empty.
unsafeTail :: ByteString -> ByteString
unsafeTail (PS ps s l) = assert (l > 0) $ PS ps (s+1) (l-1)
{-# INLINE unsafeTail #-}

-- | Unsafe 'ByteString' index (subscript) operator, starting from 0, returning a 'Word8'
-- This omits the bounds check, which means there is an accompanying
-- obligation on the programmer to ensure the bounds are checked in some
-- other way.
unsafeIndex :: ByteString -> Int -> Word8
unsafeIndex (PS x s l) i = assert (i >= 0 && i < l) $
    inlinePerformIO $ withForeignPtr x $ \p -> peekByteOff p (s+i)
{-# INLINE unsafeIndex #-}

-- | A variety of 'take' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeTake :: Int -> ByteString -> ByteString
unsafeTake n (PS x s l) = assert (0 <= n && n <= l) $ PS x s n
{-# INLINE unsafeTake #-}

-- | A variety of 'drop' which omits the checks on @n@ so there is an
-- obligation on the programmer to provide a proof that @0 <= n <= 'length' xs@.
unsafeDrop  :: Int -> ByteString -> ByteString
unsafeDrop n (PS x s l) = assert (0 <= n && n <= l) $ PS x (s+n) (l-n)
{-# INLINE unsafeDrop #-}

-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(1)/ Build a ByteString from a ForeignPtr
fromForeignPtr :: ForeignPtr Word8 -> Int -> ByteString
fromForeignPtr fp l = PS fp 0 l

-- | /O(1)/ Deconstruct a ForeignPtr from a ByteString
toForeignPtr :: ByteString -> (ForeignPtr Word8, Int, Int)
toForeignPtr (PS ps s l) = (ps, s, l)

-- | A way of creating ByteStrings outside the IO monad. The @Int@
-- argument gives the final size of the ByteString. Unlike
-- 'createAndTrim' the ByteString is not reallocated if the final size
-- is less than the estimated size.
unsafeCreate :: Int -> (Ptr Word8 -> IO ()) -> ByteString
unsafeCreate l f = unsafePerformIO (create l f)
{-# INLINE unsafeCreate #-}

-- | Create ByteString of size @l@ and use action @f@ to fill it's contents.
create :: Int -> (Ptr Word8 -> IO ()) -> IO ByteString
create l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> f p
    return $! PS fp 0 l

-- | Given the maximum size needed and a function to make the contents
-- of a ByteString, createAndTrim makes the 'ByteString'. The generating
-- function is required to return the actual final size (<= the maximum
-- size), and the resulting byte array is realloced to this size.
--
-- createAndTrim is the main mechanism for creating custom, efficient
-- ByteString functions, using Haskell or C functions to fill the space.
--
createAndTrim :: Int -> (Ptr Word8 -> IO Int) -> IO ByteString
createAndTrim l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        l' <- f p
        if assert (l' <= l) $ l' >= l
            then return $! PS fp 0 l
            else create l' $ \p' -> memcpy p' p (fromIntegral l')

createAndTrim' :: Int -> (Ptr Word8 -> IO (Int, Int, a)) -> IO (ByteString, a)
createAndTrim' l f = do
    fp <- mallocByteString l
    withForeignPtr fp $ \p -> do
        (off, l', res) <- f p
        if assert (l' <= l) $ l' >= l
            then return $! (PS fp 0 l, res)
            else do ps <- create l' $ \p' ->
                            memcpy p' (p `plusPtr` off) (fromIntegral l')
                    return $! (ps, res)

-- | Wrapper of mallocForeignPtrBytes with faster implementation
-- for GHC 6.5 builds newer than 06/06/06
mallocByteString :: Int -> IO (ForeignPtr a)
mallocByteString l = do
#if __GLASGOW_HASKELL__ >= 605 && !defined(SLOW_FOREIGN_PTR)
    mallocPlainForeignPtrBytes l
#else
    mallocForeignPtrBytes l
#endif

#if defined(__GLASGOW_HASKELL__)
-- | /O(n)/ Pack a null-terminated sequence of bytes, pointed to by an
-- Addr\# (an arbitrary machine address assumed to point outside the
-- garbage-collected heap) into a @ByteString@. A much faster way to
-- create an Addr\# is with an unboxed string literal, than to pack a
-- boxed string. A unboxed string literal is compiled to a static @char
-- []@ by GHC. Establishing the length of the string requires a call to
-- @strlen(3)@, so the Addr# must point to a null-terminated buffer (as
-- is the case with "string"# literals in GHC). Use 'unsafePackAddress'
-- if you know the length of the string statically.
--
-- An example:
--
-- > literalFS = packAddress "literal"#
--
packAddress :: Addr# -> ByteString
packAddress addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    l <- c_strlen cstr
    return $ PS p 0 (fromIntegral l)
  where
    cstr = Ptr addr#
{-# INLINE packAddress #-}

-- | /O(1)/ 'unsafePackAddress' provides constant-time construction of
-- 'ByteStrings' -- which is ideal for string literals. It packs a
-- null-terminated sequence of bytes into a 'ByteString', given a raw
-- 'Addr\#' to the string, and the length of the string. Make sure the
-- length is correct, otherwise use the safer 'packAddress' (where the
-- length will be calculated once at runtime).
unsafePackAddress :: Int -> Addr# -> ByteString
unsafePackAddress len addr# = inlinePerformIO $ do
    p <- newForeignPtr_ cstr
    return $ PS p 0 len
    where cstr = Ptr addr#

-- | /O(1)/ Construct a 'ByteString' given a C Ptr Word8 buffer, a
-- length, and an IO action representing a finalizer. This function is
-- not available on Hugs.
--
packCStringFinalizer :: Ptr Word8 -> Int -> IO () -> IO ByteString
packCStringFinalizer p l f = do
    fp <- FC.newForeignPtr p f
    return $ PS fp 0 l

-- | Explicitly run the finaliser associated with a 'ByteString'.
-- Further references to this value may generate invalid memory
-- references. This operation is unsafe, as there may be other
-- 'ByteStrings' referring to the same underlying pages. If you use
-- this, you need to have a proof of some kind that all 'ByteString's
-- ever generated from the underlying byte array are no longer live.
unsafeFinalize :: ByteString -> IO ()
unsafeFinalize (PS p _ _) = FC.finalizeForeignPtr p

#endif

------------------------------------------------------------------------

-- | Conversion between 'Word8' and 'Char'. Should compile to a no-op.
w2c :: Word8 -> Char
#if !defined(__GLASGOW_HASKELL__)
w2c = chr . fromIntegral
#else
w2c = unsafeChr . fromIntegral
#endif
{-# INLINE w2c #-}

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
c2w :: Char -> Word8
c2w = fromIntegral . ord
{-# INLINE c2w #-}

-- Selects white-space characters in the Latin-1 range
-- ordered by frequency
-- Idea from Ketil
isSpaceWord8 :: Word8 -> Bool
isSpaceWord8 w = case w of
    0x20 -> True -- SPACE
    0x0A -> True -- LF, \n
    0x09 -> True -- HT, \t
    0x0C -> True -- FF, \f
    0x0D -> True -- CR, \r
    0x0B -> True -- VT, \v
    0xA0 -> True -- spotted by QC..
    _    -> False
{-# INLINE isSpaceWord8 #-}

------------------------------------------------------------------------
-- | Just like unsafePerformIO, but we inline it. Big performance gains as
-- it exposes lots of things to further inlining
--
{-# INLINE inlinePerformIO #-}
inlinePerformIO :: IO a -> a
#if defined(__GLASGOW_HASKELL__)
inlinePerformIO (IO m) = case m realWorld# of (# _, r #) -> r
#else
inlinePerformIO = unsafePerformIO
#endif

-- | Count the number of occurrences of each byte.
--
{-# SPECIALIZE countOccurrences :: Ptr CSize -> Ptr Word8 -> Int -> IO () #-}
countOccurrences :: (Storable a, Num a) => Ptr a -> Ptr Word8 -> Int -> IO ()
STRICT3(countOccurrences)
countOccurrences counts str l = go 0
 where
    STRICT1(go)
    go i | i == l    = return ()
         | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                          x <- peekElemOff counts k
                          pokeElemOff counts k (x + 1)
                          go (i + 1)

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a
-- @CString@.  Warning: modifying the @CString@ will affect the
-- @ByteString@.  Why is this function unsafe? It relies on the null
-- byte at the end of the ByteString to be there. Unless you can
-- guarantee the null byte, you should use the safe version, which will
-- copy the string first.
unsafeUseAsCString :: ByteString -> (CString -> IO a) -> IO a
unsafeUseAsCString (PS ps s _) ac = withForeignPtr ps $ \p -> ac (castPtr p `plusPtr` s)

-- | /O(1) construction/ Use a @ByteString@ with a function requiring a
-- @CStringLen@.
unsafeUseAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
unsafeUseAsCStringLen (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s,l)

-- ---------------------------------------------------------------------
-- 
-- Standard C functions
--

foreign import ccall unsafe "string.h strlen" c_strlen
    :: CString -> IO CSize

foreign import ccall unsafe "stdlib.h malloc" c_malloc
    :: CSize -> IO (Ptr Word8)

foreign import ccall unsafe "static stdlib.h free" c_free
    :: Ptr Word8 -> IO ()

foreign import ccall unsafe "static stdlib.h &free" c_free_finalizer
    :: FunPtr (Ptr Word8 -> IO ())

foreign import ccall unsafe "string.h memchr" memchr
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)

foreign import ccall unsafe "string.h memcmp" memcmp
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO CInt

foreign import ccall unsafe "string.h memcpy" memcpy
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "string.h memmove" memmove
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

foreign import ccall unsafe "string.h memset" memset
    :: Ptr Word8 -> Word8 -> CSize -> IO (Ptr Word8)


-- ---------------------------------------------------------------------
--
-- Uses our C code
--

foreign import ccall unsafe "static fpstring.h fps_reverse" c_reverse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> IO ()

foreign import ccall unsafe "static fpstring.h fps_intersperse" c_intersperse
    :: Ptr Word8 -> Ptr Word8 -> CULong -> Word8 -> IO ()

foreign import ccall unsafe "static fpstring.h fps_maximum" c_maximum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_minimum" c_minimum
    :: Ptr Word8 -> CULong -> IO Word8

foreign import ccall unsafe "static fpstring.h fps_count" c_count
    :: Ptr Word8 -> CULong -> Word8 -> IO CULong

-- ---------------------------------------------------------------------
-- MMap

{-
foreign import ccall unsafe "static fpstring.h my_mmap" my_mmap
    :: Int -> Int -> IO (Ptr Word8)

foreign import ccall unsafe "static unistd.h close" c_close
    :: Int -> IO Int

#  if !defined(__OpenBSD__)
foreign import ccall unsafe "static sys/mman.h munmap" c_munmap
    :: Ptr Word8 -> Int -> IO Int
#  endif
-}

-- ---------------------------------------------------------------------
-- Internal GHC Haskell magic

#if defined(__GLASGOW_HASKELL__)
foreign import ccall unsafe "__hscore_memcpy_src_off"
   memcpy_ptr_baoff :: Ptr a -> RawBuffer -> CInt -> CSize -> IO (Ptr ())
#endif
