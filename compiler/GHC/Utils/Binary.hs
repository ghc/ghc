
{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnboxedTuples #-}

{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
-- We always optimise this, otherwise performance of a non-optimised
-- compiler is severely affected

--
-- (c) The University of Glasgow 2002-2006
--
-- Binary I/O library, with special tweaks for GHC
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/

module GHC.Utils.Binary
  ( {-type-}  Bin, RelBin(..), getRelBin,
    {-class-} Binary(..),
    {-type-}  ReadBinHandle, WriteBinHandle,
    SymbolTable, Dictionary,

   BinData(..), dataHandle, handleData,
   unsafeUnpackBinBuffer,

   openBinMem,
--   closeBin,

   seekBinWriter,
   seekBinReader,
   seekBinReaderRel,
   tellBinReader,
   tellBinWriter,
   castBin,
   withBinBuffer,
   freezeWriteHandle,
   shrinkBinBuffer,
   thawReadHandle,

   foldGet, foldGet',

   writeBinMem,
   readBinMem,
   readBinMemN,

   putAt, getAt,
   putAtRel,
   forwardPut, forwardPut_, forwardGet,
   forwardPutRel, forwardPutRel_, forwardGetRel,

   -- * For writing instances
   putByte,
   getByte,
   putByteString,
   getByteString,

   -- * Variable length encodings
   putULEB128,
   getULEB128,
   putSLEB128,
   getSLEB128,

   -- * Fixed length encoding
   FixedLengthEncoding(..),

   -- * Lazy Binary I/O
   lazyGet,
   lazyPut,
   lazyGet',
   lazyPut',
   lazyGetMaybe,
   lazyPutMaybe,

   -- * User data
   ReaderUserData, getReaderUserData, setReaderUserData, noReaderUserData,
   WriterUserData, getWriterUserData, setWriterUserData, noWriterUserData,
   mkWriterUserData, mkReaderUserData,
   newReadState, newWriteState,
   addReaderToUserData, addWriterToUserData,
   findUserDataReader, findUserDataWriter,
   -- * Binary Readers & Writers
   BinaryReader(..), BinaryWriter(..),
   mkWriter, mkReader,
   SomeBinaryReader, SomeBinaryWriter,
   mkSomeBinaryReader, mkSomeBinaryWriter,
   -- * Tables
   ReaderTable(..),
   WriterTable(..),
   -- * String table ("dictionary")
   initFastStringReaderTable, initFastStringWriterTable,
   putDictionary, getDictionary, putFS,
   FSTable(..), getDictFastString, putDictFastString,
   -- * Generic deduplication table
   GenericSymbolTable(..),
   initGenericSymbolTable,
   getGenericSymtab, putGenericSymTab,
   getGenericSymbolTable, putGenericSymbolTable,
   -- * Newtype wrappers
   BinSpan(..), BinSrcSpan(..), BinLocated(..),
   -- * Newtypes for types that have canonically more than one valid encoding
   BindingName(..),
   simpleBindingNameWriter,
   simpleBindingNameReader,
   FullBinData(..), freezeBinHandle, thawBinHandle, putFullBinData,
   BinArray,

   -- * FingerprintWithValue
   FingerprintWithValue(..)
  ) where

import GHC.Prelude

import Language.Haskell.Syntax.Module.Name (ModuleName(..))

import {-# SOURCE #-} GHC.Types.Name (Name)
import GHC.Data.FastString
import GHC.Data.TrieMap
import GHC.Utils.Panic.Plain
import GHC.Types.Unique.FM
import GHC.Data.FastMutInt
import GHC.Utils.Fingerprint
import GHC.Types.SrcLoc
import GHC.Types.Unique
import qualified GHC.Data.Strict as Strict
import GHC.Utils.Outputable( JoinPointHood(..) )

import Control.DeepSeq
import Control.Monad            ( when, (<$!>), unless, forM_, void )
import Foreign hiding (bit, setBit, clearBit, shiftL, shiftR, void)
import Data.Array
import Data.Array.IO
import Data.Array.Unsafe
import Data.ByteString (ByteString, copy)
import Data.Coerce
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import Data.IORef
import Data.Char                ( ord, chr )
import Data.List.NonEmpty       ( NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Proxy
import Data.Set                 ( Set )
import qualified Data.Set as Set
import Data.Time
import Data.List (unfoldr)
import System.IO as IO
import System.IO.Unsafe         ( unsafeInterleaveIO )
import System.IO.Error          ( mkIOError, eofErrorType )
import Type.Reflection          ( Typeable, SomeTypeRep(..) )
import qualified Type.Reflection as Refl
import GHC.Real                 ( Ratio(..) )
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import GHC.ForeignPtr           ( unsafeWithForeignPtr )

import Unsafe.Coerce (unsafeCoerce)

type BinArray = ForeignPtr Word8


---------------------------------------------------------------
-- BinData
---------------------------------------------------------------

data BinData = BinData Int BinArray

instance NFData BinData where
  rnf (BinData sz _) = rnf sz

instance Binary BinData where
  put_ bh (BinData sz dat) = do
    put_ bh sz
    putPrim bh sz $ \dest ->
      unsafeWithForeignPtr dat $ \orig ->
        copyBytes dest orig sz
  --
  get bh = do
    sz <- get bh
    dat <- mallocForeignPtrBytes sz
    getPrim bh sz $ \orig ->
      unsafeWithForeignPtr dat $ \dest ->
        copyBytes dest orig sz
    return (BinData sz dat)

dataHandle :: BinData -> IO ReadBinHandle
dataHandle (BinData size bin) = do
  ixr <- newFastMutInt 0
  return (ReadBinMem noReaderUserData ixr size bin)

handleData :: WriteBinHandle -> IO BinData
handleData (WriteBinMem _ ixr _ binr) = BinData <$> readFastMutInt ixr <*> readIORef binr

---------------------------------------------------------------
-- FullBinData
---------------------------------------------------------------

-- | 'FullBinData' stores a slice to a 'BinArray'.
--
-- It requires less memory than 'ReadBinHandle', and can be constructed from
-- a 'ReadBinHandle' via 'freezeBinHandle' and turned back into a
-- 'ReadBinHandle' using 'thawBinHandle'.
-- Additionally, the byte array slice can be put into a 'WriteBinHandle' without extra
-- conversions via 'putFullBinData'.
data FullBinData = FullBinData
  { fbd_readerUserData :: ReaderUserData
  -- ^ 'ReaderUserData' that can be used to resume reading.
  , fbd_off_s :: {-# UNPACK #-} !Int
  -- ^ start offset
  , fbd_off_e :: {-# UNPACK #-} !Int
  -- ^ end offset
  , fbd_size :: {-# UNPACK #-} !Int
  -- ^ total buffer size
  , fbd_buffer :: {-# UNPACK #-} !BinArray
  }

-- Equality and Ord assume that two distinct buffers are different, even if they compare the same things.
instance Eq FullBinData where
  (FullBinData _ b c d e) == (FullBinData _ b1 c1 d1 e1) = b == b1 && c == c1 && d == d1 && e == e1

instance Ord FullBinData where
  compare (FullBinData _ b c d e) (FullBinData _ b1 c1 d1 e1) =
    compare b b1 `mappend` compare c c1 `mappend` compare d d1 `mappend` compare e e1

-- | Write the 'FullBinData' slice into the 'WriteBinHandle'.
putFullBinData :: WriteBinHandle -> FullBinData -> IO ()
putFullBinData bh (FullBinData _ o1 o2 _sz ba) = do
  let sz = o2 - o1
  putPrim bh sz $ \dest ->
    unsafeWithForeignPtr (ba `plusForeignPtr` o1) $ \orig ->
    copyBytes dest orig sz

-- | Freeze a 'ReadBinHandle' and a start index into a 'FullBinData'.
--
-- 'FullBinData' stores a slice starting from the 'Bin a' location to the current
-- offset of the 'ReadBinHandle'.
freezeBinHandle :: ReadBinHandle -> Bin a -> IO FullBinData
freezeBinHandle (ReadBinMem user_data ixr sz binr) (BinPtr start) = do
  ix <- readFastMutInt ixr
  pure (FullBinData user_data start ix sz binr)

-- | Turn the 'FullBinData' into a 'ReadBinHandle', setting the 'ReadBinHandle'
-- offset to the start of the 'FullBinData' and restore the 'ReaderUserData' that was
-- obtained from 'freezeBinHandle'.
thawBinHandle :: FullBinData -> IO ReadBinHandle
thawBinHandle (FullBinData user_data ix _end sz ba) = do
  ixr <- newFastMutInt ix
  return $ ReadBinMem user_data ixr sz ba

---------------------------------------------------------------
-- BinHandle
---------------------------------------------------------------

-- | A write-only handle that can be used to serialise binary data into a buffer.
--
-- The buffer is an unboxed binary array.
data WriteBinHandle
  = WriteBinMem {
     wbm_userData :: WriterUserData,
     -- ^ User data for writing binary outputs.
     -- Allows users to overwrite certain 'Binary' instances.
     -- This is helpful when a non-canonical 'Binary' instance is required,
     -- such as in the case of 'Name'.
     wbm_off_r    :: !FastMutInt,      -- ^ the current offset
     wbm_sz_r     :: !FastMutInt,      -- ^ size of the array (cached)
     wbm_arr_r    :: !(IORef BinArray) -- ^ the array (bounds: (0,size-1))
    }

-- | A read-only handle that can be used to deserialise binary data from a buffer.
--
-- The buffer is an unboxed binary array.
data ReadBinHandle
  = ReadBinMem {
     rbm_userData :: ReaderUserData,
     -- ^ User data for reading binary inputs.
     -- Allows users to overwrite certain 'Binary' instances.
     -- This is helpful when a non-canonical 'Binary' instance is required,
     -- such as in the case of 'Name'.
     rbm_off_r    :: !FastMutInt,     -- ^ the current offset
     rbm_sz_r     :: !Int,            -- ^ size of the array (cached)
     rbm_arr_r    :: !BinArray        -- ^ the array (bounds: (0,size-1))
    }

getReaderUserData :: ReadBinHandle -> ReaderUserData
getReaderUserData bh = rbm_userData bh

getWriterUserData :: WriteBinHandle -> WriterUserData
getWriterUserData bh = wbm_userData bh

setWriterUserData :: WriteBinHandle -> WriterUserData -> WriteBinHandle
setWriterUserData bh us = bh { wbm_userData = us }

setReaderUserData :: ReadBinHandle -> ReaderUserData -> ReadBinHandle
setReaderUserData bh us = bh { rbm_userData = us }

-- | Add 'SomeBinaryReader' as a known binary decoder.
-- If a 'BinaryReader' for the associated type already exists in 'ReaderUserData',
-- it is overwritten.
addReaderToUserData :: forall a. Typeable a => BinaryReader a -> ReadBinHandle -> ReadBinHandle
addReaderToUserData reader bh = bh
  { rbm_userData = (rbm_userData bh)
      { ud_reader_data =
          let
            typRep = Refl.typeRep @a
          in
            Map.insert (SomeTypeRep typRep) (SomeBinaryReader typRep reader) (ud_reader_data (rbm_userData bh))
      }
  }

-- | Add 'SomeBinaryWriter' as a known binary encoder.
-- If a 'BinaryWriter' for the associated type already exists in 'WriterUserData',
-- it is overwritten.
addWriterToUserData :: forall a . Typeable a => BinaryWriter a -> WriteBinHandle -> WriteBinHandle
addWriterToUserData writer bh = bh
  { wbm_userData = (wbm_userData bh)
      { ud_writer_data =
          let
            typRep = Refl.typeRep @a
          in
            Map.insert (SomeTypeRep typRep) (SomeBinaryWriter typRep writer) (ud_writer_data (wbm_userData bh))
      }
  }

-- | Get access to the underlying buffer.
withBinBuffer :: WriteBinHandle -> (ByteString -> IO a) -> IO a
withBinBuffer (WriteBinMem _ ix_r _ arr_r) action = do
  ix <- readFastMutInt ix_r
  arr <- readIORef arr_r
  action $ BS.fromForeignPtr arr 0 ix

unsafeUnpackBinBuffer :: ByteString -> IO ReadBinHandle
unsafeUnpackBinBuffer (BS.BS arr len) = do
  ix_r <- newFastMutInt 0
  return (ReadBinMem noReaderUserData ix_r len arr)

---------------------------------------------------------------
-- Bin
---------------------------------------------------------------

newtype Bin a = BinPtr Int
  deriving (Eq, Ord, Show, Bounded)

-- | Like a 'Bin' but is used to store relative offset pointers.
-- Relative offset pointers store a relative location, but also contain an
-- anchor that allow to obtain the absolute offset.
data RelBin a = RelBin
  { relBin_anchor :: {-# UNPACK #-} !(Bin a)
  -- ^ Absolute position from where we read 'relBin_offset'.
  , relBin_offset :: {-# UNPACK #-} !(RelBinPtr a)
  -- ^ Relative offset to 'relBin_anchor'.
  -- The absolute position of the 'RelBin' is @relBin_anchor + relBin_offset@
  }
  deriving (Eq, Ord, Show, Bounded)

-- | A 'RelBinPtr' is like a 'Bin', but contains a relative offset pointer
-- instead of an absolute offset.
newtype RelBinPtr a = RelBinPtr (Bin a)
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i) = BinPtr i

-- | Read a relative offset location and wrap it in 'RelBin'.
--
-- The resulting 'RelBin' can be translated into an absolute offset location using
-- 'makeAbsoluteBin'
getRelBin :: ReadBinHandle -> IO (RelBin a)
getRelBin bh = do
  start <- tellBinReader bh
  off <- get bh
  pure $ RelBin start off

makeAbsoluteBin ::  RelBin a -> Bin a
makeAbsoluteBin (RelBin (BinPtr !start) (RelBinPtr (BinPtr !offset))) =
  BinPtr $ start + offset

makeRelativeBin :: RelBin a -> RelBinPtr a
makeRelativeBin (RelBin _ offset) = offset

toRelBin :: Bin (RelBinPtr a) -> Bin a -> RelBin a
toRelBin (BinPtr !start) (BinPtr !goal) =
  RelBin (BinPtr start) (RelBinPtr $ BinPtr $ goal - start)

---------------------------------------------------------------
-- class Binary
---------------------------------------------------------------

-- | Do not rely on instance sizes for general types,
-- we use variable length encoding for many of them.
class Binary a where
    put_   :: WriteBinHandle -> a -> IO ()
    put    :: WriteBinHandle -> a -> IO (Bin a)
    get    :: ReadBinHandle -> IO a

    -- define one of put_, put.  Use of put_ is recommended because it
    -- is more likely that tail-calls can kick in, and we rarely need the
    -- position return value.
    put_ bh a = do _ <- put bh a; return ()
    put bh a  = do p <- tellBinWriter bh; put_ bh a; return p

putAt  :: Binary a => WriteBinHandle -> Bin a -> a -> IO ()
putAt bh p x = do seekBinWriter bh p; put_ bh x; return ()

putAtRel :: WriteBinHandle -> Bin (RelBinPtr a) -> Bin a -> IO ()
putAtRel bh from to = putAt bh from (makeRelativeBin $ toRelBin from to)

getAt  :: Binary a => ReadBinHandle -> Bin a -> IO a
getAt bh p = do seekBinReader bh p; get bh

openBinMem :: Int -> IO WriteBinHandle
openBinMem size
 | size <= 0 = error "GHC.Utils.Binary.openBinMem: size must be >= 0"
 | otherwise = do
   arr <- mallocForeignPtrBytes size
   arr_r <- newIORef arr
   ix_r <- newFastMutInt 0
   sz_r <- newFastMutInt size
   return WriteBinMem
    { wbm_userData = noWriterUserData
    , wbm_off_r = ix_r
    , wbm_sz_r = sz_r
    , wbm_arr_r = arr_r
    }

-- | Freeze the given 'WriteBinHandle' and turn it into an equivalent 'ReadBinHandle'.
--
-- The current offset of the 'WriteBinHandle' is maintained in the new 'ReadBinHandle'.
freezeWriteHandle :: WriteBinHandle -> IO ReadBinHandle
freezeWriteHandle wbm = do
  rbm_off_r <- newFastMutInt =<< readFastMutInt (wbm_off_r wbm)
  rbm_sz_r <- readFastMutInt (wbm_sz_r wbm)
  rbm_arr_r <- readIORef (wbm_arr_r wbm)
  pure $ ReadBinMem
    { rbm_userData = noReaderUserData
    , rbm_off_r = rbm_off_r
    , rbm_sz_r = rbm_sz_r
    , rbm_arr_r = rbm_arr_r
    }

-- | Copy the BinBuffer to a new BinBuffer which is exactly the right size.
-- This performs a copy of the underlying buffer.
-- The buffer may be truncated if the offset is not at the end of the written
-- output.
--
-- UserData is also discarded during the copy
-- You should just use this when translating a Put handle into a Get handle.
shrinkBinBuffer :: WriteBinHandle -> IO ReadBinHandle
shrinkBinBuffer bh = withBinBuffer bh $ \bs -> do
  unsafeUnpackBinBuffer (copy bs)

thawReadHandle :: ReadBinHandle -> IO WriteBinHandle
thawReadHandle rbm = do
  wbm_off_r <- newFastMutInt =<< readFastMutInt (rbm_off_r rbm)
  wbm_sz_r <- newFastMutInt (rbm_sz_r rbm)
  wbm_arr_r <- newIORef (rbm_arr_r rbm)
  pure $ WriteBinMem
    { wbm_userData = noWriterUserData
    , wbm_off_r = wbm_off_r
    , wbm_sz_r = wbm_sz_r
    , wbm_arr_r = wbm_arr_r
    }

tellBinWriter :: WriteBinHandle -> IO (Bin a)
tellBinWriter (WriteBinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

tellBinReader :: ReadBinHandle -> IO (Bin a)
tellBinReader (ReadBinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBinWriter :: WriteBinHandle -> Bin a -> IO ()
seekBinWriter h@(WriteBinMem _ ix_r sz_r _) (BinPtr !p) = do
  sz <- readFastMutInt sz_r
  if (p > sz)
        then do expandBin h p; writeFastMutInt ix_r p
        else writeFastMutInt ix_r p

-- | 'seekBinNoExpandWriter' moves the index pointer to the location pointed to
-- by 'Bin a'.
-- This operation may 'panic', if the pointer location is out of bounds of the
-- buffer of 'BinHandle'.
seekBinNoExpandWriter :: WriteBinHandle -> Bin a -> IO ()
seekBinNoExpandWriter (WriteBinMem _ ix_r sz_r _) (BinPtr !p) = do
  sz <- readFastMutInt sz_r
  if (p > sz)
        then panic "seekBinNoExpandWriter: seek out of range"
        else writeFastMutInt ix_r p

-- | SeekBin but without calling expandBin
seekBinReader :: ReadBinHandle -> Bin a -> IO ()
seekBinReader (ReadBinMem _ ix_r sz_r _) (BinPtr !p) = do
  if (p > sz_r)
        then panic "seekBinReader: seek out of range"
        else writeFastMutInt ix_r p

seekBinReaderRel :: ReadBinHandle -> RelBin a -> IO ()
seekBinReaderRel (ReadBinMem _ ix_r sz_r _) relBin = do
  let (BinPtr !p) = makeAbsoluteBin relBin
  if (p > sz_r)
        then panic "seekBinReaderRel: seek out of range"
        else writeFastMutInt ix_r p

writeBinMem :: WriteBinHandle -> FilePath -> IO ()
writeBinMem (WriteBinMem _ ix_r _ arr_r) fn = do
  h <- openBinaryFile fn WriteMode
  arr <- readIORef arr_r
  ix  <- readFastMutInt ix_r
  unsafeWithForeignPtr arr $ \p -> hPutBuf h p ix
  hClose h

readBinMem :: FilePath -> IO ReadBinHandle
readBinMem filename = do
  withBinaryFile filename ReadMode $ \h -> do
    filesize' <- hFileSize h
    let filesize = fromIntegral filesize'
    readBinMem_ filesize h

readBinMemN :: Int -> FilePath -> IO (Maybe ReadBinHandle)
readBinMemN size filename = do
  withBinaryFile filename ReadMode $ \h -> do
    filesize' <- hFileSize h
    let filesize = fromIntegral filesize'
    if filesize < size
      then pure Nothing
      else Just <$> readBinMem_ size h

readBinMem_ :: Int -> Handle -> IO ReadBinHandle
readBinMem_ filesize h = do
  arr <- mallocForeignPtrBytes filesize
  count <- unsafeWithForeignPtr arr $ \p -> hGetBuf h p filesize
  when (count /= filesize) $
       error ("Binary.readBinMem: only read " ++ show count ++ " bytes")
  ix_r <- newFastMutInt 0
  return ReadBinMem
    { rbm_userData = noReaderUserData
    , rbm_off_r = ix_r
    , rbm_sz_r = filesize
    , rbm_arr_r = arr
    }

-- expand the size of the array to include a specified offset
expandBin :: WriteBinHandle -> Int -> IO ()
expandBin (WriteBinMem _ _ sz_r arr_r) !off = do
   !sz <- readFastMutInt sz_r
   let !sz' = getSize sz
   arr <- readIORef arr_r
   arr' <- mallocForeignPtrBytes sz'
   withForeignPtr arr $ \old ->
     withForeignPtr arr' $ \new ->
       copyBytes new old sz
   writeFastMutInt sz_r sz'
   writeIORef arr_r arr'
   where
    getSize :: Int -> Int
    getSize !sz
      | sz > off
      = sz
      | otherwise
      = getSize (sz * 2)

foldGet
  :: Binary a
  => Word -- n elements
  -> ReadBinHandle
  -> b -- initial accumulator
  -> (Word -> a -> b -> IO b)
  -> IO b
foldGet n bh init_b f = go 0 init_b
  where
    go i b
      | i == n    = return b
      | otherwise = do
          a <- get bh
          b' <- f i a b
          go (i+1) b'

foldGet'
  :: Binary a
  => Word -- n elements
  -> ReadBinHandle
  -> b -- initial accumulator
  -> (Word -> a -> b -> IO b)
  -> IO b
{-# INLINE foldGet' #-}
foldGet' n bh init_b f = go 0 init_b
  where
    go i !b
      | i == n    = return b
      | otherwise = do
          !a  <- get bh
          b'  <- f i a b
          go (i+1) b'


-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

-- | Takes a size and action writing up to @size@ bytes.
--   After the action has run advance the index to the buffer
--   by size bytes.
putPrim :: WriteBinHandle -> Int -> (Ptr Word8 -> IO ()) -> IO ()
putPrim h@(WriteBinMem _ ix_r sz_r arr_r) size f = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  when (ix + size > sz) $
    expandBin h (ix + size)
  arr <- readIORef arr_r
  unsafeWithForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ix_r (ix + size)

-- -- | Similar to putPrim but advances the index by the actual number of
-- -- bytes written.
-- putPrimMax :: BinHandle -> Int -> (Ptr Word8 -> IO Int) -> IO ()
-- putPrimMax h@(BinMem _ ix_r sz_r arr_r) size f = do
--   ix <- readFastMutInt ix_r
--   sz <- readFastMutInt sz_r
--   when (ix + size > sz) $
--     expandBin h (ix + size)
--   arr <- readIORef arr_r
--   written <- withForeignPtr arr $ \op -> f (op `plusPtr` ix)
--   writeFastMutInt ix_r (ix + written)

getPrim :: ReadBinHandle -> Int -> (Ptr Word8 -> IO a) -> IO a
getPrim (ReadBinMem _ ix_r sz_r arr_r) size f = do
  ix <- readFastMutInt ix_r
  when (ix + size > sz_r) $
      ioError (mkIOError eofErrorType "Data.Binary.getPrim" Nothing Nothing)
  w <- unsafeWithForeignPtr arr_r $ \p -> f (p `plusPtr` ix)
    -- This is safe WRT #17760 as we we guarantee that the above line doesn't
    -- diverge
  writeFastMutInt ix_r (ix + size)
  return w

putWord8 :: WriteBinHandle -> Word8 -> IO ()
putWord8 h !w = putPrim h 1 (\op -> poke op w)

getWord8 :: ReadBinHandle -> IO Word8
getWord8 h = getPrim h 1 peek

putWord16 :: WriteBinHandle -> Word16 -> IO ()
putWord16 h w = putPrim h 2 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 8))
  pokeElemOff op 1 (fromIntegral (w .&. 0xFF))
  )

getWord16 :: ReadBinHandle -> IO Word16
getWord16 h = getPrim h 2 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  return $! w0 `shiftL` 8 .|. w1
  )

putWord32 :: WriteBinHandle -> Word32 -> IO ()
putWord32 h w = putPrim h 4 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 24))
  pokeElemOff op 1 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
  pokeElemOff op 2 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
  pokeElemOff op 3 (fromIntegral (w .&. 0xFF))
  )

getWord32 :: ReadBinHandle -> IO Word32
getWord32 h = getPrim h 4 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  w2 <- fromIntegral <$> peekElemOff op 2
  w3 <- fromIntegral <$> peekElemOff op 3

  return $! (w0 `shiftL` 24) .|.
            (w1 `shiftL` 16) .|.
            (w2 `shiftL` 8)  .|.
            w3
  )

putWord64 :: WriteBinHandle -> Word64 -> IO ()
putWord64 h w = putPrim h 8 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 56))
  pokeElemOff op 1 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
  pokeElemOff op 2 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
  pokeElemOff op 3 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
  pokeElemOff op 4 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
  pokeElemOff op 5 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
  pokeElemOff op 6 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
  pokeElemOff op 7 (fromIntegral (w .&. 0xFF))
  )

getWord64 :: ReadBinHandle -> IO Word64
getWord64 h = getPrim h 8 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  w2 <- fromIntegral <$> peekElemOff op 2
  w3 <- fromIntegral <$> peekElemOff op 3
  w4 <- fromIntegral <$> peekElemOff op 4
  w5 <- fromIntegral <$> peekElemOff op 5
  w6 <- fromIntegral <$> peekElemOff op 6
  w7 <- fromIntegral <$> peekElemOff op 7

  return $! (w0 `shiftL` 56) .|.
            (w1 `shiftL` 48) .|.
            (w2 `shiftL` 40) .|.
            (w3 `shiftL` 32) .|.
            (w4 `shiftL` 24) .|.
            (w5 `shiftL` 16) .|.
            (w6 `shiftL` 8)  .|.
            w7
  )

putByte :: WriteBinHandle -> Word8 -> IO ()
putByte bh !w = putWord8 bh w

getByte :: ReadBinHandle -> IO Word8
getByte h = getWord8 h

-- -----------------------------------------------------------------------------
-- Encode numbers in LEB128 encoding.
-- Requires one byte of space per 7 bits of data.
--
-- There are signed and unsigned variants.
-- Do NOT use the unsigned one for signed values, at worst it will
-- result in wrong results, at best it will lead to bad performance
-- when coercing negative values to an unsigned type.
--
-- We mark them as SPECIALIZE as it's extremely critical that they get specialized
-- to their specific types.
--
-- TODO: Each use of putByte performs a bounds check,
--       we should use putPrimMax here. However it's quite hard to return
--       the number of bytes written into putPrimMax without allocating an
--       Int for it, while the code below does not allocate at all.
--       So we eat the cost of the bounds check instead of increasing allocations
--       for now.

-- Unsigned numbers
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Word -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Word64 -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Word32 -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Word16 -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Int -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Int64 -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Int32 -> IO () #-}
{-# SPECIALISE putULEB128 :: WriteBinHandle -> Int16 -> IO () #-}
putULEB128 :: forall a. (Integral a, FiniteBits a) => WriteBinHandle -> a -> IO ()
putULEB128 bh w =
#if defined(DEBUG)
    (if w < 0 then panic "putULEB128: Signed number" else id) $
#endif
    go w
  where
    go :: a -> IO ()
    go w
      | w <= (127 :: a)
      = putByte bh (fromIntegral w :: Word8)
      | otherwise = do
        -- bit 7 (8th bit) indicates more to come.
        let !byte = setBit (fromIntegral w) 7 :: Word8
        putByte bh byte
        go (w `unsafeShiftR` 7)

{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Word #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Word64 #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Word32 #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Word16 #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Int #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Int64 #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Int32 #-}
{-# SPECIALISE getULEB128 :: ReadBinHandle -> IO Int16 #-}
getULEB128 :: forall a. (Integral a, FiniteBits a) => ReadBinHandle -> IO a
getULEB128 bh =
    go 0 0
  where
    go :: Int -> a -> IO a
    go shift w = do
        b <- getByte bh
        let !hasMore = testBit b 7
        let !val = w .|. ((clearBit (fromIntegral b) 7) `unsafeShiftL` shift) :: a
        if hasMore
            then do
                go (shift+7) val
            else
                return $! val

-- Signed numbers
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Word -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Word64 -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Word32 -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Word16 -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Int -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Int64 -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Int32 -> IO () #-}
{-# SPECIALISE putSLEB128 :: WriteBinHandle -> Int16 -> IO () #-}
putSLEB128 :: forall a. (Integral a, Bits a) => WriteBinHandle -> a -> IO ()
putSLEB128 bh initial = go initial
  where
    go :: a -> IO ()
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and last value can
                -- be discriminated from a negative number.
                ((val' == 0 && not signBit) ||
                -- Signed value,
                 (val' == -1 && signBit))

        let !byte' = if done then byte else setBit byte 7
        putByte bh byte'

        unless done $ go val'

{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Word #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Word64 #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Word32 #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Word16 #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Int #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Int64 #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Int32 #-}
{-# SPECIALISE getSLEB128 :: ReadBinHandle -> IO Int16 #-}
getSLEB128 :: forall a. (Show a, Integral a, FiniteBits a) => ReadBinHandle -> IO a
getSLEB128 bh = do
    (val,shift,signed) <- go 0 0
    if signed && (shift < finiteBitSize val )
        then return $! ((complement 0 `unsafeShiftL` shift) .|. val)
        else return val
    where
        go :: Int -> a -> IO (a,Int,Bool)
        go shift val = do
            byte <- getByte bh
            let !byteVal = fromIntegral (clearBit byte 7) :: a
            let !val' = val .|. (byteVal `unsafeShiftL` shift)
            let !more = testBit byte 7
            let !shift' = shift+7
            if more
                then go (shift') val'
                else do
                    let !signed = testBit byte 6
                    return (val',shift',signed)

-- -----------------------------------------------------------------------------
-- Fixed length encoding instances

-- Sometimes words are used to represent a certain bit pattern instead
-- of a number. Using FixedLengthEncoding we will write the pattern as
-- is to the interface file without the variable length encoding we usually
-- apply.

-- | Encode the argument in its full length. This is different from many default
-- binary instances which make no guarantee about the actual encoding and
-- might do things using variable length encoding.
newtype FixedLengthEncoding a
  = FixedLengthEncoding { unFixedLength :: a }
  deriving (Eq,Ord,Show)

instance Binary (FixedLengthEncoding Word8) where
  put_ h (FixedLengthEncoding x) = putByte h x
  get h = FixedLengthEncoding <$> getByte h

instance Binary (FixedLengthEncoding Word16) where
  put_ h (FixedLengthEncoding x) = putWord16 h x
  get h = FixedLengthEncoding <$> getWord16 h

instance Binary (FixedLengthEncoding Word32) where
  put_ h (FixedLengthEncoding x) = putWord32 h x
  get h = FixedLengthEncoding <$> getWord32 h

instance Binary (FixedLengthEncoding Word64) where
  put_ h (FixedLengthEncoding x) = putWord64 h x
  get h = FixedLengthEncoding <$> getWord64 h

-- -----------------------------------------------------------------------------
-- Primitive Word writes

instance Binary Word8 where
  put_ bh !w = putWord8 bh w
  get  = getWord8

instance Binary Word16 where
  put_ = putULEB128
  get  = getULEB128

instance Binary Word32 where
  put_ = putULEB128
  get  = getULEB128

instance Binary Word64 where
  put_ = putULEB128
  get = getULEB128

-- -----------------------------------------------------------------------------
-- Primitive Int writes

instance Binary Int8 where
  put_ h w = put_ h (fromIntegral w :: Word8)
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  put_ = putSLEB128
  get = getSLEB128

instance Binary Int32 where
  put_ = putSLEB128
  get = getSLEB128

instance Binary Int64 where
  put_ h w = putSLEB128 h w
  get h    = getSLEB128 h

-- -----------------------------------------------------------------------------
-- Instances for standard types

instance Binary () where
    put_ _ () = return ()
    get  _    = return ()

instance Binary Bool where
    put_ bh b = putByte bh (fromIntegral (fromEnum b))
    get  bh   = do x <- getWord8 bh; return $! (toEnum (fromIntegral x))

instance Binary Char where
    put_  bh c = put_ bh (fromIntegral (ord c) :: Word32)
    get  bh   = do x <- get bh; return $! (chr (fromIntegral (x :: Word32)))

instance Binary Int where
    put_ bh i = put_ bh (fromIntegral i :: Int64)
    get  bh = do
        x <- get bh
        return $! (fromIntegral (x :: Int64))

instance Binary a => Binary [a] where
    put_ bh l = do
        let len = length l
        put_ bh len
        mapM_ (put_ bh) l
    get bh = do
        len <- get bh :: IO Int -- Int is variable length encoded so only
                                -- one byte for small lists.
        let loop 0 = return []
            loop n = do a <- get bh; as <- loop (n-1); return (a:as)
        loop len

-- | This instance doesn't rely on the determinism of the keys' 'Ord' instance,
-- so it works e.g. for 'Name's too.
instance (Binary a, Ord a) => Binary (Set a) where
  put_ bh s = put_ bh (Set.toAscList s)
  get bh = Set.fromAscList <$> get bh

instance Binary a => Binary (NonEmpty a) where
    put_ bh = put_ bh . NonEmpty.toList
    get bh = NonEmpty.fromList <$> get bh

instance (Ix a, Binary a, Binary b) => Binary (Array a b) where
    put_ bh arr = do
        put_ bh $ bounds arr
        put_ bh $ elems arr
    get bh = do
        bounds <- get bh
        xs <- get bh
        return $ listArray bounds xs

instance (Binary a, Binary b) => Binary (a,b) where
    put_ bh (a,b) = do put_ bh a; put_ bh b
    get bh        = do a <- get bh
                       b <- get bh
                       return (a,b)

instance (Binary a, Binary b, Binary c) => Binary (a,b,c) where
    put_ bh (a,b,c) = do put_ bh a; put_ bh b; put_ bh c
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         return (a,b,c)

instance (Binary a, Binary b, Binary c, Binary d) => Binary (a,b,c,d) where
    put_ bh (a,b,c,d) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d
    get bh            = do a <- get bh
                           b <- get bh
                           c <- get bh
                           d <- get bh
                           return (a,b,c,d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d, e) where
    put_ bh (a,b,c,d, e) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e;
    get bh               = do a <- get bh
                              b <- get bh
                              c <- get bh
                              d <- get bh
                              e <- get bh
                              return (a,b,c,d,e)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => Binary (a,b,c,d, e, f) where
    put_ bh (a,b,c,d, e, f) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e; put_ bh f;
    get bh                  = do a <- get bh
                                 b <- get bh
                                 c <- get bh
                                 d <- get bh
                                 e <- get bh
                                 f <- get bh
                                 return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f, Binary g) => Binary (a,b,c,d,e,f,g) where
    put_ bh (a,b,c,d,e,f,g) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e; put_ bh f; put_ bh g
    get bh                  = do a <- get bh
                                 b <- get bh
                                 c <- get bh
                                 d <- get bh
                                 e <- get bh
                                 f <- get bh
                                 g <- get bh
                                 return (a,b,c,d,e,f,g)

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing  = putByte bh 0
    put_ bh (Just a) = do putByte bh 1; put_ bh a
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

instance Binary a => Binary (Strict.Maybe a) where
    put_ bh Strict.Nothing = putByte bh 0
    put_ bh (Strict.Just a) = do putByte bh 1; put_ bh a
    get bh =
      do h <- getWord8 bh
         case h of
           0 -> return Strict.Nothing
           _ -> do x <- get bh; return (Strict.Just x)

instance (Binary a, Binary b) => Binary (Either a b) where
    put_ bh (Left  a) = do putByte bh 0; put_ bh a
    put_ bh (Right b) = do putByte bh 1; put_ bh b
    get bh            = do h <- getWord8 bh
                           case h of
                             0 -> do a <- get bh ; return (Left a)
                             _ -> do b <- get bh ; return (Right b)

instance Binary UTCTime where
    put_ bh u = do put_ bh (utctDay u)
                   put_ bh (utctDayTime u)
    get bh = do day <- get bh
                dayTime <- get bh
                return $ UTCTime { utctDay = day, utctDayTime = dayTime }

instance Binary Day where
    put_ bh d = put_ bh (toModifiedJulianDay d)
    get bh = do i <- get bh
                return $ ModifiedJulianDay { toModifiedJulianDay = i }

instance Binary DiffTime where
    put_ bh dt = put_ bh (toRational dt)
    get bh = do r <- get bh
                return $ fromRational r

instance Binary JoinPointHood where
    put_ bh NotJoinPoint = putByte bh 0
    put_ bh (JoinPoint ar) = do
        putByte bh 1
        put_ bh ar
    get bh = do
        h <- getByte bh
        case h of
            0 -> return NotJoinPoint
            _ -> do { ar <- get bh; return (JoinPoint ar) }

{-
Finally - a reasonable portable Integer instance.

We used to encode values in the Int32 range as such,
falling back to a string of all things. In either case
we stored a tag byte to discriminate between the two cases.

This made some sense as it's highly portable but also not very
efficient.

However GHC stores a surprisingly large number of large Integer
values. In the examples looked at between 25% and 50% of Integers
serialized were outside of the Int32 range.

Consider a value like `2724268014499746065`, some sort of hash
actually generated by GHC.
In the old scheme this was encoded as a list of 19 chars. This
gave a size of 77 Bytes, one for the length of the list and 76
since we encode chars as Word32 as well.

We can easily do better. The new plan is:

* Start with a tag byte
  * 0 => Int64 (LEB128 encoded)
  * 1 => Negative large integer
  * 2 => Positive large integer
* Followed by the value:
  * Int64 is encoded as usual
  * Large integers are encoded as a list of bytes (Word8).
    We use Data.Bits which defines a bit order independent of the representation.
    Values are stored LSB first.

This means our example value `2724268014499746065` is now only 10 bytes large.
* One byte tag
* One byte for the length of the [Word8] list.
* 8 bytes for the actual date.

The new scheme also does not depend in any way on
architecture specific details.

We still use this scheme even with LEB128 available,
as it has less overhead for truly large numbers. (> maxBound :: Int64)

The instance is used for in Binary Integer and Binary Rational in GHC.Types.Literal
-}

instance Binary Integer where
    put_ bh i
      | i >= lo64 && i <= hi64 = do
          putWord8 bh 0
          put_ bh (fromIntegral i :: Int64)
      | otherwise = do
          if i < 0
            then putWord8 bh 1
            else putWord8 bh 2
          put_ bh (unroll $ abs i)
      where
        lo64 = fromIntegral (minBound :: Int64)
        hi64 = fromIntegral (maxBound :: Int64)
    get bh = do
      int_kind <- getWord8 bh
      case int_kind of
        0 -> fromIntegral <$!> (get bh :: IO Int64)
        -- Large integer
        1 -> negate <$!> getInt
        2 -> getInt
        _ -> panic "Binary Integer - Invalid byte"
        where
          getInt :: IO Integer
          getInt = roll <$!> (get bh :: IO [Word8])

unroll :: Integer -> [Word8]
unroll = unfoldr step
  where
    step 0 = Nothing
    step i = Just (fromIntegral i, i `shiftR` 8)

roll :: [Word8] -> Integer
roll   = foldl' unstep 0 . reverse
  where
    unstep a b = a `shiftL` 8 .|. fromIntegral b


    {-
    -- This code is currently commented out.
    -- See https://gitlab.haskell.org/ghc/ghc/issues/3379#note_104346 for
    -- discussion.

    put_ bh (S# i#) = do putByte bh 0; put_ bh (I# i#)
    put_ bh (J# s# a#) = do
        putByte bh 1
        put_ bh (I# s#)
        let sz# = sizeofByteArray# a#  -- in *bytes*
        put_ bh (I# sz#)  -- in *bytes*
        putByteArray bh a# sz#

    get bh = do
        b <- getByte bh
        case b of
          0 -> do (I# i#) <- get bh
                  return (S# i#)
          _ -> do (I# s#) <- get bh
                  sz <- get bh
                  (BA a#) <- getByteArray bh sz
                  return (J# s# a#)

putByteArray :: BinHandle -> ByteArray# -> Int# -> IO ()
putByteArray bh a s# = loop 0#
  where loop n#
           | n# ==# s# = return ()
           | otherwise = do
                putByte bh (indexByteArray a n#)
                loop (n# +# 1#)

getByteArray :: BinHandle -> Int -> IO ByteArray
getByteArray bh (I# sz) = do
  (MBA arr) <- newByteArray sz
  let loop n
           | n ==# sz = return ()
           | otherwise = do
                w <- getByte bh
                writeByteArray arr n w
                loop (n +# 1#)
  loop 0#
  freezeByteArray arr
    -}

{-
data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s, arr #) ->
  (# s, BA arr #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()
writeByteArray arr i (W8# w) = IO $ \s ->
  case writeWord8Array# arr i w s of { s ->
  (# s, () #) }

indexByteArray :: ByteArray# -> Int# -> Word8
indexByteArray a# n# = W8# (indexWord8Array# a# n#)

-}
instance (Binary a) => Binary (Ratio a) where
    put_ bh (a :% b) = do put_ bh a; put_ bh b
    get bh = do a <- get bh; b <- get bh; return (a :% b)

-- Instance uses fixed-width encoding to allow inserting
-- Bin placeholders in the stream.
instance Binary (Bin a) where
  put_ bh (BinPtr i) = putWord32 bh (fromIntegral i :: Word32)
  get bh = do i <- getWord32 bh; return (BinPtr (fromIntegral (i :: Word32)))

-- Instance uses fixed-width encoding to allow inserting
-- Bin placeholders in the stream.
instance Binary (RelBinPtr a) where
  put_ bh (RelBinPtr i) = put_ bh i
  get bh = RelBinPtr <$> get bh

-- -----------------------------------------------------------------------------
-- Forward reading/writing

-- | @'forwardPut' put_A put_B@ outputs A after B but allows A to be read before B
-- by using a forward reference.
forwardPut :: WriteBinHandle -> (b -> IO a) -> IO b -> IO (a,b)
forwardPut bh put_A put_B = do
  -- write placeholder pointer to A
  pre_a <- tellBinWriter bh
  put_ bh pre_a

  -- write B
  r_b <- put_B

  -- update A's pointer
  a <- tellBinWriter bh
  putAt bh pre_a a
  seekBinNoExpandWriter bh a

  -- write A
  r_a <- put_A r_b
  pure (r_a,r_b)

forwardPut_ :: WriteBinHandle -> (b -> IO a) -> IO b -> IO ()
forwardPut_ bh put_A put_B = void $ forwardPut bh put_A put_B

-- | Read a value stored using a forward reference
--
-- The forward reference is expected to be an absolute offset.
forwardGet :: ReadBinHandle -> IO a -> IO a
forwardGet bh get_A = do
    -- read forward reference
    p <- get bh -- a BinPtr
    -- store current position
    p_a <- tellBinReader bh
    -- go read the forward value, then seek back
    seekBinReader bh p
    r <- get_A
    seekBinReader bh p_a
    pure r

-- | @'forwardPutRel' put_A put_B@ outputs A after B but allows A to be read before B
-- by using a forward reference.
--
-- This forward reference is a relative offset that allows us to skip over the
-- result of 'put_A'.
forwardPutRel :: WriteBinHandle -> (b -> IO a) -> IO b -> IO (a,b)
forwardPutRel bh put_A put_B = do
  -- write placeholder pointer to A
  pre_a <- tellBinWriter bh
  put_ bh pre_a

  -- write B
  r_b <- put_B

  -- update A's pointer
  a <- tellBinWriter bh
  putAtRel bh pre_a a
  seekBinNoExpandWriter bh a

  -- write A
  r_a <- put_A r_b
  pure (r_a,r_b)

-- | Like 'forwardGetRel', but discard the result.
forwardPutRel_ :: WriteBinHandle -> (b -> IO a) -> IO b -> IO ()
forwardPutRel_ bh put_A put_B = void $ forwardPutRel bh put_A put_B

-- | Read a value stored using a forward reference.
--
-- The forward reference is expected to be a relative offset.
forwardGetRel :: ReadBinHandle -> IO a -> IO a
forwardGetRel bh get_A = do
    -- read forward reference
    p <- getRelBin bh
    -- store current position
    p_a <- tellBinReader bh
    -- go read the forward value, then seek back
    seekBinReader bh $ makeAbsoluteBin p
    r <- get_A
    seekBinReader bh p_a
    pure r

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyPut :: Binary a => WriteBinHandle -> a -> IO ()
lazyPut = lazyPut' put_

lazyGet :: Binary a => ReadBinHandle -> IO a
lazyGet = lazyGet' get

lazyPut' :: (WriteBinHandle -> a -> IO ()) -> WriteBinHandle -> a -> IO ()
lazyPut' f bh a = do
    -- output the obj with a ptr to skip over it:
    pre_a <- tellBinWriter bh
    put_ bh pre_a       -- save a slot for the ptr
    f bh a           -- dump the object
    q <- tellBinWriter bh     -- q = ptr to after object
    putAtRel bh pre_a q    -- fill in slot before a with ptr to q
    seekBinWriter bh q        -- finally carry on writing at q

lazyGet' :: (ReadBinHandle -> IO a) -> ReadBinHandle -> IO a
lazyGet' f bh = do
    p <- getRelBin bh -- a BinPtr
    p_a <- tellBinReader bh
    a <- unsafeInterleaveIO $ do
        -- NB: Use a fresh rbm_off_r variable in the child thread, for thread
        -- safety.
        off_r <- newFastMutInt 0
        let bh' = bh { rbm_off_r = off_r }
        seekBinReader bh' p_a
        f bh'
    seekBinReader bh (makeAbsoluteBin p) -- skip over the object for now
    return a

-- | Serialize the constructor strictly but lazily serialize a value inside a
-- 'Just'.
--
-- This way we can check for the presence of a value without deserializing the
-- value itself.
lazyPutMaybe :: Binary a => WriteBinHandle -> Maybe a -> IO ()
lazyPutMaybe bh Nothing  = putWord8 bh 0
lazyPutMaybe bh (Just x) = do
  putWord8 bh 1
  lazyPut bh x

-- | Deserialize a value serialized by 'lazyPutMaybe'.
lazyGetMaybe :: Binary a => ReadBinHandle -> IO (Maybe a)
lazyGetMaybe bh = do
  h <- getWord8 bh
  case h of
    0 -> pure Nothing
    _ -> Just <$> lazyGet bh

-- -----------------------------------------------------------------------------
-- UserData
-- -----------------------------------------------------------------------------

-- Note [Binary UserData]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- Information we keep around during interface file
-- serialization/deserialization. Namely we keep the functions for serializing
-- and deserializing 'Name's and 'FastString's. We do this because we actually
-- use serialization in two distinct settings,
--
-- * When serializing interface files themselves
--
-- * When computing the fingerprint of an IfaceDecl (which we computing by
--   hashing its Binary serialization)
--
-- These two settings have different needs while serializing Names:
--
-- * Names in interface files are serialized via a symbol table (see Note
--   [Symbol table representation of names] in "GHC.Iface.Binary").
--
-- * During fingerprinting a binding Name is serialized as the OccName and a
--   non-binding Name is serialized as the fingerprint of the thing they
--   represent. See Note [Fingerprinting IfaceDecls] for further discussion.
--

-- | Newtype to serialise binding names differently to non-binding 'Name'.
-- See Note [Binary UserData]
newtype BindingName = BindingName { getBindingName :: Name }
  deriving ( Eq )

simpleBindingNameWriter :: BinaryWriter Name -> BinaryWriter BindingName
simpleBindingNameWriter = coerce

simpleBindingNameReader :: BinaryReader Name -> BinaryReader BindingName
simpleBindingNameReader = coerce

-- | Existential for 'BinaryWriter' with a type witness.
data SomeBinaryWriter = forall a . SomeBinaryWriter (Refl.TypeRep a) (BinaryWriter a)

-- | Existential for 'BinaryReader' with a type witness.
data SomeBinaryReader = forall a . SomeBinaryReader (Refl.TypeRep a) (BinaryReader a)

-- | UserData required to serialise symbols for interface files.
--
-- See Note [Binary UserData]
data WriterUserData =
   WriterUserData {
      ud_writer_data :: Map SomeTypeRep SomeBinaryWriter
      -- ^ A mapping from a type witness to the 'Writer' for the associated type.
      -- This is a 'Map' because microbenchmarks indicated this is more efficient
      -- than other representations for less than ten elements.
      --
      -- Considered representations:
      --
      -- * [(TypeRep, SomeBinaryWriter)]
      -- * bytehash (on hackage)
      -- * Map TypeRep SomeBinaryWriter
   }

-- | UserData required to deserialise symbols for interface files.
--
-- See Note [Binary UserData]
data ReaderUserData =
   ReaderUserData {
      ud_reader_data :: Map SomeTypeRep SomeBinaryReader
      -- ^ A mapping from a type witness to the 'Reader' for the associated type.
      -- This is a 'Map' because microbenchmarks indicated this is more efficient
      -- than other representations for less than ten elements.
      --
      -- Considered representations:
      --
      -- * [(TypeRep, SomeBinaryReader)]
      -- * bytehash (on hackage)
      -- * Map TypeRep SomeBinaryReader
   }

mkWriterUserData :: [SomeBinaryWriter] -> WriterUserData
mkWriterUserData caches = noWriterUserData
  { ud_writer_data = Map.fromList $ map (\cache@(SomeBinaryWriter typRep _) -> (SomeTypeRep typRep, cache)) caches
  }

mkReaderUserData :: [SomeBinaryReader] -> ReaderUserData
mkReaderUserData caches = noReaderUserData
  { ud_reader_data = Map.fromList $ map (\cache@(SomeBinaryReader typRep _) -> (SomeTypeRep typRep, cache)) caches
  }

mkSomeBinaryWriter :: forall a . Refl.Typeable a => BinaryWriter a -> SomeBinaryWriter
mkSomeBinaryWriter cb = SomeBinaryWriter (Refl.typeRep @a) cb

mkSomeBinaryReader :: forall a . Refl.Typeable a => BinaryReader a -> SomeBinaryReader
mkSomeBinaryReader cb = SomeBinaryReader (Refl.typeRep @a) cb

newtype BinaryReader s = BinaryReader
  { getEntry :: ReadBinHandle -> IO s
  } deriving (Functor)

newtype BinaryWriter s = BinaryWriter
  { putEntry :: WriteBinHandle -> s -> IO ()
  }

mkWriter :: (WriteBinHandle -> s -> IO ()) -> BinaryWriter s
mkWriter f = BinaryWriter
  { putEntry = f
  }

mkReader :: (ReadBinHandle -> IO s) -> BinaryReader s
mkReader f = BinaryReader
  { getEntry = f
  }

-- | Find the 'BinaryReader' for the 'Binary' instance for the type identified by 'Proxy a'.
--
-- If no 'BinaryReader' has been configured before, this function will panic.
findUserDataReader :: forall a . Refl.Typeable a => Proxy a -> ReadBinHandle -> BinaryReader a
findUserDataReader query bh =
  case Map.lookup (Refl.someTypeRep query) (ud_reader_data $ getReaderUserData bh) of
    Nothing -> panic $ "Failed to find BinaryReader for the key: " ++ show (Refl.someTypeRep query)
    Just (SomeBinaryReader _ (reader :: BinaryReader x)) ->
      unsafeCoerce @(BinaryReader x) @(BinaryReader a) reader
      -- This 'unsafeCoerce' could be written safely like this:
      --
      -- @
      --   Just (SomeBinaryReader _ (reader :: BinaryReader x)) ->
      --     case testEquality (typeRep @a) tyRep of
      --       Just Refl -> coerce @(BinaryReader x) @(BinaryReader a) reader
      --       Nothing -> panic $ "Invariant violated"
      -- @
      --
      -- But it comes at a slight performance cost and this function is used in
      -- binary serialisation hot loops, thus, we prefer the small performance boost over
      -- the additional type safety.

-- | Find the 'BinaryWriter' for the 'Binary' instance for the type identified by 'Proxy a'.
--
-- If no 'BinaryWriter' has been configured before, this function will panic.
findUserDataWriter :: forall a . Refl.Typeable a => Proxy a -> WriteBinHandle -> BinaryWriter a
findUserDataWriter query bh =
  case Map.lookup (Refl.someTypeRep query) (ud_writer_data $ getWriterUserData bh) of
    Nothing -> panic $ "Failed to find BinaryWriter for the key: " ++ show (Refl.someTypeRep query)
    Just (SomeBinaryWriter _ (writer :: BinaryWriter x)) ->
      unsafeCoerce @(BinaryWriter x) @(BinaryWriter a) writer
      -- This 'unsafeCoerce' could be written safely like this:
      --
      -- @
      --   Just (SomeBinaryWriter tyRep (writer :: BinaryWriter x)) ->
      --     case testEquality (typeRep @a) tyRep of
      --       Just Refl -> coerce @(BinaryWriter x) @(BinaryWriter a) writer
      --       Nothing -> panic $ "Invariant violated"
      -- @
      --
      -- But it comes at a slight performance cost and this function is used in
      -- binary serialisation hot loops, thus, we prefer the small performance boost over
      -- the additional type safety.


noReaderUserData :: ReaderUserData
noReaderUserData = ReaderUserData
  { ud_reader_data = Map.empty
  }

noWriterUserData :: WriterUserData
noWriterUserData = WriterUserData
  { ud_writer_data = Map.empty
  }

newReadState :: (ReadBinHandle -> IO Name)   -- ^ how to deserialize 'Name's
             -> (ReadBinHandle -> IO FastString)
             -> ReaderUserData
newReadState get_name get_fs =
  mkReaderUserData
    [ mkSomeBinaryReader $ mkReader get_name
    , mkSomeBinaryReader $ mkReader @BindingName (coerce get_name)
    , mkSomeBinaryReader $ mkReader get_fs
    ]

newWriteState :: (WriteBinHandle -> Name -> IO ())
                 -- ^ how to serialize non-binding 'Name's
              -> (WriteBinHandle -> Name -> IO ())
                 -- ^ how to serialize binding 'Name's
              -> (WriteBinHandle -> FastString -> IO ())
              -> WriterUserData
newWriteState put_non_binding_name put_binding_name put_fs =
  mkWriterUserData
    [ mkSomeBinaryWriter $ mkWriter (\bh name -> put_binding_name bh (getBindingName name))
    , mkSomeBinaryWriter $ mkWriter put_non_binding_name
    , mkSomeBinaryWriter $ mkWriter put_fs
    ]

-- ----------------------------------------------------------------------------
-- Types for lookup and deduplication tables.
-- ----------------------------------------------------------------------------

-- | A 'ReaderTable' describes how to deserialise a table from disk,
-- and how to create a 'BinaryReader' that looks up values in the deduplication table.
data ReaderTable a = ReaderTable
  { getTable :: ReadBinHandle -> IO (SymbolTable a)
  -- ^ Deserialise a list of elements into a 'SymbolTable'.
  , mkReaderFromTable :: SymbolTable a -> BinaryReader a
  -- ^ Given the table from 'getTable', create a 'BinaryReader'
  -- that reads values only from the 'SymbolTable'.
  }

-- | A 'WriterTable' is an interface any deduplication table can implement to
-- describe how the table can be written to disk.
newtype WriterTable = WriterTable
  { putTable :: WriteBinHandle -> IO Int
  -- ^ Serialise a table to disk. Returns the number of written elements.
  }

-- ----------------------------------------------------------------------------
-- Common data structures for constructing and maintaining lookup tables for
-- binary serialisation and deserialisation.
-- ----------------------------------------------------------------------------

-- | The 'GenericSymbolTable' stores a mapping from already seen elements to an index.
-- If an element wasn't seen before, it is added to the mapping together with a fresh
-- index.
--
-- 'GenericSymbolTable' is a variant of a 'BinSymbolTable' that is polymorphic in the table implementation.
-- As such it can be used with any container that implements the 'TrieMap' type class.
--
-- While 'GenericSymbolTable' is similar to the 'BinSymbolTable', it supports storing tree-like
-- structures such as 'Type' and 'IfaceType' more efficiently.
--
data GenericSymbolTable m = GenericSymbolTable
  { gen_symtab_next :: !FastMutInt
  -- ^ The next index to use.
  , gen_symtab_map  :: !(IORef (m Int))
  -- ^ Given a symbol, find the symbol and return its index.
  , gen_symtab_to_write :: !(IORef [Key m])
  -- ^ Reversed list of values to write into the buffer.
  -- This is an optimisation, as it allows us to write out quickly all
  -- newly discovered values that are discovered when serialising 'Key m'
  -- to disk.
  }

-- | Initialise a 'GenericSymbolTable', initialising the index to '0'.
initGenericSymbolTable :: TrieMap m => IO (GenericSymbolTable m)
initGenericSymbolTable = do
  symtab_next <- newFastMutInt 0
  symtab_map <- newIORef emptyTM
  symtab_todo <- newIORef []
  pure $ GenericSymbolTable
        { gen_symtab_next = symtab_next
        , gen_symtab_map  = symtab_map
        , gen_symtab_to_write = symtab_todo
        }

-- | Serialise the 'GenericSymbolTable' to disk.
--
-- Since 'GenericSymbolTable' stores tree-like structures, such as 'IfaceType',
-- serialising an element can add new elements to the mapping.
-- Thus, 'putGenericSymbolTable' first serialises all values, and then checks whether any
-- new elements have been discovered. If so, repeat the loop.
putGenericSymbolTable :: forall m. (TrieMap m) => GenericSymbolTable m -> (WriteBinHandle -> Key m -> IO ()) -> WriteBinHandle -> IO Int
{-# INLINE putGenericSymbolTable #-}
putGenericSymbolTable gen_sym_tab serialiser bh = do
  putGenericSymbolTable bh
  where
    symtab_next = gen_symtab_next gen_sym_tab
    symtab_to_write = gen_symtab_to_write gen_sym_tab
    putGenericSymbolTable :: WriteBinHandle -> IO Int
    putGenericSymbolTable bh  = do
      let loop = do
            vs <- atomicModifyIORef' symtab_to_write (\a -> ([], a))
            case vs of
              [] -> readFastMutInt symtab_next
              todo -> do
                mapM_ (\n -> serialiser bh n) (reverse todo)
                loop
      snd <$>
        (forwardPutRel bh (const $ readFastMutInt symtab_next >>= put_ bh) $
          loop)

-- | Read the elements of a 'GenericSymbolTable' from disk into a 'SymbolTable'.
getGenericSymbolTable :: forall a . (ReadBinHandle -> IO a) -> ReadBinHandle -> IO (SymbolTable a)
getGenericSymbolTable deserialiser bh = do
  sz <- forwardGetRel bh (get bh) :: IO Int
  mut_arr <- newArray_ (0, sz-1) :: IO (IOArray Int a)
  forM_ [0..(sz-1)] $ \i -> do
    f <- deserialiser bh
    writeArray mut_arr i f
  unsafeFreeze mut_arr

-- | Write an element 'Key m' to the given 'WriteBinHandle'.
--
-- If the element was seen before, we simply write the index of that element to the
-- 'WriteBinHandle'. If we haven't seen it before, we add the element to
-- the 'GenericSymbolTable', increment the index, and return this new index.
putGenericSymTab :: (TrieMap m) => GenericSymbolTable m -> WriteBinHandle -> Key m -> IO ()
{-# INLINE putGenericSymTab #-}
putGenericSymTab GenericSymbolTable{
               gen_symtab_map = symtab_map_ref,
               gen_symtab_next = symtab_next,
               gen_symtab_to_write = symtab_todo }
        bh val = do
  symtab_map <- readIORef symtab_map_ref
  case lookupTM val symtab_map of
    Just off -> put_ bh (fromIntegral off :: Word32)
    Nothing -> do
      off <- readFastMutInt symtab_next
      writeFastMutInt symtab_next (off+1)
      writeIORef symtab_map_ref
          $! insertTM val off symtab_map
      atomicModifyIORef symtab_todo (\todo -> (val : todo, ()))
      put_ bh (fromIntegral off :: Word32)

-- | Read a value from a 'SymbolTable'.
getGenericSymtab :: Binary a => SymbolTable a -> ReadBinHandle -> IO a
getGenericSymtab symtab bh = do
  i :: Word32 <- get bh
  return $! symtab ! fromIntegral i

---------------------------------------------------------
-- The Dictionary
---------------------------------------------------------

-- | A 'SymbolTable' of 'FastString's.
type Dictionary = SymbolTable FastString

initFastStringReaderTable :: IO (ReaderTable FastString)
initFastStringReaderTable = do
  return $
    ReaderTable
      { getTable = getDictionary
      , mkReaderFromTable = \tbl -> mkReader (getDictFastString tbl)
      }

initFastStringWriterTable :: IO (WriterTable, BinaryWriter FastString)
initFastStringWriterTable = do
  dict_next_ref <- newFastMutInt 0
  dict_map_ref <- newIORef emptyUFM
  let bin_dict =
        FSTable
          { fs_tab_next = dict_next_ref
          , fs_tab_map = dict_map_ref
          }
  let put_dict bh = do
        fs_count <- readFastMutInt dict_next_ref
        dict_map <- readIORef dict_map_ref
        putDictionary bh fs_count dict_map
        pure fs_count

  return
    ( WriterTable
        { putTable = put_dict
        }
    , mkWriter $ putDictFastString bin_dict
    )

putDictionary :: WriteBinHandle -> Int -> UniqFM FastString (Int,FastString) -> IO ()
putDictionary bh sz dict = do
  put_ bh sz
  mapM_ (putFS bh) (elems (array (0,sz-1) (nonDetEltsUFM dict)))
    -- It's OK to use nonDetEltsUFM here because the elements have indices
    -- that array uses to create order

getDictionary :: ReadBinHandle -> IO Dictionary
getDictionary bh = do
  sz <- get bh :: IO Int
  mut_arr <- newArray_ (0, sz-1) :: IO (IOArray Int FastString)
  forM_ [0..(sz-1)] $ \i -> do
    fs <- getFS bh
    writeArray mut_arr i fs
  unsafeFreeze mut_arr

getDictFastString :: Dictionary -> ReadBinHandle -> IO FastString
getDictFastString dict bh = do
    j <- get bh
    return $! (dict ! fromIntegral (j :: Word32))

putDictFastString :: FSTable -> WriteBinHandle -> FastString -> IO ()
putDictFastString dict bh fs = allocateFastString dict fs >>= put_ bh

allocateFastString :: FSTable -> FastString -> IO Word32
allocateFastString FSTable { fs_tab_next = j_r
                           , fs_tab_map  = out_r
                           } f = do
    out <- readIORef out_r
    let !uniq = getUnique f
    case lookupUFM_Directly out uniq of
        Just (j, _)  -> return (fromIntegral j :: Word32)
        Nothing -> do
           j <- readFastMutInt j_r
           writeFastMutInt j_r (j + 1)
           writeIORef out_r $! addToUFM_Directly out uniq (j, f)
           return (fromIntegral j :: Word32)

-- FSTable is an exact copy of Haddock.InterfaceFile.BinDictionary. We rename to
-- avoid a collision and copy to avoid a dependency.
data FSTable = FSTable { fs_tab_next :: !FastMutInt -- The next index to use
                       , fs_tab_map  :: !(IORef (UniqFM FastString (Int,FastString)))
                                -- indexed by FastString
  }


---------------------------------------------------------
-- The Symbol Table
---------------------------------------------------------

-- | Symbols that are read from disk.
-- The 'SymbolTable' index starts on '0'.
type SymbolTable a = Array Int a

---------------------------------------------------------
-- Reading and writing FastStrings
---------------------------------------------------------

putFS :: WriteBinHandle -> FastString -> IO ()
putFS bh fs = putBS bh $ bytesFS fs

getFS :: ReadBinHandle -> IO FastString
getFS bh = do
  l  <- get bh :: IO Int
  getPrim bh l (\src -> pure $! mkFastStringBytes src l )

-- | Put a ByteString without its length (can't be read back without knowing the
-- length!)
putByteString :: WriteBinHandle -> ByteString -> IO ()
putByteString bh bs =
  BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> do
    putPrim bh l (\op -> copyBytes op (castPtr ptr) l)

-- | Get a ByteString whose length is known
getByteString :: ReadBinHandle -> Int -> IO ByteString
getByteString bh l =
  BS.create l $ \dest -> do
    getPrim bh l (\src -> copyBytes dest src l)

putBS :: WriteBinHandle -> ByteString -> IO ()
putBS bh bs =
  BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> do
    put_ bh l
    putPrim bh l (\op -> copyBytes op (castPtr ptr) l)

getBS :: ReadBinHandle -> IO ByteString
getBS bh = do
  l <- get bh :: IO Int
  BS.create l $ \dest -> do
    getPrim bh l (\src -> copyBytes dest src l)

instance Binary ByteString where
  put_ bh f = putBS bh f
  get bh = getBS bh

instance Binary FastString where
  put_ bh f =
    case findUserDataWriter (Proxy :: Proxy FastString) bh of
      tbl -> putEntry tbl bh f

  get bh =
    case findUserDataReader (Proxy :: Proxy FastString) bh of
      tbl -> getEntry tbl bh

deriving instance Binary NonDetFastString
deriving instance Binary LexicalFastString

instance Binary Fingerprint where
  put_ h (Fingerprint w1 w2) = do put_ h w1; put_ h w2
  get  h = do w1 <- get h; w2 <- get h; return (Fingerprint w1 w2)

instance Binary ModuleName where
  put_ bh (ModuleName fs) = put_ bh fs
  get bh = do fs <- get bh; return (ModuleName fs)

-- instance Binary TupleSort where
--     put_ bh BoxedTuple      = putByte bh 0
--     put_ bh UnboxedTuple    = putByte bh 1
--     put_ bh ConstraintTuple = putByte bh 2
--     get bh = do
--       h <- getByte bh
--       case h of
--         0 -> do return BoxedTuple
--         1 -> do return UnboxedTuple
--         _ -> do return ConstraintTuple

-- instance Binary Activation where
--     put_ bh NeverActive = do
--             putByte bh 0
--     put_ bh FinalActive = do
--             putByte bh 1
--     put_ bh AlwaysActive = do
--             putByte bh 2
--     put_ bh (ActiveBefore src aa) = do
--             putByte bh 3
--             put_ bh src
--             put_ bh aa
--     put_ bh (ActiveAfter src ab) = do
--             putByte bh 4
--             put_ bh src
--             put_ bh ab
--     get bh = do
--             h <- getByte bh
--             case h of
--               0 -> do return NeverActive
--               1 -> do return FinalActive
--               2 -> do return AlwaysActive
--               3 -> do src <- get bh
--                       aa <- get bh
--                       return (ActiveBefore src aa)
--               _ -> do src <- get bh
--                       ab <- get bh
--                       return (ActiveAfter src ab)

-- instance Binary InlinePragma where
--     put_ bh (InlinePragma s a b c d) = do
--             put_ bh s
--             put_ bh a
--             put_ bh b
--             put_ bh c
--             put_ bh d

--     get bh = do
--            s <- get bh
--            a <- get bh
--            b <- get bh
--            c <- get bh
--            d <- get bh
--            return (InlinePragma s a b c d)

-- instance Binary RuleMatchInfo where
--     put_ bh FunLike = putByte bh 0
--     put_ bh ConLike = putByte bh 1
--     get bh = do
--             h <- getByte bh
--             if h == 1 then return ConLike
--                       else return FunLike

-- instance Binary InlineSpec where
--     put_ bh NoUserInlinePrag = putByte bh 0
--     put_ bh Inline           = putByte bh 1
--     put_ bh Inlinable        = putByte bh 2
--     put_ bh NoInline         = putByte bh 3

--     get bh = do h <- getByte bh
--                 case h of
--                   0 -> return NoUserInlinePrag
--                   1 -> return Inline
--                   2 -> return Inlinable
--                   _ -> return NoInline

-- instance Binary RecFlag where
--     put_ bh Recursive = do
--             putByte bh 0
--     put_ bh NonRecursive = do
--             putByte bh 1
--     get bh = do
--             h <- getByte bh
--             case h of
--               0 -> do return Recursive
--               _ -> do return NonRecursive

-- instance Binary OverlapMode where
--     put_ bh (NoOverlap    s) = putByte bh 0 >> put_ bh s
--     put_ bh (Overlaps     s) = putByte bh 1 >> put_ bh s
--     put_ bh (Incoherent   s) = putByte bh 2 >> put_ bh s
--     put_ bh (Overlapping  s) = putByte bh 3 >> put_ bh s
--     put_ bh (Overlappable s) = putByte bh 4 >> put_ bh s
--     get bh = do
--         h <- getByte bh
--         case h of
--             0 -> (get bh) >>= \s -> return $ NoOverlap s
--             1 -> (get bh) >>= \s -> return $ Overlaps s
--             2 -> (get bh) >>= \s -> return $ Incoherent s
--             3 -> (get bh) >>= \s -> return $ Overlapping s
--             4 -> (get bh) >>= \s -> return $ Overlappable s
--             _ -> panic ("get OverlapMode" ++ show h)


-- instance Binary OverlapFlag where
--     put_ bh flag = do put_ bh (overlapMode flag)
--                       put_ bh (isSafeOverlap flag)
--     get bh = do
--         h <- get bh
--         b <- get bh
--         return OverlapFlag { overlapMode = h, isSafeOverlap = b }

-- instance Binary FixityDirection where
--     put_ bh InfixL = do
--             putByte bh 0
--     put_ bh InfixR = do
--             putByte bh 1
--     put_ bh InfixN = do
--             putByte bh 2
--     get bh = do
--             h <- getByte bh
--             case h of
--               0 -> do return InfixL
--               1 -> do return InfixR
--               _ -> do return InfixN

-- instance Binary Fixity where
--     put_ bh (Fixity src aa ab) = do
--             put_ bh src
--             put_ bh aa
--             put_ bh ab
--     get bh = do
--           src <- get bh
--           aa <- get bh
--           ab <- get bh
--           return (Fixity src aa ab)

-- instance Binary WarningTxt where
--     put_ bh (WarningTxt s w) = do
--             putByte bh 0
--             put_ bh s
--             put_ bh w
--     put_ bh (DeprecatedTxt s d) = do
--             putByte bh 1
--             put_ bh s
--             put_ bh d

--     get bh = do
--             h <- getByte bh
--             case h of
--               0 -> do s <- get bh
--                       w <- get bh
--                       return (WarningTxt s w)
--               _ -> do s <- get bh
--                       d <- get bh
--                       return (DeprecatedTxt s d)

-- instance Binary StringLiteral where
--   put_ bh (StringLiteral st fs _) = do
--             put_ bh st
--             put_ bh fs
--   get bh = do
--             st <- get bh
--             fs <- get bh
--             return (StringLiteral st fs Nothing)

newtype BinLocated a = BinLocated { unBinLocated :: Located a }

instance Binary a => Binary (BinLocated a) where
    put_ bh (BinLocated (L l x)) = do
            put_ bh $ BinSrcSpan l
            put_ bh x

    get bh = do
            l <- unBinSrcSpan <$> get bh
            x <- get bh
            return $ BinLocated (L l x)

newtype BinSpan = BinSpan { unBinSpan :: RealSrcSpan }

-- See Note [Source Location Wrappers]
instance Binary BinSpan where
  put_ bh (BinSpan ss) = do
            put_ bh (srcSpanFile ss)
            put_ bh (srcSpanStartLine ss)
            put_ bh (srcSpanStartCol ss)
            put_ bh (srcSpanEndLine ss)
            put_ bh (srcSpanEndCol ss)

  get bh = do
            f <- get bh
            sl <- get bh
            sc <- get bh
            el <- get bh
            ec <- get bh
            return $ BinSpan (mkRealSrcSpan (mkRealSrcLoc f sl sc)
                                            (mkRealSrcLoc f el ec))

instance Binary UnhelpfulSpanReason where
  put_ bh r = case r of
    UnhelpfulNoLocationInfo -> putByte bh 0
    UnhelpfulWiredIn        -> putByte bh 1
    UnhelpfulInteractive    -> putByte bh 2
    UnhelpfulGenerated      -> putByte bh 3
    UnhelpfulOther fs       -> putByte bh 4 >> put_ bh fs

  get bh = do
    h <- getByte bh
    case h of
      0 -> return UnhelpfulNoLocationInfo
      1 -> return UnhelpfulWiredIn
      2 -> return UnhelpfulInteractive
      3 -> return UnhelpfulGenerated
      _ -> UnhelpfulOther <$> get bh

newtype BinSrcSpan = BinSrcSpan { unBinSrcSpan :: SrcSpan }

-- See Note [Source Location Wrappers]
instance Binary BinSrcSpan where
  put_ bh (BinSrcSpan (RealSrcSpan ss _sb)) = do
          putByte bh 0
          -- BufSpan doesn't ever get serialised because the positions depend
          -- on build location.
          put_ bh $ BinSpan ss

  put_ bh (BinSrcSpan (UnhelpfulSpan s)) = do
          putByte bh 1
          put_ bh s

  get bh = do
          h <- getByte bh
          case h of
            0 -> do BinSpan ss <- get bh
                    return $ BinSrcSpan (RealSrcSpan ss Strict.Nothing)
            _ -> do s <- get bh
                    return $ BinSrcSpan (UnhelpfulSpan s)


{-
Note [Source Location Wrappers]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Source locations are banned from interface files, to
prevent filepaths affecting interface hashes.

Unfortunately, we can't remove all binary instances,
as they're used to serialise .hie files, and we don't
want to break binary compatibility.

To this end, the Bin[Src]Span newtypes wrappers were
introduced to prevent accidentally serialising a
source location as part of a larger structure.
-}

--------------------------------------------------------------------------------
-- Instances for the containers package
--------------------------------------------------------------------------------

instance (Binary v) => Binary (IntMap v) where
  put_ bh m = put_ bh (IntMap.toAscList m)
  get bh = IntMap.fromAscList <$> get bh


{- Note [FingerprintWithValue]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
FingerprintWithValue is a wrapper which allows us to store a fingerprint and
optionally the value which was used to create the fingerprint.

This is useful for storing information in interface files, where we want to
store the fingerprint of the interface file, but also the value which was used
to create the fingerprint (e.g. the DynFlags).

The wrapper is useful to ensure that the fingerprint can be read quickly without
having to deserialise the value itself.
-}

-- | A wrapper which allows us to store a fingerprint and optionally the value which
-- was used to create the fingerprint.
data FingerprintWithValue a = FingerprintWithValue !Fingerprint (Maybe a)
  deriving Functor

instance Binary a => Binary (FingerprintWithValue a) where
  put_ bh (FingerprintWithValue fp val) = do
    put_ bh fp
    lazyPutMaybe bh val

  get bh = do
    fp <- get bh
    val <- lazyGetMaybe bh
    return $ FingerprintWithValue fp val

instance NFData a => NFData (FingerprintWithValue a) where
  rnf (FingerprintWithValue fp mflags)
    = rnf fp `seq` rnf mflags `seq` ()