{-# OPTIONS -cpp #-}
{-# OPTIONS_GHC -O -funbox-strict-fields #-}
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

module Binary
  ( {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle,
    SymbolTable, Dictionary,

   openBinIO, openBinIO_,
   openBinMem,
--   closeBin,

   seekBin,
   seekBy,
   tellBin,
   castBin,

   writeBinMem,
   readBinMem,

   fingerprintBinMem,
   computeFingerprint,

   isEOFBin,

   putAt, getAt,

   -- for writing instances:
   putByte,
   getByte,

   -- lazy Bin I/O
   lazyGet,
   lazyPut,

#ifdef __GLASGOW_HASKELL__
   -- GHC only:
   ByteArray(..),
   getByteArray,
   putByteArray,
#endif

   UserData(..), getUserData, setUserData,
   newReadState, newWriteState,
   putDictionary, getDictionary, putFS,
  ) where

#include "HsVersions.h"

-- The *host* architecture version:
#include "../includes/MachDeps.h"

import {-# SOURCE #-} Name (Name)
import FastString
import Panic
import UniqFM
import FastMutInt
import Fingerprint
import BasicTypes

import Foreign
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import Data.IORef
import Data.Char                ( ord, chr )
import Data.Time
import Data.Typeable
import Data.Typeable.Internal
import Control.Monad            ( when )
import System.IO as IO
import System.IO.Unsafe         ( unsafeInterleaveIO )
import System.IO.Error          ( mkIOError, eofErrorType )
import GHC.Real                 ( Ratio(..) )
import GHC.Exts
import GHC.Word                 ( Word8(..) )

import GHC.IO ( IO(..) )

type BinArray = ForeignPtr Word8

---------------------------------------------------------------
-- BinHandle
---------------------------------------------------------------

data BinHandle
  = BinMem {                     -- binary data stored in an unboxed array
     bh_usr :: UserData,         -- sigh, need parameterized modules :-)
     _off_r :: !FastMutInt,      -- the current offset
     _sz_r  :: !FastMutInt,      -- size of the array (cached)
     _arr_r :: !(IORef BinArray) -- the array (bounds: (0,size-1))
    }
        -- XXX: should really store a "high water mark" for dumping out
        -- the binary data to a file.

  | BinIO {                     -- binary data stored in a file
     bh_usr :: UserData,
     _off_r :: !FastMutInt,     -- the current offset (cached)
     _hdl   :: !IO.Handle       -- the file handle (must be seekable)
   }
        -- cache the file ptr in BinIO; using hTell is too expensive
        -- to call repeatedly.  If anyone else is modifying this Handle
        -- at the same time, we'll be screwed.

getUserData :: BinHandle -> UserData
getUserData bh = bh_usr bh

setUserData :: BinHandle -> UserData -> BinHandle
setUserData bh us = bh { bh_usr = us }


---------------------------------------------------------------
-- Bin
---------------------------------------------------------------

newtype Bin a = BinPtr Int
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i) = BinPtr i

---------------------------------------------------------------
-- class Binary
---------------------------------------------------------------

class Binary a where
    put_   :: BinHandle -> a -> IO ()
    put    :: BinHandle -> a -> IO (Bin a)
    get    :: BinHandle -> IO a

    -- define one of put_, put.  Use of put_ is recommended because it
    -- is more likely that tail-calls can kick in, and we rarely need the
    -- position return value.
    put_ bh a = do _ <- put bh a; return ()
    put bh a  = do p <- tellBin bh; put_ bh a; return p

putAt  :: Binary a => BinHandle -> Bin a -> a -> IO ()
putAt bh p x = do seekBin bh p; put_ bh x; return ()

getAt  :: Binary a => BinHandle -> Bin a -> IO a
getAt bh p = do seekBin bh p; get bh

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h

openBinIO :: IO.Handle -> IO BinHandle
openBinIO h = do
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BinIO noUserData r h)

openBinMem :: Int -> IO BinHandle
openBinMem size
 | size <= 0 = error "Data.Binary.openBinMem: size must be >= 0"
 | otherwise = do
   arr <- mallocForeignPtrBytes size
   arr_r <- newIORef arr
   ix_r <- newFastMutInt
   writeFastMutInt ix_r 0
   sz_r <- newFastMutInt
   writeFastMutInt sz_r size
   return (BinMem noUserData ix_r sz_r arr_r)

tellBin :: BinHandle -> IO (Bin a)
tellBin (BinIO  _ r _)   = do ix <- readFastMutInt r; return (BinPtr ix)
tellBin (BinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBin :: BinHandle -> Bin a -> IO ()
seekBin (BinIO _ ix_r h) (BinPtr p) = do
  writeFastMutInt ix_r p
  hSeek h AbsoluteSeek (fromIntegral p)
seekBin h@(BinMem _ ix_r sz_r _) (BinPtr p) = do
  sz <- readFastMutInt sz_r
  if (p >= sz)
        then do expandBin h p; writeFastMutInt ix_r p
        else writeFastMutInt ix_r p

seekBy :: BinHandle -> Int -> IO ()
seekBy (BinIO _ ix_r h) off = do
  ix <- readFastMutInt ix_r
  let ix' = ix + off
  writeFastMutInt ix_r ix'
  hSeek h AbsoluteSeek (fromIntegral ix')
seekBy h@(BinMem _ ix_r sz_r _) off = do
  sz <- readFastMutInt sz_r
  ix <- readFastMutInt ix_r
  let ix' = ix + off
  if (ix' >= sz)
        then do expandBin h ix'; writeFastMutInt ix_r ix'
        else writeFastMutInt ix_r ix'

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinMem _ ix_r sz_r _) = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  return (ix >= sz)
isEOFBin (BinIO _ _ h) = hIsEOF h

writeBinMem :: BinHandle -> FilePath -> IO ()
writeBinMem (BinIO _ _ _) _ = error "Data.Binary.writeBinMem: not a memory handle"
writeBinMem (BinMem _ ix_r _ arr_r) fn = do
  h <- openBinaryFile fn WriteMode
  arr <- readIORef arr_r
  ix  <- readFastMutInt ix_r
  withForeignPtr arr $ \p -> hPutBuf h p ix
  hClose h

readBinMem :: FilePath -> IO BinHandle
-- Return a BinHandle with a totally undefined State
readBinMem filename = do
  h <- openBinaryFile filename ReadMode
  filesize' <- hFileSize h
  let filesize = fromIntegral filesize'
  arr <- mallocForeignPtrBytes (filesize*2)
  count <- withForeignPtr arr $ \p -> hGetBuf h p filesize
  when (count /= filesize) $
       error ("Binary.readBinMem: only read " ++ show count ++ " bytes")
  hClose h
  arr_r <- newIORef arr
  ix_r <- newFastMutInt
  writeFastMutInt ix_r 0
  sz_r <- newFastMutInt
  writeFastMutInt sz_r filesize
  return (BinMem noUserData ix_r sz_r arr_r)

fingerprintBinMem :: BinHandle -> IO Fingerprint
fingerprintBinMem (BinIO _ _ _) = error "Binary.md5BinMem: not a memory handle"
fingerprintBinMem (BinMem _ ix_r _ arr_r) = do
  arr <- readIORef arr_r
  ix <- readFastMutInt ix_r
  withForeignPtr arr $ \p -> fingerprintData p ix

computeFingerprint :: Binary a
                   => (BinHandle -> Name -> IO ())
                   -> a
                   -> IO Fingerprint

computeFingerprint put_name a = do
  bh <- openBinMem (3*1024) -- just less than a block
  bh <- return $ setUserData bh $ newWriteState put_name putFS
  put_ bh a
  fingerprintBinMem bh

-- expand the size of the array to include a specified offset
expandBin :: BinHandle -> Int -> IO ()
expandBin (BinMem _ _ sz_r arr_r) off = do
   sz <- readFastMutInt sz_r
   let sz' = head (dropWhile (<= off) (iterate (* 2) sz))
   arr <- readIORef arr_r
   arr' <- mallocForeignPtrBytes sz'
   withForeignPtr arr $ \old ->
     withForeignPtr arr' $ \new ->
       copyBytes new old sz 
   writeFastMutInt sz_r sz'
   writeIORef arr_r arr'
expandBin (BinIO _ _ _) _ = return ()
-- no need to expand a file, we'll assume they expand by themselves.

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 h@(BinMem _ ix_r sz_r arr_r) w = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    -- double the size of the array if it overflows
    if (ix >= sz)
        then do expandBin h ix
                putWord8 h w
        else do arr <- readIORef arr_r
                withForeignPtr arr $ \p -> pokeByteOff p ix w
                writeFastMutInt ix_r (ix+1)
                return ()
putWord8 (BinIO _ ix_r h) w = do
    ix <- readFastMutInt ix_r
    hPutChar h (chr (fromIntegral w)) -- XXX not really correct
    writeFastMutInt ix_r (ix+1)
    return ()

getWord8 :: BinHandle -> IO Word8
getWord8 (BinMem _ ix_r sz_r arr_r) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r
    when (ix >= sz) $
        ioError (mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r
    w <- withForeignPtr arr $ \p -> peekByteOff p ix
    writeFastMutInt ix_r (ix+1)
    return w
getWord8 (BinIO _ ix_r h) = do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c)) -- XXX not really correct

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = put_ bh w

getByte :: BinHandle -> IO Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Primitve Word writes

instance Binary Word8 where
  put_ = putWord8
  get  = getWord8

instance Binary Word16 where
  put_ h w = do -- XXX too slow.. inline putWord8?
    putByte h (fromIntegral (w `shiftR` 8))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 8) .|. fromIntegral w2)


instance Binary Word32 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 24))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 8)  .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 24) .|.
               (fromIntegral w2 `shiftL` 16) .|.
               (fromIntegral w3 `shiftL`  8) .|.
               (fromIntegral w4))

instance Binary Word64 where
  put_ h w = do
    putByte h (fromIntegral (w `shiftR` 56))
    putByte h (fromIntegral ((w `shiftR` 48) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 40) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 32) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 24) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR` 16) .&. 0xff))
    putByte h (fromIntegral ((w `shiftR`  8) .&. 0xff))
    putByte h (fromIntegral (w .&. 0xff))
  get h = do
    w1 <- getWord8 h
    w2 <- getWord8 h
    w3 <- getWord8 h
    w4 <- getWord8 h
    w5 <- getWord8 h
    w6 <- getWord8 h
    w7 <- getWord8 h
    w8 <- getWord8 h
    return $! ((fromIntegral w1 `shiftL` 56) .|.
               (fromIntegral w2 `shiftL` 48) .|.
               (fromIntegral w3 `shiftL` 40) .|.
               (fromIntegral w4 `shiftL` 32) .|.
               (fromIntegral w5 `shiftL` 24) .|.
               (fromIntegral w6 `shiftL` 16) .|.
               (fromIntegral w7 `shiftL`  8) .|.
               (fromIntegral w8))

-- -----------------------------------------------------------------------------
-- Primitve Int writes

instance Binary Int8 where
  put_ h w = put_ h (fromIntegral w :: Word8)
  get h    = do w <- get h; return $! (fromIntegral (w::Word8))

instance Binary Int16 where
  put_ h w = put_ h (fromIntegral w :: Word16)
  get h    = do w <- get h; return $! (fromIntegral (w::Word16))

instance Binary Int32 where
  put_ h w = put_ h (fromIntegral w :: Word32)
  get h    = do w <- get h; return $! (fromIntegral (w::Word32))

instance Binary Int64 where
  put_ h w = put_ h (fromIntegral w :: Word64)
  get h    = do w <- get h; return $! (fromIntegral (w::Word64))

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
        if (len < 0xff)
          then putByte bh (fromIntegral len :: Word8)
          else do putByte bh 0xff; put_ bh (fromIntegral len :: Word32)
        mapM_ (put_ bh) l
    get bh = do
        b <- getByte bh
        len <- if b == 0xff
                  then get bh
                  else return (fromIntegral b :: Word32)
        let loop 0 = return []
            loop n = do a <- get bh; as <- loop (n-1); return (a:as)
        loop len

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

instance Binary a => Binary (Maybe a) where
    put_ bh Nothing  = putByte bh 0
    put_ bh (Just a) = do putByte bh 1; put_ bh a
    get bh           = do h <- getWord8 bh
                          case h of
                            0 -> return Nothing
                            _ -> do x <- get bh; return (Just x)

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

#if defined(__GLASGOW_HASKELL__) || 1
--to quote binary-0.3 on this code idea,
--
-- TODO  This instance is not architecture portable.  GMP stores numbers as
-- arrays of machine sized words, so the byte format is not portable across
-- architectures with different endianess and word size.
--
-- This makes it hard (impossible) to make an equivalent instance
-- with code that is compilable with non-GHC.  Do we need any instance
-- Binary Integer, and if so, does it have to be blazing fast?  Or can
-- we just change this instance to be portable like the rest of the
-- instances? (binary package has code to steal for that)
--
-- yes, we need Binary Integer and Binary Rational in basicTypes/Literal.lhs

instance Binary Integer where
    -- XXX This is hideous
    put_ bh i = put_ bh (show i)
    get bh = do str <- get bh
                case reads str of
                    [(i, "")] -> return i
                    _ -> fail ("Binary Integer: got " ++ show str)

    {-
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
-}

-- As for the rest of this code, even though this module
-- exports it, it doesn't seem to be used anywhere else
-- in GHC!

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

instance (Integral a, Binary a) => Binary (Ratio a) where
    put_ bh (a :% b) = do put_ bh a; put_ bh b
    get bh = do a <- get bh; b <- get bh; return (a :% b)
#endif

instance Binary (Bin a) where
  put_ bh (BinPtr i) = put_ bh (fromIntegral i :: Int32)
  get bh = do i <- get bh; return (BinPtr (fromIntegral (i :: Int32)))

-- -----------------------------------------------------------------------------
-- Instances for Data.Typeable stuff

instance Binary TyCon where
    put_ bh (TyCon _ p m n) = do
        put_ bh (p,m,n)
    get bh = do
        (p,m,n) <- get bh
        return (mkTyCon3 p m n)

instance Binary TypeRep where
    put_ bh type_rep = do
        let (ty_con, child_type_reps) = splitTyConApp type_rep
        put_ bh ty_con
        put_ bh child_type_reps
    get bh = do
        ty_con <- get bh
        child_type_reps <- get bh
        return (mkTyConApp ty_con child_type_reps)

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyPut :: Binary a => BinHandle -> a -> IO ()
lazyPut bh a = do
    -- output the obj with a ptr to skip over it:
    pre_a <- tellBin bh
    put_ bh pre_a       -- save a slot for the ptr
    put_ bh a           -- dump the object
    q <- tellBin bh     -- q = ptr to after object
    putAt bh pre_a q    -- fill in slot before a with ptr to q
    seekBin bh q        -- finally carry on writing at q

-- XXX: This function is not thread-safe on BinIO handles.
lazyGet :: Binary a => BinHandle -> IO a
lazyGet bh = do
    p <- get bh -- a BinPtr
    p_a <- tellBin bh
    a <- unsafeInterleaveIO $ do
        -- NB: Use a fresh off_r variable in the child thread, for thread
        -- safety.
        off_r <- newFastMutInt
        getAt bh { _off_r = off_r } p_a
    seekBin bh p -- skip over the object for now
    return a

-- -----------------------------------------------------------------------------
-- UserData
-- -----------------------------------------------------------------------------

data UserData =
   UserData {
        -- for *deserialising* only:
        ud_get_name :: BinHandle -> IO Name,
        ud_get_fs   :: BinHandle -> IO FastString,

        -- for *serialising* only:
        ud_put_name :: BinHandle -> Name       -> IO (),
        ud_put_fs   :: BinHandle -> FastString -> IO ()
   }

newReadState :: (BinHandle -> IO Name)
             -> (BinHandle -> IO FastString)
             -> UserData
newReadState get_name get_fs
  = UserData { ud_get_name = get_name,
               ud_get_fs   = get_fs,
               ud_put_name = undef "put_name",
               ud_put_fs   = undef "put_fs"
             }
   
newWriteState :: (BinHandle -> Name       -> IO ()) 
              -> (BinHandle -> FastString -> IO ())
              -> UserData
newWriteState put_name put_fs
  = UserData { ud_get_name = undef "get_name",
               ud_get_fs   = undef "get_fs",
               ud_put_name = put_name,
               ud_put_fs   = put_fs
             }

noUserData :: a
noUserData = undef "UserData"

undef :: String -> a
undef s = panic ("Binary.UserData: no " ++ s)

---------------------------------------------------------
-- The Dictionary
---------------------------------------------------------

type Dictionary = Array Int FastString -- The dictionary
                                       -- Should be 0-indexed

putDictionary :: BinHandle -> Int -> UniqFM (Int,FastString) -> IO ()
putDictionary bh sz dict = do
  put_ bh sz
  mapM_ (putFS bh) (elems (array (0,sz-1) (eltsUFM dict)))

getDictionary :: BinHandle -> IO Dictionary
getDictionary bh = do
  sz <- get bh
  elems <- sequence (take sz (repeat (getFS bh)))
  return (listArray (0,sz-1) elems)

---------------------------------------------------------
-- The Symbol Table
---------------------------------------------------------

-- On disk, the symbol table is an array of IfaceExtName, when
-- reading it in we turn it into a SymbolTable.

type SymbolTable = Array Int Name

---------------------------------------------------------
-- Reading and writing FastStrings
---------------------------------------------------------

putFS :: BinHandle -> FastString -> IO ()
putFS bh fs = putBS bh $ fastStringToByteString fs

getFS :: BinHandle -> IO FastString
getFS bh = do bs <- getBS bh
              mkFastStringByteString bs

putBS :: BinHandle -> ByteString -> IO ()
putBS bh bs =
  BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> do
  put_ bh l
  let
        go n | n == l    = return ()
             | otherwise = do
                b <- peekElemOff (castPtr ptr) n
                putByte bh b
                go (n+1)
  go 0

{- -- possible faster version, not quite there yet:
getBS bh@BinMem{} = do
  (I# l) <- get bh
  arr <- readIORef (arr_r bh)
  off <- readFastMutInt (off_r bh)
  return $! (mkFastSubBytesBA# arr off l)
-}
getBS :: BinHandle -> IO ByteString
getBS bh = do
  l <- get bh
  fp <- mallocForeignPtrBytes l
  withForeignPtr fp $ \ptr -> do
  let
        go n | n == l = return $ BS.fromForeignPtr fp 0 l
             | otherwise = do
                b <- getByte bh
                pokeElemOff ptr n b
                go (n+1)
  --
  go 0

instance Binary ByteString where
  put_ bh f = putBS bh f
  get bh = getBS bh

instance Binary FastString where
  put_ bh f =
    case getUserData bh of
        UserData { ud_put_fs = put_fs } -> put_fs bh f

  get bh =
    case getUserData bh of
        UserData { ud_get_fs = get_fs } -> get_fs bh

-- Here to avoid loop

instance Binary Fingerprint where
  put_ h (Fingerprint w1 w2) = do put_ h w1; put_ h w2
  get  h = do w1 <- get h; w2 <- get h; return (Fingerprint w1 w2)

instance Binary FunctionOrData where
    put_ bh IsFunction = putByte bh 0
    put_ bh IsData     = putByte bh 1
    get bh = do
        h <- getByte bh
        case h of
          0 -> return IsFunction
          1 -> return IsData
          _ -> panic "Binary FunctionOrData"

instance Binary TupleSort where
    put_ bh BoxedTuple      = putByte bh 0
    put_ bh UnboxedTuple    = putByte bh 1
    put_ bh ConstraintTuple = putByte bh 2
    get bh = do
      h <- getByte bh
      case h of
        0 -> do return BoxedTuple
        1 -> do return UnboxedTuple
        _ -> do return ConstraintTuple

instance Binary Activation where
    put_ bh NeverActive = do
            putByte bh 0
    put_ bh AlwaysActive = do
            putByte bh 1
    put_ bh (ActiveBefore aa) = do
            putByte bh 2
            put_ bh aa
    put_ bh (ActiveAfter ab) = do
            putByte bh 3
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return NeverActive
              1 -> do return AlwaysActive
              2 -> do aa <- get bh
                      return (ActiveBefore aa)
              _ -> do ab <- get bh
                      return (ActiveAfter ab)

instance Binary InlinePragma where
    put_ bh (InlinePragma a b c d) = do
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d

    get bh = do
           a <- get bh
           b <- get bh
           c <- get bh
           d <- get bh
           return (InlinePragma a b c d)

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike

instance Binary InlineSpec where
    put_ bh EmptyInlineSpec = putByte bh 0
    put_ bh Inline          = putByte bh 1
    put_ bh Inlinable       = putByte bh 2
    put_ bh NoInline        = putByte bh 3

    get bh = do h <- getByte bh
                case h of
                  0 -> return EmptyInlineSpec
                  1 -> return Inline
                  2 -> return Inlinable
                  _ -> return NoInline

instance Binary DefMethSpec where
    put_ bh NoDM      = putByte bh 0
    put_ bh VanillaDM = putByte bh 1
    put_ bh GenericDM = putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> return NoDM
              1 -> return VanillaDM
              _ -> return GenericDM

instance Binary RecFlag where
    put_ bh Recursive = do
            putByte bh 0
    put_ bh NonRecursive = do
            putByte bh 1
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return Recursive
              _ -> do return NonRecursive

instance Binary OverlapFlag where
    put_ bh (NoOverlap  b) = putByte bh 0 >> put_ bh b
    put_ bh (OverlapOk  b) = putByte bh 1 >> put_ bh b
    put_ bh (Incoherent b) = putByte bh 2 >> put_ bh b
    get bh = do
        h <- getByte bh
        b <- get bh
        case h of
            0 -> return $ NoOverlap b
            1 -> return $ OverlapOk b
            2 -> return $ Incoherent b
            _ -> panic ("get OverlapFlag " ++ show h)

instance Binary FixityDirection where
    put_ bh InfixL = do
            putByte bh 0
    put_ bh InfixR = do
            putByte bh 1
    put_ bh InfixN = do
            putByte bh 2
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return InfixL
              1 -> do return InfixR
              _ -> do return InfixN

instance Binary Fixity where
    put_ bh (Fixity aa ab) = do
            put_ bh aa
            put_ bh ab
    get bh = do
          aa <- get bh
          ab <- get bh
          return (Fixity aa ab)

instance Binary WarningTxt where
    put_ bh (WarningTxt w) = do
            putByte bh 0
            put_ bh w
    put_ bh (DeprecatedTxt d) = do
            putByte bh 1
            put_ bh d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do w <- get bh
                      return (WarningTxt w)
              _ -> do d <- get bh
                      return (DeprecatedTxt d)

