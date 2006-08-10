{-# OPTIONS_GHC -cpp -fglasgow-exts #-}
--
-- (c) The University of Glasgow 2002
--
-- Binary I/O library, with special tweaks for GHC
--
-- Based on the nhc98 Binary library, which is copyright
-- (c) Malcolm Wallace and Colin Runciman, University of York, 1998.
-- Under the terms of the license for that software, we must tell you
-- where you can obtain the original version of the Binary library, namely
--     http://www.cs.york.ac.uk/fp/nhc98/

module Binary2
  ( {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle,

   openBinIO, 
   openBinIO_,
   openBinMem,
--   closeBin,

--   getUserData,

   seekBin,
   tellBin,
   castBin,

   writeBinMem,
   readBinMem,

   isEOFBin,

   -- for writing instances:
   putByte,
   getByte,

   putString,
   getString,

   -- lazy Bin I/O
   lazyGet,
   lazyPut,

   -- GHC only:
   ByteArray(..),
   getByteArray,
   putByteArray,

--   getBinFileWithDict,	-- :: Binary a => FilePath -> IO a
--   putBinFileWithDict,	-- :: Binary a => FilePath -> Module -> a -> IO ()

    -- re-export for the benefit of other modules.
   openBinaryFile, 

   FormatVersion,
   nullFormatVersion,
   mkFormatVersion,
  ) where

#include "MachDeps.h"

import FastMutInt

import Char
import Monad

#if __GLASGOW_HASKELL__ < 503
import IOExts
import Bits
import Int
import Word
import Char
import Monad
import Exception
import GlaExts hiding (ByteArray, newByteArray, freezeByteArray)
import Array
import IO
import PrelIOBase		( IOError(..), IOErrorType(..)
#if __GLASGOW_HASKELL__ > 411
				, IOException(..)
#endif
				)
import PrelReal			( Ratio(..) )
import PrelIOBase	 	( IO(..) )
#else
import Data.Array.IO
import Data.Array
import Data.Bits
import Data.Int
import Data.Word
import Data.IORef
import Data.Char		( ord, chr )
import Data.Array.Base  	( unsafeRead, unsafeWrite )
import Control.Monad		( when )
import Control.Exception	( throwDyn )
import System.IO as IO
import System.IO.Unsafe		( unsafeInterleaveIO )
import System.IO.Error		( mkIOError, eofErrorType )
import GHC.Real			( Ratio(..) )
import GHC.Exts
import GHC.IOBase	 	( IO(..) )
import GHC.Word			( Word8(..) )
#if __GLASGOW_HASKELL__ < 601
-- openFileEx is available from the lang package, but we want to 
-- be independent of hslibs libraries.
import GHC.Handle		( openFileEx, IOModeEx(..) )
#endif
#endif

import IO

#if __GLASGOW_HASKELL__ < 601
openBinaryFile f mode = openFileEx f (BinaryMode mode)
#endif

#if __GLASGOW_HASKELL__ < 503
type BinArray = MutableByteArray RealWorld Int
newArray_ bounds     = stToIO (newCharArray bounds)
unsafeWrite arr ix e = stToIO (writeWord8Array arr ix e)
unsafeRead  arr ix   = stToIO (readWord8Array arr ix)
#if __GLASGOW_HASKELL__ < 411
newByteArray#        = newCharArray#
#endif
hPutArray h arr sz   = hPutBufBAFull h arr sz
hGetArray h sz       = hGetBufBAFull h sz

mkIOError :: IOErrorType -> String -> Maybe Handle -> Maybe FilePath -> Exception
mkIOError t location maybe_hdl maybe_filename
  = IOException (IOError maybe_hdl t location ""
#if __GLASGOW_HASKELL__ > 411
		         maybe_filename
#endif
  		)

eofErrorType = EOF

#ifndef SIZEOF_HSINT
#define SIZEOF_HSINT  INT_SIZE_IN_BYTES
#endif

#ifndef SIZEOF_HSWORD
#define SIZEOF_HSWORD WORD_SIZE_IN_BYTES
#endif

#else
type BinArray = IOUArray Int Word8
#endif

data BinHandle
  = BinMem {		-- binary data stored in an unboxed array
     state :: Int,	-- sigh, need parameterized modules :-)
     off_r :: !FastMutInt,		-- the current offset
     sz_r  :: !FastMutInt,		-- size of the array (cached)
     arr_r :: !(IORef BinArray) 	-- the array (bounds: (0,size-1))
    }
	-- XXX: should really store a "high water mark" for dumping out
	-- the binary data to a file.

  | BinIO {		-- binary data stored in a file
     state :: Int,
     off_r :: !FastMutInt,		-- the current offset (cached)
     hdl   :: !IO.Handle		-- the file handle (must be seekable)
   }
	-- cache the file ptr in BinIO; using hTell is too expensive
	-- to call repeatedly.  If anyone else is modifying this Handle
	-- at the same time, we'll be screwed.

newtype Bin a = BinPtr Int 
  deriving (Eq, Ord, Show, Bounded)

castBin :: Bin a -> Bin b
castBin (BinPtr i) = BinPtr i

class Binary a where
    put_   :: BinHandle -> a -> IO ()
    put    :: BinHandle -> a -> IO (Bin a)
    get    :: BinHandle -> IO a

    -- define one of put_, put.  Use of put_ is recommended because it
    -- is more likely that tail-calls can kick in, and we rarely need the
    -- position return value.
    put_ bh a = do put bh a; return ()
    put bh a  = do p <- tellBin bh; put_ bh a; return p

putAt  :: Binary a => BinHandle -> Bin a -> a -> IO ()
putAt bh p x = do seekBin bh p; put bh x; return ()

getAt  :: Binary a => BinHandle -> Bin a -> IO a
getAt bh p = do seekBin bh p; get bh

openBinIO_ :: IO.Handle -> IO BinHandle
openBinIO_ h = openBinIO h

openBinIO :: IO.Handle -> IO BinHandle
openBinIO h = do
  r <- newFastMutInt
  writeFastMutInt r 0
  return (BinIO undefined r h)

openBinMem :: Int -> IO BinHandle
openBinMem size
 | size <= 0 = error "Data.Binary.openBinMem: size must be >= 0"
 | otherwise = do
   arr <- newArray_ (0,size-1)
   arr_r0 <- newIORef arr
   ix_r <- newFastMutInt
   writeFastMutInt ix_r 0
   sz_r0 <- newFastMutInt
   writeFastMutInt sz_r0 size
   return (BinMem undefined ix_r sz_r0 arr_r0)

--noBinHandleUserData :: a
--noBinHandleUserData = error "Binary.BinHandle: no user data"

--getUserData :: BinHandle -> BinHandleState
--getUserData bh = state bh

tellBin :: BinHandle -> IO (Bin a)
tellBin (BinIO _ r _)   = do ix <- readFastMutInt r; return (BinPtr ix)
tellBin (BinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBin :: BinHandle -> Bin a -> IO ()
seekBin (BinIO _ ix_r h) (BinPtr p) = do 
  writeFastMutInt ix_r p
  hSeek h AbsoluteSeek (fromIntegral p)
seekBin h@(BinMem _ ix_r sz_r0 _) (BinPtr p) = do
  sz <- readFastMutInt sz_r0
  if (p >= sz)
	then do expandBin h p; writeFastMutInt ix_r p
	else writeFastMutInt ix_r p

isEOFBin :: BinHandle -> IO Bool
isEOFBin (BinMem _ ix_r sz_r0 _) = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r0
  return (ix >= sz)
isEOFBin (BinIO _ _ h) = hIsEOF h

writeBinMem :: BinHandle -> FilePath -> IO ()
writeBinMem (BinIO _ _ _) _ = error "Data.Binary.writeBinMem: not a memory handle"
writeBinMem (BinMem _ ix_r _ arr_r0) fn = do
  h <- openBinaryFile fn WriteMode
  arr <- readIORef arr_r0
  ix  <- readFastMutInt ix_r
  hPutArray h arr ix
  hClose h

readBinMem :: FilePath -> IO BinHandle
readBinMem filename = do
  h <- openBinaryFile filename ReadMode
  filesize' <- hFileSize h
  let filesize = fromIntegral filesize'
  arr <- newArray_ (0,filesize-1)
  count <- hGetArray h arr filesize
  when (count /= filesize)
	(error ("Binary.readBinMem: only read " ++ show count ++ " bytes"))
  hClose h
  arr_r0 <- newIORef arr
  ix_r <- newFastMutInt
  writeFastMutInt ix_r 0
  sz_r0 <- newFastMutInt
  writeFastMutInt sz_r0 filesize
  return (BinMem undefined {-initReadState-} ix_r sz_r0 arr_r0)

-- expand the size of the array to include a specified offset
expandBin :: BinHandle -> Int -> IO ()
expandBin (BinMem _ _ sz_r0 arr_r0) off = do
   sz <- readFastMutInt sz_r0
   let sz' = head (dropWhile (<= off) (iterate (* 2) sz))
   arr <- readIORef arr_r0
   arr' <- newArray_ (0,sz'-1)
   sequence_ [ unsafeRead arr i >>= unsafeWrite arr' i
 	     | i <- [ 0 .. sz-1 ] ]
   writeFastMutInt sz_r0 sz'
   writeIORef arr_r0 arr'
   hPutStrLn stderr ("expanding to size: " ++ show sz')
   return ()
expandBin (BinIO _ _ _) _ = return ()
	-- no need to expand a file, we'll assume they expand by themselves.

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 h@(BinMem _ ix_r sz_r0 arr_r0) w = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r0
	-- double the size of the array if it overflows
    if (ix >= sz) 
	then do expandBin h ix
		putWord8 h w
	else do arr <- readIORef arr_r0
		unsafeWrite arr ix w
    		writeFastMutInt ix_r (ix+1)
    		return ()
putWord8 (BinIO _ ix_r h) w = do
    ix <- readFastMutInt ix_r
    hPutChar h (chr (fromIntegral w))	-- XXX not really correct
    writeFastMutInt ix_r (ix+1)
    return ()

getWord8 :: BinHandle -> IO Word8
getWord8 (BinMem _ ix_r sz_r0 arr_r0) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r0
    when (ix >= sz)  $
	ioError (mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r0
    w <- unsafeRead arr ix
    writeFastMutInt ix_r (ix+1)
    return w
getWord8 (BinIO _ ix_r h) = do
    ix <- readFastMutInt ix_r
    c <- hGetChar h
    writeFastMutInt ix_r (ix+1)
    return $! (fromIntegral (ord c))	-- XXX not really correct

-- | Get the next byte, but don't change the pointer.
peekWord8 :: BinHandle -> IO Word8
peekWord8 (BinMem _ ix_r sz_r0 arr_r0) = do
    ix <- readFastMutInt ix_r
    sz <- readFastMutInt sz_r0
    when (ix >= sz)  $
	ioError (mkIOError eofErrorType "Data.Binary.getWord8" Nothing Nothing)
    arr <- readIORef arr_r0
    w <- unsafeRead arr ix
    return w
peekWord8 (BinIO _ ix_r h) = do
    c <- hLookAhead h
    return $! (fromIntegral (ord c))	-- XXX not really correct

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
--    getF bh p  = case getBitsF bh 0 p of (_,b) -> ((),b)

instance Binary Bool where
    put_ bh b = putByte bh (fromIntegral (fromEnum b))
    get  bh   = do x <- getWord8 bh; return $! (toEnum (fromIntegral x))
--    getF bh p = case getBitsF bh 1 p of (x,b) -> (toEnum x,b)

instance Binary Char where
    put_  bh c = put_ bh (fromIntegral (ord c) :: Word32)
    get  bh   = do x <- get bh; return $! (chr (fromIntegral (x :: Word32)))
--    getF bh p = case getBitsF bh 8 p of (x,b) -> (toEnum x,b)

instance Binary Int where
#if SIZEOF_HSINT == 4
    put_ bh i = put_ bh (fromIntegral i :: Int32)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int32))
#elif SIZEOF_HSINT == 8
    put_ bh i = put_ bh (fromIntegral i :: Int64)
    get  bh = do
	x <- get bh
	return $! (fromIntegral (x :: Int64))
#else
#error "unsupported sizeof(HsInt)"
#endif
--    getF bh   = getBitsF bh 32

{-
instance Binary a => Binary [a] where
    put_ bh []     = putByte bh 0
    put_ bh (x:xs) = do putByte bh 1; put_ bh x; put_ bh xs
    get bh         = do h <- getWord8 bh
                        case h of
                          0 -> return []
                          _ -> do x  <- get bh
                                  xs <- get bh
                                  return (x:xs)
-}
instance Binary a => Binary [a] where
    put_ bh l =
	do put_ bh (length l)
	   mapM (put_ bh) l
	   return ()
    get bh =
	do len <- get bh
	   mapM (\_ -> get bh) [1..(len::Int)]

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
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         return (a,b,c,d)

instance (Binary a, Binary b, Binary c, Binary d, Binary e) => Binary (a,b,c,d,e) where
    put_ bh (a,b,c,d,e) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         e <- get bh
                         return (a,b,c,d,e)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f) => Binary (a,b,c,d,e,f) where
    put_ bh (a,b,c,d,e,f) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e; put_ bh f
    get bh          = do a <- get bh
                         b <- get bh
                         c <- get bh
                         d <- get bh
                         e <- get bh
                         f <- get bh
                         return (a,b,c,d,e,f)

instance (Binary a, Binary b, Binary c, Binary d, Binary e, Binary f,Binary g) => Binary (a,b,c,d,e,f,g) where
    put_ bh (a,b,c,d,e,f,g) = do put_ bh a; put_ bh b; put_ bh c; put_ bh d; put_ bh e; put_ bh f; put_ bh g
    get bh          = do a <- get bh
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

instance (Binary a, Binary b) => Binary (Either a b) where
    put_ bh (Left  a) = do putByte bh 0; put_ bh a
    put_ bh (Right b) = do putByte bh 1; put_ bh b
    get bh            = do h <- getWord8 bh
                           case h of
                             0 -> do a <- get bh ; return (Left a)
                             _ -> do b <- get bh ; return (Right b)

instance Binary Integer where
    put_ bh (S# i#) = do putByte bh 0; put_ bh (I# i#)
    put_ bh (J# s# a#) = do
 	p <- putByte bh 1;
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


data ByteArray = BA ByteArray#
data MBA = MBA (MutableByteArray# RealWorld)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s0 ->
  case newByteArray# sz s0 of { (# s, arr #) ->
  (# s, MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr0 = IO $ \s0 ->
  case unsafeFreezeByteArray# arr0 s0 of { (# s, arr #) ->
  (# s, BA arr #) }

writeByteArray :: MutableByteArray# RealWorld -> Int# -> Word8 -> IO ()

writeByteArray arr i w8 = IO $ \s0 ->
  case fromIntegral w8 of { W# w# -> 
  case writeCharArray# arr i (chr# (word2Int# w#)) s0  of { s ->
  (# s , () #) }}

indexByteArray :: ByteArray# -> Int# -> Word8
indexByteArray a# n# = fromIntegral (I# (ord# (indexCharArray# a# n#)))

instance (Integral a, Binary a) => Binary (Ratio a) where
    put_ bh (a :% b) = do put_ bh a; put_ bh b
    get bh = do a <- get bh; b <- get bh; return (a :% b)

instance Binary (Bin a) where
  put_ bh (BinPtr i) = put_ bh i
  get bh = do i <- get bh; return (BinPtr i)

-- -----------------------------------------------------------------------------
-- Strings

-- should put a string in UTF-8 (just throws away top 24 bits at the moment)
putString :: BinHandle -> String -> IO ()
putString bh str = put_ bh word8s
 where
  word8s :: [Word8]
  word8s = map (fromIntegral.ord) str

getString :: BinHandle -> IO String
getString bh = do
  word8s <- get bh
  return (map (chr.fromIntegral) (word8s :: [Word8]))

-- -----------------------------------------------------------------------------
-- Lazy reading/writing

lazyPut :: Binary a => BinHandle -> a -> IO ()
lazyPut bh a = do
	-- output the obj with a ptr to skip over it:
    pre_a <- tellBin bh
    put_ bh pre_a	-- save a slot for the ptr
    put_ bh a		-- dump the object
    q <- tellBin bh 	-- q = ptr to after object
    putAt bh pre_a q 	-- fill in slot before a with ptr to q
    seekBin bh q	-- finally carry on writing at q

lazyGet :: Binary a => BinHandle -> IO a
lazyGet bh = do
    p <- get bh		-- a BinPtr
    p_a <- tellBin bh
    a <- unsafeInterleaveIO (getAt bh p_a)
    seekBin bh p -- skip over the object for now
    return a

-- -----------------------------------------------------------------------------
-- FormatVersion's.  
-- The FormatVersion is always non-negative.  Furthermore, if the 
-- FormatVersion is 0, nothing is output.
-- 
-- FormatVersion should only be encoded before something we KNOW to have
-- an encoding which never begins with a negative byte, such as a non-negative
-- integer or a list.
-- 
-- The advantage of this is that we can read a FormatVersion (which will
-- be the nullFormatVersion) even when we didn't write one in the first
-- place, such as from earlier versions of this program, just so long
-- as we did at any rate write a list.

newtype FormatVersion = FormatVersion Int deriving (Eq,Ord)

nullFormatVersion :: FormatVersion
nullFormatVersion = mkFormatVersion 0

mkFormatVersion :: Int -> FormatVersion
mkFormatVersion i = FormatVersion i

instance Binary FormatVersion where
   put_ bh (FormatVersion i) =
      case compare i 0 of
         EQ -> return ()
         GT -> put_ bh (-i)
         LT -> error (
            "Binary.hs: negative FormatVersion " ++ show i 
               ++ " is not allowed")
   get bh =
      do
         w8 <- peekWord8 bh   
         if testBit w8 7
            then
               do
                  i <- get bh
                  return (FormatVersion (-i))
            else
               return nullFormatVersion
