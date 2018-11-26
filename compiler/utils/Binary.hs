{-# LANGUAGE CPP, MagicHash, UnboxedTuples #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}

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

module Binary
  ( {-type-}  Bin,
    {-class-} Binary(..),
    {-type-}  BinHandle,
    SymbolTable, Dictionary,

   openBinMem,
--   closeBin,

   seekBin,
   seekBy,
   tellBin,
   castBin,
   isEOFBin,
   withBinBuffer,

   writeBinMem,
   readBinMem,

   putAt, getAt,

   -- * For writing instances
   putByte,
   getByte,

   -- * Lazy Binary I/O
   lazyGet,
   lazyPut,

   -- * User data
   UserData(..), getUserData, setUserData,
   newReadState, newWriteState,
   putDictionary, getDictionary, putFS,
  ) where

#include "HsVersions.h"

-- The *host* architecture version:
#include "../includes/MachDeps.h"

import GhcPrelude

import {-# SOURCE #-} Name (Name)
import FastString
import Panic
import UniqFM
import FastMutInt
import Fingerprint
import BasicTypes
import SrcLoc

import Foreign
import Data.Array
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe   as BS
import Data.IORef
import Data.Char                ( ord, chr )
import Data.Time
import Type.Reflection
import Type.Reflection.Unsafe
import Data.Kind (Type)
import GHC.Exts (TYPE, RuntimeRep(..), VecCount(..), VecElem(..))
import Control.Monad            ( when )
import System.IO as IO
import System.IO.Unsafe         ( unsafeInterleaveIO )
import System.IO.Error          ( mkIOError, eofErrorType )
import GHC.Real                 ( Ratio(..) )
import GHC.Serialized

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

getUserData :: BinHandle -> UserData
getUserData bh = bh_usr bh

setUserData :: BinHandle -> UserData -> BinHandle
setUserData bh us = bh { bh_usr = us }

-- | Get access to the underlying buffer.
--
-- It is quite important that no references to the 'ByteString' leak out of the
-- continuation lest terrible things happen.
withBinBuffer :: BinHandle -> (ByteString -> IO a) -> IO a
withBinBuffer (BinMem _ ix_r _ arr_r) action = do
  arr <- readIORef arr_r
  ix <- readFastMutInt ix_r
  withForeignPtr arr $ \ptr ->
    BS.unsafePackCStringLen (castPtr ptr, ix) >>= action


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
tellBin (BinMem _ r _ _) = do ix <- readFastMutInt r; return (BinPtr ix)

seekBin :: BinHandle -> Bin a -> IO ()
seekBin h@(BinMem _ ix_r sz_r _) (BinPtr p) = do
  sz <- readFastMutInt sz_r
  if (p >= sz)
        then do expandBin h p; writeFastMutInt ix_r p
        else writeFastMutInt ix_r p

seekBy :: BinHandle -> Int -> IO ()
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

writeBinMem :: BinHandle -> FilePath -> IO ()
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
  arr <- mallocForeignPtrBytes filesize
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

-- -----------------------------------------------------------------------------
-- Low-level reading/writing of bytes

putPrim :: BinHandle -> Int -> (Ptr Word8 -> IO ()) -> IO ()
putPrim h@(BinMem _ ix_r sz_r arr_r) size f = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  when (ix + size > sz) $
    expandBin h (ix + size)
  arr <- readIORef arr_r
  withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ix_r (ix + size)

getPrim :: BinHandle -> Int -> (Ptr Word8 -> IO a) -> IO a
getPrim (BinMem _ ix_r sz_r arr_r) size f = do
  ix <- readFastMutInt ix_r
  sz <- readFastMutInt sz_r
  when (ix + size > sz) $
      ioError (mkIOError eofErrorType "Data.Binary.getPrim" Nothing Nothing)
  arr <- readIORef arr_r
  w <- withForeignPtr arr $ \op -> f (op `plusPtr` ix)
  writeFastMutInt ix_r (ix + size)
  return w

putWord8 :: BinHandle -> Word8 -> IO ()
putWord8 h w = putPrim h 1 (\op -> poke op w)

getWord8 :: BinHandle -> IO Word8
getWord8 h = getPrim h 1 peek

putWord16 :: BinHandle -> Word16 -> IO ()
putWord16 h w = putPrim h 2 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 8))
  pokeElemOff op 1 (fromIntegral (w .&. 0xFF))
  )

getWord16 :: BinHandle -> IO Word16
getWord16 h = getPrim h 2 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  return $! w0 `shiftL` 8 .|. w1
  )

putWord32 :: BinHandle -> Word32 -> IO ()
putWord32 h w = putPrim h 4 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 24))
  pokeElemOff op 1 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
  pokeElemOff op 2 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
  pokeElemOff op 3 (fromIntegral (w .&. 0xFF))
  )

getWord32 :: BinHandle -> IO Word32
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

putWord64 :: BinHandle -> Word64 -> IO ()
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

getWord64 :: BinHandle -> IO Word64
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

putByte :: BinHandle -> Word8 -> IO ()
putByte bh w = putWord8 bh w

getByte :: BinHandle -> IO Word8
getByte h = getWord8 h

-- -----------------------------------------------------------------------------
-- Primitive Word writes

instance Binary Word8 where
  put_ = putWord8
  get  = getWord8

instance Binary Word16 where
  put_ h w = putWord16 h w
  get h = getWord16 h

instance Binary Word32 where
  put_ h w = putWord32 h w
  get h = getWord32 h

instance Binary Word64 where
  put_ h w = putWord64 h w
  get h = getWord64 h

-- -----------------------------------------------------------------------------
-- Primitive Int writes

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

--to quote binary-0.3 on this code idea,
--
-- TODO  This instance is not architecture portable.  GMP stores numbers as
-- arrays of machine sized words, so the byte format is not portable across
-- architectures with different endianness and word size.
--
-- This makes it hard (impossible) to make an equivalent instance
-- with code that is compilable with non-GHC.  Do we need any instance
-- Binary Integer, and if so, does it have to be blazing fast?  Or can
-- we just change this instance to be portable like the rest of the
-- instances? (binary package has code to steal for that)
--
-- yes, we need Binary Integer and Binary Rational in basicTypes/Literal.hs

instance Binary Integer where
    put_ bh i
      | i >= lo32 && i <= hi32 = do
          putWord8 bh 0
          put_ bh (fromIntegral i :: Int32)
      | otherwise = do
          putWord8 bh 1
          put_ bh (show i)
      where
        lo32 = fromIntegral (minBound :: Int32)
        hi32 = fromIntegral (maxBound :: Int32)

    get bh = do
      int_kind <- getWord8 bh
      case int_kind of
        0 -> fromIntegral <$> (get bh :: IO Int32)
        _ -> do str <- get bh
                case reads str of
                  [(i, "")] -> return i
                  _ -> fail ("Binary integer: got " ++ show str)

    {-
    -- This code is currently commented out.
    -- See https://ghc.haskell.org/trac/ghc/ticket/3379#comment:10 for
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

instance Binary (Bin a) where
  put_ bh (BinPtr i) = put_ bh (fromIntegral i :: Int32)
  get bh = do i <- get bh; return (BinPtr (fromIntegral (i :: Int32)))

-- -----------------------------------------------------------------------------
-- Instances for Data.Typeable stuff

instance Binary TyCon where
    put_ bh tc = do
        put_ bh (tyConPackage tc)
        put_ bh (tyConModule tc)
        put_ bh (tyConName tc)
        put_ bh (tyConKindArgs tc)
        put_ bh (tyConKindRep tc)
    get bh =
        mkTyCon <$> get bh <*> get bh <*> get bh <*> get bh <*> get bh

instance Binary VecCount where
    put_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary VecElem where
    put_ bh = putByte bh . fromIntegral . fromEnum
    get bh = toEnum . fromIntegral <$> getByte bh

instance Binary RuntimeRep where
    put_ bh (VecRep a b)    = putByte bh 0 >> put_ bh a >> put_ bh b
    put_ bh (TupleRep reps) = putByte bh 1 >> put_ bh reps
    put_ bh (SumRep reps)   = putByte bh 2 >> put_ bh reps
    put_ bh LiftedRep       = putByte bh 3
    put_ bh UnliftedRep     = putByte bh 4
    put_ bh IntRep          = putByte bh 5
    put_ bh WordRep         = putByte bh 6
    put_ bh Int64Rep        = putByte bh 7
    put_ bh Word64Rep       = putByte bh 8
    put_ bh AddrRep         = putByte bh 9
    put_ bh FloatRep        = putByte bh 10
    put_ bh DoubleRep       = putByte bh 11
#if __GLASGOW_HASKELL__ >= 807
    put_ bh Int8Rep         = putByte bh 12
    put_ bh Word8Rep        = putByte bh 13
    put_ bh Int16Rep        = putByte bh 14
    put_ bh Word16Rep       = putByte bh 15
#endif

    get bh = do
        tag <- getByte bh
        case tag of
          0  -> VecRep <$> get bh <*> get bh
          1  -> TupleRep <$> get bh
          2  -> SumRep <$> get bh
          3  -> pure LiftedRep
          4  -> pure UnliftedRep
          5  -> pure IntRep
          6  -> pure WordRep
          7  -> pure Int64Rep
          8  -> pure Word64Rep
          9  -> pure AddrRep
          10 -> pure FloatRep
          11 -> pure DoubleRep
#if __GLASGOW_HASKELL__ >= 807
          12 -> pure Int8Rep
          13 -> pure Word8Rep
          14 -> pure Int16Rep
          15 -> pure Word16Rep
#endif
          _  -> fail "Binary.putRuntimeRep: invalid tag"

instance Binary KindRep where
    put_ bh (KindRepTyConApp tc k) = putByte bh 0 >> put_ bh tc >> put_ bh k
    put_ bh (KindRepVar bndr) = putByte bh 1 >> put_ bh bndr
    put_ bh (KindRepApp a b) = putByte bh 2 >> put_ bh a >> put_ bh b
    put_ bh (KindRepFun a b) = putByte bh 3 >> put_ bh a >> put_ bh b
    put_ bh (KindRepTYPE r) = putByte bh 4 >> put_ bh r
    put_ bh (KindRepTypeLit sort r) = putByte bh 5 >> put_ bh sort >> put_ bh r

    get bh = do
        tag <- getByte bh
        case tag of
          0 -> KindRepTyConApp <$> get bh <*> get bh
          1 -> KindRepVar <$> get bh
          2 -> KindRepApp <$> get bh <*> get bh
          3 -> KindRepFun <$> get bh <*> get bh
          4 -> KindRepTYPE <$> get bh
          5 -> KindRepTypeLit <$> get bh <*> get bh
          _ -> fail "Binary.putKindRep: invalid tag"

instance Binary TypeLitSort where
    put_ bh TypeLitSymbol = putByte bh 0
    put_ bh TypeLitNat = putByte bh 1
    get bh = do
        tag <- getByte bh
        case tag of
          0 -> pure TypeLitSymbol
          1 -> pure TypeLitNat
          _ -> fail "Binary.putTypeLitSort: invalid tag"

putTypeRep :: BinHandle -> TypeRep a -> IO ()
-- Special handling for TYPE, (->), and RuntimeRep due to recursive kind
-- relations.
-- See Note [Mutually recursive representations of primitive types]
putTypeRep bh rep
  | Just HRefl <- rep `eqTypeRep` (typeRep :: TypeRep Type)
  = put_ bh (0 :: Word8)
putTypeRep bh (Con' con ks) = do
    put_ bh (1 :: Word8)
    put_ bh con
    put_ bh ks
putTypeRep bh (App f x) = do
    put_ bh (2 :: Word8)
    putTypeRep bh f
    putTypeRep bh x
putTypeRep bh (Fun arg res) = do
    put_ bh (3 :: Word8)
    putTypeRep bh arg
    putTypeRep bh res
putTypeRep _ _ = fail "Binary.putTypeRep: Impossible"

getSomeTypeRep :: BinHandle -> IO SomeTypeRep
getSomeTypeRep bh = do
    tag <- get bh :: IO Word8
    case tag of
        0 -> return $ SomeTypeRep (typeRep :: TypeRep Type)
        1 -> do con <- get bh :: IO TyCon
                ks <- get bh :: IO [SomeTypeRep]
                return $ SomeTypeRep $ mkTrCon con ks

        2 -> do SomeTypeRep f <- getSomeTypeRep bh
                SomeTypeRep x <- getSomeTypeRep bh
                case typeRepKind f of
                  Fun arg res ->
                      case arg `eqTypeRep` typeRepKind x of
                        Just HRefl ->
                            case typeRepKind res `eqTypeRep` (typeRep :: TypeRep Type) of
                              Just HRefl -> return $ SomeTypeRep $ mkTrApp f x
                              _ -> failure "Kind mismatch in type application" []
                        _ -> failure "Kind mismatch in type application"
                             [ "    Found argument of kind: " ++ show (typeRepKind x)
                             , "    Where the constructor:  " ++ show f
                             , "    Expects kind:           " ++ show arg
                             ]
                  _ -> failure "Applied non-arrow"
                       [ "    Applied type: " ++ show f
                       , "    To argument:  " ++ show x
                       ]
        3 -> do SomeTypeRep arg <- getSomeTypeRep bh
                SomeTypeRep res <- getSomeTypeRep bh
                if
                  | App argkcon _ <- typeRepKind arg
                  , App reskcon _ <- typeRepKind res
                  , Just HRefl <- argkcon `eqTypeRep` tYPErep
                  , Just HRefl <- reskcon `eqTypeRep` tYPErep
                  -> return $ SomeTypeRep $ Fun arg res
                  | otherwise -> failure "Kind mismatch" []
        _ -> failure "Invalid SomeTypeRep" []
  where
    tYPErep :: TypeRep TYPE
    tYPErep = typeRep

    failure description info =
        fail $ unlines $ [ "Binary.getSomeTypeRep: "++description ]
                      ++ map ("    "++) info

instance Typeable a => Binary (TypeRep (a :: k)) where
    put_ = putTypeRep
    get bh = do
        SomeTypeRep rep <- getSomeTypeRep bh
        case rep `eqTypeRep` expected of
            Just HRefl -> pure rep
            Nothing    -> fail $ unlines
                               [ "Binary: Type mismatch"
                               , "    Deserialized type: " ++ show rep
                               , "    Expected type:     " ++ show expected
                               ]
     where expected = typeRep :: TypeRep a

instance Binary SomeTypeRep where
    put_ bh (SomeTypeRep rep) = putTypeRep bh rep
    get = getSomeTypeRep

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

-- | Information we keep around during interface file
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
--   [Symbol table representation of names] in BinIface).
--
-- * During fingerprinting a binding Name is serialized as the OccName and a
--   non-binding Name is serialized as the fingerprint of the thing they
--   represent. See Note [Fingerprinting IfaceDecls] for further discussion.
--
data UserData =
   UserData {
        -- for *deserialising* only:
        ud_get_name :: BinHandle -> IO Name,
        ud_get_fs   :: BinHandle -> IO FastString,

        -- for *serialising* only:
        ud_put_nonbinding_name :: BinHandle -> Name -> IO (),
        -- ^ serialize a non-binding 'Name' (e.g. a reference to another
        -- binding).
        ud_put_binding_name :: BinHandle -> Name -> IO (),
        -- ^ serialize a binding 'Name' (e.g. the name of an IfaceDecl)
        ud_put_fs   :: BinHandle -> FastString -> IO ()
   }

newReadState :: (BinHandle -> IO Name)   -- ^ how to deserialize 'Name's
             -> (BinHandle -> IO FastString)
             -> UserData
newReadState get_name get_fs
  = UserData { ud_get_name = get_name,
               ud_get_fs   = get_fs,
               ud_put_nonbinding_name = undef "put_nonbinding_name",
               ud_put_binding_name    = undef "put_binding_name",
               ud_put_fs   = undef "put_fs"
             }

newWriteState :: (BinHandle -> Name -> IO ())
                 -- ^ how to serialize non-binding 'Name's
              -> (BinHandle -> Name -> IO ())
                 -- ^ how to serialize binding 'Name's
              -> (BinHandle -> FastString -> IO ())
              -> UserData
newWriteState put_nonbinding_name put_binding_name put_fs
  = UserData { ud_get_name = undef "get_name",
               ud_get_fs   = undef "get_fs",
               ud_put_nonbinding_name = put_nonbinding_name,
               ud_put_binding_name    = put_binding_name,
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
  mapM_ (putFS bh) (elems (array (0,sz-1) (nonDetEltsUFM dict)))
    -- It's OK to use nonDetEltsUFM here because the elements have indices
    -- that array uses to create order

getDictionary :: BinHandle -> IO Dictionary
getDictionary bh = do
  sz <- get bh
  elems <- sequence (take sz (repeat (getFS bh)))
  return (listArray (0,sz-1) elems)

---------------------------------------------------------
-- The Symbol Table
---------------------------------------------------------

-- On disk, the symbol table is an array of IfExtName, when
-- reading it in we turn it into a SymbolTable.

type SymbolTable = Array Int Name

---------------------------------------------------------
-- Reading and writing FastStrings
---------------------------------------------------------

putFS :: BinHandle -> FastString -> IO ()
putFS bh fs = putBS bh $ fastStringToByteString fs

getFS :: BinHandle -> IO FastString
getFS bh = do
  l  <- get bh :: IO Int
  getPrim bh l (\src -> pure $! mkFastStringBytes src l )

putBS :: BinHandle -> ByteString -> IO ()
putBS bh bs =
  BS.unsafeUseAsCStringLen bs $ \(ptr, l) -> do
    put_ bh l
    putPrim bh l (\op -> BS.memcpy op (castPtr ptr) l)

getBS :: BinHandle -> IO ByteString
getBS bh = do
  l <- get bh :: IO Int
  BS.create l $ \dest -> do
    getPrim bh l (\src -> BS.memcpy dest src l)

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
instance Binary LeftOrRight where
   put_ bh CLeft  = putByte bh 0
   put_ bh CRight = putByte bh 1

   get bh = do { h <- getByte bh
               ; case h of
                   0 -> return CLeft
                   _ -> return CRight }

instance Binary PromotionFlag where
   put_ bh NotPromoted = putByte bh 0
   put_ bh IsPromoted  = putByte bh 1

   get bh = do
       n <- getByte bh
       case n of
         0 -> return NotPromoted
         1 -> return IsPromoted
         _ -> fail "Binary(IsPromoted): fail)"

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
    put_ bh (ActiveBefore src aa) = do
            putByte bh 2
            put_ bh src
            put_ bh aa
    put_ bh (ActiveAfter src ab) = do
            putByte bh 3
            put_ bh src
            put_ bh ab
    get bh = do
            h <- getByte bh
            case h of
              0 -> do return NeverActive
              1 -> do return AlwaysActive
              2 -> do src <- get bh
                      aa <- get bh
                      return (ActiveBefore src aa)
              _ -> do src <- get bh
                      ab <- get bh
                      return (ActiveAfter src ab)

instance Binary InlinePragma where
    put_ bh (InlinePragma s a b c d) = do
            put_ bh s
            put_ bh a
            put_ bh b
            put_ bh c
            put_ bh d

    get bh = do
           s <- get bh
           a <- get bh
           b <- get bh
           c <- get bh
           d <- get bh
           return (InlinePragma s a b c d)

instance Binary RuleMatchInfo where
    put_ bh FunLike = putByte bh 0
    put_ bh ConLike = putByte bh 1
    get bh = do
            h <- getByte bh
            if h == 1 then return ConLike
                      else return FunLike

instance Binary InlineSpec where
    put_ bh NoUserInline    = putByte bh 0
    put_ bh Inline          = putByte bh 1
    put_ bh Inlinable       = putByte bh 2
    put_ bh NoInline        = putByte bh 3

    get bh = do h <- getByte bh
                case h of
                  0 -> return NoUserInline
                  1 -> return Inline
                  2 -> return Inlinable
                  _ -> return NoInline

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

instance Binary OverlapMode where
    put_ bh (NoOverlap    s) = putByte bh 0 >> put_ bh s
    put_ bh (Overlaps     s) = putByte bh 1 >> put_ bh s
    put_ bh (Incoherent   s) = putByte bh 2 >> put_ bh s
    put_ bh (Overlapping  s) = putByte bh 3 >> put_ bh s
    put_ bh (Overlappable s) = putByte bh 4 >> put_ bh s
    get bh = do
        h <- getByte bh
        case h of
            0 -> (get bh) >>= \s -> return $ NoOverlap s
            1 -> (get bh) >>= \s -> return $ Overlaps s
            2 -> (get bh) >>= \s -> return $ Incoherent s
            3 -> (get bh) >>= \s -> return $ Overlapping s
            4 -> (get bh) >>= \s -> return $ Overlappable s
            _ -> panic ("get OverlapMode" ++ show h)


instance Binary OverlapFlag where
    put_ bh flag = do put_ bh (overlapMode flag)
                      put_ bh (isSafeOverlap flag)
    get bh = do
        h <- get bh
        b <- get bh
        return OverlapFlag { overlapMode = h, isSafeOverlap = b }

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
    put_ bh (Fixity src aa ab) = do
            put_ bh src
            put_ bh aa
            put_ bh ab
    get bh = do
          src <- get bh
          aa <- get bh
          ab <- get bh
          return (Fixity src aa ab)

instance Binary WarningTxt where
    put_ bh (WarningTxt s w) = do
            putByte bh 0
            put_ bh s
            put_ bh w
    put_ bh (DeprecatedTxt s d) = do
            putByte bh 1
            put_ bh s
            put_ bh d

    get bh = do
            h <- getByte bh
            case h of
              0 -> do s <- get bh
                      w <- get bh
                      return (WarningTxt s w)
              _ -> do s <- get bh
                      d <- get bh
                      return (DeprecatedTxt s d)

instance Binary StringLiteral where
  put_ bh (StringLiteral st fs) = do
            put_ bh st
            put_ bh fs
  get bh = do
            st <- get bh
            fs <- get bh
            return (StringLiteral st fs)

instance Binary a => Binary (Located a) where
    put_ bh (L l x) = do
            put_ bh l
            put_ bh x

    get bh = do
            l <- get bh
            x <- get bh
            return (L l x)

instance Binary SrcSpan where
  put_ bh (RealSrcSpan ss) = do
          putByte bh 0
          put_ bh (srcSpanFile ss)
          put_ bh (srcSpanStartLine ss)
          put_ bh (srcSpanStartCol ss)
          put_ bh (srcSpanEndLine ss)
          put_ bh (srcSpanEndCol ss)

  put_ bh (UnhelpfulSpan s) = do
          putByte bh 1
          put_ bh s

  get bh = do
          h <- getByte bh
          case h of
            0 -> do f <- get bh
                    sl <- get bh
                    sc <- get bh
                    el <- get bh
                    ec <- get bh
                    return (mkSrcSpan (mkSrcLoc f sl sc)
                                      (mkSrcLoc f el ec))
            _ -> do s <- get bh
                    return (UnhelpfulSpan s)

instance Binary Serialized where
    put_ bh (Serialized the_type bytes) = do
        put_ bh the_type
        put_ bh bytes
    get bh = do
        the_type <- get bh
        bytes <- get bh
        return (Serialized the_type bytes)

instance Binary SourceText where
  put_ bh NoSourceText = putByte bh 0
  put_ bh (SourceText s) = do
        putByte bh 1
        put_ bh s

  get bh = do
    h <- getByte bh
    case h of
      0 -> return NoSourceText
      1 -> do
        s <- get bh
        return (SourceText s)
      _ -> panic $ "Binary SourceText:" ++ show h
