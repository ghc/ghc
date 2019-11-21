{-# OPTIONS_GHC -O2 -funbox-strict-fields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, BangPatterns, ScopedTypeVariables, CPP #-}

module Binary.Internal (

  -- Safe parts of `Binary.Unsafe`:

  Bin, BinData,

  Put, runPut, runPutIO,
  Get, runGet, runGetIO,

  writeBinData, readBinData, withBinBuffer,

  UserDataP(..), UserDataG(..),
  userDataP    , userDataG,
  writeState   , readState,

  tellP, seekP,
  tellG, seekG, interleaveG, getSlice,

  -- Primitives:

  Dictionary,
  putDictionary, getDictionary,

  SymbolTable,

  putWord8 , getWord8,
  putWord16, getWord16,
  putWord32, getWord32,
  putWord64, getWord64,

  putInt8 , getInt8,
  putInt16, getInt16,
  putInt32, getInt32,
  putInt64, getInt64,

  putULEB128, getULEB128,
  putSLEB128, getSLEB128,

  putByte, getByte,

  putByteString, getByteString,
  putFS, getFS,
  putAFastString, getAFastString,

  putNonBindingName, putBindingName,
  getAName,

  putBin, getBin,

  putInt, getInt,

) where

import Binary.Unsafe

import FastString
import GhcPrelude
import {-# SOURCE #-} Name (Name)

#if defined(DEBUG)
import PlainPanic
#endif

import Control.Monad

import Data.Array
import UniqFM

import Foreign
import Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Unsafe

-- -----------------------------------------------------------------------------
-- Byte
-- -----------------------------------------------------------------------------

putByte :: Word8 -> Put ()
putByte !w = putWord8 w

getByte :: Get Word8
getByte = getWord8

-- -----------------------------------------------------------------------------
-- Word
-- -----------------------------------------------------------------------------

putWord8 :: Word8 -> Put ()
putWord8 !w = putPrim 1 (\op -> poke op w)

getWord8 :: Get Word8
getWord8 = getPrim 1 peek

putWord16 :: Word16 -> Put ()
putWord16 w = putPrim 2 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 8))
  pokeElemOff op 1 (fromIntegral (w .&. 0xFF))
  )

getWord16 :: Get Word16
getWord16 = getPrim 2 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  return $! w0 `shiftL` 8 .|. w1
  )

putWord32 :: Word32 -> Put ()
putWord32 w = putPrim 4 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 24))
  pokeElemOff op 1 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
  pokeElemOff op 2 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
  pokeElemOff op 3 (fromIntegral (w .&. 0xFF))
  )

getWord32 :: Get Word32
getWord32 = getPrim 4 (\op -> do
  w0 <- fromIntegral <$> peekElemOff op 0
  w1 <- fromIntegral <$> peekElemOff op 1
  w2 <- fromIntegral <$> peekElemOff op 2
  w3 <- fromIntegral <$> peekElemOff op 3

  return $! (w0 `shiftL` 24) .|.
            (w1 `shiftL` 16) .|.
            (w2 `shiftL` 8)  .|.
            w3
  )

putWord64 :: Word64 -> Put ()
putWord64 w = putPrim 8 (\op -> do
  pokeElemOff op 0 (fromIntegral (w `shiftR` 56))
  pokeElemOff op 1 (fromIntegral ((w `shiftR` 48) .&. 0xFF))
  pokeElemOff op 2 (fromIntegral ((w `shiftR` 40) .&. 0xFF))
  pokeElemOff op 3 (fromIntegral ((w `shiftR` 32) .&. 0xFF))
  pokeElemOff op 4 (fromIntegral ((w `shiftR` 24) .&. 0xFF))
  pokeElemOff op 5 (fromIntegral ((w `shiftR` 16) .&. 0xFF))
  pokeElemOff op 6 (fromIntegral ((w `shiftR` 8) .&. 0xFF))
  pokeElemOff op 7 (fromIntegral (w .&. 0xFF))
  )

getWord64 :: Get Word64
getWord64 = getPrim 8 (\op -> do
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

-- -----------------------------------------------------------------------------
-- Fixed-size Ints
-- -----------------------------------------------------------------------------

putInt8 :: Int8 -> Put ()
putInt8 = putWord8 . fromIntegral

getInt8 :: Get Int8
getInt8 = (fromIntegral $!) <$> getWord8

putInt16 :: Int16 -> Put ()
putInt16 = putSLEB128

getInt16 :: Get Int16
getInt16 = getSLEB128

putInt32 :: Int32 -> Put ()
putInt32 = putSLEB128

getInt32 :: Get Int32
getInt32 = getSLEB128

putInt64 :: Int64 -> Put ()
putInt64 = putSLEB128

getInt64 :: Get Int64
getInt64 = getSLEB128

-- -----------------------------------------------------------------------------
-- Bin
-- -----------------------------------------------------------------------------

putBin :: Bin a -> Put ()
putBin (BinPtr !p) = putWord32 (fromIntegral p :: Word32)

getBin :: Get (Bin a)
getBin = BinPtr . fromIntegral <$> getWord32

-- -----------------------------------------------------------------------------
-- ByteString
-- -----------------------------------------------------------------------------

putByteString :: ByteString -> Put ()
putByteString bs = do
  let len = BS.length bs
  putInt len
  putPrim len $ \op ->
    unsafeUseAsCString bs $ \ptr ->
      memcpy op (castPtr ptr) len

getByteString :: Get ByteString
getByteString = do
  len <- getInt
  let l = fromIntegral len
  getPrim l $ \src ->
    create l $ \dest ->
      memcpy dest src l

-- -----------------------------------------------------------------------------
-- FastString and Name
-- -----------------------------------------------------------------------------

putAFastString :: FastString -> Put ()
putAFastString fs = put_fs <$!> userDataP >>= ($! fs)

getAFastString :: Get FastString
getAFastString = get_fs =<< userDataG

putFS :: FastString -> Put ()
putFS = putByteString . bytesFS

getFS :: Get FastString
getFS = do
  l <- getInt
  getPrim l (\src -> pure $! mkFastStringBytes src l)

putNonBindingName :: Name -> Put ()
putNonBindingName n = put_nonbinding_name <$> userDataP >>= ($ n)

putBindingName :: Name -> Put ()
putBindingName n = put_binding_name <$> userDataP >>= ($ n)

getAName :: Get Name
getAName = get_name =<< userDataG

-- -----------------------------------------------------------------------------
-- Signed and unsigned LEB128
-- -----------------------------------------------------------------------------

{-# SPECIALISE putULEB128 :: Word   -> Put () #-}
{-# SPECIALISE putULEB128 :: Word64 -> Put () #-}
{-# SPECIALISE putULEB128 :: Word32 -> Put () #-}
{-# SPECIALISE putULEB128 :: Word16 -> Put () #-}
{-# SPECIALISE putULEB128 :: Int    -> Put () #-}
{-# SPECIALISE putULEB128 :: Int64  -> Put () #-}
{-# SPECIALISE putULEB128 :: Int32  -> Put () #-}
{-# SPECIALISE putULEB128 :: Int16  -> Put () #-}
putULEB128 :: forall a. (Integral a, FiniteBits a) => a -> Put ()
putULEB128 w =
#if defined(DEBUG)
    (if w < 0 then panic "putULEB128: Signed number" else id) $
#endif
    go w
  where
    go :: a -> Put ()
    go w
      | w <= (127 :: a)
      = putByte (fromIntegral w :: Word8)
      | otherwise = do
        -- bit 7 (8th bit) indicates more to come.
        let !byte = setBit (fromIntegral w) 7 :: Word8
        putByte byte
        go (w `unsafeShiftR` 7)

{-# SPECIALISE getULEB128 :: Get Word   #-}
{-# SPECIALISE getULEB128 :: Get Word64 #-}
{-# SPECIALISE getULEB128 :: Get Word32 #-}
{-# SPECIALISE getULEB128 :: Get Word16 #-}
{-# SPECIALISE getULEB128 :: Get Int    #-}
{-# SPECIALISE getULEB128 :: Get Int64  #-}
{-# SPECIALISE getULEB128 :: Get Int32  #-}
{-# SPECIALISE getULEB128 :: Get Int16  #-}
getULEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getULEB128 =
    go 0 0
  where
    go :: Int -> a -> Get a
    go shift w = do
        b <- getByte
        let !hasMore = testBit b 7
        let !val = w .|. ((clearBit (fromIntegral b) 7) `unsafeShiftL` shift) :: a
        if hasMore
            then do
                go (shift+7) val
            else
                return $! val

{-# SPECIALISE putSLEB128 :: Word   -> Put () #-}
{-# SPECIALISE putSLEB128 :: Word64 -> Put () #-}
{-# SPECIALISE putSLEB128 :: Word32 -> Put () #-}
{-# SPECIALISE putSLEB128 :: Word16 -> Put () #-}
{-# SPECIALISE putSLEB128 :: Int    -> Put () #-}
{-# SPECIALISE putSLEB128 :: Int64  -> Put () #-}
{-# SPECIALISE putSLEB128 :: Int32  -> Put () #-}
{-# SPECIALISE putSLEB128 :: Int16  -> Put () #-}
putSLEB128 :: forall a. (Integral a, FiniteBits a) => a -> Put ()
putSLEB128 initial = go initial
  where
    go :: a -> Put ()
    go val = do
        let !byte = fromIntegral (clearBit val 7) :: Word8
        let !val' = val `unsafeShiftR` 7
        let !signBit = testBit byte 6
        let !done =
                -- Unsigned value, val' == 0 and and last value can
                -- be discriminated from a negative number.
                ((val' == 0 && not signBit) ||
                -- Signed value,
                 (val' == -1 && signBit))

        let !byte' = if done then byte else setBit byte 7
        putByte byte'

        unless done $ go val'

{-# SPECIALISE getSLEB128 :: Get Word   #-}
{-# SPECIALISE getSLEB128 :: Get Word64 #-}
{-# SPECIALISE getSLEB128 :: Get Word32 #-}
{-# SPECIALISE getSLEB128 :: Get Word16 #-}
{-# SPECIALISE getSLEB128 :: Get Int    #-}
{-# SPECIALISE getSLEB128 :: Get Int64  #-}
{-# SPECIALISE getSLEB128 :: Get Int32  #-}
{-# SPECIALISE getSLEB128 :: Get Int16  #-}
getSLEB128 :: forall a. (Integral a, FiniteBits a) => Get a
getSLEB128 = do
    (val,shift,signed) <- go 0 0
    if signed && (shift < finiteBitSize val)
        then return $! ((complement 0 `unsafeShiftL` shift) .|. val)
        else return val
    where
        go :: Int -> a -> Get (a, Int, Bool)
        go shift val = do
            byte <- getByte
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
-- Standard types
-- -----------------------------------------------------------------------------

putInt :: Int -> Put ()
putInt i = putSLEB128 (fromIntegral i :: Int64)

getInt :: Get Int
getInt = (fromIntegral $!) <$> (getSLEB128 :: Get Int64)

--------------------------------------------------------------------------------
-- The Dictionary
--------------------------------------------------------------------------------

type Dictionary = Array Int FastString -- The dictionary
                                       -- Should be 0-indexed

putDictionary :: Int -> UniqFM (Int,FastString) -> Put ()
putDictionary sz dict = do
  putInt sz
  mapM_ putFS (elems (array (0,sz-1) (nonDetEltsUFM dict)))
    -- It's OK to use nonDetEltsUFM here because the elements have indices
    -- that array uses to create order

getDictionary :: Get Dictionary
getDictionary = do
  sz <- getInt
  elems <- sequence (GhcPrelude.take sz (repeat getFS))
  return (listArray (0,sz-1) elems)

--------------------------------------------------------------------------------
-- The Symbol Table
--------------------------------------------------------------------------------

-- On disk, the symbol table is an array of IfExtName, when
-- reading it in we turn it into a SymbolTable.

type SymbolTable = Array Int Name
