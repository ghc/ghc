%
% (c) The AQUA Project, Glasgow University, 1994-1996
%

\section[PrelAddr]{Module @PrelAddr@}

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module PrelAddr (
	Addr(..), 
	nullAddr,			-- :: Addr
	plusAddr,			-- :: Addr -> Int -> Addr
   ) where

import PrelGHC
import PrelBase
import PrelST
import PrelCCall
\end{code}

\begin{code}
data Addr = A# Addr# 	deriving (Eq, Ord)

instance Show Addr where
   showsPrec p (A# a) = showsPrec p (I# (addr2Int# a))

nullAddr = ``NULL'' :: Addr

plusAddr :: Addr -> Int -> Addr
plusAddr (A# addr) (I# off) = A# (int2Addr# (addr2Int# addr +# off))

instance CCallable Addr
instance CCallable Addr#
instance CReturnable Addr
\end{code}

ToDo:

-- read value out of _immutable_ memory
      indexCharOffAddr   :: Addr -> Int -> Char
      indexIntOffAddr    :: Addr -> Int -> Int     -- should we drop this?
      indexAddrOffAddr   :: Addr -> Int -> Addr
      indexFloatOffAddr  :: Addr -> Int -> Float
      indexDoubleOffAddr :: Addr -> Int -> Double
      indexWord8OffAddr  :: Addr -> Int -> Word8
      indexWord16OffAddr :: Addr -> Int -> Word16
      indexWord32OffAddr :: Addr -> Int -> Word32
      indexWord64OffAddr :: Addr -> Int -> Word64
      indexInt8OffAddr   :: Addr -> Int -> Int8
      indexInt16OffAddr  :: Addr -> Int -> Int16
      indexInt32OffAddr  :: Addr -> Int -> Int32
      indexInt64OffAddr  :: Addr -> Int -> Int64

      -- read value out of mutable memory
      readCharOffAddr    :: Addr -> Int -> IO Char
      readIntOffAddr     :: Addr -> Int -> IO Int  -- should we drop this?
      readAddrOffAddr    :: Addr -> Int -> IO Addr
      readFloatOffAddr   :: Addr -> Int -> IO Float
      readDoubleOffAddr  :: Addr -> Int -> IO Double
      readWord8OffAddr   :: Addr -> Int -> IO Word8
      readWord16OffAddr  :: Addr -> Int -> IO Word16
      readWord32OffAddr  :: Addr -> Int -> IO Word32
      readWord64OffAddr  :: Addr -> Int -> IO Word64
      readInt8OffAddr    :: Addr -> Int -> IO Int8
      readInt16OffAddr   :: Addr -> Int -> IO Int16
      readInt32OffAddr   :: Addr -> Int -> IO Int32
      readInt64OffAddr   :: Addr -> Int -> IO Int64

      -- write value into mutable memory
      writeCharOffAddr   :: Addr -> Int -> Char   -> IO ()
      writeIntOffAddr    :: Addr -> Int -> Int    -> IO ()  -- should we drop this?
      writeAddrOffAddr   :: Addr -> Int -> Addr   -> IO ()
      writeFloatOffAddr  :: Addr -> Int -> Float  -> IO ()
      writeDoubleOffAddr :: Addr -> Int -> Double -> IO ()
      writeWord8OffAddr  :: Addr -> Int -> Word8  -> IO ()
      writeWord16OffAddr :: Addr -> Int -> Word16 -> IO ()
      writeWord32OffAddr :: Addr -> Int -> Word32 -> IO ()
      writeWord64OffAddr :: Addr -> Int -> Word64 -> IO ()
      writeInt8OffAddr   :: Addr -> Int -> Int8   -> IO ()
      writeInt16OffAddr  :: Addr -> Int -> Int16  -> IO ()
      writeInt32OffAddr  :: Addr -> Int -> Int32  -> IO ()
      writeInt64OffAddr  :: Addr -> Int -> Int64  -> IO ()

