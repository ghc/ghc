{-# OPTIONS_GHC -fglasgow-exts #-}
-- !!! cc004 -- foreign declarations
module ShouldCompile where

import Foreign
import GHC.Exts
import Data.Int
import Data.Word

-- importing functions

foreign import stdcall        "m" m_stdcall :: StablePtr a -> IO (StablePtr b)
foreign import ccall   unsafe "m" m_ccall   :: ByteArray# -> IO Int

foreign import stdcall "sin" my_sin :: Double -> IO Double
foreign import stdcall "cos" my_cos :: Double -> IO Double

foreign import stdcall "m1" m8  :: IO Int8
foreign import stdcall "m2" m16 :: IO Int16
foreign import stdcall "m3" m32 :: IO Int32
foreign import stdcall "m4" m64 :: IO Int64

foreign import stdcall "dynamic" d8  :: Ptr a -> IO Int8
foreign import stdcall "dynamic" d16 :: Ptr a -> IO Int16
foreign import stdcall "dynamic" d32 :: Ptr a -> IO Int32
foreign import stdcall "dynamic" d64 :: Ptr a -> IO Int64

foreign import ccall unsafe "kitchen"
   sink :: Ptr a
        -> ByteArray#
	-> MutableByteArray# RealWorld
	-> Int
	-> Int8
	-> Int16
	-> Int32
	-> Int64
	-> Word8
	-> Word16
	-> Word32
	-> Word64
	-> Float
	-> Double
	-> IO ()


foreign import ccall unsafe "dynamic"
  sink2 :: Ptr a
        -> (Ptr b
        -> ByteArray#
	-> MutableByteArray# RealWorld
	-> Int
	-> Int8
	-> Int16
	-> Int32
	-> Word8
	-> Word16
	-> Word32
	-> Float
	-> Double
	-> IO ())

