-- !!! cc004 -- foreign declarations
module ShouldCompile where

import Foreign
import GlaExts
import Int
import Word

-- importing functions

foreign import stdcall "m" m_stdcall :: StablePtr a -> IO (StablePtr b)
foreign import ccall   "m" unsafe m_ccall   :: ByteArray Int -> IO Int

foreign import stdcall "Math" "sin" my_sin :: Double -> IO Double
foreign import stdcall "Math" "cos" my_cos :: Double -> IO Double

foreign import stdcall "m1" m8  :: IO Int8
foreign import stdcall "m2" m16 :: IO Int16
foreign import stdcall "m3" m32 :: IO Int32
foreign import stdcall "m4" m64 :: IO Int64

foreign import stdcall dynamic d8  :: Addr -> IO Int8
foreign import stdcall dynamic d16 :: Addr -> IO Int16
foreign import stdcall dynamic d32 :: Addr -> IO Int32
foreign import stdcall dynamic d64 :: Addr -> IO Int64

foreign import ccall "kitchen" unsafe 
   sink :: ForeignObj 
        -> ByteArray Int 
	-> MutableByteArray Int RealWorld
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


foreign import ccall dynamic unsafe
  sink2 :: Addr
        -> (ForeignObj 
        -> ByteArray Int 
	-> MutableByteArray Int RealWorld
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

