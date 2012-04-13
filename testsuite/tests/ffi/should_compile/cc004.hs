{-# LANGUAGE MagicHash, UnliftedFFITypes #-}
-- !!! cc004 -- foreign declarations
module ShouldCompile where

import Foreign
import GHC.Exts
import Data.Int
import Data.Word

-- importing functions

-- We can't import the same function using both stdcall and ccall
-- calling conventions in the same file when compiling via C (this is a
-- restriction in the C backend caused by the need to emit a prototype
-- for stdcall functions).
foreign import stdcall        "p" m_stdcall :: StablePtr a -> IO (StablePtr b)
foreign import ccall   unsafe "q" m_ccall   :: ByteArray# -> IO Int

-- We can't redefine the calling conventions of certain functions (those from
-- math.h).
foreign import stdcall "my_sin" my_sin :: Double -> IO Double
foreign import stdcall "my_cos" my_cos :: Double -> IO Double

foreign import stdcall "m1" m8  :: IO Int8
foreign import stdcall "m2" m16 :: IO Int16
foreign import stdcall "m3" m32 :: IO Int32
foreign import stdcall "m4" m64 :: IO Int64

foreign import stdcall "dynamic" d8  :: FunPtr (IO Int8) -> IO Int8
foreign import stdcall "dynamic" d16 :: FunPtr (IO Int16) -> IO Int16
foreign import stdcall "dynamic" d32 :: FunPtr (IO Int32) -> IO Int32
foreign import stdcall "dynamic" d64 :: FunPtr (IO Int64) -> IO Int64

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


type Sink2 b = Ptr b
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
            -> IO ()

foreign import ccall unsafe "dynamic"
  sink2 :: Ptr (Sink2 b) -> Sink2 b

