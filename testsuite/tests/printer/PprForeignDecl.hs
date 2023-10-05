{-# LANGUAGE MagicHash, UnliftedFFITypes #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- Based on ghc/testsuite/tests/ffi/should_compile contents
module PprForeignDecl where

import Foreign
import GHC.Exts
import Data.Int
import Data.Word

-- simple functions

foreign import ccall unsafe "a" a :: IO Int

foreign import ccall unsafe "b" b :: Int -> IO Int

foreign import ccall unsafe "c"
  c :: Int -> Char -> Float -> Double -> IO Float

-- simple monadic code

d =     a               >>= \ x ->
        b x             >>= \ y ->
        c y 'f' 1.0 2.0

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

foreign import ccall unsafe "safe_qd.h safe_qd_add" c_qd_add :: Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO ();

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

-- exports
foreign export ccall "plusInt" (+) :: Int -> Int -> Int

listToJSArray :: ToJSRef a => [a] -> IO (JSArray a)
listToJSArray = toJSArray deconstr
        where deconstr (x : xs) = Just (x, xs)
              deconstr [] = Nothing

foreign import javascript unsafe "$r = new Float32Array($1);"
        float32Array :: JSArray Float -> IO Float32Array

foreign import javascript unsafe "$r = new Int32Array($1);"
        int32Array :: JSArray Int32 -> IO Int32Array

foreign import javascript unsafe "$r = new Uint16Array($1);"
        uint16Array :: JSArray Word16 -> IO Uint16Array

foreign import javascript unsafe "$r = new Uint8Array($1);"
        uint8Array :: JSArray Word8 -> IO Uint8Array

foreign import javascript unsafe "$r = $1.getContext(\"webgl\");"
        getCtx :: JSRef a -> IO Ctx
