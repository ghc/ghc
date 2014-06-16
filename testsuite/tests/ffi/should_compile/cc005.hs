-- !!! cc005 -- foreign export declarations
module ShouldCompile (d8) where

import Foreign.Ptr   --import Foreign
                 --import GlaExts
                 --import Int
                 --import Word
type Addr = FunPtr (Int -> IO ())
foreign import ccall "wrapper" d8  :: (Int -> IO ())  -> IO Addr

-- exporting functions
{-
m_stdcall :: Int -> IO Int
m_stdcall x = return x

x = putChar

foreign export ccall "m1" doo :: Int -> IO Int

doo :: Eq a => a -> IO Int
doo _ = return 2

foreign export ccall "listAppend" plusplus :: StablePtr [a] -> StablePtr [a] -> IO (StablePtr [a])

plusplus :: StablePtr [a] -> StablePtr [a] -> IO (StablePtr [a])
plusplus x y = do
  l1 <- deRefStablePtr x
  l2 <- deRefStablePtr y
  makeStablePtr (l1 ++ l2)

foreign export ccall "m11" m_stdcall :: Int -> IO Int

m_ccall :: Int -> Int -> IO Int
m_ccall x y = return (x-y)

foreign export ccall   "m2" m_ccall   :: Int -> Int -> IO Int

foreign export ccall   "putcha" putChar :: Char -> IO ()

foreign export stdcall "Math" "sin" my_sin :: Double -> IO Double
foreign export stdcall "Math" "cos" my_cos :: Double -> IO Double

my_sin = undefined
my_cos = undefined

foreign export stdcall "m111" m8  :: IO Int8
foreign export stdcall "m22" m16 :: IO Int16
foreign export stdcall "m3" m32 :: IO Int32
foreign export stdcall "m4" m64 :: IO Int64

m8 = undefined
m16 = undefined
m32 = undefined
m64 = undefined

foreign export stdcall dynamic d8  :: (Addr -> IO Int8)  -> IO Addr
foreign export stdcall dynamic d16 :: (Addr -> IO Int16) -> IO Addr
foreign export stdcall dynamic d32 :: (Addr -> IO Int32) -> IO Addr
foreign export stdcall dynamic d64 :: (Addr -> IO Int64) -> IO Addr


d8 = undefined
d16 = undefined
d32 = undefined
d64 = undefined


foreign export ccall "kitchen"
   sink :: --ForeignObj 
--        -> ByteArray Int 
--	-> MutableByteArray Int RealWorld
	   Int
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
	-> IO Int

sink  = undefined
sink2 = undefined

foreign export ccall dynamic
  sink2 :: (--ForeignObj 
--        -> ByteArray Int 
--	-> MutableByteArray Int RealWorld
	   StablePtr a
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
	-> IO ())
	-> IO Addr


-}
