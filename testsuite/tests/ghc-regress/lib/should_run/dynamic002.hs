-- !!! Testing Typeable instances
module Main(main) where

import Dynamic
import Array
import Complex
import IO
import Addr
import ForeignObj
import IOExts
import Int
import ST
import StableName
import StablePtr
import Word
import Exception
import ByteArray
import MutableArray
import PackedString
import Weak
import MArray
import IArray
import CTypes
import CTypesISO

main :: IO ()
main = do
   print (typeOf (undefined :: [()]))
   print (typeOf (undefined :: ()))
   print (typeOf (undefined :: ((),())))
   print (typeOf (undefined :: ((),(),())))
   print (typeOf (undefined :: ((),(),(),())))
   print (typeOf (undefined :: ((),(),(),(),())))
   print (typeOf (undefined :: (() -> ())))
   print (typeOf (undefined :: (Array () ())))
   print (typeOf (undefined :: Bool))
   print (typeOf (undefined :: Char))
   print (typeOf (undefined :: (Complex ())))
   print (typeOf (undefined :: Double))
   print (typeOf (undefined :: (Either () ())))
   print (typeOf (undefined :: Float))
   print (typeOf (undefined :: Handle))
   print (typeOf (undefined :: Int))
   print (typeOf (undefined :: Integer))
   print (typeOf (undefined :: IO ()))
   print (typeOf (undefined :: (Maybe ())))
   print (typeOf (undefined :: Ordering))

   print (typeOf (undefined :: Addr))
   print (typeOf (undefined :: Dynamic))
   print (typeOf (undefined :: ForeignObj))
   print (typeOf (undefined :: (IORef ())))
   print (typeOf (undefined :: Int8))
   print (typeOf (undefined :: Int16))
   print (typeOf (undefined :: Int32))
   print (typeOf (undefined :: Int64))
   print (typeOf (undefined :: (ST () ())))
   print (typeOf (undefined :: (StableName ())))
   print (typeOf (undefined :: (StablePtr ())))
   print (typeOf (undefined :: TyCon))
   print (typeOf (undefined :: TypeRep))
   print (typeOf (undefined :: Word8))
   print (typeOf (undefined :: Word16))
   print (typeOf (undefined :: Word32))
   print (typeOf (undefined :: Word64))

   print (typeOf (undefined :: ArithException))
   print (typeOf (undefined :: AsyncException))
   print (typeOf (undefined :: (ByteArray ())))
   print (typeOf (undefined :: Exception))
   print (typeOf (undefined :: (IOArray () ())))
   print (typeOf (undefined :: (IOUArray () ())))
   print (typeOf (undefined :: (MutableByteArray () ())))
   print (typeOf (undefined :: PackedString))
   print (typeOf (undefined :: (STArray () () ())))
   print (typeOf (undefined :: (STUArray () () ())))
   print (typeOf (undefined :: (StableName ())))
   print (typeOf (undefined :: (StablePtr ())))
   print (typeOf (undefined :: (UArray () ())))
   print (typeOf (undefined :: (Weak ())))

   print (typeOf (undefined :: CChar))
   print (typeOf (undefined :: CSChar))
   print (typeOf (undefined :: CUChar))
   print (typeOf (undefined :: CShort))
   print (typeOf (undefined :: CUShort))
   print (typeOf (undefined :: CInt))
   print (typeOf (undefined :: CUInt))
   print (typeOf (undefined :: CLong))
   print (typeOf (undefined :: CULong))
   print (typeOf (undefined :: CLLong))
   print (typeOf (undefined :: CULLong))
   print (typeOf (undefined :: CFloat))
   print (typeOf (undefined :: CDouble))
   print (typeOf (undefined :: CLDouble))

   print (typeOf (undefined :: CPtrdiff))
   print (typeOf (undefined :: CSize))
   print (typeOf (undefined :: CWchar))
   print (typeOf (undefined :: CSigAtomic))
   print (typeOf (undefined :: CClock))
   print (typeOf (undefined :: CTime))
