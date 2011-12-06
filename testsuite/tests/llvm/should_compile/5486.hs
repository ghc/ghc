{-# LANGUAGE ForeignFunctionInterface, UnliftedFFITypes, UnboxedTuples,
             BangPatterns, MagicHash #-}

-- | Test case for Trac #5486
-- Test case reduced from HsOpenSSL package BN module
module Bad where

import           Control.Exception hiding (try)
import           Foreign
import qualified Data.ByteString as BS

import           Foreign.C.Types
import           GHC.Base
import           GHC.Integer.GMP.Internals

newtype BigNum = BigNum (Ptr BIGNUM)
data BIGNUM

data ByteArray = BA  !ByteArray#
data MBA       = MBA !(MutableByteArray# RealWorld)

foreign import ccall unsafe "BN_free"
        _free :: Ptr BIGNUM -> IO ()

foreign import ccall unsafe "BN_bn2mpi"
        _bn2mpi :: Ptr BIGNUM -> Ptr CChar -> IO CInt

foreign import ccall unsafe "memcpy"
        _copy_in :: ByteArray# -> Ptr () -> CSize -> IO ()

foreign import ccall unsafe "memcpy"
        _copy_out :: Ptr () -> ByteArray# -> CSize -> IO ()

unwrapBN :: BigNum -> Ptr BIGNUM
unwrapBN (BigNum p) = p

wrapBN :: Ptr BIGNUM -> BigNum
wrapBN = BigNum

bnToInteger :: BigNum -> IO Integer
bnToInteger bn = do
  nlimbs <- ((\hsc_ptr -> peekByteOff hsc_ptr 8)) (unwrapBN bn) :: IO CInt
  case nlimbs of
    0 -> return 0
    1 -> do (I# i) <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) (unwrapBN bn) >>= peek
            negative <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) (unwrapBN bn) :: IO CInt
            if negative == 0
               then return $ S# i
               else return $ 0 - (S# i)
    _ -> do
      let !(I# nlimbsi) = fromIntegral nlimbs
          !(I# limbsize) = ((8))
      (MBA arr) <- newByteArray (nlimbsi *# limbsize)
      (BA ba) <- freezeByteArray arr
      limbs <- ((\hsc_ptr -> peekByteOff hsc_ptr 0)) (unwrapBN bn)
      _ <- _copy_in ba limbs $ fromIntegral $ nlimbs * ((8))
      negative <- ((\hsc_ptr -> peekByteOff hsc_ptr 16)) (unwrapBN bn) :: IO CInt
      if negative == 0
         then return $ J# nlimbsi ba
         else return $ 0 - (J# nlimbsi ba)

newByteArray :: Int# -> IO MBA
newByteArray sz = IO $ \s ->
  case newByteArray# sz s of { (# s', arr #) ->
  (# s', MBA arr #) }

freezeByteArray :: MutableByteArray# RealWorld -> IO ByteArray
freezeByteArray arr = IO $ \s ->
  case unsafeFreezeByteArray# arr s of { (# s', arr' #) ->
  (# s', BA arr' #) }

integerToBN :: Integer -> IO BigNum
integerToBN (S# 0#) = do
  bnptr <- mallocBytes ((24))
  ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) bnptr nullPtr
  let one :: CInt
      one = 1
      zero :: CInt
      zero = 0
  ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) bnptr one
  ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) bnptr zero
  ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) bnptr zero
  ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) bnptr zero
  return (wrapBN bnptr)

integerToBN (S# v) = do
  bnptr <- mallocBytes ((24))
  limbs <- malloc :: IO (Ptr CULong)
  poke limbs $ fromIntegral $ abs $ I# v
  ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) bnptr limbs
  let one :: CInt
      one = 1
  ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) bnptr one
  ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) bnptr one
  ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) bnptr one
  ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) bnptr (if (I# v) < 0 then one else 0)
  return (wrapBN bnptr)

integerToBN v@(J# nlimbs_ bytearray)
  | v >= 0 = do
      let nlimbs = (I# nlimbs_)
      bnptr <- mallocBytes ((24))
      limbs <- mallocBytes (((8)) * nlimbs)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 0)) bnptr limbs
      ((\hsc_ptr -> pokeByteOff hsc_ptr 20)) bnptr (1 :: CInt)
      _ <- _copy_out limbs bytearray (fromIntegral $ ((8)) * nlimbs)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 8)) bnptr ((fromIntegral nlimbs) :: CInt)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 12)) bnptr ((fromIntegral nlimbs) :: CInt)
      ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) bnptr (0 :: CInt)
      return (wrapBN bnptr)
  | otherwise = do bnptr <- integerToBN (0-v)
                   ((\hsc_ptr -> pokeByteOff hsc_ptr 16)) (unwrapBN bnptr) (1 :: CInt)
                   return bnptr

integerToMPI :: Integer -> IO BS.ByteString
integerToMPI v = bracket (integerToBN v) (_free . unwrapBN) bnToMPI

bnToMPI :: BigNum -> IO BS.ByteString
bnToMPI bn = do
  bytes <- _bn2mpi (unwrapBN bn) nullPtr
  allocaBytes (fromIntegral bytes) (\buffer -> do
    _ <- _bn2mpi (unwrapBN bn) buffer
    BS.packCStringLen (buffer, fromIntegral bytes))

