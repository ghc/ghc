{-# LANGUAGE FlexibleContexts, MagicHash, ScopedTypeVariables #-}
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary (Binary, get, put)
import Data.Array.Byte
import Data.Array.Unboxed as AU
import Data.Array.IO (IOUArray)
import Data.Array.MArray (MArray)
import Data.Array as A
import Data.Array.Base as A
import Foreign.Storable
import GHCi.BinaryArray
import GHCi.ResolvedBCO
import GHC.Word

roundtripTest :: (IArray UArray a, MArray IOUArray a IO, Eq a)
              => UArray Int a -> IO ()
roundtripTest arr =
    let ser  = Data.Binary.Put.runPut $ putArray arr
    in case Data.Binary.Get.runGetOrFail getArray ser of
         Right (_, _, arr')
           | arr == arr'  -> return ()
           | otherwise    -> putStrLn "failed to round-trip"
         Left _           -> putStrLn "deserialization failed"

-- See Note [BCOByteArray serialization]
roundtripTestByteArray :: forall a . (IArray UArray a, MArray IOUArray a IO, Eq a, Binary a, Storable a)
              => UArray Int a -> IO ()
roundtripTestByteArray (UArray _ _ _ arr#) =
    let val  = BCOByteArray arr# :: BCOByteArray a
        ser  = Data.Binary.Put.runPut $ put val
    in case Data.Binary.Get.runGetOrFail (get :: Get (BCOByteArray a)) ser of
         Right (_, _, BCOByteArray arr'# )
           | ByteArray arr# == ByteArray arr'#  -> return ()
           | otherwise                          -> putStrLn "failed to round-trip"
         Left _                                 -> putStrLn "deserialization failed"

main :: IO ()
main = do
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Int)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word8)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word16)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word32)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word64)
    roundtripTest (AU.listArray (1,500) ['a'..] :: UArray Int Char)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Int)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word8)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word16)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word32)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word64)
    roundtripTestByteArray (AU.listArray (1,500) ['a'..] :: UArray Int Char)
