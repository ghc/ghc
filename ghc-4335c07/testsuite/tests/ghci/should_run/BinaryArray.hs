{-# LANGUAGE FlexibleContexts #-}
import Data.Binary.Get
import Data.Binary.Put
import Data.Array.Unboxed as AU
import Data.Array.IO (IOUArray)
import Data.Array.MArray (MArray)
import Data.Array as A
import GHCi.BinaryArray
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

main :: IO ()
main = do
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Int)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word8)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word16)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word32)
    roundtripTest (AU.listArray (1,500) [1..] :: UArray Int Word64)
    roundtripTest (AU.listArray (1,500) ['a'..] :: UArray Int Char)
