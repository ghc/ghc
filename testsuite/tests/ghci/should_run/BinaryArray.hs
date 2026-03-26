{-# LANGUAGE FlexibleContexts, MagicHash, ScopedTypeVariables #-}
import Data.Binary.Get
import Data.Binary.Put
import Data.Binary (Binary, get, put)
import Data.Array.Byte
import Data.Array.Unboxed as AU
import Data.Array.Base as A
import GHCi.ResolvedBCO
import GHC.Word

-- See Note [BCOByteArray serialization]
roundtripTestByteArray :: forall a . (IArray UArray a, Eq a, Binary (BCOByteArray a))
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
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word)
    roundtripTestByteArray (AU.listArray (1,500) [1..] :: UArray Int Word16)
