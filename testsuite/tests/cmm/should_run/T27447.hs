{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

import Foreign
import GHC.Exts
import GHC.IO
import GHC.Word
import Text.Printf

foreign import prim "t27447_repro"
  t27447_repro# :: Addr# -> State# RealWorld -> (# State# RealWorld, Word64# #)

expected :: Word64
expected = 0x2222222222222222

stale :: Word64
stale = 0x1111111111111111

runCmm :: Ptr a -> IO Word64
runCmm (Ptr p#) = IO $ \s ->
  case t27447_repro# p# s of
    (# s', w# #) -> (# s', W64# w# #)

main :: IO ()
main = allocaBytesAligned 48 8 $ \raw -> do
  let node0 = raw
      node1 = raw `plusPtr` 24

  pokeByteOff node0 0 (1 :: Word64)
  pokeByteOff node0 8 node1
  pokeByteOff node0 16 stale

  pokeByteOff node1 0 (0 :: Word64)
  pokeByteOff node1 8 node1
  pokeByteOff node1 16 expected

  got <- runCmm node0
  printf "0x%016x\n" got
