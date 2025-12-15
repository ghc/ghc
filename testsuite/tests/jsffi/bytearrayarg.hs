{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Test where

import GHC.Exts
import GHC.IO
import GHC.Word (Word8(W8#))

foreign import javascript unsafe "(() => { const u8 = new Uint8Array(__exports.memory.buffer, $1, 4); return (u8[0] === 0x12 && u8[1] === 0x34 && u8[2] === 0x56 && u8[3] === 0x78) ? 1 : 0; })()"
  js_check_mba :: MutableByteArray# RealWorld -> IO Int

foreign import javascript unsafe "(() => { const u8 = new Uint8Array(__exports.memory.buffer, $1, 4); return (u8[0] === 0x12 && u8[1] === 0x34 && u8[2] === 0x56 && u8[3] === 0x78) ? 1 : 0; })()"
  js_check_ba :: ByteArray# -> IO Int

foreign export javascript "main"
  main :: IO ()

main :: IO ()
main =
  IO $ \s0 ->
    case newPinnedByteArray# 4# s0 of
      (# s1, mba# #) ->
        case (0x12 :: Word8) of { W8# b0# ->
        case (0x34 :: Word8) of { W8# b1# ->
        case (0x56 :: Word8) of { W8# b2# ->
        case (0x78 :: Word8) of { W8# b3# ->
          let s2 = writeWord8Array# mba# 0# b0# s1
              s3 = writeWord8Array# mba# 1# b1# s2
              s4 = writeWord8Array# mba# 2# b2# s3
              s5 = writeWord8Array# mba# 3# b3# s4
           in case unIO (js_check_mba mba#) s5 of
                (# s6, ok_mba #) -> case unsafeFreezeByteArray# mba# s6 of
                  (# s7, ba# #) -> case unIO (js_check_ba ba#) s7 of
                    (# s8, ok_ba #) -> case unIO (print ok_mba) s8 of
                      (# s9, _ #) -> case unIO (print ok_ba) s9 of
                        (# s10, _ #) -> (# s10, () #)
        }}}}
