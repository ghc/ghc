{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

-- Each procedure of cmm_regoff_cmm.cmm accesses memory at q + k, for offsets k on
-- both sides of the 32-bit displacement bounds; q is p - k, so the address
-- is p, a pointer into a buffer of known bytes.

import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import GHC.Exts

foreign import prim "rl01" rl01# :: Word# -> Word#
foreign import prim "rn01" rn01# :: Word# -> Word#
foreign import prim "rs01" rs01# :: Word# -> Word# -> Word#
foreign import prim "rl02" rl02# :: Word# -> Word#
foreign import prim "rn02" rn02# :: Word# -> Word#
foreign import prim "rs02" rs02# :: Word# -> Word# -> Word#
foreign import prim "rl03" rl03# :: Word# -> Word#
foreign import prim "rn03" rn03# :: Word# -> Word#
foreign import prim "rs03" rs03# :: Word# -> Word# -> Word#
foreign import prim "rl04" rl04# :: Word# -> Word#
foreign import prim "rn04" rn04# :: Word# -> Word#
foreign import prim "rs04" rs04# :: Word# -> Word# -> Word#
foreign import prim "rl05" rl05# :: Word# -> Word#
foreign import prim "rn05" rn05# :: Word# -> Word#
foreign import prim "rs05" rs05# :: Word# -> Word# -> Word#
foreign import prim "rl06" rl06# :: Word# -> Word#
foreign import prim "rn06" rn06# :: Word# -> Word#
foreign import prim "rs06" rs06# :: Word# -> Word# -> Word#
foreign import prim "rl07" rl07# :: Word# -> Word#
foreign import prim "rn07" rn07# :: Word# -> Word#
foreign import prim "rs07" rs07# :: Word# -> Word# -> Word#
foreign import prim "rl08" rl08# :: Word# -> Word#
foreign import prim "rn08" rn08# :: Word# -> Word#
foreign import prim "rs08" rs08# :: Word# -> Word# -> Word#
foreign import prim "rl09" rl09# :: Word# -> Word#
foreign import prim "rn09" rn09# :: Word# -> Word#
foreign import prim "rs09" rs09# :: Word# -> Word# -> Word#
foreign import prim "rl10" rl10# :: Word# -> Word#
foreign import prim "rn10" rn10# :: Word# -> Word#
foreign import prim "rs10" rs10# :: Word# -> Word# -> Word#
foreign import prim "rl11" rl11# :: Word# -> Word#
foreign import prim "rn11" rn11# :: Word# -> Word#
foreign import prim "rs11" rs11# :: Word# -> Word# -> Word#
foreign import prim "rl12" rl12# :: Word# -> Word#
foreign import prim "rn12" rn12# :: Word# -> Word#
foreign import prim "rs12" rs12# :: Word# -> Word# -> Word#

un :: Word -> Word#
un (W# w) = w

cases :: [(Integer, Word -> Word, Word -> Word, Word -> Word -> Word)]
cases =
  [ (8, \q -> W# (rl01# (un q)), \q -> W# (rn01# (un q)), \q v -> W# (rs01# (un q) (un v)))
  , (-8, \q -> W# (rl02# (un q)), \q -> W# (rn02# (un q)), \q v -> W# (rs02# (un q) (un v)))
  , (2147483640, \q -> W# (rl03# (un q)), \q -> W# (rn03# (un q)), \q v -> W# (rs03# (un q) (un v)))
  , (2147483647, \q -> W# (rl04# (un q)), \q -> W# (rn04# (un q)), \q v -> W# (rs04# (un q) (un v)))
  , (2147483648, \q -> W# (rl05# (un q)), \q -> W# (rn05# (un q)), \q v -> W# (rs05# (un q) (un v)))
  , (-2147483648, \q -> W# (rl06# (un q)), \q -> W# (rn06# (un q)), \q v -> W# (rs06# (un q) (un v)))
  , (-2147483649, \q -> W# (rl07# (un q)), \q -> W# (rn07# (un q)), \q v -> W# (rs07# (un q) (un v)))
  , (-2147483656, \q -> W# (rl08# (un q)), \q -> W# (rn08# (un q)), \q v -> W# (rs08# (un q) (un v)))
  , (4294967295, \q -> W# (rl09# (un q)), \q -> W# (rn09# (un q)), \q v -> W# (rs09# (un q) (un v)))
  , (4294967296, \q -> W# (rl10# (un q)), \q -> W# (rn10# (un q)), \q v -> W# (rs10# (un q) (un v)))
  , (9223372036854775807, \q -> W# (rl11# (un q)), \q -> W# (rn11# (un q)), \q v -> W# (rs11# (un q) (un v)))
  , (-9223372036854775808, \q -> W# (rl12# (un q)), \q -> W# (rn12# (un q)), \q v -> W# (rs12# (un q) (un v)))
  ]

main :: IO ()
main = allocaBytesAligned 64 8 $ \buf -> do
  forM_ [0 .. 63] $ \i -> pokeByteOff buf i (fromIntegral (37 * i + 5) :: Word8)
  let p = buf `plusPtr` 16
      pw = fromIntegral (ptrToWordPtr p) :: Word
  bad <- forM cases $ \(k, load, narrow, store) -> do
    let q = pw - fromInteger k
    w64 <- peek (castPtr p) :: IO Word64
    i8 <- peek (castPtr p) :: IO Int8
    w16 <- peek (castPtr p) :: IO Word16
    i32 <- peek (castPtr p) :: IO Int32
    let v = 0x0123456789abcdef + fromInteger k
        stored = (v .&. complement 0xff) .|. ((v `shiftR` 8) .&. 0xff)
        r = store q v
    r `seq` return ()
    back <- peek (castPtr p) :: IO Word64
    pokeByteOff p 0 w64
    let got = [load q, narrow q, r, fromIntegral back]
        want = [fromIntegral w64, fromIntegral i8 + fromIntegral w16 + fromIntegral i32, stored, stored]
    when (got /= want) $ putStrLn ("offset " ++ show k ++ ": " ++ show got ++ " /= " ++ show want)
    return (got /= want)
  putStrLn (show (length cases) ++ " offsets, mismatches " ++ show (length (filter id bad)))
