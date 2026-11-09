{-# LANGUAGE GHCForeignImportPrim, MagicHash, UnliftedFFITypes #-}

-- Regression test for #28101. See T28101_cmm.cmm.

import GHC.Exts
import Numeric (showHex)

foreign import prim "T28101_f32_qnan"     f32_qnan     :: Word# -> Word#
foreign import prim "T28101_f32_neg_qnan" f32_neg_qnan :: Word# -> Word#
foreign import prim "T28101_f32_payload"  f32_payload  :: Word# -> Word#
foreign import prim "T28101_f64_qnan"     f64_qnan     :: Word# -> Word#
foreign import prim "T28101_f64_neg_qnan" f64_neg_qnan :: Word# -> Word#
foreign import prim "T28101_f64_payload"  f64_payload  :: Word# -> Word#

run :: String -> (Word# -> Word#) -> IO ()
run name f = putStrLn (name ++ ": 0x" ++ showHex (W# (f 0##)) "")

main :: IO ()
main = do
  run "f32_qnan"     f32_qnan
  run "f32_neg_qnan" f32_neg_qnan
  run "f32_payload"  f32_payload
  run "f64_qnan"     f64_qnan
  run "f64_neg_qnan" f64_neg_qnan
  run "f64_payload"  f64_payload
