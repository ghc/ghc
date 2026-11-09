{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}

-- Each procedure of cmm_swoff_cmm.cmm returns 10 + its switch's case number.

import GHC.Exts

foreign import prim "w01" w01# :: Word# -> Word#
foreign import prim "w02" w02# :: Word# -> Word#
foreign import prim "w03" w03# :: Word# -> Word#
foreign import prim "w04" w04# :: Word# -> Word#
foreign import prim "w05" w05# :: Word# -> Word#
foreign import prim "w06" w06# :: Word# -> Word#
foreign import prim "w07" w07# :: Word# -> Word#

call :: (Word# -> Word#) -> Word -> Word
call f (W# x) = W# (f x)

main :: IO ()
main = print
  [ map (call w01#) [101, 102, 103, 104, 105, 106]
  , map (call w02#) [97, 98, 99, 100, 101, 102]
  , map (call w03#) [95, 96, 97, 98, 99, 100]
  , map (call w04#) [101, 102, 103, 104, 105, 106, 229, 1130]
  , map (call w05#) [101, 102, 103, 104, 105, 106, 4294967397, 8589934698]
  , map (call w06#) [95, 96, 97, 98, 99, 100, 4294967396]
  , map (call w07#) [2, 3, 4, 5, 6, 7]
  ]
