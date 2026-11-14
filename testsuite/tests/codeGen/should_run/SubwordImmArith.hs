{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main (main) where

import GHC.Exts

foreign import prim "runAddImmSubword8zh"  runAddImmSubword8#  :: Word# -> Word#
foreign import prim "runAddImmSubword16zh" runAddImmSubword16# :: Word# -> Word#
foreign import prim "runAddImmSubword32zh" runAddImmSubword32# :: Word# -> Word#
foreign import prim "runSubImmSubword8zh"  runSubImmSubword8#  :: Word# -> Word#
foreign import prim "runSubImmSubword16zh" runSubImmSubword16# :: Word# -> Word#
foreign import prim "runSubImmSubword32zh" runSubImmSubword32# :: Word# -> Word#
foreign import prim "runOrImmSubword8zh"   runOrImmSubword8#   :: Word# -> Word#
foreign import prim "runOrImmSubword16zh"  runOrImmSubword16#  :: Word# -> Word#
foreign import prim "runOrImmSubword32zh"  runOrImmSubword32#  :: Word# -> Word#

main :: IO ()
main = do
  putStrLn ("add W8  255: "        ++ show (W# (runAddImmSubword8# 255##)))
  putStrLn ("add W16 65535: "      ++ show (W# (runAddImmSubword16# 65535##)))
  putStrLn ("add W32 4294967295: " ++ show (W# (runAddImmSubword32# 4294967295##)))
  putStrLn ("sub W8  0: "          ++ show (W# (runSubImmSubword8# 0##)))
  putStrLn ("sub W16 0: "          ++ show (W# (runSubImmSubword16# 0##)))
  putStrLn ("sub W32 0: "          ++ show (W# (runSubImmSubword32# 0##)))
  putStrLn ("or W8  0: "           ++ show (W# (runOrImmSubword8# 0##)))
  putStrLn ("or W16 0: "           ++ show (W# (runOrImmSubword16# 0##)))
  putStrLn ("or W32 0: "           ++ show (W# (runOrImmSubword32# 0##)))
