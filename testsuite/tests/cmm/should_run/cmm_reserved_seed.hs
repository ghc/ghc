{-# LANGUAGE MagicHash, GHCForeignImportPrim, UnliftedFFITypes #-}
import GHC.Exts

foreign import prim "rs_loop" rs_loop# :: Word# -> Word#

run :: Word -> Word
run (W# n) = W# (rs_loop# n)
{-# NOINLINE run #-}

-- the turns take each of the loop's three paths and both of its joins
main :: IO ()
main = mapM_ (print . run) [0, 1, 2, 7, 12, 100]
