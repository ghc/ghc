{-# LANGUAGE GHCForeignImportPrim #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnliftedFFITypes #-}

module Main where

import GHC.Exts
import GHC.Word

foreign import prim "sumWord"
  sumWord# :: Word# -> (# (# #) | Word# #)

render :: Word# -> String
render w# =
  case sumWord# w# of
    (# (# #) | #) -> "none"
    (# | r# #) -> "some " ++ show (W# r#)

main :: IO ()
main = do
  putStrLn (render 0##)
  putStrLn (render 5##)
