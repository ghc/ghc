module Main where

import Foreign.C

foreign import ccall unsafe "T23034.h t_printf"
  t_printf :: CLong -> CInt -> CShort -> CSChar -> IO ()

main = t_printf (-1) (-2) (-3) (-4)
