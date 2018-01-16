module Main where

import Control.Monad
import Foreign.Ptr
import Graphics.Win32.GDI.Clip

main = do
  openClipboard nullPtr
  go 0
  where
    go n = do
      n' <- enumClipboardFormats n
      unless (n == 0) (go n')
