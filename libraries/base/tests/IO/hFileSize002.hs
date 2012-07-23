-- !!! Testing IO.hFileSize
module Main(main) where

import Control.Monad
import System.Directory ( removeFile, doesFileExist )
import System.IO
import System.IO.Error

main = do
  sz <- hFileSize stdin `catchIOError` (\ _ -> return (-1))
  print sz
  let fn = "hFileSize002.out"
  f <- doesFileExist fn
  when f (removeFile fn)
  hdl <- openFile fn WriteMode
  hPutStr hdl "file_size"
   -- with default buffering
  sz <- hFileSize hdl
  print sz

  hSetBuffering hdl NoBuffering
  hPutStr hdl "file_size"
   -- with no buffering
  sz <- hFileSize hdl
  print sz
  hSetBuffering hdl LineBuffering
  hPutStr hdl "file_size"
   -- with line buffering
  sz <- hFileSize hdl
  print sz
  hSetBuffering hdl (BlockBuffering (Just 4))
   -- with block buffering
  hPutStr hdl "file_size"
  sz <- hFileSize hdl
  print sz
  hClose hdl
