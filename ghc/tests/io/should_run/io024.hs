-- !!! Testing IO.hFileSize
module Main(main) where

import IO
import Directory ( removeFile )

main = do
  sz <- hFileSize stdin `catch` (\ _ -> return (-1))
  print sz
  let fn = "io025.out" 
  hdl <- openFile fn WriteMode
  removeFile fn
  hPutStrLn hdl "file_size"
   -- with default buffering
  sz <- hFileSize hdl
  print sz

  hSetBuffering hdl NoBuffering
  hPutStrLn hdl "file_size"
   -- with no buffering
  sz <- hFileSize hdl
  print sz
  hSetBuffering hdl LineBuffering
  hPutStrLn hdl "file_size"
   -- with line buffering
  sz <- hFileSize hdl
  print sz
  hSetBuffering hdl (BlockBuffering (Just 4))
   -- with block buffering
  hPutStrLn hdl "file_size"
  sz <- hFileSize hdl
  print sz
  hClose hdl
