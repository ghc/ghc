-- !!! RW files 
module Main(main) where

import IO
import Directory ( removeFile, doesFileExist )
import Monad
#if defined(__MINGW32__)
import PrelHandle(hSetBinaryMode)
#endif

main = do
  f <- doesFileExist "readwrite001.inout" 
  when f (removeFile "readwrite001.inout")
  hdl <- openFile "readwrite001.inout" ReadWriteMode
# if defined(__MINGW32__)
  hSetBinaryMode hdl True
# endif
  hSetBuffering hdl LineBuffering
  hPutStr hdl "as"
  hSeek hdl AbsoluteSeek 0
  ch <- hGetChar hdl
  print ch
  hPutStr hdl "ase"
  hSeek hdl AbsoluteSeek 0
  putChar '\n'
  ls <- hGetContents hdl
  putStrLn ls

