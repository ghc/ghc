-- !!! Flushing
module Main(main) where

import IO
import Directory ( removeFile, doesFileExist )
import Monad

main = do
  hFlush stdin `catch` \ _ -> putStrLn "No can do - flushing read-only handles isn't legal"
  putStr "Hello,"
  hFlush stdout
  putStr "Hello - "
  hFlush stderr
  hdl <- openFile "hFlush001.hs" ReadMode
  hFlush hdl `catch` \ _ -> putStrLn "No can do - flushing read-only handles isn't legal"
  hClose hdl
  remove
  hdl <- openFile "hFlush001.out" WriteMode
  hFlush hdl
  hClose hdl
  remove
  hdl <- openFile "hFlush001.out" AppendMode
  hFlush hdl
  hClose hdl
  remove
  hdl <- openFile "hFlush001.out" ReadWriteMode
  hFlush hdl
  hClose hdl
 where remove = do
         f <- doesFileExist "hFlush001.out"
         when f (removeFile "hFlush001.out")
