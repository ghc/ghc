--!!! Flushing
module Main(main) where

import IO
import Directory ( removeFile )

main = do
  hFlush stdin `catch` \ _ -> putStrLn "No can do - flushing read-only handles isn't legal"
  putStr "Hello,"
  hFlush stdout
  putStr "Hello - "
  hFlush stderr
  hdl <- openFile "io029.hs" ReadMode
  hFlush hdl `catch` \ _ -> putStrLn "No can do - flushing read-only handles isn't legal"
  hClose hdl
  hdl <- openFile "io029.out" WriteMode
  removeFile "io029.out"
  hFlush hdl
  hClose hdl
  hdl <- openFile "io029.out" AppendMode
  removeFile "io029.out"
  hFlush hdl
  hClose hdl
  hdl <- openFile "io029.out" ReadWriteMode
  removeFile "io029.out"
  hFlush hdl
  hClose hdl
