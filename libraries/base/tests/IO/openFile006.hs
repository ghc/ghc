-- !!! opening a file in WriteMode better truncate it

import System.IO

main = do
  h <- openFile "openFile006.out" AppendMode
  hPutStr h "hello, world"
  size <- hFileSize h
  print size
  hClose h
 
  h <- openFile "openFile006.out" WriteMode
  size <- hFileSize h
  print size
