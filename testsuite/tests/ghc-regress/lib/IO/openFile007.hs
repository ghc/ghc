-- !!! check that we don't truncate files if the open fails

import IO
import Monad

tmp = "openFile007.out"

main = do
  h <- openFile tmp WriteMode
  hPutStrLn h "hello, world"

  -- second open in write mode better fail, but better not truncate the file
  try (openFile tmp WriteMode) >>= print
  
  hClose h
  s <- readFile tmp -- make sure our "hello, world" is still there
  putStr s
