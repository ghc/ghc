import System.IO
import System.IO.Error

-- !!! Open a directory (should fail)

main = do
  r <- tryIOError (openFile "." ReadMode)
  print r
  r <- tryIOError (openFile "." WriteMode)
  print r
  r <- tryIOError (openFile "." AppendMode)
  print r
  r <- tryIOError (openFile "." ReadWriteMode)
  print r
