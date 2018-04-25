import System.Directory
import System.IO
import System.IO.Error

-- !!! Open a directory (should fail)

main = do
  let dir = "openFile003Dir"
  createDirectoryIfMissing False dir
  r <- tryIOError (openFile dir ReadMode)
  print r
  r <- tryIOError (openFile dir WriteMode)
  print r
  r <- tryIOError (openFile dir AppendMode)
  print r
  r <- tryIOError (openFile dir ReadWriteMode)
  print r
