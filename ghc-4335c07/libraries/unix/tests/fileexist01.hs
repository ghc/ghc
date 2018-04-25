-- test System.Posix.fileExist
import System.Posix
main = do
  fileExist "fileexist01.hs" >>= print
  fileExist "does not exist" >>= print
