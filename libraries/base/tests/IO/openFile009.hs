import System.IO
import System.Cmd
import System.FilePath
import Text.Printf
import System.Directory
import Control.Monad

testfile = "openFile009_testfile"

-- Make sure opening with append doesn't truncate files.
main = do
  h <- openFile testfile WriteMode
  hPutStr h "Hello"
  hClose h
  h <- openFile testfile AppendMode
  hPutStr h " World!"
  hClose h
  s <- readFile testfile
  putStrLn s
