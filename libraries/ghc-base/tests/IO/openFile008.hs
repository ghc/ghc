import System.IO
import System.Cmd
import System.FilePath
import Text.Printf
import System.Directory
import Control.Monad

testdir = "openFile008_testdir"

-- Test repeated opening/closing of 1000 files.  This is useful for guaging
-- the performance of open/close and file locking.
main = do
  system ("rm -rf " ++ testdir)
  createDirectory testdir
  let filenames = [testdir </> printf "file%03d" (n::Int) | n <- [1..1000]]

  forM_ [1..50] $ \_ -> do
    hs <- mapM (\f -> openFile f WriteMode) filenames
    mapM_ hClose hs

  mapM_ removeFile filenames
  removeDirectory testdir
