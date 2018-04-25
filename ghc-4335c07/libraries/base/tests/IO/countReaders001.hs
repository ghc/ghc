-- test for trac #629. We need to keep track of how many readers
-- there are rather than closing the first read handle causing the
-- lock to be released.

import System.IO
import System.IO.Error

file = "countReaders001.txt"

main = do
  writeFile file "foo"

  h1 <- openFile file ReadMode
  h2 <- openFile file ReadMode
  hClose h1
  tryIOError (openFile file AppendMode) >>= print

